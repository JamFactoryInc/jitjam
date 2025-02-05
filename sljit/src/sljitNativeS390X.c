/*
 *    Stack-less Just-In-Time compiler
 *
 *    Copyright Zoltan Herczeg (hzmester@freemail.hu). All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are
 * permitted provided that the following conditions are met:
 *
 *   1. Redistributions of source code must retain the above copyright notice, this list of
 *      conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright notice, this list
 *      of conditions and the following disclaimer in the documentation and/or other materials
 *      provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER(S) AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
 * SHALL THE COPYRIGHT HOLDER(S) OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <sys/auxv.h>

#ifdef __ARCH__
#define ENABLE_STATIC_FACILITY_DETECTION 1
#else
#define ENABLE_STATIC_FACILITY_DETECTION 0
#endif
#define ENABLE_DYNAMIC_FACILITY_DETECTION 1

SLJIT2_API_FUNC_ATTRIBUTE const char* sljit2_get_platform_name(void)
{
	return "s390x" SLJIT2_CPUINFO;
}

/* Instructions are stored as 64 bit values regardless their size. */
typedef sljit2_uw sljit2_ins;

#define TMP_REG1	(SLJIT2_NUMBER_OF_REGISTERS + 2)
#define TMP_REG2	(SLJIT2_NUMBER_OF_REGISTERS + 3)

static const sljit2_u8 reg_map[SLJIT2_NUMBER_OF_REGISTERS + 5] = {
	0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 0, 1, 14
};

/* there are also a[2-15] available, but they are slower to access and
 * their use is limited as mundaym explained:
 *   https://github.com/zherczeg/sljit/pull/91#discussion_r486895689
 */

/* General Purpose Registers [0-15]. */
typedef sljit2_uw sljit2_gpr;

/*
 * WARNING
 * the following code is non standard and should be improved for
 * consistency, but doesn't use SLJIT2_NUMBER_OF_REGISTERS based
 * registers because r0 and r1 are the ABI recommended volatiles.
 * there is a gpr() function that maps sljit to physical register numbers
 * that should be used instead of the usual index into reg_map[] and
 * will be retired ASAP (TODO: carenas)
 */

static const sljit2_gpr r0 = 0;		/* reg_map[SLJIT2_NUMBER_OF_REGISTERS + 2]: 0 in address calculations; reserved */
static const sljit2_gpr r1 = 1;		/* reg_map[SLJIT2_NUMBER_OF_REGISTERS + 3]: reserved */
static const sljit2_gpr r2 = 2;		/* reg_map[1]: 1st argument */
static const sljit2_gpr r3 = 3;		/* reg_map[2]: 2nd argument */
static const sljit2_gpr r4 = 4;		/* reg_map[3]: 3rd argument */
static const sljit2_gpr r5 = 5;		/* reg_map[4]: 4th argument */
static const sljit2_gpr r6 = 6;		/* reg_map[5]: 5th argument; 1st saved register */
static const sljit2_gpr r7 = 7;		/* reg_map[6] */
static const sljit2_gpr r8 = 8;		/* reg_map[7] */
static const sljit2_gpr r9 = 9;		/* reg_map[8] */
static const sljit2_gpr r10 = 10;	/* reg_map[9] */
static const sljit2_gpr r11 = 11;	/* reg_map[10] */
static const sljit2_gpr r12 = 12;	/* reg_map[11]: GOT */
static const sljit2_gpr r13 = 13;	/* reg_map[12]: Literal Pool pointer */
static const sljit2_gpr r14 = 14;	/* reg_map[0]: return address */
static const sljit2_gpr r15 = 15;	/* reg_map[SLJIT2_NUMBER_OF_REGISTERS + 1]: stack pointer */

/* WARNING: r12 and r13 shouldn't be used as per ABI recommendation */
/* TODO(carenas): r12 might conflict in PIC code, reserve? */
/* TODO(carenas): r13 is usually pointed to "pool" per ABI, using a tmp
 *                like we do know might be faster though, reserve?
 */

/* TODO(carenas): should be named TMP_REG[1-2] for consistency */
#define tmp0	r0
#define tmp1	r1

/* When reg cannot be unused. */
#define IS_GPR_REG(reg)		((reg > 0) && (reg) <= SLJIT2_SP)

/* Link register. */
static const sljit2_gpr link_r = 14;     /* r14 */

#define TMP_FREG1	(SLJIT2_NUMBER_OF_FLOAT_REGISTERS + 1)

static const sljit2_u8 freg_map[SLJIT2_NUMBER_OF_FLOAT_REGISTERS + 2] = {
	0, 0, 2, 4, 6, 3, 5, 7, 15, 14, 13, 12, 11, 10, 9, 8, 1
};

#define R0A(r) (r)
#define R4A(r) ((r) << 4)
#define R8A(r) ((r) << 8)
#define R12A(r) ((r) << 12)
#define R16A(r) ((r) << 16)
#define R20A(r) ((r) << 20)
#define R28A(r) ((r) << 28)
#define R32A(r) ((r) << 32)
#define R36A(r) ((r) << 36)

#define R0(r) ((sljit2_ins)reg_map[r])

#define F0(r) ((sljit2_ins)freg_map[r])
#define F4(r) (R4A((sljit2_ins)freg_map[r]))
#define F12(r) (R12A((sljit2_ins)freg_map[r]))
#define F20(r) (R20A((sljit2_ins)freg_map[r]))
#define F28(r) (R28A((sljit2_ins)freg_map[r]))
#define F32(r) (R32A((sljit2_ins)freg_map[r]))
#define F36(r) (R36A((sljit2_ins)freg_map[r]))

struct sljit2_s390x_const {
	struct sljit2_const const_; /* must be first */
	sljit2_sw init_value;       /* required to build literal pool */
};

/* Convert SLJIT register to hardware register. */
static SLJIT2_INLINE sljit2_gpr gpr(sljit2_s32 r)
{
	SLJIT2_ASSERT(r >= 0 && r < (sljit2_s32)(sizeof(reg_map) / sizeof(reg_map[0])));
	return reg_map[r];
}

static sljit2_s32 push_inst(struct sljit2_compiler *compiler, sljit2_ins ins)
{
	sljit2_ins *ibuf = (sljit2_ins *)ensure_buf(compiler, sizeof(sljit2_ins));
	FAIL_IF(!ibuf);
	*ibuf = ins;

	SLJIT2_ASSERT(ins <= 0xffffffffffffL);

	compiler->size++;
	if (ins & 0xffff00000000L)
		compiler->size++;

	if (ins & 0xffffffff0000L)
		compiler->size++;

	return SLJIT2_SUCCESS;
}

#define SLJIT2_ADD_SUB_NO_COMPARE(status_flags_state) \
	(((status_flags_state) & (SLJIT2_CURRENT_FLAGS_ADD | SLJIT2_CURRENT_FLAGS_SUB)) \
		&& !((status_flags_state) & SLJIT2_CURRENT_FLAGS_COMPARE))

/* Map the given type to a 4-bit condition code mask. */
static SLJIT2_INLINE sljit2_u8 get_cc(struct sljit2_compiler *compiler, sljit2_s32 type) {
	const sljit2_u8 cc0 = 1 << 3; /* equal {,to zero} */
	const sljit2_u8 cc1 = 1 << 2; /* less than {,zero} */
	const sljit2_u8 cc2 = 1 << 1; /* greater than {,zero} */
	const sljit2_u8 cc3 = 1 << 0; /* {overflow,NaN} */

	switch (type) {
	case SLJIT2_EQUAL:
		if (SLJIT2_ADD_SUB_NO_COMPARE(compiler->status_flags_state)) {
			sljit2_s32 type = GET_FLAG_TYPE(compiler->status_flags_state);
			if (type >= SLJIT2_SIG_LESS && type <= SLJIT2_SIG_LESS_EQUAL)
				return cc0;
			if (type == SLJIT2_OVERFLOW)
				return (cc0 | cc3);
			return (cc0 | cc2);
		}
		/* fallthrough */

	case SLJIT2_ATOMIC_STORED:
	case SLJIT2_F_EQUAL:
	case SLJIT2_ORDERED_EQUAL:
		return cc0;

	case SLJIT2_NOT_EQUAL:
		if (SLJIT2_ADD_SUB_NO_COMPARE(compiler->status_flags_state)) {
			sljit2_s32 type = GET_FLAG_TYPE(compiler->status_flags_state);
			if (type >= SLJIT2_SIG_LESS && type <= SLJIT2_SIG_LESS_EQUAL)
				return (cc1 | cc2 | cc3);
			if (type == SLJIT2_OVERFLOW)
				return (cc1 | cc2);
			return (cc1 | cc3);
		}
		/* fallthrough */

	case SLJIT2_UNORDERED_OR_NOT_EQUAL:
		return (cc1 | cc2 | cc3);

	case SLJIT2_LESS:
	case SLJIT2_ATOMIC_NOT_STORED:
		return cc1;

	case SLJIT2_GREATER_EQUAL:
	case SLJIT2_UNORDERED_OR_GREATER_EQUAL:
		return (cc0 | cc2 | cc3);

	case SLJIT2_GREATER:
		if (compiler->status_flags_state & SLJIT2_CURRENT_FLAGS_COMPARE)
			return cc2;
		return cc3;

	case SLJIT2_LESS_EQUAL:
		if (compiler->status_flags_state & SLJIT2_CURRENT_FLAGS_COMPARE)
			return (cc0 | cc1);
		return (cc0 | cc1 | cc2);

	case SLJIT2_SIG_LESS:
	case SLJIT2_F_LESS:
	case SLJIT2_ORDERED_LESS:
		return cc1;

	case SLJIT2_NOT_CARRY:
		if (compiler->status_flags_state & SLJIT2_CURRENT_FLAGS_SUB)
			return (cc2 | cc3);
		/* fallthrough */

	case SLJIT2_SIG_LESS_EQUAL:
	case SLJIT2_F_LESS_EQUAL:
	case SLJIT2_ORDERED_LESS_EQUAL:
		return (cc0 | cc1);

	case SLJIT2_CARRY:
		if (compiler->status_flags_state & SLJIT2_CURRENT_FLAGS_SUB)
			return (cc0 | cc1);
		/* fallthrough */

	case SLJIT2_SIG_GREATER:
	case SLJIT2_UNORDERED_OR_GREATER:
		/* Overflow is considered greater, see SLJIT2_SUB. */
		return cc2 | cc3;

	case SLJIT2_SIG_GREATER_EQUAL:
		return (cc0 | cc2 | cc3);

	case SLJIT2_OVERFLOW:
		if (compiler->status_flags_state & SLJIT2_SET_Z)
			return (cc2 | cc3);
		/* fallthrough */

	case SLJIT2_UNORDERED:
		return cc3;

	case SLJIT2_NOT_OVERFLOW:
		if (compiler->status_flags_state & SLJIT2_SET_Z)
			return (cc0 | cc1);
		/* fallthrough */

	case SLJIT2_ORDERED:
		return (cc0 | cc1 | cc2);

	case SLJIT2_F_NOT_EQUAL:
	case SLJIT2_ORDERED_NOT_EQUAL:
		return (cc1 | cc2);

	case SLJIT2_F_GREATER:
	case SLJIT2_ORDERED_GREATER:
		return cc2;

	case SLJIT2_F_GREATER_EQUAL:
	case SLJIT2_ORDERED_GREATER_EQUAL:
		return (cc0 | cc2);

	case SLJIT2_UNORDERED_OR_LESS_EQUAL:
		return (cc0 | cc1 | cc3);

	case SLJIT2_UNORDERED_OR_EQUAL:
		return (cc0 | cc3);

	case SLJIT2_UNORDERED_OR_LESS:
		return (cc1 | cc3);
	}

	SLJIT2_UNREACHABLE();
	return (sljit2_u8)-1;
}

/* Facility to bit index mappings.
   Note: some facilities share the same bit index. */
typedef sljit2_uw facility_bit;
#define STORE_FACILITY_LIST_EXTENDED_FACILITY 7
#define FAST_LONG_DISPLACEMENT_FACILITY 19
#define EXTENDED_IMMEDIATE_FACILITY 21
#define GENERAL_INSTRUCTION_EXTENSION_FACILITY 34
#define DISTINCT_OPERAND_FACILITY 45
#define HIGH_WORD_FACILITY 45
#define POPULATION_COUNT_FACILITY 45
#define LOAD_STORE_ON_CONDITION_1_FACILITY 45
#define MISCELLANEOUS_INSTRUCTION_EXTENSIONS_1_FACILITY 49
#define LOAD_STORE_ON_CONDITION_2_FACILITY 53
#define MISCELLANEOUS_INSTRUCTION_EXTENSIONS_2_FACILITY 58
#define VECTOR_FACILITY 129
#define VECTOR_ENHANCEMENTS_1_FACILITY 135

/* Report whether a facility is known to be present due to the compiler
   settings. This function should always be compiled to a constant
   value given a constant argument. */
static SLJIT2_INLINE int have_facility_static(facility_bit x)
{
#if ENABLE_STATIC_FACILITY_DETECTION
	switch (x) {
	case FAST_LONG_DISPLACEMENT_FACILITY:
		return (__ARCH__ >=  6 /* z990 */);
	case EXTENDED_IMMEDIATE_FACILITY:
	case STORE_FACILITY_LIST_EXTENDED_FACILITY:
		return (__ARCH__ >=  7 /* z9-109 */);
	case GENERAL_INSTRUCTION_EXTENSION_FACILITY:
		return (__ARCH__ >=  8 /* z10 */);
	case DISTINCT_OPERAND_FACILITY:
		return (__ARCH__ >=  9 /* z196 */);
	case MISCELLANEOUS_INSTRUCTION_EXTENSIONS_1_FACILITY:
		return (__ARCH__ >= 10 /* zEC12 */);
	case LOAD_STORE_ON_CONDITION_2_FACILITY:
	case VECTOR_FACILITY:
		return (__ARCH__ >= 11 /* z13 */);
	case MISCELLANEOUS_INSTRUCTION_EXTENSIONS_2_FACILITY:
	case VECTOR_ENHANCEMENTS_1_FACILITY:
		return (__ARCH__ >= 12 /* z14 */);
	default:
		SLJIT2_UNREACHABLE();
	}
#endif
	return 0;
}

static SLJIT2_INLINE unsigned long get_hwcap()
{
	static unsigned long hwcap = 0;
	if (SLJIT2_UNLIKELY(!hwcap)) {
		hwcap = getauxval(AT_HWCAP);
		SLJIT2_ASSERT(hwcap != 0);
	}
	return hwcap;
}

static SLJIT2_INLINE int have_stfle()
{
	if (have_facility_static(STORE_FACILITY_LIST_EXTENDED_FACILITY))
		return 1;

	return (get_hwcap() & HWCAP_S390_STFLE);
}

/* Report whether the given facility is available. This function always
   performs a runtime check. */
static int have_facility_dynamic(facility_bit x)
{
#if ENABLE_DYNAMIC_FACILITY_DETECTION
	static struct {
		sljit2_uw bits[4];
	} cpu_features;
	size_t size = sizeof(cpu_features);
	const sljit2_uw word_index = x >> 6;
	const sljit2_uw bit_index = ((1UL << 63) >> (x & 63));

	SLJIT2_ASSERT(x < size * 8);
	if (SLJIT2_UNLIKELY(!have_stfle()))
		return 0;

	if (SLJIT2_UNLIKELY(cpu_features.bits[0] == 0)) {
		__asm__ __volatile__ (
			"lgr   %%r0, %0;"
			"stfle 0(%1);"
			/* outputs  */:
			/* inputs   */: "d" ((size / 8) - 1), "a" (&cpu_features)
			/* clobbers */: "r0", "cc", "memory"
		);
		SLJIT2_ASSERT(cpu_features.bits[0] != 0);
	}
	return (cpu_features.bits[word_index] & bit_index) != 0;
#else
	return 0;
#endif
}

#define HAVE_FACILITY(name, bit) \
static SLJIT2_INLINE int name() \
{ \
	static int have = -1; \
	/* Static check first. May allow the function to be optimized away. */ \
	if (have_facility_static(bit)) \
		have = 1; \
	else if (SLJIT2_UNLIKELY(have < 0)) \
		have = have_facility_dynamic(bit) ? 1 : 0; \
\
	return have; \
}

HAVE_FACILITY(have_eimm,    EXTENDED_IMMEDIATE_FACILITY)
HAVE_FACILITY(have_ldisp,   FAST_LONG_DISPLACEMENT_FACILITY)
HAVE_FACILITY(have_genext,  GENERAL_INSTRUCTION_EXTENSION_FACILITY)
HAVE_FACILITY(have_lscond1, LOAD_STORE_ON_CONDITION_1_FACILITY)
HAVE_FACILITY(have_lscond2, LOAD_STORE_ON_CONDITION_2_FACILITY)
HAVE_FACILITY(have_misc2,   MISCELLANEOUS_INSTRUCTION_EXTENSIONS_2_FACILITY)
#undef HAVE_FACILITY

#define is_u12(d)	(0 <= (d) && (d) <= 0x00000fffL)
#define is_u32(d)	(0 <= (d) && (d) <= 0xffffffffL)

#define CHECK_SIGNED(v, bitlen) \
	((v) >= -(1 << ((bitlen) - 1)) && (v) < (1 << ((bitlen) - 1)))

#define is_s8(d)	CHECK_SIGNED((d), 8)
#define is_s16(d)	CHECK_SIGNED((d), 16)
#define is_s20(d)	CHECK_SIGNED((d), 20)
#define is_s32(d)	((d) == (sljit2_s32)(d))

static SLJIT2_INLINE sljit2_ins disp_s20(sljit2_s32 d)
{
	sljit2_uw dh, dl;

	SLJIT2_ASSERT(is_s20(d));

	dh = (d >> 12) & 0xff;
	dl = ((sljit2_uw)d << 8) & 0xfff00;
	return (dh | dl) << 8;
}

/* TODO(carenas): variadic macro is not strictly needed */
#define SLJIT2_S390X_INSTRUCTION(op, ...) \
static SLJIT2_INLINE sljit2_ins op(__VA_ARGS__)

/* RR form instructions. */
#define SLJIT2_S390X_RR(name, pattern) \
SLJIT2_S390X_INSTRUCTION(name, sljit2_gpr dst, sljit2_gpr src) \
{ \
	return (pattern) | ((dst & 0xf) << 4) | (src & 0xf); \
}

/* AND */
SLJIT2_S390X_RR(nr,   0x1400)

/* BRANCH AND SAVE */
SLJIT2_S390X_RR(basr, 0x0d00)

/* BRANCH ON CONDITION */
SLJIT2_S390X_RR(bcr,  0x0700) /* TODO(mundaym): type for mask? */

/* DIVIDE */
SLJIT2_S390X_RR(dr,   0x1d00)

/* EXCLUSIVE OR */
SLJIT2_S390X_RR(xr,   0x1700)

/* LOAD */
SLJIT2_S390X_RR(lr,   0x1800)

/* LOAD COMPLEMENT */
SLJIT2_S390X_RR(lcr,  0x1300)

/* OR */
SLJIT2_S390X_RR(or,   0x1600)

#undef SLJIT2_S390X_RR

/* RRE form instructions */
#define SLJIT2_S390X_RRE(name, pattern) \
SLJIT2_S390X_INSTRUCTION(name, sljit2_gpr dst, sljit2_gpr src) \
{ \
	return (pattern) | R4A(dst) | R0A(src); \
}

/* AND */
SLJIT2_S390X_RRE(ngr,   0xb9800000)

/* DIVIDE LOGICAL */
SLJIT2_S390X_RRE(dlr,   0xb9970000)
SLJIT2_S390X_RRE(dlgr,  0xb9870000)

/* DIVIDE SINGLE */
SLJIT2_S390X_RRE(dsgr,  0xb90d0000)

/* EXCLUSIVE OR */
SLJIT2_S390X_RRE(xgr,   0xb9820000)

/* LOAD */
SLJIT2_S390X_RRE(lgr,   0xb9040000)
SLJIT2_S390X_RRE(lgfr,  0xb9140000)

/* LOAD BYTE */
SLJIT2_S390X_RRE(lbr,   0xb9260000)
SLJIT2_S390X_RRE(lgbr,  0xb9060000)

/* LOAD COMPLEMENT */
SLJIT2_S390X_RRE(lcgr,  0xb9030000)

/* LOAD HALFWORD */
SLJIT2_S390X_RRE(lhr,   0xb9270000)
SLJIT2_S390X_RRE(lghr,  0xb9070000)

/* LOAD LOGICAL */
SLJIT2_S390X_RRE(llgfr, 0xb9160000)

/* LOAD LOGICAL CHARACTER */
SLJIT2_S390X_RRE(llcr,  0xb9940000)
SLJIT2_S390X_RRE(llgcr, 0xb9840000)

/* LOAD LOGICAL HALFWORD */
SLJIT2_S390X_RRE(llhr,  0xb9950000)
SLJIT2_S390X_RRE(llghr, 0xb9850000)

/* MULTIPLY LOGICAL */
SLJIT2_S390X_RRE(mlgr,  0xb9860000)

/* MULTIPLY SINGLE */
SLJIT2_S390X_RRE(msgfr, 0xb91c0000)

/* OR */
SLJIT2_S390X_RRE(ogr,   0xb9810000)

/* SUBTRACT */
SLJIT2_S390X_RRE(sgr,   0xb9090000)

#undef SLJIT2_S390X_RRE

/* RI-a form instructions */
#define SLJIT2_S390X_RIA(name, pattern, imm_type) \
SLJIT2_S390X_INSTRUCTION(name, sljit2_gpr reg, imm_type imm) \
{ \
	return (pattern) | R20A(reg) | (imm & 0xffff); \
}

/* ADD HALFWORD IMMEDIATE */
SLJIT2_S390X_RIA(aghi,  0xa70b0000, sljit2_s16)

/* LOAD HALFWORD IMMEDIATE */
SLJIT2_S390X_RIA(lhi,   0xa7080000, sljit2_s16)
SLJIT2_S390X_RIA(lghi,  0xa7090000, sljit2_s16)

/* LOAD LOGICAL IMMEDIATE */
SLJIT2_S390X_RIA(llihh, 0xa50c0000, sljit2_u16)
SLJIT2_S390X_RIA(llihl, 0xa50d0000, sljit2_u16)
SLJIT2_S390X_RIA(llilh, 0xa50e0000, sljit2_u16)
SLJIT2_S390X_RIA(llill, 0xa50f0000, sljit2_u16)

/* MULTIPLY HALFWORD IMMEDIATE */
SLJIT2_S390X_RIA(mhi,   0xa70c0000, sljit2_s16)
SLJIT2_S390X_RIA(mghi,  0xa70d0000, sljit2_s16)

/* OR IMMEDIATE */
SLJIT2_S390X_RIA(oilh,  0xa50a0000, sljit2_u16)

#undef SLJIT2_S390X_RIA

/* RIL-a form instructions (requires extended immediate facility) */
#define SLJIT2_S390X_RILA(name, pattern, imm_type) \
SLJIT2_S390X_INSTRUCTION(name, sljit2_gpr reg, imm_type imm) \
{ \
	SLJIT2_ASSERT(have_eimm()); \
	return (pattern) | R36A(reg) | ((sljit2_ins)imm & 0xffffffffu); \
}

/* ADD IMMEDIATE */
SLJIT2_S390X_RILA(agfi,  0xc20800000000, sljit2_s32)

/* ADD IMMEDIATE HIGH */
SLJIT2_S390X_RILA(aih,   0xcc0800000000, sljit2_s32) /* TODO(mundaym): high-word facility? */

/* AND IMMEDIATE */
SLJIT2_S390X_RILA(nihf,  0xc00a00000000, sljit2_u32)

/* EXCLUSIVE OR IMMEDIATE */
SLJIT2_S390X_RILA(xilf,  0xc00700000000, sljit2_u32)

/* INSERT IMMEDIATE */
SLJIT2_S390X_RILA(iihf,  0xc00800000000, sljit2_u32)
SLJIT2_S390X_RILA(iilf,  0xc00900000000, sljit2_u32)

/* LOAD IMMEDIATE */
SLJIT2_S390X_RILA(lgfi,  0xc00100000000, sljit2_s32)

/* LOAD LOGICAL IMMEDIATE */
SLJIT2_S390X_RILA(llihf, 0xc00e00000000, sljit2_u32)
SLJIT2_S390X_RILA(llilf, 0xc00f00000000, sljit2_u32)

/* SUBTRACT LOGICAL IMMEDIATE */
SLJIT2_S390X_RILA(slfi,  0xc20500000000, sljit2_u32)

#undef SLJIT2_S390X_RILA

/* RX-a form instructions */
#define SLJIT2_S390X_RXA(name, pattern) \
SLJIT2_S390X_INSTRUCTION(name, sljit2_gpr r, sljit2_s32 d, sljit2_gpr x, sljit2_gpr b) \
{ \
	SLJIT2_ASSERT((d & 0xfff) == d); \
\
	return (pattern) | R20A(r) | R16A(x) | R12A(b) | (sljit2_ins)(d & 0xfff); \
}

/* LOAD */
SLJIT2_S390X_RXA(l,   0x58000000)

/* LOAD ADDRESS */
SLJIT2_S390X_RXA(la,  0x41000000)

/* LOAD HALFWORD */
SLJIT2_S390X_RXA(lh,  0x48000000)

/* MULTIPLY SINGLE */
SLJIT2_S390X_RXA(ms,  0x71000000)

/* STORE */
SLJIT2_S390X_RXA(st,  0x50000000)

/* STORE CHARACTER */
SLJIT2_S390X_RXA(stc, 0x42000000)

/* STORE HALFWORD */
SLJIT2_S390X_RXA(sth, 0x40000000)

#undef SLJIT2_S390X_RXA

/* RXY-a instructions */
#define SLJIT2_S390X_RXYA(name, pattern, cond) \
SLJIT2_S390X_INSTRUCTION(name, sljit2_gpr r, sljit2_s32 d, sljit2_gpr x, sljit2_gpr b) \
{ \
	SLJIT2_ASSERT(cond); \
\
	return (pattern) | R36A(r) | R32A(x) | R28A(b) | disp_s20(d); \
}

/* LOAD */
SLJIT2_S390X_RXYA(ly,    0xe30000000058, have_ldisp())
SLJIT2_S390X_RXYA(lg,    0xe30000000004, 1)
SLJIT2_S390X_RXYA(lgf,   0xe30000000014, 1)

/* LOAD BYTE */
SLJIT2_S390X_RXYA(lb,    0xe30000000076, have_ldisp())
SLJIT2_S390X_RXYA(lgb,   0xe30000000077, have_ldisp())

/* LOAD HALFWORD */
SLJIT2_S390X_RXYA(lhy,   0xe30000000078, have_ldisp())
SLJIT2_S390X_RXYA(lgh,   0xe30000000015, 1)

/* LOAD LOGICAL */
SLJIT2_S390X_RXYA(llgf,  0xe30000000016, 1)

/* LOAD LOGICAL CHARACTER */
SLJIT2_S390X_RXYA(llc,   0xe30000000094, have_eimm())
SLJIT2_S390X_RXYA(llgc,  0xe30000000090, 1)

/* LOAD LOGICAL HALFWORD */
SLJIT2_S390X_RXYA(llh,   0xe30000000095, have_eimm())
SLJIT2_S390X_RXYA(llgh,  0xe30000000091, 1)

/* MULTIPLY SINGLE */
SLJIT2_S390X_RXYA(msy,   0xe30000000051, have_ldisp())
SLJIT2_S390X_RXYA(msg,   0xe3000000000c, 1)

/* STORE */
SLJIT2_S390X_RXYA(sty,   0xe30000000050, have_ldisp())
SLJIT2_S390X_RXYA(stg,   0xe30000000024, 1)

/* STORE CHARACTER */
SLJIT2_S390X_RXYA(stcy,  0xe30000000072, have_ldisp())

/* STORE HALFWORD */
SLJIT2_S390X_RXYA(sthy,  0xe30000000070, have_ldisp())

#undef SLJIT2_S390X_RXYA

/* RSY-a instructions */
#define SLJIT2_S390X_RSYA(name, pattern, cond) \
SLJIT2_S390X_INSTRUCTION(name, sljit2_gpr dst, sljit2_gpr src, sljit2_s32 d, sljit2_gpr b) \
{ \
	SLJIT2_ASSERT(cond); \
\
	return (pattern) | R36A(dst) | R32A(src) | R28A(b) | disp_s20(d); \
}

/* LOAD MULTIPLE */
SLJIT2_S390X_RSYA(lmg,   0xeb0000000004, 1)

/* SHIFT LEFT LOGICAL */
SLJIT2_S390X_RSYA(sllg,  0xeb000000000d, 1)

/* SHIFT RIGHT SINGLE */
SLJIT2_S390X_RSYA(srag,  0xeb000000000a, 1)

/* STORE MULTIPLE */
SLJIT2_S390X_RSYA(stmg,  0xeb0000000024, 1)

#undef SLJIT2_S390X_RSYA

/* RIE-f instructions (require general-instructions-extension facility) */
#define SLJIT2_S390X_RIEF(name, pattern) \
SLJIT2_S390X_INSTRUCTION(name, sljit2_gpr dst, sljit2_gpr src, sljit2_u8 start, sljit2_u8 end, sljit2_u8 rot) \
{ \
	sljit2_ins i3, i4, i5; \
\
	SLJIT2_ASSERT(have_genext()); \
	i3 = (sljit2_ins)start << 24; \
	i4 = (sljit2_ins)end << 16; \
	i5 = (sljit2_ins)rot << 8; \
\
	return (pattern) | R36A(dst & 0xf) | R32A(src & 0xf) | i3 | i4 | i5; \
}

/* ROTATE THEN AND SELECTED BITS */
/* SLJIT2_S390X_RIEF(rnsbg,  0xec0000000054) */

/* ROTATE THEN EXCLUSIVE OR SELECTED BITS */
/* SLJIT2_S390X_RIEF(rxsbg,  0xec0000000057) */

/* ROTATE THEN OR SELECTED BITS */
SLJIT2_S390X_RIEF(rosbg,  0xec0000000056)

/* ROTATE THEN INSERT SELECTED BITS */
/* SLJIT2_S390X_RIEF(risbg,  0xec0000000055) */
/* SLJIT2_S390X_RIEF(risbgn, 0xec0000000059) */

/* ROTATE THEN INSERT SELECTED BITS HIGH */
SLJIT2_S390X_RIEF(risbhg, 0xec000000005d)

/* ROTATE THEN INSERT SELECTED BITS LOW */
/* SLJIT2_S390X_RIEF(risblg, 0xec0000000051) */

#undef SLJIT2_S390X_RIEF

/* RRF-c instructions (require load/store-on-condition 1 facility) */
#define SLJIT2_S390X_RRFC(name, pattern) \
SLJIT2_S390X_INSTRUCTION(name, sljit2_gpr dst, sljit2_gpr src, sljit2_uw mask) \
{ \
	sljit2_ins m3; \
\
	SLJIT2_ASSERT(have_lscond1()); \
	m3 = (sljit2_ins)(mask & 0xf) << 12; \
\
	return (pattern) | m3 | R4A(dst) | R0A(src); \
}

/* LOAD HALFWORD IMMEDIATE ON CONDITION */
SLJIT2_S390X_RRFC(locr,  0xb9f20000)
SLJIT2_S390X_RRFC(locgr, 0xb9e20000)

#undef SLJIT2_S390X_RRFC

/* RIE-g instructions (require load/store-on-condition 2 facility) */
#define SLJIT2_S390X_RIEG(name, pattern) \
SLJIT2_S390X_INSTRUCTION(name, sljit2_gpr reg, sljit2_sw imm, sljit2_uw mask) \
{ \
	sljit2_ins m3, i2; \
\
	SLJIT2_ASSERT(have_lscond2()); \
	m3 = (sljit2_ins)(mask & 0xf) << 32; \
	i2 = (sljit2_ins)(imm & 0xffffL) << 16; \
\
	return (pattern) | R36A(reg) | m3 | i2; \
}

/* LOAD HALFWORD IMMEDIATE ON CONDITION */
SLJIT2_S390X_RIEG(lochi,  0xec0000000042)
SLJIT2_S390X_RIEG(locghi, 0xec0000000046)

#undef SLJIT2_S390X_RIEG

#define SLJIT2_S390X_RILB(name, pattern, cond) \
SLJIT2_S390X_INSTRUCTION(name, sljit2_gpr reg, sljit2_sw ri) \
{ \
	SLJIT2_ASSERT(cond); \
\
	return (pattern) | R36A(reg) | (sljit2_ins)(ri & 0xffffffff); \
}

/* BRANCH RELATIVE AND SAVE LONG */
SLJIT2_S390X_RILB(brasl, 0xc00500000000, 1)

/* LOAD ADDRESS RELATIVE LONG */
SLJIT2_S390X_RILB(larl,  0xc00000000000, 1)

/* LOAD RELATIVE LONG */
SLJIT2_S390X_RILB(lgrl,  0xc40800000000, have_genext())

#undef SLJIT2_S390X_RILB

SLJIT2_S390X_INSTRUCTION(br, sljit2_gpr target)
{
	return 0x07f0 | target;
}

SLJIT2_S390X_INSTRUCTION(brc, sljit2_uw mask, sljit2_sw target)
{
	sljit2_ins m1 = (sljit2_ins)(mask & 0xf) << 20;
	sljit2_ins ri2 = (sljit2_ins)target & 0xffff;
	return 0xa7040000L | m1 | ri2;
}

SLJIT2_S390X_INSTRUCTION(brcl, sljit2_uw mask, sljit2_sw target)
{
	sljit2_ins m1 = (sljit2_ins)(mask & 0xf) << 36;
	sljit2_ins ri2 = (sljit2_ins)target & 0xffffffff;
	return 0xc00400000000L | m1 | ri2;
}

SLJIT2_S390X_INSTRUCTION(flogr, sljit2_gpr dst, sljit2_gpr src)
{
	SLJIT2_ASSERT(have_eimm());
	return 0xb9830000 | R8A(dst) | R0A(src);
}

/* INSERT PROGRAM MASK */
SLJIT2_S390X_INSTRUCTION(ipm, sljit2_gpr dst)
{
	return 0xb2220000 | R4A(dst);
}

/* SET PROGRAM MASK */
SLJIT2_S390X_INSTRUCTION(spm, sljit2_gpr dst)
{
	return 0x0400 | R4A(dst);
}

/* ROTATE THEN INSERT SELECTED BITS HIGH (ZERO) */
SLJIT2_S390X_INSTRUCTION(risbhgz, sljit2_gpr dst, sljit2_gpr src, sljit2_u8 start, sljit2_u8 end, sljit2_u8 rot)
{
	return risbhg(dst, src, start, 0x8 | end, rot);
}

#undef SLJIT2_S390X_INSTRUCTION

static sljit2_s32 update_zero_overflow(struct sljit2_compiler *compiler, sljit2_s32 op, sljit2_gpr dst_r)
{
	/* Condition codes: bits 18 and 19.
	   Transformation:
	     0 (zero and no overflow) : unchanged
	     1 (non-zero and no overflow) : unchanged
	     2 (zero and overflow) : decreased by 1
	     3 (non-zero and overflow) : decreased by 1 if non-zero */
	FAIL_IF(push_inst(compiler, brc(0xc, 2 + 2 + ((op & SLJIT2_32) ? 1 : 2) + 2 + 3 + 1)));
	FAIL_IF(push_inst(compiler, ipm(tmp1)));
	FAIL_IF(push_inst(compiler, (op & SLJIT2_32) ? or(dst_r, dst_r) : ogr(dst_r, dst_r)));
	FAIL_IF(push_inst(compiler, brc(0x8, 2 + 3)));
	FAIL_IF(push_inst(compiler, slfi(tmp1, 0x10000000)));
	FAIL_IF(push_inst(compiler, spm(tmp1)));
	return SLJIT2_SUCCESS;
}

/* load 64-bit immediate into register without clobbering flags */
static sljit2_s32 push_load_imm_inst(struct sljit2_compiler *compiler, sljit2_gpr target, sljit2_sw v)
{
	/* 4 byte instructions */
	if (is_s16(v))
		return push_inst(compiler, lghi(target, (sljit2_s16)v));

	if (((sljit2_uw)v & ~(sljit2_uw)0x000000000000ffff) == 0)
		return push_inst(compiler, llill(target, (sljit2_u16)v));

	if (((sljit2_uw)v & ~(sljit2_uw)0x00000000ffff0000) == 0)
		return push_inst(compiler, llilh(target, (sljit2_u16)(v >> 16)));

	if (((sljit2_uw)v & ~(sljit2_uw)0x0000ffff00000000) == 0)
		return push_inst(compiler, llihl(target, (sljit2_u16)(v >> 32)));

	if (((sljit2_uw)v & ~(sljit2_uw)0xffff000000000000) == 0)
		return push_inst(compiler, llihh(target, (sljit2_u16)(v >> 48)));

	if (is_s32(v))
		return push_inst(compiler, lgfi(target, (sljit2_s32)v));

	if (((sljit2_uw)v >> 32) == 0)
		return push_inst(compiler, llilf(target, (sljit2_u32)v));

	if (((sljit2_uw)v << 32) == 0)
		return push_inst(compiler, llihf(target, (sljit2_u32)((sljit2_uw)v >> 32)));

	FAIL_IF(push_inst(compiler, llilf(target, (sljit2_u32)v)));
	return push_inst(compiler, iihf(target, (sljit2_u32)(v >> 32)));
}

struct addr {
	sljit2_gpr base;
	sljit2_gpr index;
	sljit2_s32 offset;
};

/* transform memory operand into D(X,B) form with a signed 20-bit offset */
static sljit2_s32 make_addr_bxy(struct sljit2_compiler *compiler,
	struct addr *addr, sljit2_s32 mem, sljit2_sw off,
	sljit2_gpr tmp /* clobbered, must not be r0 */)
{
	sljit2_gpr base = r0;
	sljit2_gpr index = r0;

	SLJIT2_ASSERT(tmp != r0);
	if (mem & REG_MASK)
		base = gpr(mem & REG_MASK);

	if (mem & OFFS_REG_MASK) {
		index = gpr(OFFS_REG(mem));
		if (off != 0) {
			/* shift and put the result into tmp */
			SLJIT2_ASSERT(0 <= off && off < 64);
			FAIL_IF(push_inst(compiler, sllg(tmp, index, (sljit2_s32)off, 0)));
			index = tmp;
			off = 0; /* clear offset */
		}
	}
	else if (!is_s20(off)) {
		FAIL_IF(push_load_imm_inst(compiler, tmp, off));
		index = tmp;
		off = 0; /* clear offset */
	}
	addr->base = base;
	addr->index = index;
	addr->offset = (sljit2_s32)off;
	return SLJIT2_SUCCESS;
}

/* transform memory operand into D(X,B) form with an unsigned 12-bit offset */
static sljit2_s32 make_addr_bx(struct sljit2_compiler *compiler,
	struct addr *addr, sljit2_s32 mem, sljit2_sw off,
	sljit2_gpr tmp /* clobbered, must not be r0 */)
{
	sljit2_gpr base = r0;
	sljit2_gpr index = r0;

	SLJIT2_ASSERT(tmp != r0);
	if (mem & REG_MASK)
		base = gpr(mem & REG_MASK);

	if (mem & OFFS_REG_MASK) {
		index = gpr(OFFS_REG(mem));
		if (off != 0) {
			/* shift and put the result into tmp */
			SLJIT2_ASSERT(0 <= off && off < 64);
			FAIL_IF(push_inst(compiler, sllg(tmp, index, (sljit2_s32)off, 0)));
			index = tmp;
			off = 0; /* clear offset */
		}
	}
	else if (!is_u12(off)) {
		FAIL_IF(push_load_imm_inst(compiler, tmp, off));
		index = tmp;
		off = 0; /* clear offset */
	}
	addr->base = base;
	addr->index = index;
	addr->offset = (sljit2_s32)off;
	return SLJIT2_SUCCESS;
}

#define EVAL(op, r, addr) op(r, addr.offset, addr.index, addr.base)
#define WHEN(cond, r, i1, i2, addr) \
	(cond) ? EVAL(i1, r, addr) : EVAL(i2, r, addr)

/* May clobber tmp1. */
static sljit2_s32 load_store_op(struct sljit2_compiler *compiler, sljit2_gpr reg,
		sljit2_s32 mem, sljit2_sw memw,
		sljit2_s32 is_32bit, const sljit2_ins* forms)
{
	struct addr addr;

	SLJIT2_ASSERT(mem & SLJIT2_MEM);

	if (is_32bit && ((mem & OFFS_REG_MASK) || is_u12(memw) || !is_s20(memw))) {
		FAIL_IF(make_addr_bx(compiler, &addr, mem, memw, tmp1));
		return push_inst(compiler, forms[0] | R20A(reg) | R16A(addr.index) | R12A(addr.base) | (sljit2_ins)addr.offset);
	}

	FAIL_IF(make_addr_bxy(compiler, &addr, mem, memw, tmp1));
	return push_inst(compiler, (is_32bit ? forms[1] : forms[2]) | R36A(reg) | R32A(addr.index) | R28A(addr.base) | disp_s20(addr.offset));
}

static const sljit2_ins load_forms[3] = {
	0x58000000 /* l */,
	0xe30000000058 /* ly */,
	0xe30000000004 /* lg */
};

static const sljit2_ins store_forms[3] = {
	0x50000000 /* st */,
	0xe30000000050 /* sty */,
	0xe30000000024 /* stg */
};

static const sljit2_ins load_halfword_forms[3] = {
	0x48000000 /* lh */,
	0xe30000000078 /* lhy */,
	0xe30000000015 /* lgh */
};

/* May clobber tmp1. */
static SLJIT2_INLINE sljit2_s32 load_word(struct sljit2_compiler *compiler, sljit2_gpr dst_r,
		sljit2_s32 src, sljit2_sw srcw,
		sljit2_s32 is_32bit)
{
	return load_store_op(compiler, dst_r, src, srcw, is_32bit, load_forms);
}

/* May clobber tmp1. */
static sljit2_s32 load_unsigned_word(struct sljit2_compiler *compiler, sljit2_gpr dst_r,
		sljit2_s32 src, sljit2_sw srcw,
		sljit2_s32 is_32bit)
{
	struct addr addr;
	sljit2_ins ins;

	SLJIT2_ASSERT(src & SLJIT2_MEM);

	FAIL_IF(make_addr_bxy(compiler, &addr, src, srcw, tmp1));

	ins = is_32bit ? 0xe30000000016 /* llgf */ : 0xe30000000004 /* lg */;
	return push_inst(compiler, ins | R36A(dst_r) | R32A(addr.index) | R28A(addr.base) | disp_s20(addr.offset));
}

/* May clobber tmp1. */
static SLJIT2_INLINE sljit2_s32 store_word(struct sljit2_compiler *compiler, sljit2_gpr src_r,
		sljit2_s32 dst, sljit2_sw dstw,
		sljit2_s32 is_32bit)
{
	return load_store_op(compiler, src_r, dst, dstw, is_32bit, store_forms);
}

#undef WHEN

static sljit2_s32 emit_move(struct sljit2_compiler *compiler,
	sljit2_gpr dst_r,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_gpr src_r;

	SLJIT2_ASSERT(!IS_GPR_REG(src) || dst_r != gpr(src & REG_MASK));

	if (src == SLJIT2_IMM)
		return push_load_imm_inst(compiler, dst_r, srcw);

	if (src & SLJIT2_MEM)
		return load_word(compiler, dst_r, src, srcw, (compiler->mode & SLJIT2_32) != 0);

	src_r = gpr(src & REG_MASK);
	return push_inst(compiler, (compiler->mode & SLJIT2_32) ? lr(dst_r, src_r) : lgr(dst_r, src_r));
}

static sljit2_s32 emit_rr(struct sljit2_compiler *compiler, sljit2_ins ins,
	sljit2_s32 dst,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_gpr dst_r = tmp0;
	sljit2_gpr src_r = tmp1;
	sljit2_s32 needs_move = 1;

	if (FAST_IS_REG(dst)) {
		dst_r = gpr(dst);

		if (dst == src1)
			needs_move = 0;
		else if (dst == src2) {
			dst_r = tmp0;
			needs_move = 2;
		}
	}

	if (needs_move)
		FAIL_IF(emit_move(compiler, dst_r, src1, src1w));

	if (FAST_IS_REG(src2))
		src_r = gpr(src2);
	else
		FAIL_IF(emit_move(compiler, tmp1, src2, src2w));

	FAIL_IF(push_inst(compiler, ins | R4A(dst_r) | R0A(src_r)));

	if (needs_move != 2)
		return SLJIT2_SUCCESS;

	dst_r = gpr(dst & REG_MASK);
	return push_inst(compiler, (compiler->mode & SLJIT2_32) ? lr(dst_r, tmp0) : lgr(dst_r, tmp0));
}

static sljit2_s32 emit_rr1(struct sljit2_compiler *compiler, sljit2_ins ins,
	sljit2_s32 dst,
	sljit2_s32 src1, sljit2_sw src1w)
{
	sljit2_gpr dst_r = FAST_IS_REG(dst) ? gpr(dst) : tmp0;
	sljit2_gpr src_r = tmp1;

	if (FAST_IS_REG(src1))
		src_r = gpr(src1);
	else
		FAIL_IF(emit_move(compiler, tmp1, src1, src1w));

	return push_inst(compiler, ins | R4A(dst_r) | R0A(src_r));
}

static sljit2_s32 emit_rrf(struct sljit2_compiler *compiler, sljit2_ins ins,
	sljit2_s32 dst,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_gpr dst_r = FAST_IS_REG(dst) ? gpr(dst & REG_MASK) : tmp0;
	sljit2_gpr src1_r = tmp0;
	sljit2_gpr src2_r = tmp1;

	if (FAST_IS_REG(src1))
		src1_r = gpr(src1);
	else
		FAIL_IF(emit_move(compiler, tmp0, src1, src1w));

	if (FAST_IS_REG(src2))
		src2_r = gpr(src2);
	else
		FAIL_IF(emit_move(compiler, tmp1, src2, src2w));

	return push_inst(compiler, ins | R4A(dst_r) | R0A(src1_r) | R12A(src2_r));
}

typedef enum {
	RI_A,
	RIL_A,
} emit_ril_type;

static sljit2_s32 emit_ri(struct sljit2_compiler *compiler, sljit2_ins ins,
	sljit2_s32 dst,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_sw src2w,
	emit_ril_type type)
{
	sljit2_gpr dst_r = tmp0;
	sljit2_s32 needs_move = 1;

	if (FAST_IS_REG(dst)) {
		dst_r = gpr(dst);

		if (dst == src1)
			needs_move = 0;
	}

	if (needs_move)
		FAIL_IF(emit_move(compiler, dst_r, src1, src1w));

	if (type == RIL_A)
		return push_inst(compiler, ins | R36A(dst_r) | (src2w & 0xffffffff));
	return push_inst(compiler, ins | R20A(dst_r) | (src2w & 0xffff));
}

static sljit2_s32 emit_rie_d(struct sljit2_compiler *compiler, sljit2_ins ins,
	sljit2_s32 dst,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_sw src2w)
{
	sljit2_gpr dst_r = FAST_IS_REG(dst) ? gpr(dst) : tmp0;
	sljit2_gpr src_r = tmp0;

	if (!FAST_IS_REG(src1))
		FAIL_IF(emit_move(compiler, tmp0, src1, src1w));
	else
		src_r = gpr(src1 & REG_MASK);

	return push_inst(compiler, ins | R36A(dst_r) | R32A(src_r) | (sljit2_ins)(src2w & 0xffff) << 16);
}

typedef enum {
	RX_A,
	RXY_A,
} emit_rx_type;

static sljit2_s32 emit_rx(struct sljit2_compiler *compiler, sljit2_ins ins,
	sljit2_s32 dst,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w,
	emit_rx_type type)
{
	sljit2_gpr dst_r = tmp0;
	sljit2_s32 needs_move = 1;
	sljit2_gpr base, index;

	SLJIT2_ASSERT(src2 & SLJIT2_MEM);

	if (FAST_IS_REG(dst)) {
		dst_r = gpr(dst);

		if (dst == src1)
			needs_move = 0;
		else if (dst == (src2 & REG_MASK) || (dst == OFFS_REG(src2))) {
			dst_r = tmp0;
			needs_move = 2;
		}
	}

	if (needs_move)
		FAIL_IF(emit_move(compiler, dst_r, src1, src1w));

	base = gpr(src2 & REG_MASK);
	index = tmp0;

	if (src2 & OFFS_REG_MASK) {
		index = gpr(OFFS_REG(src2));

		if (src2w != 0) {
			FAIL_IF(push_inst(compiler, sllg(tmp1, index, src2w & 0x3, 0)));
			src2w = 0;
			index = tmp1;
		}
	} else if ((type == RX_A && !is_u12(src2w)) || (type == RXY_A && !is_s20(src2w))) {
		FAIL_IF(push_load_imm_inst(compiler, tmp1, src2w));

		if (src2 & REG_MASK)
			index = tmp1;
		else
			base = tmp1;
		src2w = 0;
	}

	if (type == RX_A)
		ins |= R20A(dst_r) | R16A(index) | R12A(base) | (sljit2_ins)src2w;
	else
		ins |= R36A(dst_r) | R32A(index) | R28A(base) | disp_s20((sljit2_s32)src2w);

	FAIL_IF(push_inst(compiler, ins));

	if (needs_move != 2)
		return SLJIT2_SUCCESS;

	dst_r = gpr(dst);
	return push_inst(compiler, (compiler->mode & SLJIT2_32) ? lr(dst_r, tmp0) : lgr(dst_r, tmp0));
}

static sljit2_s32 emit_siy(struct sljit2_compiler *compiler, sljit2_ins ins,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_sw srcw)
{
	sljit2_gpr dst_r = tmp1;

	SLJIT2_ASSERT(dst & SLJIT2_MEM);

	if (dst & OFFS_REG_MASK) {
		sljit2_gpr index = tmp1;

		if ((dstw & 0x3) == 0)
			index = gpr(OFFS_REG(dst));
		else
			FAIL_IF(push_inst(compiler, sllg(tmp1, index, dstw & 0x3, 0)));

		FAIL_IF(push_inst(compiler, la(tmp1, 0, dst_r, index)));
		dstw = 0;
	}
	else if (!is_s20(dstw)) {
		FAIL_IF(push_load_imm_inst(compiler, tmp1, dstw));

		if (dst & REG_MASK)
			FAIL_IF(push_inst(compiler, la(tmp1, 0, dst_r, tmp1)));

		dstw = 0;
	}
	else
		dst_r = gpr(dst & REG_MASK);

	return push_inst(compiler, ins | ((sljit2_ins)(srcw & 0xff) << 32) | R28A(dst_r) | disp_s20((sljit2_s32)dstw));
}

struct ins_forms {
	sljit2_ins op_r;
	sljit2_ins op_gr;
	sljit2_ins op_rk;
	sljit2_ins op_grk;
	sljit2_ins op;
	sljit2_ins op_y;
	sljit2_ins op_g;
};

static sljit2_s32 emit_commutative(struct sljit2_compiler *compiler, const struct ins_forms *forms,
	sljit2_s32 dst,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_s32 mode = compiler->mode;
	sljit2_ins ins, ins_k;

	if ((src1 | src2) & SLJIT2_MEM) {
		sljit2_ins ins12, ins20;

		if (mode & SLJIT2_32) {
			ins12 = forms->op;
			ins20 = forms->op_y;
		}
		else {
			ins12 = 0;
			ins20 = forms->op_g;
		}

		if (ins12 && ins20) {
			/* Extra instructions needed for address computation can be executed independently. */
			if ((src2 & SLJIT2_MEM) && (!(src1 & SLJIT2_MEM)
					|| ((src1 & OFFS_REG_MASK) ? (src1w & 0x3) == 0 : is_s20(src1w)))) {
				if ((src2 & OFFS_REG_MASK) || is_u12(src2w) || !is_s20(src2w))
					return emit_rx(compiler, ins12, dst, src1, src1w, src2, src2w, RX_A);

				return emit_rx(compiler, ins20, dst, src1, src1w, src2, src2w, RXY_A);
			}

			if (src1 & SLJIT2_MEM) {
				if ((src1 & OFFS_REG_MASK) || is_u12(src1w) || !is_s20(src1w))
					return emit_rx(compiler, ins12, dst, src2, src2w, src1, src1w, RX_A);

				return emit_rx(compiler, ins20, dst, src2, src2w, src1, src1w, RXY_A);
			}
		}
		else if (ins12 || ins20) {
			emit_rx_type rx_type;

			if (ins12) {
				rx_type = RX_A;
				ins = ins12;
			}
			else {
				rx_type = RXY_A;
				ins = ins20;
			}

			if ((src2 & SLJIT2_MEM) && (!(src1 & SLJIT2_MEM)
					|| ((src1 & OFFS_REG_MASK) ? (src1w & 0x3) == 0 : (rx_type == RX_A ? is_u12(src1w) : is_s20(src1w)))))
				return emit_rx(compiler, ins, dst, src1, src1w, src2, src2w, rx_type);

			if (src1 & SLJIT2_MEM)
				return emit_rx(compiler, ins, dst, src2, src2w, src1, src1w, rx_type);
		}
	}

	if (mode & SLJIT2_32) {
		ins = forms->op_r;
		ins_k = forms->op_rk;
	}
	else {
		ins = forms->op_gr;
		ins_k = forms->op_grk;
	}

	SLJIT2_ASSERT(ins != 0 || ins_k != 0);

	if (ins && FAST_IS_REG(dst)) {
		if (dst == src1)
			return emit_rr(compiler, ins, dst, src1, src1w, src2, src2w);

		if (dst == src2)
			return emit_rr(compiler, ins, dst, src2, src2w, src1, src1w);
	}

	if (ins_k == 0)
		return emit_rr(compiler, ins, dst, src1, src1w, src2, src2w);

	return emit_rrf(compiler, ins_k, dst, src1, src1w, src2, src2w);
}

static sljit2_s32 emit_non_commutative(struct sljit2_compiler *compiler, const struct ins_forms *forms,
	sljit2_s32 dst,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_s32 mode = compiler->mode;
	sljit2_ins ins;

	if (src2 & SLJIT2_MEM) {
		sljit2_ins ins12, ins20;

		if (mode & SLJIT2_32) {
			ins12 = forms->op;
			ins20 = forms->op_y;
		}
		else {
			ins12 = 0;
			ins20 = forms->op_g;
		}

		if (ins12 && ins20) {
			if ((src2 & OFFS_REG_MASK) || is_u12(src2w) || !is_s20(src2w))
				return emit_rx(compiler, ins12, dst, src1, src1w, src2, src2w, RX_A);

			return emit_rx(compiler, ins20, dst, src1, src1w, src2, src2w, RXY_A);
		}
		else if (ins12)
			return emit_rx(compiler, ins12, dst, src1, src1w, src2, src2w, RX_A);
		else if (ins20)
			return emit_rx(compiler, ins20, dst, src1, src1w, src2, src2w, RXY_A);
	}

	ins = (mode & SLJIT2_32) ? forms->op_rk : forms->op_grk;

	if (ins == 0 || (FAST_IS_REG(dst) && dst == src1))
		return emit_rr(compiler, (mode & SLJIT2_32) ? forms->op_r : forms->op_gr, dst, src1, src1w, src2, src2w);

	return emit_rrf(compiler, ins, dst, src1, src1w, src2, src2w);
}

SLJIT2_API_FUNC_ATTRIBUTE void* sljit2_generate_code(struct sljit2_compiler *compiler, sljit2_s32 options, void *exec_allocator_data)
{
	struct sljit2_label *label;
	struct sljit2_jump *jump;
	struct sljit2_const *const_;
	sljit2_sw executable_offset;
	sljit2_uw ins_size = compiler->size << 1;
	sljit2_uw pool_size = 0; /* literal pool */
	sljit2_uw pad_size;
	sljit2_uw half_count;
	SLJIT2_NEXT_DEFINE_TYPES;
	struct sljit2_memory_fragment *buf;
	sljit2_ins *buf_ptr;
	sljit2_ins *buf_end;
	sljit2_u16 *code;
	sljit2_u16 *code_ptr;
	sljit2_uw *pool, *pool_ptr;
	sljit2_ins ins;
	sljit2_sw source, offset;

	CHECK_ERROR_PTR();
	CHECK_PTR(check_sljit2_generate_code(compiler));
	reverse_buf(compiler);

	jump = compiler->jumps;
	while (jump != NULL) {
		if (jump->flags & (SLJIT2_REWRITABLE_JUMP | JUMP_ADDR | JUMP_MOV_ADDR)) {
			/* encoded: */
			/*   brasl %r14, <rel_addr> (or brcl <mask>, <rel_addr>) */
			/* replace with: */
			/*   lgrl %r1, <pool_addr> */
			/*   bras %r14, %r1 (or bcr <mask>, %r1) */
			pool_size += sizeof(*pool);
			if (!(jump->flags & JUMP_MOV_ADDR))
				ins_size += 2;
		}
		jump = jump->next;
	}

	const_ = compiler->consts;
	while (const_) {
		pool_size += sizeof(*pool);
		const_ = const_->next;
	}

	/* pad code size to 8 bytes so is accessible with half word offsets */
	/* the literal pool needs to be doubleword aligned */
	pad_size = ((ins_size + 7UL) & ~7UL) - ins_size;
	SLJIT2_ASSERT(pad_size < 8UL);

	/* allocate target buffer */
	code = (sljit2_u16*)allocate_executable_memory(ins_size + pad_size + pool_size, options, exec_allocator_data, &executable_offset);
	PTR_FAIL_WITH_EXEC_IF(code);
	code_ptr = code;

	/* TODO(carenas): pool is optional, and the ABI recommends it to
         *                be created before the function code, instead of
         *                globally; if generated code is too big could
         *                need offsets bigger than 32bit words and asser()
         */
	pool = (sljit2_uw *)((sljit2_uw)code + ins_size + pad_size);
	pool_ptr = pool;
	buf = compiler->buf;
	half_count = 0;

	label = compiler->labels;
	jump = compiler->jumps;
	const_ = compiler->consts;
	SLJIT2_NEXT_INIT_TYPES();
	SLJIT2_GET_NEXT_MIN();

	do {
		buf_ptr = (sljit2_ins*)buf->memory;
		buf_end = buf_ptr + (buf->used_size >> 3);
		do {
			ins = *buf_ptr++;

			if (next_min_addr == half_count) {
				SLJIT2_ASSERT(!label || label->size >= half_count);
				SLJIT2_ASSERT(!jump || jump->addr >= half_count);
				SLJIT2_ASSERT(!const_ || const_->addr >= half_count);

				if (next_min_addr == next_label_size) {
					label->u.addr = (sljit2_uw)SLJIT2_ADD_EXEC_OFFSET(code_ptr, executable_offset);
					label = label->next;
					next_label_size = SLJIT2_GET_NEXT_SIZE(label);
				}

				if (next_min_addr == next_jump_addr) {
					if (SLJIT2_UNLIKELY(jump->flags & JUMP_MOV_ADDR)) {
						source = (sljit2_sw)SLJIT2_ADD_EXEC_OFFSET(code_ptr, executable_offset);

						jump->addr = (sljit2_uw)pool_ptr;

						/* store target into pool */
						offset = (sljit2_sw)SLJIT2_ADD_EXEC_OFFSET(pool_ptr, executable_offset) - source;
						pool_ptr++;

						SLJIT2_ASSERT(!(offset & 1));
						offset >>= 1;
						SLJIT2_ASSERT(is_s32(offset));
						ins |= (sljit2_ins)offset & 0xffffffff;
					} else if (jump->flags & (SLJIT2_REWRITABLE_JUMP | JUMP_ADDR)) {
						sljit2_ins arg;

						jump->addr = (sljit2_uw)pool_ptr;

						/* load address into tmp1 */
						source = (sljit2_sw)SLJIT2_ADD_EXEC_OFFSET(code_ptr, executable_offset);
						offset = (sljit2_sw)SLJIT2_ADD_EXEC_OFFSET(pool_ptr, executable_offset) - source;

						SLJIT2_ASSERT(!(offset & 1));
						offset >>= 1;
						SLJIT2_ASSERT(is_s32(offset));

						code_ptr[0] = (sljit2_u16)(0xc408 | R4A(tmp1) /* lgrl */);
						code_ptr[1] = (sljit2_u16)(offset >> 16);
						code_ptr[2] = (sljit2_u16)offset;
						code_ptr += 3;
						pool_ptr++;

						/* branch to tmp1 */
						arg = (ins >> 36) & 0xf;
						if (((ins >> 32) & 0xf) == 4) {
							/* brcl -> bcr */
							ins = bcr(arg, tmp1);
						} else {
							SLJIT2_ASSERT(((ins >> 32) & 0xf) == 5);
							/* brasl -> basr */
							ins = basr(arg, tmp1);
						}

						/* Adjust half_count. */
						half_count += 2;
					} else
						jump->addr = (sljit2_uw)code_ptr;

					jump = jump->next;
					next_jump_addr = SLJIT2_GET_NEXT_ADDRESS(jump);
				} else  if (next_min_addr == next_const_addr) {
					/* update instruction with relative address of constant */
					source = (sljit2_sw)code_ptr;
					offset = (sljit2_sw)pool_ptr - source;

					SLJIT2_ASSERT(!(offset & 0x1));
					offset >>= 1; /* halfword (not byte) offset */
					SLJIT2_ASSERT(is_s32(offset));

					ins |= (sljit2_ins)offset & 0xffffffff;

					/* update address */
					const_->addr = (sljit2_uw)pool_ptr;

					/* store initial value into pool and update pool address */
					*(pool_ptr++) = (sljit2_uw)(((struct sljit2_s390x_const*)const_)->init_value);

					/* move to next constant */
					const_ = const_->next;
					next_const_addr = SLJIT2_GET_NEXT_ADDRESS(const_);
				}

				SLJIT2_GET_NEXT_MIN();
			}

			if (ins & 0xffff00000000L) {
				*code_ptr++ = (sljit2_u16)(ins >> 32);
				half_count++;
			}

			if (ins & 0xffffffff0000L) {
				*code_ptr++ = (sljit2_u16)(ins >> 16);
				half_count++;
			}

			*code_ptr++ = (sljit2_u16)ins;
			half_count++;
		} while (buf_ptr < buf_end);

		buf = buf->next;
	} while (buf);

	if (next_label_size == half_count) {
		label->u.addr = (sljit2_uw)SLJIT2_ADD_EXEC_OFFSET(code_ptr, executable_offset);
		label = label->next;
	}

	SLJIT2_ASSERT(!label);
	SLJIT2_ASSERT(!jump);
	SLJIT2_ASSERT(!const_);
	SLJIT2_ASSERT(code + (ins_size >> 1) == code_ptr);
	SLJIT2_ASSERT((sljit2_u8 *)pool + pool_size == (sljit2_u8 *)pool_ptr);

	jump = compiler->jumps;
	while (jump != NULL) {
		offset = (sljit2_sw)((jump->flags & JUMP_ADDR) ? jump->u.target : jump->u.label->u.addr);

		if (jump->flags & (SLJIT2_REWRITABLE_JUMP | JUMP_ADDR | JUMP_MOV_ADDR)) {
			/* Store jump target into pool. */
			*(sljit2_uw*)(jump->addr) = (sljit2_uw)offset;
		} else {
			code_ptr = (sljit2_u16*)jump->addr;
			offset -= (sljit2_sw)SLJIT2_ADD_EXEC_OFFSET(code_ptr, executable_offset);

			/* offset must be halfword aligned */
			SLJIT2_ASSERT(!(offset & 1));
			offset >>= 1;
			SLJIT2_ASSERT(is_s32(offset)); /* TODO(mundaym): handle arbitrary offsets */

			code_ptr[1] = (sljit2_u16)(offset >> 16);
			code_ptr[2] = (sljit2_u16)offset;
		}
		jump = jump->next;
	}

	compiler->error = SLJIT2_ERR_COMPILED;
	compiler->executable_offset = executable_offset;
	compiler->executable_size = ins_size;
	if (pool_size)
		compiler->executable_size += (pad_size + pool_size);

	code = (sljit2_u16 *)SLJIT2_ADD_EXEC_OFFSET(code, executable_offset);
	code_ptr = (sljit2_u16 *)SLJIT2_ADD_EXEC_OFFSET(code_ptr, executable_offset);
	SLJIT2_CACHE_FLUSH(code, code_ptr);
	SLJIT2_UPDATE_WX_FLAGS(code, code_ptr, 1);
	return code;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_has_cpu_feature(sljit2_s32 feature_type)
{
	/* TODO(mundaym): implement all */
	switch (feature_type) {
	case SLJIT2_HAS_FPU:
#ifdef SLJIT2_IS_FPU_AVAILABLE
		return (SLJIT2_IS_FPU_AVAILABLE) != 0;
#else
		return 1;
#endif /* SLJIT2_IS_FPU_AVAILABLE */

	case SLJIT2_HAS_CLZ:
	case SLJIT2_HAS_REV:
	case SLJIT2_HAS_ROT:
	case SLJIT2_HAS_PREFETCH:
	case SLJIT2_HAS_COPY_F32:
	case SLJIT2_HAS_COPY_F64:
	case SLJIT2_HAS_SIMD:
	case SLJIT2_HAS_ATOMIC:
	case SLJIT2_HAS_MEMORY_BARRIER:
		return 1;

	case SLJIT2_HAS_CTZ:
		return 2;

	case SLJIT2_HAS_CMOV:
		return have_lscond1() ? 1 : 0;
	}
	return 0;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_cmp_info(sljit2_s32 type)
{
	SLJIT2_UNUSED_ARG(type);
	return 0;
}

/* --------------------------------------------------------------------- */
/*  Entry, exit                                                          */
/* --------------------------------------------------------------------- */

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_enter(struct sljit2_compiler *compiler,
	sljit2_s32 options, sljit2_s32 arg_types,
	sljit2_s32 scratches, sljit2_s32 saveds, sljit2_s32 local_size)
{
	sljit2_s32 fscratches = ENTER_GET_FLOAT_REGS(scratches);
	sljit2_s32 fsaveds = ENTER_GET_FLOAT_REGS(saveds);
	sljit2_s32 saved_arg_count = SLJIT2_KEPT_SAVEDS_COUNT(options);
	sljit2_s32 offset, i, tmp;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_enter(compiler, options, arg_types, scratches, saveds, local_size));
	set_emit_enter(compiler, options, arg_types, scratches, saveds, local_size);

	/* Saved registers are stored in callee allocated save area. */
	SLJIT2_ASSERT(gpr(SLJIT2_FIRST_SAVED_REG) == r6 && gpr(SLJIT2_S0) == r13);

	scratches = ENTER_GET_REGS(scratches);
	saveds = ENTER_GET_REGS(saveds);

	offset = 2 * SSIZE_OF(sw);
	if (saveds + scratches >= SLJIT2_NUMBER_OF_REGISTERS) {
		if (saved_arg_count == 0) {
			FAIL_IF(push_inst(compiler, stmg(r6, r14, offset, r15)));
			offset += 9 * SSIZE_OF(sw);
		} else {
			FAIL_IF(push_inst(compiler, stmg(r6, r13 - (sljit2_gpr)saved_arg_count, offset, r15)));
			offset += (8 - saved_arg_count) * SSIZE_OF(sw);
		}
	} else {
		if (scratches == SLJIT2_FIRST_SAVED_REG) {
			FAIL_IF(push_inst(compiler, stg(r6, offset, 0, r15)));
			offset += SSIZE_OF(sw);
		} else if (scratches > SLJIT2_FIRST_SAVED_REG) {
			FAIL_IF(push_inst(compiler, stmg(r6, r6 + (sljit2_gpr)(scratches - SLJIT2_FIRST_SAVED_REG), offset, r15)));
			offset += (scratches - (SLJIT2_FIRST_SAVED_REG - 1)) * SSIZE_OF(sw);
		}

		if (saved_arg_count == 0) {
			if (saveds == 0) {
				FAIL_IF(push_inst(compiler, stg(r14, offset, 0, r15)));
				offset += SSIZE_OF(sw);
			} else {
				FAIL_IF(push_inst(compiler, stmg(r14 - (sljit2_gpr)saveds, r14, offset, r15)));
				offset += (saveds + 1) * SSIZE_OF(sw);
			}
		} else if (saveds > saved_arg_count) {
			if (saveds == saved_arg_count + 1) {
				FAIL_IF(push_inst(compiler, stg(r14 - (sljit2_gpr)saveds, offset, 0, r15)));
				offset += SSIZE_OF(sw);
			} else {
				FAIL_IF(push_inst(compiler, stmg(r14 - (sljit2_gpr)saveds, r13 - (sljit2_gpr)saved_arg_count, offset, r15)));
				offset += (saveds - saved_arg_count) * SSIZE_OF(sw);
			}
		}
	}

	if (saved_arg_count > 0) {
		FAIL_IF(push_inst(compiler, stg(r14, offset, 0, r15)));
		offset += SSIZE_OF(sw);
	}

	tmp = SLJIT2_FS0 - fsaveds;
	for (i = SLJIT2_FS0; i > tmp; i--) {
		FAIL_IF(push_inst(compiler, 0x60000000 /* std */ | F20(i) | R12A(r15) | (sljit2_ins)offset));
		offset += SSIZE_OF(sw);
	}

	for (i = fscratches; i >= SLJIT2_FIRST_SAVED_FLOAT_REG; i--) {
		FAIL_IF(push_inst(compiler, 0x60000000 /* std */ | F20(i) | R12A(r15) | (sljit2_ins)offset));
		offset += SSIZE_OF(sw);
	}

	local_size = (local_size + SLJIT2_S390X_DEFAULT_STACK_FRAME_SIZE + 0xf) & ~0xf;
	compiler->local_size = local_size;

	if (is_s20(-local_size))
		FAIL_IF(push_inst(compiler, 0xe30000000071 /* lay */ | R36A(r15) | R28A(r15) | disp_s20(-local_size)));
	else
		FAIL_IF(push_inst(compiler, 0xc20400000000 /* slgfi */ | R36A(r15) | (sljit2_ins)local_size));

	if (options & SLJIT2_ENTER_REG_ARG)
		return SLJIT2_SUCCESS;

	arg_types >>= SLJIT2_ARG_SHIFT;
	saved_arg_count = 0;
	tmp = 0;
	while (arg_types > 0) {
		if ((arg_types & SLJIT2_ARG_MASK) < SLJIT2_ARG_TYPE_F64) {
			if (!(arg_types & SLJIT2_ARG_TYPE_SCRATCH_REG)) {
				FAIL_IF(push_inst(compiler, lgr(gpr(SLJIT2_S0 - saved_arg_count), gpr(SLJIT2_R0 + tmp))));
				saved_arg_count++;
			}
			tmp++;
		}

		arg_types >>= SLJIT2_ARG_SHIFT;
	}

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_set_context(struct sljit2_compiler *compiler,
	sljit2_s32 options, sljit2_s32 arg_types,
	sljit2_s32 scratches, sljit2_s32 saveds, sljit2_s32 local_size)
{
	CHECK_ERROR();
	CHECK(check_sljit2_set_context(compiler, options, arg_types, scratches, saveds, local_size));
	set_set_context(compiler, options, arg_types, scratches, saveds, local_size);

	compiler->local_size = (local_size + SLJIT2_S390X_DEFAULT_STACK_FRAME_SIZE + 0xf) & ~0xf;
	return SLJIT2_SUCCESS;
}

static sljit2_s32 emit_stack_frame_release(struct sljit2_compiler *compiler, sljit2_gpr last_reg)
{
	sljit2_s32 offset, i, tmp;
	sljit2_s32 local_size = compiler->local_size;
	sljit2_s32 saveds = compiler->saveds;
	sljit2_s32 scratches = compiler->scratches;
	sljit2_s32 kept_saveds_count = SLJIT2_KEPT_SAVEDS_COUNT(compiler->options);

	if (is_u12(local_size))
		FAIL_IF(push_inst(compiler, 0x41000000 /* ly */ | R20A(r15) | R12A(r15) | (sljit2_ins)local_size));
	else if (is_s20(local_size))
		FAIL_IF(push_inst(compiler, 0xe30000000071 /* lay */ | R36A(r15) | R28A(r15) | disp_s20(local_size)));
	else
		FAIL_IF(push_inst(compiler, 0xc20a00000000 /* algfi */ | R36A(r15) | (sljit2_ins)local_size));

	offset = 2 * SSIZE_OF(sw);
	if (saveds + scratches >= SLJIT2_NUMBER_OF_REGISTERS) {
		if (kept_saveds_count == 0) {
			FAIL_IF(push_inst(compiler, lmg(r6, last_reg, offset, r15)));
			offset += 9 * SSIZE_OF(sw);
		} else {
			FAIL_IF(push_inst(compiler, lmg(r6, r13 - (sljit2_gpr)kept_saveds_count, offset, r15)));
			offset += (8 - kept_saveds_count) * SSIZE_OF(sw);
		}
	} else {
		if (scratches == SLJIT2_FIRST_SAVED_REG) {
			FAIL_IF(push_inst(compiler, lg(r6, offset, 0, r15)));
			offset += SSIZE_OF(sw);
		} else if (scratches > SLJIT2_FIRST_SAVED_REG) {
			FAIL_IF(push_inst(compiler, lmg(r6, r6 + (sljit2_gpr)(scratches - SLJIT2_FIRST_SAVED_REG), offset, r15)));
			offset += (scratches - (SLJIT2_FIRST_SAVED_REG - 1)) * SSIZE_OF(sw);
		}

		if (kept_saveds_count == 0) {
			if (saveds == 0) {
				if (last_reg == r14)
					FAIL_IF(push_inst(compiler, lg(r14, offset, 0, r15)));
				offset += SSIZE_OF(sw);
			} else if (saveds == 1 && last_reg == r13) {
				FAIL_IF(push_inst(compiler, lg(r13, offset, 0, r15)));
				offset += 2 * SSIZE_OF(sw);
			} else {
				FAIL_IF(push_inst(compiler, lmg(r14 - (sljit2_gpr)saveds, last_reg, offset, r15)));
				offset += (saveds + 1) * SSIZE_OF(sw);
			}
		} else if (saveds > kept_saveds_count) {
			if (saveds == kept_saveds_count + 1) {
				FAIL_IF(push_inst(compiler, lg(r14 - (sljit2_gpr)saveds, offset, 0, r15)));
				offset += SSIZE_OF(sw);
			} else {
				FAIL_IF(push_inst(compiler, lmg(r14 - (sljit2_gpr)saveds, r13 - (sljit2_gpr)kept_saveds_count, offset, r15)));
				offset += (saveds - kept_saveds_count) * SSIZE_OF(sw);
			}
		}
	}

	if (kept_saveds_count > 0) {
		if (last_reg == r14)
			FAIL_IF(push_inst(compiler, lg(r14, offset, 0, r15)));
		offset += SSIZE_OF(sw);
	}

	tmp = SLJIT2_FS0 - compiler->fsaveds;
	for (i = SLJIT2_FS0; i > tmp; i--) {
		FAIL_IF(push_inst(compiler, 0x68000000 /* ld */ | F20(i) | R12A(r15) | (sljit2_ins)offset));
		offset += SSIZE_OF(sw);
	}

	for (i = compiler->fscratches; i >= SLJIT2_FIRST_SAVED_FLOAT_REG; i--) {
		FAIL_IF(push_inst(compiler, 0x68000000 /* ld */ | F20(i) | R12A(r15) | (sljit2_ins)offset));
		offset += SSIZE_OF(sw);
	}

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_return_void(struct sljit2_compiler *compiler)
{
	CHECK_ERROR();
	CHECK(check_sljit2_emit_return_void(compiler));

	FAIL_IF(emit_stack_frame_release(compiler, r14));
	return push_inst(compiler, br(r14)); /* return */
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_return_to(struct sljit2_compiler *compiler,
	sljit2_s32 src, sljit2_sw srcw)
{
	CHECK_ERROR();
	CHECK(check_sljit2_emit_return_to(compiler, src, srcw));

	if (src & SLJIT2_MEM) {
		ADJUST_LOCAL_OFFSET(src, srcw);
		FAIL_IF(load_word(compiler, tmp1, src, srcw, 0 /* 64-bit */));
		src = TMP_REG2;
		srcw = 0;
	} else if (src >= SLJIT2_FIRST_SAVED_REG && src <= (SLJIT2_S0 - SLJIT2_KEPT_SAVEDS_COUNT(compiler->options))) {
		FAIL_IF(push_inst(compiler, lgr(tmp1, gpr(src))));
		src = TMP_REG2;
		srcw = 0;
	}

	FAIL_IF(emit_stack_frame_release(compiler, r13));

	SLJIT2_SKIP_CHECKS(compiler);
	return sljit2_emit_ijump(compiler, SLJIT2_JUMP, src, srcw);
}

/* --------------------------------------------------------------------- */
/*  Operators                                                            */
/* --------------------------------------------------------------------- */

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op0(struct sljit2_compiler *compiler, sljit2_s32 op)
{
	sljit2_gpr arg0 = gpr(SLJIT2_R0);
	sljit2_gpr arg1 = gpr(SLJIT2_R1);

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op0(compiler, op));

	op = GET_OPCODE(op) | (op & SLJIT2_32);
	switch (op) {
	case SLJIT2_BREAKPOINT:
		/* The following invalid instruction is emitted by gdb. */
		return push_inst(compiler, 0x0001 /* 2-byte trap */);
	case SLJIT2_NOP:
		return push_inst(compiler, 0x0700 /* 2-byte nop */);
	case SLJIT2_LMUL_UW:
		FAIL_IF(push_inst(compiler, mlgr(arg0, arg0)));
		break;
	case SLJIT2_LMUL_SW:
		/* signed multiplication from: */
		/* Hacker's Delight, Second Edition: Chapter 8-3. */
		FAIL_IF(push_inst(compiler, srag(tmp0, arg0, 63, 0)));
		FAIL_IF(push_inst(compiler, srag(tmp1, arg1, 63, 0)));
		FAIL_IF(push_inst(compiler, ngr(tmp0, arg1)));
		FAIL_IF(push_inst(compiler, ngr(tmp1, arg0)));

		/* unsigned multiplication */
		FAIL_IF(push_inst(compiler, mlgr(arg0, arg0)));

		FAIL_IF(push_inst(compiler, sgr(arg0, tmp0)));
		FAIL_IF(push_inst(compiler, sgr(arg0, tmp1)));
		break;
	case SLJIT2_DIV_U32:
	case SLJIT2_DIVMOD_U32:
		FAIL_IF(push_inst(compiler, lhi(tmp0, 0)));
		FAIL_IF(push_inst(compiler, lr(tmp1, arg0)));
		FAIL_IF(push_inst(compiler, dlr(tmp0, arg1)));
		FAIL_IF(push_inst(compiler, lr(arg0, tmp1))); /* quotient */
		if (op == SLJIT2_DIVMOD_U32)
			return push_inst(compiler, lr(arg1, tmp0)); /* remainder */

		return SLJIT2_SUCCESS;
	case SLJIT2_DIV_S32:
	case SLJIT2_DIVMOD_S32:
		FAIL_IF(push_inst(compiler, lhi(tmp0, 0)));
		FAIL_IF(push_inst(compiler, lr(tmp1, arg0)));
		FAIL_IF(push_inst(compiler, dr(tmp0, arg1)));
		FAIL_IF(push_inst(compiler, lr(arg0, tmp1))); /* quotient */
		if (op == SLJIT2_DIVMOD_S32)
			return push_inst(compiler, lr(arg1, tmp0)); /* remainder */

		return SLJIT2_SUCCESS;
	case SLJIT2_DIV_UW:
	case SLJIT2_DIVMOD_UW:
		FAIL_IF(push_inst(compiler, lghi(tmp0, 0)));
		FAIL_IF(push_inst(compiler, lgr(tmp1, arg0)));
		FAIL_IF(push_inst(compiler, dlgr(tmp0, arg1)));
		FAIL_IF(push_inst(compiler, lgr(arg0, tmp1))); /* quotient */
		if (op == SLJIT2_DIVMOD_UW)
			return push_inst(compiler, lgr(arg1, tmp0)); /* remainder */

		return SLJIT2_SUCCESS;
	case SLJIT2_DIV_SW:
	case SLJIT2_DIVMOD_SW:
		FAIL_IF(push_inst(compiler, lgr(tmp1, arg0)));
		FAIL_IF(push_inst(compiler, dsgr(tmp0, arg1)));
		FAIL_IF(push_inst(compiler, lgr(arg0, tmp1))); /* quotient */
		if (op == SLJIT2_DIVMOD_SW)
			return push_inst(compiler, lgr(arg1, tmp0)); /* remainder */

		return SLJIT2_SUCCESS;
	case SLJIT2_MEMORY_BARRIER:
		return push_inst(compiler, 0x0700 /* bcr */ | (0xe << 4) | 0);
	case SLJIT2_ENDBR:
		return SLJIT2_SUCCESS;
	case SLJIT2_SKIP_FRAMES_BEFORE_RETURN:
		return SLJIT2_SUCCESS;
	default:
		SLJIT2_UNREACHABLE();
	}
	/* swap result registers */
	FAIL_IF(push_inst(compiler, lgr(tmp0, arg0)));
	FAIL_IF(push_inst(compiler, lgr(arg0, arg1)));
	return push_inst(compiler, lgr(arg1, tmp0));
}

static sljit2_s32 sljit2_emit_clz_ctz(struct sljit2_compiler *compiler, sljit2_s32 op, sljit2_gpr dst_r, sljit2_gpr src_r)
{
	sljit2_s32 is_ctz = (GET_OPCODE(op) == SLJIT2_CTZ);

	if ((op & SLJIT2_32) && src_r != tmp0) {
		FAIL_IF(push_inst(compiler, 0xb9160000 /* llgfr */ | R4A(tmp0) | R0A(src_r)));
		src_r = tmp0;
	}

	if (is_ctz) {
		FAIL_IF(push_inst(compiler, ((op & SLJIT2_32) ? 0x1300 /* lcr */ : 0xb9030000 /* lcgr */) | R4A(tmp1) | R0A(src_r)));

		if (src_r == tmp0)
			FAIL_IF(push_inst(compiler, ((op & SLJIT2_32) ? 0x1400 /* nr */ : 0xb9800000 /* ngr */) | R4A(tmp0) | R0A(tmp1)));
		else
			FAIL_IF(push_inst(compiler, 0xb9e40000 /* ngrk */ | R12A(tmp1) | R4A(tmp0) | R0A(src_r)));

		src_r = tmp0;
	}

	FAIL_IF(push_inst(compiler, 0xb9830000 /* flogr */ | R4A(tmp0) | R0A(src_r)));

	if (is_ctz)
		FAIL_IF(push_inst(compiler, 0xec00000000d9 /* aghik */ | R36A(tmp1) | R32A(tmp0) | ((sljit2_ins)(-64 & 0xffff) << 16)));

	if (op & SLJIT2_32) {
		if (!is_ctz && dst_r != tmp0)
			return push_inst(compiler, 0xec00000000d9 /* aghik */ | R36A(dst_r) | R32A(tmp0) | ((sljit2_ins)(-32 & 0xffff) << 16));

		FAIL_IF(push_inst(compiler, 0xc20800000000 /* agfi */ | R36A(tmp0) | (sljit2_u32)-32));
	}

	if (is_ctz)
		FAIL_IF(push_inst(compiler, 0xec0000000057 /* rxsbg */ | R36A(tmp0) | R32A(tmp1) | ((sljit2_ins)((op & SLJIT2_32) ? 59 : 58) << 24) | (63 << 16) | ((sljit2_ins)((op & SLJIT2_32) ? 5 : 6) << 8)));

	if (dst_r == tmp0)
		return SLJIT2_SUCCESS;

	return push_inst(compiler, ((op & SLJIT2_32) ? 0x1800 /* lr */ : 0xb9040000 /* lgr */) | R4A(dst_r) | R0A(tmp0));
}

static sljit2_s32 sljit2_emit_rev(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	struct addr addr;
	sljit2_gpr reg;
	sljit2_ins ins;
	sljit2_s32 opcode = GET_OPCODE(op);
	sljit2_s32 is_16bit = (opcode == SLJIT2_REV_U16 || opcode == SLJIT2_REV_S16);

	if (dst & SLJIT2_MEM) {
		if (src & SLJIT2_MEM) {
			FAIL_IF(load_store_op(compiler, tmp0, src, srcw, op & SLJIT2_32, is_16bit ? load_halfword_forms : load_forms));
			reg = tmp0;
		} else
			reg = gpr(src);

		FAIL_IF(make_addr_bxy(compiler, &addr, dst, dstw, tmp1));

		if (is_16bit)
			ins = 0xe3000000003f /* strvh */;
		else
			ins = (op & SLJIT2_32) ? 0xe3000000003e /* strv */ : 0xe3000000002f /* strvg */;

		return push_inst(compiler, ins | R36A(reg) | R32A(addr.index) | R28A(addr.base) | disp_s20(addr.offset));
	}

	reg = gpr(dst);

	if (src & SLJIT2_MEM) {
		FAIL_IF(make_addr_bxy(compiler, &addr, src, srcw, tmp1));

		if (is_16bit)
			ins = 0xe3000000001f /* lrvh */;
		else
			ins = (op & SLJIT2_32) ? 0xe3000000001e /* lrv */ : 0xe3000000000f /* lrvg */;

		FAIL_IF(push_inst(compiler, ins | R36A(reg) | R32A(addr.index) | R28A(addr.base) | disp_s20(addr.offset)));

		if (opcode == SLJIT2_REV)
			return SLJIT2_SUCCESS;

		if (is_16bit) {
			if (op & SLJIT2_32)
				ins = (opcode == SLJIT2_REV_U16) ? 0xb9950000 /* llhr */ : 0xb9270000 /* lhr */;
			else
				ins = (opcode == SLJIT2_REV_U16) ? 0xb9850000 /* llghr */ : 0xb9070000 /* lghr */;
		} else
			ins = (opcode == SLJIT2_REV_U32) ? 0xb9160000 /* llgfr */ : 0xb9140000 /* lgfr */;

		return push_inst(compiler, ins | R4A(reg) | R0A(reg));
	}

	ins = (op & SLJIT2_32) ? 0xb91f0000 /* lrvr */ : 0xb90f0000 /* lrvgr */;
	FAIL_IF(push_inst(compiler, ins | R4A(reg) | R0A(gpr(src))));

	if (opcode == SLJIT2_REV)
		return SLJIT2_SUCCESS;

	if (!is_16bit) {
		ins = (opcode == SLJIT2_REV_U32) ? 0xb9160000 /* llgfr */ : 0xb9140000 /* lgfr */;
		return push_inst(compiler, ins | R4A(reg) | R0A(reg));
	}

	if (op & SLJIT2_32) {
		ins = (opcode == SLJIT2_REV_U16) ? 0x88000000 /* srl */ : 0x8a000000 /* sra */;
		return push_inst(compiler, ins | R20A(reg) | 16);
	}

	ins = (opcode == SLJIT2_REV_U16) ? 0xeb000000000c /* srlg */ : 0xeb000000000a /* srag */;
	return push_inst(compiler, ins | R36A(reg) | R32A(reg) | (48 << 16));
}

/* LEVAL will be defined later with different parameters as needed */
#define WHEN2(cond, i1, i2) (cond) ? LEVAL(i1) : LEVAL(i2)

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op1(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_ins ins;
	struct addr mem;
	sljit2_gpr dst_r;
	sljit2_gpr src_r;
	sljit2_s32 opcode = GET_OPCODE(op);

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op1(compiler, op, dst, dstw, src, srcw));
	ADJUST_LOCAL_OFFSET(dst, dstw);
	ADJUST_LOCAL_OFFSET(src, srcw);

	if (opcode >= SLJIT2_MOV && opcode <= SLJIT2_MOV_P) {
		/* LOAD REGISTER */
		if (FAST_IS_REG(dst) && FAST_IS_REG(src)) {
			dst_r = gpr(dst);
			src_r = gpr(src);
			switch (opcode | (op & SLJIT2_32)) {
			/* 32-bit */
			case SLJIT2_MOV32_U8:
				ins = llcr(dst_r, src_r);
				break;
			case SLJIT2_MOV32_S8:
				ins = lbr(dst_r, src_r);
				break;
			case SLJIT2_MOV32_U16:
				ins = llhr(dst_r, src_r);
				break;
			case SLJIT2_MOV32_S16:
				ins = lhr(dst_r, src_r);
				break;
			case SLJIT2_MOV32:
				if (dst_r == src_r)
					return SLJIT2_SUCCESS;
				ins = lr(dst_r, src_r);
				break;
			/* 64-bit */
			case SLJIT2_MOV_U8:
				ins = llgcr(dst_r, src_r);
				break;
			case SLJIT2_MOV_S8:
				ins = lgbr(dst_r, src_r);
				break;
			case SLJIT2_MOV_U16:
				ins = llghr(dst_r, src_r);
				break;
			case SLJIT2_MOV_S16:
				ins = lghr(dst_r, src_r);
				break;
			case SLJIT2_MOV_U32:
				ins = llgfr(dst_r, src_r);
				break;
			case SLJIT2_MOV_S32:
				ins = lgfr(dst_r, src_r);
				break;
			case SLJIT2_MOV:
			case SLJIT2_MOV_P:
				if (dst_r == src_r)
					return SLJIT2_SUCCESS;
				ins = lgr(dst_r, src_r);
				break;
			default:
				ins = 0;
				SLJIT2_UNREACHABLE();
				break;
			}
			FAIL_IF(push_inst(compiler, ins));
			return SLJIT2_SUCCESS;
		}
		/* LOAD IMMEDIATE */
		if (FAST_IS_REG(dst) && src == SLJIT2_IMM) {
			switch (opcode) {
			case SLJIT2_MOV_U8:
				srcw = (sljit2_sw)((sljit2_u8)(srcw));
				break;
			case SLJIT2_MOV_S8:
				srcw = (sljit2_sw)((sljit2_s8)(srcw));
				break;
			case SLJIT2_MOV_U16:
				srcw = (sljit2_sw)((sljit2_u16)(srcw));
				break;
			case SLJIT2_MOV_S16:
				srcw = (sljit2_sw)((sljit2_s16)(srcw));
				break;
			case SLJIT2_MOV_U32:
				srcw = (sljit2_sw)((sljit2_u32)(srcw));
				break;
			case SLJIT2_MOV_S32:
			case SLJIT2_MOV32:
				srcw = (sljit2_sw)((sljit2_s32)(srcw));
				break;
			}
			return push_load_imm_inst(compiler, gpr(dst), srcw);
		}
		/* LOAD */
		/* TODO(carenas): avoid reg being defined later */
		#define LEVAL(i) EVAL(i, reg, mem)
		if (FAST_IS_REG(dst) && (src & SLJIT2_MEM)) {
			sljit2_gpr reg = gpr(dst);

			FAIL_IF(make_addr_bxy(compiler, &mem, src, srcw, tmp1));
			/* TODO(carenas): convert all calls below to LEVAL */
			switch (opcode | (op & SLJIT2_32)) {
			case SLJIT2_MOV32_U8:
				ins = llc(reg, mem.offset, mem.index, mem.base);
				break;
			case SLJIT2_MOV32_S8:
				ins = lb(reg, mem.offset, mem.index, mem.base);
				break;
			case SLJIT2_MOV32_U16:
				ins = llh(reg, mem.offset, mem.index, mem.base);
				break;
			case SLJIT2_MOV32_S16:
				ins = WHEN2(is_u12(mem.offset), lh, lhy);
				break;
			case SLJIT2_MOV32:
				ins = WHEN2(is_u12(mem.offset), l, ly);
				break;
			case SLJIT2_MOV_U8:
				ins = LEVAL(llgc);
				break;
			case SLJIT2_MOV_S8:
				ins = lgb(reg, mem.offset, mem.index, mem.base);
				break;
			case SLJIT2_MOV_U16:
				ins = LEVAL(llgh);
				break;
			case SLJIT2_MOV_S16:
				ins = lgh(reg, mem.offset, mem.index, mem.base);
				break;
			case SLJIT2_MOV_U32:
				ins = LEVAL(llgf);
				break;
			case SLJIT2_MOV_S32:
				ins = lgf(reg, mem.offset, mem.index, mem.base);
				break;
			case SLJIT2_MOV_P:
			case SLJIT2_MOV:
				ins = lg(reg, mem.offset, mem.index, mem.base);
				break;
			default:
				ins = 0;
				SLJIT2_UNREACHABLE();
				break;
			}
			FAIL_IF(push_inst(compiler, ins));
			return SLJIT2_SUCCESS;
		}
		/* STORE and STORE IMMEDIATE */
		if ((dst & SLJIT2_MEM) && (FAST_IS_REG(src) || src == SLJIT2_IMM)) {
			struct addr mem;
			sljit2_gpr reg = FAST_IS_REG(src) ? gpr(src) : tmp0;

			if (src == SLJIT2_IMM) {
				/* TODO(mundaym): MOVE IMMEDIATE? */
				FAIL_IF(push_load_imm_inst(compiler, reg, srcw));
			}
			FAIL_IF(make_addr_bxy(compiler, &mem, dst, dstw, tmp1));
			switch (opcode) {
			case SLJIT2_MOV_U8:
			case SLJIT2_MOV_S8:
				return push_inst(compiler,
					WHEN2(is_u12(mem.offset), stc, stcy));
			case SLJIT2_MOV_U16:
			case SLJIT2_MOV_S16:
				return push_inst(compiler,
					WHEN2(is_u12(mem.offset), sth, sthy));
			case SLJIT2_MOV_U32:
			case SLJIT2_MOV_S32:
			case SLJIT2_MOV32:
				return push_inst(compiler,
					WHEN2(is_u12(mem.offset), st, sty));
			case SLJIT2_MOV_P:
			case SLJIT2_MOV:
				FAIL_IF(push_inst(compiler, LEVAL(stg)));
				return SLJIT2_SUCCESS;
			default:
				SLJIT2_UNREACHABLE();
			}
		}
		#undef LEVAL
		/* MOVE CHARACTERS */
		if ((dst & SLJIT2_MEM) && (src & SLJIT2_MEM)) {
			struct addr mem;
			FAIL_IF(make_addr_bxy(compiler, &mem, src, srcw, tmp1));
			switch (opcode) {
			case SLJIT2_MOV_U8:
			case SLJIT2_MOV_S8:
				FAIL_IF(push_inst(compiler,
					EVAL(llgc, tmp0, mem)));
				FAIL_IF(make_addr_bxy(compiler, &mem, dst, dstw, tmp1));
				return push_inst(compiler,
					EVAL(stcy, tmp0, mem));
			case SLJIT2_MOV_U16:
			case SLJIT2_MOV_S16:
				FAIL_IF(push_inst(compiler,
					EVAL(llgh, tmp0, mem)));
				FAIL_IF(make_addr_bxy(compiler, &mem, dst, dstw, tmp1));
				return push_inst(compiler,
					EVAL(sthy, tmp0, mem));
			case SLJIT2_MOV_U32:
			case SLJIT2_MOV_S32:
			case SLJIT2_MOV32:
				FAIL_IF(push_inst(compiler,
					EVAL(ly, tmp0, mem)));
				FAIL_IF(make_addr_bxy(compiler, &mem, dst, dstw, tmp1));
				return push_inst(compiler,
					EVAL(sty, tmp0, mem));
			case SLJIT2_MOV_P:
			case SLJIT2_MOV:
				FAIL_IF(push_inst(compiler,
					EVAL(lg, tmp0, mem)));
				FAIL_IF(make_addr_bxy(compiler, &mem, dst, dstw, tmp1));
				FAIL_IF(push_inst(compiler,
					EVAL(stg, tmp0, mem)));
				return SLJIT2_SUCCESS;
			default:
				SLJIT2_UNREACHABLE();
			}
		}
		SLJIT2_UNREACHABLE();
	}

	SLJIT2_ASSERT(src != SLJIT2_IMM);

	dst_r = FAST_IS_REG(dst) ? gpr(dst) : tmp0;
	src_r = FAST_IS_REG(src) ? gpr(src) : tmp0;

	compiler->status_flags_state = op & (VARIABLE_FLAG_MASK | SLJIT2_SET_Z);

	/* TODO(mundaym): optimize loads and stores */
	switch (opcode) {
	case SLJIT2_CLZ:
	case SLJIT2_CTZ:
		if (src & SLJIT2_MEM)
			FAIL_IF(load_unsigned_word(compiler, src_r, src, srcw, op & SLJIT2_32));

		FAIL_IF(sljit2_emit_clz_ctz(compiler, op, dst_r, src_r));
		break;
	case SLJIT2_REV_U32:
	case SLJIT2_REV_S32:
		op |= SLJIT2_32;
		/* fallthrough */
	case SLJIT2_REV:
	case SLJIT2_REV_U16:
	case SLJIT2_REV_S16:
		return sljit2_emit_rev(compiler, op, dst, dstw, src, srcw);
	default:
		SLJIT2_UNREACHABLE();
	}

	if (dst & SLJIT2_MEM)
		return store_word(compiler, dst_r, dst, dstw, op & SLJIT2_32);

	return SLJIT2_SUCCESS;
}

static SLJIT2_INLINE int is_commutative(sljit2_s32 op)
{
	switch (GET_OPCODE(op)) {
	case SLJIT2_ADD:
	case SLJIT2_ADDC:
	case SLJIT2_MUL:
	case SLJIT2_AND:
	case SLJIT2_OR:
	case SLJIT2_XOR:
		return 1;
	}
	return 0;
}

static const struct ins_forms add_forms = {
	0x1a00, /* ar */
	0xb9080000, /* agr */
	0xb9f80000, /* ark */
	0xb9e80000, /* agrk */
	0x5a000000, /* a */
	0xe3000000005a, /* ay */
	0xe30000000008, /* ag */
};

static const struct ins_forms logical_add_forms = {
	0x1e00, /* alr */
	0xb90a0000, /* algr */
	0xb9fa0000, /* alrk */
	0xb9ea0000, /* algrk */
	0x5e000000, /* al */
	0xe3000000005e, /* aly */
	0xe3000000000a, /* alg */
};

static sljit2_s32 sljit2_emit_add(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	int sets_overflow = (op & VARIABLE_FLAG_MASK) == SLJIT2_SET_OVERFLOW;
	int sets_zero_overflow = (op & (SLJIT2_SET_Z | VARIABLE_FLAG_MASK)) == (SLJIT2_SET_Z | SLJIT2_SET_OVERFLOW);
	const struct ins_forms *forms;
	sljit2_ins ins;

	if (src2 == SLJIT2_IMM) {
		if (!sets_zero_overflow && is_s8(src2w) && (src1 & SLJIT2_MEM) && (dst == src1 && dstw == src1w)) {
			if (sets_overflow)
				ins = (op & SLJIT2_32) ? 0xeb000000006a /* asi */ : 0xeb000000007a /* agsi */;
			else
				ins = (op & SLJIT2_32) ? 0xeb000000006e /* alsi */ : 0xeb000000007e /* algsi */;
			return emit_siy(compiler, ins, dst, dstw, src2w);
		}

		if (is_s16(src2w)) {
			if (sets_overflow)
				ins = (op & SLJIT2_32) ? 0xec00000000d8 /* ahik */ : 0xec00000000d9 /* aghik */;
			else
				ins = (op & SLJIT2_32) ? 0xec00000000da /* alhsik */ : 0xec00000000db /* alghsik */;
			FAIL_IF(emit_rie_d(compiler, ins, dst, src1, src1w, src2w));
			goto done;
		}

		if (!sets_overflow) {
			if ((op & SLJIT2_32) || is_u32(src2w)) {
				ins = (op & SLJIT2_32) ? 0xc20b00000000 /* alfi */ : 0xc20a00000000 /* algfi */;
				FAIL_IF(emit_ri(compiler, ins, dst, src1, src1w, src2w, RIL_A));
				goto done;
			}
			if (is_u32(-src2w)) {
				FAIL_IF(emit_ri(compiler, 0xc20400000000 /* slgfi */, dst, src1, src1w, -src2w, RIL_A));
				goto done;
			}
		}
		else if ((op & SLJIT2_32) || is_s32(src2w)) {
			ins = (op & SLJIT2_32) ? 0xc20900000000 /* afi */ : 0xc20800000000 /* agfi */;
			FAIL_IF(emit_ri(compiler, ins, dst, src1, src1w, src2w, RIL_A));
			goto done;
		}
	}

	forms = sets_overflow ? &add_forms : &logical_add_forms;
	FAIL_IF(emit_commutative(compiler, forms, dst, src1, src1w, src2, src2w));

done:
	if (sets_zero_overflow)
		FAIL_IF(update_zero_overflow(compiler, op, FAST_IS_REG(dst) ? gpr(dst & REG_MASK) : tmp0));

	if (dst & SLJIT2_MEM)
		return store_word(compiler, tmp0, dst, dstw, op & SLJIT2_32);

	return SLJIT2_SUCCESS;
}

static const struct ins_forms sub_forms = {
	0x1b00, /* sr */
	0xb9090000, /* sgr */
	0xb9f90000, /* srk */
	0xb9e90000, /* sgrk */
	0x5b000000, /* s */
	0xe3000000005b, /* sy */
	0xe30000000009, /* sg */
};

static const struct ins_forms logical_sub_forms = {
	0x1f00, /* slr */
	0xb90b0000, /* slgr */
	0xb9fb0000, /* slrk */
	0xb9eb0000, /* slgrk */
	0x5f000000, /* sl */
	0xe3000000005f, /* sly */
	0xe3000000000b, /* slg */
};

static sljit2_s32 sljit2_emit_sub(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_s32 flag_type = GET_FLAG_TYPE(op);
	int sets_signed = (flag_type >= SLJIT2_SIG_LESS && flag_type <= SLJIT2_NOT_OVERFLOW);
	int sets_zero_overflow = (op & (SLJIT2_SET_Z | VARIABLE_FLAG_MASK)) == (SLJIT2_SET_Z | SLJIT2_SET_OVERFLOW);
	const struct ins_forms *forms;
	sljit2_ins ins;

	if (dst == TMP_REG2 && flag_type <= SLJIT2_SIG_LESS_EQUAL) {
		int compare_signed = flag_type >= SLJIT2_SIG_LESS;

		compiler->status_flags_state |= SLJIT2_CURRENT_FLAGS_COMPARE;

		if (src2 == SLJIT2_IMM) {
			if (compare_signed || ((op & VARIABLE_FLAG_MASK) == 0 && is_s32(src2w))) {
				if ((op & SLJIT2_32) || is_s32(src2w)) {
					ins = (op & SLJIT2_32) ? 0xc20d00000000 /* cfi */ : 0xc20c00000000 /* cgfi */;
					return emit_ri(compiler, ins, src1, src1, src1w, src2w, RIL_A);
				}
			} else if ((op & SLJIT2_32) || is_u32(src2w)) {
				ins = (op & SLJIT2_32) ? 0xc20f00000000 /* clfi */ : 0xc20e00000000 /* clgfi */;
				return emit_ri(compiler, ins, src1, src1, src1w, src2w, RIL_A);
			}
		}
		else if (src2 & SLJIT2_MEM) {
			if ((op & SLJIT2_32) && ((src2 & OFFS_REG_MASK) || is_u12(src2w))) {
				ins = compare_signed ? 0x59000000 /* c */ : 0x55000000 /* cl */;
				return emit_rx(compiler, ins, src1, src1, src1w, src2, src2w, RX_A);
			}

			if (compare_signed)
				ins = (op & SLJIT2_32) ? 0xe30000000059 /* cy */ : 0xe30000000020 /* cg */;
			else
				ins = (op & SLJIT2_32) ? 0xe30000000055 /* cly */ : 0xe30000000021 /* clg */;
			return emit_rx(compiler, ins, src1, src1, src1w, src2, src2w, RXY_A);
		}

		if (compare_signed)
			ins = (op & SLJIT2_32) ? 0x1900 /* cr */ : 0xb9200000 /* cgr */;
		else
			ins = (op & SLJIT2_32) ? 0x1500 /* clr */ : 0xb9210000 /* clgr */;
		return emit_rr(compiler, ins, src1, src1, src1w, src2, src2w);
	}

	if (src1 == SLJIT2_IMM && src1w == 0 && (flag_type == 0 || sets_signed)) {
		ins = (op & SLJIT2_32) ? 0x1300 /* lcr */ : 0xb9030000 /* lcgr */;
		FAIL_IF(emit_rr1(compiler, ins, dst, src2, src2w));
		goto done;
	}

	if (src2 == SLJIT2_IMM) {
		sljit2_sw neg_src2w = -src2w;

		if (sets_signed || neg_src2w != 0 || (op & (SLJIT2_SET_Z | VARIABLE_FLAG_MASK)) == 0) {
			if (!sets_zero_overflow && is_s8(neg_src2w) && (src1 & SLJIT2_MEM) && (dst == src1 && dstw == src1w)) {
				if (sets_signed)
					ins = (op & SLJIT2_32) ? 0xeb000000006a /* asi */ : 0xeb000000007a /* agsi */;
				else
					ins = (op & SLJIT2_32) ? 0xeb000000006e /* alsi */ : 0xeb000000007e /* algsi */;
				return emit_siy(compiler, ins, dst, dstw, neg_src2w);
			}

			if (is_s16(neg_src2w)) {
				if (sets_signed)
					ins = (op & SLJIT2_32) ? 0xec00000000d8 /* ahik */ : 0xec00000000d9 /* aghik */;
				else
					ins = (op & SLJIT2_32) ? 0xec00000000da /* alhsik */ : 0xec00000000db /* alghsik */;
				FAIL_IF(emit_rie_d(compiler, ins, dst, src1, src1w, neg_src2w));
				goto done;
			}
		}

		if (!sets_signed) {
			if ((op & SLJIT2_32) || is_u32(src2w)) {
				ins = (op & SLJIT2_32) ? 0xc20500000000 /* slfi */ : 0xc20400000000 /* slgfi */;
				FAIL_IF(emit_ri(compiler, ins, dst, src1, src1w, src2w, RIL_A));
				goto done;
			}
			if (is_u32(neg_src2w)) {
				FAIL_IF(emit_ri(compiler, 0xc20a00000000 /* algfi */, dst, src1, src1w, neg_src2w, RIL_A));
				goto done;
			}
		}
		else if ((op & SLJIT2_32) || is_s32(neg_src2w)) {
			ins = (op & SLJIT2_32) ? 0xc20900000000 /* afi */ : 0xc20800000000 /* agfi */;
			FAIL_IF(emit_ri(compiler, ins, dst, src1, src1w, neg_src2w, RIL_A));
			goto done;
		}
	}

	forms = sets_signed ? &sub_forms : &logical_sub_forms;
	FAIL_IF(emit_non_commutative(compiler, forms, dst, src1, src1w, src2, src2w));

done:
	if (sets_signed) {
		sljit2_gpr dst_r = FAST_IS_REG(dst) ? gpr(dst & REG_MASK) : tmp0;

		if ((op & VARIABLE_FLAG_MASK) != SLJIT2_SET_OVERFLOW) {
			/* In case of overflow, the sign bit of the two source operands must be different, and
			     - the first operand is greater if the sign bit of the result is set
			     - the first operand is less if the sign bit of the result is not set
			   The -result operation sets the corrent sign, because the result cannot be zero.
			   The overflow is considered greater, since the result must be equal to INT_MIN so its sign bit is set. */
			FAIL_IF(push_inst(compiler, brc(0xe, (op & SLJIT2_32) ? (2 + 1) : (2 + 2))));
			FAIL_IF(push_inst(compiler, (op & SLJIT2_32) ? lcr(tmp1, dst_r) : lcgr(tmp1, dst_r)));
		}
		else if (op & SLJIT2_SET_Z)
			FAIL_IF(update_zero_overflow(compiler, op, dst_r));
	}

	if (dst & SLJIT2_MEM)
		return store_word(compiler, tmp0, dst, dstw, op & SLJIT2_32);

	return SLJIT2_SUCCESS;
}

static const struct ins_forms multiply_forms = {
	0xb2520000, /* msr */
	0xb90c0000, /* msgr */
	0xb9fd0000, /* msrkc */
	0xb9ed0000, /* msgrkc */
	0x71000000, /* ms */
	0xe30000000051, /* msy */
	0xe3000000000c, /* msg */
};

static const struct ins_forms multiply_overflow_forms = {
	0,
	0,
	0xb9fd0000, /* msrkc */
	0xb9ed0000, /* msgrkc */
	0,
	0xe30000000053, /* msc */
	0xe30000000083, /* msgc */
};

static sljit2_s32 sljit2_emit_multiply(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_ins ins;

	if (HAS_FLAGS(op)) {
		/* if have_misc2 fails, this operation should be emulated. 32 bit emulation:
		FAIL_IF(push_inst(compiler, lgfr(tmp0, src1_r)));
		FAIL_IF(push_inst(compiler, msgfr(tmp0, src2_r)));
		if (dst_r != tmp0) {
			FAIL_IF(push_inst(compiler, lr(dst_r, tmp0)));
		}
		FAIL_IF(push_inst(compiler, aih(tmp0, 1)));
		FAIL_IF(push_inst(compiler, nihf(tmp0, ~1U)));
		FAIL_IF(push_inst(compiler, ipm(tmp1)));
		FAIL_IF(push_inst(compiler, oilh(tmp1, 0x2000))); */

		return emit_commutative(compiler, &multiply_overflow_forms, dst, src1, src1w, src2, src2w);
	}

	if (src2 == SLJIT2_IMM) {
		if (is_s16(src2w)) {
			ins = (op & SLJIT2_32) ? 0xa70c0000 /* mhi */ : 0xa70d0000 /* mghi */;
			return emit_ri(compiler, ins, dst, src1, src1w, src2w, RI_A);
		}

		if (is_s32(src2w)) {
			ins = (op & SLJIT2_32) ? 0xc20100000000 /* msfi */ : 0xc20000000000 /* msgfi */;
			return emit_ri(compiler, ins, dst, src1, src1w, src2w, RIL_A);
		}
	}

	return emit_commutative(compiler, &multiply_forms, dst, src1, src1w, src2, src2w);
}

static sljit2_s32 sljit2_emit_bitwise_imm(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 dst,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_uw imm, sljit2_s32 count16)
{
	sljit2_s32 mode = compiler->mode;
	sljit2_gpr dst_r = tmp0;
	sljit2_s32 needs_move = 1;

	if (IS_GPR_REG(dst)) {
		dst_r = gpr(dst & REG_MASK);
		if (dst == src1)
			needs_move = 0;
	}

	if (needs_move)
		FAIL_IF(emit_move(compiler, dst_r, src1, src1w));

	if (type == SLJIT2_AND) {
		if (!(mode & SLJIT2_32))
			FAIL_IF(push_inst(compiler, 0xc00a00000000 /* nihf */ | R36A(dst_r) | (imm >> 32)));
		return push_inst(compiler, 0xc00b00000000 /* nilf */ | R36A(dst_r) | (imm & 0xffffffff));
	}
	else if (type == SLJIT2_OR) {
		if (count16 >= 3) {
			FAIL_IF(push_inst(compiler, 0xc00c00000000 /* oihf */ | R36A(dst_r) | (imm >> 32)));
			return push_inst(compiler, 0xc00d00000000 /* oilf */ | R36A(dst_r) | (imm & 0xffffffff));
		}

		if (count16 >= 2) {
			if ((imm & 0x00000000ffffffffull) == 0)
				return push_inst(compiler, 0xc00c00000000 /* oihf */ | R36A(dst_r) | (imm >> 32));
			if ((imm & 0xffffffff00000000ull) == 0)
				return push_inst(compiler, 0xc00d00000000 /* oilf */ | R36A(dst_r) | (imm & 0xffffffff));
		}

		if ((imm & 0xffff000000000000ull) != 0)
			FAIL_IF(push_inst(compiler, 0xa5080000 /* oihh */ | R20A(dst_r) | (imm >> 48)));
		if ((imm & 0x0000ffff00000000ull) != 0)
			FAIL_IF(push_inst(compiler, 0xa5090000 /* oihl */ | R20A(dst_r) | ((imm >> 32) & 0xffff)));
		if ((imm & 0x00000000ffff0000ull) != 0)
			FAIL_IF(push_inst(compiler, 0xa50a0000 /* oilh */ | R20A(dst_r) | ((imm >> 16) & 0xffff)));
		if ((imm & 0x000000000000ffffull) != 0 || imm == 0)
			return push_inst(compiler, 0xa50b0000 /* oill */ | R20A(dst_r) | (imm & 0xffff));
		return SLJIT2_SUCCESS;
	}

	if ((imm & 0xffffffff00000000ull) != 0)
		FAIL_IF(push_inst(compiler, 0xc00600000000 /* xihf */ | R36A(dst_r) | (imm >> 32)));
	if ((imm & 0x00000000ffffffffull) != 0 || imm == 0)
		return push_inst(compiler, 0xc00700000000 /* xilf */ | R36A(dst_r) | (imm & 0xffffffff));
	return SLJIT2_SUCCESS;
}

static const struct ins_forms bitwise_and_forms = {
	0x1400, /* nr */
	0xb9800000, /* ngr */
	0xb9f40000, /* nrk */
	0xb9e40000, /* ngrk */
	0x54000000, /* n */
	0xe30000000054, /* ny */
	0xe30000000080, /* ng */
};

static const struct ins_forms bitwise_or_forms = {
	0x1600, /* or */
	0xb9810000, /* ogr */
	0xb9f60000, /* ork */
	0xb9e60000, /* ogrk */
	0x56000000, /* o */
	0xe30000000056, /* oy */
	0xe30000000081, /* og */
};

static const struct ins_forms bitwise_xor_forms = {
	0x1700, /* xr */
	0xb9820000, /* xgr */
	0xb9f70000, /* xrk */
	0xb9e70000, /* xgrk */
	0x57000000, /* x */
	0xe30000000057, /* xy */
	0xe30000000082, /* xg */
};

static sljit2_s32 sljit2_emit_bitwise(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_s32 type = GET_OPCODE(op);
	const struct ins_forms *forms;

	if (src2 == SLJIT2_IMM && (!(op & SLJIT2_SET_Z) || (type == SLJIT2_AND && dst == TMP_REG2))) {
		sljit2_s32 count16 = 0;
		sljit2_uw imm = (sljit2_uw)src2w;

		if (op & SLJIT2_32)
			imm &= 0xffffffffull;

		if ((imm & 0x000000000000ffffull) != 0 || imm == 0)
			count16++;
		if ((imm & 0x00000000ffff0000ull) != 0)
			count16++;
		if ((imm & 0x0000ffff00000000ull) != 0)
			count16++;
		if ((imm & 0xffff000000000000ull) != 0)
			count16++;

		if (type == SLJIT2_AND && dst == TMP_REG2 && count16 == 1) {
			sljit2_gpr src_r = tmp1;

			if (FAST_IS_REG(src1))
				src_r = gpr(src1 & REG_MASK);
			else
				FAIL_IF(emit_move(compiler, tmp1, src1, src1w));

			if ((imm & 0x000000000000ffffull) != 0 || imm == 0)
				return push_inst(compiler, 0xa7010000 /* tmll */ | R20A(src_r) | imm);
			if ((imm & 0x00000000ffff0000ull) != 0)
				return push_inst(compiler, 0xa7000000 /* tmlh */ | R20A(src_r) | (imm >> 16));
			if ((imm & 0x0000ffff00000000ull) != 0)
				return push_inst(compiler, 0xa7030000 /* tmhl */ | R20A(src_r) | (imm >> 32));
			return push_inst(compiler, 0xa7020000 /* tmhh */ | R20A(src_r) | (imm >> 48));
		}

		if (!(op & SLJIT2_SET_Z))
			return sljit2_emit_bitwise_imm(compiler, type, dst, src1, src1w, imm, count16);
	}

	if (type == SLJIT2_AND)
		forms = &bitwise_and_forms;
	else if (type == SLJIT2_OR)
		forms = &bitwise_or_forms;
	else
		forms = &bitwise_xor_forms;

	return emit_commutative(compiler, forms, dst, src1, src1w, src2, src2w);
}

static sljit2_s32 sljit2_emit_shift(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_s32 type = GET_OPCODE(op);
	sljit2_gpr dst_r = FAST_IS_REG(dst) ? gpr(dst & REG_MASK) : tmp0;
	sljit2_gpr src_r = tmp0;
	sljit2_gpr base_r = tmp0;
	sljit2_ins imm = 0;
	sljit2_ins ins;

	if (FAST_IS_REG(src1))
		src_r = gpr(src1);
	else
		FAIL_IF(emit_move(compiler, tmp0, src1, src1w));

	if (src2 != SLJIT2_IMM) {
		if (FAST_IS_REG(src2))
			base_r = gpr(src2);
		else {
			FAIL_IF(emit_move(compiler, tmp1, src2, src2w));
			base_r = tmp1;
		}

		if ((op & SLJIT2_32) && (type == SLJIT2_MSHL || type == SLJIT2_MLSHR || type == SLJIT2_MASHR)) {
			if (base_r != tmp1) {
				FAIL_IF(push_inst(compiler, 0xec0000000055 /* risbg */ | R36A(tmp1) | R32A(base_r) | (59 << 24) | (1 << 23) | (63 << 16)));
				base_r = tmp1;
			} else
				FAIL_IF(push_inst(compiler, 0xa5070000 /* nill */ | R20A(tmp1) | 0x1f));
		}
	} else
		imm = (sljit2_ins)(src2w & ((op & SLJIT2_32) ? 0x1f : 0x3f));

	if ((op & SLJIT2_32) && dst_r == src_r) {
		if (type == SLJIT2_SHL || type == SLJIT2_MSHL)
			ins = 0x89000000 /* sll */;
		else if (type == SLJIT2_LSHR || type == SLJIT2_MLSHR)
			ins = 0x88000000 /* srl */;
		else
			ins = 0x8a000000 /* sra */;

		FAIL_IF(push_inst(compiler, ins | R20A(dst_r) | R12A(base_r) | imm));
	} else {
		if (type == SLJIT2_SHL || type == SLJIT2_MSHL)
			ins = (op & SLJIT2_32) ? 0xeb00000000df /* sllk */ : 0xeb000000000d /* sllg */;
		else if (type == SLJIT2_LSHR || type == SLJIT2_MLSHR)
			ins = (op & SLJIT2_32) ? 0xeb00000000de /* srlk */ : 0xeb000000000c /* srlg */;
		else
			ins = (op & SLJIT2_32) ? 0xeb00000000dc /* srak */ : 0xeb000000000a /* srag */;

		FAIL_IF(push_inst(compiler, ins | R36A(dst_r) | R32A(src_r) | R28A(base_r) | (imm << 16)));
	}

	if ((op & SLJIT2_SET_Z) && type != SLJIT2_ASHR)
		return push_inst(compiler, (op & SLJIT2_32) ? or(dst_r, dst_r) : ogr(dst_r, dst_r));

	return SLJIT2_SUCCESS;
}

static sljit2_s32 sljit2_emit_rotate(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_gpr dst_r = FAST_IS_REG(dst) ? gpr(dst & REG_MASK) : tmp0;
	sljit2_gpr src_r = tmp0;
	sljit2_gpr base_r = tmp0;
	sljit2_ins imm = 0;
	sljit2_ins ins;

	if (FAST_IS_REG(src1))
		src_r = gpr(src1);
	else
		FAIL_IF(emit_move(compiler, tmp0, src1, src1w));

	if (src2 != SLJIT2_IMM) {
		if (FAST_IS_REG(src2))
			base_r = gpr(src2);
		else {
			FAIL_IF(emit_move(compiler, tmp1, src2, src2w));
			base_r = tmp1;
		}
	}

	if (GET_OPCODE(op) == SLJIT2_ROTR) {
		if (src2 != SLJIT2_IMM) {
			ins = (op & SLJIT2_32) ? 0x1300 /* lcr */ : 0xb9030000 /* lcgr */;
			FAIL_IF(push_inst(compiler, ins | R4A(tmp1) | R0A(base_r)));
			base_r = tmp1;
		} else
			src2w = -src2w;
	}

	if (src2 == SLJIT2_IMM)
		imm = (sljit2_ins)(src2w & ((op & SLJIT2_32) ? 0x1f : 0x3f));

	ins = (op & SLJIT2_32) ? 0xeb000000001d /* rll */ : 0xeb000000001c /* rllg */;
	return push_inst(compiler, ins | R36A(dst_r) | R32A(src_r) | R28A(base_r) | (imm << 16));
}

static const struct ins_forms addc_forms = {
	0xb9980000, /* alcr */
	0xb9880000, /* alcgr */
	0,
	0,
	0,
	0xe30000000098, /* alc */
	0xe30000000088, /* alcg */
};

static const struct ins_forms subc_forms = {
	0xb9990000, /* slbr */
	0xb9890000, /* slbgr */
	0,
	0,
	0,
	0xe30000000099, /* slb */
	0xe30000000089, /* slbg */
};

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op2(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	CHECK_ERROR();
	CHECK(check_sljit2_emit_op2(compiler, op, 0, dst, dstw, src1, src1w, src2, src2w));
	ADJUST_LOCAL_OFFSET(dst, dstw);
	ADJUST_LOCAL_OFFSET(src1, src1w);
	ADJUST_LOCAL_OFFSET(src2, src2w);

	compiler->mode = op & SLJIT2_32;
	compiler->status_flags_state = op & (VARIABLE_FLAG_MASK | SLJIT2_SET_Z);

	if (is_commutative(op) && src1 == SLJIT2_IMM && src2 != SLJIT2_IMM) {
		src1 ^= src2;
		src2 ^= src1;
		src1 ^= src2;

		src1w ^= src2w;
		src2w ^= src1w;
		src1w ^= src2w;
	}

	switch (GET_OPCODE(op)) {
	case SLJIT2_ADD:
		compiler->status_flags_state |= SLJIT2_CURRENT_FLAGS_ADD;
		return sljit2_emit_add(compiler, op, dst, dstw, src1, src1w, src2, src2w);
	case SLJIT2_ADDC:
		compiler->status_flags_state |= SLJIT2_CURRENT_FLAGS_ADD;
		FAIL_IF(emit_commutative(compiler, &addc_forms, dst, src1, src1w, src2, src2w));
		if (dst & SLJIT2_MEM)
			return store_word(compiler, tmp0, dst, dstw, op & SLJIT2_32);
		return SLJIT2_SUCCESS;
	case SLJIT2_SUB:
		compiler->status_flags_state |= SLJIT2_CURRENT_FLAGS_SUB;
		return sljit2_emit_sub(compiler, op, dst, dstw, src1, src1w, src2, src2w);
	case SLJIT2_SUBC:
		compiler->status_flags_state |= SLJIT2_CURRENT_FLAGS_SUB;
		FAIL_IF(emit_non_commutative(compiler, &subc_forms, dst, src1, src1w, src2, src2w));
		if (dst & SLJIT2_MEM)
			return store_word(compiler, tmp0, dst, dstw, op & SLJIT2_32);
		return SLJIT2_SUCCESS;
	case SLJIT2_MUL:
		FAIL_IF(sljit2_emit_multiply(compiler, op, dst, src1, src1w, src2, src2w));
		break;
	case SLJIT2_AND:
	case SLJIT2_OR:
	case SLJIT2_XOR:
		FAIL_IF(sljit2_emit_bitwise(compiler, op, dst, src1, src1w, src2, src2w));
		break;
	case SLJIT2_SHL:
	case SLJIT2_MSHL:
	case SLJIT2_LSHR:
	case SLJIT2_MLSHR:
	case SLJIT2_ASHR:
	case SLJIT2_MASHR:
		FAIL_IF(sljit2_emit_shift(compiler, op, dst, src1, src1w, src2, src2w));
		break;
	case SLJIT2_ROTL:
	case SLJIT2_ROTR:
		FAIL_IF(sljit2_emit_rotate(compiler, op, dst, src1, src1w, src2, src2w));
		break;
	}

	if (dst & SLJIT2_MEM)
		return store_word(compiler, tmp0, dst, dstw, op & SLJIT2_32);
	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op2u(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_s32 dst_reg = (GET_OPCODE(op) == SLJIT2_SUB || GET_OPCODE(op) == SLJIT2_AND) ? TMP_REG2 : TMP_REG1;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op2(compiler, op, 1, 0, 0, src1, src1w, src2, src2w));

	SLJIT2_SKIP_CHECKS(compiler);
	return sljit2_emit_op2(compiler, op, dst_reg, 0, src1, src1w, src2, src2w);
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op2r(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst_reg,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	CHECK_ERROR();
	CHECK(check_sljit2_emit_op2r(compiler, op, dst_reg, src1, src1w, src2, src2w));

	switch (GET_OPCODE(op)) {
	case SLJIT2_MULADD:
		SLJIT2_SKIP_CHECKS(compiler);
		FAIL_IF(sljit2_emit_op2(compiler, SLJIT2_MUL | (op & SLJIT2_32), 0 /* tmp0 */, 0, src1, src1w, src2, src2w));
		return push_inst(compiler, ((op & SLJIT2_32) ? 0x1a00 /* ar */ : 0xb9080000 /* agr */) | R4A(gpr(dst_reg)) | R0A(tmp0));
	}

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_shift_into(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst_reg,
	sljit2_s32 src1_reg,
	sljit2_s32 src2_reg,
	sljit2_s32 src3, sljit2_sw src3w)
{
	sljit2_s32 is_right;
	sljit2_sw bit_length = (op & SLJIT2_32) ? 32 : 64;
	sljit2_gpr dst_r = gpr(dst_reg);
	sljit2_gpr src1_r = gpr(src1_reg);
	sljit2_gpr src2_r = gpr(src2_reg);
	sljit2_gpr src3_r = tmp1;
	sljit2_ins ins;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_shift_into(compiler, op, dst_reg, src1_reg, src2_reg, src3, src3w));

	is_right = (GET_OPCODE(op) == SLJIT2_LSHR || GET_OPCODE(op) == SLJIT2_MLSHR);

	if (src1_reg == src2_reg) {
		SLJIT2_SKIP_CHECKS(compiler);
		return sljit2_emit_op2(compiler, (is_right ? SLJIT2_ROTR : SLJIT2_ROTL) | (op & SLJIT2_32), dst_reg, 0, src1_reg, 0, src3, src3w);
	}

	ADJUST_LOCAL_OFFSET(src3, src3w);

	if (src3 == SLJIT2_IMM) {
		src3w &= bit_length - 1;

		if (src3w == 0)
			return SLJIT2_SUCCESS;

		if (op & SLJIT2_32) {
			if (dst_r == src1_r) {
				ins = is_right ? 0x88000000 /* srl */ : 0x89000000 /* sll */;
				FAIL_IF(push_inst(compiler, ins | R20A(dst_r) | (sljit2_ins)src3w));
			} else {
				ins = is_right ? 0xeb00000000de /* srlk */ : 0xeb00000000df /* sllk */;
				FAIL_IF(push_inst(compiler, ins | R36A(dst_r) | R32A(src1_r) | ((sljit2_ins)src3w << 16)));
			}
		} else {
			ins = is_right ? 0xeb000000000c /* srlg */ : 0xeb000000000d /* sllg */;
			FAIL_IF(push_inst(compiler, ins | R36A(dst_r) | R32A(src1_r) | ((sljit2_ins)src3w << 16)));
		}

		ins = 0xec0000000055 /* risbg */;

		if (is_right) {
			src3w = bit_length - src3w;
			ins |= ((sljit2_ins)(64 - bit_length) << 24) | ((sljit2_ins)(63 - src3w) << 16) | ((sljit2_ins)src3w << 8);
		} else
			ins |= ((sljit2_ins)(64 - src3w) << 24) | ((sljit2_ins)63 << 16) | ((sljit2_ins)(src3w + 64 - bit_length) << 8);

		return push_inst(compiler, ins | R36A(dst_r) | R32A(src2_r));
	}

	if (!(src3 & SLJIT2_MEM)) {
		src3_r = gpr(src3);

		if (dst_r == src3_r) {
			FAIL_IF(push_inst(compiler, 0x1800 /* lr */ | R4A(tmp1) | R0A(src3_r)));
			src3_r = tmp1;
		}
	} else
		FAIL_IF(load_word(compiler, tmp1, src3, src3w, op & SLJIT2_32));

	if (op & SLJIT2_32) {
		if (GET_OPCODE(op) == SLJIT2_MSHL || GET_OPCODE(op) == SLJIT2_MLSHR) {
			if (src3_r != tmp1) {
				FAIL_IF(push_inst(compiler, 0xec0000000055 /* risbg */ | R36A(tmp1) | R32A(src3_r) | (59 << 24) | (1 << 23) | (63 << 16)));
				src3_r = tmp1;
			} else
				FAIL_IF(push_inst(compiler, 0xa5070000 /* nill */ | R20A(tmp1) | 0x1f));
		}

		if (dst_r == src1_r) {
			ins = is_right ? 0x88000000 /* srl */ : 0x89000000 /* sll */;
			FAIL_IF(push_inst(compiler, ins | R20A(dst_r) | R12A(src3_r)));
		} else {
			ins = is_right ? 0xeb00000000de /* srlk */ : 0xeb00000000df /* sllk */;
			FAIL_IF(push_inst(compiler, ins | R36A(dst_r) | R32A(src1_r) | R28A(src3_r)));
		}

		if (src3_r != tmp1) {
			FAIL_IF(push_inst(compiler, 0xa50f0000 /* llill */ | R20A(tmp1) | 0x1f));
			FAIL_IF(push_inst(compiler, 0x1700 /* xr */ | R4A(tmp1) | R0A(src3_r)));
		} else
			FAIL_IF(push_inst(compiler, 0xc00700000000 /* xilf */ | R36A(tmp1) | 0x1f));

		ins = is_right ? 0xeb00000000df /* sllk */ : 0xeb00000000de /* srlk */;
		FAIL_IF(push_inst(compiler, ins | R36A(tmp0) | R32A(src2_r) | R28A(tmp1) | (0x1 << 16)));

		return push_inst(compiler, 0x1600 /* or */ | R4A(dst_r) | R0A(tmp0));
	}

	ins = is_right ? 0xeb000000000c /* srlg */ : 0xeb000000000d /* sllg */;
	FAIL_IF(push_inst(compiler, ins | R36A(dst_r) | R32A(src1_r) | R28A(src3_r)));

	ins = is_right ? 0xeb000000000d /* sllg */ : 0xeb000000000c /* srlg */;

	if (!(op & SLJIT2_SHIFT_INTO_NON_ZERO)) {
		if (src3_r != tmp1)
			FAIL_IF(push_inst(compiler, 0xa50f0000 /* llill */ | R20A(tmp1) | 0x3f));

		FAIL_IF(push_inst(compiler, ins | R36A(tmp0) | R32A(src2_r) | (0x1 << 16)));
		src2_r = tmp0;

		if (src3_r != tmp1)
			FAIL_IF(push_inst(compiler, 0xb9820000 /* xgr */ | R4A(tmp1) | R0A(src3_r)));
		else
			FAIL_IF(push_inst(compiler, 0xc00700000000 /* xilf */ | R36A(tmp1) | 0x3f));
	} else
		FAIL_IF(push_inst(compiler, 0xb9030000 /* lcgr */ | R4A(tmp1) | R0A(src3_r)));

	FAIL_IF(push_inst(compiler, ins | R36A(tmp0) | R32A(src2_r) | R28A(tmp1)));
	return push_inst(compiler, 0xb9810000 /* ogr */ | R4A(dst_r) | R0A(tmp0));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op_src(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_gpr src_r;
	struct addr addr;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op_src(compiler, op, src, srcw));
	ADJUST_LOCAL_OFFSET(src, srcw);

	switch (op) {
	case SLJIT2_FAST_RETURN:
		src_r = FAST_IS_REG(src) ? gpr(src) : tmp1;
		if (src & SLJIT2_MEM)
			FAIL_IF(load_word(compiler, tmp1, src, srcw, 0));

		return push_inst(compiler, br(src_r));
	case SLJIT2_SKIP_FRAMES_BEFORE_FAST_RETURN:
		return SLJIT2_SUCCESS;
	case SLJIT2_PREFETCH_L1:
	case SLJIT2_PREFETCH_L2:
	case SLJIT2_PREFETCH_L3:
	case SLJIT2_PREFETCH_ONCE:
		FAIL_IF(make_addr_bxy(compiler, &addr, src, srcw, tmp1));
		return push_inst(compiler, 0xe31000000036 /* pfd */ | R32A(addr.index) | R28A(addr.base) | disp_s20(addr.offset));
	default:
		return SLJIT2_SUCCESS;
	}

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op_dst(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw)
{
	sljit2_gpr dst_r = link_r;
	sljit2_s32 size;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op_dst(compiler, op, dst, dstw));
	ADJUST_LOCAL_OFFSET(dst, dstw);

	switch (op) {
	case SLJIT2_FAST_ENTER:
		if (FAST_IS_REG(dst))
			return push_inst(compiler, lgr(gpr(dst), link_r));
		break;
	case SLJIT2_GET_RETURN_ADDRESS:
		dst_r = FAST_IS_REG(dst) ? gpr(dst) : tmp0;

		size = GET_SAVED_REGISTERS_SIZE(compiler->scratches, compiler->saveds - SLJIT2_KEPT_SAVEDS_COUNT(compiler->options), 2);
		FAIL_IF(load_word(compiler, dst_r, SLJIT2_MEM1(SLJIT2_SP), compiler->local_size + size, 0));
		break;
	}

	if (dst & SLJIT2_MEM)
		return store_word(compiler, dst_r, dst, dstw, 0);

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_get_register_index(sljit2_s32 type, sljit2_s32 reg)
{
	CHECK_REG_INDEX(check_sljit2_get_register_index(type, reg));

	if (type == SLJIT2_GP_REGISTER)
		return (sljit2_s32)gpr(reg);

	if (type != SLJIT2_FLOAT_REGISTER)
		return -1;

	return (sljit2_s32)freg_map[reg];
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op_custom(struct sljit2_compiler *compiler,
	void *instruction, sljit2_u32 size)
{
	sljit2_ins ins = 0;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op_custom(compiler, instruction, size));

	memcpy((sljit2_u8 *)&ins + sizeof(ins) - size, instruction, size);
	return push_inst(compiler, ins);
}

/* --------------------------------------------------------------------- */
/*  Floating point operators                                             */
/* --------------------------------------------------------------------- */

#define FLOAT_LOAD 0
#define FLOAT_STORE 1

static sljit2_s32 float_mem(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 reg,
	sljit2_s32 mem, sljit2_sw memw)
{
	struct addr addr;
	sljit2_ins ins;

	SLJIT2_ASSERT(mem & SLJIT2_MEM);

	if ((mem & OFFS_REG_MASK) || is_u12(memw) || !is_s20(memw)) {
		FAIL_IF(make_addr_bx(compiler, &addr, mem, memw, tmp1));

		if (op & FLOAT_STORE)
			ins = (op & SLJIT2_32) ? 0x70000000 /* ste */ : 0x60000000 /* std */;
		else
			ins = (op & SLJIT2_32) ? 0x78000000 /* le */ : 0x68000000 /* ld */;

		return push_inst(compiler, ins | F20(reg) | R16A(addr.index) | R12A(addr.base) | (sljit2_ins)addr.offset);
	}

	FAIL_IF(make_addr_bxy(compiler, &addr, mem, memw, tmp1));

	if (op & FLOAT_STORE)
		ins = (op & SLJIT2_32) ? 0xed0000000066 /* stey */ : 0xed0000000067 /* stdy */;
	else
		ins = (op & SLJIT2_32) ? 0xed0000000064 /* ley */ : 0xed0000000065 /* ldy */;

	return push_inst(compiler, ins | F36(reg) | R32A(addr.index) | R28A(addr.base) | disp_s20(addr.offset));
}

static sljit2_s32 emit_float(struct sljit2_compiler *compiler, sljit2_ins ins_r, sljit2_ins ins,
	sljit2_s32 reg,
	sljit2_s32 src, sljit2_sw srcw)
{
	struct addr addr;

	if (!(src & SLJIT2_MEM))
		return push_inst(compiler, ins_r | F4(reg) | F0(src));

	FAIL_IF(make_addr_bx(compiler, &addr, src, srcw, tmp1));
	return push_inst(compiler, ins | F36(reg) | R32A(addr.index) | R28A(addr.base) | ((sljit2_ins)addr.offset << 16));
}

static SLJIT2_INLINE sljit2_s32 sljit2_emit_fop1_conv_sw_from_f64(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_ins dst_r = FAST_IS_REG(dst) ? gpr(dst) : tmp0;
	sljit2_ins ins;

	if (src & SLJIT2_MEM) {
		FAIL_IF(float_mem(compiler, FLOAT_LOAD | (op & SLJIT2_32), TMP_FREG1, src, srcw));
		src = TMP_FREG1;
	}

	/* M3 is set to 5 */
	if (GET_OPCODE(op) == SLJIT2_CONV_SW_FROM_F64)
		ins = (op & SLJIT2_32) ? 0xb3a85000 /* cgebr */ : 0xb3a95000 /* cgdbr */;
	else
		ins = (op & SLJIT2_32) ? 0xb3985000 /* cfebr */ : 0xb3995000 /* cfdbr */;

	FAIL_IF(push_inst(compiler, ins | R4A(dst_r) | F0(src)));

	if (dst & SLJIT2_MEM)
		return store_word(compiler, dst_r, dst, dstw, GET_OPCODE(op) >= SLJIT2_CONV_S32_FROM_F64);

	return SLJIT2_SUCCESS;
}

static sljit2_s32 sljit2_emit_fop1_conv_f64_from_w(struct sljit2_compiler *compiler, sljit2_ins ins,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_s32 dst_r = FAST_IS_REG(dst) ? dst : TMP_FREG1;

	if (src == SLJIT2_IMM) {
		FAIL_IF(push_load_imm_inst(compiler, tmp0, srcw));
		src = (sljit2_s32)tmp0;
	}
	else if (src & SLJIT2_MEM) {
		FAIL_IF(load_word(compiler, tmp0, src, srcw, ins & 0x100000));
		src = (sljit2_s32)tmp0;
	}

	FAIL_IF(push_inst(compiler, ins | F4(dst_r) | R0(src)));

	if (dst & SLJIT2_MEM)
		return float_mem(compiler, FLOAT_STORE | ((ins & 0x10000) ? 0 : SLJIT2_32), TMP_FREG1, dst, dstw);

	return SLJIT2_SUCCESS;
}

static SLJIT2_INLINE sljit2_s32 sljit2_emit_fop1_conv_f64_from_sw(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_ins ins;

	if (src == SLJIT2_IMM && GET_OPCODE(op) == SLJIT2_CONV_F64_FROM_S32)
		srcw = (sljit2_s32)srcw;

	if (GET_OPCODE(op) == SLJIT2_CONV_F64_FROM_SW)
		ins = (op & SLJIT2_32) ? 0xb3a40000 /* cegbr */ : 0xb3a50000 /* cdgbr */;
	else
		ins = (op & SLJIT2_32) ? 0xb3940000 /* cefbr */ : 0xb3950000 /* cdfbr */;

	return sljit2_emit_fop1_conv_f64_from_w(compiler, ins, dst, dstw, src, srcw);
}

static SLJIT2_INLINE sljit2_s32 sljit2_emit_fop1_conv_f64_from_uw(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_ins ins;

	if (src == SLJIT2_IMM && GET_OPCODE(op) == SLJIT2_CONV_F64_FROM_U32)
		srcw = (sljit2_u32)srcw;

	if (GET_OPCODE(op) == SLJIT2_CONV_F64_FROM_UW)
		ins = (op & SLJIT2_32) ? 0xb3a00000 /* celgbr */ : 0xb3a10000 /* cdlgbr */;
	else
		ins = (op & SLJIT2_32) ? 0xb3900000 /* celfbr */ : 0xb3910000 /* cdlfbr */;

	return sljit2_emit_fop1_conv_f64_from_w(compiler, ins, dst, dstw, src, srcw);
}

static SLJIT2_INLINE sljit2_s32 sljit2_emit_fop1_cmp(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_ins ins_r, ins;

	if (src1 & SLJIT2_MEM) {
		FAIL_IF(float_mem(compiler, FLOAT_LOAD | (op & SLJIT2_32), TMP_FREG1, src1, src1w));
		src1 = TMP_FREG1;
	}

	if (op & SLJIT2_32) {
		ins_r = 0xb3090000 /* cebr */;
		ins = 0xed0000000009 /* ceb */;
	} else {
		ins_r = 0xb3190000 /* cdbr */;
		ins = 0xed0000000019 /* cdb */;
	}

	return emit_float(compiler, ins_r, ins, src1, src2, src2w);
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_fop1(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_s32 dst_r;
	sljit2_ins ins;

	CHECK_ERROR();

	SELECT_FOP1_OPERATION_WITH_CHECKS(compiler, op, dst, dstw, src, srcw);

	dst_r = FAST_IS_REG(dst) ? dst : TMP_FREG1;

	if (op == SLJIT2_CONV_F64_FROM_F32)
		FAIL_IF(emit_float(compiler, 0xb3040000 /* ldebr */, 0xed0000000004 /* ldeb */, dst_r, src, srcw));
	else {
		if (src & SLJIT2_MEM) {
			FAIL_IF(float_mem(compiler, FLOAT_LOAD | (op == SLJIT2_CONV_F32_FROM_F64 ? 0 : (op & SLJIT2_32)), dst_r, src, srcw));
			src = dst_r;
		}

		switch (GET_OPCODE(op)) {
		case SLJIT2_MOV_F64:
			if (FAST_IS_REG(dst)) {
				if (dst == src)
					return SLJIT2_SUCCESS;

				ins = (op & SLJIT2_32) ? 0x3800 /* ler */ : 0x2800 /* ldr */;
				break;
			}
			return float_mem(compiler, FLOAT_STORE | (op & SLJIT2_32), src, dst, dstw);
		case SLJIT2_CONV_F64_FROM_F32:
			/* Only SLJIT2_CONV_F32_FROM_F64. */
			ins = 0xb3440000 /* ledbr */;
			break;
		case SLJIT2_NEG_F64:
			ins = (op & SLJIT2_32) ? 0xb3030000 /* lcebr */ : 0xb3130000 /* lcdbr */;
			break;
		default:
			SLJIT2_ASSERT(GET_OPCODE(op) == SLJIT2_ABS_F64);
			ins = (op & SLJIT2_32) ? 0xb3000000 /* lpebr */ : 0xb3100000 /* lpdbr */;
			break;
		}

		FAIL_IF(push_inst(compiler, ins | F4(dst_r) | F0(src)));
	}

	if (dst & SLJIT2_MEM)
		return float_mem(compiler, FLOAT_STORE | (op & SLJIT2_32), TMP_FREG1, dst, dstw);

	return SLJIT2_SUCCESS;
}

#define FLOAT_MOV(op, dst_r, src_r) \
	(((op & SLJIT2_32) ? 0x3800 /* ler */ : 0x2800 /* ldr */) | F4(dst_r) | F0(src_r))

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_fop2(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_s32 dst_r = TMP_FREG1;
	sljit2_ins ins_r, ins;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_fop2(compiler, op, dst, dstw, src1, src1w, src2, src2w));
	ADJUST_LOCAL_OFFSET(dst, dstw);
	ADJUST_LOCAL_OFFSET(src1, src1w);
	ADJUST_LOCAL_OFFSET(src2, src2w);

	do {
		if (FAST_IS_REG(dst)) {
			dst_r = dst;

			if (dst == src1)
				break;

			if (dst == src2) {
				if (GET_OPCODE(op) == SLJIT2_ADD_F64 || GET_OPCODE(op) == SLJIT2_MUL_F64) {
					src2 = src1;
					src2w = src1w;
					src1 = dst;
					break;
				}

				FAIL_IF(push_inst(compiler, FLOAT_MOV(op, TMP_FREG1, src2)));
				src2 = TMP_FREG1;
			}
		}

		if (src1 & SLJIT2_MEM)
			FAIL_IF(float_mem(compiler, FLOAT_LOAD | (op & SLJIT2_32), dst_r, src1, src1w));
		else
			FAIL_IF(push_inst(compiler, FLOAT_MOV(op, dst_r, src1)));
	} while (0);

	switch (GET_OPCODE(op)) {
	case SLJIT2_ADD_F64:
		ins_r = (op & SLJIT2_32) ? 0xb30a0000 /* aebr */ : 0xb31a0000 /* adbr */;
		ins = (op & SLJIT2_32) ? 0xed000000000a /* aeb */ : 0xed000000001a /* adb */;
		break;
	case SLJIT2_SUB_F64:
		ins_r = (op & SLJIT2_32) ? 0xb30b0000 /* sebr */ : 0xb31b0000 /* sdbr */;
		ins = (op & SLJIT2_32) ? 0xed000000000b /* seb */ : 0xed000000001b /* sdb */;
		break;
	case SLJIT2_MUL_F64:
		ins_r = (op & SLJIT2_32) ? 0xb3170000 /* meebr */ : 0xb31c0000 /* mdbr */;
		ins = (op & SLJIT2_32) ? 0xed0000000017 /* meeb */ : 0xed000000001c /* mdb */;
		break;
	default:
		SLJIT2_ASSERT(GET_OPCODE(op) == SLJIT2_DIV_F64);
		ins_r = (op & SLJIT2_32) ? 0xb30d0000 /* debr */ : 0xb31d0000 /* ddbr */;
		ins = (op & SLJIT2_32) ? 0xed000000000d /* deb */ : 0xed000000001d /* ddb */;
		break;
	}

	FAIL_IF(emit_float(compiler, ins_r, ins, dst_r, src2, src2w));

	if (dst & SLJIT2_MEM)
		return float_mem(compiler, FLOAT_STORE | (op & SLJIT2_32), TMP_FREG1, dst, dstw);

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_fop2r(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst_freg,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_s32 reg;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_fop2r(compiler, op, dst_freg, src1, src1w, src2, src2w));
	ADJUST_LOCAL_OFFSET(src1, src1w);
	ADJUST_LOCAL_OFFSET(src2, src2w);

	if (src2 & SLJIT2_MEM) {
		FAIL_IF(float_mem(compiler, FLOAT_LOAD | (op & SLJIT2_32), TMP_FREG1, src2, src2w));
		src2 = TMP_FREG1;
	}

	if (src1 & SLJIT2_MEM) {
		reg = (dst_freg == src2) ? TMP_FREG1 : dst_freg;
		FAIL_IF(float_mem(compiler, FLOAT_LOAD | (op & SLJIT2_32), reg, src1, src1w));
		src1 = reg;
	}

	return push_inst(compiler, 0xb3720000 /* cpsdr */ | F12(src2) | F4(dst_freg) | F0(src1));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_fset32(struct sljit2_compiler *compiler,
	sljit2_s32 freg, sljit2_f32 value)
{
	union {
		sljit2_s32 imm;
		sljit2_f32 value;
	} u;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_fset32(compiler, freg, value));

	u.value = value;

	FAIL_IF(push_load_imm_inst(compiler, tmp1, (sljit2_sw)(((sljit2_uw)u.imm << 32))));
	return push_inst(compiler, 0xb3c10000 /* ldgr */ | F4(freg) | R0A(tmp1));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_fset64(struct sljit2_compiler *compiler,
	sljit2_s32 freg, sljit2_f64 value)
{
	union {
		sljit2_sw imm;
		sljit2_f64 value;
	} u;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_fset64(compiler, freg, value));

	u.value = value;

	FAIL_IF(push_load_imm_inst(compiler, tmp1, (sljit2_sw)u.imm));
	return push_inst(compiler, 0xb3c10000 /* ldgr */ | F4(freg) | R0A(tmp1));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_fcopy(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 freg, sljit2_s32 reg)
{
	sljit2_gpr gen_r;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_fcopy(compiler, op, freg, reg));

	gen_r = gpr(reg);

	if (GET_OPCODE(op) == SLJIT2_COPY_TO_F64) {
		if (op & SLJIT2_32) {
			FAIL_IF(push_inst(compiler, 0xeb000000000d /* sllg */ | R36A(tmp0) | R32A(gen_r) | (32 << 16)));
			gen_r = tmp0;
		}

		return push_inst(compiler, 0xb3c10000 /* ldgr */ | F4(freg) | R0A(gen_r));
	}

	FAIL_IF(push_inst(compiler, 0xb3cd0000 /* lgdr */ | R4A(gen_r) | F0(freg)));

	if (!(op & SLJIT2_32))
		return SLJIT2_SUCCESS;

	return push_inst(compiler, 0xeb000000000c /* srlg */ | R36A(gen_r) | R32A(gen_r) | (32 << 16));
}

/* --------------------------------------------------------------------- */
/*  Conditional instructions                                             */
/* --------------------------------------------------------------------- */

SLJIT2_API_FUNC_ATTRIBUTE struct sljit2_label* sljit2_emit_label(struct sljit2_compiler *compiler)
{
	struct sljit2_label *label;

	CHECK_ERROR_PTR();
	CHECK_PTR(check_sljit2_emit_label(compiler));

	if (compiler->last_label && compiler->last_label->size == compiler->size)
		return compiler->last_label;

	label = (struct sljit2_label*)ensure_abuf(compiler, sizeof(struct sljit2_label));
	PTR_FAIL_IF(!label);
	set_label(label, compiler);
	return label;
}

SLJIT2_API_FUNC_ATTRIBUTE struct sljit2_jump* sljit2_emit_jump(struct sljit2_compiler *compiler, sljit2_s32 type)
{
	struct sljit2_jump *jump;
	sljit2_u8 mask = ((type & 0xff) < SLJIT2_JUMP) ? get_cc(compiler, type & 0xff) : 0xf;

	CHECK_ERROR_PTR();
	CHECK_PTR(check_sljit2_emit_jump(compiler, type));

	/* record jump */
	jump = (struct sljit2_jump *)ensure_abuf(compiler, sizeof(struct sljit2_jump));
	PTR_FAIL_IF(!jump);
	set_jump(jump, compiler, type & SLJIT2_REWRITABLE_JUMP);
	jump->addr = compiler->size;

	/* emit jump instruction */
	type &= 0xff;
	if (type >= SLJIT2_FAST_CALL)
		PTR_FAIL_IF(push_inst(compiler, brasl(link_r, 0)));
	else
		PTR_FAIL_IF(push_inst(compiler, brcl(mask, 0)));

	return jump;
}

SLJIT2_API_FUNC_ATTRIBUTE struct sljit2_jump* sljit2_emit_call(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 arg_types)
{
	SLJIT2_UNUSED_ARG(arg_types);
	CHECK_ERROR_PTR();
	CHECK_PTR(check_sljit2_emit_call(compiler, type, arg_types));

	if (type & SLJIT2_CALL_RETURN) {
		PTR_FAIL_IF(emit_stack_frame_release(compiler, r14));
		type = SLJIT2_JUMP | (type & SLJIT2_REWRITABLE_JUMP);
	}

	SLJIT2_SKIP_CHECKS(compiler);
	return sljit2_emit_jump(compiler, type);
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_ijump(struct sljit2_compiler *compiler, sljit2_s32 type, sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_gpr src_r = FAST_IS_REG(src) ? gpr(src) : tmp1;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_ijump(compiler, type, src, srcw));

	if (src == SLJIT2_IMM) {
		SLJIT2_ASSERT(!(srcw & 1)); /* target address must be even */
		FAIL_IF(push_load_imm_inst(compiler, src_r, srcw));
	}
	else if (src & SLJIT2_MEM) {
		ADJUST_LOCAL_OFFSET(src, srcw);
		FAIL_IF(load_word(compiler, src_r, src, srcw, 0 /* 64-bit */));
	}

	/* emit jump instruction */
	if (type >= SLJIT2_FAST_CALL)
		return push_inst(compiler, basr(link_r, src_r));

	return push_inst(compiler, br(src_r));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_icall(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 arg_types,
	sljit2_s32 src, sljit2_sw srcw)
{
	SLJIT2_UNUSED_ARG(arg_types);

	CHECK_ERROR();
	CHECK(check_sljit2_emit_icall(compiler, type, arg_types, src, srcw));

	SLJIT2_ASSERT(gpr(TMP_REG2) == tmp1);

	if (src & SLJIT2_MEM) {
		ADJUST_LOCAL_OFFSET(src, srcw);
		FAIL_IF(load_word(compiler, tmp1, src, srcw, 0 /* 64-bit */));
		src = TMP_REG2;
		srcw = 0;
	}

	if (type & SLJIT2_CALL_RETURN) {
		if (src >= SLJIT2_FIRST_SAVED_REG && src <= (SLJIT2_S0 - SLJIT2_KEPT_SAVEDS_COUNT(compiler->options))) {
			FAIL_IF(push_inst(compiler, lgr(tmp1, gpr(src))));
			src = TMP_REG2;
			srcw = 0;
		}

		FAIL_IF(emit_stack_frame_release(compiler, r14));
		type = SLJIT2_JUMP;
	}

	SLJIT2_SKIP_CHECKS(compiler);
	return sljit2_emit_ijump(compiler, type, src, srcw);
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op_flags(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 type)
{
	sljit2_gpr dst_r = FAST_IS_REG(dst) ? gpr(dst & REG_MASK) : tmp0;
	sljit2_gpr loc_r = tmp1;
	sljit2_u8 mask = get_cc(compiler, type);

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op_flags(compiler, op, dst, dstw, type));

	switch (GET_OPCODE(op)) {
	case SLJIT2_AND:
	case SLJIT2_OR:
	case SLJIT2_XOR:
		compiler->status_flags_state = op & SLJIT2_SET_Z;

		/* dst is also source operand */
		if (dst & SLJIT2_MEM)
			FAIL_IF(load_word(compiler, dst_r, dst, dstw, op & SLJIT2_32));

		break;
	case SLJIT2_MOV32:
		op |= SLJIT2_32;
		/* fallthrough */
	case SLJIT2_MOV:
		/* can write straight into destination */
		loc_r = dst_r;
		break;
	default:
		SLJIT2_UNREACHABLE();
	}

	/* TODO(mundaym): fold into cmov helper function? */
	#define LEVAL(i) i(loc_r, 1, mask)
	if (have_lscond2()) {
		FAIL_IF(push_load_imm_inst(compiler, loc_r, 0));
		FAIL_IF(push_inst(compiler,
			WHEN2(op & SLJIT2_32, lochi, locghi)));
	} else {
		FAIL_IF(push_load_imm_inst(compiler, loc_r, 1));
		FAIL_IF(push_inst(compiler, brc(mask, 2 + 2)));
		FAIL_IF(push_load_imm_inst(compiler, loc_r, 0));
	}
	#undef LEVAL

	/* apply bitwise op and set condition codes */
	switch (GET_OPCODE(op)) {
	#define LEVAL(i) i(dst_r, loc_r)
	case SLJIT2_AND:
		FAIL_IF(push_inst(compiler,
			WHEN2(op & SLJIT2_32, nr, ngr)));
		break;
	case SLJIT2_OR:
		FAIL_IF(push_inst(compiler,
			WHEN2(op & SLJIT2_32, or, ogr)));
		break;
	case SLJIT2_XOR:
		FAIL_IF(push_inst(compiler,
			WHEN2(op & SLJIT2_32, xr, xgr)));
		break;
	#undef LEVAL
	}

	/* store result to memory if required */
	if (dst & SLJIT2_MEM)
		return store_word(compiler, dst_r, dst, dstw, (op & SLJIT2_32));

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_select(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 dst_reg,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2_reg)
{
	sljit2_ins mask;
	sljit2_gpr src_r;
	sljit2_gpr dst_r = gpr(dst_reg);
	sljit2_ins ins;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_select(compiler, type, dst_reg, src1, src1w, src2_reg));

	ADJUST_LOCAL_OFFSET(src1, src1w);

	if (dst_reg != src2_reg) {
		if (src1 == dst_reg) {
			src1 = src2_reg;
			src1w = 0;
			type ^= 0x1;
		} else {
			if (ADDRESSING_DEPENDS_ON(src1, dst_reg)) {
				FAIL_IF(load_word(compiler, dst_r, src1, src1w, type & SLJIT2_32));
				src1 = src2_reg;
				src1w = 0;
				type ^= 0x1;
			} else
				FAIL_IF(push_inst(compiler, ((type & SLJIT2_32) ? 0x1800 /* lr */ : 0xb9040000 /* lgr */) | R4A(dst_r) | R0A(gpr(src2_reg))));
		}
	}

	mask = get_cc(compiler, type & ~SLJIT2_32);

	if (src1 & SLJIT2_MEM) {
		if (src1 & OFFS_REG_MASK) {
			src_r = gpr(OFFS_REG(src1));

			if (src1w != 0) {
				FAIL_IF(push_inst(compiler, 0xeb000000000d /* sllg */ | R36A(tmp1) | R32A(src_r) | ((sljit2_ins)(src1w & 0x3) << 16)));
				src_r = tmp1;
			}

			FAIL_IF(push_inst(compiler, 0xb9e80000 /* agrk */ | R12A(src_r) | R4A(tmp1) | R0A(gpr(src1 & REG_MASK))));
			src_r = tmp1;
			src1w = 0;
		} else if (!is_s20(src1w)) {
			FAIL_IF(push_load_imm_inst(compiler, tmp1, src1w));

			if (src1 & REG_MASK)
				FAIL_IF(push_inst(compiler, 0xb9e80000 /* agrk */ | R12A(tmp1) | R4A(tmp1) | R0A(gpr(src1 & REG_MASK))));

			src_r = tmp1;
			src1w = 0;
		} else
			src_r = gpr(src1 & REG_MASK);

		ins = (type & SLJIT2_32) ? 0xeb00000000f2 /* loc */ : 0xeb00000000e2 /* locg */;
		return push_inst(compiler, ins | R36A(dst_r) | (mask << 32) | R28A(src_r) | disp_s20((sljit2_s32)src1w));
	}

	if (src1 == SLJIT2_IMM) {
		if (type & SLJIT2_32)
			src1w = (sljit2_s32)src1w;

		if (have_lscond2() && is_s16(src1w)) {
			ins = (type & SLJIT2_32) ? 0xec0000000042 /* lochi */ : 0xec0000000046 /* locghi */;
			return push_inst(compiler, ins | R36A(dst_r) | (mask << 32) | (sljit2_ins)(src1w & 0xffff) << 16);
		}

		FAIL_IF(push_load_imm_inst(compiler, tmp1, src1w));
		src_r = tmp1;
	} else
		src_r = gpr(src1);

	ins = (type & SLJIT2_32) ? 0xb9f20000 /* locr */ : 0xb9e20000 /* locgr */;
	return push_inst(compiler, ins | (mask << 12) | R4A(dst_r) | R0A(src_r));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_fselect(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 dst_freg,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2_freg)
{
	sljit2_ins ins;
	struct sljit2_label *label;
	struct sljit2_jump *jump;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_fselect(compiler, type, dst_freg, src1, src1w, src2_freg));

	ADJUST_LOCAL_OFFSET(src1, src1w);

	if (dst_freg != src2_freg) {
		if (dst_freg == src1) {
			src1 = src2_freg;
			src1w = 0;
			type ^= 0x1;
		} else {
			ins = (type & SLJIT2_32) ? 0x3800 /* ler */ : 0x2800 /* ldr */;
			FAIL_IF(push_inst(compiler, ins | F4(dst_freg) | F0(src2_freg)));
		}
	}

	SLJIT2_SKIP_CHECKS(compiler);
	jump = sljit2_emit_jump(compiler, (type & ~SLJIT2_32) ^ 0x1);
	FAIL_IF(!jump);

	if (!(src1 & SLJIT2_MEM)) {
		ins = (type & SLJIT2_32) ? 0x3800 /* ler */ : 0x2800 /* ldr */;
		FAIL_IF(push_inst(compiler, ins | F4(dst_freg) | F0(src1)));
	} else
		FAIL_IF(float_mem(compiler, FLOAT_LOAD | (type & SLJIT2_32), dst_freg, src1, src1w));

	SLJIT2_SKIP_CHECKS(compiler);
	label = sljit2_emit_label(compiler);
	FAIL_IF(!label);

	sljit2_set_label(jump, label);
	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_mem(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 reg,
	sljit2_s32 mem, sljit2_sw memw)
{
	sljit2_ins ins, reg1, reg2, base, offs = 0;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_mem(compiler, type, reg, mem, memw));

	if (!(reg & REG_PAIR_MASK))
		return sljit2_emit_mem_unaligned(compiler, type, reg, mem, memw);

	ADJUST_LOCAL_OFFSET(mem, memw);

	base = gpr(mem & REG_MASK);
	reg1 = gpr(REG_PAIR_FIRST(reg));
	reg2 = gpr(REG_PAIR_SECOND(reg));

	if (mem & OFFS_REG_MASK) {
		memw &= 0x3;
		offs = gpr(OFFS_REG(mem));

		if (memw != 0) {
			FAIL_IF(push_inst(compiler, 0xeb000000000d /* sllg */ | R36A(tmp1) | R32A(offs) | ((sljit2_ins)memw << 16)));
			offs = tmp1;
		} else if (!(type & SLJIT2_MEM_STORE) && (base == reg1 || base == reg2) && (offs == reg1 || offs == reg2)) {
			FAIL_IF(push_inst(compiler, 0xb9f80000 | R12A(tmp1) | R4A(base) | R0A(offs)));
			base = tmp1;
			offs = 0;
		}

		memw = 0;
	} else if (memw < -0x80000 || memw > 0x7ffff - ((reg2 == reg1 + 1) ? 0 : SSIZE_OF(sw))) {
		FAIL_IF(push_load_imm_inst(compiler, tmp1, memw));

		if (base == 0)
			base = tmp1;
		else
			offs = tmp1;

		memw = 0;
	}

	if (offs == 0 && reg2 == (reg1 + 1)) {
		ins = (type & SLJIT2_MEM_STORE) ? 0xeb0000000024 /* stmg */ : 0xeb0000000004 /* lmg */;
		return push_inst(compiler, ins | R36A(reg1) | R32A(reg2) | R28A(base) | disp_s20((sljit2_s32)memw));
	}

	ins = ((type & SLJIT2_MEM_STORE) ? 0xe30000000024 /* stg */ : 0xe30000000004 /* lg */) | R32A(offs) | R28A(base);

	if (!(type & SLJIT2_MEM_STORE) && base == reg1) {
		FAIL_IF(push_inst(compiler, ins | R36A(reg2) | disp_s20((sljit2_s32)memw + SSIZE_OF(sw))));
		return push_inst(compiler, ins | R36A(reg1) | disp_s20((sljit2_s32)memw));
	}

	FAIL_IF(push_inst(compiler, ins | R36A(reg1) | disp_s20((sljit2_s32)memw)));
	return push_inst(compiler, ins | R36A(reg2) | disp_s20((sljit2_s32)memw + SSIZE_OF(sw)));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_simd_mov(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 vreg,
	sljit2_s32 srcdst, sljit2_sw srcdstw)
{
	sljit2_s32 reg_size = SLJIT2_SIMD_GET_REG_SIZE(type);
	sljit2_s32 elem_size = SLJIT2_SIMD_GET_ELEM_SIZE(type);
	sljit2_s32 alignment = SLJIT2_SIMD_GET_ELEM2_SIZE(type);
	struct addr addr;
	sljit2_ins ins;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_simd_mov(compiler, type, vreg, srcdst, srcdstw));

	ADJUST_LOCAL_OFFSET(srcdst, srcdstw);

	if (reg_size != 4)
		return SLJIT2_ERR_UNSUPPORTED;

	if ((type & SLJIT2_SIMD_FLOAT) && (elem_size < 2 || elem_size > 3))
		return SLJIT2_ERR_UNSUPPORTED;

	if (type & SLJIT2_SIMD_TEST)
		return SLJIT2_SUCCESS;

	if (!(srcdst & SLJIT2_MEM)) {
		if (type & SLJIT2_SIMD_STORE)
			ins = F36(srcdst) | F32(vreg);
		else
			ins = F36(vreg) | F32(srcdst);

		return push_inst(compiler, 0xe70000000056 /* vlr */ | ins);
	}

	FAIL_IF(make_addr_bx(compiler, &addr, srcdst, srcdstw, tmp1));
	ins = F36(vreg) | R32A(addr.index) | R28A(addr.base) | disp_s20(addr.offset);

	if (alignment >= 4)
		ins |= 4 << 12;
	else if (alignment == 3)
		ins |= 3 << 12;

	return push_inst(compiler, ((type & SLJIT2_SIMD_STORE) ? 0xe7000000000e /* vst */ : 0xe70000000006 /* vl */) | ins);
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_simd_replicate(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 vreg,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_s32 reg_size = SLJIT2_SIMD_GET_REG_SIZE(type);
	sljit2_s32 elem_size = SLJIT2_SIMD_GET_ELEM_SIZE(type);
	struct addr addr;
	sljit2_gpr reg;
	sljit2_sw sign_ext;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_simd_replicate(compiler, type, vreg, src, srcw));

	ADJUST_LOCAL_OFFSET(src, srcw);

	if (reg_size != 4)
		return SLJIT2_ERR_UNSUPPORTED;

	if ((type & SLJIT2_SIMD_FLOAT) && elem_size < 2)
		return SLJIT2_ERR_UNSUPPORTED;

	if (type & SLJIT2_SIMD_TEST)
		return SLJIT2_SUCCESS;

	if (src & SLJIT2_MEM) {
		FAIL_IF(make_addr_bx(compiler, &addr, src, srcw, tmp1));
		return push_inst(compiler, 0xe70000000005 /* vlrep */ | F36(vreg)
			| R32A(addr.index) | R28A(addr.base) | disp_s20(addr.offset) | ((sljit2_ins)elem_size << 12));
	}

	if (type & SLJIT2_SIMD_FLOAT) {
		if (src == SLJIT2_IMM)
			return push_inst(compiler, 0xe70000000044 /* vgbm */ | F36(vreg));

		return push_inst(compiler, 0xe7000000004d /* vrep */ | F36(vreg) | F32(src) | ((sljit2_ins)elem_size << 12));
	}

	if (src == SLJIT2_IMM) {
		sign_ext = 0x10000;

		switch (elem_size) {
		case 0:
			srcw &= 0xff;
			sign_ext = (sljit2_s8)srcw;
			break;
		case 1:
			srcw &= 0xffff;
			sign_ext = (sljit2_s16)srcw;
			break;
		case 2:
			if ((sljit2_s32)srcw == (sljit2_s16)srcw) {
				srcw &= 0xffff;
				sign_ext = (sljit2_s16)srcw;
			} else
				srcw &= 0xffffffff;
			break;
		default:
			if (srcw == (sljit2_s16)srcw) {
				srcw &= 0xffff;
				sign_ext = (sljit2_s16)srcw;
			}
			break;
		}

		if (sign_ext != 0x10000) {
			if (sign_ext == 0 || sign_ext == -1)
				return push_inst(compiler, 0xe70000000044 /* vgbm */ | F36(vreg)
					| (sign_ext == 0 ? 0 : ((sljit2_ins)0xffff << 16)));

			return push_inst(compiler, 0xe70000000045 /* vrepi */ | F36(vreg)
				| ((sljit2_ins)srcw << 16) | ((sljit2_ins)elem_size << 12));
		}

		push_load_imm_inst(compiler, tmp0, srcw);
		reg = tmp0;
	} else
		reg = gpr(src);

	FAIL_IF(push_inst(compiler, 0xe70000000022 /* vlvg */ | F36(vreg) | R32A(reg) | ((sljit2_ins)elem_size << 12)));
	return push_inst(compiler, 0xe7000000004d /* vrep */ | F36(vreg) | F32(vreg) | ((sljit2_ins)elem_size << 12));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_simd_lane_mov(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 vreg, sljit2_s32 lane_index,
	sljit2_s32 srcdst, sljit2_sw srcdstw)
{
	sljit2_s32 reg_size = SLJIT2_SIMD_GET_REG_SIZE(type);
	sljit2_s32 elem_size = SLJIT2_SIMD_GET_ELEM_SIZE(type);
	struct addr addr;
	sljit2_gpr reg;
	sljit2_ins ins = 0;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_simd_lane_mov(compiler, type, vreg, lane_index, srcdst, srcdstw));

	ADJUST_LOCAL_OFFSET(srcdst, srcdstw);

	if (reg_size != 4)
		return SLJIT2_ERR_UNSUPPORTED;

	if ((type & SLJIT2_SIMD_FLOAT) && elem_size < 2)
		return SLJIT2_ERR_UNSUPPORTED;

	if (type & SLJIT2_SIMD_TEST)
		return SLJIT2_SUCCESS;

	if (srcdst & SLJIT2_MEM) {
		FAIL_IF(make_addr_bx(compiler, &addr, srcdst, srcdstw, tmp1));
		ins = F36(vreg) | R32A(addr.index) | R28A(addr.base) | disp_s20(addr.offset);
	}

	if (type & SLJIT2_SIMD_LANE_ZERO) {
		if ((srcdst & SLJIT2_MEM) && lane_index == ((1 << (3 - elem_size)) - 1))
			return push_inst(compiler, 0xe70000000004 /* vllez */ | ins | ((sljit2_ins)elem_size << 12));

		if ((type & SLJIT2_SIMD_FLOAT) && vreg == srcdst) {
			FAIL_IF(push_inst(compiler, 0xe70000000056 /* vlr */ | F36(TMP_FREG1) | F32(vreg)));
			srcdst = TMP_FREG1;
			srcdstw = 0;
		}

		FAIL_IF(push_inst(compiler, 0xe70000000044 /* vgbm */ | F36(vreg)));
	}

	if (srcdst & SLJIT2_MEM) {
		switch (elem_size) {
		case 0:
			ins |= 0xe70000000000 /* vleb */;
			break;
		case 1:
			ins |= 0xe70000000001 /* vleh */;
			break;
		case 2:
			ins |= 0xe70000000003 /* vlef */;
			break;
		default:
			ins |= 0xe70000000002 /* vleg */;
			break;
		}

		/* Convert to vsteb - vsteg  */
		if (type & SLJIT2_SIMD_STORE)
			ins |= 0x8;

		return push_inst(compiler, ins | ((sljit2_ins)lane_index << 12));
	}

	if (type & SLJIT2_SIMD_FLOAT) {
		if (type & SLJIT2_SIMD_STORE)
			return push_inst(compiler, 0xe7000000004d /* vrep */ | F36(srcdst) | F32(vreg) | ((sljit2_ins)lane_index << 16) | ((sljit2_ins)elem_size << 12));

		if (elem_size == 3) {
			if (lane_index == 0)
				ins = F32(srcdst) | F28(vreg) | (1 << 12);
			else
				ins = F32(vreg) | F28(srcdst);

			return push_inst(compiler, 0xe70000000084 /* vpdi */ | F36(vreg) | ins);
		}

		FAIL_IF(push_inst(compiler, 0xe70000000021 /* vlgv */ | R36A(tmp0) | F32(srcdst) | ((sljit2_ins)2 << 12)));
		return push_inst(compiler, 0xe70000000022 /* vlvg */ | F36(vreg) | R32A(tmp0) | ((sljit2_ins)lane_index << 16) | ((sljit2_ins)2 << 12));
	}

	if (srcdst == SLJIT2_IMM) {
		switch (elem_size) {
		case 0:
			ins = 0xe70000000040 /* vleib */;
			srcdstw &= 0xff;
			break;
		case 1:
			ins = 0xe70000000041 /* vleih */;
			srcdstw &= 0xffff;
			break;
		case 2:
			if ((sljit2_s32)srcdstw == (sljit2_s16)srcdstw) {
				srcdstw &= 0xffff;
				ins = 0xe70000000043 /* vleif */;
			} else
				srcdstw &= 0xffffffff;
			break;
		default:
			if (srcdstw == (sljit2_s16)srcdstw) {
				srcdstw &= 0xffff;
				ins = 0xe70000000042 /* vleig */;
			}
			break;
		}

		if (ins != 0)
			return push_inst(compiler, ins | F36(vreg) | ((sljit2_ins)srcdstw << 16) | ((sljit2_ins)lane_index << 12));

		push_load_imm_inst(compiler, tmp0, srcdstw);
		reg = tmp0;
	} else
		reg = gpr(srcdst);

	ins = ((sljit2_ins)lane_index << 16) | ((sljit2_ins)elem_size << 12);

	if (!(type & SLJIT2_SIMD_STORE))
		return push_inst(compiler, 0xe70000000022 /* vlvg */ | F36(vreg) | R32A(reg) | ins);

	FAIL_IF(push_inst(compiler, 0xe70000000021 /* vlgv */ | R36A(reg) | F32(vreg) | ins));

	if (!(type & SLJIT2_SIMD_LANE_SIGNED) || elem_size >= 3)
		return SLJIT2_SUCCESS;

	switch (elem_size) {
	case 0:
		ins = 0xb9060000 /* lgbr */;
		break;
	case 1:
		ins = 0xb9070000 /* lghr */;
		break;
	default:
		ins = 0xb9140000 /* lgfr */;
		break;
	}

	return push_inst(compiler, ins | R4A(reg) | R0A(reg));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_simd_lane_replicate(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 vreg,
	sljit2_s32 src, sljit2_s32 src_lane_index)
{
	sljit2_s32 reg_size = SLJIT2_SIMD_GET_REG_SIZE(type);
	sljit2_s32 elem_size = SLJIT2_SIMD_GET_ELEM_SIZE(type);

	CHECK_ERROR();
	CHECK(check_sljit2_emit_simd_lane_replicate(compiler, type, vreg, src, src_lane_index));

	if (reg_size != 4)
		return SLJIT2_ERR_UNSUPPORTED;

	if ((type & SLJIT2_SIMD_FLOAT) && elem_size < 2)
		return SLJIT2_ERR_UNSUPPORTED;

	if (type & SLJIT2_SIMD_TEST)
		return SLJIT2_SUCCESS;

	return push_inst(compiler, 0xe7000000004d /* vrep */ | F36(vreg) | F32(src)
		| ((sljit2_ins)src_lane_index << 16) | ((sljit2_ins)elem_size << 12));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_simd_extend(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 vreg,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_s32 reg_size = SLJIT2_SIMD_GET_REG_SIZE(type);
	sljit2_s32 elem_size = SLJIT2_SIMD_GET_ELEM_SIZE(type);
	sljit2_s32 elem2_size = SLJIT2_SIMD_GET_ELEM2_SIZE(type);
	struct addr addr;
	sljit2_ins ins;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_simd_extend(compiler, type, vreg, src, srcw));

	ADJUST_LOCAL_OFFSET(src, srcw);

	if (reg_size != 4)
		return SLJIT2_ERR_UNSUPPORTED;

	if ((type & SLJIT2_SIMD_FLOAT) && elem_size < 2)
		return SLJIT2_ERR_UNSUPPORTED;

	if (type & SLJIT2_SIMD_TEST)
		return SLJIT2_SUCCESS;

	if (src & SLJIT2_MEM) {
		FAIL_IF(make_addr_bx(compiler, &addr, src, srcw, tmp1));
		ins = F36(vreg) | R32A(addr.index) | R28A(addr.base) | disp_s20(addr.offset);

		switch (elem2_size - elem_size) {
		case 1:
			ins |= 0xe70000000002 /* vleg */;
			break;
		case 2:
			ins |= 0xe70000000003 /* vlef */;
			break;
		default:
			ins |= 0xe70000000001 /* vleh */;
			break;
		}

		FAIL_IF(push_inst(compiler, ins));
		src = vreg;
	}

	if (type & SLJIT2_SIMD_FLOAT) {
		FAIL_IF(push_inst(compiler, 0xe700000000d5 /* vuplh */ | F36(vreg) | F32(src) | (2 << 12)));
		FAIL_IF(push_inst(compiler, 0xe70000000030 /* vesl */ | F36(vreg) | F32(vreg) | (32 << 16) | (3 << 12)));
		return push_inst(compiler, 0xe700000000c4 /* vfll */ | F36(vreg) | F32(vreg) | (2 << 12));
	}

	ins = ((type & SLJIT2_SIMD_EXTEND_SIGNED) ? 0xe700000000d7 /* vuph */ : 0xe700000000d5 /* vuplh */) | F36(vreg);

	do {
		FAIL_IF(push_inst(compiler, ins | F32(src) | ((sljit2_ins)elem_size << 12)));
		src = vreg;
	} while (++elem_size < elem2_size);

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_simd_sign(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 vreg,
	sljit2_s32 dst, sljit2_sw dstw)
{
	sljit2_s32 reg_size = SLJIT2_SIMD_GET_REG_SIZE(type);
	sljit2_s32 elem_size = SLJIT2_SIMD_GET_ELEM_SIZE(type);
	sljit2_gpr dst_r;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_simd_sign(compiler, type, vreg, dst, dstw));

	ADJUST_LOCAL_OFFSET(dst, dstw);

	if (reg_size != 4)
		return SLJIT2_ERR_UNSUPPORTED;

	if ((type & SLJIT2_SIMD_FLOAT) && elem_size < 2)
		return SLJIT2_ERR_UNSUPPORTED;

	if (type & SLJIT2_SIMD_TEST)
		return SLJIT2_SUCCESS;

	switch (elem_size) {
	case 0:
		push_load_imm_inst(compiler, tmp0, (sljit2_sw)0x4048505860687078);
		push_load_imm_inst(compiler, tmp1, (sljit2_sw)0x0008101820283038);
		FAIL_IF(push_inst(compiler, 0xe70000000062 /* vlvgp */ | F36(TMP_FREG1) | R32A(tmp1) | R28A(tmp0)));
		break;
	case 1:
		push_load_imm_inst(compiler, tmp0, (sljit2_sw)0x0010203040506070);
		break;
	case 2:
		push_load_imm_inst(compiler, tmp0, (sljit2_sw)0x8080808000204060);
		break;
	default:
		push_load_imm_inst(compiler, tmp0, (sljit2_sw)0x8080808080800040);
		break;
	}

	if (elem_size != 0)
		FAIL_IF(push_inst(compiler, 0xe70000000022 /* vlvg */ | F36(TMP_FREG1) | R32A(tmp0) | (1 << 16) | (3 << 12)));

	FAIL_IF(push_inst(compiler, 0xe70000000085 /* vbperm */ | F36(TMP_FREG1) | F32(vreg) | F28(TMP_FREG1)));

	dst_r = FAST_IS_REG(dst) ? gpr(dst) : tmp0;
	FAIL_IF(push_inst(compiler, 0xe70000000021 /* vlgv */ | R36A(dst_r) | F32(TMP_FREG1)
		| (elem_size == 0 ? ((3 << 16) | (1 << 12)) : (7 << 16))));

	if (dst_r == tmp0)
		return store_word(compiler, tmp0, dst, dstw, type & SLJIT2_32);

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_simd_op2(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 dst_vreg, sljit2_s32 src1_vreg, sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_s32 reg_size = SLJIT2_SIMD_GET_REG_SIZE(type);
	sljit2_s32 elem_size = SLJIT2_SIMD_GET_ELEM_SIZE(type);
	sljit2_s32 alignment;
	struct addr addr;
	sljit2_ins ins = 0, load_ins;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_simd_op2(compiler, type, dst_vreg, src1_vreg, src2, src2w));
	ADJUST_LOCAL_OFFSET(src2, src2w);

	if (reg_size != 4)
		return SLJIT2_ERR_UNSUPPORTED;

	if ((type & SLJIT2_SIMD_FLOAT) && (elem_size < 2 || elem_size > 3))
		return SLJIT2_ERR_UNSUPPORTED;

	if (type & SLJIT2_SIMD_TEST)
		return SLJIT2_SUCCESS;

	switch (SLJIT2_SIMD_GET_OPCODE(type)) {
	case SLJIT2_SIMD_OP2_AND:
		ins = 0xe70000000068 /* vn */;
		break;
	case SLJIT2_SIMD_OP2_OR:
		ins = 0xe7000000006a /* vo */;
		break;
	case SLJIT2_SIMD_OP2_XOR:
		ins = 0xe7000000006d /* vx */;
		break;
	case SLJIT2_SIMD_OP2_SHUFFLE:
		ins = 0xe7000000008c /* vperm */;
		break;
	}

	if (src2 & SLJIT2_MEM) {
		FAIL_IF(make_addr_bx(compiler, &addr, src2, src2w, tmp1));
		load_ins = 0xe70000000006 /* vl */ | F36(TMP_FREG1) | R32A(addr.index) | R28A(addr.base) | disp_s20(addr.offset);
		alignment = SLJIT2_SIMD_GET_ELEM2_SIZE(type);

		if (alignment >= 4)
			load_ins |= 4 << 12;
		else if (alignment == 3)
			load_ins |= 3 << 12;

		FAIL_IF(push_inst(compiler, load_ins));
		src2 = TMP_FREG1;
	}

	if (SLJIT2_SIMD_GET_OPCODE(type) == SLJIT2_SIMD_OP2_SHUFFLE)
		return push_inst(compiler, ins | F36(dst_vreg) | F32(src1_vreg) | F28(src1_vreg) | F12(src2));

	return push_inst(compiler, ins | F36(dst_vreg) | F32(src1_vreg) | F28(src2));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_atomic_load(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst_reg,
	sljit2_s32 mem_reg)
{
	CHECK_ERROR();
	CHECK(check_sljit2_emit_atomic_load(compiler, op, dst_reg, mem_reg));

	if (op & SLJIT2_ATOMIC_USE_LS)
		return SLJIT2_ERR_UNSUPPORTED;

	switch (GET_OPCODE(op)) {
	case SLJIT2_MOV32:
	case SLJIT2_MOV_U32:
	case SLJIT2_MOV:
	case SLJIT2_MOV_P:
		if (op & SLJIT2_ATOMIC_TEST)
			return SLJIT2_SUCCESS;

		SLJIT2_SKIP_CHECKS(compiler);
		return sljit2_emit_op1(compiler, op & ~SLJIT2_ATOMIC_USE_CAS, dst_reg, 0, SLJIT2_MEM1(mem_reg), 0);
	default:
		return SLJIT2_ERR_UNSUPPORTED;
	}
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_atomic_store(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 src_reg,
	sljit2_s32 mem_reg,
	sljit2_s32 temp_reg)
{
	sljit2_ins ins;
	sljit2_gpr tmp_r = gpr(temp_reg);
	sljit2_gpr mem_r = gpr(mem_reg);

	CHECK_ERROR();
	CHECK(check_sljit2_emit_atomic_store(compiler, op, src_reg, mem_reg, temp_reg));

	if (op & SLJIT2_ATOMIC_USE_LS)
		return SLJIT2_ERR_UNSUPPORTED;

	switch (GET_OPCODE(op)) {
	case SLJIT2_MOV32:
	case SLJIT2_MOV_U32:
		ins = 0xba000000 /* cs */ | R20A(tmp_r) | R16A(gpr(src_reg)) | R12A(mem_r);
		break;
	case SLJIT2_MOV:
	case SLJIT2_MOV_P:
		ins = 0xeb0000000030 /* csg */ | R36A(tmp_r) | R32A(gpr(src_reg)) | R28A(mem_r);
		break;
	default:
		return SLJIT2_ERR_UNSUPPORTED;
	}

	if (op & SLJIT2_ATOMIC_TEST)
		return SLJIT2_SUCCESS;

	return push_inst(compiler, ins);
}

/* --------------------------------------------------------------------- */
/*  Other instructions                                                   */
/* --------------------------------------------------------------------- */

/* On s390x we build a literal pool to hold constants. This has two main
   advantages:

     1. we only need one instruction in the instruction stream (LGRL)
     2. we can store 64 bit addresses and use 32 bit offsets

   To retrofit the extra information needed to build the literal pool we
   add a new sljit2_s390x_const struct that contains the initial value but
   can still be cast to a sljit2_const. */

SLJIT2_API_FUNC_ATTRIBUTE struct sljit2_const* sljit2_emit_const(struct sljit2_compiler *compiler, sljit2_s32 dst, sljit2_sw dstw, sljit2_sw init_value)
{
	struct sljit2_s390x_const *const_;
	sljit2_gpr dst_r;

	CHECK_ERROR_PTR();
	CHECK_PTR(check_sljit2_emit_const(compiler, dst, dstw, init_value));

	const_ = (struct sljit2_s390x_const*)ensure_abuf(compiler,
					sizeof(struct sljit2_s390x_const));
	PTR_FAIL_IF(!const_);
	set_const((struct sljit2_const*)const_, compiler);
	const_->init_value = init_value;

	dst_r = FAST_IS_REG(dst) ? gpr(dst & REG_MASK) : tmp0;
	if (have_genext())
		PTR_FAIL_IF(push_inst(compiler, lgrl(dst_r, 0)));
	else {
		PTR_FAIL_IF(push_inst(compiler, larl(tmp1, 0)));
		PTR_FAIL_IF(push_inst(compiler, lg(dst_r, 0, r0, tmp1)));
	}

	if (dst & SLJIT2_MEM)
		PTR_FAIL_IF(store_word(compiler, dst_r, dst, dstw, 0 /* always 64-bit */));

	return (struct sljit2_const*)const_;
}

SLJIT2_API_FUNC_ATTRIBUTE void sljit2_set_jump_addr(sljit2_uw addr, sljit2_uw new_target, sljit2_sw executable_offset)
{
	/* Update the constant pool. */
	sljit2_uw *ptr = (sljit2_uw *)addr;
	SLJIT2_UNUSED_ARG(executable_offset);

	SLJIT2_UPDATE_WX_FLAGS(ptr, ptr + 1, 0);
	*ptr = new_target;
	SLJIT2_UPDATE_WX_FLAGS(ptr, ptr + 1, 1);
	SLJIT2_CACHE_FLUSH(ptr, ptr + 1);
}

SLJIT2_API_FUNC_ATTRIBUTE void sljit2_set_const(sljit2_uw addr, sljit2_sw new_constant, sljit2_sw executable_offset)
{
	sljit2_set_jump_addr(addr, (sljit2_uw)new_constant, executable_offset);
}

SLJIT2_API_FUNC_ATTRIBUTE struct sljit2_jump* sljit2_emit_mov_addr(struct sljit2_compiler *compiler, sljit2_s32 dst, sljit2_sw dstw)
{
	struct sljit2_jump *jump;
	sljit2_gpr dst_r;

	CHECK_ERROR_PTR();
	CHECK_PTR(check_sljit2_emit_mov_addr(compiler, dst, dstw));
	ADJUST_LOCAL_OFFSET(dst, dstw);

	jump = (struct sljit2_jump*)ensure_abuf(compiler, sizeof(struct sljit2_jump));
	PTR_FAIL_IF(!jump);
	set_mov_addr(jump, compiler, 0);

	dst_r = FAST_IS_REG(dst) ? gpr(dst & REG_MASK) : tmp0;

	if (have_genext())
		PTR_FAIL_IF(push_inst(compiler, lgrl(dst_r, 0)));
	else {
		PTR_FAIL_IF(push_inst(compiler, larl(tmp1, 0)));
		PTR_FAIL_IF(push_inst(compiler, lg(dst_r, 0, r0, tmp1)));
	}

	if (dst & SLJIT2_MEM)
		PTR_FAIL_IF(store_word(compiler, dst_r, dst, dstw, 0));

	return jump;
}

/* TODO(carenas): EVAL probably should move up or be refactored */
#undef WHEN2
#undef EVAL

#undef tmp1
#undef tmp0

/* TODO(carenas): undef other macros that spill like is_u12? */
