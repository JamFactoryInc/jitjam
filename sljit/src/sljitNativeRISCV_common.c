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

SLJIT2_API_FUNC_ATTRIBUTE const char* sljit2_get_platform_name(void)
{
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	return "RISC-V-32" SLJIT2_CPUINFO;
#else /* !SLJIT2_CONFIG_RISCV_32 */
	return "RISC-V-64" SLJIT2_CPUINFO;
#endif /* SLJIT2_CONFIG_RISCV_32 */
}

/* Length of an instruction word
   Both for riscv-32 and riscv-64 */
typedef sljit2_u32 sljit2_ins;

#define TMP_REG1	(SLJIT2_NUMBER_OF_REGISTERS + 2)
#define TMP_REG2	(SLJIT2_NUMBER_OF_REGISTERS + 3)
#define TMP_REG3	(SLJIT2_NUMBER_OF_REGISTERS + 4)
#define TMP_ZERO	0

/* Flags are kept in volatile registers. */
#define EQUAL_FLAG	(SLJIT2_NUMBER_OF_REGISTERS + 5)
#define RETURN_ADDR_REG	TMP_REG2
#define OTHER_FLAG	(SLJIT2_NUMBER_OF_REGISTERS + 6)

#define TMP_FREG1	(SLJIT2_NUMBER_OF_FLOAT_REGISTERS + 1)
#define TMP_FREG2	(SLJIT2_NUMBER_OF_FLOAT_REGISTERS + 2)

#define TMP_VREG1	(SLJIT2_NUMBER_OF_VECTOR_REGISTERS + 1)
#define TMP_VREG2	(SLJIT2_NUMBER_OF_VECTOR_REGISTERS + 2)

static const sljit2_u8 reg_map[SLJIT2_NUMBER_OF_REGISTERS + 7] = {
	0, 10, 11, 12, 13, 14, 15, 16, 17, 29, 30, 31, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 9, 8, 2, 6, 1, 7, 5, 28
};

static const sljit2_u8 freg_map[SLJIT2_NUMBER_OF_FLOAT_REGISTERS + 3] = {
	0, 10, 11, 12, 13, 14, 15, 16, 17, 2, 3, 4, 5, 6, 7, 28, 29, 30, 31, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 9, 8, 0, 1,
};

static const sljit2_u8 vreg_map[SLJIT2_NUMBER_OF_VECTOR_REGISTERS + 3] = {
	0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31
};

/* --------------------------------------------------------------------- */
/*  Instrucion forms                                                     */
/* --------------------------------------------------------------------- */

#define RD(rd)		((sljit2_ins)reg_map[rd] << 7)
#define RS1(rs1)	((sljit2_ins)reg_map[rs1] << 15)
#define RS2(rs2)	((sljit2_ins)reg_map[rs2] << 20)
#define FRD(rd)		((sljit2_ins)freg_map[rd] << 7)
#define FRS1(rs1)	((sljit2_ins)freg_map[rs1] << 15)
#define FRS2(rs2)	((sljit2_ins)freg_map[rs2] << 20)
#define VRD(rd)		((sljit2_ins)vreg_map[rd] << 7)
#define VRS1(rs1)	((sljit2_ins)vreg_map[rs1] << 15)
#define VRS2(rs2)	((sljit2_ins)vreg_map[rs2] << 20)
#define IMM_I(imm)	((sljit2_ins)(imm) << 20)
#define IMM_S(imm)	((((sljit2_ins)(imm) & 0xfe0) << 20) | (((sljit2_ins)(imm) & 0x1f) << 7))

/* Represents funct(i) parts of the instructions. */
#define OPC(o)		((sljit2_ins)(o))
#define F3(f)		((sljit2_ins)(f) << 12)
#define F12(f)		((sljit2_ins)(f) << 20)
#define F7(f)		((sljit2_ins)(f) << 25)

/* Vector instruction types. */
#define OPFVF		(F3(0x5) | OPC(0x57))
#define OPFVV		(F3(0x1) | OPC(0x57))
#define OPIVI		(F3(0x3) | OPC(0x57))
#define OPIVV		(F3(0x0) | OPC(0x57))
#define OPIVX		(F3(0x4) | OPC(0x57))
#define OPMVV		(F3(0x2) | OPC(0x57))
#define OPMVX		(F3(0x6) | OPC(0x57))

#define ADD		(F7(0x0) | F3(0x0) | OPC(0x33))
#define ADDI		(F3(0x0) | OPC(0x13))
#define AND		(F7(0x0) | F3(0x7) | OPC(0x33))
#define ANDI		(F3(0x7) | OPC(0x13))
#define AUIPC		(OPC(0x17))
#define BEQ		(F3(0x0) | OPC(0x63))
#define BNE		(F3(0x1) | OPC(0x63))
#define BLT		(F3(0x4) | OPC(0x63))
#define BGE		(F3(0x5) | OPC(0x63))
#define BLTU		(F3(0x6) | OPC(0x63))
#define BGEU		(F3(0x7) | OPC(0x63))
#if defined __riscv_zbb
#define CLZ		(F7(0x30) | F3(0x1) | OPC(0x13))
#define CTZ		(F7(0x30) | F12(0x1) | F3(0x1) | OPC(0x13))
#endif /* __riscv_zbb */
#define DIV		(F7(0x1) | F3(0x4) | OPC(0x33))
#define DIVU		(F7(0x1) | F3(0x5) | OPC(0x33))
#define EBREAK		(F12(0x1) | F3(0x0) | OPC(0x73))
#define FADD_S		(F7(0x0) | F3(0x7) | OPC(0x53))
#define FDIV_S		(F7(0xc) | F3(0x7) | OPC(0x53))
#define FENCE		(F3(0x0) | OPC(0xf))
#define FEQ_S		(F7(0x50) | F3(0x2) | OPC(0x53))
#define FLD		(F3(0x3) | OPC(0x7))
#define FLE_S		(F7(0x50) | F3(0x0) | OPC(0x53))
#define FLT_S		(F7(0x50) | F3(0x1) | OPC(0x53))
/* These conversion opcodes are partly defined. */
#define FCVT_S_D	(F7(0x20) | OPC(0x53))
#define FCVT_S_W	(F7(0x68) | OPC(0x53))
#define FCVT_S_WU	(F7(0x68) | F12(0x1) | OPC(0x53))
#define FCVT_W_S	(F7(0x60) | F3(0x1) | OPC(0x53))
#define FMUL_S		(F7(0x8) | F3(0x7) | OPC(0x53))
#define FMV_X_W		(F7(0x70) | F3(0x0) | OPC(0x53))
#define FMV_W_X		(F7(0x78) | F3(0x0) | OPC(0x53))
#define FSD		(F3(0x3) | OPC(0x27))
#define FSGNJ_S		(F7(0x10) | F3(0x0) | OPC(0x53))
#define FSGNJN_S	(F7(0x10) | F3(0x1) | OPC(0x53))
#define FSGNJX_S	(F7(0x10) | F3(0x2) | OPC(0x53))
#define FSUB_S		(F7(0x4) | F3(0x7) | OPC(0x53))
#define FSW		(F3(0x2) | OPC(0x27))
#define JAL		(OPC(0x6f))
#define JALR		(F3(0x0) | OPC(0x67))
#define LD		(F3(0x3) | OPC(0x3))
#define LUI		(OPC(0x37))
#define LW		(F3(0x2) | OPC(0x3))
#define LR		(F7(0x8) | OPC(0x2f))
#define MUL		(F7(0x1) | F3(0x0) | OPC(0x33))
#define MULH		(F7(0x1) | F3(0x1) | OPC(0x33))
#define MULHU		(F7(0x1) | F3(0x3) | OPC(0x33))
#define OR		(F7(0x0) | F3(0x6) | OPC(0x33))
#define ORI		(F3(0x6) | OPC(0x13))
#define REM		(F7(0x1) | F3(0x6) | OPC(0x33))
#define REMU		(F7(0x1) | F3(0x7) | OPC(0x33))
#if defined __riscv_zbb
#if defined SLJIT2_CONFIG_RISCV_32
#define REV8		(F12(0x698) | F3(0x5) | OPC(0x13))
#elif defined SLJIT2_CONFIG_RISCV_64
#define REV8		(F12(0x6b8) | F3(0x5) | OPC(0x13))
#endif
#define ROL		(F7(0x30) | F3(0x1) | OPC(0x33))
#define ROR		(F7(0x30) | F3(0x5) | OPC(0x33))
#define RORI		(F7(0x30) | F3(0x5) | OPC(0x13))
#endif /* __riscv_zbb */
#define SC		(F7(0xc) | OPC(0x2f))
#define SD		(F3(0x3) | OPC(0x23))
#if defined __riscv_zbb
#define SEXTB		(F7(0x30) | F12(0x4) | F3(0x1) | OPC(0x13))
#define SEXTH		(F7(0x30) | F12(0x5) | F3(0x1) | OPC(0x13))
#endif /* __riscv_zbb */
#if defined __riscv_zba
#define SH1ADD		(F7(0x10) | F3(0x2) | OPC(0x33))
#define SH2ADD		(F7(0x10) | F3(0x4) | OPC(0x33))
#define SH3ADD		(F7(0x10) | F3(0x6) | OPC(0x33))
#endif /* __riscv_zba */
#define SLL		(F7(0x0) | F3(0x1) | OPC(0x33))
#define SLLI		(F3(0x1) | OPC(0x13))
#define SLT		(F7(0x0) | F3(0x2) | OPC(0x33))
#define SLTI		(F3(0x2) | OPC(0x13))
#define SLTU		(F7(0x0) | F3(0x3) | OPC(0x33))
#define SLTUI		(F3(0x3) | OPC(0x13))
#define SRL		(F7(0x0) | F3(0x5) | OPC(0x33))
#define SRLI		(F3(0x5) | OPC(0x13))
#define SRA		(F7(0x20) | F3(0x5) | OPC(0x33))
#define SRAI		(F7(0x20) | F3(0x5) | OPC(0x13))
#define SUB		(F7(0x20) | F3(0x0) | OPC(0x33))
#define SW		(F3(0x2) | OPC(0x23))
#define VAND_VV		(F7(0x13) | OPIVV)
#define VFMV_FS		(F7(0x21) | OPFVV)
#define VFMV_SF		(F7(0x21) | OPFVF)
#define VFMV_VF		(F7(0x2f) | OPFVF)
#define VFWCVT_FFV	(F7(0x25) | (0xc << 15) | OPFVV)
#define VL		(F7(0x1) | OPC(0x7))
#define VMSLE_VI	(F7(0x3b) | OPIVI)
#define VMV_SX		(F7(0x21) | OPMVX)
#define VMV_VI		(F7(0x2f) | OPIVI)
#define VMV_VV		(F7(0x2f) | OPIVV)
#define VMV_VX		(F7(0x2f) | OPIVX)
#define VMV_XS		(F7(0x21) | OPMVV)
#define VOR_VV		(F7(0x15) | OPIVV)
#define VSETIVLI	(F7(0x60) | F3(0x7) | OPC(0x57))
#define VS		(F7(0x1) | OPC(0x27))
#define VSLIDEDOWN_VX	(F7(0x1f) | OPIVX)
#define VSLIDEDOWN_VI	(F7(0x1f) | OPIVI)
#define VSLIDEUP_VX	(F7(0x1d) | OPIVX)
#define VSLIDEUP_VI	(F7(0x1d) | OPIVI)
#define VRGATHER_VI	(F7(0x19) | OPIVI)
#define VRGATHER_VV	(F7(0x19) | OPIVV)
#define VXOR_VV		(F7(0x17) | OPIVV)
#define VZEXT_VF2	(F7(0x25) | (0x6 << 15) | OPMVV)
#define VZEXT_VF4	(F7(0x25) | (0x4 << 15) | OPMVV)
#define VZEXT_VF8	(F7(0x25) | (0x2 << 15) | OPMVV)
#define XOR		(F7(0x0) | F3(0x4) | OPC(0x33))
#define XORI		(F3(0x4) | OPC(0x13))
#if defined __riscv_zbb
#if defined SLJIT2_CONFIG_RISCV_32
#define ZEXTH		(F7(0x4) | F3(0x4) | OPC(0x33))
#elif defined SLJIT2_CONFIG_RISCV_64
#define ZEXTH		(F7(0x4) | F3(0x4) | OPC(0x3B))
#endif
#endif /* __riscv_zbb */

#define SIMM_MAX	(0x7ff)
#define SIMM_MIN	(-0x800)
#define BRANCH_MAX	(0xfff)
#define BRANCH_MIN	(-0x1000)
#define JUMP_MAX	(0xfffff)
#define JUMP_MIN	(-0x100000)

#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
#define S32_MAX		(0x7ffff7ffl)
#define S32_MIN		(-0x80000000l)
#define S44_MAX		(0x7fffffff7ffl)
#define S52_MAX		(0x7ffffffffffffl)
#endif

static sljit2_s32 push_inst(struct sljit2_compiler *compiler, sljit2_ins ins)
{
	sljit2_ins *ptr = (sljit2_ins*)ensure_buf(compiler, sizeof(sljit2_ins));
	FAIL_IF(!ptr);
	*ptr = ins;
	compiler->size++;
	return SLJIT2_SUCCESS;
}

static sljit2_s32 push_imm_s_inst(struct sljit2_compiler *compiler, sljit2_ins ins, sljit2_sw imm)
{
	return push_inst(compiler, ins | IMM_S(imm));
}

static SLJIT2_INLINE sljit2_ins* detect_jump_type(struct sljit2_jump *jump, sljit2_ins *code, sljit2_sw executable_offset)
{
	sljit2_sw diff;
	sljit2_uw target_addr;
	sljit2_ins *inst;

	inst = (sljit2_ins *)jump->addr;

	if (jump->flags & SLJIT2_REWRITABLE_JUMP)
		goto exit;

	if (jump->flags & JUMP_ADDR)
		target_addr = jump->u.target;
	else {
		SLJIT2_ASSERT(jump->u.label != NULL);
		target_addr = (sljit2_uw)(code + jump->u.label->size) + (sljit2_uw)executable_offset;
	}

	diff = (sljit2_sw)target_addr - (sljit2_sw)inst - executable_offset;

	if (jump->flags & IS_COND) {
		diff += SSIZE_OF(ins);

		if (diff >= BRANCH_MIN && diff <= BRANCH_MAX) {
			inst--;
			inst[0] = (inst[0] & 0x1fff07f) ^ 0x1000;
			jump->flags |= PATCH_B;
			jump->addr = (sljit2_uw)inst;
			return inst;
		}

		diff -= SSIZE_OF(ins);
	}

	if (diff >= JUMP_MIN && diff <= JUMP_MAX) {
		if (jump->flags & IS_COND) {
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
			inst[-1] -= (sljit2_ins)(1 * sizeof(sljit2_ins)) << 7;
#else
			inst[-1] -= (sljit2_ins)(5 * sizeof(sljit2_ins)) << 7;
#endif
		}

		jump->flags |= PATCH_J;
		return inst;
	}

#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	if (diff >= S32_MIN && diff <= S32_MAX) {
		if (jump->flags & IS_COND)
			inst[-1] -= (sljit2_ins)(4 * sizeof(sljit2_ins)) << 7;

		jump->flags |= PATCH_REL32;
		inst[1] = inst[0];
		return inst + 1;
	}

	if (target_addr <= (sljit2_uw)S32_MAX) {
		if (jump->flags & IS_COND)
			inst[-1] -= (sljit2_ins)(4 * sizeof(sljit2_ins)) << 7;

		jump->flags |= PATCH_ABS32;
		inst[1] = inst[0];
		return inst + 1;
	}

	if (target_addr <= S44_MAX) {
		if (jump->flags & IS_COND)
			inst[-1] -= (sljit2_ins)(2 * sizeof(sljit2_ins)) << 7;

		jump->flags |= PATCH_ABS44;
		inst[3] = inst[0];
		return inst + 3;
	}

	if (target_addr <= S52_MAX) {
		if (jump->flags & IS_COND)
			inst[-1] -= (sljit2_ins)(1 * sizeof(sljit2_ins)) << 7;

		jump->flags |= PATCH_ABS52;
		inst[4] = inst[0];
		return inst + 4;
	}
#endif

exit:
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	inst[1] = inst[0];
	return inst + 1;
#else
	inst[5] = inst[0];
	return inst + 5;
#endif
}

#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)

static SLJIT2_INLINE sljit2_sw mov_addr_get_length(struct sljit2_jump *jump, sljit2_ins *code_ptr, sljit2_ins *code, sljit2_sw executable_offset)
{
	sljit2_uw addr;
	sljit2_sw diff;
	SLJIT2_UNUSED_ARG(executable_offset);

	SLJIT2_ASSERT(jump->flags < ((sljit2_uw)6 << JUMP_SIZE_SHIFT));
	if (jump->flags & JUMP_ADDR)
		addr = jump->u.target;
	else
		addr = (sljit2_uw)SLJIT2_ADD_EXEC_OFFSET(code + jump->u.label->size, executable_offset);

	diff = (sljit2_sw)addr - (sljit2_sw)SLJIT2_ADD_EXEC_OFFSET(code_ptr, executable_offset);

	if (diff >= S32_MIN && diff <= S32_MAX) {
		SLJIT2_ASSERT(jump->flags >= ((sljit2_uw)1 << JUMP_SIZE_SHIFT));
		jump->flags |= PATCH_REL32;
		return 1;
	}

	if (addr <= S32_MAX) {
		SLJIT2_ASSERT(jump->flags >= ((sljit2_uw)1 << JUMP_SIZE_SHIFT));
		jump->flags |= PATCH_ABS32;
		return 1;
	}

	if (addr <= S44_MAX) {
		SLJIT2_ASSERT(jump->flags >= ((sljit2_uw)3 << JUMP_SIZE_SHIFT));
		jump->flags |= PATCH_ABS44;
		return 3;
	}

	if (addr <= S52_MAX) {
		SLJIT2_ASSERT(jump->flags >= ((sljit2_uw)4 << JUMP_SIZE_SHIFT));
		jump->flags |= PATCH_ABS52;
		return 4;
	}

	SLJIT2_ASSERT(jump->flags >= ((sljit2_uw)5 << JUMP_SIZE_SHIFT));
	return 5;
}

#endif /* SLJIT2_CONFIG_RISCV_64 */

static SLJIT2_INLINE void load_addr_to_reg(struct sljit2_jump *jump, sljit2_sw executable_offset)
{
	sljit2_uw flags = jump->flags;
	sljit2_uw addr = (flags & JUMP_ADDR) ? jump->u.target : jump->u.label->u.addr;
	sljit2_ins *ins = (sljit2_ins*)jump->addr;
	sljit2_u32 reg = (flags & JUMP_MOV_ADDR) ? *ins : TMP_REG1;
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	sljit2_sw high;
#endif
	SLJIT2_UNUSED_ARG(executable_offset);

#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	if (flags & PATCH_REL32) {
		addr -= (sljit2_uw)SLJIT2_ADD_EXEC_OFFSET(ins, executable_offset);

		SLJIT2_ASSERT((sljit2_sw)addr >= S32_MIN && (sljit2_sw)addr <= S32_MAX);

		if ((addr & 0x800) != 0)
			addr += 0x1000;

		ins[0] = AUIPC | RD(reg) | (sljit2_ins)((sljit2_sw)addr & ~0xfff);

		if (!(flags & JUMP_MOV_ADDR)) {
			SLJIT2_ASSERT((ins[1] & 0x707f) == JALR);
			ins[1] = (ins[1] & 0xfffff) | IMM_I(addr);
		} else
			ins[1] = ADDI | RD(reg) | RS1(reg) | IMM_I(addr);
		return;
	}
#endif

	if ((addr & 0x800) != 0)
		addr += 0x1000;

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	ins[0] = LUI | RD(reg) | (sljit2_ins)((sljit2_sw)addr & ~0xfff);
#else /* !SLJIT2_CONFIG_RISCV_32 */

	if (flags & PATCH_ABS32) {
		SLJIT2_ASSERT(addr <= S32_MAX);
		ins[0] = LUI | RD(reg) | (sljit2_ins)((sljit2_sw)addr & ~0xfff);
	} else if (flags & PATCH_ABS44) {
		high = (sljit2_sw)addr >> 12;
		SLJIT2_ASSERT((sljit2_uw)high <= 0x7fffffff);

		if (high > S32_MAX) {
			SLJIT2_ASSERT((high & 0x800) != 0);
			ins[0] = LUI | RD(reg) | (sljit2_ins)0x80000000u;
			ins[1] = XORI | RD(reg) | RS1(reg) | IMM_I(high);
		} else {
			if ((high & 0x800) != 0)
				high += 0x1000;

			ins[0] = LUI | RD(reg) | (sljit2_ins)(high & ~0xfff);
			ins[1] = ADDI | RD(reg) | RS1(reg) | IMM_I(high);
		}

		ins[2] = SLLI | RD(reg) | RS1(reg) | IMM_I(12);
		ins += 2;
	} else {
		high = (sljit2_sw)addr >> 32;

		if ((addr & 0x80000000l) != 0)
			high = ~high;

		if (flags & PATCH_ABS52) {
			SLJIT2_ASSERT(addr <= S52_MAX);
			ins[0] = LUI | RD(TMP_REG3) | (sljit2_ins)(high << 12);
		} else {
			if ((high & 0x800) != 0)
				high += 0x1000;
			ins[0] = LUI | RD(TMP_REG3) | (sljit2_ins)(high & ~0xfff);
			ins[1] = ADDI | RD(TMP_REG3) | RS1(TMP_REG3) | IMM_I(high);
			ins++;
		}

		ins[1] = LUI | RD(reg) | (sljit2_ins)((sljit2_sw)addr & ~0xfff);
		ins[2] = SLLI | RD(TMP_REG3) | RS1(TMP_REG3) | IMM_I((flags & PATCH_ABS52) ? 20 : 32);
		ins[3] = XOR | RD(reg) | RS1(reg) | RS2(TMP_REG3);
		ins += 3;
	}
#endif /* !SLJIT2_CONFIG_RISCV_32 */

	if (!(flags & JUMP_MOV_ADDR)) {
		SLJIT2_ASSERT((ins[1] & 0x707f) == JALR);
		ins[1] = (ins[1] & 0xfffff) | IMM_I(addr);
	} else
		ins[1] = ADDI | RD(reg) | RS1(reg) | IMM_I(addr);
}

static void reduce_code_size(struct sljit2_compiler *compiler)
{
	struct sljit2_label *label;
	struct sljit2_jump *jump;
	struct sljit2_const *const_;
	SLJIT2_NEXT_DEFINE_TYPES;
	sljit2_uw total_size;
	sljit2_uw size_reduce = 0;
	sljit2_sw diff;

	label = compiler->labels;
	jump = compiler->jumps;
	const_ = compiler->consts;
	SLJIT2_NEXT_INIT_TYPES();

	while (1) {
		SLJIT2_GET_NEXT_MIN();

		if (next_min_addr == SLJIT2_MAX_ADDRESS)
			break;

		if (next_min_addr == next_label_size) {
			label->size -= size_reduce;

			label = label->next;
			next_label_size = SLJIT2_GET_NEXT_SIZE(label);
		}

		if (next_min_addr == next_const_addr) {
			const_->addr -= size_reduce;
			const_ = const_->next;
			next_const_addr = SLJIT2_GET_NEXT_ADDRESS(const_);
			continue;
		}

		if (next_min_addr != next_jump_addr)
			continue;

		jump->addr -= size_reduce;
		if (!(jump->flags & JUMP_MOV_ADDR)) {
			total_size = JUMP_MAX_SIZE;

			if (!(jump->flags & SLJIT2_REWRITABLE_JUMP)) {
				if (jump->flags & JUMP_ADDR) {
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
					if (jump->u.target <= S32_MAX)
						total_size = 2;
					else if (jump->u.target <= S44_MAX)
						total_size = 4;
					else if (jump->u.target <= S52_MAX)
						total_size = 5;
#endif /* SLJIT2_CONFIG_RISCV_64 */
				} else {
					/* Unit size: instruction. */
					diff = (sljit2_sw)jump->u.label->size - (sljit2_sw)jump->addr;

					if ((jump->flags & IS_COND) && (diff + 1) <= (BRANCH_MAX / SSIZE_OF(ins)) && (diff + 1) >= (BRANCH_MIN / SSIZE_OF(ins)))
						total_size = 0;
					else if (diff >= (JUMP_MIN / SSIZE_OF(ins)) && diff <= (JUMP_MAX / SSIZE_OF(ins)))
						total_size = 1;
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
					else if (diff >= (S32_MIN / SSIZE_OF(ins)) && diff <= (S32_MAX / SSIZE_OF(ins)))
						total_size = 2;
#endif /* SLJIT2_CONFIG_RISCV_64 */
				}
			}

			size_reduce += JUMP_MAX_SIZE - total_size;
			jump->flags |= total_size << JUMP_SIZE_SHIFT;
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
		} else {
			total_size = 5;

			if (!(jump->flags & JUMP_ADDR)) {
				/* Real size minus 1. Unit size: instruction. */
				diff = (sljit2_sw)jump->u.label->size - (sljit2_sw)jump->addr;

				if (diff >= (S32_MIN / SSIZE_OF(ins)) && diff <= (S32_MAX / SSIZE_OF(ins)))
					total_size = 1;
			} else if (jump->u.target < S32_MAX)
				total_size = 1;
			else if (jump->u.target < S44_MAX)
				total_size = 3;
			else if (jump->u.target <= S52_MAX)
				total_size = 4;

			size_reduce += 5 - total_size;
			jump->flags |= total_size << JUMP_SIZE_SHIFT;
#endif /* !SLJIT2_CONFIG_RISCV_64 */
		}

		jump = jump->next;
		next_jump_addr = SLJIT2_GET_NEXT_ADDRESS(jump);
	}

	compiler->size -= size_reduce;
}

SLJIT2_API_FUNC_ATTRIBUTE void* sljit2_generate_code(struct sljit2_compiler *compiler, sljit2_s32 options, void *exec_allocator_data)
{
	struct sljit2_memory_fragment *buf;
	sljit2_ins *code;
	sljit2_ins *code_ptr;
	sljit2_ins *buf_ptr;
	sljit2_ins *buf_end;
	sljit2_uw word_count;
	SLJIT2_NEXT_DEFINE_TYPES;
	sljit2_sw executable_offset;
	sljit2_uw addr;

	struct sljit2_label *label;
	struct sljit2_jump *jump;
	struct sljit2_const *const_;

	CHECK_ERROR_PTR();
	CHECK_PTR(check_sljit2_generate_code(compiler));

	reduce_code_size(compiler);

	code = (sljit2_ins*)allocate_executable_memory(compiler->size * sizeof(sljit2_ins), options, exec_allocator_data, &executable_offset);
	PTR_FAIL_WITH_EXEC_IF(code);

	reverse_buf(compiler);
	buf = compiler->buf;

	code_ptr = code;
	word_count = 0;
	label = compiler->labels;
	jump = compiler->jumps;
	const_ = compiler->consts;
	SLJIT2_NEXT_INIT_TYPES();
	SLJIT2_GET_NEXT_MIN();

	do {
		buf_ptr = (sljit2_ins*)buf->memory;
		buf_end = buf_ptr + (buf->used_size >> 2);
		do {
			*code_ptr = *buf_ptr++;
			if (next_min_addr == word_count) {
				SLJIT2_ASSERT(!label || label->size >= word_count);
				SLJIT2_ASSERT(!jump || jump->addr >= word_count);
				SLJIT2_ASSERT(!const_ || const_->addr >= word_count);

				/* These structures are ordered by their address. */
				if (next_min_addr == next_label_size) {
					label->u.addr = (sljit2_uw)SLJIT2_ADD_EXEC_OFFSET(code_ptr, executable_offset);
					label->size = (sljit2_uw)(code_ptr - code);
					label = label->next;
					next_label_size = SLJIT2_GET_NEXT_SIZE(label);
				}

				if (next_min_addr == next_jump_addr) {
					if (!(jump->flags & JUMP_MOV_ADDR)) {
						word_count = word_count - 1 + (jump->flags >> JUMP_SIZE_SHIFT);
						jump->addr = (sljit2_uw)code_ptr;
						code_ptr = detect_jump_type(jump, code, executable_offset);
						SLJIT2_ASSERT((jump->flags & PATCH_B) || ((sljit2_uw)code_ptr - jump->addr < (jump->flags >> JUMP_SIZE_SHIFT) * sizeof(sljit2_ins)));
					} else {
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
						word_count += 1;
						jump->addr = (sljit2_uw)code_ptr;
						code_ptr += 1;
#else /* !SLJIT2_CONFIG_RISCV_32 */
						word_count += jump->flags >> JUMP_SIZE_SHIFT;
						addr = (sljit2_uw)code_ptr;
						code_ptr += mov_addr_get_length(jump, code_ptr, code, executable_offset);
						jump->addr = addr;
#endif /* SLJIT2_CONFIG_RISCV_32 */
					}
					jump = jump->next;
					next_jump_addr = SLJIT2_GET_NEXT_ADDRESS(jump);
				} else if (next_min_addr == next_const_addr) {
					const_->addr = (sljit2_uw)code_ptr;
					const_ = const_->next;
					next_const_addr = SLJIT2_GET_NEXT_ADDRESS(const_);
				}

				SLJIT2_GET_NEXT_MIN();
			}
			code_ptr++;
			word_count++;
		} while (buf_ptr < buf_end);

		buf = buf->next;
	} while (buf);

	if (label && label->size == word_count) {
		label->u.addr = (sljit2_uw)code_ptr;
		label->size = (sljit2_uw)(code_ptr - code);
		label = label->next;
	}

	SLJIT2_ASSERT(!label);
	SLJIT2_ASSERT(!jump);
	SLJIT2_ASSERT(!const_);
	SLJIT2_ASSERT(code_ptr - code <= (sljit2_sw)compiler->size);

	jump = compiler->jumps;
	while (jump) {
		do {
			if (!(jump->flags & (PATCH_B | PATCH_J)) || (jump->flags & JUMP_MOV_ADDR)) {
				load_addr_to_reg(jump, executable_offset);
				break;
			}

			addr = (jump->flags & JUMP_ADDR) ? jump->u.target : jump->u.label->u.addr;
			buf_ptr = (sljit2_ins *)jump->addr;
			addr -= (sljit2_uw)SLJIT2_ADD_EXEC_OFFSET(buf_ptr, executable_offset);

			if (jump->flags & PATCH_B) {
				SLJIT2_ASSERT((sljit2_sw)addr >= BRANCH_MIN && (sljit2_sw)addr <= BRANCH_MAX);
				addr = ((addr & 0x800) >> 4) | ((addr & 0x1e) << 7) | ((addr & 0x7e0) << 20) | ((addr & 0x1000) << 19);
				buf_ptr[0] |= (sljit2_ins)addr;
				break;
			}

			SLJIT2_ASSERT((sljit2_sw)addr >= JUMP_MIN && (sljit2_sw)addr <= JUMP_MAX);
			addr = (addr & 0xff000) | ((addr & 0x800) << 9) | ((addr & 0x7fe) << 20) | ((addr & 0x100000) << 11);
			buf_ptr[0] = JAL | RD((jump->flags & IS_CALL) ? RETURN_ADDR_REG : TMP_ZERO) | (sljit2_ins)addr;
		} while (0);

		jump = jump->next;
	}

	compiler->error = SLJIT2_ERR_COMPILED;
	compiler->executable_offset = executable_offset;
	compiler->executable_size = (sljit2_uw)(code_ptr - code) * sizeof(sljit2_ins);

	code = (sljit2_ins *)SLJIT2_ADD_EXEC_OFFSET(code, executable_offset);
	code_ptr = (sljit2_ins *)SLJIT2_ADD_EXEC_OFFSET(code_ptr, executable_offset);

	SLJIT2_CACHE_FLUSH(code, code_ptr);
	SLJIT2_UPDATE_WX_FLAGS(code, code_ptr, 1);
	return code;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_has_cpu_feature(sljit2_s32 feature_type)
{
	switch (feature_type) {
	case SLJIT2_HAS_FPU:
#ifdef SLJIT2_IS_FPU_AVAILABLE
		return (SLJIT2_IS_FPU_AVAILABLE) != 0;
#elif defined(__riscv_float_abi_soft)
		return 0;
#else
		return 1;
#endif /* SLJIT2_IS_FPU_AVAILABLE */
	case SLJIT2_HAS_ZERO_REGISTER:
	case SLJIT2_HAS_COPY_F32:
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	case SLJIT2_HAS_COPY_F64:
#endif /* !SLJIT2_CONFIG_RISCV_64 */
	case SLJIT2_HAS_ATOMIC:
	case SLJIT2_HAS_MEMORY_BARRIER:
#ifdef __riscv_vector
	case SLJIT2_HAS_SIMD:
#endif
		return 1;
#ifdef __riscv_zbb
	case SLJIT2_HAS_CLZ:
	case SLJIT2_HAS_CTZ:
	case SLJIT2_HAS_REV:
	case SLJIT2_HAS_ROT:
		return 1;
#endif
	default:
		return 0;
	}
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_cmp_info(sljit2_s32 type)
{
	switch (type) {
	case SLJIT2_UNORDERED_OR_EQUAL:
	case SLJIT2_ORDERED_NOT_EQUAL:
		return 2;

	case SLJIT2_UNORDERED:
	case SLJIT2_ORDERED:
		return 1;
	}

	return 0;
}

/* --------------------------------------------------------------------- */
/*  Entry, exit                                                          */
/* --------------------------------------------------------------------- */

/* Creates an index in data_transfer_insts array. */
#define LOAD_DATA	0x01
#define WORD_DATA	0x00
#define BYTE_DATA	0x02
#define HALF_DATA	0x04
#define INT_DATA	0x06
#define SIGNED_DATA	0x08
/* Separates integer and floating point registers */
#define GPR_REG		0x0f
#define DOUBLE_DATA	0x10
#define SINGLE_DATA	0x12

#define MEM_MASK	0x1f

#define ARG_TEST	0x00020
#define ALT_KEEP_CACHE	0x00040
#define CUMULATIVE_OP	0x00080
#define IMM_OP		0x00100
#define MOVE_OP		0x00200
#define SRC2_IMM	0x00400

#define UNUSED_DEST	0x00800
#define REG_DEST	0x01000
#define REG1_SOURCE	0x02000
#define REG2_SOURCE	0x04000
#define SLOW_SRC1	0x08000
#define SLOW_SRC2	0x10000
#define SLOW_DEST	0x20000
#define MEM_USE_TMP2	0x40000

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
#define STACK_STORE	SW
#define STACK_LOAD	LW
#else
#define STACK_STORE	SD
#define STACK_LOAD	LD
#endif

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
#include "sljitNativeRISCV_32.c"
#else
#include "sljitNativeRISCV_64.c"
#endif

#define STACK_MAX_DISTANCE (-SIMM_MIN)

static sljit2_s32 emit_op_mem(struct sljit2_compiler *compiler, sljit2_s32 flags, sljit2_s32 reg, sljit2_s32 arg, sljit2_sw argw);

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_enter(struct sljit2_compiler *compiler,
	sljit2_s32 options, sljit2_s32 arg_types,
	sljit2_s32 scratches, sljit2_s32 saveds, sljit2_s32 local_size)
{
	sljit2_s32 fscratches = ENTER_GET_FLOAT_REGS(scratches);
	sljit2_s32 fsaveds = ENTER_GET_FLOAT_REGS(saveds);
	sljit2_s32 i, tmp, offset;
	sljit2_s32 saved_arg_count = SLJIT2_KEPT_SAVEDS_COUNT(options);

	CHECK_ERROR();
	CHECK(check_sljit2_emit_enter(compiler, options, arg_types, scratches, saveds, local_size));
	set_emit_enter(compiler, options, arg_types, scratches, saveds, local_size);

	scratches = ENTER_GET_REGS(scratches);
	saveds = ENTER_GET_REGS(saveds);
	local_size += GET_SAVED_REGISTERS_SIZE(scratches, saveds - saved_arg_count, 1);
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	if (fsaveds > 0 || fscratches >= SLJIT2_FIRST_SAVED_FLOAT_REG) {
		if ((local_size & SSIZE_OF(sw)) != 0)
			local_size += SSIZE_OF(sw);
		local_size += GET_SAVED_FLOAT_REGISTERS_SIZE(fscratches, fsaveds, f64);
	}
#else
	local_size += GET_SAVED_FLOAT_REGISTERS_SIZE(fscratches, fsaveds, f64);
#endif
	local_size = (local_size + SLJIT2_LOCALS_OFFSET + 15) & ~0xf;
	compiler->local_size = local_size;

	if (local_size <= STACK_MAX_DISTANCE) {
		/* Frequent case. */
		FAIL_IF(push_inst(compiler, ADDI | RD(SLJIT2_SP) | RS1(SLJIT2_SP) | IMM_I(-local_size)));
		offset = local_size - SSIZE_OF(sw);
		local_size = 0;
	} else {
		FAIL_IF(push_inst(compiler, ADDI | RD(SLJIT2_SP) | RS1(SLJIT2_SP) | IMM_I(STACK_MAX_DISTANCE)));
		local_size -= STACK_MAX_DISTANCE;

		if (local_size > STACK_MAX_DISTANCE)
			FAIL_IF(load_immediate(compiler, TMP_REG1, local_size, TMP_REG3));
		offset = STACK_MAX_DISTANCE - SSIZE_OF(sw);
	}

	FAIL_IF(push_imm_s_inst(compiler, STACK_STORE | RS1(SLJIT2_SP) | RS2(RETURN_ADDR_REG), offset));

	tmp = SLJIT2_S0 - saveds;
	for (i = SLJIT2_S0 - saved_arg_count; i > tmp; i--) {
		offset -= SSIZE_OF(sw);
		FAIL_IF(push_imm_s_inst(compiler, STACK_STORE | RS1(SLJIT2_SP) | RS2(i), offset));
	}

	for (i = scratches; i >= SLJIT2_FIRST_SAVED_REG; i--) {
		offset -= SSIZE_OF(sw);
		FAIL_IF(push_imm_s_inst(compiler, STACK_STORE | RS1(SLJIT2_SP) | RS2(i), offset));
	}

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	/* This alignment is valid because offset is not used after storing FPU regs. */
	if ((offset & SSIZE_OF(sw)) != 0)
		offset -= SSIZE_OF(sw);
#endif

	tmp = SLJIT2_FS0 - fsaveds;
	for (i = SLJIT2_FS0; i > tmp; i--) {
		offset -= SSIZE_OF(f64);
		FAIL_IF(push_imm_s_inst(compiler, FSD | RS1(SLJIT2_SP) | FRS2(i), offset));
	}

	for (i = fscratches; i >= SLJIT2_FIRST_SAVED_FLOAT_REG; i--) {
		offset -= SSIZE_OF(f64);
		FAIL_IF(push_imm_s_inst(compiler, FSD | RS1(SLJIT2_SP) | FRS2(i), offset));
	}

	if (local_size > STACK_MAX_DISTANCE)
		FAIL_IF(push_inst(compiler, SUB | RD(SLJIT2_SP) | RS1(SLJIT2_SP) | RS2(TMP_REG1)));
	else if (local_size > 0)
		FAIL_IF(push_inst(compiler, ADDI | RD(SLJIT2_SP) | RS1(SLJIT2_SP) | IMM_I(-local_size)));

	if (options & SLJIT2_ENTER_REG_ARG)
		return SLJIT2_SUCCESS;

	arg_types >>= SLJIT2_ARG_SHIFT;
	saved_arg_count = 0;
	tmp = SLJIT2_R0;

	while (arg_types > 0) {
		if ((arg_types & SLJIT2_ARG_MASK) < SLJIT2_ARG_TYPE_F64) {
			if (!(arg_types & SLJIT2_ARG_TYPE_SCRATCH_REG)) {
				FAIL_IF(push_inst(compiler, ADDI | RD(SLJIT2_S0 - saved_arg_count) | RS1(tmp) | IMM_I(0)));
				saved_arg_count++;
			}
			tmp++;
		}

		arg_types >>= SLJIT2_ARG_SHIFT;
	}

	return SLJIT2_SUCCESS;
}

#undef STACK_MAX_DISTANCE

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_set_context(struct sljit2_compiler *compiler,
	sljit2_s32 options, sljit2_s32 arg_types,
	sljit2_s32 scratches, sljit2_s32 saveds, sljit2_s32 local_size)
{
	sljit2_s32 fscratches = ENTER_GET_FLOAT_REGS(scratches);
	sljit2_s32 fsaveds = ENTER_GET_FLOAT_REGS(saveds);

	CHECK_ERROR();
	CHECK(check_sljit2_set_context(compiler, options, arg_types, scratches, saveds, local_size));
	set_set_context(compiler, options, arg_types, scratches, saveds, local_size);

	scratches = ENTER_GET_REGS(scratches);
	saveds = ENTER_GET_REGS(saveds);
	local_size += GET_SAVED_REGISTERS_SIZE(scratches, saveds - SLJIT2_KEPT_SAVEDS_COUNT(options), 1);
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	if (fsaveds > 0 || fscratches >= SLJIT2_FIRST_SAVED_FLOAT_REG) {
		if ((local_size & SSIZE_OF(sw)) != 0)
			local_size += SSIZE_OF(sw);
		local_size += GET_SAVED_FLOAT_REGISTERS_SIZE(fscratches, fsaveds, f64);
	}
#else
	local_size += GET_SAVED_FLOAT_REGISTERS_SIZE(fscratches, fsaveds, f64);
#endif
	compiler->local_size = (local_size + SLJIT2_LOCALS_OFFSET + 15) & ~0xf;

	return SLJIT2_SUCCESS;
}

#define STACK_MAX_DISTANCE (-SIMM_MIN - 16)

static sljit2_s32 emit_stack_frame_release(struct sljit2_compiler *compiler, sljit2_s32 is_return_to)
{
	sljit2_s32 i, tmp, offset;
	sljit2_s32 local_size = compiler->local_size;

	if (local_size > STACK_MAX_DISTANCE) {
		local_size -= STACK_MAX_DISTANCE;

		if (local_size > STACK_MAX_DISTANCE) {
			FAIL_IF(load_immediate(compiler, TMP_REG2, local_size, TMP_REG3));
			FAIL_IF(push_inst(compiler, ADD | RD(SLJIT2_SP) | RS1(SLJIT2_SP) | RS2(TMP_REG2)));
		} else
			FAIL_IF(push_inst(compiler, ADDI | RD(SLJIT2_SP) | RS1(SLJIT2_SP) | IMM_I(local_size)));

		local_size = STACK_MAX_DISTANCE;
	}

	SLJIT2_ASSERT(local_size > 0);

	offset = local_size - SSIZE_OF(sw);
	if (!is_return_to)
		FAIL_IF(push_inst(compiler, STACK_LOAD | RD(RETURN_ADDR_REG) | RS1(SLJIT2_SP) | IMM_I(offset)));

	tmp = SLJIT2_S0 - compiler->saveds;
	for (i = SLJIT2_S0 - SLJIT2_KEPT_SAVEDS_COUNT(compiler->options); i > tmp; i--) {
		offset -= SSIZE_OF(sw);
		FAIL_IF(push_inst(compiler, STACK_LOAD | RD(i) | RS1(SLJIT2_SP) | IMM_I(offset)));
	}

	for (i = compiler->scratches; i >= SLJIT2_FIRST_SAVED_REG; i--) {
		offset -= SSIZE_OF(sw);
		FAIL_IF(push_inst(compiler, STACK_LOAD | RD(i) | RS1(SLJIT2_SP) | IMM_I(offset)));
	}

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	/* This alignment is valid because offset is not used after storing FPU regs. */
	if ((offset & SSIZE_OF(sw)) != 0)
		offset -= SSIZE_OF(sw);
#endif

	tmp = SLJIT2_FS0 - compiler->fsaveds;
	for (i = SLJIT2_FS0; i > tmp; i--) {
		offset -= SSIZE_OF(f64);
		FAIL_IF(push_inst(compiler, FLD | FRD(i) | RS1(SLJIT2_SP) | IMM_I(offset)));
	}

	for (i = compiler->fscratches; i >= SLJIT2_FIRST_SAVED_FLOAT_REG; i--) {
		offset -= SSIZE_OF(f64);
		FAIL_IF(push_inst(compiler, FLD | FRD(i) | RS1(SLJIT2_SP) | IMM_I(offset)));
	}

	return push_inst(compiler, ADDI | RD(SLJIT2_SP) | RS1(SLJIT2_SP) | IMM_I(local_size));
}

#undef STACK_MAX_DISTANCE

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_return_void(struct sljit2_compiler *compiler)
{
	CHECK_ERROR();
	CHECK(check_sljit2_emit_return_void(compiler));

	FAIL_IF(emit_stack_frame_release(compiler, 0));
	return push_inst(compiler, JALR | RD(TMP_ZERO) | RS1(RETURN_ADDR_REG) | IMM_I(0));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_return_to(struct sljit2_compiler *compiler,
	sljit2_s32 src, sljit2_sw srcw)
{
	CHECK_ERROR();
	CHECK(check_sljit2_emit_return_to(compiler, src, srcw));

	if (src & SLJIT2_MEM) {
		ADJUST_LOCAL_OFFSET(src, srcw);
		FAIL_IF(emit_op_mem(compiler, WORD_DATA | LOAD_DATA, TMP_REG1, src, srcw));
		src = TMP_REG1;
		srcw = 0;
	} else if (src >= SLJIT2_FIRST_SAVED_REG && src <= (SLJIT2_S0 - SLJIT2_KEPT_SAVEDS_COUNT(compiler->options))) {
		FAIL_IF(push_inst(compiler, ADDI | RD(TMP_REG1) | RS1(src) | IMM_I(0)));
		src = TMP_REG1;
		srcw = 0;
	}

	FAIL_IF(emit_stack_frame_release(compiler, 1));

	SLJIT2_SKIP_CHECKS(compiler);
	return sljit2_emit_ijump(compiler, SLJIT2_JUMP, src, srcw);
}

/* --------------------------------------------------------------------- */
/*  Operators                                                            */
/* --------------------------------------------------------------------- */

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
#define ARCH_32_64(a, b)	a
#else
#define ARCH_32_64(a, b)	b
#endif

static const sljit2_ins data_transfer_insts[16 + 4] = {
/* u w s */ ARCH_32_64(F3(0x2) | OPC(0x23) /* sw */, F3(0x3) | OPC(0x23) /* sd */),
/* u w l */ ARCH_32_64(F3(0x2) | OPC(0x3) /* lw */, F3(0x3) | OPC(0x3) /* ld */),
/* u b s */ F3(0x0) | OPC(0x23) /* sb */,
/* u b l */ F3(0x4) | OPC(0x3) /* lbu */,
/* u h s */ F3(0x1) | OPC(0x23) /* sh */,
/* u h l */ F3(0x5) | OPC(0x3) /* lhu */,
/* u i s */ F3(0x2) | OPC(0x23) /* sw */,
/* u i l */ ARCH_32_64(F3(0x2) | OPC(0x3) /* lw */, F3(0x6) | OPC(0x3) /* lwu */),

/* s w s */ ARCH_32_64(F3(0x2) | OPC(0x23) /* sw */, F3(0x3) | OPC(0x23) /* sd */),
/* s w l */ ARCH_32_64(F3(0x2) | OPC(0x3) /* lw */, F3(0x3) | OPC(0x3) /* ld */),
/* s b s */ F3(0x0) | OPC(0x23) /* sb */,
/* s b l */ F3(0x0) | OPC(0x3) /* lb */,
/* s h s */ F3(0x1) | OPC(0x23) /* sh */,
/* s h l */ F3(0x1) | OPC(0x3) /* lh */,
/* s i s */ F3(0x2) | OPC(0x23) /* sw */,
/* s i l */ F3(0x2) | OPC(0x3) /* lw */,

/* d   s */ F3(0x3) | OPC(0x27) /* fsd */,
/* d   l */ F3(0x3) | OPC(0x7) /* fld */,
/* s   s */ F3(0x2) | OPC(0x27) /* fsw */,
/* s   l */ F3(0x2) | OPC(0x7) /* flw */,
};

#undef ARCH_32_64

static sljit2_s32 push_mem_inst(struct sljit2_compiler *compiler, sljit2_s32 flags, sljit2_s32 reg, sljit2_s32 base, sljit2_sw offset)
{
	sljit2_ins ins;

	SLJIT2_ASSERT(FAST_IS_REG(base) && offset <= 0xfff && offset >= SIMM_MIN);

	ins = data_transfer_insts[flags & MEM_MASK] | RS1(base);
	if (flags & LOAD_DATA)
		ins |= ((flags & MEM_MASK) <= GPR_REG ? RD(reg) : FRD(reg)) | IMM_I(offset);
	else
		ins |= ((flags & MEM_MASK) <= GPR_REG ? RS2(reg) : FRS2(reg)) | IMM_S(offset);

	return push_inst(compiler, ins);
}

/* Can perform an operation using at most 1 instruction. */
static sljit2_s32 getput_arg_fast(struct sljit2_compiler *compiler, sljit2_s32 flags, sljit2_s32 reg, sljit2_s32 arg, sljit2_sw argw)
{
	SLJIT2_ASSERT(arg & SLJIT2_MEM);

	if (!(arg & OFFS_REG_MASK) && argw <= SIMM_MAX && argw >= SIMM_MIN) {
		/* Works for both absoulte and relative addresses. */
		if (SLJIT2_UNLIKELY(flags & ARG_TEST))
			return 1;

		FAIL_IF(push_mem_inst(compiler, flags, reg, arg & REG_MASK, argw));
		return -1;
	}
	return 0;
}

#define TO_ARGW_HI(argw) (((argw) & ~0xfff) + (((argw) & 0x800) ? 0x1000 : 0))

/* See getput_arg below.
   Note: can_cache is called only for binary operators. */
static sljit2_s32 can_cache(sljit2_s32 arg, sljit2_sw argw, sljit2_s32 next_arg, sljit2_sw next_argw)
{
	SLJIT2_ASSERT((arg & SLJIT2_MEM) && (next_arg & SLJIT2_MEM));

	/* Simple operation except for updates. */
	if (arg & OFFS_REG_MASK) {
		argw &= 0x3;
		next_argw &= 0x3;
		if (argw && argw == next_argw && (arg == next_arg || (arg & OFFS_REG_MASK) == (next_arg & OFFS_REG_MASK)))
			return 1;
		return 0;
	}

	if (arg == next_arg) {
		if (((next_argw - argw) <= SIMM_MAX && (next_argw - argw) >= SIMM_MIN)
				|| TO_ARGW_HI(argw) == TO_ARGW_HI(next_argw))
			return 1;
		return 0;
	}

	return 0;
}

/* Emit the necessary instructions. See can_cache above. */
static sljit2_s32 getput_arg(struct sljit2_compiler *compiler, sljit2_s32 flags, sljit2_s32 reg, sljit2_s32 arg, sljit2_sw argw, sljit2_s32 next_arg, sljit2_sw next_argw)
{
	sljit2_s32 base = arg & REG_MASK;
	sljit2_s32 tmp_r = (flags & MEM_USE_TMP2) ? TMP_REG2 : TMP_REG1;
	sljit2_sw offset, argw_hi;
#if defined __riscv_zba
	sljit2_ins ins = ADD;
#endif /* __riscv_zba */

	SLJIT2_ASSERT(arg & SLJIT2_MEM);
	if (!(next_arg & SLJIT2_MEM)) {
		next_arg = 0;
		next_argw = 0;
	}

	if (SLJIT2_UNLIKELY(arg & OFFS_REG_MASK)) {
		argw &= 0x3;

#if defined __riscv_zba
		switch (argw) {
			case 1:
				ins = SH1ADD;
				break;
			case 2:
				ins = SH2ADD;
				break;
			case 3:
				ins = SH3ADD;
				break;
		}
		FAIL_IF(push_inst(compiler, ins | RD(tmp_r) | RS1(OFFS_REG(arg)) | RS2(base)));
#else /* !__riscv_zba */
		/* Using the cache. */
		if (argw == compiler->cache_argw) {
			if (arg == compiler->cache_arg)
				return push_mem_inst(compiler, flags, reg, TMP_REG3, 0);

			if ((SLJIT2_MEM | (arg & OFFS_REG_MASK)) == compiler->cache_arg) {
				if (arg == next_arg && argw == (next_argw & 0x3)) {
					compiler->cache_arg = arg;
					compiler->cache_argw = argw;
					FAIL_IF(push_inst(compiler, ADD | RD(TMP_REG3) | RS1(TMP_REG3) | RS2(base)));
					return push_mem_inst(compiler, flags, reg, TMP_REG3, 0);
				}
				FAIL_IF(push_inst(compiler, ADD | RD(tmp_r) | RS1(base) | RS2(TMP_REG3)));
				return push_mem_inst(compiler, flags, reg, tmp_r, 0);
			}
		}

		if (SLJIT2_UNLIKELY(argw)) {
			compiler->cache_arg = SLJIT2_MEM | (arg & OFFS_REG_MASK);
			compiler->cache_argw = argw;
			FAIL_IF(push_inst(compiler, SLLI | RD(TMP_REG3) | RS1(OFFS_REG(arg)) | IMM_I(argw)));
		}

		if (arg == next_arg && argw == (next_argw & 0x3)) {
			compiler->cache_arg = arg;
			compiler->cache_argw = argw;
			FAIL_IF(push_inst(compiler, ADD | RD(TMP_REG3) | RS1(base) | RS2(!argw ? OFFS_REG(arg) : TMP_REG3)));
			tmp_r = TMP_REG3;
		}
		else
			FAIL_IF(push_inst(compiler, ADD | RD(tmp_r) | RS1(base) | RS2(!argw ? OFFS_REG(arg) : TMP_REG3)));
#endif /* __riscv_zba */

		return push_mem_inst(compiler, flags, reg, tmp_r, 0);
	}

	if (compiler->cache_arg == arg && argw - compiler->cache_argw <= SIMM_MAX && argw - compiler->cache_argw >= SIMM_MIN)
		return push_mem_inst(compiler, flags, reg, TMP_REG3, argw - compiler->cache_argw);

	if (compiler->cache_arg == SLJIT2_MEM && (argw - compiler->cache_argw <= SIMM_MAX) && (argw - compiler->cache_argw >= SIMM_MIN)) {
		offset = argw - compiler->cache_argw;
	} else {
		compiler->cache_arg = SLJIT2_MEM;

		argw_hi = TO_ARGW_HI(argw);

		if (next_arg && next_argw - argw <= SIMM_MAX && next_argw - argw >= SIMM_MIN && argw_hi != TO_ARGW_HI(next_argw)) {
			FAIL_IF(load_immediate(compiler, TMP_REG3, argw, tmp_r));
			compiler->cache_argw = argw;
			offset = 0;
		} else {
			FAIL_IF(load_immediate(compiler, TMP_REG3, argw_hi, tmp_r));
			compiler->cache_argw = argw_hi;
			offset = argw & 0xfff;
			argw = argw_hi;
		}
	}

	if (!base)
		return push_mem_inst(compiler, flags, reg, TMP_REG3, offset);

	if (arg == next_arg && next_argw - argw <= SIMM_MAX && next_argw - argw >= SIMM_MIN) {
		compiler->cache_arg = arg;
		FAIL_IF(push_inst(compiler, ADD | RD(TMP_REG3) | RS1(TMP_REG3) | RS2(base)));
		return push_mem_inst(compiler, flags, reg, TMP_REG3, offset);
	}

	FAIL_IF(push_inst(compiler, ADD | RD(tmp_r) | RS1(TMP_REG3) | RS2(base)));
	return push_mem_inst(compiler, flags, reg, tmp_r, offset);
}

static sljit2_s32 emit_op_mem(struct sljit2_compiler *compiler, sljit2_s32 flags, sljit2_s32 reg, sljit2_s32 arg, sljit2_sw argw)
{
	sljit2_s32 base = arg & REG_MASK;
	sljit2_s32 tmp_r = TMP_REG1;

	if (getput_arg_fast(compiler, flags, reg, arg, argw))
		return compiler->error;

	if ((flags & MEM_MASK) <= GPR_REG && (flags & LOAD_DATA))
		tmp_r = reg;

	if (SLJIT2_UNLIKELY(arg & OFFS_REG_MASK)) {
		argw &= 0x3;

		if (SLJIT2_UNLIKELY(argw)) {
			FAIL_IF(push_inst(compiler, SLLI | RD(tmp_r) | RS1(OFFS_REG(arg)) | IMM_I(argw)));
			FAIL_IF(push_inst(compiler, ADD | RD(tmp_r) | RS1(tmp_r) | RS2(base)));
		}
		else
			FAIL_IF(push_inst(compiler, ADD | RD(tmp_r) | RS1(base) | RS2(OFFS_REG(arg))));

		argw = 0;
	} else {
		FAIL_IF(load_immediate(compiler, tmp_r, TO_ARGW_HI(argw), TMP_REG3));

		if (base != 0)
			FAIL_IF(push_inst(compiler, ADD | RD(tmp_r) | RS1(tmp_r) | RS2(base)));
	}

	return push_mem_inst(compiler, flags, reg, tmp_r, argw & 0xfff);
}

static SLJIT2_INLINE sljit2_s32 emit_op_mem2(struct sljit2_compiler *compiler, sljit2_s32 flags, sljit2_s32 reg, sljit2_s32 arg1, sljit2_sw arg1w, sljit2_s32 arg2, sljit2_sw arg2w)
{
	if (getput_arg_fast(compiler, flags, reg, arg1, arg1w))
		return compiler->error;
	return getput_arg(compiler, flags, reg, arg1, arg1w, arg2, arg2w);
}

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
#define WORD 0
#define WORD_32 0
#define IMM_EXTEND(v) (IMM_I(v))
#else /* !SLJIT2_CONFIG_RISCV_32 */
#define WORD word
#define WORD_32 0x08
#define IMM_EXTEND(v) (IMM_I((op & SLJIT2_32) ? (v) : (32 + (v))))
#endif /* SLJIT2_CONFIG_RISCV_32 */
#ifndef __riscv_zbb
static sljit2_s32 emit_clz_ctz(struct sljit2_compiler *compiler, sljit2_s32 op, sljit2_s32 dst, sljit2_sw src)
{
	sljit2_s32 is_clz = (GET_OPCODE(op) == SLJIT2_CLZ);
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	sljit2_ins word = (sljit2_ins)(op & SLJIT2_32) >> 5;
	sljit2_ins word_size = (op & SLJIT2_32) ? 32 : 64;
#else /* !SLJIT2_CONFIG_RISCV_64 */
	sljit2_ins word_size = 32;
#endif /* SLJIT2_CONFIG_RISCV_64 */

	SLJIT2_ASSERT(WORD == 0 || WORD == 0x8);

	/* The OTHER_FLAG is the counter. */
	FAIL_IF(push_inst(compiler, ADDI | WORD | RD(OTHER_FLAG) | RS1(TMP_ZERO) | IMM_I(word_size)));

	/* The TMP_REG2 is the next value. */
	if (src != TMP_REG2)
		FAIL_IF(push_inst(compiler, ADDI | WORD | RD(TMP_REG2) | RS1(src) | IMM_I(0)));

	FAIL_IF(push_inst(compiler, BEQ | RS1(TMP_REG2) | RS2(TMP_ZERO) | ((sljit2_ins)((is_clz ? 4 : 5) * SSIZE_OF(ins)) << 7) | ((sljit2_ins)(8 * SSIZE_OF(ins)) << 20)));

	FAIL_IF(push_inst(compiler, ADDI | WORD | RD(OTHER_FLAG) | RS1(TMP_ZERO) | IMM_I(0)));
	if (!is_clz) {
		FAIL_IF(push_inst(compiler, ANDI | RD(TMP_REG1) | RS1(TMP_REG2) | IMM_I(1)));
		FAIL_IF(push_inst(compiler, BNE | RS1(TMP_REG1) | RS2(TMP_ZERO) | ((sljit2_ins)(2 * SSIZE_OF(ins)) << 7) | ((sljit2_ins)(8 * SSIZE_OF(ins)) << 20)));
	} else
		FAIL_IF(push_inst(compiler, BLT | RS1(TMP_REG2) | RS2(TMP_ZERO) | ((sljit2_ins)(2 * SSIZE_OF(ins)) << 7) | ((sljit2_ins)(8 * SSIZE_OF(ins)) << 20)));

	/* The TMP_REG1 is the next shift. */
	FAIL_IF(push_inst(compiler, ADDI | WORD | RD(TMP_REG1) | RS1(TMP_ZERO) | IMM_I(word_size)));

	FAIL_IF(push_inst(compiler, ADDI | WORD | RD(EQUAL_FLAG) | RS1(TMP_REG2) | IMM_I(0)));
	FAIL_IF(push_inst(compiler, SRLI | WORD | RD(TMP_REG1) | RS1(TMP_REG1) | IMM_I(1)));

	FAIL_IF(push_inst(compiler, (is_clz ? SRL : SLL) | WORD | RD(TMP_REG2) | RS1(EQUAL_FLAG) | RS2(TMP_REG1)));
	FAIL_IF(push_inst(compiler, BNE | RS1(TMP_REG2) | RS2(TMP_ZERO) | ((sljit2_ins)0xfe000e80 - ((2 * SSIZE_OF(ins)) << 7))));
	FAIL_IF(push_inst(compiler, ADDI | WORD | RD(TMP_REG2) | RS1(TMP_REG1) | IMM_I(-1)));
	FAIL_IF(push_inst(compiler, (is_clz ? SRL : SLL) | WORD | RD(TMP_REG2) | RS1(EQUAL_FLAG) | RS2(TMP_REG2)));
	FAIL_IF(push_inst(compiler, OR | RD(OTHER_FLAG) | RS1(OTHER_FLAG) | RS2(TMP_REG1)));
	FAIL_IF(push_inst(compiler, BEQ | RS1(TMP_REG2) | RS2(TMP_ZERO) | ((sljit2_ins)0xfe000e80 - ((5 * SSIZE_OF(ins)) << 7))));

	return push_inst(compiler, ADDI | WORD | RD(dst) | RS1(OTHER_FLAG) | IMM_I(0));
}

static sljit2_s32 emit_rev(struct sljit2_compiler *compiler, sljit2_s32 op, sljit2_s32 dst, sljit2_sw src)
{
	SLJIT2_UNUSED_ARG(op);

#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	if (!(op & SLJIT2_32)) {
		FAIL_IF(push_inst(compiler, LUI | RD(OTHER_FLAG) | 0x10000));
		FAIL_IF(push_inst(compiler, SRLI | RD(TMP_REG1) | RS1(src) | IMM_I(32)));
		FAIL_IF(push_inst(compiler, ADDI | RD(OTHER_FLAG) | RS1(OTHER_FLAG) | IMM_I(0xfff)));
		FAIL_IF(push_inst(compiler, SLLI | RD(dst) | RS1(src) | IMM_I(32)));
		FAIL_IF(push_inst(compiler, SLLI | RD(EQUAL_FLAG) | RS1(OTHER_FLAG) | IMM_I(32)));
		FAIL_IF(push_inst(compiler, OR | RD(dst) | RS1(dst) | RS2(TMP_REG1)));
		FAIL_IF(push_inst(compiler, OR | RD(OTHER_FLAG) | RS1(OTHER_FLAG) | RS2(EQUAL_FLAG)));

		FAIL_IF(push_inst(compiler, SRLI | RD(TMP_REG1) | RS1(dst) | IMM_I(16)));
		FAIL_IF(push_inst(compiler, AND | RD(dst) | RS1(dst) | RS2(OTHER_FLAG)));
		FAIL_IF(push_inst(compiler, AND | RD(TMP_REG1) | RS1(TMP_REG1) | RS2(OTHER_FLAG)));
		FAIL_IF(push_inst(compiler, SLLI | RD(EQUAL_FLAG) | RS1(OTHER_FLAG) | IMM_I(8)));
		FAIL_IF(push_inst(compiler, SLLI | RD(dst) | RS1(dst) | IMM_I(16)));
		FAIL_IF(push_inst(compiler, XOR | RD(OTHER_FLAG) | RS1(OTHER_FLAG) | RS2(EQUAL_FLAG)));
		FAIL_IF(push_inst(compiler, OR | RD(dst) | RS1(dst) | RS2(TMP_REG1)));

		FAIL_IF(push_inst(compiler, SRLI | RD(TMP_REG1) | RS1(dst) | IMM_I(8)));
		FAIL_IF(push_inst(compiler, AND | RD(dst) | RS1(dst) | RS2(OTHER_FLAG)));
		FAIL_IF(push_inst(compiler, AND | RD(TMP_REG1) | RS1(TMP_REG1) | RS2(OTHER_FLAG)));
		FAIL_IF(push_inst(compiler, SLLI | RD(dst) | RS1(dst) | IMM_I(8)));
		return push_inst(compiler, OR | RD(dst) | RS1(dst) | RS2(TMP_REG1));
	}
#endif /* SLJIT2_CONFIG_RISCV_64 */

	FAIL_IF(push_inst(compiler, SRLI | WORD_32 | RD(TMP_REG1) | RS1(src) | IMM_I(16)));
	FAIL_IF(push_inst(compiler, LUI | RD(OTHER_FLAG) | 0xff0000));
	FAIL_IF(push_inst(compiler, SLLI | WORD_32 | RD(dst) | RS1(src) | IMM_I(16)));
	FAIL_IF(push_inst(compiler, ORI | RD(OTHER_FLAG) | RS1(OTHER_FLAG) | IMM_I(0xff)));
	FAIL_IF(push_inst(compiler, OR | RD(dst) | RS1(dst) | RS2(TMP_REG1)));

	FAIL_IF(push_inst(compiler, SRLI | WORD_32 | RD(TMP_REG1) | RS1(dst) | IMM_I(8)));
	FAIL_IF(push_inst(compiler, AND | RD(dst) | RS1(dst) | RS2(OTHER_FLAG)));
	FAIL_IF(push_inst(compiler, AND | RD(TMP_REG1) | RS1(TMP_REG1) | RS2(OTHER_FLAG)));
	FAIL_IF(push_inst(compiler, SLLI | WORD_32 | RD(dst) | RS1(dst) | IMM_I(8)));
	return push_inst(compiler, OR | RD(dst) | RS1(dst) | RS2(TMP_REG1));
}

static sljit2_s32 emit_rev16(struct sljit2_compiler *compiler, sljit2_s32 op, sljit2_s32 dst, sljit2_sw src)
{
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	sljit2_ins word = (sljit2_ins)(op & SLJIT2_32) >> 5;
	sljit2_ins word_size = (op & SLJIT2_32) ? 32 : 64;
#else /* !SLJIT2_CONFIG_RISCV_64 */
	sljit2_ins word_size = 32;
#endif /* SLJIT2_CONFIG_RISCV_64 */

	FAIL_IF(push_inst(compiler, SRLI | WORD | RD(TMP_REG1) | RS1(src) | IMM_I(8)));
	FAIL_IF(push_inst(compiler, SLLI | WORD | RD(dst) | RS1(src) | IMM_I(word_size - 8)));
	FAIL_IF(push_inst(compiler, ANDI | RD(TMP_REG1) | RS1(TMP_REG1) | IMM_I(0xff)));
	FAIL_IF(push_inst(compiler, (GET_OPCODE(op) == SLJIT2_REV_U16 ? SRLI : SRAI) | WORD | RD(dst) | RS1(dst) | IMM_I(word_size - 16)));
	return push_inst(compiler, OR | RD(dst) | RS1(dst) | RS2(TMP_REG1));
}
#endif /* !__riscv_zbb */

#define EMIT_LOGICAL(op_imm, op_reg) \
	if (flags & SRC2_IMM) { \
		if (op & SLJIT2_SET_Z) \
			FAIL_IF(push_inst(compiler, op_imm | RD(EQUAL_FLAG) | RS1(src1) | IMM_I(src2))); \
		if (!(flags & UNUSED_DEST)) \
			FAIL_IF(push_inst(compiler, op_imm | RD(dst) | RS1(src1) | IMM_I(src2))); \
	} \
	else { \
		if (op & SLJIT2_SET_Z) \
			FAIL_IF(push_inst(compiler, op_reg | RD(EQUAL_FLAG) | RS1(src1) | RS2(src2))); \
		if (!(flags & UNUSED_DEST)) \
			FAIL_IF(push_inst(compiler, op_reg | RD(dst) | RS1(src1) | RS2(src2))); \
	}

#define EMIT_SHIFT(imm, reg) \
	op_imm = (imm); \
	op_reg = (reg);

static SLJIT2_INLINE sljit2_s32 emit_single_op(struct sljit2_compiler *compiler, sljit2_s32 op, sljit2_s32 flags,
	sljit2_s32 dst, sljit2_s32 src1, sljit2_sw src2)
{
	sljit2_s32 is_overflow, is_carry, carry_src_r, is_handled, reg;
	sljit2_ins op_imm, op_reg;
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	sljit2_ins word = (sljit2_ins)(op & SLJIT2_32) >> 5;
#endif /* SLJIT2_CONFIG_RISCV_64 */

	SLJIT2_ASSERT(WORD == 0 || WORD == 0x8);

	switch (GET_OPCODE(op)) {
	case SLJIT2_MOV:
		SLJIT2_ASSERT(src1 == TMP_ZERO && !(flags & SRC2_IMM));
		if (dst != src2)
			return push_inst(compiler, ADDI | RD(dst) | RS1(src2) | IMM_I(0));
		return SLJIT2_SUCCESS;

	case SLJIT2_MOV_U8:
		SLJIT2_ASSERT(src1 == TMP_ZERO && !(flags & SRC2_IMM));
		if ((flags & (REG_DEST | REG2_SOURCE)) == (REG_DEST | REG2_SOURCE))
			return push_inst(compiler, ANDI | RD(dst) | RS1(src2) | IMM_I(0xff));
		SLJIT2_ASSERT(dst == src2);
		return SLJIT2_SUCCESS;

	case SLJIT2_MOV_S8:
#if defined __riscv_zbb
		return push_inst(compiler, SEXTB | RD(dst) | RS1(src2));
#else /* !__riscv_zbb */
		SLJIT2_ASSERT(src1 == TMP_ZERO && !(flags & SRC2_IMM));
		if ((flags & (REG_DEST | REG2_SOURCE)) == (REG_DEST | REG2_SOURCE)) {
			FAIL_IF(push_inst(compiler, SLLI | WORD | RD(dst) | RS1(src2) | IMM_EXTEND(24)));
			return push_inst(compiler, SRAI | WORD | RD(dst) | RS1(dst) | IMM_EXTEND(24));
		}
		SLJIT2_ASSERT(dst == src2);
		return SLJIT2_SUCCESS;
#endif /* __riscv_zbb */

	case SLJIT2_MOV_U16:
#if defined __riscv_zbb
		return push_inst(compiler, ZEXTH | RD(dst) | RS1(src2));
#else /* !__riscv_zbb */
		SLJIT2_ASSERT(src1 == TMP_ZERO && !(flags & SRC2_IMM));
		if ((flags & (REG_DEST | REG2_SOURCE)) == (REG_DEST | REG2_SOURCE)) {
			FAIL_IF(push_inst(compiler, SLLI | WORD | RD(dst) | RS1(src2) | IMM_EXTEND(16)));
			return push_inst(compiler, SRLI | WORD | RD(dst) | RS1(dst) | IMM_EXTEND(16));
		}
		SLJIT2_ASSERT(dst == src2);
		return SLJIT2_SUCCESS;
#endif /* __riscv_zbb */

	case SLJIT2_MOV_S16:
#if defined __riscv_zbb
		return push_inst(compiler, SEXTH | RD(dst) | RS1(src2));
#else /* !__riscv_zbb */
		SLJIT2_ASSERT(src1 == TMP_ZERO && !(flags & SRC2_IMM));
		if ((flags & (REG_DEST | REG2_SOURCE)) == (REG_DEST | REG2_SOURCE)) {
			FAIL_IF(push_inst(compiler, SLLI | WORD | RD(dst) | RS1(src2) | IMM_EXTEND(16)));
			return push_inst(compiler, SRAI | WORD | RD(dst) | RS1(dst) | IMM_EXTEND(16));
		}
		SLJIT2_ASSERT(dst == src2);
		return SLJIT2_SUCCESS;
#endif /* !__riscv_zbb */

#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	case SLJIT2_MOV_U32:
		SLJIT2_ASSERT(src1 == TMP_ZERO && !(flags & SRC2_IMM));
		if ((flags & (REG_DEST | REG2_SOURCE)) == (REG_DEST | REG2_SOURCE)) {
			FAIL_IF(push_inst(compiler, SLLI | RD(dst) | RS1(src2) | IMM_I(32)));
			return push_inst(compiler, SRLI | RD(dst) | RS1(dst) | IMM_I(32));
		}
		SLJIT2_ASSERT(dst == src2);
		return SLJIT2_SUCCESS;

	case SLJIT2_MOV_S32:
		SLJIT2_ASSERT(src1 == TMP_ZERO && !(flags & SRC2_IMM));
		if ((flags & (REG_DEST | REG2_SOURCE)) == (REG_DEST | REG2_SOURCE))
			return push_inst(compiler, ADDI | 0x8 | RD(dst) | RS1(src2) | IMM_I(0));
		SLJIT2_ASSERT(dst == src2);
		return SLJIT2_SUCCESS;
#endif /* SLJIT2_CONFIG_RISCV_64 */

	case SLJIT2_CLZ:
#if defined __riscv_zbb
		return push_inst(compiler, CLZ | WORD | RD(dst) | RS1(src2));
#endif /* __riscv_zbb */
	case SLJIT2_CTZ:
#if defined __riscv_zbb
		return push_inst(compiler, CTZ | WORD | RD(dst) | RS1(src2));
#else /* !__riscv_zbb */
		SLJIT2_ASSERT(src1 == TMP_ZERO && !(flags & SRC2_IMM));
		return emit_clz_ctz(compiler, op, dst, src2);
#endif /* __riscv_zbb */

	case SLJIT2_REV:
#if defined __riscv_zbb
		SLJIT2_ASSERT(src1 == TMP_ZERO && !(flags & SRC2_IMM));
		FAIL_IF(push_inst(compiler, REV8 | RD(dst) | RS1(src2)));
#if defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64
		if (op & SLJIT2_32)
			return push_inst(compiler, SRAI | RD(dst) | RS1(dst) | IMM_I(32));
		return SLJIT2_SUCCESS;
#else /* !SLJIT2_CONFIG_RISCV_64 */
		return SLJIT2_SUCCESS;
#endif /* SLJIT2_CONFIG_RISCV_64 */
#endif /* __riscv_zbb */
	case SLJIT2_REV_S32:
#if ((defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32) || defined __riscv_zbb)
	case SLJIT2_REV_U32:
#endif /* SLJIT2_CONFIG_RISCV_32 || __riscv_zbb */
		SLJIT2_ASSERT(src1 == TMP_ZERO && !(flags & SRC2_IMM));
#if defined __riscv_zbb
		FAIL_IF(push_inst(compiler, REV8 | RD(dst) | RS1(src2)));
#if defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64
		return push_inst(compiler, (GET_OPCODE(op) == SLJIT2_REV_U32 ? SRLI : SRAI )| RD(dst) | RS1(dst) | IMM_I(32));
#else /* !SLJIT2_CONFIG_RISCV_64 */
		return SLJIT2_SUCCESS;
#endif /* SLJIT2_CONFIG_RISCV_64 */
#else /* !__riscv_zbb */
		return emit_rev(compiler, op, dst, src2);
#endif /* __riscv_zbb */
	case SLJIT2_REV_U16:
	case SLJIT2_REV_S16:
		SLJIT2_ASSERT(src1 == TMP_ZERO && !(flags & SRC2_IMM));
#if defined __riscv_zbb
		FAIL_IF(push_inst(compiler, REV8 | RD(dst) | RS1(src2)));
#if defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64
		return push_inst(compiler, (GET_OPCODE(op) == SLJIT2_REV_U16 ? SRLI : SRAI )| RD(dst) | RS1(dst) | IMM_I(48));
#else /* !SLJIT2_CONFIG_RISCV_64 */
		return push_inst(compiler, (GET_OPCODE(op) == SLJIT2_REV_U16 ? SRLI : SRAI) | RD(dst) | RS1(dst) | IMM_I(16));
#endif /* SLJIT2_CONFIG_RISCV_64 */
#else /* !__riscv_zbb */
		return emit_rev16(compiler, op, dst, src2);
#endif /* __riscv_zbb */

#if ((defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64) && !defined __riscv_zbb)
	case SLJIT2_REV_U32:
		SLJIT2_ASSERT(src1 == TMP_ZERO && !(flags & SRC2_IMM) && dst != TMP_REG1);
		FAIL_IF(emit_rev(compiler, op, dst, src2));
		if (dst == TMP_REG2)
			return SLJIT2_SUCCESS;
		FAIL_IF(push_inst(compiler, SLLI | RD(dst) | RS1(dst) | IMM_I(32)));
		return push_inst(compiler, SRLI | RD(dst) | RS1(dst) | IMM_I(32));
#endif /* SLJIT2_CONFIG_RISCV_64 && !__riscv_zbb */
	case SLJIT2_ADD:
		/* Overflow computation (both add and sub): overflow = src1_sign ^ src2_sign ^ result_sign ^ carry_flag */
		is_overflow = GET_FLAG_TYPE(op) == SLJIT2_OVERFLOW;
		carry_src_r = GET_FLAG_TYPE(op) == SLJIT2_CARRY;

		if (flags & SRC2_IMM) {
			if (is_overflow) {
				if (src2 >= 0)
					FAIL_IF(push_inst(compiler, ADDI | RD(EQUAL_FLAG) | RS1(src1) | IMM_I(0)));
				else
					FAIL_IF(push_inst(compiler, XORI | RD(EQUAL_FLAG) | RS1(src1) | IMM_I(-1)));
			}
			else if (op & SLJIT2_SET_Z)
				FAIL_IF(push_inst(compiler, ADDI | WORD | RD(EQUAL_FLAG) | RS1(src1) | IMM_I(src2)));

			/* Only the zero flag is needed. */
			if (!(flags & UNUSED_DEST) || (op & VARIABLE_FLAG_MASK))
				FAIL_IF(push_inst(compiler, ADDI | WORD | RD(dst) | RS1(src1) | IMM_I(src2)));
		}
		else {
			if (is_overflow)
				FAIL_IF(push_inst(compiler, XOR | RD(EQUAL_FLAG) | RS1(src1) | RS2(src2)));
			else if (op & SLJIT2_SET_Z)
				FAIL_IF(push_inst(compiler, ADD | WORD | RD(EQUAL_FLAG) | RS1(src1) | RS2(src2)));

			if (is_overflow || carry_src_r != 0) {
				if (src1 != dst)
					carry_src_r = (sljit2_s32)src1;
				else if (src2 != dst)
					carry_src_r = (sljit2_s32)src2;
				else {
					FAIL_IF(push_inst(compiler, ADDI | RD(OTHER_FLAG) | RS1(src1) | IMM_I(0)));
					carry_src_r = OTHER_FLAG;
				}
			}

			/* Only the zero flag is needed. */
			if (!(flags & UNUSED_DEST) || (op & VARIABLE_FLAG_MASK))
				FAIL_IF(push_inst(compiler, ADD | WORD | RD(dst) | RS1(src1) | RS2(src2)));
		}

		/* Carry is zero if a + b >= a or a + b >= b, otherwise it is 1. */
		if (is_overflow || carry_src_r != 0) {
			if (flags & SRC2_IMM)
				FAIL_IF(push_inst(compiler, SLTUI | RD(OTHER_FLAG) | RS1(dst) | IMM_I(src2)));
			else
				FAIL_IF(push_inst(compiler, SLTU | RD(OTHER_FLAG) | RS1(dst) | RS2(carry_src_r)));
		}

		if (!is_overflow)
			return SLJIT2_SUCCESS;

		FAIL_IF(push_inst(compiler, XOR | RD(TMP_REG1) | RS1(dst) | RS2(EQUAL_FLAG)));
		if (op & SLJIT2_SET_Z)
			FAIL_IF(push_inst(compiler, ADDI | RD(EQUAL_FLAG) | RS1(dst) | IMM_I(0)));
		FAIL_IF(push_inst(compiler, SRLI | WORD | RD(TMP_REG1) | RS1(TMP_REG1) | IMM_EXTEND(31)));
		return push_inst(compiler, XOR | RD(OTHER_FLAG) | RS1(TMP_REG1) | RS2(OTHER_FLAG));

	case SLJIT2_ADDC:
		carry_src_r = GET_FLAG_TYPE(op) == SLJIT2_CARRY;

		if (flags & SRC2_IMM) {
			FAIL_IF(push_inst(compiler, ADDI | WORD | RD(dst) | RS1(src1) | IMM_I(src2)));
		} else {
			if (carry_src_r != 0) {
				if (src1 != dst)
					carry_src_r = (sljit2_s32)src1;
				else if (src2 != dst)
					carry_src_r = (sljit2_s32)src2;
				else {
					FAIL_IF(push_inst(compiler, ADDI | RD(EQUAL_FLAG) | RS1(src1) | IMM_I(0)));
					carry_src_r = EQUAL_FLAG;
				}
			}

			FAIL_IF(push_inst(compiler, ADD | WORD | RD(dst) | RS1(src1) | RS2(src2)));
		}

		/* Carry is zero if a + b >= a or a + b >= b, otherwise it is 1. */
		if (carry_src_r != 0) {
			if (flags & SRC2_IMM)
				FAIL_IF(push_inst(compiler, SLTUI | RD(EQUAL_FLAG) | RS1(dst) | IMM_I(src2)));
			else
				FAIL_IF(push_inst(compiler, SLTU | RD(EQUAL_FLAG) | RS1(dst) | RS2(carry_src_r)));
		}

		FAIL_IF(push_inst(compiler, ADD | WORD | RD(dst) | RS1(dst) | RS2(OTHER_FLAG)));

		if (carry_src_r == 0)
			return SLJIT2_SUCCESS;

		/* Set ULESS_FLAG (dst == 0) && (OTHER_FLAG == 1). */
		FAIL_IF(push_inst(compiler, SLTU | RD(OTHER_FLAG) | RS1(dst) | RS2(OTHER_FLAG)));
		/* Set carry flag. */
		return push_inst(compiler, OR | RD(OTHER_FLAG) | RS1(OTHER_FLAG) | RS2(EQUAL_FLAG));

	case SLJIT2_SUB:
		if ((flags & SRC2_IMM) && src2 == SIMM_MIN) {
			FAIL_IF(push_inst(compiler, ADDI | RD(TMP_REG2) | RS1(TMP_ZERO) | IMM_I(src2)));
			src2 = TMP_REG2;
			flags &= ~SRC2_IMM;
		}

		is_handled = 0;

		if (flags & SRC2_IMM) {
			if (GET_FLAG_TYPE(op) == SLJIT2_LESS) {
				FAIL_IF(push_inst(compiler, SLTUI | RD(OTHER_FLAG) | RS1(src1) | IMM_I(src2)));
				is_handled = 1;
			}
			else if (GET_FLAG_TYPE(op) == SLJIT2_SIG_LESS) {
				FAIL_IF(push_inst(compiler, SLTI | RD(OTHER_FLAG) | RS1(src1) | IMM_I(src2)));
				is_handled = 1;
			}
		}

		if (!is_handled && GET_FLAG_TYPE(op) >= SLJIT2_LESS && GET_FLAG_TYPE(op) <= SLJIT2_SIG_LESS_EQUAL) {
			is_handled = 1;

			if (flags & SRC2_IMM) {
				reg = (src1 == TMP_REG1) ? TMP_REG2 : TMP_REG1;
				FAIL_IF(push_inst(compiler, ADDI | RD(reg) | RS1(TMP_ZERO) | IMM_I(src2)));
				src2 = reg;
				flags &= ~SRC2_IMM;
			}

			switch (GET_FLAG_TYPE(op)) {
			case SLJIT2_LESS:
				FAIL_IF(push_inst(compiler, SLTU | RD(OTHER_FLAG) | RS1(src1) | RS2(src2)));
				break;
			case SLJIT2_GREATER:
				FAIL_IF(push_inst(compiler, SLTU | RD(OTHER_FLAG) | RS1(src2) | RS2(src1)));
				break;
			case SLJIT2_SIG_LESS:
				FAIL_IF(push_inst(compiler, SLT | RD(OTHER_FLAG) | RS1(src1) | RS2(src2)));
				break;
			case SLJIT2_SIG_GREATER:
				FAIL_IF(push_inst(compiler, SLT | RD(OTHER_FLAG) | RS1(src2) | RS2(src1)));
				break;
			}
		}

		if (is_handled) {
			if (flags & SRC2_IMM) {
				if (op & SLJIT2_SET_Z)
					FAIL_IF(push_inst(compiler, ADDI | WORD | RD(EQUAL_FLAG) | RS1(src1) | IMM_I(-src2)));
				if (!(flags & UNUSED_DEST))
					return push_inst(compiler, ADDI | WORD | RD(dst) | RS1(src1) | IMM_I(-src2));
			}
			else {
				if (op & SLJIT2_SET_Z)
					FAIL_IF(push_inst(compiler, SUB | WORD | RD(EQUAL_FLAG) | RS1(src1) | RS2(src2)));
				if (!(flags & UNUSED_DEST))
					return push_inst(compiler, SUB | WORD | RD(dst) | RS1(src1) | RS2(src2));
			}
			return SLJIT2_SUCCESS;
		}

		is_overflow = GET_FLAG_TYPE(op) == SLJIT2_OVERFLOW;
		is_carry = GET_FLAG_TYPE(op) == SLJIT2_CARRY;

		if (flags & SRC2_IMM) {
			if (is_overflow) {
				if (src2 >= 0)
					FAIL_IF(push_inst(compiler, ADDI | RD(EQUAL_FLAG) | RS1(src1) | IMM_I(0)));
				else
					FAIL_IF(push_inst(compiler, XORI | RD(EQUAL_FLAG) | RS1(src1) | IMM_I(-1)));
			}
			else if (op & SLJIT2_SET_Z)
				FAIL_IF(push_inst(compiler, ADDI | WORD | RD(EQUAL_FLAG) | RS1(src1) | IMM_I(-src2)));

			if (is_overflow || is_carry)
				FAIL_IF(push_inst(compiler, SLTUI | RD(OTHER_FLAG) | RS1(src1) | IMM_I(src2)));

			/* Only the zero flag is needed. */
			if (!(flags & UNUSED_DEST) || (op & VARIABLE_FLAG_MASK))
				FAIL_IF(push_inst(compiler, ADDI | WORD | RD(dst) | RS1(src1) | IMM_I(-src2)));
		}
		else {
			if (is_overflow)
				FAIL_IF(push_inst(compiler, XOR | RD(EQUAL_FLAG) | RS1(src1) | RS2(src2)));
			else if (op & SLJIT2_SET_Z)
				FAIL_IF(push_inst(compiler, SUB | WORD | RD(EQUAL_FLAG) | RS1(src1) | RS2(src2)));

			if (is_overflow || is_carry)
				FAIL_IF(push_inst(compiler, SLTU | RD(OTHER_FLAG) | RS1(src1) | RS2(src2)));

			/* Only the zero flag is needed. */
			if (!(flags & UNUSED_DEST) || (op & VARIABLE_FLAG_MASK))
				FAIL_IF(push_inst(compiler, SUB | WORD | RD(dst) | RS1(src1) | RS2(src2)));
		}

		if (!is_overflow)
			return SLJIT2_SUCCESS;

		FAIL_IF(push_inst(compiler, XOR | RD(TMP_REG1) | RS1(dst) | RS2(EQUAL_FLAG)));
		if (op & SLJIT2_SET_Z)
			FAIL_IF(push_inst(compiler, ADDI | RD(EQUAL_FLAG) | RS1(dst) | IMM_I(0)));
		FAIL_IF(push_inst(compiler, SRLI | WORD | RD(TMP_REG1) | RS1(TMP_REG1) | IMM_EXTEND(31)));
		return push_inst(compiler, XOR | RD(OTHER_FLAG) | RS1(TMP_REG1) | RS2(OTHER_FLAG));

	case SLJIT2_SUBC:
		if ((flags & SRC2_IMM) && src2 == SIMM_MIN) {
			FAIL_IF(push_inst(compiler, ADDI | RD(TMP_REG2) | RS1(TMP_ZERO) | IMM_I(src2)));
			src2 = TMP_REG2;
			flags &= ~SRC2_IMM;
		}

		is_carry = GET_FLAG_TYPE(op) == SLJIT2_CARRY;

		if (flags & SRC2_IMM) {
			if (is_carry)
				FAIL_IF(push_inst(compiler, SLTUI | RD(EQUAL_FLAG) | RS1(src1) | IMM_I(src2)));

			FAIL_IF(push_inst(compiler, ADDI | WORD | RD(dst) | RS1(src1) | IMM_I(-src2)));
		}
		else {
			if (is_carry)
				FAIL_IF(push_inst(compiler, SLTU | RD(EQUAL_FLAG) | RS1(src1) | RS2(src2)));

			FAIL_IF(push_inst(compiler, SUB | WORD | RD(dst) | RS1(src1) | RS2(src2)));
		}

		if (is_carry)
			FAIL_IF(push_inst(compiler, SLTU | RD(TMP_REG1) | RS1(dst) | RS2(OTHER_FLAG)));

		FAIL_IF(push_inst(compiler, SUB | WORD | RD(dst) | RS1(dst) | RS2(OTHER_FLAG)));

		if (!is_carry)
			return SLJIT2_SUCCESS;

		return push_inst(compiler, OR | RD(OTHER_FLAG) | RS1(EQUAL_FLAG) | RS2(TMP_REG1));

	case SLJIT2_MUL:
		SLJIT2_ASSERT(!(flags & SRC2_IMM));

		if (GET_FLAG_TYPE(op) != SLJIT2_OVERFLOW)
			return push_inst(compiler, MUL | WORD | RD(dst) | RS1(src1) | RS2(src2));

#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
		if (word) {
			FAIL_IF(push_inst(compiler, MUL | RD(OTHER_FLAG) | RS1(src1) | RS2(src2)));
			FAIL_IF(push_inst(compiler, MUL | 0x8 | RD(dst) | RS1(src1) | RS2(src2)));
			return push_inst(compiler, SUB | RD(OTHER_FLAG) | RS1(dst) | RS2(OTHER_FLAG));
		}
#endif /* SLJIT2_CONFIG_RISCV_64 */

		FAIL_IF(push_inst(compiler, MULH | RD(EQUAL_FLAG) | RS1(src1) | RS2(src2)));
		FAIL_IF(push_inst(compiler, MUL | RD(dst) | RS1(src1) | RS2(src2)));
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
		FAIL_IF(push_inst(compiler, SRAI | RD(OTHER_FLAG) | RS1(dst) | IMM_I(31)));
#else /* !SLJIT2_CONFIG_RISCV_32 */
		FAIL_IF(push_inst(compiler, SRAI | RD(OTHER_FLAG) | RS1(dst) | IMM_I(63)));
#endif /* SLJIT2_CONFIG_RISCV_32 */
		return push_inst(compiler, SUB | RD(OTHER_FLAG) | RS1(EQUAL_FLAG) | RS2(OTHER_FLAG));

	case SLJIT2_AND:
		EMIT_LOGICAL(ANDI, AND);
		return SLJIT2_SUCCESS;

	case SLJIT2_OR:
		EMIT_LOGICAL(ORI, OR);
		return SLJIT2_SUCCESS;

	case SLJIT2_XOR:
		EMIT_LOGICAL(XORI, XOR);
		return SLJIT2_SUCCESS;

	case SLJIT2_SHL:
	case SLJIT2_MSHL:
		EMIT_SHIFT(SLLI, SLL);
		break;

	case SLJIT2_LSHR:
	case SLJIT2_MLSHR:
		EMIT_SHIFT(SRLI, SRL);
		break;

	case SLJIT2_ASHR:
	case SLJIT2_MASHR:
		EMIT_SHIFT(SRAI, SRA);
		break;

	case SLJIT2_ROTL:
	case SLJIT2_ROTR:
		if (flags & SRC2_IMM) {
			SLJIT2_ASSERT(src2 != 0);
#if defined __riscv_zbb
			if (GET_OPCODE(op) == SLJIT2_ROTL) {
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
				src2 = ((op & SLJIT2_32) ? 32 : 64) - src2;
#else /* !SLJIT2_CONFIG_RISCV_64 */
				src2 = 32 - src2;
#endif /* SLJIT2_CONFIG_RISCV_64 */
			}
			return push_inst(compiler, RORI | WORD | RD(dst) | RS1(src1) | IMM_I(src2));
#else /* !__riscv_zbb */
			op_imm = (GET_OPCODE(op) == SLJIT2_ROTL) ? SLLI : SRLI;
			FAIL_IF(push_inst(compiler, op_imm | WORD | RD(OTHER_FLAG) | RS1(src1) | IMM_I(src2)));

#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
			src2 = ((op & SLJIT2_32) ? 32 : 64) - src2;
#else /* !SLJIT2_CONFIG_RISCV_64 */
			src2 = 32 - src2;
#endif /* SLJIT2_CONFIG_RISCV_64 */
			op_imm = (GET_OPCODE(op) == SLJIT2_ROTL) ? SRLI : SLLI;
			FAIL_IF(push_inst(compiler, op_imm | WORD | RD(dst) | RS1(src1) | IMM_I(src2)));
			return push_inst(compiler, OR | RD(dst) | RS1(dst) | RS2(OTHER_FLAG));
#endif /* !__riscv_zbb */
		}

#if defined __riscv_zbb
		return push_inst(compiler, (GET_OPCODE(op) == SLJIT2_ROTL ? ROL : ROR) | WORD | RD(dst) | RS1(src1) | RS2(src2));
#else /* !__riscv_zbb */
		if (src2 == TMP_ZERO) {
			if (dst != src1)
				return push_inst(compiler, ADDI | WORD | RD(dst) | RS1(src1) | IMM_I(0));
			return SLJIT2_SUCCESS;
		}

		FAIL_IF(push_inst(compiler, SUB | WORD | RD(EQUAL_FLAG) | RS1(TMP_ZERO) | RS2(src2)));
		op_reg = (GET_OPCODE(op) == SLJIT2_ROTL) ? SLL : SRL;
		FAIL_IF(push_inst(compiler, op_reg | WORD | RD(OTHER_FLAG) | RS1(src1) | RS2(src2)));
		op_reg = (GET_OPCODE(op) == SLJIT2_ROTL) ? SRL : SLL;
		FAIL_IF(push_inst(compiler, op_reg | WORD | RD(dst) | RS1(src1) | RS2(EQUAL_FLAG)));
		return push_inst(compiler, OR | RD(dst) | RS1(dst) | RS2(OTHER_FLAG));
#endif /* !riscv_zbb */
	default:
		SLJIT2_UNREACHABLE();
		return SLJIT2_SUCCESS;
	}

	if (flags & SRC2_IMM) {
		if (op & SLJIT2_SET_Z)
			FAIL_IF(push_inst(compiler, op_imm | WORD | RD(EQUAL_FLAG) | RS1(src1) | IMM_I(src2)));

		if (flags & UNUSED_DEST)
			return SLJIT2_SUCCESS;
		return push_inst(compiler, op_imm | WORD | RD(dst) | RS1(src1) | IMM_I(src2));
	}

	if (op & SLJIT2_SET_Z)
		FAIL_IF(push_inst(compiler, op_reg | WORD | RD(EQUAL_FLAG) | RS1(src1) | RS2(src2)));

	if (flags & UNUSED_DEST)
		return SLJIT2_SUCCESS;
	return push_inst(compiler, op_reg | WORD | RD(dst) | RS1(src1) | RS2(src2));
}

#undef IMM_EXTEND

static sljit2_s32 emit_op(struct sljit2_compiler *compiler, sljit2_s32 op, sljit2_s32 flags,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	/* arg1 goes to TMP_REG1 or src reg
	   arg2 goes to TMP_REG2, imm or src reg
	   TMP_REG3 can be used for caching
	   result goes to TMP_REG2, so put result can use TMP_REG1 and TMP_REG3. */
	sljit2_s32 dst_r = TMP_REG2;
	sljit2_s32 src1_r;
	sljit2_sw src2_r = 0;
	sljit2_s32 src2_tmp_reg = (GET_OPCODE(op) >= SLJIT2_OP2_BASE && FAST_IS_REG(src1)) ? TMP_REG1 : TMP_REG2;

	if (!(flags & ALT_KEEP_CACHE)) {
		compiler->cache_arg = 0;
		compiler->cache_argw = 0;
	}

	if (dst == 0) {
		SLJIT2_ASSERT(HAS_FLAGS(op));
		flags |= UNUSED_DEST;
		dst = TMP_REG2;
	}
	else if (FAST_IS_REG(dst)) {
		dst_r = dst;
		flags |= REG_DEST;
		if (flags & MOVE_OP)
			src2_tmp_reg = dst_r;
	}
	else if ((dst & SLJIT2_MEM) && !getput_arg_fast(compiler, flags | ARG_TEST, TMP_REG1, dst, dstw))
		flags |= SLOW_DEST;

	if (flags & IMM_OP) {
		if (src2 == SLJIT2_IMM && src2w != 0 && src2w <= SIMM_MAX && src2w >= SIMM_MIN) {
			flags |= SRC2_IMM;
			src2_r = src2w;
		}
		else if ((flags & CUMULATIVE_OP) && src1 == SLJIT2_IMM && src1w != 0 && src1w <= SIMM_MAX && src1w >= SIMM_MIN) {
			flags |= SRC2_IMM;
			src2_r = src1w;

			/* And swap arguments. */
			src1 = src2;
			src1w = src2w;
			src2 = SLJIT2_IMM;
			/* src2w = src2_r unneeded. */
		}
	}

	/* Source 1. */
	if (FAST_IS_REG(src1)) {
		src1_r = src1;
		flags |= REG1_SOURCE;
	} else if (src1 == SLJIT2_IMM) {
		if (src1w) {
			FAIL_IF(load_immediate(compiler, TMP_REG1, src1w, TMP_REG3));
			src1_r = TMP_REG1;
		}
		else
			src1_r = TMP_ZERO;
	} else {
		if (getput_arg_fast(compiler, flags | LOAD_DATA, TMP_REG1, src1, src1w))
			FAIL_IF(compiler->error);
		else
			flags |= SLOW_SRC1;
		src1_r = TMP_REG1;
	}

	/* Source 2. */
	if (FAST_IS_REG(src2)) {
		src2_r = src2;
		flags |= REG2_SOURCE;
		if ((flags & (REG_DEST | MOVE_OP)) == MOVE_OP)
			dst_r = (sljit2_s32)src2_r;
	} else if (src2 == SLJIT2_IMM) {
		if (!(flags & SRC2_IMM)) {
			if (src2w) {
				FAIL_IF(load_immediate(compiler, src2_tmp_reg, src2w, TMP_REG3));
				src2_r = src2_tmp_reg;
			} else {
				src2_r = TMP_ZERO;
				if (flags & MOVE_OP) {
					if (dst & SLJIT2_MEM)
						dst_r = 0;
					else
						op = SLJIT2_MOV;
				}
			}
		}
	} else {
		if (getput_arg_fast(compiler, flags | LOAD_DATA, src2_tmp_reg, src2, src2w))
			FAIL_IF(compiler->error);
		else
			flags |= SLOW_SRC2;
		src2_r = src2_tmp_reg;
	}

	if ((flags & (SLOW_SRC1 | SLOW_SRC2)) == (SLOW_SRC1 | SLOW_SRC2)) {
		SLJIT2_ASSERT(src2_r == TMP_REG2);
		if ((flags & SLOW_DEST) && !can_cache(src2, src2w, src1, src1w) && can_cache(src2, src2w, dst, dstw)) {
			FAIL_IF(getput_arg(compiler, flags | LOAD_DATA, TMP_REG1, src1, src1w, src2, src2w));
			FAIL_IF(getput_arg(compiler, flags | LOAD_DATA | MEM_USE_TMP2, TMP_REG2, src2, src2w, dst, dstw));
		} else {
			FAIL_IF(getput_arg(compiler, flags | LOAD_DATA, TMP_REG2, src2, src2w, src1, src1w));
			FAIL_IF(getput_arg(compiler, flags | LOAD_DATA, TMP_REG1, src1, src1w, dst, dstw));
		}
	}
	else if (flags & SLOW_SRC1)
		FAIL_IF(getput_arg(compiler, flags | LOAD_DATA, TMP_REG1, src1, src1w, dst, dstw));
	else if (flags & SLOW_SRC2)
		FAIL_IF(getput_arg(compiler, flags | LOAD_DATA | ((src1_r == TMP_REG1) ? MEM_USE_TMP2 : 0), src2_tmp_reg, src2, src2w, dst, dstw));

	FAIL_IF(emit_single_op(compiler, op, flags, dst_r, src1_r, src2_r));

	if (dst & SLJIT2_MEM) {
		if (!(flags & SLOW_DEST)) {
			getput_arg_fast(compiler, flags, dst_r, dst, dstw);
			return compiler->error;
		}
		return getput_arg(compiler, flags, dst_r, dst, dstw, 0, 0);
	}

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op0(struct sljit2_compiler *compiler, sljit2_s32 op)
{
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	sljit2_ins word = (sljit2_ins)(op & SLJIT2_32) >> 5;

	SLJIT2_ASSERT(word == 0 || word == 0x8);
#endif /* SLJIT2_CONFIG_RISCV_64 */

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op0(compiler, op));

	switch (GET_OPCODE(op)) {
	case SLJIT2_BREAKPOINT:
		return push_inst(compiler, EBREAK);
	case SLJIT2_NOP:
		return push_inst(compiler, ADDI | RD(TMP_ZERO) | RS1(TMP_ZERO) | IMM_I(0));
	case SLJIT2_LMUL_UW:
		FAIL_IF(push_inst(compiler, ADDI | RD(TMP_REG1) | RS1(SLJIT2_R1) | IMM_I(0)));
		FAIL_IF(push_inst(compiler, MULHU | RD(SLJIT2_R1) | RS1(SLJIT2_R0) | RS2(SLJIT2_R1)));
		return push_inst(compiler, MUL | RD(SLJIT2_R0) | RS1(SLJIT2_R0) | RS2(TMP_REG1));
	case SLJIT2_LMUL_SW:
		FAIL_IF(push_inst(compiler, ADDI | RD(TMP_REG1) | RS1(SLJIT2_R1) | IMM_I(0)));
		FAIL_IF(push_inst(compiler, MULH | RD(SLJIT2_R1) | RS1(SLJIT2_R0) | RS2(SLJIT2_R1)));
		return push_inst(compiler, MUL | RD(SLJIT2_R0) | RS1(SLJIT2_R0) | RS2(TMP_REG1));
	case SLJIT2_DIVMOD_UW:
		FAIL_IF(push_inst(compiler, ADDI | RD(TMP_REG1) | RS1(SLJIT2_R0) | IMM_I(0)));
		FAIL_IF(push_inst(compiler, DIVU | WORD | RD(SLJIT2_R0) | RS1(SLJIT2_R0) | RS2(SLJIT2_R1)));
		return push_inst(compiler, REMU | WORD | RD(SLJIT2_R1) | RS1(TMP_REG1) | RS2(SLJIT2_R1));
	case SLJIT2_DIVMOD_SW:
		FAIL_IF(push_inst(compiler, ADDI | RD(TMP_REG1) | RS1(SLJIT2_R0) | IMM_I(0)));
		FAIL_IF(push_inst(compiler, DIV | WORD | RD(SLJIT2_R0) | RS1(SLJIT2_R0) | RS2(SLJIT2_R1)));
		return push_inst(compiler, REM | WORD | RD(SLJIT2_R1) | RS1(TMP_REG1) | RS2(SLJIT2_R1));
	case SLJIT2_DIV_UW:
		return push_inst(compiler, DIVU | WORD | RD(SLJIT2_R0) | RS1(SLJIT2_R0) | RS2(SLJIT2_R1));
	case SLJIT2_DIV_SW:
		return push_inst(compiler, DIV | WORD | RD(SLJIT2_R0) | RS1(SLJIT2_R0) | RS2(SLJIT2_R1));
	case SLJIT2_MEMORY_BARRIER:
		return push_inst(compiler, FENCE | 0x0ff00000);
	case SLJIT2_ENDBR:
	case SLJIT2_SKIP_FRAMES_BEFORE_RETURN:
		return SLJIT2_SUCCESS;
	}

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op1(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_s32 flags = 0;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op1(compiler, op, dst, dstw, src, srcw));
	ADJUST_LOCAL_OFFSET(dst, dstw);
	ADJUST_LOCAL_OFFSET(src, srcw);

#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	if (op & SLJIT2_32)
		flags = INT_DATA | SIGNED_DATA;
#endif

	switch (GET_OPCODE(op)) {
	case SLJIT2_MOV:
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	case SLJIT2_MOV_U32:
	case SLJIT2_MOV_S32:
	case SLJIT2_MOV32:
#endif
	case SLJIT2_MOV_P:
		return emit_op(compiler, SLJIT2_MOV, WORD_DATA | MOVE_OP, dst, dstw, TMP_ZERO, 0, src, srcw);

#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	case SLJIT2_MOV_U32:
		return emit_op(compiler, SLJIT2_MOV_U32, INT_DATA | MOVE_OP, dst, dstw, TMP_ZERO, 0, src, (src == SLJIT2_IMM) ? (sljit2_u32)srcw : srcw);

	case SLJIT2_MOV_S32:
	/* Logical operators have no W variant, so sign extended input is necessary for them. */
	case SLJIT2_MOV32:
		return emit_op(compiler, SLJIT2_MOV_S32, INT_DATA | SIGNED_DATA | MOVE_OP, dst, dstw, TMP_ZERO, 0, src, (src == SLJIT2_IMM) ? (sljit2_s32)srcw : srcw);
#endif

	case SLJIT2_MOV_U8:
		return emit_op(compiler, op, BYTE_DATA | MOVE_OP, dst, dstw, TMP_ZERO, 0, src, (src == SLJIT2_IMM) ? (sljit2_u8)srcw : srcw);

	case SLJIT2_MOV_S8:
		return emit_op(compiler, op, BYTE_DATA | SIGNED_DATA | MOVE_OP, dst, dstw, TMP_ZERO, 0, src, (src == SLJIT2_IMM) ? (sljit2_s8)srcw : srcw);

	case SLJIT2_MOV_U16:
		return emit_op(compiler, op, HALF_DATA | MOVE_OP, dst, dstw, TMP_ZERO, 0, src, (src == SLJIT2_IMM) ? (sljit2_u16)srcw : srcw);

	case SLJIT2_MOV_S16:
		return emit_op(compiler, op, HALF_DATA | SIGNED_DATA | MOVE_OP, dst, dstw, TMP_ZERO, 0, src, (src == SLJIT2_IMM) ? (sljit2_s16)srcw : srcw);

	case SLJIT2_CLZ:
	case SLJIT2_CTZ:
	case SLJIT2_REV:
		return emit_op(compiler, op, flags, dst, dstw, TMP_ZERO, 0, src, srcw);

	case SLJIT2_REV_U16:
	case SLJIT2_REV_S16:
		return emit_op(compiler, op, HALF_DATA, dst, dstw, TMP_ZERO, 0, src, srcw);

	case SLJIT2_REV_U32:
	case SLJIT2_REV_S32:
		return emit_op(compiler, op | SLJIT2_32, INT_DATA, dst, dstw, TMP_ZERO, 0, src, srcw);
	}

	SLJIT2_UNREACHABLE();
	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op2(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_s32 flags = 0;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op2(compiler, op, 0, dst, dstw, src1, src1w, src2, src2w));
	ADJUST_LOCAL_OFFSET(dst, dstw);
	ADJUST_LOCAL_OFFSET(src1, src1w);
	ADJUST_LOCAL_OFFSET(src2, src2w);

#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	if (op & SLJIT2_32) {
		flags |= INT_DATA | SIGNED_DATA;
		if (src1 == SLJIT2_IMM)
			src1w = (sljit2_s32)src1w;
		if (src2 == SLJIT2_IMM)
			src2w = (sljit2_s32)src2w;
	}
#endif

	switch (GET_OPCODE(op)) {
	case SLJIT2_ADD:
	case SLJIT2_ADDC:
		compiler->status_flags_state = SLJIT2_CURRENT_FLAGS_ADD;
		return emit_op(compiler, op, flags | CUMULATIVE_OP | IMM_OP, dst, dstw, src1, src1w, src2, src2w);

	case SLJIT2_SUB:
	case SLJIT2_SUBC:
		compiler->status_flags_state = SLJIT2_CURRENT_FLAGS_SUB;
		return emit_op(compiler, op, flags | IMM_OP, dst, dstw, src1, src1w, src2, src2w);

	case SLJIT2_MUL:
		compiler->status_flags_state = 0;
		return emit_op(compiler, op, flags | CUMULATIVE_OP, dst, dstw, src1, src1w, src2, src2w);

	case SLJIT2_AND:
	case SLJIT2_OR:
	case SLJIT2_XOR:
		return emit_op(compiler, op, flags | CUMULATIVE_OP | IMM_OP, dst, dstw, src1, src1w, src2, src2w);

	case SLJIT2_SHL:
	case SLJIT2_MSHL:
	case SLJIT2_LSHR:
	case SLJIT2_MLSHR:
	case SLJIT2_ASHR:
	case SLJIT2_MASHR:
	case SLJIT2_ROTL:
	case SLJIT2_ROTR:
		if (src2 == SLJIT2_IMM) {
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
			src2w &= 0x1f;
#else /* !SLJIT2_CONFIG_RISCV_32 */
			if (op & SLJIT2_32)
				src2w &= 0x1f;
			else
				src2w &= 0x3f;
#endif /* SLJIT2_CONFIG_RISCV_32 */
		}

		return emit_op(compiler, op, flags | IMM_OP, dst, dstw, src1, src1w, src2, src2w);
	}

	SLJIT2_UNREACHABLE();
	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op2u(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	CHECK_ERROR();
	CHECK(check_sljit2_emit_op2(compiler, op, 1, 0, 0, src1, src1w, src2, src2w));

	SLJIT2_SKIP_CHECKS(compiler);
	return sljit2_emit_op2(compiler, op, 0, 0, src1, src1w, src2, src2w);
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op2r(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst_reg,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	sljit2_ins word = (sljit2_ins)(op & SLJIT2_32) >> 5;
#endif /* SLJIT2_CONFIG_RISCV_64 */

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op2r(compiler, op, dst_reg, src1, src1w, src2, src2w));

	SLJIT2_ASSERT(WORD == 0 || WORD == 0x8);

	switch (GET_OPCODE(op)) {
	case SLJIT2_MULADD:
		SLJIT2_SKIP_CHECKS(compiler);
		FAIL_IF(sljit2_emit_op2(compiler, SLJIT2_MUL | (op & SLJIT2_32), TMP_REG2, 0, src1, src1w, src2, src2w));
		return push_inst(compiler, ADD | WORD | RD(dst_reg) | RS1(dst_reg) | RS2(TMP_REG2));
	}

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_shift_into(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst_reg,
	sljit2_s32 src1_reg,
	sljit2_s32 src2_reg,
	sljit2_s32 src3, sljit2_sw src3w)
{
	sljit2_s32 is_left;
	sljit2_ins ins1, ins2, ins3;
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	sljit2_ins word = (sljit2_ins)(op & SLJIT2_32) >> 5;
	sljit2_s32 inp_flags = ((op & SLJIT2_32) ? INT_DATA : WORD_DATA) | LOAD_DATA;
	sljit2_sw bit_length = (op & SLJIT2_32) ? 32 : 64;
#else /* !SLJIT2_CONFIG_RISCV_64 */
	sljit2_s32 inp_flags = WORD_DATA | LOAD_DATA;
	sljit2_sw bit_length = 32;
#endif /* SLJIT2_CONFIG_RISCV_64 */

	SLJIT2_ASSERT(WORD == 0 || WORD == 0x8);

	CHECK_ERROR();
	CHECK(check_sljit2_emit_shift_into(compiler, op, dst_reg, src1_reg, src2_reg, src3, src3w));

	is_left = (GET_OPCODE(op) == SLJIT2_SHL || GET_OPCODE(op) == SLJIT2_MSHL);

	if (src1_reg == src2_reg) {
		SLJIT2_SKIP_CHECKS(compiler);
		return sljit2_emit_op2(compiler, (is_left ? SLJIT2_ROTL : SLJIT2_ROTR) | (op & SLJIT2_32), dst_reg, 0, src1_reg, 0, src3, src3w);
	}

	ADJUST_LOCAL_OFFSET(src3, src3w);

	if (src3 == SLJIT2_IMM) {
		src3w &= bit_length - 1;

		if (src3w == 0)
			return SLJIT2_SUCCESS;

		if (is_left) {
			ins1 = SLLI | WORD | IMM_I(src3w);
			src3w = bit_length - src3w;
			ins2 = SRLI | WORD | IMM_I(src3w);
		} else {
			ins1 = SRLI | WORD | IMM_I(src3w);
			src3w = bit_length - src3w;
			ins2 = SLLI | WORD | IMM_I(src3w);
		}

		FAIL_IF(push_inst(compiler, ins1 | RD(dst_reg) | RS1(src1_reg)));
		FAIL_IF(push_inst(compiler, ins2 | RD(TMP_REG1) | RS1(src2_reg)));
		return push_inst(compiler, OR | RD(dst_reg) | RS1(dst_reg) | RS2(TMP_REG1));
	}

	if (src3 & SLJIT2_MEM) {
		FAIL_IF(emit_op_mem(compiler, inp_flags, TMP_REG2, src3, src3w));
		src3 = TMP_REG2;
	} else if (dst_reg == src3) {
		push_inst(compiler, ADDI | WORD | RD(TMP_REG2) | RS1(src3) | IMM_I(0));
		src3 = TMP_REG2;
	}

	if (is_left) {
		ins1 = SLL;
		ins2 = SRLI;
		ins3 = SRL;
	} else {
		ins1 = SRL;
		ins2 = SLLI;
		ins3 = SLL;
	}

	FAIL_IF(push_inst(compiler, ins1 | WORD | RD(dst_reg) | RS1(src1_reg) | RS2(src3)));

	if (!(op & SLJIT2_SHIFT_INTO_NON_ZERO)) {
		FAIL_IF(push_inst(compiler, ins2 | WORD | RD(TMP_REG1) | RS1(src2_reg) | IMM_I(1)));
		FAIL_IF(push_inst(compiler, XORI | RD(TMP_REG2) | RS1(src3) | IMM_I((sljit2_ins)bit_length - 1)));
		src2_reg = TMP_REG1;
	} else
		FAIL_IF(push_inst(compiler, SUB | WORD | RD(TMP_REG2) | RS1(TMP_ZERO) | RS2(src3)));

	FAIL_IF(push_inst(compiler, ins3 | WORD | RD(TMP_REG1) | RS1(src2_reg) | RS2(TMP_REG2)));
	return push_inst(compiler, OR | RD(dst_reg) | RS1(dst_reg) | RS2(TMP_REG1));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op_src(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 src, sljit2_sw srcw)
{
	CHECK_ERROR();
	CHECK(check_sljit2_emit_op_src(compiler, op, src, srcw));
	ADJUST_LOCAL_OFFSET(src, srcw);

	switch (op) {
	case SLJIT2_FAST_RETURN:
		if (FAST_IS_REG(src))
			FAIL_IF(push_inst(compiler, ADDI | RD(RETURN_ADDR_REG) | RS1(src) | IMM_I(0)));
		else
			FAIL_IF(emit_op_mem(compiler, WORD_DATA | LOAD_DATA, RETURN_ADDR_REG, src, srcw));

		return push_inst(compiler, JALR | RD(TMP_ZERO) | RS1(RETURN_ADDR_REG) | IMM_I(0));
	case SLJIT2_SKIP_FRAMES_BEFORE_FAST_RETURN:
		return SLJIT2_SUCCESS;
	case SLJIT2_PREFETCH_L1:
	case SLJIT2_PREFETCH_L2:
	case SLJIT2_PREFETCH_L3:
	case SLJIT2_PREFETCH_ONCE:
		return SLJIT2_SUCCESS;
	}

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op_dst(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw)
{
	sljit2_s32 dst_r;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op_dst(compiler, op, dst, dstw));
	ADJUST_LOCAL_OFFSET(dst, dstw);

	switch (op) {
	case SLJIT2_FAST_ENTER:
		if (FAST_IS_REG(dst))
			return push_inst(compiler, ADDI | RD(dst) | RS1(RETURN_ADDR_REG) | IMM_I(0));

		SLJIT2_ASSERT(RETURN_ADDR_REG == TMP_REG2);
		break;
	case SLJIT2_GET_RETURN_ADDRESS:
		dst_r = FAST_IS_REG(dst) ? dst : TMP_REG2;
		FAIL_IF(emit_op_mem(compiler, WORD_DATA | LOAD_DATA, dst_r, SLJIT2_MEM1(SLJIT2_SP), compiler->local_size - SSIZE_OF(sw)));
		break;
	}

	if (dst & SLJIT2_MEM)
		return emit_op_mem(compiler, WORD_DATA, TMP_REG2, dst, dstw);

	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_get_register_index(sljit2_s32 type, sljit2_s32 reg)
{
	CHECK_REG_INDEX(check_sljit2_get_register_index(type, reg));

	if (type == SLJIT2_GP_REGISTER)
		return reg_map[reg];

	if (type != SLJIT2_FLOAT_REGISTER)
		return -1;

	return freg_map[reg];
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op_custom(struct sljit2_compiler *compiler,
	void *instruction, sljit2_u32 size)
{
	SLJIT2_UNUSED_ARG(size);

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op_custom(compiler, instruction, size));

	return push_inst(compiler, *(sljit2_ins*)instruction);
}

/* --------------------------------------------------------------------- */
/*  Floating point operators                                             */
/* --------------------------------------------------------------------- */

#define FLOAT_DATA(op) (DOUBLE_DATA | ((op & SLJIT2_32) >> 7))
#define FMT(op) ((sljit2_ins)((op & SLJIT2_32) ^ SLJIT2_32) << 17)

static SLJIT2_INLINE sljit2_s32 sljit2_emit_fop1_conv_sw_from_f64(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
#	define flags (sljit2_u32)0
#else
	sljit2_u32 flags = ((sljit2_u32)(GET_OPCODE(op) == SLJIT2_CONV_SW_FROM_F64)) << 21;
#endif
	sljit2_s32 dst_r = FAST_IS_REG(dst) ? dst : TMP_REG2;

	if (src & SLJIT2_MEM) {
		FAIL_IF(emit_op_mem2(compiler, FLOAT_DATA(op) | LOAD_DATA, TMP_FREG1, src, srcw, dst, dstw));
		src = TMP_FREG1;
	}

	FAIL_IF(push_inst(compiler, FCVT_W_S | FMT(op) | flags | RD(dst_r) | FRS1(src)));

	/* Store the integer value from a VFP register. */
	if (dst & SLJIT2_MEM) {
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
		return emit_op_mem2(compiler, WORD_DATA, TMP_REG2, dst, dstw, 0, 0);
#else
		return emit_op_mem2(compiler, flags ? WORD_DATA : INT_DATA, TMP_REG2, dst, dstw, 0, 0);
#endif
	}
	return SLJIT2_SUCCESS;

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
#	undef flags
#endif
}

static sljit2_s32 sljit2_emit_fop1_conv_f64_from_w(struct sljit2_compiler *compiler, sljit2_ins ins,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_s32 dst_r = FAST_IS_REG(dst) ? dst : TMP_FREG1;

	if (src & SLJIT2_MEM) {
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
		FAIL_IF(emit_op_mem2(compiler, WORD_DATA | LOAD_DATA, TMP_REG1, src, srcw, dst, dstw));
#else /* SLJIT2_CONFIG_RISCV_32 */
		FAIL_IF(emit_op_mem2(compiler, ((ins & (1 << 21)) ? WORD_DATA : INT_DATA) | LOAD_DATA, TMP_REG1, src, srcw, dst, dstw));
#endif /* !SLJIT2_CONFIG_RISCV_32 */
		src = TMP_REG1;
	} else if (src == SLJIT2_IMM) {
		FAIL_IF(load_immediate(compiler, TMP_REG1, srcw, TMP_REG3));
		src = TMP_REG1;
	}

	FAIL_IF(push_inst(compiler, ins | FRD(dst_r) | RS1(src)));

	if (dst & SLJIT2_MEM)
		return emit_op_mem2(compiler, DOUBLE_DATA | ((sljit2_s32)(~ins >> 24) & 0x2), TMP_FREG1, dst, dstw, 0, 0);
	return SLJIT2_SUCCESS;
}

static SLJIT2_INLINE sljit2_s32 sljit2_emit_fop1_conv_f64_from_sw(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_ins ins = FCVT_S_W | FMT(op);

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	if (op & SLJIT2_32)
		ins |= F3(0x7);
#else /* !SLJIT2_CONFIG_RISCV_32 */
	if (GET_OPCODE(op) == SLJIT2_CONV_F64_FROM_SW)
		ins |= (1 << 21);
	else if (src == SLJIT2_IMM)
		srcw = (sljit2_s32)srcw;

	if (op != SLJIT2_CONV_F64_FROM_S32)
		ins |= F3(0x7);
#endif /* SLJIT2_CONFIG_RISCV_32 */

	return sljit2_emit_fop1_conv_f64_from_w(compiler, ins, dst, dstw, src, srcw);
}

static SLJIT2_INLINE sljit2_s32 sljit2_emit_fop1_conv_f64_from_uw(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_ins ins = FCVT_S_WU | FMT(op);

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	if (op & SLJIT2_32)
		ins |= F3(0x7);
#else /* !SLJIT2_CONFIG_RISCV_32 */
	if (GET_OPCODE(op) == SLJIT2_CONV_F64_FROM_UW)
		ins |= (1 << 21);
	else if (src == SLJIT2_IMM)
		srcw = (sljit2_u32)srcw;

	if (op != SLJIT2_CONV_F64_FROM_S32)
		ins |= F3(0x7);
#endif /* SLJIT2_CONFIG_RISCV_32 */

	return sljit2_emit_fop1_conv_f64_from_w(compiler, ins, dst, dstw, src, srcw);
}

static SLJIT2_INLINE sljit2_s32 sljit2_emit_fop1_cmp(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_ins inst;

	if (src1 & SLJIT2_MEM) {
		FAIL_IF(emit_op_mem2(compiler, FLOAT_DATA(op) | LOAD_DATA, TMP_FREG1, src1, src1w, src2, src2w));
		src1 = TMP_FREG1;
	}

	if (src2 & SLJIT2_MEM) {
		FAIL_IF(emit_op_mem2(compiler, FLOAT_DATA(op) | LOAD_DATA, TMP_FREG2, src2, src2w, 0, 0));
		src2 = TMP_FREG2;
	}

	switch (GET_FLAG_TYPE(op)) {
	case SLJIT2_F_EQUAL:
	case SLJIT2_ORDERED_EQUAL:
		inst = FEQ_S | FMT(op) | RD(OTHER_FLAG) | FRS1(src1) | FRS2(src2);
		break;
	case SLJIT2_F_LESS:
	case SLJIT2_ORDERED_LESS:
		inst = FLT_S | FMT(op) | RD(OTHER_FLAG) | FRS1(src1) | FRS2(src2);
		break;
	case SLJIT2_ORDERED_GREATER:
		inst = FLT_S | FMT(op) | RD(OTHER_FLAG) | FRS1(src2) | FRS2(src1);
		break;
	case SLJIT2_F_GREATER:
	case SLJIT2_UNORDERED_OR_GREATER:
		inst = FLE_S | FMT(op) | RD(OTHER_FLAG) | FRS1(src1) | FRS2(src2);
		break;
	case SLJIT2_UNORDERED_OR_LESS:
		inst = FLE_S | FMT(op) | RD(OTHER_FLAG) | FRS1(src2) | FRS2(src1);
		break;
	case SLJIT2_UNORDERED_OR_EQUAL:
		FAIL_IF(push_inst(compiler, FLT_S | FMT(op) | RD(OTHER_FLAG) | FRS1(src1) | FRS2(src2)));
		FAIL_IF(push_inst(compiler, FLT_S | FMT(op) | RD(TMP_REG1) | FRS1(src2) | FRS2(src1)));
		inst = OR | RD(OTHER_FLAG) | RS1(OTHER_FLAG) | RS2(TMP_REG1);
		break;
	default: /* SLJIT2_UNORDERED */
		if (src1 == src2) {
			inst = FEQ_S | FMT(op) | RD(OTHER_FLAG) | FRS1(src1) | FRS2(src1);
			break;
		}
		FAIL_IF(push_inst(compiler, FEQ_S | FMT(op) | RD(OTHER_FLAG) | FRS1(src1) | FRS2(src1)));
		FAIL_IF(push_inst(compiler, FEQ_S | FMT(op) | RD(TMP_REG1) | FRS1(src2) | FRS2(src2)));
		inst = AND | RD(OTHER_FLAG) | RS1(OTHER_FLAG) | RS2(TMP_REG1);
		break;
	}

	return push_inst(compiler, inst);
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_fop1(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_s32 dst_r;

	CHECK_ERROR();
	compiler->cache_arg = 0;
	compiler->cache_argw = 0;

	SLJIT2_COMPILE_ASSERT((SLJIT2_32 == 0x100) && !(DOUBLE_DATA & 0x2), float_transfer_bit_error);
	SELECT_FOP1_OPERATION_WITH_CHECKS(compiler, op, dst, dstw, src, srcw);

	if (GET_OPCODE(op) == SLJIT2_CONV_F64_FROM_F32)
		op ^= SLJIT2_32;

	dst_r = FAST_IS_REG(dst) ? dst : TMP_FREG1;

	if (src & SLJIT2_MEM) {
		FAIL_IF(emit_op_mem2(compiler, FLOAT_DATA(op) | LOAD_DATA, dst_r, src, srcw, dst, dstw));
		src = dst_r;
	}

	switch (GET_OPCODE(op)) {
	case SLJIT2_MOV_F64:
		if (src != dst_r) {
			if (!(dst & SLJIT2_MEM))
				FAIL_IF(push_inst(compiler, FSGNJ_S | FMT(op) | FRD(dst_r) | FRS1(src) | FRS2(src)));
			else
				dst_r = src;
		}
		break;
	case SLJIT2_NEG_F64:
		FAIL_IF(push_inst(compiler, FSGNJN_S | FMT(op) | FRD(dst_r) | FRS1(src) | FRS2(src)));
		break;
	case SLJIT2_ABS_F64:
		FAIL_IF(push_inst(compiler, FSGNJX_S | FMT(op) | FRD(dst_r) | FRS1(src) | FRS2(src)));
		break;
	case SLJIT2_CONV_F64_FROM_F32:
		/* The SLJIT2_32 bit is inverted because sljit2_f32 needs to be loaded from the memory. */
		FAIL_IF(push_inst(compiler, FCVT_S_D | ((op & SLJIT2_32) ? (1 << 25) : ((1 << 20) | F3(7))) | FRD(dst_r) | FRS1(src)));
		op ^= SLJIT2_32;
		break;
	}

	if (dst & SLJIT2_MEM)
		return emit_op_mem2(compiler, FLOAT_DATA(op), dst_r, dst, dstw, 0, 0);
	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_fop2(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_s32 dst_r, flags = 0;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_fop2(compiler, op, dst, dstw, src1, src1w, src2, src2w));
	ADJUST_LOCAL_OFFSET(dst, dstw);
	ADJUST_LOCAL_OFFSET(src1, src1w);
	ADJUST_LOCAL_OFFSET(src2, src2w);

	compiler->cache_arg = 0;
	compiler->cache_argw = 0;

	dst_r = FAST_IS_REG(dst) ? dst : TMP_FREG2;

	if (src1 & SLJIT2_MEM) {
		if (getput_arg_fast(compiler, FLOAT_DATA(op) | LOAD_DATA, TMP_FREG1, src1, src1w)) {
			FAIL_IF(compiler->error);
			src1 = TMP_FREG1;
		} else
			flags |= SLOW_SRC1;
	}

	if (src2 & SLJIT2_MEM) {
		if (getput_arg_fast(compiler, FLOAT_DATA(op) | LOAD_DATA, TMP_FREG2, src2, src2w)) {
			FAIL_IF(compiler->error);
			src2 = TMP_FREG2;
		} else
			flags |= SLOW_SRC2;
	}

	if ((flags & (SLOW_SRC1 | SLOW_SRC2)) == (SLOW_SRC1 | SLOW_SRC2)) {
		if ((dst & SLJIT2_MEM) && !can_cache(src1, src1w, src2, src2w) && can_cache(src1, src1w, dst, dstw)) {
			FAIL_IF(getput_arg(compiler, FLOAT_DATA(op) | LOAD_DATA, TMP_FREG2, src2, src2w, src1, src1w));
			FAIL_IF(getput_arg(compiler, FLOAT_DATA(op) | LOAD_DATA, TMP_FREG1, src1, src1w, dst, dstw));
		} else {
			FAIL_IF(getput_arg(compiler, FLOAT_DATA(op) | LOAD_DATA, TMP_FREG1, src1, src1w, src2, src2w));
			FAIL_IF(getput_arg(compiler, FLOAT_DATA(op) | LOAD_DATA, TMP_FREG2, src2, src2w, dst, dstw));
		}
	}
	else if (flags & SLOW_SRC1)
		FAIL_IF(getput_arg(compiler, FLOAT_DATA(op) | LOAD_DATA, TMP_FREG1, src1, src1w, dst, dstw));
	else if (flags & SLOW_SRC2)
		FAIL_IF(getput_arg(compiler, FLOAT_DATA(op) | LOAD_DATA, TMP_FREG2, src2, src2w, dst, dstw));

	if (flags & SLOW_SRC1)
		src1 = TMP_FREG1;
	if (flags & SLOW_SRC2)
		src2 = TMP_FREG2;

	switch (GET_OPCODE(op)) {
	case SLJIT2_ADD_F64:
		FAIL_IF(push_inst(compiler, FADD_S | FMT(op) | FRD(dst_r) | FRS1(src1) | FRS2(src2)));
		break;

	case SLJIT2_SUB_F64:
		FAIL_IF(push_inst(compiler, FSUB_S | FMT(op) | FRD(dst_r) | FRS1(src1) | FRS2(src2)));
		break;

	case SLJIT2_MUL_F64:
		FAIL_IF(push_inst(compiler, FMUL_S | FMT(op) | FRD(dst_r) | FRS1(src1) | FRS2(src2)));
		break;

	case SLJIT2_DIV_F64:
		FAIL_IF(push_inst(compiler, FDIV_S | FMT(op) | FRD(dst_r) | FRS1(src1) | FRS2(src2)));
		break;

	case SLJIT2_COPYSIGN_F64:
		return push_inst(compiler, FSGNJ_S | FMT(op) | FRD(dst_r) | FRS1(src1) | FRS2(src2));
	}

	if (dst_r != dst)
		FAIL_IF(emit_op_mem2(compiler, FLOAT_DATA(op), TMP_FREG2, dst, dstw, 0, 0));

	return SLJIT2_SUCCESS;
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

	if (u.imm == 0)
		return push_inst(compiler, FMV_W_X | RS1(TMP_ZERO) | FRD(freg));

	FAIL_IF(load_immediate(compiler, TMP_REG1, u.imm, TMP_REG3));
	return push_inst(compiler, FMV_W_X | RS1(TMP_REG1) | FRD(freg));
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

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
#define BRANCH_LENGTH	((sljit2_ins)(3 * sizeof(sljit2_ins)) << 7)
#else
#define BRANCH_LENGTH	((sljit2_ins)(7 * sizeof(sljit2_ins)) << 7)
#endif

static sljit2_ins get_jump_instruction(sljit2_s32 type)
{
	switch (type) {
	case SLJIT2_EQUAL:
		return BNE | RS1(EQUAL_FLAG) | RS2(TMP_ZERO);
	case SLJIT2_NOT_EQUAL:
		return BEQ | RS1(EQUAL_FLAG) | RS2(TMP_ZERO);
	case SLJIT2_LESS:
	case SLJIT2_GREATER:
	case SLJIT2_SIG_LESS:
	case SLJIT2_SIG_GREATER:
	case SLJIT2_OVERFLOW:
	case SLJIT2_CARRY:
	case SLJIT2_ATOMIC_NOT_STORED:
	case SLJIT2_F_EQUAL:
	case SLJIT2_ORDERED_EQUAL:
	case SLJIT2_ORDERED_NOT_EQUAL:
	case SLJIT2_F_LESS:
	case SLJIT2_ORDERED_LESS:
	case SLJIT2_ORDERED_GREATER:
	case SLJIT2_F_LESS_EQUAL:
	case SLJIT2_ORDERED_LESS_EQUAL:
	case SLJIT2_ORDERED_GREATER_EQUAL:
	case SLJIT2_ORDERED:
		return BEQ | RS1(OTHER_FLAG) | RS2(TMP_ZERO);
		break;
	case SLJIT2_GREATER_EQUAL:
	case SLJIT2_LESS_EQUAL:
	case SLJIT2_SIG_GREATER_EQUAL:
	case SLJIT2_SIG_LESS_EQUAL:
	case SLJIT2_NOT_OVERFLOW:
	case SLJIT2_NOT_CARRY:
	case SLJIT2_ATOMIC_STORED:
	case SLJIT2_F_NOT_EQUAL:
	case SLJIT2_UNORDERED_OR_NOT_EQUAL:
	case SLJIT2_UNORDERED_OR_EQUAL:
	case SLJIT2_F_GREATER_EQUAL:
	case SLJIT2_UNORDERED_OR_GREATER_EQUAL:
	case SLJIT2_UNORDERED_OR_LESS_EQUAL:
	case SLJIT2_F_GREATER:
	case SLJIT2_UNORDERED_OR_GREATER:
	case SLJIT2_UNORDERED_OR_LESS:
	case SLJIT2_UNORDERED:
		return BNE | RS1(OTHER_FLAG) | RS2(TMP_ZERO);
	default:
		/* Not conditional branch. */
		return 0;
	}
}

SLJIT2_API_FUNC_ATTRIBUTE struct sljit2_jump* sljit2_emit_jump(struct sljit2_compiler *compiler, sljit2_s32 type)
{
	struct sljit2_jump *jump;
	sljit2_ins inst;

	CHECK_ERROR_PTR();
	CHECK_PTR(check_sljit2_emit_jump(compiler, type));

	jump = (struct sljit2_jump*)ensure_abuf(compiler, sizeof(struct sljit2_jump));
	PTR_FAIL_IF(!jump);
	set_jump(jump, compiler, type & SLJIT2_REWRITABLE_JUMP);
	type &= 0xff;

	inst = get_jump_instruction(type);

	if (inst != 0) {
		PTR_FAIL_IF(push_inst(compiler, inst | BRANCH_LENGTH));
		jump->flags |= IS_COND;
	}

	jump->addr = compiler->size;
	inst = JALR | RS1(TMP_REG1) | IMM_I(0);

	if (type >= SLJIT2_FAST_CALL) {
		jump->flags |= IS_CALL;
		inst |= RD(RETURN_ADDR_REG);
	}

	PTR_FAIL_IF(push_inst(compiler, inst));

	/* Maximum number of instructions required for generating a constant. */
	compiler->size += JUMP_MAX_SIZE - 1;
	return jump;
}

SLJIT2_API_FUNC_ATTRIBUTE struct sljit2_jump* sljit2_emit_call(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 arg_types)
{
	SLJIT2_UNUSED_ARG(arg_types);
	CHECK_ERROR_PTR();
	CHECK_PTR(check_sljit2_emit_call(compiler, type, arg_types));

	if (type & SLJIT2_CALL_RETURN) {
		PTR_FAIL_IF(emit_stack_frame_release(compiler, 0));
		type = SLJIT2_JUMP | (type & SLJIT2_REWRITABLE_JUMP);
	}

	SLJIT2_SKIP_CHECKS(compiler);
	return sljit2_emit_jump(compiler, type);
}

SLJIT2_API_FUNC_ATTRIBUTE struct sljit2_jump* sljit2_emit_cmp(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2, sljit2_sw src2w)
{
	struct sljit2_jump *jump;
	sljit2_s32 flags;
	sljit2_ins inst;
	sljit2_s32 src2_tmp_reg = FAST_IS_REG(src1) ? TMP_REG1 : TMP_REG2;

	CHECK_ERROR_PTR();
	CHECK_PTR(check_sljit2_emit_cmp(compiler, type, src1, src1w, src2, src2w));
	ADJUST_LOCAL_OFFSET(src1, src1w);
	ADJUST_LOCAL_OFFSET(src2, src2w);

	compiler->cache_arg = 0;
	compiler->cache_argw = 0;
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	flags = WORD_DATA | LOAD_DATA;
#else /* !SLJIT2_CONFIG_RISCV_32 */
	flags = ((type & SLJIT2_32) ? INT_DATA : WORD_DATA) | LOAD_DATA;
#endif /* SLJIT2_CONFIG_RISCV_32 */

	if (src1 & SLJIT2_MEM) {
		PTR_FAIL_IF(emit_op_mem2(compiler, flags, TMP_REG1, src1, src1w, src2, src2w));
		src1 = TMP_REG1;
	}

	if (src2 & SLJIT2_MEM) {
		PTR_FAIL_IF(emit_op_mem2(compiler, flags | (src1 == TMP_REG1 ? MEM_USE_TMP2 : 0), src2_tmp_reg, src2, src2w, 0, 0));
		src2 = src2_tmp_reg;
	}

	if (src1 == SLJIT2_IMM) {
		if (src1w != 0) {
			PTR_FAIL_IF(load_immediate(compiler, TMP_REG1, src1w, TMP_REG3));
			src1 = TMP_REG1;
		}
		else
			src1 = TMP_ZERO;
	}

	if (src2 == SLJIT2_IMM) {
		if (src2w != 0) {
			PTR_FAIL_IF(load_immediate(compiler, src2_tmp_reg, src2w, TMP_REG3));
			src2 = src2_tmp_reg;
		}
		else
			src2 = TMP_ZERO;
	}

	jump = (struct sljit2_jump*)ensure_abuf(compiler, sizeof(struct sljit2_jump));
	PTR_FAIL_IF(!jump);
	set_jump(jump, compiler, (sljit2_u32)((type & SLJIT2_REWRITABLE_JUMP) | IS_COND));
	type &= 0xff;

	switch (type) {
	case SLJIT2_EQUAL:
		inst = BNE | RS1(src1) | RS2(src2) | BRANCH_LENGTH;
		break;
	case SLJIT2_NOT_EQUAL:
		inst = BEQ | RS1(src1) | RS2(src2) | BRANCH_LENGTH;
		break;
	case SLJIT2_LESS:
		inst = BGEU | RS1(src1) | RS2(src2) | BRANCH_LENGTH;
		break;
	case SLJIT2_GREATER_EQUAL:
		inst = BLTU | RS1(src1) | RS2(src2) | BRANCH_LENGTH;
		break;
	case SLJIT2_GREATER:
		inst = BGEU | RS1(src2) | RS2(src1) | BRANCH_LENGTH;
		break;
	case SLJIT2_LESS_EQUAL:
		inst = BLTU | RS1(src2) | RS2(src1) | BRANCH_LENGTH;
		break;
	case SLJIT2_SIG_LESS:
		inst = BGE | RS1(src1) | RS2(src2) | BRANCH_LENGTH;
		break;
	case SLJIT2_SIG_GREATER_EQUAL:
		inst = BLT | RS1(src1) | RS2(src2) | BRANCH_LENGTH;
		break;
	case SLJIT2_SIG_GREATER:
		inst = BGE | RS1(src2) | RS2(src1) | BRANCH_LENGTH;
		break;
	case SLJIT2_SIG_LESS_EQUAL:
		inst = BLT | RS1(src2) | RS2(src1) | BRANCH_LENGTH;
		break;
	}

	PTR_FAIL_IF(push_inst(compiler, inst));

	jump->addr = compiler->size;
	PTR_FAIL_IF(push_inst(compiler, JALR | RD(TMP_ZERO) | RS1(TMP_REG1) | IMM_I(0)));

	/* Maximum number of instructions required for generating a constant. */
	compiler->size += JUMP_MAX_SIZE - 1;
	return jump;
}

#undef BRANCH_LENGTH

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_ijump(struct sljit2_compiler *compiler, sljit2_s32 type, sljit2_s32 src, sljit2_sw srcw)
{
	struct sljit2_jump *jump;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_ijump(compiler, type, src, srcw));

	if (src != SLJIT2_IMM) {
		if (src & SLJIT2_MEM) {
			ADJUST_LOCAL_OFFSET(src, srcw);
			FAIL_IF(emit_op_mem(compiler, WORD_DATA | LOAD_DATA, TMP_REG1, src, srcw));
			src = TMP_REG1;
		}
		return push_inst(compiler, JALR | RD((type >= SLJIT2_FAST_CALL) ? RETURN_ADDR_REG : TMP_ZERO) | RS1(src) | IMM_I(0));
	}

	/* These jumps are converted to jump/call instructions when possible. */
	jump = (struct sljit2_jump*)ensure_abuf(compiler, sizeof(struct sljit2_jump));
	FAIL_IF(!jump);
	set_jump(jump, compiler, JUMP_ADDR | ((type >= SLJIT2_FAST_CALL) ? IS_CALL : 0));
	jump->u.target = (sljit2_uw)srcw;

	jump->addr = compiler->size;
	FAIL_IF(push_inst(compiler, JALR | RD((type >= SLJIT2_FAST_CALL) ? RETURN_ADDR_REG : TMP_ZERO) | RS1(TMP_REG1) | IMM_I(0)));

	/* Maximum number of instructions required for generating a constant. */
	compiler->size += JUMP_MAX_SIZE - 1;
	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_icall(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 arg_types,
	sljit2_s32 src, sljit2_sw srcw)
{
	SLJIT2_UNUSED_ARG(arg_types);
	CHECK_ERROR();
	CHECK(check_sljit2_emit_icall(compiler, type, arg_types, src, srcw));

	if (src & SLJIT2_MEM) {
		ADJUST_LOCAL_OFFSET(src, srcw);
		FAIL_IF(emit_op_mem(compiler, WORD_DATA | LOAD_DATA, TMP_REG1, src, srcw));
		src = TMP_REG1;
	}

	if (type & SLJIT2_CALL_RETURN) {
		if (src >= SLJIT2_FIRST_SAVED_REG && src <= (SLJIT2_S0 - SLJIT2_KEPT_SAVEDS_COUNT(compiler->options))) {
			FAIL_IF(push_inst(compiler, ADDI | RD(TMP_REG1) | RS1(src) | IMM_I(0)));
			src = TMP_REG1;
		}

		FAIL_IF(emit_stack_frame_release(compiler, 0));
		type = SLJIT2_JUMP;
	}

	SLJIT2_SKIP_CHECKS(compiler);
	return sljit2_emit_ijump(compiler, type, src, srcw);
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_op_flags(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 type)
{
	sljit2_s32 src_r, dst_r, invert;
	sljit2_s32 saved_op = op;
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	sljit2_s32 mem_type = WORD_DATA;
#else
	sljit2_s32 mem_type = ((op & SLJIT2_32) || op == SLJIT2_MOV32) ? (INT_DATA | SIGNED_DATA) : WORD_DATA;
#endif

	CHECK_ERROR();
	CHECK(check_sljit2_emit_op_flags(compiler, op, dst, dstw, type));
	ADJUST_LOCAL_OFFSET(dst, dstw);

	op = GET_OPCODE(op);
	dst_r = (op < SLJIT2_ADD && FAST_IS_REG(dst)) ? dst : TMP_REG2;

	compiler->cache_arg = 0;
	compiler->cache_argw = 0;

	if (op >= SLJIT2_ADD && (dst & SLJIT2_MEM))
		FAIL_IF(emit_op_mem2(compiler, mem_type | LOAD_DATA, TMP_REG1, dst, dstw, dst, dstw));

	if (type < SLJIT2_F_EQUAL) {
		src_r = OTHER_FLAG;
		invert = type & 0x1;

		switch (type) {
		case SLJIT2_EQUAL:
		case SLJIT2_NOT_EQUAL:
			FAIL_IF(push_inst(compiler, SLTUI | RD(dst_r) | RS1(EQUAL_FLAG) | IMM_I(1)));
			src_r = dst_r;
			break;
		case SLJIT2_OVERFLOW:
		case SLJIT2_NOT_OVERFLOW:
			if (compiler->status_flags_state & (SLJIT2_CURRENT_FLAGS_ADD | SLJIT2_CURRENT_FLAGS_SUB)) {
				src_r = OTHER_FLAG;
				break;
			}
			FAIL_IF(push_inst(compiler, SLTUI | RD(dst_r) | RS1(OTHER_FLAG) | IMM_I(1)));
			src_r = dst_r;
			invert ^= 0x1;
			break;
		case SLJIT2_ATOMIC_STORED:
		case SLJIT2_ATOMIC_NOT_STORED:
			invert ^= 0x1;
			break;
		}
	} else {
		invert = 0;
		src_r = OTHER_FLAG;

		switch (type) {
		case SLJIT2_F_NOT_EQUAL:
		case SLJIT2_UNORDERED_OR_NOT_EQUAL:
		case SLJIT2_UNORDERED_OR_EQUAL: /* Not supported. */
		case SLJIT2_F_GREATER_EQUAL:
		case SLJIT2_UNORDERED_OR_GREATER_EQUAL:
		case SLJIT2_UNORDERED_OR_LESS_EQUAL:
		case SLJIT2_F_GREATER:
		case SLJIT2_UNORDERED_OR_GREATER:
		case SLJIT2_UNORDERED_OR_LESS:
		case SLJIT2_UNORDERED:
			invert = 1;
			break;
		}
	}

	if (invert) {
		FAIL_IF(push_inst(compiler, XORI | RD(dst_r) | RS1(src_r) | IMM_I(1)));
		src_r = dst_r;
	}

	if (op < SLJIT2_ADD) {
		if (dst & SLJIT2_MEM)
			return emit_op_mem(compiler, mem_type, src_r, dst, dstw);

		if (src_r != dst_r)
			return push_inst(compiler, ADDI | RD(dst_r) | RS1(src_r) | IMM_I(0));
		return SLJIT2_SUCCESS;
	}

	mem_type |= CUMULATIVE_OP | IMM_OP | ALT_KEEP_CACHE;

	if (dst & SLJIT2_MEM)
		return emit_op(compiler, saved_op, mem_type, dst, dstw, TMP_REG1, 0, src_r, 0);
	return emit_op(compiler, saved_op, mem_type, dst, dstw, dst, dstw, src_r, 0);
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_select(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 dst_reg,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2_reg)
{
	sljit2_ins *ptr;
	sljit2_uw size;
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	sljit2_ins word = (sljit2_ins)(type & SLJIT2_32) >> 5;
	sljit2_s32 inp_flags = ((type & SLJIT2_32) ? INT_DATA : WORD_DATA) | LOAD_DATA;
#else /* !SLJIT2_CONFIG_RISCV_64 */
        sljit2_s32 inp_flags = WORD_DATA | LOAD_DATA;
#endif /* SLJIT2_CONFIG_RISCV_64 */

	SLJIT2_ASSERT(WORD == 0 || WORD == 0x8);

	CHECK_ERROR();
	CHECK(check_sljit2_emit_select(compiler, type, dst_reg, src1, src1w, src2_reg));

	ADJUST_LOCAL_OFFSET(src1, src1w);

	if (dst_reg != src2_reg) {
		if (dst_reg == src1) {
			src1 = src2_reg;
			src1w = 0;
			type ^= 0x1;
		} else {
			if (ADDRESSING_DEPENDS_ON(src1, dst_reg)) {
				FAIL_IF(push_inst(compiler, ADDI | RD(TMP_REG1) | RS1(dst_reg) | IMM_I(0)));

				if ((src1 & REG_MASK) == dst_reg)
					src1 = (src1 & ~REG_MASK) | TMP_REG1;

				if (OFFS_REG(src1) == dst_reg)
					src1 = (src1 & ~OFFS_REG_MASK) | TO_OFFS_REG(TMP_REG1);
			}

			FAIL_IF(push_inst(compiler, ADDI | WORD | RD(dst_reg) | RS1(src2_reg) | IMM_I(0)));
		}
	}

	size = compiler->size;

	ptr = (sljit2_ins*)ensure_buf(compiler, sizeof(sljit2_ins));
	FAIL_IF(!ptr);
	compiler->size++;

	if (src1 & SLJIT2_MEM) {
		FAIL_IF(emit_op_mem(compiler, inp_flags, dst_reg, src1, src1w));
	} else if (src1 == SLJIT2_IMM) {
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
		if (word)
			src1w = (sljit2_s32)src1w;
#endif /* SLJIT2_CONFIG_RISCV_64 */
		FAIL_IF(load_immediate(compiler, dst_reg, src1w, TMP_REG1));
	} else
		FAIL_IF(push_inst(compiler, ADDI | WORD | RD(dst_reg) | RS1(src1) | IMM_I(0)));

	size = compiler->size - size;
	*ptr = get_jump_instruction(type & ~SLJIT2_32) | (sljit2_ins)((size & 0x7) << 9) | (sljit2_ins)((size >> 3) << 25);
	return SLJIT2_SUCCESS;
}

#undef WORD

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_fselect(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 dst_freg,
	sljit2_s32 src1, sljit2_sw src1w,
	sljit2_s32 src2_freg)
{
	sljit2_ins *ptr;
	sljit2_uw size;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_fselect(compiler, type, dst_freg, src1, src1w, src2_freg));

	ADJUST_LOCAL_OFFSET(src1, src1w);

	if (dst_freg != src2_freg) {
		if (dst_freg == src1) {
			src1 = src2_freg;
			src1w = 0;
			type ^= 0x1;
		} else
			FAIL_IF(push_inst(compiler, FSGNJ_S | FMT(type) | FRD(dst_freg) | FRS1(src2_freg) | FRS2(src2_freg)));
	}

	size = compiler->size;

	ptr = (sljit2_ins*)ensure_buf(compiler, sizeof(sljit2_ins));
	FAIL_IF(!ptr);
	compiler->size++;

	if (src1 & SLJIT2_MEM)
		FAIL_IF(emit_op_mem(compiler, FLOAT_DATA(type) | LOAD_DATA, dst_freg, src1, src1w));
	else
		FAIL_IF(push_inst(compiler, FSGNJ_S | FMT(type) | FRD(dst_freg) | FRS1(src1) | FRS2(src1)));

	size = compiler->size - size;
	*ptr = get_jump_instruction(type & ~SLJIT2_32) | (sljit2_ins)((size & 0x7) << 9) | (sljit2_ins)((size >> 3) << 25);
	return SLJIT2_SUCCESS;
}

#undef FLOAT_DATA
#undef FMT

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_mem(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 reg,
	sljit2_s32 mem, sljit2_sw memw)
{
	sljit2_s32 flags;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_mem(compiler, type, reg, mem, memw));

	if (!(reg & REG_PAIR_MASK))
		return sljit2_emit_mem_unaligned(compiler, type, reg, mem, memw);

	if (SLJIT2_UNLIKELY(mem & OFFS_REG_MASK)) {
		memw &= 0x3;

		if (SLJIT2_UNLIKELY(memw != 0)) {
			FAIL_IF(push_inst(compiler, SLLI | RD(TMP_REG1) | RS1(OFFS_REG(mem)) | IMM_I(memw)));
			FAIL_IF(push_inst(compiler, ADD | RD(TMP_REG1) | RS1(TMP_REG1) | RS2(mem & REG_MASK)));
		} else
			FAIL_IF(push_inst(compiler, ADD | RD(TMP_REG1) | RS1(mem & REG_MASK) | RS2(OFFS_REG(mem))));

		mem = TMP_REG1;
		memw = 0;
	} else if (memw > SIMM_MAX - SSIZE_OF(sw) || memw < SIMM_MIN) {
		if (((memw + 0x800) & 0xfff) <= 0xfff - SSIZE_OF(sw)) {
			FAIL_IF(load_immediate(compiler, TMP_REG1, TO_ARGW_HI(memw), TMP_REG3));
			memw &= 0xfff;
		} else {
			FAIL_IF(load_immediate(compiler, TMP_REG1, memw, TMP_REG3));
			memw = 0;
		}

		if (mem & REG_MASK)
			FAIL_IF(push_inst(compiler, ADD | RD(TMP_REG1) | RS1(TMP_REG1) | RS2(mem & REG_MASK)));

		mem = TMP_REG1;
	} else {
		mem &= REG_MASK;
		memw &= 0xfff;
	}

	SLJIT2_ASSERT((memw >= 0 && memw <= SIMM_MAX - SSIZE_OF(sw)) || (memw > SIMM_MAX && memw <= 0xfff));

	if (!(type & SLJIT2_MEM_STORE) && mem == REG_PAIR_FIRST(reg)) {
		FAIL_IF(push_mem_inst(compiler, WORD_DATA | LOAD_DATA, REG_PAIR_SECOND(reg), mem, (memw + SSIZE_OF(sw)) & 0xfff));
		return push_mem_inst(compiler, WORD_DATA | LOAD_DATA, REG_PAIR_FIRST(reg), mem, memw);
	}

	flags = WORD_DATA | (!(type & SLJIT2_MEM_STORE) ? LOAD_DATA : 0);

	FAIL_IF(push_mem_inst(compiler, flags, REG_PAIR_FIRST(reg), mem, memw));
	return push_mem_inst(compiler, flags, REG_PAIR_SECOND(reg), mem, (memw + SSIZE_OF(sw)) & 0xfff);
}

#undef TO_ARGW_HI

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_atomic_load(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst_reg,
	sljit2_s32 mem_reg)
{
	sljit2_ins ins;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_atomic_load(compiler, op, dst_reg, mem_reg));

	if (op & SLJIT2_ATOMIC_USE_CAS)
		return SLJIT2_ERR_UNSUPPORTED;

	switch (GET_OPCODE(op)) {
	case SLJIT2_MOV:
	case SLJIT2_MOV_P:
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
		ins = LR | (3 << 12);
		break;
#endif /* SLJIT2_CONFIG_RISCV_64 */
	case SLJIT2_MOV_S32:
	case SLJIT2_MOV32:
		ins = LR | (2 << 12);
		break;

	default:
		return SLJIT2_ERR_UNSUPPORTED;
	}

	if (op & SLJIT2_ATOMIC_TEST)
		return SLJIT2_SUCCESS;

	return push_inst(compiler, ins | RD(dst_reg) | RS1(mem_reg));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_atomic_store(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 src_reg,
	sljit2_s32 mem_reg,
	sljit2_s32 temp_reg)
{
	sljit2_ins ins;

	/* temp_reg == mem_reg is undefined so use another temp register */
	SLJIT2_UNUSED_ARG(temp_reg);

	CHECK_ERROR();
	CHECK(check_sljit2_emit_atomic_store(compiler, op, src_reg, mem_reg, temp_reg));

	if (op & SLJIT2_ATOMIC_USE_CAS)
		return SLJIT2_ERR_UNSUPPORTED;

	switch (GET_OPCODE(op)) {
	case SLJIT2_MOV:
	case SLJIT2_MOV_P:
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
		ins = SC | (3 << 12);
		break;
#endif /* SLJIT2_CONFIG_RISCV_64 */
	case SLJIT2_MOV_S32:
	case SLJIT2_MOV32:
		ins = SC | (2 << 12);
		break;

	default:
		return SLJIT2_ERR_UNSUPPORTED;
	}

	if (op & SLJIT2_ATOMIC_TEST)
		return SLJIT2_SUCCESS;

	return push_inst(compiler, ins | RD(OTHER_FLAG) | RS1(mem_reg) | RS2(src_reg));
}

/*
  SEW = Selected element width
  LMUL = Vector register group multiplier

  VLMUL values (in binary):
    100 : reserved
    101 : 1/8
    110 : 1/4
    111 : 1/2
    000 : 1
    001 : 2
    010 : 4
    011 : 8
*/

static SLJIT2_INLINE sljit2_s32 sljit2_emit_vsetivli(struct sljit2_compiler *compiler, sljit2_s32 type, sljit2_ins vlmul)
{
	sljit2_ins elem_size = (sljit2_ins)SLJIT2_SIMD_GET_ELEM_SIZE(type);
	sljit2_ins avl = (sljit2_ins)1 << (SLJIT2_SIMD_GET_REG_SIZE(type) - elem_size);

	return push_inst(compiler, VSETIVLI | RD(TMP_REG1) | (elem_size << 23) | (vlmul << 20) | (avl << 15));
}

static SLJIT2_INLINE sljit2_s32 sljit2_emit_vsetivli_size(struct sljit2_compiler *compiler, sljit2_s32 reg_size, sljit2_s32 elem_size)
{
	sljit2_ins avl = (sljit2_ins)1 << (reg_size - elem_size);
	return push_inst(compiler, VSETIVLI | RD(TMP_REG1) | ((sljit2_ins)elem_size << 23) | (avl << 15));
}

static sljit2_s32 sljit2_emit_vmem(struct sljit2_compiler *compiler, sljit2_ins ins, sljit2_s32 elem_size, sljit2_s32 mem, sljit2_sw memw)
{
	sljit2_s32 base = mem & REG_MASK;

	if (elem_size > 0)
		ins |= (1 << 14) | ((sljit2_ins)elem_size << 12);

	if (SLJIT2_UNLIKELY(mem & OFFS_REG_MASK)) {
		memw &= 0x3;

		if (SLJIT2_UNLIKELY(memw)) {
			FAIL_IF(push_inst(compiler, SLLI | RD(TMP_REG1) | RS1(OFFS_REG(mem)) | IMM_I(memw)));
		}

		FAIL_IF(push_inst(compiler, ADD | RD(TMP_REG1) | RS1(base) | RS2(!memw ? OFFS_REG(mem) : TMP_REG1)));
		return push_inst(compiler, ins | RS1(TMP_REG1));
	}

	if (memw == 0)
		return push_inst(compiler, ins | RS1(base));

	if (memw <= SIMM_MAX && memw >= SIMM_MIN) {
		FAIL_IF(push_inst(compiler, ADDI | RD(TMP_REG1) | RS1(base) | IMM_I(memw)));
		return push_inst(compiler, ins | RS1(TMP_REG1));
	}

	FAIL_IF(load_immediate(compiler, TMP_REG1, memw, TMP_REG3));

	if (base != 0)
		FAIL_IF(push_inst(compiler, ADD | RD(TMP_REG1) | RS1(TMP_REG1) | RS2(base)));

	return push_inst(compiler, ins | RS1(TMP_REG1));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_simd_mov(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 vreg,
	sljit2_s32 srcdst, sljit2_sw srcdstw)
{
	sljit2_s32 reg_size = SLJIT2_SIMD_GET_REG_SIZE(type);
	sljit2_s32 elem_size = SLJIT2_SIMD_GET_ELEM_SIZE(type);
	sljit2_ins ins;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_simd_mov(compiler, type, vreg, srcdst, srcdstw));

	ADJUST_LOCAL_OFFSET(srcdst, srcdstw);

	if (reg_size != 4)
		return SLJIT2_ERR_UNSUPPORTED;

	if (type & SLJIT2_SIMD_TEST)
		return SLJIT2_SUCCESS;

	if (elem_size > 3)
		elem_size = 3;

	FAIL_IF(sljit2_emit_vsetivli_size(compiler, reg_size, elem_size));

	if (srcdst & SLJIT2_MEM) {
		ins = (type & SLJIT2_SIMD_STORE) ? VS : VL;
		return sljit2_emit_vmem(compiler, ins | VRD(vreg), elem_size, srcdst, srcdstw);
	}

	if (type & SLJIT2_SIMD_STORE)
		ins = VRD(srcdst) | VRS1(vreg);
	else
		ins = VRD(vreg) | VRS1(srcdst);

	return push_inst(compiler, VMV_VV | ins);
}

static sljit2_s32 sljit2_simd_get_mem_flags(sljit2_s32 elem_size)
{
	switch (elem_size) {
	case 0:
		return BYTE_DATA;
	case 1:
		return HALF_DATA;
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	case 2:
		return INT_DATA;
#endif /* SLJIT2_CONFIG_RISCV_64 */
	default:
		return WORD_DATA;
	}
}

static sljit2_sw sljit2_simd_get_imm(sljit2_s32 elem_size, sljit2_sw imm)
{
	switch (elem_size) {
	case 0:
		return (sljit2_s8)imm;
	case 1:
		return (sljit2_s16)imm;
#if (defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64)
	case 2:
		return (sljit2_s32)imm;
#endif /* SLJIT2_CONFIG_RISCV_64 */
	default:
		return imm;
	}
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_simd_replicate(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 vreg,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_s32 reg_size = SLJIT2_SIMD_GET_REG_SIZE(type);
	sljit2_s32 elem_size = SLJIT2_SIMD_GET_ELEM_SIZE(type);
	sljit2_s32 flags;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_simd_replicate(compiler, type, vreg, src, srcw));

	ADJUST_LOCAL_OFFSET(src, srcw);

	if (reg_size != 4)
		return SLJIT2_ERR_UNSUPPORTED;

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	if ((type & SLJIT2_SIMD_FLOAT) ? (elem_size < 2 || elem_size > 3) : elem_size > 2)
		return SLJIT2_ERR_UNSUPPORTED;
#else /* !SLJIT2_CONFIG_RISCV_32 */
	if (((type & SLJIT2_SIMD_FLOAT) && elem_size < 2) || elem_size > 3)
		return SLJIT2_ERR_UNSUPPORTED;
#endif /* SLJIT2_CONFIG_RISCV_32 */

	if (type & SLJIT2_SIMD_TEST)
		return SLJIT2_SUCCESS;

	FAIL_IF(sljit2_emit_vsetivli(compiler, type, 0));

	if (type & SLJIT2_SIMD_FLOAT) {
		if (src == SLJIT2_IMM)
			return push_inst(compiler, VMV_VI | VRD(vreg) | ((sljit2_ins)(srcw & 0x1f) << 15));

		if (src & SLJIT2_MEM) {
			flags = (elem_size == 2) ? SINGLE_DATA : DOUBLE_DATA;
			FAIL_IF(emit_op_mem(compiler, flags | LOAD_DATA, TMP_FREG1, src, srcw));
			src = TMP_FREG1;
		}

		return push_inst(compiler, VFMV_VF | VRD(vreg) | FRS1(src));
	}

	if (src == SLJIT2_IMM) {
		srcw = sljit2_simd_get_imm(elem_size, srcw);

		if (srcw >= -0x10 && srcw <= 0xf)
			return push_inst(compiler, VMV_VI | VRD(vreg) | ((sljit2_ins)(srcw & 0x1f) << 15));

		FAIL_IF(load_immediate(compiler, TMP_REG1, srcw, TMP_REG3));
		src = TMP_REG1;
	} else if (src & SLJIT2_MEM) {
		FAIL_IF(emit_op_mem(compiler, sljit2_simd_get_mem_flags(elem_size) | LOAD_DATA, TMP_REG1, src, srcw));
		src = TMP_REG1;
	}

	return push_inst(compiler, VMV_VX | VRD(vreg) | RS1(src));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_simd_lane_mov(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 vreg, sljit2_s32 lane_index,
	sljit2_s32 srcdst, sljit2_sw srcdstw)
{
	sljit2_s32 reg_size = SLJIT2_SIMD_GET_REG_SIZE(type);
	sljit2_s32 elem_size = SLJIT2_SIMD_GET_ELEM_SIZE(type);
	sljit2_s32 flags;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_simd_lane_mov(compiler, type, vreg, lane_index, srcdst, srcdstw));

	ADJUST_LOCAL_OFFSET(srcdst, srcdstw);

	if (reg_size != 4)
		return SLJIT2_ERR_UNSUPPORTED;

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	if ((type & SLJIT2_SIMD_FLOAT) ? (elem_size < 2 || elem_size > 3) : elem_size > 2)
		return SLJIT2_ERR_UNSUPPORTED;
#else /* !SLJIT2_CONFIG_RISCV_32 */
	if (((type & SLJIT2_SIMD_FLOAT) && elem_size < 2) || elem_size > 3)
		return SLJIT2_ERR_UNSUPPORTED;
#endif /* SLJIT2_CONFIG_RISCV_32 */

	if (type & SLJIT2_SIMD_TEST)
		return SLJIT2_SUCCESS;

	if (type & SLJIT2_SIMD_STORE) {
		FAIL_IF(push_inst(compiler, VSETIVLI | RD(TMP_REG1) | ((sljit2_ins)elem_size << 23) | (1 << 15)));

		if (lane_index > 0) {
			FAIL_IF(push_inst(compiler, VSLIDEDOWN_VI | VRD(TMP_VREG1) | ((sljit2_ins)lane_index << 15) | VRS2(vreg)));
			vreg = TMP_VREG1;
		}

		if (srcdst & SLJIT2_MEM)
			return sljit2_emit_vmem(compiler, VS | VRD(vreg), elem_size, srcdst, srcdstw);

		if (type & SLJIT2_SIMD_FLOAT)
			return push_inst(compiler, VFMV_FS | FRD(srcdst) | VRS2(vreg));

		FAIL_IF(push_inst(compiler, VMV_XS | RD(srcdst) | VRS2(vreg)));

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
		if ((type & SLJIT2_SIMD_LANE_SIGNED) || elem_size >= 2)
			return SLJIT2_SUCCESS;
#else /* !SLJIT2_CONFIG_RISCV_32 */
		if ((type & SLJIT2_SIMD_LANE_SIGNED) || elem_size >= 3 || (elem_size == 2 && (type & SLJIT2_32)))
			return SLJIT2_SUCCESS;
#endif /* SLJIT2_CONFIG_RISCV_32 */

		if (elem_size == 0)
			return push_inst(compiler, ANDI | RD(srcdst) | RS1(srcdst) | IMM_I(0xff));

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
		flags = 16;
#else /* !SLJIT2_CONFIG_RISCV_32 */
		flags = (elem_size == 1) ? 48 : 32;
#endif /* SLJIT2_CONFIG_RISCV_32 */

		FAIL_IF(push_inst(compiler, SLLI | RD(srcdst) | RS1(srcdst) | IMM_I(flags)));
		return push_inst(compiler, SRLI | RD(srcdst) | RS1(srcdst) | IMM_I(flags));
	}

	if (type & SLJIT2_SIMD_LANE_ZERO) {
		FAIL_IF(sljit2_emit_vsetivli(compiler, type, 0));
		FAIL_IF(push_inst(compiler, VMV_VI | VRD(vreg)));
	}

	if (srcdst & SLJIT2_MEM) {
		FAIL_IF(push_inst(compiler, VSETIVLI | RD(TMP_REG1) | ((sljit2_ins)elem_size << 23) | (1 << 15)));
		FAIL_IF(sljit2_emit_vmem(compiler, VL | VRD(lane_index > 0 ? TMP_VREG1 : vreg), elem_size, srcdst, srcdstw));

		if (lane_index == 0)
			return SLJIT2_SUCCESS;

		FAIL_IF(push_inst(compiler, VSETIVLI | RD(TMP_REG1) | ((sljit2_ins)elem_size << 23) | ((sljit2_ins)(lane_index + 1) << 15)));
		return push_inst(compiler, VSLIDEUP_VI | VRD(vreg) | ((sljit2_ins)lane_index << 15) | VRS2(TMP_VREG1));
	}

	if (!(type & SLJIT2_SIMD_LANE_ZERO) || lane_index > 0)
		FAIL_IF(push_inst(compiler, VSETIVLI | RD(TMP_REG1) | ((sljit2_ins)elem_size << 23) | ((sljit2_ins)(lane_index + 1) << 15)));

	if (type & SLJIT2_SIMD_FLOAT) {
		FAIL_IF(push_inst(compiler, VFMV_SF | VRD(lane_index > 0 ? TMP_VREG1 : vreg) | FRS1(srcdst)));

		if (lane_index == 0)
			return SLJIT2_SUCCESS;

		return push_inst(compiler, VSLIDEUP_VI | VRD(vreg) | ((sljit2_ins)lane_index << 15) | VRS2(TMP_VREG1));
	}

	if (srcdst == SLJIT2_IMM) {
		srcdstw = sljit2_simd_get_imm(elem_size, srcdstw);
		FAIL_IF(load_immediate(compiler, TMP_REG1, srcdstw, TMP_REG3));
		srcdst = TMP_REG1;
	}

	FAIL_IF(push_inst(compiler, VMV_SX | VRD(lane_index > 0 ? TMP_VREG1 : vreg) | RS1(srcdst)));

	if (lane_index == 0)
		return SLJIT2_SUCCESS;

	return push_inst(compiler, VSLIDEUP_VI | VRD(vreg) | ((sljit2_ins)lane_index << 15) | VRS2(TMP_VREG1));
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

	if (((type & SLJIT2_SIMD_FLOAT) && elem_size < 2) || elem_size > 3)
		return SLJIT2_ERR_UNSUPPORTED;

	if (type & SLJIT2_SIMD_TEST)
		return SLJIT2_SUCCESS;

	FAIL_IF(sljit2_emit_vsetivli(compiler, type, 0));

	FAIL_IF(push_inst(compiler, VRGATHER_VI | VRD(vreg != src ? vreg : TMP_VREG1) | ((sljit2_ins)src_lane_index << 15) | VRS2(src)));
	if (vreg == src)
		return push_inst(compiler, VMV_VV | VRD(vreg) | VRS1(TMP_VREG1));
	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_simd_extend(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 vreg,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_s32 reg_size = SLJIT2_SIMD_GET_REG_SIZE(type);
	sljit2_s32 elem_size = SLJIT2_SIMD_GET_ELEM_SIZE(type);
	sljit2_s32 elem2_size = SLJIT2_SIMD_GET_ELEM2_SIZE(type);
	sljit2_ins ins;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_simd_extend(compiler, type, vreg, src, srcw));

	ADJUST_LOCAL_OFFSET(src, srcw);

	if (reg_size != 4)
		return SLJIT2_ERR_UNSUPPORTED;

#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	if ((type & SLJIT2_SIMD_FLOAT) ? (elem_size < 2 || elem_size > 3) : elem_size > 2)
		return SLJIT2_ERR_UNSUPPORTED;
#else /* !SLJIT2_CONFIG_RISCV_32 */
	if (((type & SLJIT2_SIMD_FLOAT) && elem_size < 2) || elem_size > 3)
		return SLJIT2_ERR_UNSUPPORTED;
#endif /* SLJIT2_CONFIG_RISCV_32 */

	if (type & SLJIT2_SIMD_TEST)
		return SLJIT2_SUCCESS;

	if ((src & SLJIT2_MEM) || vreg == src) {
		ins = (sljit2_ins)1 << (reg_size - elem2_size);
		FAIL_IF(push_inst(compiler, VSETIVLI | RD(TMP_REG1) | ((sljit2_ins)elem_size << 23) | (ins << 15)));

		if (src & SLJIT2_MEM)
			FAIL_IF(sljit2_emit_vmem(compiler, VL | VRD(TMP_VREG1), elem_size, src, srcw));
		else
			FAIL_IF(push_inst(compiler, VMV_VV | VRD(TMP_VREG1) | VRS1(src)));

		src = TMP_VREG1;
	}

	if (type & SLJIT2_SIMD_FLOAT) {
		FAIL_IF(sljit2_emit_vsetivli(compiler, type, 0x7));
		return push_inst(compiler, VFWCVT_FFV | VRD(vreg) | VRS2(src));
	}

	ins = (sljit2_ins)1 << (reg_size - elem2_size);
	FAIL_IF(push_inst(compiler, VSETIVLI | RD(TMP_REG1) | ((sljit2_ins)elem2_size << 23) | (ins << 15)));

	switch (elem2_size - elem_size) {
	case 1:
		ins = VZEXT_VF2;
		break;
	case 2:
		ins = VZEXT_VF4;
		break;
	default:
		ins = VZEXT_VF8;
		break;
	}

	if (type & SLJIT2_SIMD_EXTEND_SIGNED)
		ins |= 1 << 15;

	return push_inst(compiler, ins | VRD(vreg) | VRS2(src));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_simd_sign(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 vreg,
	sljit2_s32 dst, sljit2_sw dstw)
{
	sljit2_s32 reg_size = SLJIT2_SIMD_GET_REG_SIZE(type);
	sljit2_s32 elem_size = SLJIT2_SIMD_GET_ELEM_SIZE(type);
	sljit2_s32 dst_r = FAST_IS_REG(dst) ? dst : TMP_REG2;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_simd_sign(compiler, type, vreg, dst, dstw));

	ADJUST_LOCAL_OFFSET(dst, dstw);

	if (reg_size != 4)
		return SLJIT2_ERR_UNSUPPORTED;

	if (((type & SLJIT2_SIMD_FLOAT) && elem_size < 2) || elem_size > 3)
		return SLJIT2_ERR_UNSUPPORTED;

	FAIL_IF(sljit2_emit_vsetivli(compiler, type, 0));
	FAIL_IF(push_inst(compiler, VMV_VI | VRD(TMP_VREG1) | (0x0 << 15)));
	FAIL_IF(push_inst(compiler, VMSLE_VI | VRD(TMP_VREG1) | (0x0 << 15) | VRS2(vreg)));

	FAIL_IF(sljit2_emit_vsetivli_size(compiler, 2, 2));
	FAIL_IF(push_inst(compiler, VMV_XS | RD(dst_r) | VRS2(TMP_VREG1)));

	if (dst & SLJIT2_MEM)
		return emit_op_mem(compiler, (type & SLJIT2_32) ? INT_DATA : WORD_DATA, dst_r, dst, dstw);
	return SLJIT2_SUCCESS;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_simd_op2(struct sljit2_compiler *compiler, sljit2_s32 type,
	sljit2_s32 dst_vreg, sljit2_s32 src1_vreg, sljit2_s32 src2, sljit2_sw src2w)
{
	sljit2_s32 reg_size = SLJIT2_SIMD_GET_REG_SIZE(type);
	sljit2_s32 elem_size = SLJIT2_SIMD_GET_ELEM_SIZE(type);
	sljit2_ins ins = 0;

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
		ins = VAND_VV;
		break;
	case SLJIT2_SIMD_OP2_OR:
		ins = VOR_VV;
		break;
	case SLJIT2_SIMD_OP2_XOR:
		ins = VXOR_VV;
		break;
	case SLJIT2_SIMD_OP2_SHUFFLE:
		ins = VRGATHER_VV;
		elem_size = 0;
		break;
	}

	if (elem_size > 3)
		elem_size = 3;

	FAIL_IF(sljit2_emit_vsetivli_size(compiler, reg_size, elem_size));

	if (src2 & SLJIT2_MEM) {
		FAIL_IF(sljit2_emit_vmem(compiler, VL | VRD(TMP_VREG1), elem_size, src2, src2w));
		src2 = TMP_VREG1;
	}

	if (SLJIT2_SIMD_GET_OPCODE(type) != SLJIT2_SIMD_OP2_SHUFFLE)
		return push_inst(compiler, ins | VRD(dst_vreg) | VRS1(src1_vreg) | VRS2(src2));

	if (dst_vreg == src2) {
		FAIL_IF(push_inst(compiler, VMV_VV | VRD(TMP_VREG1) | VRS1(src2)));
		src2 = TMP_VREG1;
	}

	if (dst_vreg == src1_vreg) {
		FAIL_IF(push_inst(compiler, VMV_VV | VRD(TMP_VREG2) | VRS1(src1_vreg)));
		src1_vreg = TMP_VREG2;
	}

	return push_inst(compiler, ins | VRD(dst_vreg) | VRS1(src2) | VRS2(src1_vreg));
}

SLJIT2_API_FUNC_ATTRIBUTE struct sljit2_const* sljit2_emit_const(struct sljit2_compiler *compiler, sljit2_s32 dst, sljit2_sw dstw, sljit2_sw init_value)
{
	struct sljit2_const *const_;
	sljit2_s32 dst_r;

	CHECK_ERROR_PTR();
	CHECK_PTR(check_sljit2_emit_const(compiler, dst, dstw, init_value));
	ADJUST_LOCAL_OFFSET(dst, dstw);

	const_ = (struct sljit2_const*)ensure_abuf(compiler, sizeof(struct sljit2_const));
	PTR_FAIL_IF(!const_);
	set_const(const_, compiler);

	dst_r = FAST_IS_REG(dst) ? dst : TMP_REG2;
	PTR_FAIL_IF(emit_const(compiler, dst_r, init_value, ADDI | RD(dst_r)));

	if (dst & SLJIT2_MEM)
		PTR_FAIL_IF(emit_op_mem(compiler, WORD_DATA, TMP_REG2, dst, dstw));

	return const_;
}

SLJIT2_API_FUNC_ATTRIBUTE struct sljit2_jump* sljit2_emit_mov_addr(struct sljit2_compiler *compiler, sljit2_s32 dst, sljit2_sw dstw)
{
	struct sljit2_jump *jump;
	sljit2_s32 dst_r;

	CHECK_ERROR_PTR();
	CHECK_PTR(check_sljit2_emit_mov_addr(compiler, dst, dstw));
	ADJUST_LOCAL_OFFSET(dst, dstw);

	jump = (struct sljit2_jump*)ensure_abuf(compiler, sizeof(struct sljit2_jump));
	PTR_FAIL_IF(!jump);
	set_mov_addr(jump, compiler, 0);

	dst_r = FAST_IS_REG(dst) ? dst : TMP_REG2;
	PTR_FAIL_IF(push_inst(compiler, (sljit2_ins)dst_r));
#if (defined SLJIT2_CONFIG_RISCV_32 && SLJIT2_CONFIG_RISCV_32)
	compiler->size += 1;
#else /* !SLJIT2_CONFIG_RISCV_32 */
	compiler->size += 5;
#endif /* SLJIT2_CONFIG_RISCV_32 */

	if (dst & SLJIT2_MEM)
		PTR_FAIL_IF(emit_op_mem(compiler, WORD_DATA, TMP_REG2, dst, dstw));

	return jump;
}

SLJIT2_API_FUNC_ATTRIBUTE void sljit2_set_const(sljit2_uw addr, sljit2_sw new_constant, sljit2_sw executable_offset)
{
	sljit2_set_jump_addr(addr, (sljit2_uw)new_constant, executable_offset);
}
