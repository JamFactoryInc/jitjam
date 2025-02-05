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

/* ppc 64-bit arch dependent functions. */

#if defined(__GNUC__) || (defined(__IBM_GCC_ASM) && __IBM_GCC_ASM)
#define ASM_SLJIT2_CLZ(src, dst) \
	__asm__ volatile ( "cntlzd %0, %1" : "=r"(dst) : "r"(src) )
#elif defined(__xlc__)
#error "Please enable GCC syntax for inline assembly statements"
#else
#error "Must implement count leading zeroes"
#endif

/* Computes SLDI(63 - shift). */
#define PUSH_SLDI_NEG(reg, shift) \
	push_inst(compiler, RLDICR | S(reg) | A(reg) | RLDI_SH(63 - shift) | RLDI_ME(shift))

static sljit2_s32 load_immediate(struct sljit2_compiler *compiler, sljit2_s32 reg, sljit2_sw imm)
{
	sljit2_uw tmp;
	sljit2_uw shift;
	sljit2_uw tmp2;
	sljit2_uw shift2;

	if (imm <= SIMM_MAX && imm >= SIMM_MIN)
		return push_inst(compiler, ADDI | D(reg) | A(0) | IMM(imm));

	if (((sljit2_uw)imm >> 16) == 0)
		return push_inst(compiler, ORI | S(TMP_ZERO) | A(reg) | IMM(imm));

	if (imm <= 0x7fffffffl && imm >= -0x80000000l) {
		FAIL_IF(push_inst(compiler, ADDIS | D(reg) | A(0) | IMM(imm >> 16)));
		return (imm & 0xffff) ? push_inst(compiler, ORI | S(reg) | A(reg) | IMM(imm)) : SLJIT2_SUCCESS;
	}

	if (((sljit2_uw)imm >> 32) == 0) {
		FAIL_IF(push_inst(compiler, ORIS | S(TMP_ZERO) | A(reg) | IMM(imm >> 16)));
		return (imm & 0xffff) ? push_inst(compiler, ORI | S(reg) | A(reg) | IMM(imm)) : SLJIT2_SUCCESS;
	}

	/* Count leading zeroes. */
	tmp = (sljit2_uw)((imm >= 0) ? imm : ~imm);
	ASM_SLJIT2_CLZ(tmp, shift);
	SLJIT2_ASSERT(shift > 0);
	shift--;
	tmp = ((sljit2_uw)imm << shift);

	if ((tmp & ~0xffff000000000000ul) == 0) {
		FAIL_IF(push_inst(compiler, ADDI | D(reg) | A(0) | (sljit2_ins)(tmp >> 48)));
		shift += 15;
		return PUSH_SLDI_NEG(reg, shift);
	}

	if ((tmp & ~0xffffffff00000000ul) == 0) {
		FAIL_IF(push_inst(compiler, ADDIS | D(reg) | A(0) | (sljit2_ins)(tmp >> 48)));
		FAIL_IF(push_inst(compiler, ORI | S(reg) | A(reg) | IMM(tmp >> 32)));
		shift += 31;
		return PUSH_SLDI_NEG(reg, shift);
	}

	/* Cut out the 16 bit from immediate. */
	shift += 15;
	tmp2 = (sljit2_uw)imm & (((sljit2_uw)1 << (63 - shift)) - 1);

	if (tmp2 <= 0xffff) {
		FAIL_IF(push_inst(compiler, ADDI | D(reg) | A(0) | (sljit2_ins)(tmp >> 48)));
		FAIL_IF(PUSH_SLDI_NEG(reg, shift));
		return push_inst(compiler, ORI | S(reg) | A(reg) | (sljit2_ins)tmp2);
	}

	if (tmp2 <= 0xffffffff) {
		FAIL_IF(push_inst(compiler, ADDI | D(reg) | A(0) | IMM(tmp >> 48)));
		FAIL_IF(PUSH_SLDI_NEG(reg, shift));
		FAIL_IF(push_inst(compiler, ORIS | S(reg) | A(reg) | (sljit2_ins)(tmp2 >> 16)));
		return (imm & 0xffff) ? push_inst(compiler, ORI | S(reg) | A(reg) | IMM(tmp2)) : SLJIT2_SUCCESS;
	}

	ASM_SLJIT2_CLZ(tmp2, shift2);
	tmp2 <<= shift2;

	if ((tmp2 & ~0xffff000000000000ul) == 0) {
		FAIL_IF(push_inst(compiler, ADDI | D(reg) | A(0) | (sljit2_ins)(tmp >> 48)));
		shift2 += 15;
		shift += (63 - shift2);
		FAIL_IF(PUSH_SLDI_NEG(reg, shift));
		FAIL_IF(push_inst(compiler, ORI | S(reg) | A(reg) | (sljit2_ins)(tmp2 >> 48)));
		return PUSH_SLDI_NEG(reg, shift2);
	}

	/* The general version. */
	FAIL_IF(push_inst(compiler, ADDIS | D(reg) | A(0) | (sljit2_ins)((sljit2_uw)imm >> 48)));
	FAIL_IF(push_inst(compiler, ORI | S(reg) | A(reg) | IMM(imm >> 32)));
	FAIL_IF(PUSH_SLDI_NEG(reg, 31));
	FAIL_IF(push_inst(compiler, ORIS | S(reg) | A(reg) | IMM(imm >> 16)));
	return push_inst(compiler, ORI | S(reg) | A(reg) | IMM(imm));
}

#undef PUSH_SLDI_NEG

#define CLRLDI(dst, src, n) \
	(RLDICL | S(src) | A(dst) | RLDI_SH(0) | RLDI_MB(n))

/* Sign extension for integer operations. */
#define UN_EXTS() \
	if ((flags & (ALT_SIGN_EXT | REG2_SOURCE)) == (ALT_SIGN_EXT | REG2_SOURCE)) { \
		FAIL_IF(push_inst(compiler, EXTSW | S(src2) | A(TMP_REG2))); \
		src2 = TMP_REG2; \
	}

#define BIN_EXTS() \
	if (flags & ALT_SIGN_EXT) { \
		if (flags & REG1_SOURCE) { \
			FAIL_IF(push_inst(compiler, EXTSW | S(src1) | A(TMP_REG1))); \
			src1 = TMP_REG1; \
		} \
		if (flags & REG2_SOURCE) { \
			FAIL_IF(push_inst(compiler, EXTSW | S(src2) | A(TMP_REG2))); \
			src2 = TMP_REG2; \
		} \
	}

#define BIN_IMM_EXTS() \
	if ((flags & (ALT_SIGN_EXT | REG1_SOURCE)) == (ALT_SIGN_EXT | REG1_SOURCE)) { \
		FAIL_IF(push_inst(compiler, EXTSW | S(src1) | A(TMP_REG1))); \
		src1 = TMP_REG1; \
	}

static SLJIT2_INLINE sljit2_s32 emit_single_op(struct sljit2_compiler *compiler, sljit2_s32 op, sljit2_s32 flags,
	sljit2_s32 dst, sljit2_s32 src1, sljit2_s32 src2)
{
	sljit2_u32 imm;

	switch (op) {
	case SLJIT2_MOV:
	case SLJIT2_MOV_P:
		SLJIT2_ASSERT(src1 == TMP_REG1);
		if (dst != src2)
			return push_inst(compiler, OR | S(src2) | A(dst) | B(src2));
		return SLJIT2_SUCCESS;

	case SLJIT2_MOV_U32:
	case SLJIT2_MOV_S32:
		SLJIT2_ASSERT(src1 == TMP_REG1);
		if ((flags & (REG_DEST | REG2_SOURCE)) == (REG_DEST | REG2_SOURCE)) {
			if (op == SLJIT2_MOV_S32)
				return push_inst(compiler, EXTSW | S(src2) | A(dst));
			return push_inst(compiler, CLRLDI(dst, src2, 32));
		}
		else {
			SLJIT2_ASSERT(dst == src2);
		}
		return SLJIT2_SUCCESS;

	case SLJIT2_MOV_U8:
	case SLJIT2_MOV_S8:
		SLJIT2_ASSERT(src1 == TMP_REG1);
		if ((flags & (REG_DEST | REG2_SOURCE)) == (REG_DEST | REG2_SOURCE)) {
			if (op == SLJIT2_MOV_S8)
				return push_inst(compiler, EXTSB | S(src2) | A(dst));
			return push_inst(compiler, CLRLDI(dst, src2, 56));
		}
		else if ((flags & REG_DEST) && op == SLJIT2_MOV_S8)
			return push_inst(compiler, EXTSB | S(src2) | A(dst));
		else {
			SLJIT2_ASSERT(dst == src2);
		}
		return SLJIT2_SUCCESS;

	case SLJIT2_MOV_U16:
	case SLJIT2_MOV_S16:
		SLJIT2_ASSERT(src1 == TMP_REG1);
		if ((flags & (REG_DEST | REG2_SOURCE)) == (REG_DEST | REG2_SOURCE)) {
			if (op == SLJIT2_MOV_S16)
				return push_inst(compiler, EXTSH | S(src2) | A(dst));
			return push_inst(compiler, CLRLDI(dst, src2, 48));
		}
		else {
			SLJIT2_ASSERT(dst == src2);
		}
		return SLJIT2_SUCCESS;

	case SLJIT2_CLZ:
		SLJIT2_ASSERT(src1 == TMP_REG1);
		return push_inst(compiler, ((flags & ALT_FORM1) ? CNTLZW : CNTLZD) | S(src2) | A(dst));

	case SLJIT2_CTZ:
		SLJIT2_ASSERT(src1 == TMP_REG1);
		FAIL_IF(push_inst(compiler, NEG | D(TMP_REG1) | A(src2)));
		FAIL_IF(push_inst(compiler, AND | S(src2) | A(dst) | B(TMP_REG1)));
		FAIL_IF(push_inst(compiler, ((flags & ALT_FORM1) ? CNTLZW : CNTLZD) | S(dst) | A(dst)));
		FAIL_IF(push_inst(compiler, ADDI | D(TMP_REG1) | A(dst) | IMM((flags & ALT_FORM1) ? -32 : -64)));
		/* The highest bits are set, if dst < bit width, zero otherwise. */
		FAIL_IF(push_inst(compiler, ((flags & ALT_FORM1) ? SRWI(27) : SRDI(58)) | S(TMP_REG1) | A(TMP_REG1)));
		return push_inst(compiler, XOR | S(dst) | A(dst) | B(TMP_REG1));

	case SLJIT2_ADD:
		if (flags & ALT_FORM1) {
			if (flags & ALT_SIGN_EXT) {
				FAIL_IF(push_inst(compiler, SLDI(32) | S(src1) | A(TMP_REG1)));
				src1 = TMP_REG1;
				FAIL_IF(push_inst(compiler, SLDI(32) | S(src2) | A(TMP_REG2)));
				src2 = TMP_REG2;
			}
			/* Setting XER SO is not enough, CR SO is also needed. */
			FAIL_IF(push_inst(compiler, ADD | OE(ALT_SET_FLAGS) | RC(ALT_SET_FLAGS) | D(dst) | A(src1) | B(src2)));
			if (flags & ALT_SIGN_EXT)
				return push_inst(compiler, SRDI(32) | S(dst) | A(dst));
			return SLJIT2_SUCCESS;
		}

		if (flags & ALT_FORM2) {
			/* Flags does not set: BIN_IMM_EXTS unnecessary. */
			SLJIT2_ASSERT(src2 == TMP_REG2);

			if (flags & ALT_FORM3)
				return push_inst(compiler, ADDIS | D(dst) | A(src1) | compiler->imm);

			imm = compiler->imm;

			if (flags & ALT_FORM4) {
				FAIL_IF(push_inst(compiler, ADDIS | D(dst) | A(src1) | (((imm >> 16) & 0xffff) + ((imm >> 15) & 0x1))));
				src1 = dst;
			}

			return push_inst(compiler, ADDI | D(dst) | A(src1) | (imm & 0xffff));
		}
		if (flags & ALT_FORM3) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			BIN_IMM_EXTS();
			return push_inst(compiler, ADDIC | D(dst) | A(src1) | compiler->imm);
		}
		if (flags & ALT_FORM4) {
			if (flags & ALT_FORM5)
				FAIL_IF(push_inst(compiler, ADDI | D(dst) | A(src1) | compiler->imm));
			else
				FAIL_IF(push_inst(compiler, ADD | D(dst) | A(src1) | B(src2)));
			return push_inst(compiler, CMPI | A(dst) | 0);
		}
		if (!(flags & ALT_SET_FLAGS))
			return push_inst(compiler, ADD | D(dst) | A(src1) | B(src2));
		BIN_EXTS();
		if (flags & ALT_FORM5)
			return push_inst(compiler, ADDC | RC(ALT_SET_FLAGS) | D(dst) | A(src1) | B(src2));
		return push_inst(compiler, ADD | RC(flags) | D(dst) | A(src1) | B(src2));

	case SLJIT2_ADDC:
		BIN_EXTS();
		return push_inst(compiler, ADDE | D(dst) | A(src1) | B(src2));

	case SLJIT2_SUB:
		if (flags & ALT_FORM1) {
			if (flags & ALT_FORM2) {
				FAIL_IF(push_inst(compiler, CMPLI | CRD(0 | ((flags & ALT_SIGN_EXT) ? 0 : 1)) | A(src1) | compiler->imm));
				if (!(flags & ALT_FORM3))
					return SLJIT2_SUCCESS;
				return push_inst(compiler, ADDI | D(dst) | A(src1) | (-compiler->imm & 0xffff));
			}
			FAIL_IF(push_inst(compiler, CMPL | CRD(0 | ((flags & ALT_SIGN_EXT) ? 0 : 1)) | A(src1) | B(src2)));
			if (!(flags & ALT_FORM3))
				return SLJIT2_SUCCESS;
			return push_inst(compiler, SUBF | D(dst) | A(src2) | B(src1));
		}

		if (flags & ALT_FORM2) {
			if (flags & ALT_FORM3) {
				FAIL_IF(push_inst(compiler, CMPI | CRD(0 | ((flags & ALT_SIGN_EXT) ? 0 : 1)) | A(src1) | compiler->imm));
				if (!(flags & ALT_FORM4))
					return SLJIT2_SUCCESS;
				return push_inst(compiler, ADDI | D(dst) | A(src1) | (-compiler->imm & 0xffff));
			}
			FAIL_IF(push_inst(compiler, CMP | CRD(0 | ((flags & ALT_SIGN_EXT) ? 0 : 1)) | A(src1) | B(src2)));
			if (!(flags & ALT_FORM4))
				return SLJIT2_SUCCESS;
			return push_inst(compiler, SUBF | D(dst) | A(src2) | B(src1));
		}

		if (flags & ALT_FORM3) {
			if (flags & ALT_SIGN_EXT) {
				if (src1 != TMP_ZERO) {
					FAIL_IF(push_inst(compiler, SLDI(32) | S(src1) | A(TMP_REG1)));
					src1 = TMP_REG1;
				}
				if (src2 != TMP_ZERO) {
					FAIL_IF(push_inst(compiler, SLDI(32) | S(src2) | A(TMP_REG2)));
					src2 = TMP_REG2;
				}
			}

			/* Setting XER SO is not enough, CR SO is also needed. */
			if (src1 != TMP_ZERO)
				FAIL_IF(push_inst(compiler, SUBF | OE(ALT_SET_FLAGS) | RC(ALT_SET_FLAGS) | D(dst) | A(src2) | B(src1)));
			else
				FAIL_IF(push_inst(compiler, NEG | OE(ALT_SET_FLAGS) | RC(ALT_SET_FLAGS) | D(dst) | A(src2)));

			if (flags & ALT_SIGN_EXT)
				return push_inst(compiler, SRDI(32) | S(dst) | A(dst));
			return SLJIT2_SUCCESS;
		}

		if (flags & ALT_FORM4) {
			/* Flags does not set: BIN_IMM_EXTS unnecessary. */
			SLJIT2_ASSERT(src2 == TMP_REG2);
			return push_inst(compiler, SUBFIC | D(dst) | A(src1) | compiler->imm);
		}

		if (!(flags & ALT_SET_FLAGS)) {
			SLJIT2_ASSERT(src1 != TMP_ZERO);
			return push_inst(compiler, SUBF | D(dst) | A(src2) | B(src1));
		}

		BIN_EXTS();
		if (flags & ALT_FORM5)
			return push_inst(compiler, SUBFC | RC(ALT_SET_FLAGS) | D(dst) | A(src2) | B(src1));

		if (src1 != TMP_ZERO)
			return push_inst(compiler, SUBF | RC(ALT_SET_FLAGS) | D(dst) | A(src2) | B(src1));
		return push_inst(compiler, NEG | RC(ALT_SET_FLAGS) | D(dst) | A(src2));

	case SLJIT2_SUBC:
		BIN_EXTS();
		return push_inst(compiler, SUBFE | D(dst) | A(src2) | B(src1));

	case SLJIT2_MUL:
		if (flags & ALT_FORM1) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			return push_inst(compiler, MULLI | D(dst) | A(src1) | compiler->imm);
		}
		BIN_EXTS();
		if (flags & ALT_FORM2)
			return push_inst(compiler, MULLW | OE(flags) | RC(flags) | D(dst) | A(src2) | B(src1));
		return push_inst(compiler, MULLD | OE(flags) | RC(flags) | D(dst) | A(src2) | B(src1));

	case SLJIT2_AND:
		if (flags & ALT_FORM1) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			return push_inst(compiler, ANDI | S(src1) | A(dst) | compiler->imm);
		}
		if (flags & ALT_FORM2) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			return push_inst(compiler, ANDIS | S(src1) | A(dst) | compiler->imm);
		}
		return push_inst(compiler, AND | RC(flags) | S(src1) | A(dst) | B(src2));

	case SLJIT2_OR:
		if (flags & ALT_FORM1) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			return push_inst(compiler, ORI | S(src1) | A(dst) | compiler->imm);
		}
		if (flags & ALT_FORM2) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			return push_inst(compiler, ORIS | S(src1) | A(dst) | compiler->imm);
		}
		if (flags & ALT_FORM3) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			imm = compiler->imm;

			FAIL_IF(push_inst(compiler, ORI | S(src1) | A(dst) | IMM(imm)));
			return push_inst(compiler, ORIS | S(dst) | A(dst) | IMM(imm >> 16));
		}
		return push_inst(compiler, OR | RC(flags) | S(src1) | A(dst) | B(src2));

	case SLJIT2_XOR:
		if (flags & ALT_FORM1) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			return push_inst(compiler, XORI | S(src1) | A(dst) | compiler->imm);
		}
		if (flags & ALT_FORM2) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			return push_inst(compiler, XORIS | S(src1) | A(dst) | compiler->imm);
		}
		if (flags & ALT_FORM3) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			imm = compiler->imm;

			FAIL_IF(push_inst(compiler, XORI | S(src1) | A(dst) | IMM(imm)));
			return push_inst(compiler, XORIS | S(dst) | A(dst) | IMM(imm >> 16));
		}
		if (flags & ALT_FORM4) {
			SLJIT2_ASSERT(src1 == TMP_REG1);
			UN_EXTS();
			return push_inst(compiler, NOR | RC(flags) | S(src2) | A(dst) | B(src2));
		}
		return push_inst(compiler, XOR | RC(flags) | S(src1) | A(dst) | B(src2));

	case SLJIT2_SHL:
	case SLJIT2_MSHL:
		if (flags & ALT_FORM1) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			imm = compiler->imm;

			if (flags & ALT_FORM2) {
				imm &= 0x1f;
				return push_inst(compiler, SLWI(imm) | RC(flags) | S(src1) | A(dst));
			}

			imm &= 0x3f;
			return push_inst(compiler, SLDI(imm) | RC(flags) | S(src1) | A(dst));
		}

		if (op == SLJIT2_MSHL) {
			FAIL_IF(push_inst(compiler, ANDI | S(src2) | A(TMP_REG2) | ((flags & ALT_FORM2) ? 0x1f : 0x3f)));
			src2 = TMP_REG2;
		}

		return push_inst(compiler, ((flags & ALT_FORM2) ? SLW : SLD) | RC(flags) | S(src1) | A(dst) | B(src2));

	case SLJIT2_LSHR:
	case SLJIT2_MLSHR:
		if (flags & ALT_FORM1) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			imm = compiler->imm;

			if (flags & ALT_FORM2) {
				imm &= 0x1f;
				/* Since imm can be 0, SRWI() cannot be used. */
				return push_inst(compiler, RLWINM | RC(flags) | S(src1) | A(dst) | RLWI_SH((32 - imm) & 0x1f) | RLWI_MBE(imm, 31));
			}

			imm &= 0x3f;
			/* Since imm can be 0, SRDI() cannot be used. */
			return push_inst(compiler, RLDICL | RC(flags) | S(src1) | A(dst) | RLDI_SH((64 - imm) & 0x3f) | RLDI_MB(imm));
		}

		if (op == SLJIT2_MLSHR) {
			FAIL_IF(push_inst(compiler, ANDI | S(src2) | A(TMP_REG2) | ((flags & ALT_FORM2) ? 0x1f : 0x3f)));
			src2 = TMP_REG2;
		}

		return push_inst(compiler, ((flags & ALT_FORM2) ? SRW : SRD) | RC(flags) | S(src1) | A(dst) | B(src2));

	case SLJIT2_ASHR:
	case SLJIT2_MASHR:
		if (flags & ALT_FORM1) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			imm = compiler->imm;

			if (flags & ALT_FORM2) {
				imm &= 0x1f;
				return push_inst(compiler, SRAWI | RC(flags) | S(src1) | A(dst) | (imm << 11));
			}

			imm &= 0x3f;
			return push_inst(compiler, SRADI | RC(flags) | S(src1) | A(dst) | RLDI_SH(imm));
		}

		if (op == SLJIT2_MASHR) {
			FAIL_IF(push_inst(compiler, ANDI | S(src2) | A(TMP_REG2) | ((flags & ALT_FORM2) ? 0x1f : 0x3f)));
			src2 = TMP_REG2;
		}

		return push_inst(compiler, ((flags & ALT_FORM2) ? SRAW : SRAD) | RC(flags) | S(src1) | A(dst) | B(src2));

	case SLJIT2_ROTL:
	case SLJIT2_ROTR:
		if (flags & ALT_FORM1) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			imm = compiler->imm;

			if (op == SLJIT2_ROTR)
				imm = (sljit2_u32)(-(sljit2_s32)imm);

			if (flags & ALT_FORM2) {
				imm &= 0x1f;
				return push_inst(compiler, RLWINM | S(src1) | A(dst) | RLWI_SH(imm) | RLWI_MBE(0, 31));
			}

			imm &= 0x3f;
			return push_inst(compiler, RLDICL | S(src1) | A(dst) | RLDI_SH(imm));
		}

		if (op == SLJIT2_ROTR) {
			FAIL_IF(push_inst(compiler, SUBFIC | D(TMP_REG2) | A(src2) | 0));
			src2 = TMP_REG2;
		}

		return push_inst(compiler, ((flags & ALT_FORM2) ? (RLWNM | RLWI_MBE(0, 31)) : (RLDCL | RLDI_MB(0))) | S(src1) | A(dst) | B(src2));
	}

	SLJIT2_UNREACHABLE();
	return SLJIT2_SUCCESS;
}

static sljit2_s32 call_with_args(struct sljit2_compiler *compiler, sljit2_s32 arg_types, sljit2_s32 *src)
{
	sljit2_s32 arg_count = 0;
	sljit2_s32 word_arg_count = 0;
	sljit2_s32 types = 0;
	sljit2_s32 reg = 0;

	if (src)
		reg = *src & REG_MASK;

	arg_types >>= SLJIT2_ARG_SHIFT;

	while (arg_types) {
		types = (types << SLJIT2_ARG_SHIFT) | (arg_types & SLJIT2_ARG_MASK);

		switch (arg_types & SLJIT2_ARG_MASK) {
		case SLJIT2_ARG_TYPE_F64:
		case SLJIT2_ARG_TYPE_F32:
			arg_count++;
			break;
		default:
			arg_count++;
			word_arg_count++;

			if (arg_count != word_arg_count && arg_count == reg) {
				FAIL_IF(push_inst(compiler, OR | S(reg) | A(TMP_CALL_REG) | B(reg)));
				*src = TMP_CALL_REG;
			}
			break;
		}

		arg_types >>= SLJIT2_ARG_SHIFT;
	}

	while (types) {
		switch (types & SLJIT2_ARG_MASK) {
		case SLJIT2_ARG_TYPE_F64:
		case SLJIT2_ARG_TYPE_F32:
			arg_count--;
			break;
		default:
			if (arg_count != word_arg_count)
				FAIL_IF(push_inst(compiler, OR | S(word_arg_count) | A(arg_count) | B(word_arg_count)));

			arg_count--;
			word_arg_count--;
			break;
		}

		types >>= SLJIT2_ARG_SHIFT;
	}

	return SLJIT2_SUCCESS;
}

static SLJIT2_INLINE sljit2_s32 emit_const(struct sljit2_compiler *compiler, sljit2_s32 reg, sljit2_sw init_value)
{
	FAIL_IF(push_inst(compiler, ADDIS | D(reg) | A(0) | IMM(init_value >> 48)));
	FAIL_IF(push_inst(compiler, ORI | S(reg) | A(reg) | IMM(init_value >> 32)));
	FAIL_IF(push_inst(compiler, SLDI(32) | S(reg) | A(reg)));
	FAIL_IF(push_inst(compiler, ORIS | S(reg) | A(reg) | IMM(init_value >> 16)));
	return push_inst(compiler, ORI | S(reg) | A(reg) | IMM(init_value));
}

static SLJIT2_INLINE sljit2_s32 sljit2_emit_fop1_conv_f64_from_sw(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_s32 dst_r = FAST_IS_REG(dst) ? dst : TMP_FREG1;

	if (src == SLJIT2_IMM) {
		if (GET_OPCODE(op) == SLJIT2_CONV_F64_FROM_S32)
			srcw = (sljit2_s32)srcw;

		FAIL_IF(load_immediate(compiler, TMP_REG1, srcw));
		src = TMP_REG1;
	} else if (GET_OPCODE(op) == SLJIT2_CONV_F64_FROM_S32) {
		if (FAST_IS_REG(src))
			FAIL_IF(push_inst(compiler, EXTSW | S(src) | A(TMP_REG1)));
		else
			FAIL_IF(emit_op_mem(compiler, INT_DATA | SIGNED_DATA | LOAD_DATA, TMP_REG1, src, srcw, TMP_REG1));
		src = TMP_REG1;
	}

	if (FAST_IS_REG(src)) {
		FAIL_IF(push_inst(compiler, STD | S(src) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
		FAIL_IF(push_inst(compiler, LFD | FS(TMP_FREG1) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
	} else
		FAIL_IF(emit_op_mem(compiler, DOUBLE_DATA | LOAD_DATA, TMP_FREG1, src, srcw, TMP_REG1));

	FAIL_IF(push_inst(compiler, FCFID | FD(dst_r) | FB(TMP_FREG1)));

	if (op & SLJIT2_32)
		FAIL_IF(push_inst(compiler, FRSP | FD(dst_r) | FB(dst_r)));

	if (dst & SLJIT2_MEM)
		return emit_op_mem(compiler, FLOAT_DATA(op), TMP_FREG1, dst, dstw, TMP_REG1);
	return SLJIT2_SUCCESS;
}

static SLJIT2_INLINE sljit2_s32 sljit2_emit_fop1_conv_f64_from_uw(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_s32 dst_r = FAST_IS_REG(dst) ? dst : TMP_FREG1;

	if (GET_OPCODE(op) == SLJIT2_CONV_F64_FROM_U32) {
		if (src == SLJIT2_IMM) {
			FAIL_IF(load_immediate(compiler, TMP_REG1, (sljit2_u32)srcw));
			src = TMP_REG1;
		} else {
			if (FAST_IS_REG(src))
				FAIL_IF(push_inst(compiler, CLRLDI(TMP_REG1, src, 32)));
			else
				FAIL_IF(emit_op_mem(compiler, INT_DATA | LOAD_DATA, TMP_REG1, src, srcw, TMP_REG1));
			src = TMP_REG1;
		}

		FAIL_IF(push_inst(compiler, STD | S(src) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
		FAIL_IF(push_inst(compiler, LFD | FS(TMP_FREG1) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
		FAIL_IF(push_inst(compiler, FCFID | FD(dst_r) | FB(TMP_FREG1)));
	} else {
		if (src == SLJIT2_IMM) {
			FAIL_IF(load_immediate(compiler, TMP_REG1, srcw));
			src = TMP_REG1;
		} else if (src & SLJIT2_MEM) {
			FAIL_IF(emit_op_mem(compiler, WORD_DATA | LOAD_DATA, TMP_REG1, src, srcw, TMP_REG1));
			src = TMP_REG1;
		}

		FAIL_IF(push_inst(compiler, CMPI | CRD(0 | 1) | A(src) | 0));
		FAIL_IF(push_inst(compiler, BCx | (12 << 21) | (0 << 16) | 20));
		FAIL_IF(push_inst(compiler, STD | S(src) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
		FAIL_IF(push_inst(compiler, LFD | FS(TMP_FREG1) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
		FAIL_IF(push_inst(compiler, FCFID | FD(dst_r) | FB(TMP_FREG1)));
		FAIL_IF(push_inst(compiler, Bx | ((op & SLJIT2_32) ? 36 : 32)));

		if (op & SLJIT2_32)
			FAIL_IF(push_inst(compiler, RLWINM | S(src) | A(TMP_REG2) | RLWI_SH(10) | RLWI_MBE(10, 21)));
		else
			FAIL_IF(push_inst(compiler, ANDI | S(src) | A(TMP_REG2) | 0x1));

		/* Shift right. */
		FAIL_IF(push_inst(compiler, RLDICL | S(src) | A(TMP_REG1) | RLDI_SH(63) | RLDI_MB(1)));

		if (op & SLJIT2_32)
			FAIL_IF(push_inst(compiler, RLDICR | S(TMP_REG1) | A(TMP_REG1) | RLDI_SH(0) | RLDI_ME(53)));

		FAIL_IF(push_inst(compiler, OR | S(TMP_REG1) | A(TMP_REG1) | B(TMP_REG2)));

		FAIL_IF(push_inst(compiler, STD | S(TMP_REG1) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
		FAIL_IF(push_inst(compiler, LFD | FS(TMP_FREG1) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
		FAIL_IF(push_inst(compiler, FCFID | FD(dst_r) | FB(TMP_FREG1)));
		FAIL_IF(push_inst(compiler, FADD | FD(dst_r) | FA(dst_r) | FB(dst_r)));
	}

	if (op & SLJIT2_32)
		FAIL_IF(push_inst(compiler, FRSP | FD(dst_r) | FB(dst_r)));

	if (dst & SLJIT2_MEM)
		return emit_op_mem(compiler, FLOAT_DATA(op), TMP_FREG1, dst, dstw, TMP_REG1);
	return SLJIT2_SUCCESS;
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

	if (u.imm != 0)
		FAIL_IF(load_immediate(compiler, TMP_REG1, u.imm));

	FAIL_IF(push_inst(compiler, STD | S(u.imm != 0 ? TMP_REG1 : TMP_ZERO) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
	return push_inst(compiler, LFD | FS(freg) | A(SLJIT2_SP) | TMP_MEM_OFFSET);
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_fcopy(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 freg, sljit2_s32 reg)
{
	CHECK_ERROR();
	CHECK(check_sljit2_emit_fcopy(compiler, op, freg, reg));

	if (GET_OPCODE(op) == SLJIT2_COPY_TO_F64) {
		FAIL_IF(push_inst(compiler, ((op & SLJIT2_32) ? STW : STD) | S(reg) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
		return push_inst(compiler, ((op & SLJIT2_32) ? LFS : LFD) | FS(freg) | A(SLJIT2_SP) | TMP_MEM_OFFSET);
	}

	FAIL_IF(push_inst(compiler, ((op & SLJIT2_32) ? STFS : STFD) | FS(freg) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
	return push_inst(compiler, ((op & SLJIT2_32) ? LWZ : LD) | S(reg) | A(SLJIT2_SP) | TMP_MEM_OFFSET);
}

SLJIT2_API_FUNC_ATTRIBUTE void sljit2_set_jump_addr(sljit2_uw addr, sljit2_uw new_target, sljit2_sw executable_offset)
{
	sljit2_ins *inst = (sljit2_ins*)addr;
	SLJIT2_UNUSED_ARG(executable_offset);

	SLJIT2_UPDATE_WX_FLAGS(inst, inst + 5, 0);
	inst[0] = (inst[0] & 0xffff0000u) | ((sljit2_ins)(new_target >> 48) & 0xffff);
	inst[1] = (inst[1] & 0xffff0000u) | ((sljit2_ins)(new_target >> 32) & 0xffff);
	inst[3] = (inst[3] & 0xffff0000u) | ((sljit2_ins)(new_target >> 16) & 0xffff);
	inst[4] = (inst[4] & 0xffff0000u) | ((sljit2_ins)new_target & 0xffff);
	SLJIT2_UPDATE_WX_FLAGS(inst, inst + 5, 1);
	inst = (sljit2_ins *)SLJIT2_ADD_EXEC_OFFSET(inst, executable_offset);
	SLJIT2_CACHE_FLUSH(inst, inst + 5);
}
