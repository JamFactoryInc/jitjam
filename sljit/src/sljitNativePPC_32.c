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

/* ppc 32-bit arch dependent functions. */

static sljit2_s32 load_immediate(struct sljit2_compiler *compiler, sljit2_s32 reg, sljit2_sw imm)
{
	if (imm <= SIMM_MAX && imm >= SIMM_MIN)
		return push_inst(compiler, ADDI | D(reg) | A(0) | IMM(imm));

	if (!(imm & ~0xffff))
		return push_inst(compiler, ORI | S(TMP_ZERO) | A(reg) | IMM(imm));

	FAIL_IF(push_inst(compiler, ADDIS | D(reg) | A(0) | IMM(imm >> 16)));
	return (imm & 0xffff) ? push_inst(compiler, ORI | S(reg) | A(reg) | IMM(imm)) : SLJIT2_SUCCESS;
}

/* Simplified mnemonics: clrlwi. */
#define INS_CLEAR_LEFT(dst, src, from) \
	(RLWINM | S(src) | A(dst) | RLWI_MBE(from, 31))

static SLJIT2_INLINE sljit2_s32 emit_single_op(struct sljit2_compiler *compiler, sljit2_s32 op, sljit2_s32 flags,
	sljit2_s32 dst, sljit2_s32 src1, sljit2_s32 src2)
{
	sljit2_u32 imm;

	switch (op) {
	case SLJIT2_MOV:
	case SLJIT2_MOV_U32:
	case SLJIT2_MOV_S32:
	case SLJIT2_MOV_P:
		SLJIT2_ASSERT(src1 == TMP_REG1);
		if (dst != src2)
			return push_inst(compiler, OR | S(src2) | A(dst) | B(src2));
		return SLJIT2_SUCCESS;

	case SLJIT2_MOV_U8:
	case SLJIT2_MOV_S8:
		SLJIT2_ASSERT(src1 == TMP_REG1);
		if ((flags & (REG_DEST | REG2_SOURCE)) == (REG_DEST | REG2_SOURCE)) {
			if (op == SLJIT2_MOV_S8)
				return push_inst(compiler, EXTSB | S(src2) | A(dst));
			return push_inst(compiler, INS_CLEAR_LEFT(dst, src2, 24));
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
			return push_inst(compiler, INS_CLEAR_LEFT(dst, src2, 16));
		}
		else {
			SLJIT2_ASSERT(dst == src2);
		}
		return SLJIT2_SUCCESS;

	case SLJIT2_CLZ:
		SLJIT2_ASSERT(src1 == TMP_REG1);
		return push_inst(compiler, CNTLZW | S(src2) | A(dst));

	case SLJIT2_CTZ:
		SLJIT2_ASSERT(src1 == TMP_REG1);
		FAIL_IF(push_inst(compiler, NEG | D(TMP_REG1) | A(src2)));
		FAIL_IF(push_inst(compiler, AND | S(src2) | A(dst) | B(TMP_REG1)));
		FAIL_IF(push_inst(compiler, CNTLZW | S(dst) | A(dst)));
		FAIL_IF(push_inst(compiler, ADDI | D(TMP_REG1) | A(dst) | IMM(-32)));
		/* The highest bits are set, if dst < 32, zero otherwise. */
		FAIL_IF(push_inst(compiler, SRWI(27) | S(TMP_REG1) | A(TMP_REG1)));
		return push_inst(compiler, XOR | S(dst) | A(dst) | B(TMP_REG1));

	case SLJIT2_ADD:
		if (flags & ALT_FORM1) {
			/* Setting XER SO is not enough, CR SO is also needed. */
			return push_inst(compiler, ADD | OE(ALT_SET_FLAGS) | RC(ALT_SET_FLAGS) | D(dst) | A(src1) | B(src2));
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
			return push_inst(compiler, ADDIC | D(dst) | A(src1) | compiler->imm);
		}
		SLJIT2_ASSERT(!(flags & ALT_FORM4));
		if (!(flags & ALT_SET_FLAGS))
			return push_inst(compiler, ADD | D(dst) | A(src1) | B(src2));
		if (flags & ALT_FORM5)
			return push_inst(compiler, ADDC | RC(ALT_SET_FLAGS) | D(dst) | A(src1) | B(src2));
		return push_inst(compiler, ADD | RC(flags) | D(dst) | A(src1) | B(src2));

	case SLJIT2_ADDC:
		return push_inst(compiler, ADDE | D(dst) | A(src1) | B(src2));

	case SLJIT2_SUB:
		if (flags & ALT_FORM1) {
			if (flags & ALT_FORM2) {
				FAIL_IF(push_inst(compiler, CMPLI | CRD(0) | A(src1) | compiler->imm));
				if (!(flags & ALT_FORM3))
					return SLJIT2_SUCCESS;
				return push_inst(compiler, ADDI | D(dst) | A(src1) | (-compiler->imm & 0xffff));
			}
			FAIL_IF(push_inst(compiler, CMPL | CRD(0) | A(src1) | B(src2)));
			if (!(flags & ALT_FORM3))
				return SLJIT2_SUCCESS;
			return push_inst(compiler, SUBF | D(dst) | A(src2) | B(src1));
		}

		if (flags & ALT_FORM2) {
			if (flags & ALT_FORM3) {
				FAIL_IF(push_inst(compiler, CMPI | CRD(0) | A(src1) | compiler->imm));
				if (!(flags & ALT_FORM4))
					return SLJIT2_SUCCESS;
				return push_inst(compiler, ADDI | D(dst) | A(src1) | (-compiler->imm & 0xffff));
			}
			FAIL_IF(push_inst(compiler, CMP | CRD(0) | A(src1) | B(src2)));
			if (!(flags & ALT_FORM4))
				return SLJIT2_SUCCESS;
			return push_inst(compiler, SUBF | D(dst) | A(src2) | B(src1));
		}

		if (flags & ALT_FORM3) {
			/* Setting XER SO is not enough, CR SO is also needed. */
			if (src1 != TMP_ZERO)
				return push_inst(compiler, SUBF | OE(ALT_SET_FLAGS) | RC(ALT_SET_FLAGS) | D(dst) | A(src2) | B(src1));
			return push_inst(compiler, NEG | OE(ALT_SET_FLAGS) | RC(ALT_SET_FLAGS) | D(dst) | A(src2));
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

		if (flags & ALT_FORM5)
			return push_inst(compiler, SUBFC | RC(ALT_SET_FLAGS) | D(dst) | A(src2) | B(src1));

		if (src1 != TMP_ZERO)
			return push_inst(compiler, SUBF | RC(ALT_SET_FLAGS) | D(dst) | A(src2) | B(src1));
		return push_inst(compiler, NEG | RC(ALT_SET_FLAGS) | D(dst) | A(src2));

	case SLJIT2_SUBC:
		return push_inst(compiler, SUBFE | D(dst) | A(src2) | B(src1));

	case SLJIT2_MUL:
		if (flags & ALT_FORM1) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			return push_inst(compiler, MULLI | D(dst) | A(src1) | compiler->imm);
		}
		return push_inst(compiler, MULLW | OE(flags) | RC(flags) | D(dst) | A(src2) | B(src1));

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
			return push_inst(compiler, NOR | RC(flags) | S(src2) | A(dst) | B(src2));
		}
		return push_inst(compiler, XOR | RC(flags) | S(src1) | A(dst) | B(src2));

	case SLJIT2_SHL:
	case SLJIT2_MSHL:
		if (flags & ALT_FORM1) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			imm = compiler->imm & 0x1f;
			return push_inst(compiler, SLWI(imm) | RC(flags) | S(src1) | A(dst));
		}

		if (op == SLJIT2_MSHL) {
			FAIL_IF(push_inst(compiler, ANDI | S(src2) | A(TMP_REG2) | 0x1f));
			src2 = TMP_REG2;
		}

		return push_inst(compiler, SLW | RC(flags) | S(src1) | A(dst) | B(src2));

	case SLJIT2_LSHR:
	case SLJIT2_MLSHR:
		if (flags & ALT_FORM1) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			imm = compiler->imm & 0x1f;
			/* Since imm can be 0, SRWI() cannot be used. */
			return push_inst(compiler, RLWINM | RC(flags) | S(src1) | A(dst) | RLWI_SH((32 - imm) & 0x1f) | RLWI_MBE(imm, 31));
		}

		if (op == SLJIT2_MLSHR) {
			FAIL_IF(push_inst(compiler, ANDI | S(src2) | A(TMP_REG2) | 0x1f));
			src2 = TMP_REG2;
		}

		return push_inst(compiler, SRW | RC(flags) | S(src1) | A(dst) | B(src2));

	case SLJIT2_ASHR:
	case SLJIT2_MASHR:
		if (flags & ALT_FORM1) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			imm = compiler->imm & 0x1f;
			return push_inst(compiler, SRAWI | RC(flags) | S(src1) | A(dst) | (imm << 11));
		}

		if (op == SLJIT2_MASHR) {
			FAIL_IF(push_inst(compiler, ANDI | S(src2) | A(TMP_REG2) | 0x1f));
			src2 = TMP_REG2;
		}

		return push_inst(compiler, SRAW | RC(flags) | S(src1) | A(dst) | B(src2));

	case SLJIT2_ROTL:
	case SLJIT2_ROTR:
		if (flags & ALT_FORM1) {
			SLJIT2_ASSERT(src2 == TMP_REG2);
			imm = compiler->imm;

			if (op == SLJIT2_ROTR)
				imm = (sljit2_u32)(-(sljit2_s32)imm);

			imm &= 0x1f;
			return push_inst(compiler, RLWINM | S(src1) | A(dst) | RLWI_SH(imm) | RLWI_MBE(0, 31));
		}

		if (op == SLJIT2_ROTR) {
			FAIL_IF(push_inst(compiler, SUBFIC | D(TMP_REG2) | A(src2) | 0));
			src2 = TMP_REG2;
		}

		return push_inst(compiler, RLWNM | S(src1) | A(dst) | B(src2) | RLWI_MBE(0, 31));
	}

	SLJIT2_UNREACHABLE();
	return SLJIT2_SUCCESS;
}

static SLJIT2_INLINE sljit2_s32 emit_const(struct sljit2_compiler *compiler, sljit2_s32 reg, sljit2_sw init_value)
{
	FAIL_IF(push_inst(compiler, ADDIS | D(reg) | A(0) | IMM(init_value >> 16)));
	return push_inst(compiler, ORI | S(reg) | A(reg) | IMM(init_value));
}

static SLJIT2_INLINE sljit2_s32 sljit2_emit_fop1_conv_f64_from_sw(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 dst, sljit2_sw dstw,
	sljit2_s32 src, sljit2_sw srcw)
{
	sljit2_s32 dst_r = FAST_IS_REG(dst) ? dst : TMP_FREG1;
	sljit2_s32 invert_sign = 1;

	if (src == SLJIT2_IMM) {
		FAIL_IF(load_immediate(compiler, TMP_REG1, srcw ^ (sljit2_sw)0x80000000));
		src = TMP_REG1;
		invert_sign = 0;
	} else if (!FAST_IS_REG(src)) {
		FAIL_IF(emit_op_mem(compiler, WORD_DATA | SIGNED_DATA | LOAD_DATA, TMP_REG1, src, srcw, TMP_REG1));
		src = TMP_REG1;
	}

	/* First, a special double precision floating point value is constructed:
	      (2^53 + (src xor (2^31)))
	   The upper 32 bits of this number is a constant, and the lower 32 bits
	   is simply the value of the source argument. The xor 2^31 operation adds
	   0x80000000 to the source argument, which moves it into the 0 - 0xffffffff
	   range. Finally we substract 2^53 + 2^31 to get the converted value. */
	FAIL_IF(push_inst(compiler, ADDIS | D(TMP_REG2) | A(0) | 0x4330));
	if (invert_sign)
		FAIL_IF(push_inst(compiler, XORIS | S(src) | A(TMP_REG1) | 0x8000));
	FAIL_IF(push_inst(compiler, STW | S(TMP_REG2) | A(SLJIT2_SP) | TMP_MEM_OFFSET_HI));
	FAIL_IF(push_inst(compiler, STW | S(TMP_REG1) | A(SLJIT2_SP) | TMP_MEM_OFFSET_LO));
	FAIL_IF(push_inst(compiler, ADDIS | D(TMP_REG1) | A(0) | 0x8000));
	FAIL_IF(push_inst(compiler, LFD | FS(TMP_FREG1) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
	FAIL_IF(push_inst(compiler, STW | S(TMP_REG1) | A(SLJIT2_SP) | TMP_MEM_OFFSET_LO));
	FAIL_IF(push_inst(compiler, LFD | FS(TMP_FREG2) | A(SLJIT2_SP) | TMP_MEM_OFFSET));

	FAIL_IF(push_inst(compiler, FSUB | FD(dst_r) | FA(TMP_FREG1) | FB(TMP_FREG2)));

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

	if (src == SLJIT2_IMM) {
		FAIL_IF(load_immediate(compiler, TMP_REG1, srcw));
		src = TMP_REG1;
	} else if (!FAST_IS_REG(src)) {
		FAIL_IF(emit_op_mem(compiler, WORD_DATA | SIGNED_DATA | LOAD_DATA, TMP_REG1, src, srcw, TMP_REG1));
		src = TMP_REG1;
	}

	/* First, a special double precision floating point value is constructed:
	      (2^53 + src)
	   The upper 32 bits of this number is a constant, and the lower 32 bits
	   is simply the value of the source argument. Finally we substract 2^53
	   to get the converted value. */
	FAIL_IF(push_inst(compiler, ADDIS | D(TMP_REG2) | A(0) | 0x4330));
	FAIL_IF(push_inst(compiler, STW | S(src) | A(SLJIT2_SP) | TMP_MEM_OFFSET_LO));
	FAIL_IF(push_inst(compiler, STW | S(TMP_REG2) | A(SLJIT2_SP) | TMP_MEM_OFFSET_HI));

	FAIL_IF(push_inst(compiler, LFD | FS(TMP_FREG1) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
	FAIL_IF(push_inst(compiler, STW | S(TMP_ZERO) | A(SLJIT2_SP) | TMP_MEM_OFFSET_LO));
	FAIL_IF(push_inst(compiler, LFD | FS(TMP_FREG2) | A(SLJIT2_SP) | TMP_MEM_OFFSET));

	FAIL_IF(push_inst(compiler, FSUB | FD(dst_r) | FA(TMP_FREG1) | FB(TMP_FREG2)));

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
		sljit2_s32 imm[2];
		sljit2_f64 value;
	} u;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_fset64(compiler, freg, value));

	u.value = value;

	if (u.imm[0] != 0)
		FAIL_IF(load_immediate(compiler, TMP_REG1, u.imm[0]));
	if (u.imm[1] != 0)
		FAIL_IF(load_immediate(compiler, TMP_REG2, u.imm[1]));

	/* Saved in the same endianness. */
	FAIL_IF(push_inst(compiler, STW | S(u.imm[0] != 0 ? TMP_REG1 : TMP_ZERO) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
	FAIL_IF(push_inst(compiler, STW | S(u.imm[1] != 0 ? TMP_REG2 : TMP_ZERO) | A(SLJIT2_SP) | (TMP_MEM_OFFSET + sizeof(sljit2_s32))));
	return push_inst(compiler, LFD | FS(freg) | A(SLJIT2_SP) | TMP_MEM_OFFSET);
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_fcopy(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 freg, sljit2_s32 reg)
{
	sljit2_s32 reg2 = 0;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_fcopy(compiler, op, freg, reg));

	if (op & SLJIT2_32) {
		if (op == SLJIT2_COPY32_TO_F32) {
			FAIL_IF(push_inst(compiler, STW | S(reg) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
			return push_inst(compiler, LFS | FS(freg) | A(SLJIT2_SP) | TMP_MEM_OFFSET);
		}

		FAIL_IF(push_inst(compiler, STFS | FS(freg) | A(SLJIT2_SP) | TMP_MEM_OFFSET));
		return push_inst(compiler, LWZ | S(reg) | A(SLJIT2_SP) | TMP_MEM_OFFSET);
	}

	if (reg & REG_PAIR_MASK) {
		reg2 = REG_PAIR_SECOND(reg);
		reg = REG_PAIR_FIRST(reg);
	}

	if (op == SLJIT2_COPY_TO_F64) {
		FAIL_IF(push_inst(compiler, STW | S(reg) | A(SLJIT2_SP) | TMP_MEM_OFFSET_HI));

		if (reg2 != 0)
			FAIL_IF(push_inst(compiler, STW | S(reg2) | A(SLJIT2_SP) | TMP_MEM_OFFSET_LO));
		else
			FAIL_IF(push_inst(compiler, STFD | FS(freg) | A(SLJIT2_SP) | TMP_MEM_OFFSET_LO));

		return push_inst(compiler, LFD | FS(freg) | A(SLJIT2_SP) | TMP_MEM_OFFSET);
	}

	FAIL_IF(push_inst(compiler, STFD | FS(freg) | A(SLJIT2_SP) | TMP_MEM_OFFSET));

	if (reg2 != 0)
		FAIL_IF(push_inst(compiler, LWZ | S(reg2) | A(SLJIT2_SP) | TMP_MEM_OFFSET_LO));

	return push_inst(compiler, LWZ | S(reg) | A(SLJIT2_SP) | TMP_MEM_OFFSET_HI);
}

SLJIT2_API_FUNC_ATTRIBUTE void sljit2_set_jump_addr(sljit2_uw addr, sljit2_uw new_target, sljit2_sw executable_offset)
{
	sljit2_ins *inst = (sljit2_ins *)addr;
	SLJIT2_UNUSED_ARG(executable_offset);

	SLJIT2_UPDATE_WX_FLAGS(inst, inst + 2, 0);
	SLJIT2_ASSERT((inst[0] & 0xfc1f0000) == ADDIS && (inst[1] & 0xfc000000) == ORI);
	inst[0] = (inst[0] & 0xffff0000) | ((new_target >> 16) & 0xffff);
	inst[1] = (inst[1] & 0xffff0000) | (new_target & 0xffff);
	SLJIT2_UPDATE_WX_FLAGS(inst, inst + 2, 1);
	inst = (sljit2_ins *)SLJIT2_ADD_EXEC_OFFSET(inst, executable_offset);
	SLJIT2_CACHE_FLUSH(inst, inst + 2);
}
