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

static sljit2_s32 load_immediate(struct sljit2_compiler *compiler, sljit2_s32 dst_r, sljit2_sw imm, sljit2_s32 tmp_r)
{
	SLJIT2_UNUSED_ARG(tmp_r);

	if (imm <= SIMM_MAX && imm >= SIMM_MIN)
		return push_inst(compiler, ADDI | RD(dst_r) | RS1(TMP_ZERO) | IMM_I(imm));

	if (imm & 0x800)
		imm += 0x1000;

	FAIL_IF(push_inst(compiler, LUI | RD(dst_r) | (sljit2_ins)(imm & ~0xfff)));

	if ((imm & 0xfff) == 0)
		return SLJIT2_SUCCESS;

	return push_inst(compiler, ADDI | RD(dst_r) | RS1(dst_r) | IMM_I(imm));
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
		FAIL_IF(load_immediate(compiler, TMP_REG1, u.imm[0], TMP_REG3));
	if (u.imm[1] != 0)
		FAIL_IF(load_immediate(compiler, TMP_REG2, u.imm[1], TMP_REG3));

	FAIL_IF(push_inst(compiler, ADDI | RD(SLJIT2_SP) | RS1(SLJIT2_SP) | IMM_I(-16)));
	FAIL_IF(push_inst(compiler, SW | RS1(SLJIT2_SP) | RS2(u.imm[0] != 0 ? TMP_REG1 : TMP_ZERO) | (8 << 7)));
	FAIL_IF(push_inst(compiler, SW | RS1(SLJIT2_SP) | RS2(u.imm[1] != 0 ? TMP_REG2 : TMP_ZERO) | (12 << 7)));
	FAIL_IF(push_inst(compiler, FLD | FRD(freg) | RS1(SLJIT2_SP) | IMM_I(8)));
	return push_inst(compiler, ADDI | RD(SLJIT2_SP) | RS1(SLJIT2_SP) | IMM_I(16));
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_emit_fcopy(struct sljit2_compiler *compiler, sljit2_s32 op,
	sljit2_s32 freg, sljit2_s32 reg)
{
	sljit2_ins inst;
	sljit2_s32 reg2 = 0;

	CHECK_ERROR();
	CHECK(check_sljit2_emit_fcopy(compiler, op, freg, reg));

	if (op & SLJIT2_32) {
		if (op == SLJIT2_COPY32_TO_F32)
			inst = FMV_W_X | RS1(reg) | FRD(freg);
		else
			inst = FMV_X_W | FRS1(freg) | RD(reg);

		return push_inst(compiler, inst);
	}

	FAIL_IF(push_inst(compiler, ADDI | RD(SLJIT2_SP) | RS1(SLJIT2_SP) | IMM_I(-16)));

	if (reg & REG_PAIR_MASK) {
		reg2 = REG_PAIR_SECOND(reg);
		reg = REG_PAIR_FIRST(reg);
	}

	if (op == SLJIT2_COPY_TO_F64) {
		if (reg2 != 0)
			FAIL_IF(push_inst(compiler, SW | RS1(SLJIT2_SP) | RS2(reg2) | (8 << 7)));
		else
			FAIL_IF(push_inst(compiler, FSW | RS1(SLJIT2_SP) | FRS2(freg) | (8 << 7)));

		FAIL_IF(push_inst(compiler, SW | RS1(SLJIT2_SP) | RS2(reg) | (12 << 7)));
		FAIL_IF(push_inst(compiler, FLD | FRD(freg) | RS1(SLJIT2_SP) | IMM_I(8)));
	} else {
		FAIL_IF(push_inst(compiler, FSD | RS1(SLJIT2_SP) | FRS2(freg) | (8 << 7)));

		if (reg2 != 0)
			FAIL_IF(push_inst(compiler, FMV_X_W | FRS1(freg) | RD(reg2)));

		FAIL_IF(push_inst(compiler, LW | RD(reg) | RS1(SLJIT2_SP) | IMM_I(12)));
	}

	return push_inst(compiler, ADDI | RD(SLJIT2_SP) | RS1(SLJIT2_SP) | IMM_I(16));
}

static SLJIT2_INLINE sljit2_s32 emit_const(struct sljit2_compiler *compiler, sljit2_s32 dst, sljit2_sw init_value, sljit2_ins last_ins)
{
	if ((init_value & 0x800) != 0)
		init_value += 0x1000;

	FAIL_IF(push_inst(compiler, LUI | RD(dst) | (sljit2_ins)(init_value & ~0xfff)));
	return push_inst(compiler, last_ins | RS1(dst) | IMM_I(init_value));
}

SLJIT2_API_FUNC_ATTRIBUTE void sljit2_set_jump_addr(sljit2_uw addr, sljit2_uw new_target, sljit2_sw executable_offset)
{
	sljit2_ins *inst = (sljit2_ins*)addr;
	SLJIT2_UNUSED_ARG(executable_offset);

	if ((new_target & 0x800) != 0)
		new_target += 0x1000;

	SLJIT2_UPDATE_WX_FLAGS(inst, inst + 5, 0);

	SLJIT2_ASSERT((inst[0] & 0x7f) == LUI);
	inst[0] = (inst[0] & 0xfff) | (sljit2_ins)((sljit2_sw)new_target & ~0xfff);
	SLJIT2_ASSERT((inst[1] & 0x707f) == ADDI || (inst[1] & 0x707f) == JALR);
	inst[1] = (inst[1] & 0xfffff) | IMM_I(new_target);

	SLJIT2_UPDATE_WX_FLAGS(inst, inst + 5, 1);
	inst = (sljit2_ins *)SLJIT2_ADD_EXEC_OFFSET(inst, executable_offset);
	SLJIT2_CACHE_FLUSH(inst, inst + 5);
}
