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

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_jump_has_label(struct sljit2_jump *jump)
{
	return !(jump->flags & JUMP_ADDR) && (jump->u.label != NULL);
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_jump_has_target(struct sljit2_jump *jump)
{
	return (jump->flags & JUMP_ADDR) != 0;
}

SLJIT2_API_FUNC_ATTRIBUTE sljit2_s32 sljit2_jump_is_mov_addr(struct sljit2_jump *jump)
{
	return (jump->flags & JUMP_MOV_ADDR) != 0;
}

#define SLJIT2_SERIALIZE_DEBUG ((sljit2_u16)0x1)

struct sljit2_serialized_compiler {
	sljit2_u32 signature;
	sljit2_u16 version;
	sljit2_u16 cpu_type;

	sljit2_uw buf_segment_count;
	sljit2_uw label_count;
	sljit2_uw jump_count;
	sljit2_uw const_count;

	sljit2_s32 options;
	sljit2_s32 scratches;
	sljit2_s32 saveds;
	sljit2_s32 fscratches;
	sljit2_s32 fsaveds;
	sljit2_s32 local_size;
	sljit2_uw size;

#if (defined SLJIT2_HAS_STATUS_FLAGS_STATE && SLJIT2_HAS_STATUS_FLAGS_STATE)
	sljit2_s32 status_flags_state;
#endif /* SLJIT2_HAS_STATUS_FLAGS_STATE */

#if (defined SLJIT2_CONFIG_X86_32 && SLJIT2_CONFIG_X86_32)
	sljit2_s32 args_size;
#endif /* SLJIT2_CONFIG_X86_32 */

#if ((defined SLJIT2_CONFIG_ARM_32 && SLJIT2_CONFIG_ARM_32) && (defined __SOFTFP__)) \
		|| (defined SLJIT2_CONFIG_MIPS_32 && SLJIT2_CONFIG_MIPS_32)
	sljit2_uw args_size;
#endif /* (SLJIT2_CONFIG_ARM_32 && __SOFTFP__) || SLJIT2_CONFIG_MIPS_32 */

#if (defined SLJIT2_CONFIG_ARM_V6 && SLJIT2_CONFIG_ARM_V6)
	sljit2_uw cpool_diff;
	sljit2_uw cpool_fill;
	sljit2_uw patches;
#endif /* SLJIT2_CONFIG_ARM_V6 */

#if (defined SLJIT2_CONFIG_MIPS && SLJIT2_CONFIG_MIPS)
	sljit2_s32 delay_slot;
#endif /* SLJIT2_CONFIG_MIPS */

};

struct sljit2_serialized_debug_info {
	sljit2_sw last_flags;
	sljit2_s32 last_return;
	sljit2_s32 logical_local_size;
};

struct sljit2_serialized_label {
	sljit2_uw size;
};

struct sljit2_serialized_jump {
	sljit2_uw addr;
	sljit2_uw flags;
	sljit2_uw value;
};

struct sljit2_serialized_const {
	sljit2_uw addr;
};

#define SLJIT2_SERIALIZE_ALIGN(v) (((v) + sizeof(sljit2_uw) - 1) & ~(sljit2_uw)(sizeof(sljit2_uw) - 1))
#if (defined SLJIT2_LITTLE_ENDIAN && SLJIT2_LITTLE_ENDIAN)
#define SLJIT2_SERIALIZE_SIGNATURE 0x534c4a54
#else /* !SLJIT2_LITTLE_ENDIAN */
#define SLJIT2_SERIALIZE_SIGNATURE 0x544a4c53
#endif /* SLJIT2_LITTLE_ENDIAN */
#define SLJIT2_SERIALIZE_VERSION 1

SLJIT2_API_FUNC_ATTRIBUTE sljit2_uw* sljit2_serialize_compiler(struct sljit2_compiler *compiler,
	sljit2_s32 options, sljit2_uw *size)
{
	sljit2_uw serialized_size = sizeof(struct sljit2_serialized_compiler);
	struct sljit2_memory_fragment *buf;
	struct sljit2_label *label;
	struct sljit2_jump *jump;
	struct sljit2_const *const_;
	struct sljit2_serialized_compiler *serialized_compiler;
	struct sljit2_serialized_label *serialized_label;
	struct sljit2_serialized_jump *serialized_jump;
	struct sljit2_serialized_const *serialized_const;
#if (defined SLJIT2_ARGUMENT_CHECKS && SLJIT2_ARGUMENT_CHECKS) \
		|| (defined SLJIT2_DEBUG && SLJIT2_DEBUG)
	struct sljit2_serialized_debug_info *serialized_debug_info;
#endif /* SLJIT2_ARGUMENT_CHECKS || SLJIT2_DEBUG */
	sljit2_uw counter, used_size;
	sljit2_u8 *result;
	sljit2_u8 *ptr;
	SLJIT2_UNUSED_ARG(options);

	if (size != NULL)
		*size = 0;

	PTR_FAIL_IF(compiler->error);

#if (defined SLJIT2_ARGUMENT_CHECKS && SLJIT2_ARGUMENT_CHECKS) \
		|| (defined SLJIT2_DEBUG && SLJIT2_DEBUG)
	if (!(options & SLJIT2_SERIALIZE_IGNORE_DEBUG))
		serialized_size += sizeof(struct sljit2_serialized_debug_info);
#endif /* SLJIT2_ARGUMENT_CHECKS || SLJIT2_DEBUG */

#if (defined SLJIT2_CONFIG_ARM_V6 && SLJIT2_CONFIG_ARM_V6)
	serialized_size += SLJIT2_SERIALIZE_ALIGN(compiler->cpool_fill * (sizeof(sljit2_uw) + 1));
#endif /* SLJIT2_CONFIG_ARM_V6 */

	/* Compute the size of the data. */
	buf = compiler->buf;
	while (buf != NULL) {
		serialized_size += sizeof(sljit2_uw) + SLJIT2_SERIALIZE_ALIGN(buf->used_size);
		buf = buf->next;
	}

	serialized_size += compiler->label_count * sizeof(struct sljit2_serialized_label);

	jump = compiler->jumps;
	while (jump != NULL) {
		serialized_size += sizeof(struct sljit2_serialized_jump);
		jump = jump->next;
	}

	const_ = compiler->consts;
	while (const_ != NULL) {
		serialized_size += sizeof(struct sljit2_serialized_const);
		const_ = const_->next;
	}

	result = (sljit2_u8*)SLJIT2_MALLOC(serialized_size, compiler->allocator_data);
	PTR_FAIL_IF_NULL(result);

	if (size != NULL)
		*size = serialized_size;

	ptr = result;
	serialized_compiler = (struct sljit2_serialized_compiler*)ptr;
	ptr += sizeof(struct sljit2_serialized_compiler);

	serialized_compiler->signature = SLJIT2_SERIALIZE_SIGNATURE;
	serialized_compiler->version = SLJIT2_SERIALIZE_VERSION;
	serialized_compiler->cpu_type = 0;
	serialized_compiler->label_count = compiler->label_count;
	serialized_compiler->options = compiler->options;
	serialized_compiler->scratches = compiler->scratches;
	serialized_compiler->saveds = compiler->saveds;
	serialized_compiler->fscratches = compiler->fscratches;
	serialized_compiler->fsaveds = compiler->fsaveds;
	serialized_compiler->local_size = compiler->local_size;
	serialized_compiler->size = compiler->size;

#if (defined SLJIT2_HAS_STATUS_FLAGS_STATE && SLJIT2_HAS_STATUS_FLAGS_STATE)
	serialized_compiler->status_flags_state = compiler->status_flags_state;
#endif /* SLJIT2_HAS_STATUS_FLAGS_STATE */

#if (defined SLJIT2_CONFIG_X86_32 && SLJIT2_CONFIG_X86_32) \
		|| ((defined SLJIT2_CONFIG_ARM_32 && SLJIT2_CONFIG_ARM_32) && (defined __SOFTFP__)) \
		|| (defined SLJIT2_CONFIG_MIPS_32 && SLJIT2_CONFIG_MIPS_32)
	serialized_compiler->args_size = compiler->args_size;
#endif /* SLJIT2_CONFIG_X86_32 || (SLJIT2_CONFIG_ARM_32 && __SOFTFP__) || SLJIT2_CONFIG_MIPS_32 */

#if (defined SLJIT2_CONFIG_ARM_V6 && SLJIT2_CONFIG_ARM_V6)
	serialized_compiler->cpool_diff = compiler->cpool_diff;
	serialized_compiler->cpool_fill = compiler->cpool_fill;
	serialized_compiler->patches = compiler->patches;

	SLJIT2_MEMCPY(ptr, compiler->cpool, compiler->cpool_fill * sizeof(sljit2_uw));
	SLJIT2_MEMCPY(ptr + compiler->cpool_fill * sizeof(sljit2_uw), compiler->cpool_unique, compiler->cpool_fill);
	ptr += SLJIT2_SERIALIZE_ALIGN(compiler->cpool_fill * (sizeof(sljit2_uw) + 1));
#endif /* SLJIT2_CONFIG_ARM_V6 */

#if (defined SLJIT2_CONFIG_MIPS && SLJIT2_CONFIG_MIPS)
	serialized_compiler->delay_slot = compiler->delay_slot;
#endif /* SLJIT2_CONFIG_MIPS */

	buf = compiler->buf;
	counter = 0;
	while (buf != NULL) {
		used_size = buf->used_size;
		*(sljit2_uw*)ptr = used_size;
		ptr += sizeof(sljit2_uw);
		SLJIT2_MEMCPY(ptr, buf->memory, used_size);
		ptr += SLJIT2_SERIALIZE_ALIGN(used_size);
		buf = buf->next;
		counter++;
	}
	serialized_compiler->buf_segment_count = counter;

	label = compiler->labels;
	while (label != NULL) {
		serialized_label = (struct sljit2_serialized_label*)ptr;
		serialized_label->size = label->size;
		ptr += sizeof(struct sljit2_serialized_label);
		label = label->next;
	}

	jump = compiler->jumps;
	counter = 0;
	while (jump != NULL) {
		serialized_jump = (struct sljit2_serialized_jump*)ptr;
		serialized_jump->addr = jump->addr;
		serialized_jump->flags = jump->flags;

		if (jump->flags & JUMP_ADDR)
			serialized_jump->value = jump->u.target;
		else if (jump->u.label != NULL)
			serialized_jump->value = jump->u.label->u.index;
		else
			serialized_jump->value = SLJIT2_MAX_ADDRESS;

		ptr += sizeof(struct sljit2_serialized_jump);
		jump = jump->next;
		counter++;
	}
	serialized_compiler->jump_count = counter;

	const_ = compiler->consts;
	counter = 0;
	while (const_ != NULL) {
		serialized_const = (struct sljit2_serialized_const*)ptr;
		serialized_const->addr = const_->addr;
		ptr += sizeof(struct sljit2_serialized_const);
		const_ = const_->next;
		counter++;
	}
	serialized_compiler->const_count = counter;

#if (defined SLJIT2_ARGUMENT_CHECKS && SLJIT2_ARGUMENT_CHECKS) \
		|| (defined SLJIT2_DEBUG && SLJIT2_DEBUG)
	if (!(options & SLJIT2_SERIALIZE_IGNORE_DEBUG)) {
		serialized_debug_info = (struct sljit2_serialized_debug_info*)ptr;
		serialized_debug_info->last_flags = compiler->last_flags;
		serialized_debug_info->last_return = compiler->last_return;
		serialized_debug_info->logical_local_size = compiler->logical_local_size;
		serialized_compiler->cpu_type |= SLJIT2_SERIALIZE_DEBUG;
#if (defined SLJIT2_DEBUG && SLJIT2_DEBUG)
		ptr += sizeof(struct sljit2_serialized_debug_info);
#endif /* SLJIT2_DEBUG */
	}
#endif /* SLJIT2_ARGUMENT_CHECKS || SLJIT2_DEBUG */

	SLJIT2_ASSERT((sljit2_uw)(ptr - result) == serialized_size);
	return (sljit2_uw*)result;
}

SLJIT2_API_FUNC_ATTRIBUTE struct sljit2_compiler *sljit2_deserialize_compiler(sljit2_uw* buffer, sljit2_uw size,
	sljit2_s32 options, void *allocator_data)
{
	struct sljit2_compiler *compiler;
	struct sljit2_serialized_compiler *serialized_compiler;
	struct sljit2_serialized_label *serialized_label;
	struct sljit2_serialized_jump *serialized_jump;
	struct sljit2_serialized_const *serialized_const;
#if (defined SLJIT2_ARGUMENT_CHECKS && SLJIT2_ARGUMENT_CHECKS) \
		|| (defined SLJIT2_DEBUG && SLJIT2_DEBUG)
	struct sljit2_serialized_debug_info *serialized_debug_info;
#endif /* SLJIT2_ARGUMENT_CHECKS || SLJIT2_DEBUG */
	struct sljit2_memory_fragment *buf;
	struct sljit2_memory_fragment *last_buf;
	struct sljit2_label *label;
	struct sljit2_label *last_label;
	struct sljit2_label **label_list = NULL;
	struct sljit2_jump *jump;
	struct sljit2_jump *last_jump;
	struct sljit2_const *const_;
	struct sljit2_const *last_const;
	sljit2_u8 *ptr = (sljit2_u8*)buffer;
	sljit2_u8 *end = ptr + size;
	sljit2_uw i, used_size, aligned_size, label_count;
	SLJIT2_UNUSED_ARG(options);

	if (size < sizeof(struct sljit2_serialized_compiler) || (size & (sizeof(sljit2_uw) - 1)) != 0)
		return NULL;

	serialized_compiler = (struct sljit2_serialized_compiler*)ptr;

	if (serialized_compiler->signature != SLJIT2_SERIALIZE_SIGNATURE || serialized_compiler->version != SLJIT2_SERIALIZE_VERSION)
		return NULL;

	compiler = sljit2_create_compiler(allocator_data);
	PTR_FAIL_IF(compiler == NULL);

	compiler->label_count = serialized_compiler->label_count;
	compiler->options = serialized_compiler->options;
	compiler->scratches = serialized_compiler->scratches;
	compiler->saveds = serialized_compiler->saveds;
	compiler->fscratches = serialized_compiler->fscratches;
	compiler->fsaveds = serialized_compiler->fsaveds;
	compiler->local_size = serialized_compiler->local_size;
	compiler->size = serialized_compiler->size;

#if (defined SLJIT2_HAS_STATUS_FLAGS_STATE && SLJIT2_HAS_STATUS_FLAGS_STATE)
	compiler->status_flags_state = serialized_compiler->status_flags_state;
#endif /* SLJIT2_HAS_STATUS_FLAGS_STATE */

#if (defined SLJIT2_CONFIG_X86_32 && SLJIT2_CONFIG_X86_32) \
		|| ((defined SLJIT2_CONFIG_ARM_32 && SLJIT2_CONFIG_ARM_32) && (defined __SOFTFP__)) \
		|| (defined SLJIT2_CONFIG_MIPS_32 && SLJIT2_CONFIG_MIPS_32)
	compiler->args_size = serialized_compiler->args_size;
#endif /* SLJIT2_CONFIG_X86_32 || (SLJIT2_CONFIG_ARM_32 && __SOFTFP__) || SLJIT2_CONFIG_MIPS_32 */

#if (defined SLJIT2_CONFIG_ARM_V6 && SLJIT2_CONFIG_ARM_V6)
	used_size = serialized_compiler->cpool_fill;
	aligned_size = SLJIT2_SERIALIZE_ALIGN(used_size * (sizeof(sljit2_uw) + 1));
	compiler->cpool_diff = serialized_compiler->cpool_diff;
	compiler->cpool_fill = used_size;
	compiler->patches = serialized_compiler->patches;

	if ((sljit2_uw)(end - ptr) < aligned_size)
		goto error;

	SLJIT2_MEMCPY(compiler->cpool, ptr, used_size * sizeof(sljit2_uw));
	SLJIT2_MEMCPY(compiler->cpool_unique, ptr + used_size * sizeof(sljit2_uw), used_size);
	ptr += aligned_size;
#endif /* SLJIT2_CONFIG_ARM_V6 */

#if (defined SLJIT2_CONFIG_MIPS && SLJIT2_CONFIG_MIPS)
	compiler->delay_slot = serialized_compiler->delay_slot;
#endif /* SLJIT2_CONFIG_MIPS */

#if (defined SLJIT2_ARGUMENT_CHECKS && SLJIT2_ARGUMENT_CHECKS) \
		|| (defined SLJIT2_DEBUG && SLJIT2_DEBUG)
	if (!(serialized_compiler->cpu_type & SLJIT2_SERIALIZE_DEBUG))
		goto error;
#endif /* SLJIT2_ARGUMENT_CHECKS || SLJIT2_DEBUG */

	ptr += sizeof(struct sljit2_serialized_compiler);
	i = serialized_compiler->buf_segment_count;
	last_buf = NULL;
	while (i > 0) {
		if ((sljit2_uw)(end - ptr) < sizeof(sljit2_uw))
			goto error;

		used_size = *(sljit2_uw*)ptr;
		aligned_size = SLJIT2_SERIALIZE_ALIGN(used_size);
		ptr += sizeof(sljit2_uw);

		if ((sljit2_uw)(end - ptr) < aligned_size)
			goto error;

		if (last_buf == NULL) {
			SLJIT2_ASSERT(compiler->buf != NULL && compiler->buf->next == NULL);
			buf = compiler->buf;
		} else {
			buf = (struct sljit2_memory_fragment*)SLJIT2_MALLOC(BUF_SIZE, allocator_data);
			if (!buf)
				goto error;
			buf->next = NULL;
		}

		buf->used_size = used_size;
		SLJIT2_MEMCPY(buf->memory, ptr, used_size);

		if (last_buf != NULL)
			last_buf->next = buf;
		last_buf = buf;

		ptr += aligned_size;
		i--;
	}

	last_label = NULL;
	label_count = serialized_compiler->label_count;
	if ((sljit2_uw)(end - ptr) < label_count * sizeof(struct sljit2_serialized_label))
		goto error;

	label_list = (struct sljit2_label **)SLJIT2_MALLOC(label_count * sizeof(struct sljit2_label*), allocator_data);
	if (label_list == NULL)
		goto error;

	for (i = 0; i < label_count; i++) {
		label = (struct sljit2_label*)ensure_abuf(compiler, sizeof(struct sljit2_label));
		if (label == NULL)
			goto error;

		serialized_label = (struct sljit2_serialized_label*)ptr;
		label->next = NULL;
		label->u.index = i;
		label->size = serialized_label->size;

		if (last_label != NULL)
			last_label->next = label;
		else
			compiler->labels = label;
		last_label = label;

		label_list[i] = label;
		ptr += sizeof(struct sljit2_serialized_label);
	}
	compiler->last_label = last_label;

	last_jump = NULL;
	i = serialized_compiler->jump_count;
	if ((sljit2_uw)(end - ptr) < i * sizeof(struct sljit2_serialized_jump))
		goto error;

	while (i > 0) {
		jump = (struct sljit2_jump*)ensure_abuf(compiler, sizeof(struct sljit2_jump));
		if (jump == NULL)
			goto error;

		serialized_jump = (struct sljit2_serialized_jump*)ptr;
		jump->next = NULL;
		jump->addr = serialized_jump->addr;
		jump->flags = serialized_jump->flags;

		if (!(serialized_jump->flags & JUMP_ADDR)) {
			if (serialized_jump->value != SLJIT2_MAX_ADDRESS) {
				if (serialized_jump->value >= label_count)
					goto error;
				jump->u.label = label_list[serialized_jump->value];
			} else
				jump->u.label = NULL;
		} else
			jump->u.target = serialized_jump->value;

		if (last_jump != NULL)
			last_jump->next = jump;
		else
			compiler->jumps = jump;
		last_jump = jump;

		ptr += sizeof(struct sljit2_serialized_jump);
		i--;
	}
	compiler->last_jump = last_jump;

	SLJIT2_FREE(label_list, allocator_data);
	label_list = NULL;

	last_const = NULL;
	i = serialized_compiler->const_count;
	if ((sljit2_uw)(end - ptr) < i * sizeof(struct sljit2_serialized_const))
		goto error;

	while (i > 0) {
		const_ = (struct sljit2_const*)ensure_abuf(compiler, sizeof(struct sljit2_const));
		if (const_ == NULL)
			goto error;

		serialized_const = (struct sljit2_serialized_const*)ptr;
		const_->next = NULL;
		const_->addr = serialized_const->addr;

		if (last_const != NULL)
			last_const->next = const_;
		else
			compiler->consts = const_;
		last_const = const_;

		ptr += sizeof(struct sljit2_serialized_const);
		i--;
	}
	compiler->last_const = last_const;

#if (defined SLJIT2_ARGUMENT_CHECKS && SLJIT2_ARGUMENT_CHECKS) \
		|| (defined SLJIT2_DEBUG && SLJIT2_DEBUG)
	if ((sljit2_uw)(end - ptr) < sizeof(struct sljit2_serialized_debug_info))
		goto error;

	serialized_debug_info = (struct sljit2_serialized_debug_info*)ptr;
	compiler->last_flags = (sljit2_s32)serialized_debug_info->last_flags;
	compiler->last_return = serialized_debug_info->last_return;
	compiler->logical_local_size = serialized_debug_info->logical_local_size;
#endif /* SLJIT2_ARGUMENT_CHECKS || SLJIT2_DEBUG */

	return compiler;

error:
	sljit2_free_compiler(compiler);
	if (label_list != NULL)
		SLJIT2_FREE(label_list, allocator_data);
	return NULL;
}
