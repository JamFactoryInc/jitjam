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

#ifndef SLJIT2_CONFIG_INTERNAL_H_
#define SLJIT2_CONFIG_INTERNAL_H_

#if (defined SLJIT2_VERBOSE && SLJIT2_VERBOSE) \
	|| (defined SLJIT2_DEBUG && SLJIT2_DEBUG && (!defined(SLJIT2_ASSERT) || !defined(SLJIT2_UNREACHABLE)))
#include <stdio.h>
#endif

#if (defined SLJIT2_DEBUG && SLJIT2_DEBUG \
	&& (!defined(SLJIT2_ASSERT) || !defined(SLJIT2_UNREACHABLE) || !defined(SLJIT2_HALT_PROCESS)))
#include <stdlib.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
   SLJIT defines the following architecture dependent types and macros:

   Types:
     sljit2_s8, sljit2_u8   : signed and unsigned 8 bit integer type
     sljit2_s16, sljit2_u16 : signed and unsigned 16 bit integer type
     sljit2_s32, sljit2_u32 : signed and unsigned 32 bit integer type
     sljit2_sw, sljit2_uw   : signed and unsigned machine word, enough to store a pointer
     sljit2_sp, sljit2_up   : signed and unsigned pointer value (usually the same as
                            sljit2_uw, but some 64 bit ABIs may use 32 bit pointers)
     sljit2_f32            : 32 bit single precision floating point value
     sljit2_f64            : 64 bit double precision floating point value

   Macros for feature detection (boolean):
     SLJIT2_32BIT_ARCHITECTURE : 32 bit architecture
     SLJIT2_64BIT_ARCHITECTURE : 64 bit architecture
     SLJIT2_LITTLE_ENDIAN : little endian architecture
     SLJIT2_BIG_ENDIAN : big endian architecture
     SLJIT2_UNALIGNED : unaligned memory accesses for non-fpu operations are supported
     SLJIT2_FPU_UNALIGNED : unaligned memory accesses for fpu operations are supported
     SLJIT2_MASKED_SHIFT : all word shifts are always masked
     SLJIT2_MASKED_SHIFT32 : all 32 bit shifts are always masked
     SLJIT2_INDIRECT_CALL : see SLJIT2_FUNC_ADDR() for more information

   Constants:
     SLJIT2_NUMBER_OF_REGISTERS : number of available registers
     SLJIT2_NUMBER_OF_SCRATCH_REGISTERS : number of available scratch registers
     SLJIT2_NUMBER_OF_SAVED_REGISTERS : number of available saved registers
     SLJIT2_NUMBER_OF_FLOAT_REGISTERS : number of available floating point registers
     SLJIT2_NUMBER_OF_SCRATCH_FLOAT_REGISTERS : number of available scratch floating point registers
     SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS : number of available saved floating point registers
     SLJIT2_NUMBER_OF_VECTOR_REGISTERS : number of available vector registers
     SLJIT2_NUMBER_OF_SCRATCH_VECTOR_REGISTERS : number of available scratch vector registers
     SLJIT2_NUMBER_OF_SAVED_VECTOR_REGISTERS : number of available saved vector registers
     SLJIT2_NUMBER_OF_TEMPORARY_REGISTERS : number of available temporary registers
     SLJIT2_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS : number of available temporary floating point registers
     SLJIT2_NUMBER_OF_TEMPORARY_VECTOR_REGISTERS : number of available temporary vector registers
     SLJIT2_SEPARATE_VECTOR_REGISTERS : if this macro is defined, the vector registers do not
                                       overlap with floating point registers
     SLJIT2_WORD_SHIFT : the shift required to apply when accessing a sljit2_sw/sljit2_uw array by index
     SLJIT2_F32_SHIFT : the shift required to apply when accessing
                       a single precision floating point array by index
     SLJIT2_F64_SHIFT : the shift required to apply when accessing
                       a double precision floating point array by index
     SLJIT2_PREF_SHIFT_REG : x86 systems prefers ecx for shifting by register
                            the scratch register index of ecx is stored in this variable
     SLJIT2_LOCALS_OFFSET : local space starting offset (SLJIT2_SP + SLJIT2_LOCALS_OFFSET)
     SLJIT2_RETURN_ADDRESS_OFFSET : a return instruction always adds this offset to the return address
     SLJIT2_CONV_MAX_FLOAT : result when a floating point value is converted to integer
                            and the floating point value is higher than the maximum integer value
                            (possible values: SLJIT2_CONV_RESULT_MAX_INT or SLJIT2_CONV_RESULT_MIN_INT)
     SLJIT2_CONV_MIN_FLOAT : result when a floating point value is converted to integer
                            and the floating point value is lower than the minimum integer value
                            (possible values: SLJIT2_CONV_RESULT_MAX_INT or SLJIT2_CONV_RESULT_MIN_INT)
     SLJIT2_CONV_NAN_FLOAT : result when a NaN floating point value is converted to integer
                            (possible values: SLJIT2_CONV_RESULT_MAX_INT, SLJIT2_CONV_RESULT_MIN_INT,
                            or SLJIT2_CONV_RESULT_ZERO)

   Other macros:
     SLJIT2_TMP_R0 .. R9 : accessing temporary registers
     SLJIT2_TMP_R(i) : accessing temporary registers
     SLJIT2_TMP_FR0 .. FR9 : accessing temporary floating point registers
     SLJIT2_TMP_FR(i) : accessing temporary floating point registers
     SLJIT2_TMP_VR0 .. VR9 : accessing temporary vector registers
     SLJIT2_TMP_VR(i) : accessing temporary vector registers
     SLJIT2_TMP_DEST_REG : a temporary register for results
     SLJIT2_TMP_MEM_REG : a temporary base register for accessing memory
                         (can be the same as SLJIT2_TMP_DEST_REG)
     SLJIT2_TMP_DEST_FREG : a temporary register for float results
     SLJIT2_TMP_DEST_VREG : a temporary register for vector results
     SLJIT2_FUNC : calling convention attribute for both calling JIT from C and C calling back from JIT
     SLJIT2_W(number) : defining 64 bit constants on 64 bit architectures (platform independent helper)
     SLJIT2_F64_SECOND(reg) : provides the register index of the second 32 bit part of a 64 bit
                             floating point register when SLJIT2_HAS_F64_AS_F32_PAIR returns non-zero
*/

/***********************************************************/
/* Intel Control-flow Enforcement Technology (CET) spport. */
/***********************************************************/

#ifdef SLJIT2_CONFIG_X86

#if defined(__CET__) && !(defined SLJIT2_CONFIG_X86_CET && SLJIT2_CONFIG_X86_CET)
#define SLJIT2_CONFIG_X86_CET 1
#endif

#if (defined SLJIT2_CONFIG_X86_CET && SLJIT2_CONFIG_X86_CET) && defined(__GNUC__)
#include <x86intrin.h>
#endif

#endif /* SLJIT2_CONFIG_X86 */

/**********************************/
/* External function definitions. */
/**********************************/

/* General macros:
   Note: SLJIT is designed to be independent from them as possible.

   In release mode (SLJIT2_DEBUG is not defined) only the following
   external functions are needed:
*/

#ifndef SLJIT2_MALLOC
#define SLJIT2_MALLOC(size, allocator_data) (malloc(size))
#endif

#ifndef SLJIT2_FREE
#define SLJIT2_FREE(ptr, allocator_data) (free(ptr))
#endif

#ifndef SLJIT2_MEMCPY
#define SLJIT2_MEMCPY(dest, src, len) (memcpy(dest, src, len))
#endif

#ifndef SLJIT2_MEMMOVE
#define SLJIT2_MEMMOVE(dest, src, len) (memmove(dest, src, len))
#endif

#ifndef SLJIT2_ZEROMEM
#define SLJIT2_ZEROMEM(dest, len) (memset(dest, 0, len))
#endif

/***************************/
/* Compiler helper macros. */
/***************************/

#if !defined(SLJIT2_LIKELY) && !defined(SLJIT2_UNLIKELY)

#if defined(__GNUC__) && (__GNUC__ >= 3)
#define SLJIT2_LIKELY(x)		__builtin_expect((x), 1)
#define SLJIT2_UNLIKELY(x)	__builtin_expect((x), 0)
#else
#define SLJIT2_LIKELY(x)		(x)
#define SLJIT2_UNLIKELY(x)	(x)
#endif

#endif /* !defined(SLJIT2_LIKELY) && !defined(SLJIT2_UNLIKELY) */

#ifndef SLJIT2_INLINE
/* Inline functions. Some old compilers do not support them. */
#ifdef __SUNPRO_C
#if __SUNPRO_C < 0x560
#define SLJIT2_INLINE
#else
#define SLJIT2_INLINE inline
#endif /* __SUNPRO_C */
#else
#define SLJIT2_INLINE __inline
#endif
#endif /* !SLJIT2_INLINE */

#ifndef SLJIT2_NOINLINE
/* Not inline functions. */
#if defined(__GNUC__)
#define SLJIT2_NOINLINE __attribute__ ((noinline))
#else
#define SLJIT2_NOINLINE
#endif
#endif /* !SLJIT2_INLINE */

#ifndef SLJIT2_UNUSED_ARG
/* Unused arguments. */
#define SLJIT2_UNUSED_ARG(arg) (void)arg
#endif

/*********************************/
/* Type of public API functions. */
/*********************************/

#ifndef SLJIT2_API_FUNC_ATTRIBUTE
#if (defined SLJIT2_CONFIG_STATIC && SLJIT2_CONFIG_STATIC)
/* Static ABI functions. For all-in-one programs. */

#if defined(__GNUC__)
/* Disable unused warnings in gcc. */
#define SLJIT2_API_FUNC_ATTRIBUTE static __attribute__((unused))
#else
#define SLJIT2_API_FUNC_ATTRIBUTE static
#endif

#else
#define SLJIT2_API_FUNC_ATTRIBUTE
#endif /* (defined SLJIT2_CONFIG_STATIC && SLJIT2_CONFIG_STATIC) */
#endif /* defined SLJIT2_API_FUNC_ATTRIBUTE */

/****************************/
/* Instruction cache flush. */
/****************************/

#ifdef __APPLE__
#include <AvailabilityMacros.h>
#endif

/*
 * TODO:
 *
 * clang >= 15 could be safe to enable below
 * older versions are known to abort in some targets
 * https://github.com/PhilipHazel/pcre2/issues/92
 *
 * beware some vendors (ex: Microsoft, Apple) are known to have
 * removed the code to support this builtin even if the call for
 * __has_builtin reports it is available.
 *
 * make sure linking doesn't fail because __clear_cache() is
 * missing before changing it or add an exception so that the
 * system provided method that should be defined below is used
 * instead.
 */
#if (!defined SLJIT2_CACHE_FLUSH && defined __has_builtin)
#if __has_builtin(__builtin___clear_cache) && !defined(__clang__)

/*
 * https://gcc.gnu.org/bugzilla//show_bug.cgi?id=91248
 * https://gcc.gnu.org/bugzilla//show_bug.cgi?id=93811
 * gcc's clear_cache builtin for power is broken
 */
#if !defined(SLJIT2_CONFIG_PPC)
#define SLJIT2_CACHE_FLUSH(from, to) \
	__builtin___clear_cache((char*)(from), (char*)(to))
#endif

#endif /* gcc >= 10 */
#endif /* (!defined SLJIT2_CACHE_FLUSH && defined __has_builtin) */

#ifndef SLJIT2_CACHE_FLUSH

#if (defined SLJIT2_CONFIG_X86 && SLJIT2_CONFIG_X86) \
	|| (defined SLJIT2_CONFIG_S390X && SLJIT2_CONFIG_S390X)

/* Not required to implement on archs with unified caches. */
#define SLJIT2_CACHE_FLUSH(from, to)

#elif defined(__APPLE__) && MAC_OS_X_VERSION_MIN_REQUIRED >= 1050

/* Supported by all macs since Mac OS 10.5.
   However, it does not work on non-jailbroken iOS devices,
   although the compilation is successful. */
#include <libkern/OSCacheControl.h>
#define SLJIT2_CACHE_FLUSH(from, to) \
	sys_icache_invalidate((void*)(from), (size_t)((char*)(to) - (char*)(from)))

#elif (defined SLJIT2_CONFIG_PPC && SLJIT2_CONFIG_PPC)

/* The __clear_cache() implementation of GCC is a dummy function on PowerPC. */
#define SLJIT2_CACHE_FLUSH(from, to) \
	ppc_cache_flush((from), (to))
#define SLJIT2_CACHE_FLUSH_OWN_IMPL 1

#elif defined(_WIN32)

#define SLJIT2_CACHE_FLUSH(from, to) \
	FlushInstructionCache(GetCurrentProcess(), (void*)(from), (size_t)((char*)(to) - (char*)(from)))

#elif (defined(__GNUC__) && (__GNUC__ >= 5 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 3))) || defined(__clang__)

#define SLJIT2_CACHE_FLUSH(from, to) \
	__builtin___clear_cache((char*)(from), (char*)(to))

#elif defined __ANDROID__

/* Android ARMv7 with gcc lacks __clear_cache; use cacheflush instead. */
#include <sys/cachectl.h>
#define SLJIT2_CACHE_FLUSH(from, to) \
	cacheflush((long)(from), (long)(to), 0)

#else

/* Call __ARM_NR_cacheflush on ARM-Linux or the corresponding MIPS syscall. */
#define SLJIT2_CACHE_FLUSH(from, to) \
	__clear_cache((char*)(from), (char*)(to))

#endif

#endif /* !SLJIT2_CACHE_FLUSH */

/******************************************************/
/*    Integer and floating point type definitions.    */
/******************************************************/

/* 8 bit byte type. */
typedef unsigned char sljit2_u8;
typedef signed char sljit2_s8;

/* 16 bit half-word type. */
typedef unsigned short int sljit2_u16;
typedef signed short int sljit2_s16;

/* 32 bit integer type. */
typedef unsigned int sljit2_u32;
typedef signed int sljit2_s32;

/* Machine word type. Enough for storing a pointer.
     32 bit for 32 bit machines.
     64 bit for 64 bit machines. */
#if (defined SLJIT2_CONFIG_UNSUPPORTED && SLJIT2_CONFIG_UNSUPPORTED)
/* Just to have something. */
#define SLJIT2_WORD_SHIFT 0
typedef unsigned int sljit2_uw;
typedef int sljit2_sw;
#elif !(defined SLJIT2_CONFIG_X86_64 && SLJIT2_CONFIG_X86_64) \
	&& !(defined SLJIT2_CONFIG_ARM_64 && SLJIT2_CONFIG_ARM_64) \
	&& !(defined SLJIT2_CONFIG_PPC_64 && SLJIT2_CONFIG_PPC_64) \
	&& !(defined SLJIT2_CONFIG_MIPS_64 && SLJIT2_CONFIG_MIPS_64) \
	&& !(defined SLJIT2_CONFIG_RISCV_64 && SLJIT2_CONFIG_RISCV_64) \
	&& !(defined SLJIT2_CONFIG_S390X && SLJIT2_CONFIG_S390X) \
	&& !(defined SLJIT2_CONFIG_LOONGARCH_64 && SLJIT2_CONFIG_LOONGARCH_64)
#define SLJIT2_32BIT_ARCHITECTURE 1
#define SLJIT2_WORD_SHIFT 2
typedef unsigned int sljit2_uw;
typedef int sljit2_sw;
#else
#define SLJIT2_64BIT_ARCHITECTURE 1
#define SLJIT2_WORD_SHIFT 3
#ifdef _WIN32
#ifdef __GNUC__
/* These types do not require windows.h */
typedef unsigned long long sljit2_uw;
typedef long long sljit2_sw;
#else
typedef unsigned __int64 sljit2_uw;
typedef __int64 sljit2_sw;
#endif
#else /* !_WIN32 */
typedef unsigned long int sljit2_uw;
typedef long int sljit2_sw;
#endif /* _WIN32 */
#endif

typedef sljit2_sw sljit2_sp;
typedef sljit2_uw sljit2_up;

/* Floating point types. */
typedef float sljit2_f32;
typedef double sljit2_f64;

/* Shift for pointer sized data. */
#define SLJIT2_POINTER_SHIFT SLJIT2_WORD_SHIFT

/* Shift for double precision sized data. */
#define SLJIT2_F32_SHIFT 2
#define SLJIT2_F64_SHIFT 3

#define SLJIT2_CONV_RESULT_MAX_INT 0
#define SLJIT2_CONV_RESULT_MIN_INT 1
#define SLJIT2_CONV_RESULT_ZERO 2

#if (defined SLJIT2_CONFIG_X86 && SLJIT2_CONFIG_X86)
#define SLJIT2_CONV_MAX_FLOAT SLJIT2_CONV_RESULT_MIN_INT
#define SLJIT2_CONV_MIN_FLOAT SLJIT2_CONV_RESULT_MIN_INT
#define SLJIT2_CONV_NAN_FLOAT SLJIT2_CONV_RESULT_MIN_INT
#elif (defined SLJIT2_CONFIG_ARM && SLJIT2_CONFIG_ARM)
#define SLJIT2_CONV_MAX_FLOAT SLJIT2_CONV_RESULT_MAX_INT
#define SLJIT2_CONV_MIN_FLOAT SLJIT2_CONV_RESULT_MIN_INT
#define SLJIT2_CONV_NAN_FLOAT SLJIT2_CONV_RESULT_ZERO
#elif (defined SLJIT2_CONFIG_MIPS && SLJIT2_CONFIG_MIPS)
#define SLJIT2_CONV_MAX_FLOAT SLJIT2_CONV_RESULT_MAX_INT
#define SLJIT2_CONV_MIN_FLOAT SLJIT2_CONV_RESULT_MAX_INT
#define SLJIT2_CONV_NAN_FLOAT SLJIT2_CONV_RESULT_MAX_INT
#elif (defined SLJIT2_CONFIG_PPC && SLJIT2_CONFIG_PPC)
#define SLJIT2_CONV_MAX_FLOAT SLJIT2_CONV_RESULT_MAX_INT
#define SLJIT2_CONV_MIN_FLOAT SLJIT2_CONV_RESULT_MIN_INT
#define SLJIT2_CONV_NAN_FLOAT SLJIT2_CONV_RESULT_MIN_INT
#elif (defined SLJIT2_CONFIG_RISCV && SLJIT2_CONFIG_RISCV)
#define SLJIT2_CONV_MAX_FLOAT SLJIT2_CONV_RESULT_MAX_INT
#define SLJIT2_CONV_MIN_FLOAT SLJIT2_CONV_RESULT_MIN_INT
#define SLJIT2_CONV_NAN_FLOAT SLJIT2_CONV_RESULT_MAX_INT
#elif (defined SLJIT2_CONFIG_S390X && SLJIT2_CONFIG_S390X)
#define SLJIT2_CONV_MAX_FLOAT SLJIT2_CONV_RESULT_MAX_INT
#define SLJIT2_CONV_MIN_FLOAT SLJIT2_CONV_RESULT_MIN_INT
#define SLJIT2_CONV_NAN_FLOAT SLJIT2_CONV_RESULT_MIN_INT
#elif (defined SLJIT2_CONFIG_LOONGARCH && SLJIT2_CONFIG_LOONGARCH)
#define SLJIT2_CONV_MAX_FLOAT SLJIT2_CONV_RESULT_MAX_INT
#define SLJIT2_CONV_MIN_FLOAT SLJIT2_CONV_RESULT_MIN_INT
#define SLJIT2_CONV_NAN_FLOAT SLJIT2_CONV_RESULT_ZERO
#else
#error "Result for float to integer conversion is not defined"
#endif

#ifndef SLJIT2_W

/* Defining long constants. */
#if (defined SLJIT2_64BIT_ARCHITECTURE && SLJIT2_64BIT_ARCHITECTURE)
#ifdef _WIN64
#define SLJIT2_W(w)	(w##ll)
#else /* !windows */
#define SLJIT2_W(w)	(w##l)
#endif /* windows */
#else /* 32 bit */
#define SLJIT2_W(w)	(w)
#endif /* unknown */

#endif /* !SLJIT2_W */

/*************************/
/* Endianness detection. */
/*************************/

#if !defined(SLJIT2_BIG_ENDIAN) && !defined(SLJIT2_LITTLE_ENDIAN)

/* These macros are mostly useful for the applications. */
#if (defined SLJIT2_CONFIG_PPC && SLJIT2_CONFIG_PPC)

#ifdef __LITTLE_ENDIAN__
#define SLJIT2_LITTLE_ENDIAN 1
#else
#define SLJIT2_BIG_ENDIAN 1
#endif

#elif (defined SLJIT2_CONFIG_MIPS && SLJIT2_CONFIG_MIPS)

#ifdef __MIPSEL__
#define SLJIT2_LITTLE_ENDIAN 1
#else
#define SLJIT2_BIG_ENDIAN 1
#endif

#ifndef SLJIT2_MIPS_REV

/* Auto detecting mips revision. */
#if (defined __mips_isa_rev) && (__mips_isa_rev >= 6)
#define SLJIT2_MIPS_REV 6
#elif defined(__mips_isa_rev) && __mips_isa_rev >= 1
#define SLJIT2_MIPS_REV __mips_isa_rev
#elif defined(__clang__) \
	&& (defined(_MIPS_ARCH_OCTEON) || defined(_MIPS_ARCH_P5600))
/* clang either forgets to define (clang-7) __mips_isa_rev at all
 * or sets it to zero (clang-8,-9) for -march=octeon (MIPS64 R2+)
 * and -march=p5600 (MIPS32 R5).
 * It also sets the __mips macro to 64 or 32 for -mipsN when N <= 5
 * (should be set to N exactly) so we cannot rely on this too.
 */
#define SLJIT2_MIPS_REV 1
#endif

#endif /* !SLJIT2_MIPS_REV */

#elif (defined SLJIT2_CONFIG_S390X && SLJIT2_CONFIG_S390X)

#define SLJIT2_BIG_ENDIAN 1

#else
#define SLJIT2_LITTLE_ENDIAN 1
#endif

#endif /* !defined(SLJIT2_BIG_ENDIAN) && !defined(SLJIT2_LITTLE_ENDIAN) */

/* Sanity check. */
#if (defined SLJIT2_BIG_ENDIAN && SLJIT2_BIG_ENDIAN) && (defined SLJIT2_LITTLE_ENDIAN && SLJIT2_LITTLE_ENDIAN)
#error "Exactly one endianness must be selected"
#endif

#if !(defined SLJIT2_BIG_ENDIAN && SLJIT2_BIG_ENDIAN) && !(defined SLJIT2_LITTLE_ENDIAN && SLJIT2_LITTLE_ENDIAN)
#error "Exactly one endianness must be selected"
#endif

#ifndef SLJIT2_UNALIGNED

#if (defined SLJIT2_CONFIG_X86 && SLJIT2_CONFIG_X86) \
	|| (defined SLJIT2_CONFIG_ARM_V7 && SLJIT2_CONFIG_ARM_V7) \
	|| (defined SLJIT2_CONFIG_ARM_THUMB2 && SLJIT2_CONFIG_ARM_THUMB2) \
	|| (defined SLJIT2_CONFIG_ARM_64 && SLJIT2_CONFIG_ARM_64) \
	|| (defined SLJIT2_CONFIG_PPC && SLJIT2_CONFIG_PPC) \
	|| (defined SLJIT2_CONFIG_RISCV && SLJIT2_CONFIG_RISCV) \
	|| (defined SLJIT2_CONFIG_S390X && SLJIT2_CONFIG_S390X) \
	|| (defined SLJIT2_CONFIG_LOONGARCH && SLJIT2_CONFIG_LOONGARCH)
#define SLJIT2_UNALIGNED 1
#endif

#endif /* !SLJIT2_UNALIGNED */

#ifndef SLJIT2_FPU_UNALIGNED

#if (defined SLJIT2_CONFIG_X86 && SLJIT2_CONFIG_X86) \
	|| (defined SLJIT2_CONFIG_ARM_64 && SLJIT2_CONFIG_ARM_64) \
	|| (defined SLJIT2_CONFIG_PPC && SLJIT2_CONFIG_PPC) \
	|| (defined SLJIT2_CONFIG_RISCV && SLJIT2_CONFIG_RISCV) \
	|| (defined SLJIT2_CONFIG_S390X && SLJIT2_CONFIG_S390X) \
	|| (defined SLJIT2_CONFIG_LOONGARCH && SLJIT2_CONFIG_LOONGARCH)
#define SLJIT2_FPU_UNALIGNED 1
#endif

#endif /* !SLJIT2_FPU_UNALIGNED */

#if (defined SLJIT2_CONFIG_X86_32 && SLJIT2_CONFIG_X86_32)
/* Auto detect SSE2 support using CPUID.
   On 64 bit x86 cpus, sse2 must be present. */
#define SLJIT2_DETECT_SSE2 1
#endif

/*****************************************************************************************/
/* Calling convention of functions generated by SLJIT or called from the generated code. */
/*****************************************************************************************/

#ifndef SLJIT2_FUNC
#define SLJIT2_FUNC
#endif /* !SLJIT2_FUNC */

#ifndef SLJIT2_INDIRECT_CALL
#if ((defined SLJIT2_CONFIG_PPC_64 && SLJIT2_CONFIG_PPC_64) && (!defined _CALL_ELF || _CALL_ELF == 1)) \
	|| ((defined SLJIT2_CONFIG_PPC_32 && SLJIT2_CONFIG_PPC_32) && defined _AIX)
/* It seems certain ppc compilers use an indirect addressing for functions
   which makes things complicated. */
#define SLJIT2_INDIRECT_CALL 1
#endif
#endif /* SLJIT2_INDIRECT_CALL */

/* The offset which needs to be subtracted from the return address to
determine the next executed instruction after return. */
#ifndef SLJIT2_RETURN_ADDRESS_OFFSET
#define SLJIT2_RETURN_ADDRESS_OFFSET 0
#endif /* SLJIT2_RETURN_ADDRESS_OFFSET */

/***************************************************/
/* Functions of the built-in executable allocator. */
/***************************************************/

#if (defined SLJIT2_EXECUTABLE_ALLOCATOR && SLJIT2_EXECUTABLE_ALLOCATOR)
SLJIT2_API_FUNC_ATTRIBUTE void* sljit2_malloc_exec(sljit2_uw size);
SLJIT2_API_FUNC_ATTRIBUTE void sljit2_free_exec(void* ptr);
/* Note: sljitLir.h also defines sljit2_free_unused_memory_exec() function. */
#define SLJIT2_BUILTIN_MALLOC_EXEC(size, exec_allocator_data) sljit2_malloc_exec(size)
#define SLJIT2_BUILTIN_FREE_EXEC(ptr, exec_allocator_data) sljit2_free_exec(ptr)

#ifndef SLJIT2_MALLOC_EXEC
#define SLJIT2_MALLOC_EXEC(size, exec_allocator_data) SLJIT2_BUILTIN_MALLOC_EXEC((size), (exec_allocator_data))
#endif /* SLJIT2_MALLOC_EXEC */

#ifndef SLJIT2_FREE_EXEC
#define SLJIT2_FREE_EXEC(ptr, exec_allocator_data) SLJIT2_BUILTIN_FREE_EXEC((ptr), (exec_allocator_data))
#endif /* SLJIT2_FREE_EXEC */

#if (defined SLJIT2_PROT_EXECUTABLE_ALLOCATOR && SLJIT2_PROT_EXECUTABLE_ALLOCATOR)
SLJIT2_API_FUNC_ATTRIBUTE sljit2_sw sljit2_exec_offset(void *code);
#define SLJIT2_EXEC_OFFSET(code) sljit2_exec_offset(code)
#endif /* SLJIT2_PROT_EXECUTABLE_ALLOCATOR */

#endif /* SLJIT2_EXECUTABLE_ALLOCATOR */

#ifndef SLJIT2_EXEC_OFFSET
#define SLJIT2_EXEC_OFFSET(ptr) 0
#endif

/**********************************************/
/* Registers and locals offset determination. */
/**********************************************/

#if (defined SLJIT2_CONFIG_X86_32 && SLJIT2_CONFIG_X86_32)

#define SLJIT2_NUMBER_OF_REGISTERS 12
#define SLJIT2_NUMBER_OF_SAVED_REGISTERS 7
#define SLJIT2_NUMBER_OF_TEMPORARY_REGISTERS 1
#define SLJIT2_NUMBER_OF_FLOAT_REGISTERS 7
#define SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS 0
#define SLJIT2_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS 1
#define SLJIT2_TMP_DEST_REG SLJIT2_TMP_R0
#define SLJIT2_TMP_MEM_REG SLJIT2_TMP_R0
#define SLJIT2_TMP_DEST_FREG SLJIT2_TMP_FR0
#define SLJIT2_LOCALS_OFFSET_BASE (8 * (sljit2_s32)sizeof(sljit2_sw))
#define SLJIT2_PREF_SHIFT_REG SLJIT2_R2
#define SLJIT2_MASKED_SHIFT 1
#define SLJIT2_MASKED_SHIFT32 1

#elif (defined SLJIT2_CONFIG_X86_64 && SLJIT2_CONFIG_X86_64)

#define SLJIT2_NUMBER_OF_REGISTERS 13
#define SLJIT2_NUMBER_OF_TEMPORARY_REGISTERS 2
#define SLJIT2_NUMBER_OF_FLOAT_REGISTERS 15
#define SLJIT2_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS 1
#ifndef _WIN64
#define SLJIT2_NUMBER_OF_SAVED_REGISTERS 6
#define SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS 0
#define SLJIT2_LOCALS_OFFSET_BASE 0
#else /* _WIN64 */
#define SLJIT2_NUMBER_OF_SAVED_REGISTERS 8
#define SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS 10
#define SLJIT2_LOCALS_OFFSET_BASE (4 * (sljit2_s32)sizeof(sljit2_sw))
#endif /* !_WIN64 */
#define SLJIT2_TMP_DEST_REG SLJIT2_TMP_R0
#define SLJIT2_TMP_MEM_REG SLJIT2_TMP_R0
#define SLJIT2_TMP_DEST_FREG SLJIT2_TMP_FR0
#define SLJIT2_PREF_SHIFT_REG SLJIT2_R3
#define SLJIT2_MASKED_SHIFT 1
#define SLJIT2_MASKED_SHIFT32 1

#elif (defined SLJIT2_CONFIG_ARM_32 && SLJIT2_CONFIG_ARM_32)

#define SLJIT2_NUMBER_OF_REGISTERS 12
#define SLJIT2_NUMBER_OF_SAVED_REGISTERS 8
#define SLJIT2_NUMBER_OF_TEMPORARY_REGISTERS 2
#define SLJIT2_NUMBER_OF_FLOAT_REGISTERS 14
#define SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS 8
#define SLJIT2_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS 2
#define SLJIT2_TMP_DEST_REG SLJIT2_TMP_R1
#define SLJIT2_TMP_MEM_REG SLJIT2_TMP_R1
#define SLJIT2_TMP_DEST_FREG SLJIT2_TMP_FR0
#define SLJIT2_LOCALS_OFFSET_BASE 0

#elif (defined SLJIT2_CONFIG_ARM_64 && SLJIT2_CONFIG_ARM_64)

#define SLJIT2_NUMBER_OF_REGISTERS 26
#define SLJIT2_NUMBER_OF_SAVED_REGISTERS 10
#define SLJIT2_NUMBER_OF_TEMPORARY_REGISTERS 3
#define SLJIT2_NUMBER_OF_FLOAT_REGISTERS 30
#define SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS 8
#define SLJIT2_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS 2
#define SLJIT2_TMP_DEST_REG SLJIT2_TMP_R0
#define SLJIT2_TMP_MEM_REG SLJIT2_TMP_R0
#define SLJIT2_TMP_DEST_FREG SLJIT2_TMP_FR0
#define SLJIT2_LOCALS_OFFSET_BASE (2 * (sljit2_s32)sizeof(sljit2_sw))
#define SLJIT2_MASKED_SHIFT 1
#define SLJIT2_MASKED_SHIFT32 1

#elif (defined SLJIT2_CONFIG_PPC && SLJIT2_CONFIG_PPC)

#define SLJIT2_NUMBER_OF_REGISTERS 23
#define SLJIT2_NUMBER_OF_SAVED_REGISTERS 17
#define SLJIT2_NUMBER_OF_TEMPORARY_REGISTERS 3
#define SLJIT2_NUMBER_OF_FLOAT_REGISTERS 30
#define SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS 18
#define SLJIT2_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS 2
#define SLJIT2_TMP_DEST_REG SLJIT2_TMP_R1
#define SLJIT2_TMP_MEM_REG SLJIT2_TMP_R1
#define SLJIT2_TMP_DEST_FREG SLJIT2_TMP_FR0
#if (defined SLJIT2_CONFIG_PPC_64 && SLJIT2_CONFIG_PPC_64) || (defined _AIX)
#define SLJIT2_LOCALS_OFFSET_BASE ((6 + 8) * (sljit2_s32)sizeof(sljit2_sw))
#elif (defined SLJIT2_CONFIG_PPC_32 && SLJIT2_CONFIG_PPC_32)
/* Add +1 for double alignment. */
#define SLJIT2_LOCALS_OFFSET_BASE ((3 + 1) * (sljit2_s32)sizeof(sljit2_sw))
#else
#define SLJIT2_LOCALS_OFFSET_BASE (3 * (sljit2_s32)sizeof(sljit2_sw))
#endif /* SLJIT2_CONFIG_PPC_64 || _AIX */

#elif (defined SLJIT2_CONFIG_MIPS && SLJIT2_CONFIG_MIPS)

#define SLJIT2_NUMBER_OF_REGISTERS 21
#define SLJIT2_NUMBER_OF_SAVED_REGISTERS 8
#if (defined SLJIT2_CONFIG_MIPS_32 && SLJIT2_CONFIG_MIPS_32)
#define SLJIT2_LOCALS_OFFSET_BASE (4 * (sljit2_s32)sizeof(sljit2_sw))
#define SLJIT2_NUMBER_OF_FLOAT_REGISTERS 13
#define SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS 6
#else
#define SLJIT2_LOCALS_OFFSET_BASE 0
#define SLJIT2_NUMBER_OF_FLOAT_REGISTERS 29
#define SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS 8
#endif
#define SLJIT2_NUMBER_OF_TEMPORARY_REGISTERS 5
#define SLJIT2_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS 3
#define SLJIT2_TMP_DEST_REG SLJIT2_TMP_R1
#define SLJIT2_TMP_MEM_REG SLJIT2_TMP_R1
#define SLJIT2_TMP_DEST_FREG SLJIT2_TMP_FR0
#define SLJIT2_MASKED_SHIFT 1
#define SLJIT2_MASKED_SHIFT32 1

#elif (defined SLJIT2_CONFIG_RISCV && SLJIT2_CONFIG_RISCV)

#define SLJIT2_NUMBER_OF_REGISTERS 23
#define SLJIT2_NUMBER_OF_SAVED_REGISTERS 12
#define SLJIT2_NUMBER_OF_TEMPORARY_REGISTERS 5
#define SLJIT2_NUMBER_OF_FLOAT_REGISTERS 30
#define SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS 12
#define SLJIT2_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS 2
#define SLJIT2_SEPARATE_VECTOR_REGISTERS 1
#define SLJIT2_NUMBER_OF_VECTOR_REGISTERS 30
#define SLJIT2_NUMBER_OF_SAVED_VECTOR_REGISTERS 0
#define SLJIT2_NUMBER_OF_TEMPORARY_VECTOR_REGISTERS 2
#define SLJIT2_TMP_DEST_REG SLJIT2_TMP_R1
#define SLJIT2_TMP_MEM_REG SLJIT2_TMP_R1
#define SLJIT2_TMP_DEST_FREG SLJIT2_TMP_FR0
#define SLJIT2_TMP_DEST_VREG SLJIT2_TMP_VR0
#define SLJIT2_LOCALS_OFFSET_BASE 0
#define SLJIT2_MASKED_SHIFT 1
#define SLJIT2_MASKED_SHIFT32 1

#elif (defined SLJIT2_CONFIG_S390X && SLJIT2_CONFIG_S390X)

/*
 * https://refspecs.linuxbase.org/ELF/zSeries/lzsabi0_zSeries.html#STACKFRAME
 *
 * 160
 *  .. FR6
 *  .. FR4
 *  .. FR2
 * 128 FR0
 * 120 R15 (used for SP)
 * 112 R14
 * 104 R13
 *  96 R12
 *  ..
 *  48 R6
 *  ..
 *  16 R2
 *   8 RESERVED
 *   0 SP
 */
#define SLJIT2_S390X_DEFAULT_STACK_FRAME_SIZE 160

#define SLJIT2_NUMBER_OF_REGISTERS 12
#define SLJIT2_NUMBER_OF_SAVED_REGISTERS 8
#define SLJIT2_NUMBER_OF_TEMPORARY_REGISTERS 3
#define SLJIT2_NUMBER_OF_FLOAT_REGISTERS 15
#define SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS 8
#define SLJIT2_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS 1
#define SLJIT2_TMP_DEST_REG SLJIT2_TMP_R0
#define SLJIT2_TMP_MEM_REG SLJIT2_TMP_R2
#define SLJIT2_TMP_DEST_FREG SLJIT2_TMP_FR0
#define SLJIT2_LOCALS_OFFSET_BASE SLJIT2_S390X_DEFAULT_STACK_FRAME_SIZE
#define SLJIT2_MASKED_SHIFT 1

#elif (defined SLJIT2_CONFIG_LOONGARCH && SLJIT2_CONFIG_LOONGARCH)

#define SLJIT2_NUMBER_OF_REGISTERS 23
#define SLJIT2_NUMBER_OF_SAVED_REGISTERS 10
#define SLJIT2_NUMBER_OF_TEMPORARY_REGISTERS 5
#define SLJIT2_NUMBER_OF_FLOAT_REGISTERS 30
#define SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS 12
#define SLJIT2_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS 2
#define SLJIT2_TMP_DEST_REG SLJIT2_TMP_R1
#define SLJIT2_TMP_MEM_REG SLJIT2_TMP_R1
#define SLJIT2_TMP_DEST_FREG SLJIT2_TMP_FR0
#define SLJIT2_LOCALS_OFFSET_BASE 0
#define SLJIT2_MASKED_SHIFT 1
#define SLJIT2_MASKED_SHIFT32 1

#elif (defined SLJIT2_CONFIG_UNSUPPORTED && SLJIT2_CONFIG_UNSUPPORTED)

/* Just to have something. */
#define SLJIT2_NUMBER_OF_REGISTERS 0
#define SLJIT2_NUMBER_OF_SAVED_REGISTERS 0
#define SLJIT2_NUMBER_OF_TEMPORARY_REGISTERS 0
#define SLJIT2_NUMBER_OF_FLOAT_REGISTERS 0
#define SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS 0
#define SLJIT2_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS 0
#define SLJIT2_TMP_DEST_REG 0
#define SLJIT2_TMP_MEM_REG 0
#define SLJIT2_TMP_DEST_FREG 0
#define SLJIT2_LOCALS_OFFSET_BASE 0

#endif

#if !(defined SLJIT2_SEPARATE_VECTOR_REGISTERS && SLJIT2_SEPARATE_VECTOR_REGISTERS)
#define SLJIT2_NUMBER_OF_VECTOR_REGISTERS (SLJIT2_NUMBER_OF_FLOAT_REGISTERS)
#define SLJIT2_NUMBER_OF_SAVED_VECTOR_REGISTERS (SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS)
#define SLJIT2_NUMBER_OF_TEMPORARY_VECTOR_REGISTERS (SLJIT2_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS)
#define SLJIT2_TMP_DEST_VREG (SLJIT2_TMP_DEST_FREG)
#endif /* !SLJIT2_SEPARATE_VECTOR_REGISTERS */

#define SLJIT2_LOCALS_OFFSET (SLJIT2_LOCALS_OFFSET_BASE)

#define SLJIT2_NUMBER_OF_SCRATCH_REGISTERS \
	(SLJIT2_NUMBER_OF_REGISTERS - SLJIT2_NUMBER_OF_SAVED_REGISTERS)

#define SLJIT2_NUMBER_OF_SCRATCH_FLOAT_REGISTERS \
	(SLJIT2_NUMBER_OF_FLOAT_REGISTERS - SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS)

#define SLJIT2_NUMBER_OF_SCRATCH_VECTOR_REGISTERS \
	(SLJIT2_NUMBER_OF_VECTOR_REGISTERS - SLJIT2_NUMBER_OF_SAVED_VECTOR_REGISTERS)

/**********************************/
/* Temporary register management. */
/**********************************/

#define SLJIT2_TMP_REGISTER_BASE (SLJIT2_NUMBER_OF_REGISTERS + 2)
#define SLJIT2_TMP_FREGISTER_BASE (SLJIT2_NUMBER_OF_FLOAT_REGISTERS + 1)
#define SLJIT2_TMP_VREGISTER_BASE (SLJIT2_NUMBER_OF_VECTOR_REGISTERS + 1)

/* WARNING: Accessing temporary registers is not recommended, because they
   are also used by the JIT compiler for various computations. Using them
   might have any side effects including incorrect operations and crashes,
   so use them at your own risk. The machine registers themselves might have
   limitations, e.g. the r0 register on s390x / ppc cannot be used as
   base address for memory operations. */

/* Temporary registers */
#define SLJIT2_TMP_R0		(SLJIT2_TMP_REGISTER_BASE + 0)
#define SLJIT2_TMP_R1		(SLJIT2_TMP_REGISTER_BASE + 1)
#define SLJIT2_TMP_R2		(SLJIT2_TMP_REGISTER_BASE + 2)
#define SLJIT2_TMP_R3		(SLJIT2_TMP_REGISTER_BASE + 3)
#define SLJIT2_TMP_R4		(SLJIT2_TMP_REGISTER_BASE + 4)
#define SLJIT2_TMP_R5		(SLJIT2_TMP_REGISTER_BASE + 5)
#define SLJIT2_TMP_R6		(SLJIT2_TMP_REGISTER_BASE + 6)
#define SLJIT2_TMP_R7		(SLJIT2_TMP_REGISTER_BASE + 7)
#define SLJIT2_TMP_R8		(SLJIT2_TMP_REGISTER_BASE + 8)
#define SLJIT2_TMP_R9		(SLJIT2_TMP_REGISTER_BASE + 9)
#define SLJIT2_TMP_R(i)		(SLJIT2_TMP_REGISTER_BASE + (i))

#define SLJIT2_TMP_FR0		(SLJIT2_TMP_FREGISTER_BASE + 0)
#define SLJIT2_TMP_FR1		(SLJIT2_TMP_FREGISTER_BASE + 1)
#define SLJIT2_TMP_FR2		(SLJIT2_TMP_FREGISTER_BASE + 2)
#define SLJIT2_TMP_FR3		(SLJIT2_TMP_FREGISTER_BASE + 3)
#define SLJIT2_TMP_FR4		(SLJIT2_TMP_FREGISTER_BASE + 4)
#define SLJIT2_TMP_FR5		(SLJIT2_TMP_FREGISTER_BASE + 5)
#define SLJIT2_TMP_FR6		(SLJIT2_TMP_FREGISTER_BASE + 6)
#define SLJIT2_TMP_FR7		(SLJIT2_TMP_FREGISTER_BASE + 7)
#define SLJIT2_TMP_FR8		(SLJIT2_TMP_FREGISTER_BASE + 8)
#define SLJIT2_TMP_FR9		(SLJIT2_TMP_FREGISTER_BASE + 9)
#define SLJIT2_TMP_FR(i)		(SLJIT2_TMP_FREGISTER_BASE + (i))

#define SLJIT2_TMP_VR0		(SLJIT2_TMP_VREGISTER_BASE + 0)
#define SLJIT2_TMP_VR1		(SLJIT2_TMP_VREGISTER_BASE + 1)
#define SLJIT2_TMP_VR2		(SLJIT2_TMP_VREGISTER_BASE + 2)
#define SLJIT2_TMP_VR3		(SLJIT2_TMP_VREGISTER_BASE + 3)
#define SLJIT2_TMP_VR4		(SLJIT2_TMP_VREGISTER_BASE + 4)
#define SLJIT2_TMP_VR5		(SLJIT2_TMP_VREGISTER_BASE + 5)
#define SLJIT2_TMP_VR6		(SLJIT2_TMP_VREGISTER_BASE + 6)
#define SLJIT2_TMP_VR7		(SLJIT2_TMP_VREGISTER_BASE + 7)
#define SLJIT2_TMP_VR8		(SLJIT2_TMP_VREGISTER_BASE + 8)
#define SLJIT2_TMP_VR9		(SLJIT2_TMP_VREGISTER_BASE + 9)
#define SLJIT2_TMP_VR(i)		(SLJIT2_TMP_VREGISTER_BASE + (i))

/********************************/
/* CPU status flags management. */
/********************************/

#if (defined SLJIT2_CONFIG_ARM && SLJIT2_CONFIG_ARM) \
	|| (defined SLJIT2_CONFIG_PPC && SLJIT2_CONFIG_PPC) \
	|| (defined SLJIT2_CONFIG_MIPS && SLJIT2_CONFIG_MIPS) \
	|| (defined SLJIT2_CONFIG_RISCV && SLJIT2_CONFIG_RISCV) \
	|| (defined SLJIT2_CONFIG_S390X && SLJIT2_CONFIG_S390X) \
	|| (defined SLJIT2_CONFIG_LOONGARCH && SLJIT2_CONFIG_LOONGARCH)
#define SLJIT2_HAS_STATUS_FLAGS_STATE 1
#endif

/***************************************/
/* Floating point register management. */
/***************************************/

#if (defined SLJIT2_CONFIG_ARM_32 && SLJIT2_CONFIG_ARM_32) \
	|| (defined SLJIT2_CONFIG_MIPS_32 && SLJIT2_CONFIG_MIPS_32)
#define SLJIT2_F64_SECOND(reg) \
	((reg) + SLJIT2_FS0 + SLJIT2_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS)
#else /* !SLJIT2_CONFIG_ARM_32 && !SLJIT2_CONFIG_MIPS_32 */
#define SLJIT2_F64_SECOND(reg) \
	(reg)
#endif /* SLJIT2_CONFIG_ARM_32 || SLJIT2_CONFIG_MIPS_32 */

/*************************************/
/* Debug and verbose related macros. */
/*************************************/

#if (defined SLJIT2_DEBUG && SLJIT2_DEBUG)

#if !defined(SLJIT2_ASSERT) || !defined(SLJIT2_UNREACHABLE)

/* SLJIT2_HALT_PROCESS must halt the process. */
#ifndef SLJIT2_HALT_PROCESS
#define SLJIT2_HALT_PROCESS() \
	abort();
#endif /* !SLJIT2_HALT_PROCESS */

#endif /* !SLJIT2_ASSERT || !SLJIT2_UNREACHABLE */

/* Feel free to redefine these two macros. */
#ifndef SLJIT2_ASSERT

#define SLJIT2_ASSERT(x) \
	do { \
		if (SLJIT2_UNLIKELY(!(x))) { \
			printf("Assertion failed at " __FILE__ ":%d\n", __LINE__); \
			SLJIT2_HALT_PROCESS(); \
		} \
	} while (0)

#endif /* !SLJIT2_ASSERT */

#ifndef SLJIT2_UNREACHABLE

#define SLJIT2_UNREACHABLE() \
	do { \
		printf("Should never been reached " __FILE__ ":%d\n", __LINE__); \
		SLJIT2_HALT_PROCESS(); \
	} while (0)

#endif /* !SLJIT2_UNREACHABLE */

#else /* (defined SLJIT2_DEBUG && SLJIT2_DEBUG) */

/* Forcing empty, but valid statements. */
#undef SLJIT2_ASSERT
#undef SLJIT2_UNREACHABLE

#define SLJIT2_ASSERT(x) \
	do { } while (0)
#define SLJIT2_UNREACHABLE() \
	do { } while (0)

#endif /* (defined SLJIT2_DEBUG && SLJIT2_DEBUG) */

#ifndef SLJIT2_COMPILE_ASSERT

#define SLJIT2_COMPILE_ASSERT(x, description) \
	switch(0) { case 0: case ((x) ? 1 : 0): break; }

#endif /* !SLJIT2_COMPILE_ASSERT */

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* SLJIT2_CONFIG_INTERNAL_H_ */
