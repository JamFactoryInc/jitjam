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

#ifndef SLJIT2_CONFIG_H_
#define SLJIT2_CONFIG_H_

#ifdef __cplusplus
extern "C" {
#endif

/*
  This file contains the basic configuration options for the SLJIT compiler
  and their default values. These options can be overridden in the
  sljitConfigPre.h header file when SLJIT2_HAVE_CONFIG_PRE is set to a
  non-zero value.
*/

/* --------------------------------------------------------------------- */
/*  Utilities                                                            */
/* --------------------------------------------------------------------- */

/* Implements a stack like data structure (by using mmap / VirtualAlloc  */
/* or a custom allocator). */
#ifndef SLJIT2_UTIL_STACK
/* Enabled by default */
#define SLJIT2_UTIL_STACK 1
#endif

/* Uses user provided allocator to allocate the stack (see SLJIT2_UTIL_STACK) */
#ifndef SLJIT2_UTIL_SIMPLE_STACK_ALLOCATION
/* Disabled by default */
#define SLJIT2_UTIL_SIMPLE_STACK_ALLOCATION 0
#endif

/* Single threaded application. Does not require any locks. */
#ifndef SLJIT2_SINGLE_THREADED
/* Disabled by default. */
#define SLJIT2_SINGLE_THREADED 0
#endif

/* --------------------------------------------------------------------- */
/*  Configuration                                                        */
/* --------------------------------------------------------------------- */

/* If SLJIT2_STD_MACROS_DEFINED is not defined, the application should
   define SLJIT2_MALLOC, SLJIT2_FREE, SLJIT2_MEMCPY, and NULL. */
#ifndef SLJIT2_STD_MACROS_DEFINED
/* Disabled by default. */
#define SLJIT2_STD_MACROS_DEFINED 0
#endif

/* Executable code allocation:
   If SLJIT2_EXECUTABLE_ALLOCATOR is not defined, the application should
   define SLJIT2_MALLOC_EXEC and SLJIT2_FREE_EXEC.
   Optionally, depending on the implementation used for the allocator,
   SLJIT2_EXEC_OFFSET and SLJIT2_UPDATE_WX_FLAGS might also be needed. */
#ifndef SLJIT2_EXECUTABLE_ALLOCATOR
/* Enabled by default. */
#define SLJIT2_EXECUTABLE_ALLOCATOR 1

/* When SLJIT2_PROT_EXECUTABLE_ALLOCATOR is enabled SLJIT uses
   an allocator which does not set writable and executable
   permission flags at the same time.
   Instead, it creates a shared memory segment (usually backed by a file)
   and maps it twice, with different permissions, depending on the use
   case.
   The trade-off is increased use of virtual memory, incompatibility with
   fork(), and some possible additional security risks by the use of
   publicly accessible files for the generated code. */
#ifndef SLJIT2_PROT_EXECUTABLE_ALLOCATOR
/* Disabled by default. */
#define SLJIT2_PROT_EXECUTABLE_ALLOCATOR 0
#endif

/* When SLJIT2_WX_EXECUTABLE_ALLOCATOR is enabled SLJIT uses an
   allocator which does not set writable and executable permission
   flags at the same time.
   Instead, it creates a new independent map on each invocation and
   switches permissions at the underlying pages as needed.
   The trade-off is increased memory use and degraded performance. */
#ifndef SLJIT2_WX_EXECUTABLE_ALLOCATOR
/* Disabled by default. */
#define SLJIT2_WX_EXECUTABLE_ALLOCATOR 0
#endif

#endif /* !SLJIT2_EXECUTABLE_ALLOCATOR */

/* Return with error when an invalid argument is passed. */
#ifndef SLJIT2_ARGUMENT_CHECKS
/* Disabled by default */
#define SLJIT2_ARGUMENT_CHECKS 0
#endif

/* Debug checks (assertions, etc.). */
#ifndef SLJIT2_DEBUG
/* Enabled by default */
#define SLJIT2_DEBUG 1
#endif

/* Verbose operations. */
#ifndef SLJIT2_VERBOSE
/* Enabled by default */
#define SLJIT2_VERBOSE 1
#endif

/*
  SLJIT2_IS_FPU_AVAILABLE
    The availability of the FPU can be controlled by SLJIT2_IS_FPU_AVAILABLE.
      zero value - FPU is NOT present.
      nonzero value - FPU is present.
*/

/* For further configurations, see the beginning of sljitConfigInternal.h */

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* SLJIT2_CONFIG_H_ */
