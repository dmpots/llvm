/*===-- TraceFileManagement.h - Trace support library support routines -----===*\
|*
|*                     The LLVM Compiler Infrastructure
|*
|* This file is distributed under the University of Illinois Open Source
|* License. See LICENSE.TXT for details.
|*
|*===----------------------------------------------------------------------===*|
|*
|* This file defines functions shared by the various different profiling
|* implementations.
|*
\*===----------------------------------------------------------------------===*/

#ifndef LLVM_RUNTIME_TRACE_FILE_MANAGEMENT_RUNTIME_H
#define LLVM_RUNTIME_TRACE_FILE_MANAGEMENT_RUNTIME_H

#ifdef __cplusplus
extern "C" {
#endif

/* Command line flag to collect trace completion data */
extern int flag_BuildShadowTrace;

/* save_arguments - Save argc and argv as passed into the program for the file
 * we output.
 */
int save_arguments(int argc, const char **argv);

/*
 * Retrieves the file descriptor for the profile file.
 */
int getOutFile();

#ifdef __cplusplus
}
#endif
#endif
