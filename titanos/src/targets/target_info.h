#pragma once
//
// Created by Christoffer Lerno on 2019-01-12.
// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
//

#include "common.h"
#include "lexer.h"

typedef enum
{
    ARCH_UNKNOWN,
    ARCH_I686,
    ARCH_X86_64,
    ARCH_ARM
} Arch;

typedef enum
{
    SYS_UNKNOWN,
    SYS_LINUX,
    SYS_DARWIN,
    SYS_CYGWIN
} System;

typedef enum
{
    VENDOR_UNKNOWN,
    VENDOR_APPLE
} Vendor;

typedef enum
{
    ABI_UNKNOWN,
    ABI_GNU,
    ABI_GNUEABI,
    ABI_MACHO,
    ABI_WIN32
} Abi;

typedef struct _TargetInfo
{
    Arch arch;
    System sys;
    Vendor vendor;
    Abi abi;
    unsigned int_width;

    struct {
      const char * const aliases[5];
      const char * const reg;
    } gcc_reg_alias;

    struct {
      const char * const names[5];
      const unsigned reg_num;
    } addl_reg_name;

} TargetInfo;


typedef struct
{
    bool allows_memory : 1;
    bool allows_register : 1;
    bool read_write : 1;      // "+r" output constraint (read and write).
    bool has_matching_input;  // This output operand has a matching input.
    bool immediate_constant;  // This operand must be an immediate constant
    int tied_operand;
    const char *constraint_str;  // constraint: "=rm"
    const char *name;            // Operand name: [foo] with no []'s.
    struct {
      int min;
      int max;
    } imm_range;
    //llvm::SmallSet<int, 4> ImmSet;

} ConstraintInfo;


void target_info_from_native(TargetInfo *info);
bool target_info_from_string(TargetInfo *info, const char *triple);
const char *target_info_to_string(TargetInfo *info);

bool target_info_is_valid_clobber(TargetInfo *info, Token *name);
bool target_info_is_valid_gcc_register_name(TargetInfo *info, Token *name);

