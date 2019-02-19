//
// Created by Christoffer Lernö on 2019-02-11.
//

#include "module.h"
#include "decl.h"
#include "error.h"
#include "builtins.h"
#include <llvm-c/Core.h>

static void register_c2_builtin(Module *module, Decl *decl)
{
    decl->module = module;
    module_add_symbol(module, &decl->name, decl);
}

#define TYPES(X) \
  X(bool, TYPE_BOOL, false, 1, LLVMIntType(1)) \
  X(i8, TYPE_INT, true, 8, LLVMIntType(8)) \
  X(i16, TYPE_INT, true, 16, LLVMIntType(16)) \
  X(i32, TYPE_INT, true, 32, LLVMIntType(32)) \
  X(i64, TYPE_INT, true, 64, LLVMIntType(64)) \
  X(u8, TYPE_INT, false, 8, LLVMIntType(8)) \
  X(u16, TYPE_INT, false, 16, LLVMIntType(16)) \
  X(u32, TYPE_INT, false, 32, LLVMIntType(32)) \
  X(u64, TYPE_INT, false, 64, LLVMIntType(64)) \
  X(f16, TYPE_FLOAT, true, 16, LLVMHalfType()) \
  X(f32, TYPE_FLOAT, true, 32, LLVMFloatType()) \
  X(f64, TYPE_FLOAT, true, 64, LLVMDoubleType()) \
  X(f128, TYPE_FLOAT, true, 128, LLVMFP128Type())

void register_builtins(Module *module)
{
#define REG(__name, __type, __signed, __bits, __init) register_c2_builtin(module, decl_builtin_ ## __name());
TYPES(REG)
#undef REG
}


#define DECL(__name, __type, __signed, __bits, __init) Decl *decl_builtin_ ## __name() { \
  static Decl *decl = NULL; \
  if (!decl) { \
    Token token = token_wrap(#__name); \
    decl = decl_new(DECL_BUILTIN, &token, &token, true); \
    decl->type.type_id = __type; \
    if (__type == TYPE_INT)  decl->type.integer = (TypeInt) { (uint16_t)__bits, __signed }; \
    if (__type == TYPE_FLOAT) decl->type.float_bits = (uint16_t)__bits; \
    decl->type.llvm_type = __init; \
  } \
  return decl; \
}

#define TYPE(__name, __type, __signed, __bits, __init) Type *type_builtin_ ## __name() { return &decl_builtin_ ## __name()->type; }

TYPES(DECL)
TYPES(TYPE)

#undef TYPES
#undef DECL
#undef TYPE