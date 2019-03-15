//
// Created by Christoffer Lernö on 2019-02-11.
//

#include "module.h"
#include "decl.h"
#include "error.h"
#include "builtins.h"
#include <llvm-c/Core.h>
#include "types/type.h"
#include <string.h>

static void register_c2_builtin(Module *module, Decl *decl)
{
    decl->module = module;
    module_add_symbol(module, decl->name, decl);
}

QualifiedType type_get_signed(Type *type)
{
    // TODO qt?
    if (type_is_signed(type)) return (QualifiedType) { .type = type };
    switch (type->type_id)
    {
        case TYPE_U64:
            return type_builtin_i64();
        case TYPE_U32:
            return type_builtin_i32();
        case TYPE_U16:
            return type_builtin_i16();
        case TYPE_U8:
            return type_builtin_i8();
        default:
            UNREACHABLE
    }
}

QualifiedType type_get_unsigned(Type *type)
{
    if (!type_is_signed(type)) return (QualifiedType) { .type = type };
    switch (type->type_id)
    {
        case TYPE_I64:
            return type_builtin_u64();
        case TYPE_I32:
            return type_builtin_u32();
        case TYPE_I16:
            return type_builtin_u16();
        case TYPE_I8:
            return type_builtin_u8();
        default:
            UNREACHABLE
    }
}


#define TYPES(X) \
  X(bool, TYPE_BOOL, false, 1, LLVMIntType(1)) \
  X(i8, TYPE_I8, true, 8, LLVMIntType(8)) \
  X(i16, TYPE_I16, true, 16, LLVMIntType(16)) \
  X(i32, TYPE_I32, true, 32, LLVMIntType(32)) \
  X(i64, TYPE_I64, true, 64, LLVMIntType(64)) \
  X(u8, TYPE_U8, false, 8, LLVMIntType(8)) \
  X(u16, TYPE_U16, false, 16, LLVMIntType(16)) \
  X(u32, TYPE_U32, false, 32, LLVMIntType(32)) \
  X(u64, TYPE_U64, false, 64, LLVMIntType(64)) \
  X(f32, TYPE_F32, true, 32, LLVMFloatType()) \
  X(f64, TYPE_F64, true, 64, LLVMDoubleType())

void register_builtins(Module *module)
{
#define REG(__name, __type, __signed, __bits, __init) register_c2_builtin(module, decl_builtin_ ## __name());
    TYPES(REG)
#undef REG
}


#define DECL(__name, __type, __signed, __bits, __init) Decl *decl_builtin_ ## __name() { \
  static Decl *decl = NULL; \
  if (!decl) { \
    const char *x = #__name; \
    const char *name = symtab_add(x, strlen(x)); \
    decl = decl_new(DECL_BUILTIN, (SourceRange) { .loc.id = UINT32_MAX }, name, true); \
    decl->type.type = type_new(__type); \
    decl->type.type->decl = decl; \
    decl->type.type->canonical_type = (QualifiedType) { decl->type.type }; \
    decl->type.type->llvm_type = __init; \
  } \
  return decl; \
}

#define TYPE(__name, __type, __signed, __bits, __init) QualifiedType type_builtin_ ## __name() { return decl_builtin_ ## __name()->type; }

TYPES(DECL)
TYPES(TYPE)

#undef TYPES
#undef DECL
#undef TYPE

