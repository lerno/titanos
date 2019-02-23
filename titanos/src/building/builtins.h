#pragma once
//
// Created by Christoffer Lernö on 2019-02-11.
//

typedef struct _Type Type;
typedef struct _Decl Decl;
typedef struct _Module Module;

void register_builtins(Module *module);

Type *type_builtin_bool();
Type *type_builtin_i8();
Type *type_builtin_i16();
Type *type_builtin_i32();
Type *type_builtin_i64();
Type *type_builtin_u8();
Type *type_builtin_u16();
Type *type_builtin_u32();
Type *type_builtin_u64();
Type *type_builtin_f16();
Type *type_builtin_f32();
Type *type_builtin_f64();
Type *type_builtin_f128();
Decl *decl_builtin_bool();
Decl *decl_builtin_i8();
Decl *decl_builtin_i16();
Decl *decl_builtin_i32();
Decl *decl_builtin_i64();
Decl *decl_builtin_u8();
Decl *decl_builtin_u16();
Decl *decl_builtin_u32();
Decl *decl_builtin_u64();
Decl *decl_builtin_f16();
Decl *decl_builtin_f32();
Decl *decl_builtin_f64();
Decl *decl_builtin_f128();

Type *type_get_signed(Type *type);
Type *type_get_unsigned(Type *type);