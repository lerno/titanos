#pragma once
//
// Created by Christoffer Lernö on 2019-02-11.
//

#include "types/type.h"

typedef struct _Type Type;
typedef struct _Decl Decl;
typedef struct _Module Module;

void register_builtins(Module *module);

QualifiedType type_builtin_bool();
QualifiedType type_builtin_i8();
QualifiedType type_builtin_i16();
QualifiedType type_builtin_i32();
QualifiedType type_builtin_i64();
QualifiedType type_builtin_u8();
QualifiedType type_builtin_u16();
QualifiedType type_builtin_u32();
QualifiedType type_builtin_u64();
//QualifiedType type_builtin_f16();
QualifiedType type_builtin_f32();
QualifiedType type_builtin_f64();
//QualifiedType type_builtin_f128();
Decl *decl_builtin_bool();
Decl *decl_builtin_i8();
Decl *decl_builtin_i16();
Decl *decl_builtin_i32();
Decl *decl_builtin_i64();
Decl *decl_builtin_u8();
Decl *decl_builtin_u16();
Decl *decl_builtin_u32();
Decl *decl_builtin_u64();
//Decl *decl_builtin_f16();
Decl *decl_builtin_f32();
Decl *decl_builtin_f64();
//Decl *decl_builtin_f128();

QualifiedType type_get_signed(Type *type);
QualifiedType type_get_unsigned(Type *type);