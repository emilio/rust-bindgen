#pragma once

#include "BindgenItem.h"

namespace clang {
class RecordType;
}

namespace bindgen {

class Type : public Item {
  std::string m_name;

protected:
  Type(ItemId id, ItemId parent, std::string comment, std::string name)
    : Item(id, parent, comment), m_name(name) {}

public:
  static bool fromClangTy(BindgenContext&, const clang::Type&, ItemId& out);
  static bool fromClangTy(BindgenContext&, const clang::QualType&, ItemId& out);

  const std::string& name() const {
    return m_name;
  }

  virtual Type* asType() override {
    return this;
  }
};

class QualifiedType : public Type {
  bool m_isConst;
public:
  QualifiedType(ItemId id, const Type& wrapping, bool isConst)
    : Type(id, wrapping.parent(), "", wrapping.name())
    , m_isConst(isConst)
  {}

  bool isConst() const {
    return m_isConst;
  }
};

class PointerType : public Type {
  ItemId m_pointee;

public:
  PointerType(ItemId id, ItemId pointee)
    : Type(id, 0, "", "")
    , m_pointee(pointee)
  {}
};

class ArrayType : public Type {
  ItemId m_inner;
  size_t m_length;

public:
  ArrayType(ItemId id, ItemId inner, size_t length)
    : Type(id, 0, "", "")
    , m_inner(inner)
    , m_length(length)
  {}
};

enum class IntKind : uint8_t {
  Bool,
  Char,
  UChar,
  Short,
  UShort,
  Int,
  UInt,
  Long,
  ULong,
  LongLong,
  ULongLong,
  I8,
  U8,
  I16,
  U16,
  I32,
  U32,
  I64,
  U64,
  I128,
  U128,
};

class IntegerType : public Type {
  IntKind m_kind;

public:
  IntegerType(ItemId id,
              ItemId parent,
              const std::string& comment,
              const std::string& name,
              IntKind kind)
    : Type(id, parent, comment, name), m_kind(kind) {}

  bool isSigned() const {
    switch (m_kind) {
      case IntKind::Bool:
      case IntKind::UChar:
      case IntKind::UShort:
      case IntKind::UInt:
      case IntKind::ULong:
      case IntKind::ULongLong:
      case IntKind::U8:
      case IntKind::U16:
      case IntKind::U32:
      case IntKind::U64:
      case IntKind::U128:
        return false;
      case IntKind::Char:
      case IntKind::Short:
      case IntKind::Int:
      case IntKind::Long:
      case IntKind::LongLong:
      case IntKind::I8:
      case IntKind::I16:
      case IntKind::I32:
      case IntKind::I64:
      case IntKind::I128:
        return true;
    }
  }
};

class Record : public Type {
  std::vector<RecordField> m_fields;

public:
  Record(ItemId id, ItemId parent, const std::string& comment, const std::string& name, const std::vector<RecordField>& fields)
    : Type(id, parent, comment, name)
    , m_fields(fields)
  {}

  static bool fromClangTy(BindgenContext&, const clang::RecordType&, ItemId&);
};

}  // namespace bindgen
