#pragma once

#include "BindgenContext.h"
#include <string>
#include <memory>
#include <vector>

namespace clang {
class Decl;
class RecordDecl;
class Type;
}

namespace bindgen {

class Var;
class Type;
class Mod;
class Function;

class Item {
  ItemId m_id;
  ItemId m_parent;
  std::string m_comment;

protected:
  Item(ItemId id, ItemId parent, std::string comment)
    : m_id(id), m_parent(parent), m_comment(comment) {}

public:
  static std::unique_ptr<Item> create(BindgenContext&, const clang::Decl&);

  ItemId id() const {
    return m_id;
  }

  virtual Var* asVar() {
    return nullptr;
  }
  virtual Type* asType() {
    return nullptr;
  }
  virtual Mod* asMod() {
    return nullptr;
  }
  virtual Function* asFunction() {
    return nullptr;
  }
};

class Type : public Item {
  std::string m_name;

protected:
  Type(ItemId id, ItemId parent, std::string comment, std::string name)
    : Item(id, parent, comment), m_name(name) {}

public:
  static ItemId fromClangTy(BindgenContext&, const clang::Type&);

  virtual Type* asType() override {
    return this;
  }
};

class Mod : public Item {
  std::string m_name;
  std::vector<ItemId> m_children;

public:
  void addChild(ItemId id) {
    m_children.push_back(id);
  }
  virtual Mod* asMod() override {
    return this;
  }
};

struct RecordField {
  std::string m_name;
  bool m_isConst;
  ItemId m_type;
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
              std::string comment,
              std::string name,
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
  static std::unique_ptr<Record> create(BindgenContext&,
                                        const clang::RecordDecl&);
};

}  // namespace bindgen
