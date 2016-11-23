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
  static ItemId findParent(const clang::Decl& decl);

  static bool create(BindgenContext&, const clang::Decl&, ItemId& out);

  ItemId id() const {
    return m_id;
  }

  ItemId parent() const {
    return m_parent;
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
  ItemId m_type;
};

}  // namespace bindgen
