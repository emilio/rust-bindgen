#pragma once

#include <cstdint>
#include <memory>
#include <unordered_map>

namespace clang {
class Type;
class ASTContext;
class CompilerInstance;
class QualType;
}

namespace bindgen {

class Item;
typedef std::size_t ItemId;

class BindgenContext {
  clang::CompilerInstance& m_compilerInstance;
  clang::ASTContext& m_context;
  std::unordered_map<const clang::Type*, ItemId> m_registeredTypes;
  std::unordered_map<ItemId, std::unique_ptr<Item>> m_items;
  ItemId m_lastItemId;

  void registerBuiltins();

public:
  BindgenContext(clang::ASTContext&, clang::CompilerInstance&);
  ItemId nextItemId() {
    return ++m_lastItemId;
  }

  const clang::ASTContext& context() const {
    return m_context;
  }

  ItemId registerType(const clang::Type&);
  void addItem(std::unique_ptr<Item>);
  Item& getItem(ItemId id);
  bool getRegisteredType(const clang::Type&, ItemId& out) const;
  bool getBuiltinOrRegisteredTy(const clang::Type&, ItemId& out) const;
  ItemId maybeBuildWrapperForQualTy(ItemId wrapping, clang::QualType);
};

}  // namespace bindgen
