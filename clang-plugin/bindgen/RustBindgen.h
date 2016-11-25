#pragma once

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "BindgenItem.h"

#include <cstdint>
#include <set>
#include <string>

namespace clang {
class CompilerInstance;
}

namespace bindgen {

class BindgenContext;

class IRGenerator : public clang::ASTConsumer {
  clang::CompilerInstance& m_compilerInstance;
  std::set<std::unique_ptr<Item>> m_items;

public:
  IRGenerator(clang::CompilerInstance& instance)
    : m_compilerInstance(instance) {}

  void HandleTranslationUnit(clang::ASTContext&) override;
};

}  // namespace bindgen
