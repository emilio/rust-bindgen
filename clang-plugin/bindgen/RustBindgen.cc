#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"

#include "RustBindgen.h"
#include "BindgenContext.h"
#include "BindgenItem.h"

namespace bindgen {

using clang::ASTConsumer;
using clang::CompilerInstance;
using clang::PluginASTAction;
using clang::dyn_cast;
using clang::CXXRecordDecl;
using clang::RecordDecl;

IRGenerator::IRGenerator(clang::CompilerInstance& instance)
  : m_compilerInstance(instance) {}

void IRGenerator::HandleTranslationUnit(clang::ASTContext& astContext) {
  BindgenContext ctx(astContext, m_compilerInstance);
}

class BindgenAction : public PluginASTAction {
protected:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance&,
                                                 llvm::StringRef) override {
    return nullptr;
  }

  bool ParseArgs(const CompilerInstance&,
                 const std::vector<std::string>&) override {
    return true;
  }
};

}  // namespace bindgen
