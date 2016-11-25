#include "BindgenItem.h"
#include "BindgenType.h"

#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"

namespace bindgen {

using clang::CXXRecordDecl;
using clang::RecordDecl;
using clang::TypeDecl;
using clang::QualType;
using clang::FunctionDecl;
using clang::NamespaceDecl;

bool Item::create(BindgenContext& ctx, const clang::Decl& decl, ItemId& out) {
  if (const TypeDecl* tyDecl = clang::dyn_cast<TypeDecl>(&decl)) {
    const QualType ty = ctx.context().getTypeDeclType(tyDecl);
    assert(!ty.isNull());
    if (ty.isNull())
      return false;

    return Type::fromClangTy(ctx, ty, out);
  }

  if (/* const FunctionDecl* fnDecl = */ clang::dyn_cast<FunctionDecl>(&decl))
    return false;  // TODO

  if (/* const NamespaceDecl* nsDecl = */ clang::dyn_cast<NamespaceDecl>(&decl))
    return false;  // TODO

  return false;
}

}  // namespace bindgen
