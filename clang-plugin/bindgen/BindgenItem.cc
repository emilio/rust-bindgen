#include "BindgenItem.h"

#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"

namespace bindgen {

using clang::RecordDecl;
using clang::CXXRecordDecl;

std::unique_ptr<Item> Item::create(BindgenContext& ctx,
                                   const clang::Decl& decl) {
  if (const RecordDecl* recordDecl = clang::dyn_cast<RecordDecl>(&decl))
    return Record::create(ctx, *recordDecl);

  return nullptr;
}

std::unique_ptr<Record> Record::create(BindgenContext& ctx,
                                       const RecordDecl& decl) {
  std::vector<RecordField> fields;
  ctx.registerType(*decl.getTypeForDecl());
  for (auto* field : decl.fields()) {
    const clang::QualType type = field->getType();
    const ItemId bindgenType = Type::fromClangTy(ctx, *type.getTypePtr());

    fields.push_back(RecordField{
        field->getNameAsString(), type.isConstQualified(), bindgenType,
    });
  }
  // TODO
  return nullptr;
}

}  // namespace bindgen
