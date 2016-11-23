#include "BindgenType.h"
#include "BindgenContext.h"

#include "clang/AST/AST.h"

namespace bindgen {

bool Type::fromClangTy(BindgenContext& ctx, const clang::Type& ty, ItemId& out) {
  if (ctx.getRegisteredType(ty, out))
    return true;

  // That should've covered integer types, builtins, etc.
  assert(!ty.isBuiltinType());

  if (ty.isPointerType()) {
    ItemId inner;
    if (!fromClangTy(ctx, ty.getPointeeType(), inner))
      return false;
    out = ctx.nextItemId();
    ctx.addItem(std::make_unique<PointerType>(out, inner));
    return true;
  }

  if (ty.isArrayType()) {
    ItemId inner;
    const clang::ArrayType* asArray = clang::dyn_cast<clang::ArrayType>(&ty);
    assert(asArray);

    if (!fromClangTy(ctx, asArray->getElementType(), inner))
      return false;

    size_t length = 0;
    if (const clang::ConstantArrayType* constArr = clang::dyn_cast<clang::ConstantArrayType>(asArray))
      length = constArr->getSize().getZExtValue();

    out = ctx.nextItemId();
    ctx.addItem(std::make_unique<ArrayType>(out, inner, length));
    return true;
  }

  if (const clang::RecordType* record = clang::dyn_cast<clang::RecordType>(&ty))
    return Record::fromClangTy(ctx, *record, out);

  return false;
}

bool Type::fromClangTy(BindgenContext& ctx, const clang::QualType& ty, ItemId& out) {
  assert(!ty.isNull());

  if (!fromClangTy(ctx, ty, out))
    return false;

  out = ctx.maybeBuildWrapperForQualTy(out, ty);
  return true;
}

bool Record::fromClangTy(BindgenContext& ctx, const clang::RecordType& ty, ItemId& out) {
  std::vector<RecordField> fields;
  // This one can be recursive, and point to ourselves, so register us and let
  // it assign us an id.
  out = ctx.registerType(ty);

  const clang::RecordDecl& decl = *ty.getDecl();
  for (auto* field : decl.fields()) {
    ItemId inner;
    bool found = Type::fromClangTy(ctx, field->getType(), inner);
    assert(found);

    fields.push_back(RecordField{
        field->getNameAsString(), inner,
    });
  }

  // FIXME(emilio): Comment.
  ItemId parent = Item::findParent(decl);
  ctx.addItem(std::make_unique<Record>(out, parent, "", decl.getNameAsString(), fields));
  return true;
}

}  // namespace bindgen
