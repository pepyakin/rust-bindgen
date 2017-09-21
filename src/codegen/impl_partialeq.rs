
use ir::comp::{CompInfo, CompKind, Field, FieldMethods};
use ir::context::BindgenContext;
use ir::item::Item;
use ir::ty::{TypeKind, RUST_DERIVE_IN_ARRAY_LIMIT};
use quote;

pub fn gen_partialeq_impl(ctx: &BindgenContext, comp_info: &CompInfo, item: &Item, ty_for_impl: &quote::Tokens) -> Option<quote::Tokens> {
    let _ty = item.expect_type();    
    let mut tokens = vec![];

    if comp_info.kind() == CompKind::Union {
        // TODO: Is other cases are possible?
        tokens.push(quote! {
            &self.bindgen_union_field[..] == &other.bindgen_union_field[..]
        });
    } else {
        for (i, base) in comp_info.base_members().iter().enumerate() {
            // TODO: Move base field name generation into either in IR or in context.
            if base.is_virtual() {
                continue;
            }

            let base_ty = ctx.resolve_type(base.ty);
            // NB: We won't include unsized types in our base chain because they
            // would contribute to our size given the dummy field we insert for
            // unsized types.
            if base_ty.is_unsized(ctx, &base.ty) {
                continue;
            }

            let field_name = if i == 0 {
                "_base".into()
            } else {
                format!("_base_{}", i)
            };

            let ty_item = ctx.resolve_item(base.ty);
            tokens.push(gen_field(ctx, ty_item, &field_name));
        }

        for field in comp_info.fields() {
            match *field {
                Field::DataMember(ref fd) => {
                    let ty_item = ctx.resolve_item(fd.ty());
                    let name = match fd.name() {
                        Some(name) => name,
                        None => {
                            // TODO: Bitfield stuff
                            warn!("can't process field {:?} in {:?} with type {:?}", fd.name(), item.id(), ty_item.id());
                            return None;
                        }
                    };
                    tokens.push(gen_field(ctx, ty_item, name));
                }
                Field::Bitfields(ref bu) => {
                    for bitfield in bu.bitfields() {
                        let name_ident = ctx.rust_ident_raw(bitfield.name());
                        tokens.push(quote! {
                            self.#name_ident () == other.#name_ident ()
                        });
                    }
                }
            }
        }
    }    

    Some(quote! {
        fn eq(&self, other: & #ty_for_impl) -> bool {
            #( #tokens )&&*
        }
    })
}

fn gen_field(ctx: &BindgenContext, item: &Item, name: &str) -> quote::Tokens {
    fn quote_equals(name_ident: quote::Ident) -> quote::Tokens {
        quote! { self.#name_ident == other.#name_ident }
    }

    let name_ident = ctx.rust_ident(name);
    let ty = item.expect_type();

    match *ty.kind() {
        TypeKind::Void |
        TypeKind::NullPtr |
        TypeKind::Int(..) |
        TypeKind::Complex(..) |
        TypeKind::Float(..) |
        TypeKind::Enum(..) |
        TypeKind::TypeParam |
        TypeKind::UnresolvedTypeRef(..) |
        TypeKind::BlockPointer |
        TypeKind::Reference(..) |
        TypeKind::ObjCInterface(..) |
        TypeKind::ObjCId |
        TypeKind::ObjCSel |
        TypeKind::Comp(..) |
        TypeKind::Pointer(_) |
        TypeKind::Function(..) |
        TypeKind::TemplateInstantiation(..) |
        TypeKind::Opaque => {
            quote_equals(name_ident)
        }

        TypeKind::Array(_t, len) => {
            if len <= RUST_DERIVE_IN_ARRAY_LIMIT {
                quote_equals(name_ident)
            } else {
                quote! {
                    &self. #name_ident [..] == &other. #name_ident [..]
                }
            }
        }

        TypeKind::ResolvedTypeRef(t) |
        TypeKind::TemplateAlias(t, _) |
        TypeKind::Alias(t) => {
            let inner_item = ctx.resolve_item(t);
            gen_field(ctx, inner_item, name)
        }
    }
}
