
use ir::comp::{CompInfo, CompKind, Field, FieldMethods};
use ir::context::BindgenContext;
use ir::item::{Item, IsOpaque};
use ir::ty::{TypeKind, RUST_DERIVE_IN_ARRAY_LIMIT};
use quote;

pub fn gen_partialeq_impl(ctx: &BindgenContext, comp_info: &CompInfo, item: &Item, ty_for_impl: &quote::Tokens) -> Option<quote::Tokens> {
    let _ty = item.expect_type();    
    if item.is_opaque(ctx, &()) {
        // TODO: We can't generate PartialEq for opaque types.
        panic!();
    }

    let mut tokens = vec![];

    if comp_info.kind() == CompKind::Union {
        // TODO: Is other cases possible?
        tokens.push(quote! {
            &self.bindgen_union_field[..] == &other.bindgen_union_field[..]
        });
    } else {
        for (i, base) in comp_info.base_members().iter().enumerate() {
            // TODO: see quirks in codegen/mod.rs
            let ty_item = ctx.resolve_item(base.ty);

            // TODO: Move base field name generation into either in IR or in context.
            let field_name = if i == 0 {
                "_base".into()
            } else {
                format!("_base_{}", i)
            };
            match gen_field(ctx, ty_item, &field_name) {
                Some(t) => tokens.push(t),
                None => return None,
            }
        }

        for field in comp_info.fields() {
            match *field {
                Field::DataMember(ref fd) => {
                    let item = ctx.resolve_item(fd.ty());
                    let name = match fd.name() {
                        Some(name) => name,
                        None => {
                            // TODO: Bitfield stuff
                            panic!()
                        }
                    };
                    match gen_field(ctx, item, name) {
                        Some(t) => tokens.push(t),
                        None => return None,
                    }
                }
                Field::Bitfields(_) => {
                    // TODO: We don't know how to generate Bitfields
                    panic!();
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

fn gen_field(ctx: &BindgenContext, item: &Item, name: &str) -> Option<quote::Tokens> {
    let name_ident = ctx.rust_ident(name);

    let ty = match item.as_type() {
        Some(ty) => ty,
        None => {
            panic!();
            // return None;
        }
    };

    fn quote_equals(name_ident: quote::Ident) -> Option<quote::Tokens> {
        Some(quote! {
            self.#name_ident == other.#name_ident
        })
    }

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
        TypeKind::Comp(..) => {
            quote_equals(name_ident)
        }

        TypeKind::Array(_t, len) => {
            if len <= RUST_DERIVE_IN_ARRAY_LIMIT {
                quote_equals(name_ident)
            } else {
                Some(quote! {
                    &self. #name_ident [..] == &other. #name_ident [..]
                })
            }
        }

        TypeKind::ResolvedTypeRef(t) |
        TypeKind::TemplateAlias(t, _) |
        TypeKind::Alias(t) => {
            let inner_item = ctx.resolve_item(t);
            gen_field(ctx, inner_item, name)
        }

        TypeKind::Pointer(_) => {
            quote_equals(name_ident)
        }

        ref other => {
            // TODO: List explicitly
            // Not supported
            panic!("{:?}", other);
            // return None;
        }
    }
}
