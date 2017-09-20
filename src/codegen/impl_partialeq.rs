
use ir::comp::{CompKind, Field, FieldMethods};
use ir::context::BindgenContext;
use ir::item::{Item, ItemCanonicalName, IsOpaque};
use ir::ty::{TypeKind, RUST_DERIVE_IN_ARRAY_LIMIT};
use quote;

pub fn gen_partialeq_impl(ctx: &BindgenContext, fields: &[Field], item: &Item, kind: CompKind) -> Option<quote::Tokens> {
    let _ty = item.expect_type();    
    if item.is_opaque(ctx, &()) {
        // We can't generate PartialEq for opaque types.
        return None;
    }

    if kind == CompKind::Union {
        // Don't know how to generate PartialEq for Union
        return None;
    }

    let canonical_name = item.canonical_name(ctx);
    let canonical_ident = ctx.rust_ident(&canonical_name);

    let mut tokens = vec![];
    for field in fields {
        match *field {
            Field::DataMember(ref fd) => {
                let item = ctx.resolve_item(fd.ty());
                let name = match fd.name() {
                    Some(name) => name,
                    None => {
                        // Bitfield stuff
                        return None
                    }
                };
                match gen_field(ctx, item, name) {
                    Some(t) => tokens.push(t),
                    None => return None,
                }
            }
            Field::Bitfields(_) => {
                // We don't know how to generate Bitfields
                return None;
            }
        }
    }

    Some(quote! {
        fn eq(&self, other: & #canonical_ident) -> bool {
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
            // TODO: What if t is not debug?
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
