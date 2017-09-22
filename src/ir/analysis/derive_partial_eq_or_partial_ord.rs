//! Determining which types for which we can emit `#[derive(PartialEq)]`.

use super::{ConstrainResult, MonotoneFramework, generate_dependencies};
use ir::comp::CompKind;
use ir::comp::Field;
use ir::comp::FieldMethods;
use ir::context::{BindgenContext, ItemId};
use ir::derive::{CanTriviallyDerivePartialEqOrPartialOrd, CanDerive, CantDeriveReason};
use ir::item::IsOpaque;
use ir::traversal::EdgeKind;
use ir::ty::RUST_DERIVE_IN_ARRAY_LIMIT;
use ir::ty::TypeKind;
use std::collections::HashMap;

/// An analysis that finds for each IR item whether partialeq or partialord cannot be derived.
///
/// We use the monotone constraint function `cannot_derive_partialeq_or_partialord`, defined as
/// follows:
///
/// * If T is Opaque and layout of the type is known, get this layout as opaque
///   type and check whether it can be derived using trivial checks.
/// * If T is Array type, partialeq or partialord cannot be derived if the length of
///   the array is larger than the limit or the type of data the array contains cannot derive
///   partialeq or partialord.
/// * If T is a type alias, a templated alias or an indirection to another type,
///   partialeq or partialord cannot be derived if the type T refers to cannot be derived partialeq or partialord.
/// * If T is a compound type, partialeq or partialord cannot be derived if any of its base member
///   or field cannot be derived partialeq or partialord.
/// * If T is a pointer, T cannot be derived partialeq or partialord if T is a function pointer
///   and the function signature cannot be derived partialeq or partialord.
/// * If T is an instantiation of an abstract template definition, T cannot be
///   derived partialeq or partialord if any of the template arguments or template definition
///   cannot derive partialeq or partialord.
#[derive(Debug, Clone)]
pub struct CannotDerivePartialEqOrPartialOrd<'ctx> {
    ctx: &'ctx BindgenContext,

    // The incremental result of this analysis's computation. Everything in this
    // set cannot derive partialeq or partialord.
    cannot_derive_partialeq_or_partialord: HashMap<ItemId, CantDeriveReason>,

    // Dependencies saying that if a key ItemId has been inserted into the
    // `cannot_derive_partialeq_or_partialord` set, then each of the ids
    // in Vec<ItemId> need to be considered again.
    //
    // This is a subset of the natural IR graph with reversed edges, where we
    // only include the edges from the IR graph that can affect whether a type
    // can derive partialeq or partialord.
    dependencies: HashMap<ItemId, Vec<ItemId>>,
}

impl<'ctx> CannotDerivePartialEqOrPartialOrd<'ctx> {
    fn consider_edge(kind: EdgeKind) -> bool {
        match kind {
            // These are the only edges that can affect whether a type can derive
            // partialeq or partialord.
            EdgeKind::BaseMember |
            EdgeKind::Field |
            EdgeKind::TypeReference |
            EdgeKind::VarType |
            EdgeKind::TemplateArgument |
            EdgeKind::TemplateDeclaration |
            EdgeKind::TemplateParameterDefinition => true,

            EdgeKind::Constructor |
            EdgeKind::Destructor |
            EdgeKind::FunctionReturn |
            EdgeKind::FunctionParameter |
            EdgeKind::InnerType |
            EdgeKind::InnerVar |
            EdgeKind::Method => false,
            EdgeKind::Generic => false,
        }
    }

    fn insert(&mut self, id: ItemId, reason: CantDeriveReason) -> ConstrainResult {
        trace!("inserting {:?} into the cannot_derive_partialeq_or_partialord because {:?}", id, reason);
        self.cannot_derive_partialeq_or_partialord.insert(id, reason);
        ConstrainResult::Changed
    }
}

impl<'ctx> MonotoneFramework for CannotDerivePartialEqOrPartialOrd<'ctx> {
    type Node = ItemId;
    type Extra = &'ctx BindgenContext;
    type Output = HashMap<ItemId, CantDeriveReason>;

    fn new(
        ctx: &'ctx BindgenContext,
    ) -> CannotDerivePartialEqOrPartialOrd<'ctx> {
        let cannot_derive_partialeq_or_partialord = HashMap::new();
        let dependencies = generate_dependencies(ctx, Self::consider_edge);

        CannotDerivePartialEqOrPartialOrd {
            ctx,
            cannot_derive_partialeq_or_partialord,
            dependencies,
        }
    }

    fn initial_worklist(&self) -> Vec<ItemId> {
        self.ctx.whitelisted_items().iter().cloned().collect()
    }

    fn constrain(&mut self, id: ItemId) -> ConstrainResult {
        trace!("constrain: {:?}", id);

        if Some(CantDeriveReason::Other) ==
            self.cannot_derive_partialeq_or_partialord.get(&id).cloned()
        {
            trace!("    already know it cannot derive PartialEq or PartialOrd");
            return ConstrainResult::Same;
        }

        let item = self.ctx.resolve_item(id);
        let ty = match item.as_type() {
            Some(ty) => ty,
            None => {
                trace!("    not a type; ignoring");
                return ConstrainResult::Same;
            }
        };

        if self.ctx.no_partialeq_by_name(&item) {
            return self.insert(id, CantDeriveReason::Other);
        }

        trace!("ty: {:?}", ty);
        if item.is_opaque(self.ctx, &()) {
            if ty.is_union() && self.ctx.options().rust_features().untagged_union() {
                trace!("    cannot derive PartialEq or PartialOrd for Rust unions");
                return self.insert(id, CantDeriveReason::Other);
            }

            let layout_can_derive = ty.layout(self.ctx)
                .map_or(CanDerive::No(CantDeriveReason::Other), |l| {
                    l.opaque().can_trivially_derive_partialeq_or_partialord()
                });

            return match layout_can_derive {
                CanDerive::Yes => {
                    trace!("    we can trivially derive PartialEq or PartialOrd for the layout");
                    ConstrainResult::Same
                }
                CanDerive::No(reason) => {
                    trace!("    we cannot derive PartialEq or PartialOrd for the layout");
                    self.insert(id, reason)
                }
            };
        }

        if ty.layout(self.ctx).map_or(false, |l| {
            l.align > RUST_DERIVE_IN_ARRAY_LIMIT
        })
        {
            // We have to be conservative: the struct *could* have enough
            // padding that we emit an array that is longer than
            // `RUST_DERIVE_IN_ARRAY_LIMIT`. If we moved padding calculations
            // into the IR and computed them before this analysis, then we could
            // be precise rather than conservative here.
            return self.insert(id, CantDeriveReason::ArrayTooLarge);
        }

        match *ty.kind() {
            // Handle the simple cases. These can derive partialeq without further
            // information.
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
            TypeKind::ObjCSel => {
                trace!("    simple type that can always derive PartialEq or PartialOrd");
                ConstrainResult::Same
            }

            TypeKind::Array(t, len) => {
                if self.cannot_derive_partialeq_or_partialord.contains_key(&t) {
                    trace!(
                        "    arrays of T for which we cannot derive PartialEq or PartialOrd \
                            also cannot derive PartialEq or PartialOrd"
                    );
                    return self.insert(id, CantDeriveReason::Other);
                }

                if len <= RUST_DERIVE_IN_ARRAY_LIMIT {
                    trace!("    array is small enough to derive PartialEq or PartialOrd");
                    ConstrainResult::Same
                } else {
                    trace!("    array is too large to derive PartialEq or PartialOrd");
                    self.insert(id, CantDeriveReason::ArrayTooLarge)
                }
            }

            TypeKind::Pointer(inner) => {
                let inner_type =
                    self.ctx.resolve_type(inner).canonical_type(self.ctx);
                if let TypeKind::Function(ref sig) = *inner_type.kind() {
                    if let CanDerive::No(_) = sig.can_trivially_derive_partialeq_or_partialord() {
                        trace!(
                            "    function pointer that can't trivially derive PartialEq or PartialOrd"
                        );
                        return self.insert(id, CantDeriveReason::Other);
                    }
                }
                trace!("    pointers can derive PartialEq");
                ConstrainResult::Same
            }

            TypeKind::Function(ref sig) => {
                if let CanDerive::No(_) = sig.can_trivially_derive_partialeq_or_partialord() {
                    trace!(
                        "    function that can't trivially derive PartialEq or PartialOrd"
                    );
                    return self.insert(id, CantDeriveReason::Other);
                }
                trace!("    function can derive PartialEq or PartialOrd");
                ConstrainResult::Same
            }

            TypeKind::ResolvedTypeRef(t) |
            TypeKind::TemplateAlias(t, _) |
            TypeKind::Alias(t) => {
                let reason =
                    self.cannot_derive_partialeq_or_partialord.get(&t).cloned();
                if let Some(reason) = reason {
                    trace!(
                        "    aliases and type refs to T which cannot derive \
                            PartialEq or PartialOrd also cannot derive PartialEq or PartialOrd"
                    );
                    self.insert(id, reason)
                } else {
                    trace!(
                        "    aliases and type refs to T which can derive \
                            PartialEq or PartialOrd can also derive PartialEq or PartialOrd"
                    );
                    ConstrainResult::Same
                }
            }

            TypeKind::Comp(ref info) => {
                assert!(
                    !info.has_non_type_template_params(),
                    "The early ty.is_opaque check should have handled this case"
                );

                if info.kind() == CompKind::Union {
                    if self.ctx.options().rust_features().untagged_union() {
                        trace!("    cannot derive PartialEq or PartialOrd for Rust unions");
                        return self.insert(id, CantDeriveReason::Other);
                    }

                    let layout_can_derive = ty.layout(self.ctx)
                        .map_or(CanDerive::No(CantDeriveReason::Other), |l| {
                            l.opaque().can_trivially_derive_partialeq_or_partialord()
                        });
                    return match layout_can_derive {
                        CanDerive::Yes => {
                            trace!(
                                "    union layout can trivially derive PartialEq or PartialOrd"
                            );
                            ConstrainResult::Same
                        },
                        CanDerive::No(reason) => {
                            trace!("    union layout cannot derive PartialEq or PartialOrd");
                            self.insert(id, reason)
                        }
                    };
                }

                let bases_cannot_derive: Vec<CantDeriveReason> =
                    info.base_members().iter().filter_map(|base| {
                        if !self.ctx.whitelisted_items().contains(&base.ty) {
                            return Some(CantDeriveReason::Other);
                        }
                        self.cannot_derive_partialeq_or_partialord.get(
                            &base.ty
                        ).cloned()
                    }).collect();
                if let Some(reason) = choose_reason(&bases_cannot_derive) {
                    trace!(
                        "    base members cannot derive PartialEq or PartialOrd, so we can't \
                            either"
                    );
                    return self.insert(id, reason);
                }

                let fields_cannot_derive_reasons: Vec<CantDeriveReason> = 
                    info.fields().iter().filter_map(|f| match *f {
                        Field::DataMember(ref data) => {
                            if !self.ctx.whitelisted_items().contains(
                                &data.ty(),
                            ) {
                                return Some(CantDeriveReason::Other);
                            }
                            self.cannot_derive_partialeq_or_partialord.get(
                                &data.ty(),
                            ).cloned()
                        }
                        Field::Bitfields(ref bfu) => {
                            if bfu.layout().align > RUST_DERIVE_IN_ARRAY_LIMIT {
                                trace!(
                                    "   we cannot derive PartialEq for a bitfield larger then \
                                        the limit"
                                );
                                return Some(CantDeriveReason::Other);
                            }
                            let reasons: Vec<CantDeriveReason> = bfu.bitfields().iter().filter_map(|b| {
                                if !self.ctx.whitelisted_items().contains(
                                    &b.ty(),
                                ) {
                                    return Some(CantDeriveReason::Other);
                                }
                                self.cannot_derive_partialeq_or_partialord.get(
                                    &b.ty(),
                                ).cloned()
                            }).collect();
                            choose_reason(&reasons)
                        }
                    }).collect();
                if let Some(reason) = choose_reason(&fields_cannot_derive_reasons) {
                    trace!(
                        "    fields cannot derive PartialEq or PartialOrd, so we can't either"
                    );
                    return self.insert(id, reason);
                }

                trace!("    comp can derive PartialEq");
                ConstrainResult::Same
            }

            TypeKind::TemplateInstantiation(ref template) => {
                let args_cannot_derive_reasons: Vec<CantDeriveReason> = template
                    .template_arguments()
                    .iter()
                    .filter_map(|arg| {
                        self.cannot_derive_partialeq_or_partialord
                            .get(&arg)
                            .cloned()
                    })
                    .collect();
                if let Some(reason) = choose_reason(&args_cannot_derive_reasons) {
                    trace!(
                        "    template args cannot derive PartialEq or PartialOrd, so \
                            insantiation can't either"
                    );
                    return self.insert(id, reason);
                }

                assert!(
                    !template.template_definition().is_opaque(self.ctx, &()),
                    "The early ty.is_opaque check should have handled this case"
                );
                let def_cannot_derive_reason =
                    self.cannot_derive_partialeq_or_partialord
                        .get(&template.template_definition())
                        .cloned();
                if let Some(def_cannot_derive_reason) = def_cannot_derive_reason
                {
                    trace!(
                        "    template definition cannot derive PartialEq or PartialOrd, so \
                            insantiation can't either"
                    );
                    return self.insert(id, def_cannot_derive_reason);
                }

                trace!("    template instantiation can derive PartialEq or PartialOrd");
                ConstrainResult::Same
            }

            TypeKind::Opaque => {
                unreachable!(
                    "The early ty.is_opaque check should have handled this case"
                )
            }
        }
    }

    fn each_depending_on<F>(&self, id: ItemId, mut f: F)
    where
        F: FnMut(ItemId),
    {
        if let Some(edges) = self.dependencies.get(&id) {
            for item in edges {
                trace!("enqueue {:?} into worklist", item);
                f(*item);
            }
        }
    }
}

impl<'ctx> From<CannotDerivePartialEqOrPartialOrd<'ctx>> for HashMap<ItemId, CantDeriveReason> {
    fn from(analysis: CannotDerivePartialEqOrPartialOrd<'ctx>) -> Self {
        analysis.cannot_derive_partialeq_or_partialord
    }
}

/// Chooses the most appropriate CantDeriveReason from provided list. 
/// Returns None if list is empty.
fn choose_reason(reasons: &[CantDeriveReason]) -> Option<CantDeriveReason> {
    if reasons.is_empty() {
       return None; 
    }
    let all_is_too_large = reasons
        .iter()
        .all(|r| *r == CantDeriveReason::ArrayTooLarge);
    Some(if all_is_too_large {
        CantDeriveReason::ArrayTooLarge
    } else {
        CantDeriveReason::Other
    })
}
