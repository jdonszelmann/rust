use rustc_ast::AttrId;
use rustc_data_structures::stable_hasher::{HashStable, StableHasher, ToStableHashKey};
use rustc_macros::{Decodable, Encodable};
use rustc_span::def_id::DefPathHash;

/// Requirements for a `StableHashingContext` to be used in this crate.
/// This is a hack to allow using the `HashStable_Generic` derive macro
/// instead of implementing everything in `rustc_middle`.
pub trait HashStableContext: rustc_ast::HashStableContext + rustc_abi::HashStableContext {}

/// Each lint expectation has a `LintExpectationId` assigned by the `LintLevelsBuilder`.
/// Expected diagnostics get the lint level `Expect` which stores the `LintExpectationId`
/// to match it with the actual expectation later on.
///
/// The `LintExpectationId` has to be stable between compilations, as diagnostic
/// instances might be loaded from cache. Lint messages can be emitted during an
/// `EarlyLintPass` operating on the AST and during a `LateLintPass` traversing the
/// HIR tree. The AST doesn't have enough information to create a stable id. The
/// `LintExpectationId` will instead store the [`AttrId`] defining the expectation.
/// These `LintExpectationId` will be updated to use the stable [`HirId`] once the
/// AST has been lowered. The transformation is done by the `LintLevelsBuilder`
///
/// Each lint inside the `expect` attribute is tracked individually, the `lint_index`
/// identifies the lint inside the attribute and ensures that the IDs are unique.
///
/// The index values have a type of `u16` to reduce the size of the `LintExpectationId`.
/// It's reasonable to assume that no user will define 2^16 attributes on one node or
/// have that amount of lints listed. `u16` values should therefore suffice.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Encodable, Decodable)]
pub enum LintExpectationId {
    /// Used for lints emitted during the `EarlyLintPass`. This id is not
    /// hash stable and should not be cached.
    Unstable { attr_id: AttrId, lint_index: Option<u16> },
    /// The [`HirId`] that the lint expectation is attached to. This id is
    /// stable and can be cached. The additional index ensures that nodes with
    /// several expectations can correctly match diagnostics to the individual
    /// expectation.
    Stable { hir_id: HirId, attr_index: u16, lint_index: Option<u16> },
}

impl LintExpectationId {
    pub fn is_stable(&self) -> bool {
        match self {
            LintExpectationId::Unstable { .. } => false,
            LintExpectationId::Stable { .. } => true,
        }
    }

    pub fn get_lint_index(&self) -> Option<u16> {
        let (LintExpectationId::Unstable { lint_index, .. }
        | LintExpectationId::Stable { lint_index, .. }) = self;

        *lint_index
    }

    pub fn set_lint_index(&mut self, new_lint_index: Option<u16>) {
        let (LintExpectationId::Unstable { lint_index, .. }
        | LintExpectationId::Stable { lint_index, .. }) = self;

        *lint_index = new_lint_index
    }
}

impl<HCX: HashStableContext> HashStable<HCX> for LintExpectationId {
    #[inline]
    fn hash_stable(&self, hcx: &mut HCX, hasher: &mut StableHasher) {
        match self {
            LintExpectationId::Stable { hir_id, attr_index, lint_index: Some(lint_index) } => {
                hir_id.hash_stable(hcx, hasher);
                attr_index.hash_stable(hcx, hasher);
                lint_index.hash_stable(hcx, hasher);
            }
            _ => {
                unreachable!(
                    "HashStable should only be called for filled and stable `LintExpectationId`"
                )
            }
        }
    }
}

impl<HCX: HashStableContext> ToStableHashKey<HCX> for LintExpectationId {
    type KeyType = (DefPathHash, ItemLocalId, u16, u16);

    #[inline]
    fn to_stable_hash_key(&self, hcx: &HCX) -> Self::KeyType {
        match self {
            LintExpectationId::Stable { hir_id, attr_index, lint_index: Some(lint_index) } => {
                let (def_path_hash, lint_idx) = hir_id.to_stable_hash_key(hcx);
                (def_path_hash, lint_idx, *attr_index, *lint_index)
            }
            _ => {
                unreachable!("HashStable should only be called for a filled `LintExpectationId`")
            }
        }
    }
}
