use bound::{BoundPattern, PatternSubsts, ScopeState};
use var::{BoundVar, FreeVar, PatternIndex};

/// Multiple parallel binding 0
///
/// Contrast with `Nest`, where each subsequent binder is bound by the previous
/// binders.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Multi<P>(pub Vec<P>);

impl<P> BoundPattern for Multi<P>
where
    P: BoundPattern,
{
    fn pattern_eq(&self, other: &Multi<P>) -> bool {
        self.0.len() == other.0.len()
            && <_>::zip(self.0.iter(), other.0.iter()).all(|(lhs, rhs)| P::pattern_eq(lhs, rhs))
    }

    fn freshen(&mut self) -> PatternSubsts<FreeVar> {
        // FIXME: intermediate allocations
        PatternSubsts::new(self.0.iter_mut().flat_map(P::freshen).collect())
    }

    fn rename(&mut self, perm: &PatternSubsts<FreeVar>) {
        assert_eq!(self.0.len(), perm.len()); // FIXME: assertion

        for (pattern, perm) in <_>::zip(self.0.iter_mut(), perm.iter()) {
            pattern.rename(&PatternSubsts::new(vec![perm.clone()])); // FIXME: clone
        }
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        for elem in &mut self.0 {
            elem.close_pattern(state, pattern);
        }
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        for elem in &mut self.0 {
            elem.open_pattern(state, pattern);
        }
    }

    fn on_free(&self, state: ScopeState, name: &FreeVar) -> Option<BoundVar> {
        self.0
            .iter()
            .enumerate()
            .filter_map(|(i, pattern)| {
                pattern.on_free(state, name).map(|bound| {
                    assert_eq!(bound.pattern, PatternIndex(0));
                    BoundVar {
                        pattern: PatternIndex(i as u32),
                        ..bound
                    }
                })
            })
            .next()
    }

    fn on_bound(&self, state: ScopeState, name: BoundVar) -> Option<FreeVar> {
        self.0.get(name.pattern.0 as usize).and_then(|pattern| {
            pattern.on_bound(
                state,
                BoundVar {
                    pattern: PatternIndex(0),
                    ..name
                },
            )
        })
    }
}
