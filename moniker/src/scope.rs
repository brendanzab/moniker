use bound::{BoundPattern, BoundTerm, ScopeState};
use var::Var;

/// A bound scope
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope<P, T> {
    /// The pattern that binds the body of the scope
    ///
    /// You can access this directly, but only if you understand what you are
    /// doing! Prefer calling `Scope::unbind` instead.
    pub unsafe_pattern: P,
    /// The body of the scope
    ///
    /// You can access this directly, but only if you understand what you are
    /// doing! Prefer calling `Scope::unbind` instead.
    pub unsafe_body: T,
}

impl<P, T> Scope<P, T> {
    /// Create a new scope by binding a term with the given pattern
    pub fn new<Ident>(pattern: P, mut body: T) -> Scope<P, T>
    where
        P: BoundPattern<Ident>,
        T: BoundTerm<Ident>,
    {
        body.close_term(ScopeState::new(), &pattern);

        Scope {
            unsafe_pattern: pattern,
            unsafe_body: body,
        }
    }

    /// Unbind a term, returning the freshened pattern and body
    pub fn unbind<Ident>(self) -> (P, T)
    where
        P: BoundPattern<Ident>,
        T: BoundTerm<Ident>,
    {
        let mut pattern = self.unsafe_pattern;
        let mut body = self.unsafe_body;

        pattern.freshen();
        body.open_term(ScopeState::new(), &pattern);

        (pattern, body)
    }

    /// Simultaneously unbind two terms
    ///
    /// The fresh names in the first pattern with be used for the second pattern
    pub fn unbind2<Ident, P2, T2>(self, other: Scope<P2, T2>) -> (P, T, P2, T2)
    where
        P: BoundPattern<Ident>,
        T: BoundTerm<Ident>,
        P2: BoundPattern<Ident>,
        T2: BoundTerm<Ident>,
    {
        let mut self_pattern = self.unsafe_pattern;
        let mut self_body = self.unsafe_body;
        let mut other_pattern = other.unsafe_pattern;
        let mut other_body = other.unsafe_body;

        {
            let names = self_pattern.freshen();
            other_pattern.swaps(&names);
            self_body.open_term(ScopeState::new(), &self_pattern);
            other_body.open_term(ScopeState::new(), &other_pattern);
        }

        (self_pattern, self_body, other_pattern, other_body)
    }
}

impl<Ident, P, T> BoundTerm<Ident> for Scope<P, T>
where
    P: BoundPattern<Ident>,
    T: BoundTerm<Ident>,
{
    fn term_eq(&self, other: &Scope<P, T>) -> bool {
        P::pattern_eq(&self.unsafe_pattern, &other.unsafe_pattern)
            && T::term_eq(&self.unsafe_body, &other.unsafe_body)
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.unsafe_pattern.close_pattern(state, pattern);
        self.unsafe_body.close_term(state.incr(), pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<Ident>) {
        self.unsafe_pattern.open_pattern(state, pattern);
        self.unsafe_body.open_term(state.incr(), pattern);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<Ident>)) {
        self.unsafe_body.visit_vars(on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<Ident>)) {
        self.unsafe_body.visit_mut_vars(on_var);
    }
}
