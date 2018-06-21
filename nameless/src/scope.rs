use {BoundPattern, BoundTerm, ScopeState, Var};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope<P, T> {
    pub unsafe_pattern: P,
    pub unsafe_body: T,
}

impl<P, T> Scope<P, T>
where
    P: BoundPattern,
    T: BoundTerm,
{
    /// Create a new scope by binding a term with the given pattern
    pub fn new(pattern: P, mut body: T) -> Scope<P, T> {
        body.close_term(ScopeState::new(), &pattern);

        Scope {
            unsafe_pattern: pattern,
            unsafe_body: body,
        }
    }

    /// Unbind a term, returning the freshened pattern and body
    pub fn unbind(self) -> (P, T) {
        let mut pattern = self.unsafe_pattern;
        let mut body = self.unsafe_body;

        pattern.freshen();
        body.open_term(ScopeState::new(), &pattern);

        (pattern, body)
    }

    /// Simultaneously unbind two terms
    ///
    /// The fresh names in the first pattern with be used for the second pattern
    pub fn unbind2<P2, T2>(self, other: Scope<P2, T2>) -> (P, T, P2, T2)
    where
        P2: BoundPattern,
        T2: BoundTerm,
    {
        let mut self_pattern = self.unsafe_pattern;
        let mut self_body = self.unsafe_body;
        let mut other_pattern = other.unsafe_pattern;
        let mut other_body = other.unsafe_body;

        {
            let names = self_pattern.freshen();
            other_pattern.rename(&names);
            self_body.open_term(ScopeState::new(), &self_pattern);
            other_body.open_term(ScopeState::new(), &other_pattern);
        }

        (self_pattern, self_body, other_pattern, other_body)
    }
}

impl<P, T> BoundTerm for Scope<P, T>
where
    P: BoundPattern,
    T: BoundTerm,
{
    fn term_eq(&self, other: &Scope<P, T>) -> bool {
        P::pattern_eq(&self.unsafe_pattern, &other.unsafe_pattern)
            && T::term_eq(&self.unsafe_body, &other.unsafe_body)
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        self.unsafe_pattern.close_pattern(state, pattern);
        self.unsafe_body.close_term(state.incr(), pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern) {
        self.unsafe_pattern.open_pattern(state, pattern);
        self.unsafe_body.open_term(state.incr(), pattern);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var)) {
        self.unsafe_body.visit_vars(on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var)) {
        self.unsafe_body.visit_mut_vars(on_var);
    }
}
