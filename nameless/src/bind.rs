use {BoundPattern, BoundTerm, ScopeState};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bind<P, T> {
    pub unsafe_pattern: P,
    pub unsafe_body: T,
}

impl<P, T> BoundTerm for Bind<P, T>
where
    P: BoundPattern,
    T: BoundTerm,
{
    fn term_eq(&self, other: &Bind<P, T>) -> bool {
        P::pattern_eq(&self.unsafe_pattern, &other.unsafe_pattern)
            && T::term_eq(&self.unsafe_body, &other.unsafe_body)
    }

    fn close_term<P1: BoundPattern>(&mut self, state: ScopeState, pattern: &P1) {
        self.unsafe_pattern.close_pattern(state, pattern);
        self.unsafe_body.close_term(state.incr(), pattern);
    }

    fn open_term<P1: BoundPattern>(&mut self, state: ScopeState, pattern: &P1) {
        self.unsafe_pattern.open_pattern(state, pattern);
        self.unsafe_body.open_term(state.incr(), pattern);
    }
}

/// Bind a term with the given pattern
pub fn bind<P, T>(pattern: P, mut body: T) -> Bind<P, T>
where
    P: BoundPattern,
    T: BoundTerm,
{
    body.close_term(ScopeState::new(), &pattern);

    Bind {
        unsafe_pattern: pattern,
        unsafe_body: body,
    }
}

/// Unbind a term, returning the freshened pattern and body
pub fn unbind<P, T>(term: Bind<P, T>) -> (P, T)
where
    P: BoundPattern,
    T: BoundTerm,
{
    let mut pattern = term.unsafe_pattern;
    let mut body = term.unsafe_body;

    pattern.freshen();
    body.open_term(ScopeState::new(), &pattern);

    (pattern, body)
}

/// Simultaneously unbind two terms
///
/// The fresh names in the first pattern with be used for the second pattern
pub fn unbind2<P1, T1, P2, T2>(term1: Bind<P1, T1>, term2: Bind<P2, T2>) -> (P1, T1, P2, T2)
where
    P1: BoundPattern,
    T1: BoundTerm,
    P2: BoundPattern,
    T2: BoundTerm,
{
    let mut term1_pattern = term1.unsafe_pattern;
    let mut term1_body = term1.unsafe_body;
    let mut term2_pattern = term2.unsafe_pattern;
    let mut term2_body = term2.unsafe_body;

    {
        let names = term1_pattern.freshen();
        term2_pattern.rename(&names);
        term1_body.open_term(ScopeState::new(), &term1_pattern);
        term2_body.open_term(ScopeState::new(), &term2_pattern);
    }

    (term1_pattern, term1_body, term2_pattern, term2_body)
}
