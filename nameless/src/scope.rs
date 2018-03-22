use {Pattern, ScopeState, Term};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope<P, T> {
    pub unsafe_pattern: P,
    pub unsafe_body: T,
}

impl<P, T> Scope<P, T>
where
    P: Pattern,
    T: Term<Free = P::Free>,
{
    pub fn bind(pattern: P, mut body: T) -> Scope<P, T> {
        body.close_term(ScopeState::new(), &pattern);

        Scope {
            unsafe_pattern: pattern,
            unsafe_body: body,
        }
    }
}

impl<P, T> Term for Scope<P, T>
where
    P: Pattern,
    T: Term<Free = P::Free>,
{
    type Free = P::Free;

    fn term_eq(&self, other: &Scope<P, T>) -> bool {
        P::pattern_eq(&self.unsafe_pattern, &other.unsafe_pattern)
            && T::term_eq(&self.unsafe_body, &other.unsafe_body)
    }

    fn close_term<P1>(&mut self, state: ScopeState, pattern: &P1)
    where
        P1: Pattern<Free = P::Free>,
    {
        self.unsafe_pattern.close_pattern(state, pattern);
        self.unsafe_body.close_term(state.incr(), pattern);
    }

    fn open_term<P1>(&mut self, state: ScopeState, pattern: &P1)
    where
        P1: Pattern<Free = P::Free>,
    {
        self.unsafe_pattern.open_pattern(state, pattern);
        self.unsafe_body.open_term(state.incr(), pattern);
    }
}

/// Unbind a scope, returning the freshened pattern and body
pub fn unbind<P, T>(scope: Scope<P, T>) -> (P, T)
where
    P: Pattern,
    T: Term<Free = P::Free>,
{
    let mut pattern = scope.unsafe_pattern;
    let mut body = scope.unsafe_body;

    pattern.freshen();
    body.open_term(ScopeState::new(), &pattern);

    (pattern, body)
}

pub fn unbind2<P1, T1, P2, T2>(scope1: Scope<P1, T1>, scope2: Scope<P2, T2>) -> (P1, T1, P2, T2)
where
    P1: Pattern,
    T1: Term<Free = P1::Free>,
    P2: Pattern<Free = P1::Free>,
    T2: Term<Free = P1::Free>,
{
    let mut scope1_pattern = scope1.unsafe_pattern;
    let mut scope1_body = scope1.unsafe_body;
    let mut scope2_pattern = scope2.unsafe_pattern;
    let mut scope2_body = scope2.unsafe_body;

    {
        let names = scope1_pattern.freshen();
        scope2_pattern.rename(&names);
        scope1_body.open_term(ScopeState::new(), &scope1_pattern);
        scope2_body.open_term(ScopeState::new(), &scope2_pattern);
    }

    (scope1_pattern, scope1_body, scope2_pattern, scope2_body)
}
