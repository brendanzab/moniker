use {AlphaEq, Pattern, ScopeState, Term};

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
        body.close_term(&pattern);

        Scope {
            unsafe_pattern: pattern,
            unsafe_body: body,
        }
    }
}

impl<P: AlphaEq, T: AlphaEq> AlphaEq for Scope<P, T> {
    fn alpha_eq(&self, other: &Scope<P, T>) -> bool {
        P::alpha_eq(&self.unsafe_pattern, &other.unsafe_pattern)
            && T::alpha_eq(&self.unsafe_body, &other.unsafe_body)
    }
}

impl<P, T> Term for Scope<P, T>
where
    P: Pattern,
    T: Term<Free = P::Free>,
{
    type Free = P::Free;

    fn close_term_at<P1>(&mut self, state: ScopeState, pattern: &P1)
    where
        P1: Pattern<Free = P::Free>,
    {
        self.unsafe_pattern.close_pattern_at(state, pattern);
        self.unsafe_body.close_term_at(state.incr(), pattern);
    }

    fn open_term_at<P1>(&mut self, state: ScopeState, pattern: &P1)
    where
        P1: Pattern<Free = P::Free>,
    {
        self.unsafe_pattern.open_pattern_at(state, pattern);
        self.unsafe_body.open_term_at(state.incr(), pattern);
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
    body.open_term(&pattern);

    (pattern, body)
}

pub fn unbind2<P1, T1, P2, T2>(scope1: Scope<P1, T1>, scope2: Scope<P2, T2>) -> (P1, T1, P2, T2)
where
    P1: Pattern,
    T1: Term<Free = P1::Free>,
    P2: Pattern<Free = P1::Free, NamePerm = P1::NamePerm>,
    T2: Term<Free = P1::Free>,
{
    let mut scope1_pattern = scope1.unsafe_pattern;
    let mut scope1_body = scope1.unsafe_body;
    let mut scope2_pattern = scope2.unsafe_pattern;
    let mut scope2_body = scope2.unsafe_body;

    {
        let names = scope1_pattern.freshen();
        scope2_pattern.rename(&names);
        scope1_body.open_term(&scope1_pattern);
        scope2_body.open_term(&scope2_pattern);
    }

    (scope1_pattern, scope1_body, scope2_pattern, scope2_body)
}
