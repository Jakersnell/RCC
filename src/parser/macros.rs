macro_rules! is {
    ($invoker:ident, $current_or_next:ident,  $pattern:pat $(if $guard:expr)? $(,)?) => {
        $invoker.$current_or_next.as_ref().is_some_and(|locatable| matches!(&locatable.value, $pattern $(if $guard)?))
    };
}
pub(super) use is;

macro_rules! match_token {
    ($invoker:ident, $current_or_next:ident, $closure:expr, $pattern:pat $(if $guard:expr)? => $if_ok:expr) => {
        $invoker.$current_or_next.as_ref().map(|next|{
            #[allow(clippy::redundant_closure_call)]
            match $closure(&next.value) {
                $pattern $(if $guard)? => Some(Locatable{
                    location: next.location,
                    value: $if_ok
                }),
                _ => None
            }
        }).flatten()
    };
    ($invoker:ident, $current_or_next:ident, $pattern:pat $(if $guard:expr)? => $if_ok:expr) => {
        match_token!($invoker, $current_or_next, |x|{x}, $pattern $(if $guard)? => $if_ok)
    };
    ($invoker:ident, $current_or_next:ident, $closure:expr, $pattern:pat $(if $guard:expr)?) => {
        match_token!($invoker, $current_or_next, $closure, $pattern $(if $guard)? => Some($if_ok))
    };
    ($invoker:ident, $current_or_next:ident, $pattern:pat $(if $guard:expr)?) => {
        match_token!($invoker, $current_or_next, $pattern $(if $guard)? => Some(()))
    };

}
pub(super) use match_token;

macro_rules! confirm {

    (
        $invoker:ident,
        consume,
        $closure:expr,
        $pattern:pat $(if $guard:expr)? => $if_ok:expr,
        $if_err:literal
    ) => {{
        let locatable = $invoker.consume()?;
        let location = locatable.location;
        let value = locatable.value;
        confirm!($invoker, value, location, $closure, $pattern $(if $guard)? => $if_ok, $if_err)
    }};

    (
        $invoker:ident,
        consume,
        $pattern:pat $(if $guard:expr)? => $if_ok:expr,
        $if_err:literal
    ) => {
        confirm!( $invoker, consume, |x| {x}, $pattern $(if $guard)? => $if_ok, $if_err)
    };

    (
        $invoker:ident,
        consume,
        $pattern:pat $(if $guard:expr)?,
        $if_err:literal
    ) => {
        confirm!( $invoker, consume, |x| {x}, $pattern $(if $guard)? => (), $if_err)
    };

    (
        $invoker:ident,
        borrow,
        $closure:expr,
        $pattern:pat $(if $guard:expr)? => $if_ok:expr,
        $if_err:literal
    ) => {{
        let locatable = &$invoker.current.as_ref().unwrap();
        let value = &locatable.value;
        let location = locatable.location;
        confirm!($invoker, value, location, $closure, $pattern $(if $guard)? => $if_ok, $if_err)
    }};

        (
        $invoker:ident,
        borrow,
        $pattern:pat $(if $guard:expr)? => $if_ok:expr,
        $if_err:literal
    ) => {
        confirm!( $invoker, borrow, |x| {x}, $pattern $(if $guard)? => $if_ok, $if_err)
    };

    (
        $invoker:ident,
        borrow,
        $pattern:pat $(if $guard:expr)?,
        $if_err:literal
    ) => {
        confirm!( $invoker, borrow, |x| {x}, $pattern $(if $guard)? => (), $if_err)
    };

    (
        $invoker:ident,
        $value:ident,
        $location:ident,
        $closure:expr,
        $pattern:pat $(if $guard:expr)? => $if_ok:expr,
        $if_err:literal
    ) => {{
        let formatted = format!("{:#?}", $value);
        #[allow(clippy::redundant_closure_call)]
        match $closure($value) {
            $pattern $(if $guard)? => Ok(Locatable::new($location, $if_ok)),
            _ => {
                $invoker.report_error(CompilerError::ExpectedVariety($if_err.to_string(), formatted, $location));
                Err(())
            }
        }
    }};
}
pub(super) use confirm;
