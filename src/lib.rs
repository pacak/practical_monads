#![allow(dead_code)]

/// Let's make a simple parser:
///
/// 1. we are going to parse user input, to make things easier let's limit input to numbers and
///    have them come in a slice
/// 2. parser should provide helpful errors to user and is expected to operate on invalid input too
/// 3. we'll define a few operations and go though several iterations adding complexity to see what
///    breaks

mod rev1 {
    //! not actually taking input, ignoring errors

    // primitive parser you can run to produce a value
    fn num() -> usize {
        10
    }

    // a pair combinator that takes two parsers and gives a parser that consumed two items
    fn pair<F, FV, G, GV>(f: F, g: G) -> impl Fn() -> (FV, GV)
    where
        F: Fn() -> FV,
        G: Fn() -> GV,
    {
        move || (f(), g())
    }

    // a simple parser how user would do it
    fn parser() -> usize {
        let nested = pair(pair(num, num), num)();
        let single = num();

        single + nested.0 .0 + nested.0 .1 + nested.1
    }

    #[test]
    fn works() {
        let r = parser();
        assert_eq!(r, 40);
    }
}

// ------------------------------------------------------------------------------------------------------

mod rev2 {
    //! add handling errors

    fn num() -> Option<usize> {
        Some(10)
    }

    // a pair combinator
    fn pair<F, G, FV, GV>(f: F, g: G) -> impl Fn() -> Option<(FV, GV)>
    where
        F: Fn() -> Option<FV>,
        G: Fn() -> Option<GV>,
    {
        move || Some((f()?, g()?))
    }

    // a simple parser how user would do it
    fn parser() -> Option<usize> {
        let p = pair(num, num);
        let nested = pair(p, num)()?;
        let single = num()?;

        Some(single + nested.0 .0 + nested.0 .1 + nested.1)
    }

    #[test]
    fn works() {
        let r = parser();
        assert_eq!(r, Some(40));
    }
}

// ------------------------------------------------------------------------------------------------------

mod rev3 {
    //! actually consume some input

    fn num1() -> impl Fn(&[usize]) -> Option<(usize, &[usize])> {
        move |input| match input {
            [v, rest @ ..] => Some((*v, rest)),
            _ => None,
        }
    }

    fn num(input: &[usize]) -> Option<(usize, &[usize])> {
        match input {
            [v, rest @ ..] => Some((*v, rest)),
            _ => None,
        }
    }

    // a pair combinator
    fn pair<F, G, FV, GV>(f: F, g: G) -> impl Fn(&[usize]) -> Option<((FV, GV), &[usize])>
    where
        F: Fn(&[usize]) -> Option<(FV, &[usize])>,
        G: Fn(&[usize]) -> Option<(GV, &[usize])>,
    {
        move |input| {
            let (fv, input) = f(input)?;
            let (gv, input) = g(input)?;
            Some(((fv, gv), input))
        }
    }

    // a simple parser how user would do it
    fn parser(input: &[usize]) -> Option<usize> {
        let p = pair(num, num);
        let (nested, input) = pair(p, num)(input)?;

        let (single, _input) = num(input)?;

        Some(single + nested.0 .0 + nested.0 .1 + nested.1)
    }

    #[test]
    fn works() {
        let r = parser(&[1, 2, 3, 4]);
        assert_eq!(r, Some(10));
    }
}

// ------------------------------------------------------------------------------------------------------

mod rev4 {
    //! add some cosmetics.... Maybe.
    //! https://github.com/rust-lang/rust/issues/58052
    //! https://github.com/rust-lang/rust/issues/97362

    fn num(
        name: &'static str,
    ) -> impl for<'a> Fn(&'a [usize]) -> Result<(usize, &'a [usize]), &'static str> {
        fn annotate<F>(f: F) -> F
        where
            F: Fn(&[usize]) -> Result<(usize, &[usize]), &'static str>,
        {
            f
        }

        annotate(move |input: &[usize]| match input {
            [v, rest @ ..] => Ok((*v, rest)),
            _ => Err(name),
        })
    }
}

// ------------------------------------------------------------------------------------------------------

mod rev5 {
    // take a closer look at the question mark operator used in some context

    fn maybe_inc(a: Option<usize>) -> Option<usize> {
        Some(a? + 1)
    }

    // it consists of following parts:
    // - value in a container/context, here it's Option<a>
    // - something that takes an unpacked value and gives you a new Option:
    //   conceptually it's `|a| Some(a + 1)`
    //
    //   ? merges those parts together and gives Option<usize> back:
    //   `Option<usize>` combined with `Fn(usize) -> Option<usize>` gives `Option<usize>` back
    //
    //  Or in general
    //  `F<A>` + `Fn(A)->(B)` gives `F<B>` back

    // you've seen this operation as `and_then`

    fn maybe_and_then(a: Option<usize>) -> Option<usize> {
        #[allow(clippy::bind_instead_of_map)]
        a.and_then(|a| Some(a + 1))
    }

    fn result_and_then(a: Result<usize, String>) -> Result<usize, String> {
        #[allow(clippy::bind_instead_of_map)]
        a.and_then(|a| Ok(a + 1))
    }
}

/*
mod rev6 {
    #[allow(clippy::type_complexity)]
    struct Parser<T>(Box<dyn for<'a> Fn(&'a [usize]) -> Result<(T, &'a [usize]), &'static str>>);

    impl<T: 'static> Parser<T> {
        // running it easy as well
        fn run<'a>(&self, input: &'a [usize]) -> Result<(T, &'a [usize]), &'static str> {
            (self.0)(input)
        }

        fn bind<R>(self, second: impl Fn(T) -> Parser<R> + 'static) -> Parser<R> {
            Parser(Box::new(move |input| {
                self.0(input).and_then(|(t, input)| second(t).0(input))
            }))
        }
    }

    // implementing such parser is easy
    fn num(name: &'static str) -> Parser<usize> {
        Parser(Box::new(move |input: &[usize]| match input {
            [v, rest @ ..] => Ok((*v, rest)),
            _ => Err(name),
        }))
    }

    fn pair<F, G>(_f: Parser<F>, _g: Parser<G>) -> Parser<(F, G)> {
        todo!("this should be doable with some persuasion")
    }
}
*/

mod rev6 {
    // and_then is limited to a specific type and is not very exciting to compose
    // On the other hand there are iterators

    pub trait Iter {
        type Item;

        // fn next(&mut self) -> Option<Self::Item>; // <- user must provide that

        // fn rev(self) -> Rev<Self> { ... }
        // fn skip(self, n: usize) -> Skip<Self> { ... }
        // fn take(self, n: usize) -> Take<Self> { ... }
        // fn filter<P>(self, predicate: P) -> Filter<Self, P> {..}

        // fn sum(self) -> S {..}
        // fn product(self) -> P {..}
        // fn nth(&mut self) -> Option<Self::Item> {..}
    }

    impl<T> Iter for Vec<T> {
        type Item = T;
    }
}
mod rev7 {
    use core::marker::PhantomData;
    // to make this work similarly to iterators we want a trait with some operations
    // plus some types that implement it.
    //
    trait Parser<T: Clone>: Clone {
        // Most interesting operation we care about is bind:
        //
        // that takes `impl Parser<T>` and `Fn(T) -> impl Parser<T>`
        // and gives something that implements Parser<T> back.
        fn bind<F, B, N>(&self, f: F) -> Bind<Self, F, T, N, B>
        where
            F: Fn(T) -> N, // N = impl Parser<B>
            N: Parser<B>,
            Self: Sized + Parser<T>,
            B: Clone,
        {
            // bind simply stores function and the parser plus keeps the context to keep
            Bind {
                from: self.clone(),
                to: f,
                ctx: PhantomData,
            }
        }

        // second operation we care about is something that runs the parser -
        // equivalent operation in Iterator is `next` and user needs to provide those.
        fn run<'a>(&self, input: &'a [usize]) -> Result<(T, &'a [usize]), &'static str>;
    }

    #[derive(Clone)]
    struct Bind<S, F, A, N, B> {
        from: S,
        to: F,
        ctx: PhantomData<(N, A, B)>,
    }

    impl<S, F, A, N, B> Parser<B> for Bind<S, F, A, N, B>
    where
        F: Fn(A) -> N + Clone,
        N: Parser<B>,
        S: Parser<A>,
        B: Clone,
        A: Clone,
    {
        fn run<'a>(&self, input: &'a [usize]) -> Result<(B, &'a [usize]), &'static str> {
            let (a, input) = self.from.run(input)?;
            (self.to)(a).run(input)
        }
    }

    // second useful operation would be "pure" - something that takes a value and puts it into
    // monadic context. In `and_then`  and `?` those were `Ok` and `Some`, since we are working
    // with traits - it can be a new type
    //
    // bind operation looks like this:
    // Parser<A> + Fn(A) -> Parser<B>  => Parser<B>
    //
    // `Parser<A>` is usually present and users will have to come up with something of shape
    // `Fn(A) -> Parser<B>` or even `Fn(A) -> Parser<A>` - `Pure` can do just that

    #[derive(Clone)]
    struct Pure<T>(T);

    impl<T: Clone> Parser<T> for Pure<T> {
        fn run<'a>(&self, input: &'a [usize]) -> Result<(T, &'a [usize]), &'static str> {
            Ok((self.0.clone(), input))
        }
    }

    // Now we can make a simple parser that takes a single input

    #[derive(Clone, Copy)]
    struct Num(&'static str /* field name used for error messages */);

    // and implement a Parser for that
    impl Parser<usize> for Num {
        fn run<'a>(&self, input: &'a [usize]) -> Result<(usize, &'a [usize]), &'static str> {
            match input {
                [v, rest @ ..] => Ok((*v, rest)),
                _ => Err(self.0),
            }
        }
    }

    // now we want syntax. ? comes with magical support from the compiler
    // for bind we are going to steal it from Haskell
    // a? binds result to some nameless expression, we are going to bind
    // it to a named variable using `<-` operator, we'll call it bind
    //
    //     result <- monadic_expression;
    //
    // also we want to support regular expressions:
    //
    //     let result = non_monadic_expression;
    //
    // and tailing return syntax.

    macro_rules! mdo {
        ($pat:ident <- $mexpr:expr ; $($rest:tt)*) => {{
            $mexpr.bind(move |$pat| { mdo!($($rest)*) })
        }};

        (_  <- $mexpr:expr ; $($rest:tt)*) => {
            $mexpr.bind(move |_| { mdo!($($rest)*) })
        };

        ($stmt:stmt ; $($rest:tt)*) => {{
            $stmt;
            mdo!($($rest)*)
        }};

        ($a:expr) => {
            Pure($a)
        }
    }

    fn pair<F, FV, G, GV>(f: F, g: G) -> impl Parser<(FV, GV)>
    where
        F: Parser<FV>,
        G: Parser<GV>,
        GV: Copy,
        FV: Copy,
    {
        mdo! {
            fv <- f;
            gv <- g;
            (fv, gv)
        }
    }

    fn foo() -> impl Parser<(usize, usize, usize)> {
        mdo! {
            a <- Num("a");
            b <- Num("b");
            c <- Num("c");
            (a, b, c)
        }
    }

    fn parser(input: &[usize]) -> Result<usize, &'static str> {
        Ok(mdo! {
            nested <- mdo! {
                ab <- foo();
                c <- Num("c");
                (ab, c)
            };
            single <- Num("x");
            single + nested.0.0 + nested.0.1 + nested.1
        }
        .run(input)?
        .0)
    }

    #[test]
    fn this_works() {
        let r = parser(&[1, 2, 3, 4, 5, 6]);
        assert_eq!(r, Ok(12));
    }
}
