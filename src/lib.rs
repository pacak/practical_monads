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

    // primitive parser
    fn num() -> usize {
        10
    }

    // a pair combinator
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

mod rev5 {
    // take a closer look at bind

    fn maybe_inc(a: Option<usize>) -> Option<usize> {
        let a = a?;
        Some(a + 1)
    }

    fn maybe_inc2(a: Option<usize>) -> Option<usize> {
        #[allow(clippy::bind_instead_of_map)]
        a.and_then(|a| Some(a + 1))
    }

    // in both cases `?` and `and_then` take  `a: Option<usize>` and `Fn(usize) -> Option<usize>`
    // and produce Option<usize> back, let's see
}

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

mod rev7 {
    use core::marker::PhantomData;

    trait Parser<T> {
        fn bind<F, B, N>(self, f: F) -> Bind<Self, F, T, N, B>
        where
            F: Fn(T) -> N,
            N: Parser<B>,
            Self: Sized + Parser<T>,
        {
            Bind {
                from: self,
                to: f,
                ctx: PhantomData,
            }
        }

        fn run<'a>(&self, input: &'a [usize]) -> Result<(T, &'a [usize]), &'static str>;
    }

    struct Bind<S, F, A, N, B> {
        from: S,
        to: F,
        ctx: PhantomData<(N, A, B)>,
    }

    impl<S, F, A, N, B> Parser<B> for Bind<S, F, A, N, B>
    where
        F: Fn(A) -> N,
        N: Parser<B>,
        S: Parser<A>,
    {
        fn run<'a>(&self, input: &'a [usize]) -> Result<(B, &'a [usize]), &'static str> {
            let (a, input) = self.from.run(input)?;
            (self.to)(a).run(input)
        }
    }

    struct Pure<T>(T);
    impl<T: Clone> Parser<T> for Pure<T> {
        fn run<'a>(&self, input: &'a [usize]) -> Result<(T, &'a [usize]), &'static str> {
            Ok((self.0.clone(), input))
        }
    }

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

            // xxx.bind(|val|    ......
            Pure($a)
        }
    }

    struct Num(&'static str);
    impl Parser<usize> for Num {
        fn run<'a>(&self, input: &'a [usize]) -> Result<(usize, &'a [usize]), &'static str> {
            match input {
                [v, rest @ ..] => Ok((*v, rest)),
                _ => Err(self.0),
            }
        }
    }

    fn pair<F, FV, G, GV>(f: F, g: G) -> impl Parser<(FV, GV)>
    where
        F: Parser<FV>,
        G: Parser<GV>,
        GV: Clone,
        FV: Clone,
    {
        f.bind(move |fv| g.bind(move |gv| Pure((fv.clone(), gv.clone()))))

        //        mdo! {
        //            fv <- f;
        //            gv <- g;
        //            (fv, gv)
        //        }
    }

    fn parser(input: &[usize]) -> Result<usize, &'static str> {
        Ok(mdo! {
            nested <- mdo! {
                ab <- mdo! {
                    a <- Num("a");
                    b <- Num("b");
                    (a, b)
                };
                c <- Num("c");
                (ab, c)
            };
            single <- Num("x");
            single + nested.0.0 + nested.0.1 + nested.1
        }
        .run(input)?
        .0)
    }
}

// introduce trait
