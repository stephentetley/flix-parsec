{-# LANGUAGE KindSignatures #-}

module Parser1 where


type FailK (ka :: *) (st :: *) (err :: *) = st -> err -> ka

type SuccessK (ka :: *) (env :: *) (st :: *) (err :: *) (a :: *) = a -> FailK ka st err -> env -> st -> ka

newtype Parser1 (ka :: *) (env :: *) (st :: *) (err :: *) (a :: *) = 
    Parser1 (SuccessK ka env st err a -> FailK ka st err -> env -> st -> ka)

apply1 :: Parser1 ka env st err a -> SuccessK ka env st err a -> FailK ka st err -> env -> st -> ka
apply1 p sk fk env st = 
    let Parser1(pf) = p in pf sk fk env st

point :: a -> Parser1 ka env st err a
point x =  Parser1(\sk fk env st -> sk x fk env st)

ap :: Parser1 ka env st err (a -> b) -> Parser1 ka env st err a -> Parser1 ka env st err b
ap pf pa = 
    Parser1(\sk fk env st ->
        let sk1 = \f fk1 env1 st1 -> let sk2 = \a fk2 env2 st2 -> sk (f a) fk2 env2 st2 in apply1 pa sk2 fk1 env1 st1 in apply1 pf sk1 fk env st
    )

flatMap :: (a -> Parser1 ka env st err b) -> Parser1 ka env st err a -> Parser1 ka env st err b
flatMap k p = 
    Parser1(\sk fk env st ->
        let sk1 = \x fk1 env1 st1 -> apply1 (k x) sk fk1 env1 st1 in
        apply1 p sk1 fk env st
    )


throwError :: err -> Parser1 ka env st err a
throwError err = Parser1(\_ fk _ st -> fk st err)


mapError :: (err -> err) -> Parser1 ka env st err a -> Parser1 ka env st err a
mapError f ma = 
    Parser1(\ sk fk env st ->
        let fk1 = \pos err -> fk pos (f err) in 
        let sk1 = \x _ _ st1 -> sk x fk env st1 in 
        apply1 ma sk1 fk1 env st
    )

