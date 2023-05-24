{-# LANGUAGE KindSignatures #-}

module Parser1 where

data Result err a = Ok a | Err err deriving (Eq, Ord, Show)


type FailK (ka :: *) (st :: *) (err :: *) = err -> st -> ka

type SuccessK (ka :: *) (env :: *) (st :: *) (err :: *) (a :: *) = a -> FailK ka st err -> env -> st -> ka


newtype Parser1 (ka :: *) (env :: *) (st :: *) (err :: *) (a :: *) = 
    Parser1 (SuccessK ka env st err a -> FailK ka st err -> env -> st -> ka)

runParser1 :: Parser1 (Result (st, err) (st, a)) env st err a -> env -> st -> Result (st, err) (st, a)
runParser1 p env st =
    let fk = \err st2 -> Err (st2, err) in
    let sk = \a _ _ st1 -> Ok (st1, a) in
    let Parser1(pf) = p in
    pf sk fk env st


apply1 :: Parser1 ka env st err a -> SuccessK ka env st err a -> FailK ka st err -> env -> st -> ka
apply1 p sk fk env st = 
    let Parser1(pf) = p in pf sk fk env st

point :: a -> Parser1 ka env st err a
point x =  Parser1(\sk fk env st -> sk x fk env st)

ap :: Parser1 ka env st err (a -> b) -> Parser1 ka env st err a -> Parser1 ka env st err b
ap pf pa = 
    Parser1(\sk fk env st ->
        let sk1 = \f fk1 env1 st1 -> let sk2 = \a fk2 env2 st2 -> sk (f a) fk2 env2 st2 in 
                apply1 pa sk2 fk1 env1 st1 in 
            apply1 pf sk1 fk env st
    )

flatMap :: (a -> Parser1 ka env st err b) -> Parser1 ka env st err a -> Parser1 ka env st err b
flatMap k p = 
    Parser1(\sk fk env st ->
        let sk1 = \x fk1 env1 st1 -> apply1 (k x) sk fk1 env1 st1 in
        apply1 p sk1 fk env st
    )


throwError :: err -> Parser1 ka env st err a
throwError err = Parser1(\_ fk _ st -> fk err st)


catchError :: Parser1 ka env st err a -> (err -> Parser1 ka env st err a) -> Parser1 ka env st err a
catchError p handler = 
    Parser1(\sk fk env st ->
        let fk1 = \err st1 -> apply1 (handler err) sk fk env st1 in
        apply1 p sk fk1 env st
    )

mapError :: (err -> err) -> Parser1 ka env st err a -> Parser1 ka env st err a
mapError f ma = 
    Parser1(\ sk fk env st ->
        let fk1 = \err st2 -> fk (f err) st2 in 
        let sk1 = \x _ _ st1 -> sk x fk env st1 in 
        apply1 ma sk1 fk1 env st
    )


flatMapOr :: Parser1 ka env st err a 
                -> (a -> Parser1 ka env st err b) 
                -> (err -> Parser1 ka env st err b) -> Parser1 ka env st err b
flatMapOr p pnext pelse = 
    Parser1(\ sk fk env st ->
        let fk1 = \err st2 -> apply1 (pelse err) sk fk env st2 in
        let sk1 = \x _ _ st1 -> apply1 (pnext x) sk fk env st1 in 
        apply1 p sk1 fk1 env st
    )

-- restores state updated by first parser (Okasaki)
lookahead :: Parser1 ka env st err a -> (a -> Parser1 ka env st err b) -> Parser1 ka env st err b
lookahead p k = 
    Parser1(\sk fk env st ->
        let sk1 = \x fk1 env1 _ -> apply1 (k x) sk fk1 env1 st in
        apply1 p sk1 fk env st
    )

-- restore state on failure (Okasaki)
tryParse :: Parser1 ka env st err a -> Parser1 ka env st err a
tryParse p = 
    Parser1(\sk fk env st ->
        let fk1 = \err _ -> fk err st in 
        apply1 p sk fk env st
    )



ask :: Parser1 ka env st err env
ask = Parser1(\ sk fk env st -> sk env fk env st)

asks :: (env -> a) -> Parser1 ka env st err a
asks f = Parser1(\ sk fk env st -> sk (f env) fk env st)

local :: (env -> env) -> Parser1 ka env st err a -> Parser1 ka env st err a
local f p = 
    Parser1(\ sk fk env st ->
        let sk1 = \x fk1 _ st1 -> sk x fk1 env st1 in
        apply1 p sk1 fk (f env) st
    )

get :: Parser1 ka env st err st
get = Parser1(\ sk fk env st -> sk st fk env st)

gets :: (st -> a) -> Parser1 ka env st err a
gets f = Parser1(\ sk fk env st -> sk (f st) fk env st)

put :: st -> Parser1 ka env st err ()
put st1 = Parser1(\ sk fk env st -> sk () fk env st1)

puts :: (st -> st) -> Parser1 ka env st err ()
puts f = Parser1(\ sk fk env st -> sk () fk env (f st))


-- Testing...

type ErrMsg = String

runSimple :: Parser1 (Result (Int, ErrMsg) (Int, a)) String Int String a -> String -> (Result (Int, ErrMsg) (Int, a))
runSimple p input = runParser1 p input 0

test_ap01 :: Bool
test_ap01 = runSimple (ap (point (\x -> x + 1)) (point 10)) "input string" == Ok (0, 11)


test_local01 :: Bool
test_local01 = runSimple (local (\_ -> "INPUT2") ask) "input string" == Ok (0, "INPUT2")


