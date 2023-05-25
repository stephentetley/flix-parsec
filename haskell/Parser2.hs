{-# LANGUAGE KindSignatures #-}

-- The `apply` helper of Parser1 has been inlined.

module Parser2 where

data Result err a = Ok a | Err err deriving (Eq, Ord, Show)


type FailK (ka :: *) (st :: *) (err :: *) = err -> st -> ka

type SuccessK (ka :: *) (env :: *) (st :: *) (err :: *) (a :: *) = a -> FailK ka st err -> env -> st -> ka


newtype Parser (ka :: *) (env :: *) (st :: *) (err :: *) (a :: *) = 
    Parser (SuccessK ka env st err a -> FailK ka st err -> env -> st -> ka)

runParser :: Parser (Result (st, err) (st, a)) env st err a -> env -> st -> Result (st, err) (st, a)
runParser p env st =
    let fk = \err st2 -> Err (st2, err) in
    let sk = \a _ _ st1 -> Ok (st1, a) in
    let Parser(pf) = p in
    pf sk fk env st


map :: (a -> b) -> Parser ka env st err a -> Parser ka env st err b
map f pa = 
    Parser(\ sk fk env st ->
        let Parser(paf) = pa in
        let sk1 = \x fk1 env1 st1 -> sk (f x) fk1 env1 st1 in 
        paf sk1 fk env st
    )


point :: a -> Parser ka env st err a
point x =  Parser(\sk fk env st -> sk x fk env st)

ap :: Parser ka env st err (a -> b) -> Parser ka env st err a -> Parser ka env st err b
ap pf pa = 
    Parser(\sk fk env st ->
        let Parser(pff) = pf in 
        let Parser(paf) = pa in 
        let sk1 = \f fk1 env1 st1 -> let sk2 = \a fk2 env2 st2 -> sk (f a) fk2 env2 st2 in 
                paf sk2 fk1 env1 st1 in 
            pff sk1 fk env st
    )

flatMap :: (a -> Parser ka env st err b) -> Parser ka env st err a -> Parser ka env st err b
flatMap k p = 
    Parser(\sk fk env st ->
        let Parser(pf) = p in 
        let sk1 = \x fk1 env1 st1 -> let Parser(kf) = k x in kf sk fk1 env1 st1 in
        pf sk1 fk env st
    )


alt :: Parser ka env st err a -> Parser ka env st err a -> Parser ka env st err a
alt p q = 
    Parser(\sk fk env st ->
        let Parser(pf) = p in 
        let Parser(qf) = q in 
        let fk1 = \_ _ -> qf sk fk env st in
        pf sk fk env st
    )


throwError :: err -> Parser ka env st err a
throwError err = Parser(\_ fk _ st -> fk err st)


catchError :: Parser ka env st err a -> (err -> Parser ka env st err a) -> Parser ka env st err a
catchError p handler = 
    Parser(\sk fk env st ->
        let Parser(pf) = p in 
        let fk1 = \err st1 -> let Parser(hf) = handler err in hf sk fk env st1 in
        pf sk fk1 env st
    )

mapError :: (err -> err) -> Parser ka env st err a -> Parser ka env st err a
mapError f p = 
    Parser(\ sk fk env st ->
        let Parser(pf) = p in 
        let fk1 = \err st2 -> fk (f err) st2 in 
        pf sk fk1 env st
    )


flatMapOr :: Parser ka env st err a 
                -> (a -> Parser ka env st err b) 
                -> (err -> Parser ka env st err b) -> Parser ka env st err b
flatMapOr p pnext pelse = 
    Parser(\ sk fk env st ->
        let Parser(pf) = p in 
        let fk2 = \err st2 -> let Parser(elsef) = pelse err in elsef sk fk env st2 in
        let sk1 = \x fk1 env1 st1 -> let Parser(nextf) = pnext x in nextf sk fk1 env1 st1 in 
        pf sk1 fk2 env st
    )

-- restores state updated by first parser (Okasaki)
lookahead :: Parser ka env st err a -> (a -> Parser ka env st err b) -> Parser ka env st err b
lookahead p k = 
    Parser(\sk fk env st ->
        let Parser(pf) = p in
        let sk1 = \x fk1 env1 _ -> let Parser(kf) = k x in kf sk fk1 env1 st in
        pf sk1 fk env st
    )

-- restore state on failure (Okasaki)
tryParse :: Parser ka env st err a -> Parser ka env st err a
tryParse p = 
    Parser(\sk fk env st ->
        let Parser(pf) = p in
        let fk1 = \err _ -> fk err st in 
        pf sk fk env st
    )



ask :: Parser ka env st err env
ask = Parser(\ sk fk env st -> sk env fk env st)

asks :: (env -> a) -> Parser ka env st err a
asks f = Parser(\ sk fk env st -> sk (f env) fk env st)

local :: (env -> env) -> Parser ka env st err a -> Parser ka env st err a
local f p = 
    Parser(\ sk fk env st ->        
        let Parser(pf) = p in
        let sk1 = \x fk1 _ st1 -> sk x fk1 env st1 in
        pf sk1 fk (f env) st
    )

get :: Parser ka env st err st
get = Parser(\ sk fk env st -> sk st fk env st)

gets :: (st -> a) -> Parser ka env st err a
gets f = Parser(\ sk fk env st -> sk (f st) fk env st)

put :: st -> Parser ka env st err ()
put st1 = Parser(\ sk fk env st -> sk () fk env st1)

puts :: (st -> st) -> Parser ka env st err ()
puts f = Parser(\ sk fk env st -> sk () fk env (f st))


-- Testing...

type ErrMsg = String

runSimple :: Parser (Result (Int, ErrMsg) (Int, a)) String Int String a -> String -> (Result (Int, ErrMsg) (Int, a))
runSimple p input = runParser p input 0

test_ap01 :: Bool
test_ap01 = runSimple (ap (point (\x -> x + 1)) (point 10)) "input string" == Ok (0, 11)


test_local01 :: Bool
test_local01 = runSimple (local (\_ -> "INPUT2") ask) "input string" == Ok (0, "INPUT2")


