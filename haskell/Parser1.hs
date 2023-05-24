{-# LANGUAGE KindSignatures #-}

module Parser1 where

data Result err a = Ok a | Err err deriving (Eq, Ord, Show)


type FailK (ka :: *) (st :: *) (err :: *) = st -> err -> ka

type SuccessK (ka :: *) (env :: *) (st :: *) (err :: *) (a :: *) = a -> FailK ka st err -> env -> st -> ka


newtype Parser1 (ka :: *) (env :: *) (st :: *) (err :: *) (a :: *) = 
    Parser1 (SuccessK ka env st err a -> FailK ka st err -> env -> st -> ka)

runParser1 :: Parser1 (Result (st, err) (st, a))  env  st err a -> st -> env -> Result (st, err) (st, a)
runParser1 p st env =
    let fk = \st2 err -> Err (st2, err) in
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