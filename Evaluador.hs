{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Evaluador where
import Control.Applicative (pure, (<*>))
import Control.Monad (liftM, ap, when)
import Ast
import Parser

data Econtrol a = Fine a
                | Error String
                | State [(String, Integer)]
                deriving Show

type Env = [(String, Integer)]

type VariableF = (String, Integer)

initState :: Env
initState = [("COUNTER", 0)]

newtype Id a = Id a
runId :: Id a -> a
runId (Id a) = a

-- En versiones nuevas del ghc para definir una monada tambien necesitas definirlo como functor y como applicative

-- instance Monad Econtrol where
--     return x        = Fine x
--     Fine x >>= f    = f x
--     Error stk >>= f = Error stk

instance Functor Set where
    fmap = liftM

instance Applicative Set where
    pure  = return
    (<*>) = ap

newtype Set a = Set { runSet :: Env -> Maybe (a, Env, Integer) }

instance Monad Set where
    return x = Set (\s -> Just (x, s, 0))
    m >>= f  = Set (\s -> do (v, s', t) <- runSet m s
                             (v', s'', t') <- runSet (f v) s'
                             return (v', s'', t+t'))

class Monad m => MonadState m where
    lookfor :: String -> m Integer
    update :: String -> Integer -> m ()



instance MonadState Set where
    lookfor var = Set (\s -> maybe Nothing (\v -> Just (v, s, 0)) (lookfor' var s))
                        where lookfor' var [] = Nothing
                              lookfor' var ((var', val):ss) | var == var' = Just val
                                                            | otherwise = lookfor' var ss

    update var val = Set (\s -> Just ((), update' var val s,0))
                    where update' var val [] = [(var, val)]
                          update' var val ((var', val'):ss) | var == var' = (var, val):ss
                                                            | otherwise = (var', val'):update' var val ss

class Monad m => MonadError m where
    throw :: m a

instance MonadError Set where
    throw = Set (const Nothing)

class Monad m => MonadTick m where
    tick :: m ()

instance MonadTick Set where
    tick = Set (\s -> Just ((), s, 1))

eval :: Cmd -> (Env, Integer)
eval p = case runSet (evalComm p) initState of
            Just (v, s, t) -> (s, t)
            Nothing        -> error "Error"

evalComm :: (MonadState m, MonadError m, MonadTick m) => Cmd -> m ()
evalComm Pass           = return ()
evalComm (Let v e)      = do val <- evalIntExp e
                             update v val
evalComm (Seq l r)      = do evalComm l
                             evalComm r
evalComm (If b tc fc)   = do bval <- evalBoolExp b
                             if bval then evalComm tc
                             else evalComm fc
evalComm (For b c)      = do (var, value) <- evalForDef $ (\(Forc d _ _) -> d) b
                             counter <- lookfor "COUNTER"
                             if counter == 0 then update var value
                             else tick -- este tick no deberia ir, pero todavia no se que iria en este else
                             condicion <- evalForCondition $ (\(Forc _ c _) -> c) b
                             (var2, value2) <- evalForInc $ (\(Forc _ _ i) -> i) b
                             when condicion $ do update var2 value2
                                                 update "COUNTER" (counter + 1)
                                                 evalComm (Seq c (For b c))

evalForCond :: (MonadState m, MonadError m, MonadTick m) => Forcond -> m (VariableF, Bool, VariableF)
evalForCond (Forc d c i) = do t <- evalForDef d
                              b <- evalForCondition c
                              {- Tira error porque al momento de evaluar la expresion no encuentra la variable
                              hay que ver como definir la variable en el momento de evalForDef, para que de esta
                              forma la variable ya quede definida y luego no tire error 
                              
                              - en evalComm llamar a evalForDef por separado, hacer un update con esa variable para
                              definirla y luego seguir con el resto
                              
                              -}
                              t2 <- evalForInc i
                              return (t, b, t2)

-- Aca devolvemos el nombre de la variable para poder pasarsela al evaluador de incremento
evalForDef :: (MonadState m, MonadError m, MonadTick m) => Definicion -> m VariableF
evalForDef (Def2 var exp) = do valor <- evalIntExp exp
                               return (var, valor)

evalForCondition :: (MonadState m, MonadError m, MonadTick m) => Condicion -> m Bool
evalForCondition (Cond exp) = do evalBoolExp exp

evalForInc :: (MonadState m, MonadError m, MonadTick m) => Definicion -> m VariableF
evalForInc (Def2 var exp) = do valor <- evalIntExp exp
                               return (var, valor)

evalIntExp :: (MonadState m, MonadError m, MonadTick m) => Iexp -> m Integer
evalIntExp (Const n)    = return n
evalIntExp (Plus l r)   = do e1 <- evalIntExp l
                             e2 <- evalIntExp r
                             return (e1 + e2)
evalIntExp (Minus l r)  = do e1 <- evalIntExp l
                             e2 <- evalIntExp r
                             return (e1 - e2)
evalIntExp (Div l r)    = do e1 <- evalIntExp l
                             e2 <- evalIntExp r
                             return (e1 `div` e2)
evalIntExp (Times l r)  = do e1 <- evalIntExp l
                             e2 <- evalIntExp r
                             return (e1 * e2)
evalIntExp (Uminus l)   = do e1 <- evalIntExp l
                             return (negate e1)
evalIntExp (Variable v) = do lookfor v

evalBoolExp :: (MonadState m, MonadError m, MonadTick m) => Bexp -> m Bool
evalBoolExp Btrue           = return True
evalBoolExp Bfalse          = return False
evalBoolExp (And l r)       = do e1 <- evalBoolExp l
                                 e2 <- evalBoolExp r
                                 return (e1 && e2)
evalBoolExp (Or l r)        = do e1 <- evalBoolExp l
                                 e2 <- evalBoolExp r
                                 return (e1 || e2)
evalBoolExp (Not l)         = do e1 <- evalBoolExp l
                                 return (not e1)
evalBoolExp (Greater l r)   = do e1 <- evalIntExp l
                                 e2 <- evalIntExp r
                                 return (e1 > e2)
evalBoolExp (Less l r)      = do e1 <- evalIntExp l
                                 e2 <- evalIntExp r
                                 return (e1 < e2)
evalBoolExp (GreaterEq l r) = do e1 <- evalIntExp l
                                 e2 <- evalIntExp r
                                 return (e1 >= e2)
evalBoolExp (LessEq l r)    = do e1 <- evalIntExp l
                                 e2 <- evalIntExp r
                                 return (e1 <= e2)
evalBoolExp (Eq l r)        = do e1 <- evalIntExp l
                                 e2 <- evalIntExp r
                                 return (e1 == e2)