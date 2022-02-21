module Ast where

type Var = String 

data GenExpType = ExpStr StringExp | ExpInt Iexp deriving (Show, Eq)

data Iexp = Plus Iexp Iexp 
            | Minus Iexp Iexp 
            | Div Iexp Iexp 
            | Times Iexp Iexp 
            | Uminus Iexp
            | Const Integer 
            | Variable Var 
            | Len StringExp
            | IntCast StringExp
            deriving (Show, Eq)

data Bexp = Btrue 
            | Bfalse 
            | Not Bexp
            | And Bexp Bexp 
            | Or Bexp Bexp 
            | Greater Iexp Iexp
            | GreaterEq Iexp Iexp
            | Less Iexp Iexp 
            | LessEq Iexp Iexp 
            | Eq Iexp Iexp 
            | EqStr StringExp StringExp
            | NotEq Iexp Iexp
            | NotEqStr StringExp StringExp
            deriving (Show, Eq)

data StringExp = Str String
                 | VariableStr Var
                 | Concat StringExp StringExp 
                 | StrCast Iexp
                 | Input StringExp
                 | ReadFile StringExp 
                 deriving (Show, Eq)

{- Para poder solucionar el error de la asignacion, necesito un tipo que generalice StringExp y Iexp -}
-- Luego el valor de esta expresion se evaluara en tiempo de evaluacion y se va a determinar el tipo de la asignacion y el tipo de la variable a la que
--  se quiere asignar

data Cmd = Let Var Iexp                 -- Definicion
           | LetStr Var StringExp       -- Definicion
           | Assign Var GenExpType      -- Asignacion
           | If Bexp Cmd Cmd 
           | For Forcond Cmd 
           | Seq Cmd Cmd 
           | Print StringExp
        --    path, to_write, append
           | WriteFile StringExp StringExp Bexp
           | Pass 
           deriving (Show, Eq)

data Forcond = Def Var Iexp 
               | Fcond Bexp 
               | Fint Iexp 
               | Seqf Forcond Forcond 
               | Forc Definicion Condicion Definicion 
               deriving (Show, Eq)

data Definicion = Def2 Var Iexp | DefS Var StringExp deriving (Show, Eq)

data Condicion = Cond Bexp deriving (Show, Eq)

data Incremento = Inc Cmd deriving (Show, Eq)