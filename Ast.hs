module Ast where

type Var = String 

data Iexp = Plus Iexp Iexp 
            | Minus Iexp Iexp 
            | Div Iexp Iexp 
            | Times Iexp Iexp 
            | Uminus Iexp
            | Const Integer 
            | Variable Var 
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
            | NotEq Iexp Iexp
            deriving (Show, Eq)

-- Para solucionar el error de las definiciones de int o string se deberia anteponer la palabra strin o int en las definiciones
-- para eso se debe modificar el ast

data StringExp = Str String
                 | VariableStr Var
                 | Concat StringExp StringExp 
                 deriving (Show, Eq)

data Cmd = Let Var Iexp
           | LetStr Var StringExp
           | If Bexp Cmd Cmd 
           | For Forcond Cmd 
           | Seq Cmd Cmd 
           | Pass 
           deriving (Show, Eq)

data Forcond = Def Var Iexp 
               | Fcond Bexp 
               | Fint Iexp 
               | Seqf Forcond Forcond 
               | Forc Definicion Condicion Definicion 
               deriving (Show, Eq)

data Definicion = Def2 Var Iexp deriving (Show, Eq)

data Condicion = Cond Bexp deriving (Show, Eq)

data Incremento = Inc Cmd deriving (Show, Eq)