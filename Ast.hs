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

data Position = Above | Below | PRight | PLeft deriving (Show, Eq)

data Nodexp = LeftTo Nodexp Nodexp 
              | RightTo Nodexp Nodexp
              | NodeVar Var
              | ConstNode StringExp
              | Node StringExp StringExp StringExp Iexp
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

data Cmd = Let Var Iexp                 -- Definicion
           | LetStr Var StringExp       -- Definicion
           | LetNode Var (Maybe (Position, StringExp)) StringExp      -- id position(optional) tag
           | Set Nodexp
           | If Bexp Cmd Cmd 
           | For Iexp Iexp Cmd 
           | While Bexp Cmd
           | Seq Cmd Cmd 
           | Print StringExp
           | WriteFile StringExp StringExp Bexp
           | Pass 
           | Init 
           | End
           deriving (Show, Eq)

data Forcond = Forc Definicion Condicion Definicion deriving (Show, Eq)

data Definicion = Def2 Bool Var Iexp deriving (Show, Eq)

data Condicion = Cond Bexp deriving (Show, Eq)

data Incremento = Inc Cmd deriving (Show, Eq)