{-# LANGUAGE DeriveDataTypeable #-}
module Ast where
import Data.Data
-- import OrientationMapper (Mapper)
import Matriz
type Var = String 

-- Este tipo se tiene que sacar
data GenExpType = ExpStr StringExp | ExpInt Iexp deriving (Show, Eq)

type Mapper = Matrix DataType'

data DataType' = Entero Integer
                | Cadena String
                -- Id: string X: Integer Y: Integer
                | Node String Integer Integer
                | Output [String]
                | Grid Mapper
                deriving (Data)

-- Tipo de dato para especificar los distintos tipos

instance Show DataType' where
    show (Entero x)   = "Entero " ++ show x
    show (Cadena s)   = "Cadena " ++ show s
    show (Node i d t) = "Nodo " ++ show i ++ " " ++ show d ++ " " ++ show t
    show (Output l)   = "Output " ++ concat l
    show (Grid m)     = show m

-- Instancia de comparacion para los tipos
instance Eq DataType' where
    (==) (Entero x) (Entero x2)       = x == x2
    (==) (Entero _) (Cadena _)        = False
    (==) (Cadena _) (Entero _)        = False
    (==) (Cadena s) (Cadena s2)       = s == s2
    (==) Node {} (Entero _)           = False
    (==) Node {} (Cadena _)           = False
    (==) (Node i d t) (Node i2 d2 t2) = (i,d,t) == (i2,d2,t2)
    (==) _ Node {}                    = False
    (==) (Output l) _                 = False
    (==) _ (Output l)                 = False
    (==) (Grid m) (Grid m2)           = m == m 
    (==) _ (Grid m)                   = False
    (==) (Grid m) _                   = False  

-- Expresiones enteras
data Iexp = Plus Iexp Iexp          -- Suma
            | Minus Iexp Iexp       -- Resta
            | Div Iexp Iexp         -- Division
            | Times Iexp Iexp       -- Multiplicacion
            | Uminus Iexp           -- Menos unario
            | Const Integer         -- Constantes
            | Variable Var          -- Variable
            | Len StringExp         -- Longitud de string
            | IntCast StringExp     -- Casteo a entero
            deriving (Show, Eq)

data Position = Above | Below | PRight | PLeft deriving (Eq)

-- Instancia de Show para el tipo de dato Position
instance Show Position where
    show PRight = "right"
    show PLeft  = "left"
    show Above  = "above"
    show Below  = "below"

-- Expresiones de nodos
data Nodexp = LeftTo Nodexp Nodexp          -- Arista de izquierda a derecha n->n2
              | RightTo Nodexp Nodexp       -- Arista de derecha a izquierda n<-n2
              | LeftRight Nodexp Nodexp     -- Arista bidireccional n-n2
              | NodeVar Var                 -- Variable nodo 
              | ConstNode StringExp         -- Expresion de string para los id de nodos
              deriving (Show, Eq)

-- Expresiones booleanas
data Bexp = Btrue                           -- True
            | Bfalse                        -- False
            | Not Bexp                      -- Not
            | And Bexp Bexp                 -- And
            | Or Bexp Bexp                  -- Or
            | Greater Iexp Iexp             -- Mayor que
            | GreaterEq Iexp Iexp           -- Mayor o igual que
            | Less Iexp Iexp                -- Menor que
            | LessEq Iexp Iexp              -- Menor o igual que
            | Eq Iexp Iexp                  -- Igual (Enteros)
            | EqStr StringExp StringExp     -- Igual (Strings)
            | NotEq Iexp Iexp               -- No igual (Enteros)
            | NotEqStr StringExp StringExp  -- No igual (Strings)
            deriving (Show, Eq)

data StringExp = Str String                     -- String literal
                 | VariableStr Var              -- Variable
                 | Concat StringExp StringExp   -- Concatenacion de strings
                 | StrCast Iexp                 -- Casteo a entero
                 | Error
                 deriving (Show, Eq)

data Cmd = Let Var Iexp                                                     -- Definicion entera
           | LetStr Var StringExp                                           -- Definicion string
           | LetNode StringExp (Maybe ([Position], Nodexp)) StringExp   -- Definicion de nodo LetNode(id, posiciones, tag visualizable)
           | LetNodeCoord StringExp Iexp Iexp 
           | Set Nodexp                                                     -- Seteo de arista
           | If Bexp Cmd Cmd                                                -- Condicional
           | For Iexp Iexp Cmd                                              -- For
           | While Bexp Cmd                                                 -- While
           | Seq Cmd Cmd                                                    -- Secuenciador
           | Log StringExp                                                -- Salida por pantalla
           | Pass                                                           -- Pass
           | Graph StringExp Iexp Iexp Cmd                                           -- Definicion de grafo
           deriving (Show, Eq)