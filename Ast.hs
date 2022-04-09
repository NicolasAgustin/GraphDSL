module Ast where

type Var = String 

-- Este tipo se tiene que sacar
data GenExpType = ExpStr StringExp | ExpInt Iexp deriving (Show, Eq)

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
data Nodexp = LeftTo Nodexp Nodexp          -- Arista de izquierda a derecha 
              | RightTo Nodexp Nodexp       -- Arista de derecha a izquierda
              | LeftRight Nodexp Nodexp     -- Arista bidireccional
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
           | LetNode StringExp (Maybe ([Position], StringExp)) StringExp    -- Definicion de nodo LetNode(id, posiciones, tag visualizable)
           | Set Nodexp                                                     -- Seteo de arista
           | If Bexp Cmd Cmd                                                -- Condicional
           | For Iexp Iexp Cmd                                              -- For
           | While Bexp Cmd                                                 -- While
           | Seq Cmd Cmd                                                    -- Secuenciador
           | Print StringExp                                                -- Salida por pantalla
           | Pass                                                           -- Pass
           | Graph StringExp Iexp                                           -- Definicion de grafo
           | End                                                            -- Cierre de definicion de grafo
           deriving (Show, Eq)