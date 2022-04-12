{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Ast
import Text.Parsec.Char
import Text.Parsec (modifyState, ParsecT, putState, updateParserState, Parsec)
import Control.Monad (liftM, ap, when)

-- Tipo para soportar Enteros y Strings
data Types = PEntero | PCadena deriving (Show, Eq)

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentLine   = "#"
                                  , reservedNames = ["true","false","pass","if",
                                                     "then","else","end",
                                                     "for", "or", "and", "not", "string", "int", "str", "to", "while",
                                                     "print", "input", "write", "read", "insert", "edge", "above", "below",
                                                     "right", "left", "of", "GRAPH", "END"]
                                  , reservedOpNames = [  "+"
                                                       , "-"
                                                       , "*"
                                                       , "/"
                                                       , "<"
                                                       , "<="
                                                       , ">"
                                                       , ">="
                                                       , "&"
                                                       , "|"
                                                       , "=="
                                                       , ";"
                                                       , "~"
                                                       , "="
                                                       , "++"
                                                       , "!="
                                                       , "len"
                                                       , "<-"
                                                       , "->"
                                                       , "<->"
                                                       ]
                                   }
                                 )

-- Tipo para definir el estado de Parsec
type Parser' = Parsec String [(String, Types)]

{- EXPRESIONES DE NODO -}

{- Cada orden sintactico intenta parsear lo mismo pero entre parentesis, con esto podemos tener:
        ("Nodo" & str(i)) -> ("Nodo" & str(i+1)) === "Nodo" & str(i) -> "Nodo" & str(i+1)
   Esto aporta meramente a la legibilidad
-}

-- Primer orden sintactico
nodexp :: Parser' Nodexp
nodexp = chainl1 (try (do parens lis nodexp2)
                  <|> try nodexp2) (try (do whiteSpace lis; reserved lis "->"; return LeftTo))

-- Segundo orden sintactico
nodexp2 :: Parser' Nodexp
nodexp2 = chainl1 (try (do parens lis nodexp3) 
                   <|> nodexp3) (try (do whiteSpace lis; reserved lis "<-"; return RightTo))

-- Tercer orden sintactico
nodexp3 :: Parser' Nodexp
nodexp3 = chainl1 (try (do parens lis nodexp4)
                   <|> nodexp4) (try (do whiteSpace lis; reserved lis "<->"; return LeftRight))

-- Cuarto orden sintactico
nodexp4 :: Parser' Nodexp
nodexp4 = try (do whiteSpace lis
                  var <- identifier lis
                  return (NodeVar var))
          <|> try (do whiteSpace lis
                      str <- strexp
                      return (ConstNode str))

{--------------------------------------------------------------------------------------------------}

{- EXPRESIONES STRING -}

-- Primer orden sintactico
strexp :: Parser' StringExp
strexp = chainl1 (try strexp2) (try (do whiteSpace lis; reserved lis "&";whiteSpace lis; return Concat))

-- Segundo orden sintactico 
strexp2 :: Parser' StringExp
strexp2 = try (do whiteSpace lis
                  strvar <- identifier lis
                  whiteSpace lis
                  return (VariableStr strvar))
          <|> try (do whiteSpace lis
                      s <- between (char '\"') (char '\"') (many $ noneOf "\"")
                      whiteSpace lis
                      return (Str s))
          <|> try (do whiteSpace lis
                      reserved lis "str"
                      n <- parens lis intexp
                      whiteSpace lis
                      return (StrCast n))

{--------------------------------------------------------------------------------------------------}

{- EXPRESIONES BOOLEANAS -}

-- Primer orden sintactico
boolexp :: Parser' Bexp
boolexp = chainl1 boolexp2 $ try (do reserved lis "or"
                                     return Or)

-- Segundo orden sintactico
boolexp2 :: Parser' Bexp
boolexp2 = chainl1 boolexp3 $ try (do reserved lis "and"
                                      return And)

-- Tercer orden sintactico
boolexp3 :: Parser' Bexp
boolexp3 = try (parens lis boolexp)
           <|> notp             -- Not
           <|> intcomp          -- Comparacion entera
           <|> strcomp          -- Comparacion de strings
           <|> bvalue           -- Valores booleanos

-- Comparacion de strings
strcomp :: Parser' Bexp
strcomp = try (do s1 <- strexp                          -- Primer operando
                  op <- compopStr                       -- Operacion
                  s2 <- strexp                          -- Segundo operando
                  return (op s1 s2))
          <|> try (do s1 <- strexp                      -- Comparamos las strings con sus longitudes
                      op <- compop
                      s2 <- strexp
                      return (op (Len s1) (Len s2)))

-- Comparacion entera
intcomp :: Parser' Bexp
intcomp = try (do t <- intexp           -- Primer operando
                  op <- compop          -- Operacion
                  t2 <- intexp          -- Segundo operando
                  return (op t t2))

-- Comparacion de strings
compopStr :: Parser' (StringExp -> StringExp -> Bexp)
compopStr = try (do reservedOp lis "=="
                    return EqStr)
            <|> try (do reservedOp lis "!="
                        return NotEqStr)

-- Operador de comparacion
compop :: Parser' (Iexp -> Iexp -> Bexp)
compop = try (do reservedOp lis "=="
                 return Eq)
         <|> try (do reservedOp lis "!="
                     return NotEq)
         <|> try (do reservedOp lis "<"
                     return Less)
         <|> try (do reservedOp lis "<="
                     return LessEq)
         <|> try (do reservedOp lis ">"
                     return Greater)
         <|> try (do reservedOp lis ">="
                     return GreaterEq)

-- Valor booleano
bvalue :: Parser' Bexp
bvalue = try (do reserved lis "true"
                 return Btrue)
         <|> try (do reserved lis "false"
                     return Bfalse)
-- Not
notp :: Parser' Bexp
notp = try $ do reservedOp lis "not"
                Not <$> boolexp3

{--------------------------------------------------------------------------------------------------}

{- EXPRESIONES ENTERAS -}

-- Primer orden sintactico
intexp :: Parser' Iexp
intexp = try (chainl1 term sumap)

-- Segundo orden sintactico
term :: Parser' Iexp
term = try (chainl1 factor multp)

-- Tercer orden sintactico
factor :: Parser' Iexp
factor = try (parens lis intexp)
         <|> try (do reservedOp lis "-"
                     Uminus <$> factor)
         <|> try (do reservedOp lis "len"
                     Len <$> strexp)
         <|> try (do reserved lis "int"
                     str <- parens lis strexp
                     return (IntCast str))
         <|> try (do n <- integer lis
                     return (Const n)
                  <|> do str <- identifier lis
                         st <- getState
                         case lookforPState str st of
                            Nothing -> fail "Error de parseo"
                            Just c -> case c of
                                        PCadena -> fail "Int expected"
                                        PEntero -> return (Variable str))

{-
        Usamos el estado de parsec debido a que necesitamos una forma de chequear cuando llamar al parser de
        expresiones enteras y cuando al de expresiones de string
-}

sumap :: Parser' (Iexp -> Iexp -> Iexp)
sumap = try (do reservedOp lis "+"
                return Plus)
        <|> try (do reservedOp lis "-"
                    return Minus)

multp :: Parser' (Iexp -> Iexp -> Iexp)
multp = try (do reservedOp lis "*"
                return Times)
        <|> try (do reservedOp lis "/"
                    return Div)

{--------------------------------------------------------------------------------------------------}

{- PARSER DE COMANDOS -}

-- Inicializador para el estado de parsec
initParser :: [(String, Types)]
initParser = [("COUNTER", PEntero), ("ERR", PCadena)]

-- Primer orden sintactico
cmdparse :: Parser' Cmd                                         -- Parseamos los comandos separandolos por ;
cmdparse = chainl1 cmdparser (try (do reservedOp lis ";"
                                      return Seq))

-- Segundo orden sintactico
cmdparser :: Parser' Cmd
cmdparser = try (do reserved lis "if"
                    whiteSpace lis
                    cond <- parens lis boolexp          -- Parseamos la expresion booleana entre parentesis
                    cmd <- braces lis cmdparse          -- Parseamos el comando entre llaves
                    option (If cond cmd Pass) (do reserved lis "else"           -- Else opcional
                                                  cmd2 <- braces lis cmdparse           -- Parseamos comando entre llaves           
                                                  return (If cond cmd cmd2)))
            <|> try (do reserved lis "GRAPH"            -- Definicion de grafo
                        -- Parseamos el nombre para el grafo (.pdf final) y la distancia entre nodos 
                        (name, distancia) <- parens lis (do n <- strexp; char ','; dist <- intexp; return (n, dist))
                        cmd <- cmdparse
                        reserved lis "END"
                        return (Graph name distancia cmd))
            <|> try (do reserved lis "log"
                        whiteSpace lis
                        str <- parens lis strexp
                        return (Log str))
            <|> try (do l <- reserved lis "pass"
                        return Pass)
            <|> try (do whiteSpace lis
                        reserved lis "for"
                        (f, l) <- parens lis forp       -- Parseamos el limite inferior y superior de la iteracion
                        cmd <- braces lis cmdparse      -- Comando entre llaves
                        return (For f l cmd))
            <|> try (do whiteSpace lis
                        reserved lis "while"
                        cond <- between (whiteSpace lis) (whiteSpace lis) (parens lis boolexp)
                        cmd <- braces lis cmdparse
                        return (While cond cmd))
            <|> try (do whiteSpace lis
                        reserved lis "insert"
                        (nd, tag, dir) <- parens lis parseInsert        -- Parseamos nodo id, posiciones, tag
                        return (LetNode nd dir tag))
            <|> try (do whiteSpace lis
                        reserved lis "edge"
                        nexp <- nodexp          -- Parseamos la expresion de nodo (ConstNode, LeftTo, RightTo, LeftRight, etc)
                        return (Set nexp))
            <|> try (do tipo <- reserved lis "int"
                        str <- identifier lis
                        reservedOp lis "="
                        def <- intexp
                        modifyState (updatePState str PEntero)          -- Agregamos la variable con su tipo para saber a que 
                        return (Let str def))                           --      parser llamar
            <|> try (do tipo <- reserved lis "string"
                        str <- identifier lis
                        r <- reservedOp lis "="
                        sdef <- strexp
                        modifyState (updatePState str PCadena)          -- Idem 
                        return (LetStr str sdef))
            <|> try (do str <- identifier lis
                        reservedOp lis "="
                        st <- getState          -- Obtenemos el estado de parsec
                        case lookforPState str st of    -- Buscamos la variable en el estado
                            Nothing -> fail $ "Variable \"" ++ str ++ "\" no definida"
                            Just c -> case c of
                                        PCadena -> LetStr str <$> strexp
                                        PEntero -> Let str <$> intexp)

-- Parser para el comando insert
parseInsert :: Parser' (StringExp, StringExp, Maybe ([Position], StringExp))
parseInsert = do whiteSpace lis
                 nd <- parseStrParens           -- Intenta parsear con o sin parentesis
                 char ','
                 tag <- parseStrParens
                 dir <- optionMaybe (do 
                            whiteSpace lis
                            char ','
                            p <- many parseDirection    -- Parseamos las diferentes direcciones (above, below, right, left)
                            reserved lis "of"
                            id <- parseStrParens
                            return (p, id))
                 return (nd, tag, dir)

-- Parser para intentar parsear lo mismo con parentesis y sin
parseStrParens :: Parser' StringExp 
parseStrParens = try (do parens lis strexp)
                 <|> try strexp

-- Parser de direcciones
parseDirection :: Parser' Position
parseDirection = try (do whiteSpace lis
                         reserved lis "right"
                         return PRight)
                 <|> try (do whiteSpace lis
                             reserved lis "left"
                             return PLeft)
                 <|> try (do whiteSpace lis
                             reserved lis "below"
                             return Below
                 <|> try (do whiteSpace lis
                             reserved lis "above"
                             return Above))

-- Parser para parametros del for
forp :: Parser' (Iexp, Iexp)
forp = do whiteSpace lis
          from <- intexp
          reserved lis "to"
          limit <- intexp
          return (from, limit)

-- Funcion para actualizar el estado de parsec
updatePState :: String -> Types -> [(String, Types)] -> [(String, Types)]
updatePState str tipo []         = [(str, tipo)]
updatePState str tipo ((x,y):xs) = if str == x then (x, tipo):xs else (x,y):updatePState str tipo xs

-- Funcion para buscar una variable en el estado de parsec
lookforPState :: String -> [(String, Types)] -> Maybe Types
lookforPState str []          = Nothing
lookforPState str ((x, y):xs) = if str == x then Just y else lookforPState str xs

totParser :: Parser' a -> Parser' a
totParser p = do whiteSpace lis
                 t <- p
                 eof
                 return t

{--------------------------------------------------------------------------------------------------}
