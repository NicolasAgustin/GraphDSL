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

data Types = PEntero | PCadena deriving (Show, Eq)

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentLine   = "#"
                                  , reservedNames = ["true","false","pass","if",
                                                     "then","else","end",
                                                     "for", "or", "and", "not", "string", "int", "str", "to", "while",
                                                     "print", "input", "write", "read", "node", "set", "above", "below",
                                                     "right", "left", "of", "INIT", "END"]
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
                                                       ]
                                   }
                                 )


type Parser' = Parsec String [(String, Types)]

nodexp :: Parser' Nodexp 
nodexp = chainl1 (try nodexp2) (try (do whiteSpace lis; reserved lis "<-"; return LeftTo))

nodexp2 :: Parser' Nodexp 
nodexp2 = chainl1 (try nodexp3) (try (do whiteSpace lis; reserved lis "->"; return RightTo))

nodexp3 :: Parser' Nodexp 
nodexp3 = try (do whiteSpace lis
                  var <- identifier lis
                  return (NodeVar var))
          <|> try (do whiteSpace lis
                      str <- strexp
                      return (ConstNode str))

strexp :: Parser' StringExp
strexp = chainl1 (try strexp2) (try (do whiteSpace lis; reserved lis "&";whiteSpace lis; return Concat))

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
          <|> try (do whiteSpace lis
                      reserved lis "read"
                      path <- parens lis strexp
                      return (ReadFile path))
          <|> try (do reserved lis "input"
                      whiteSpace lis
                      option (Input (Str "")) (try (do str <- parens lis strexp; whiteSpace lis; return (Input str))))

boolexp :: Parser' Bexp
boolexp = chainl1 boolexp2 $ try (do reserved lis "or"
                                     return Or)

boolexp2 :: Parser' Bexp
boolexp2 = chainl1 boolexp3 $ try (do reserved lis "and"
                                      return And)

boolexp3 :: Parser' Bexp
boolexp3 = try (parens lis boolexp)
           <|> notp
           <|> intcomp
           <|> strcomp
           <|> bvalue

-- Comparacion strings
strcomp :: Parser' Bexp
strcomp = try (do s1 <- strexp
                  op <- compopStr
                  s2 <- strexp
                  return (op s1 s2))
          <|> try (do s1 <- strexp
                      op <- compop
                      s2 <- strexp
                      return (op (Len s1) (Len s2)))

-- Comparacion entera
intcomp :: Parser' Bexp
intcomp = try (do t <- intexp
                  op <- compop
                  t2 <- intexp
                  return (op t t2))

compopStr = try (do reservedOp lis "=="
                    return EqStr)
            <|> try (do reservedOp lis "!="
                        return NotEqStr)

-- Operador de comparacion
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

intexp :: Parser' Iexp
intexp = try (chainl1 term sumap)

term :: Parser' Iexp
term = try (chainl1 factor multp)

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

-- Parser de comandos

cmdparse :: Parser' Cmd
cmdparse = chainl1 cmdparser (try (do reservedOp lis ";"
                                      return Seq))

initParser :: [(String, Types)]
initParser = [("COUNTER", PEntero), ("ERR", PCadena)]

cmdparser :: Parser' Cmd
cmdparser = try (do reserved lis "if"
                    whiteSpace lis
                    cond <- parens lis boolexp
                    cmd <- braces lis cmdparse
                    option (If cond cmd Pass) (do reserved lis "else"
                                                  cmd2 <- braces lis cmdparse
                                                  return (If cond cmd cmd2)))
            <|> try (do reserved lis "INIT"
                        return Init)
            <|> try (do reserved lis "END"
                        return End)
            <|> try (do reserved lis "print"
                        whiteSpace lis
                        str <- parens lis strexp
                        whiteSpace lis
                        return (Print str))
            <|> try (do reserved lis "write"
                        whiteSpace lis
                        (p, tw, append) <- parens lis params
                        return (WriteFile p tw append))
            <|> try (do l <- reserved lis "pass"
                        return Pass)
            <|> try (do whiteSpace lis
                        reserved lis "for"
                        (f, l) <- parens lis forp
                        cmd <- braces lis cmdparse
                        return (For f l cmd))
            <|> try (do whiteSpace lis
                        reserved lis "while"
                        cond <- between (whiteSpace lis) (whiteSpace lis) (parens lis boolexp)
                        cmd <- braces lis cmdparse
                        return (While cond cmd))
            <|> try (do reserved lis "node"
                        nd <- identifier lis
                        reservedOp lis "="
                        tag <- strexp
                        dir <- optionMaybe (do p <- parseDirection
                                               reserved lis "of" 
                                               id <- strexp
                                               return (p, id))
                        return (LetNode nd dir tag))
            <|> try (do whiteSpace lis
                        reserved lis "set"
                        nexp <- nodexp 
                        return (Set nexp))
            <|> try (do tipo <- reserved lis "int"
                        str <- identifier lis
                        r <- reservedOp lis "="
                        def <- intexp
                        modifyState (updatePState str PEntero)
                        return (Let str def))
            <|> try (do tipo <- reserved lis "string"
                        str <- identifier lis
                        r <- reservedOp lis "="
                        sdef <- strexp
                        modifyState (updatePState str PCadena)
                        return (LetStr str sdef))
            <|> try (do str <- identifier lis
                        r <- reservedOp lis "="
                        st <- getState
                        case lookforPState str st of
                            Nothing -> fail $ "Variable \"" ++ str ++ "\" no definida"
                            Just c -> case c of
                                        PCadena -> LetStr str <$> strexp
                                        PEntero -> Let str <$> intexp)

parseDirection :: Parser' Position 
parseDirection = try (do reserved lis "right"
                         return PRight)
                 <|> try (do reserved lis "left"
                             return PLeft)
                 <|> try (do reserved lis "below"
                             return Below
                 <|> try (do reserved lis "above"
                             return Above))

params :: Parser' (StringExp, StringExp, Bexp)
params = do path <- strexp
            comma lis
            towrite <- strexp
            comma lis
            append <- boolexp
            return (path, towrite, append)

updatePState :: String -> Types -> [(String, Types)] -> [(String, Types)]
updatePState str tipo []         = [(str, tipo)]
updatePState str tipo ((x,y):xs) = if str == x then (x, tipo):xs else (x,y):updatePState str tipo xs

lookforPState :: String -> [(String, Types)] -> Maybe Types
lookforPState str []          = Nothing
lookforPState str ((x, y):xs) = if str == x then Just y else lookforPState str xs

forp :: Parser' (Iexp, Iexp)
forp = do whiteSpace lis
          from <- intexp
          reserved lis "to"
          limit <- intexp
          return (from, limit)

forDefParser :: Parser' Definicion
forDefParser = try (do whiteSpace lis
                       result <- option False $ try (do reserved lis "int"; return True)
                       str <- identifier lis
                       reservedOp lis "="
                       modifyState (updatePState str PEntero)
                       Def2 result str <$> intexp)

forCondParser :: Parser' Condicion
forCondParser = try (do whiteSpace lis
                        Cond <$> boolexp)

forIncParser :: Parser' Definicion
forIncParser = try (do whiteSpace lis
                       str <- identifier lis
                       reservedOp lis "="
                       Def2 False str <$> intexp)

totParser :: Parser' a -> Parser' a
totParser p = do whiteSpace lis
                 t <- p
                 eof
                 return t
