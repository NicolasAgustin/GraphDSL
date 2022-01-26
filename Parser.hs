module Parser where
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Ast
import Text.Parsec.Char
import Text.Parsec (modifyState, ParsecT, putState, updateParserState, Parsec)
import Control.Monad (liftM, ap, when)

data Types = PEntero | PCadena deriving (Show, Eq)

{-
        TODO: 
        - Revisar definiciones, porque cada vez que asigne una variable voy a tener que escribir int o string y eso solamente deberia hacerlo una vez
-}

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentLine   = "#"
                                  , reservedNames = ["true","false","pass","if",
                                                     "then","else","end",
                                                     "for", "or", "and", "not", "string", "int"]
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
                                                       ]
                                   }
                                 )


type Parser' = Parsec String [(String, Types)]

strexp :: Parser' StringExp
strexp = try $ chainl1 strexp2 (try (do whiteSpace lis; reserved lis "&";whiteSpace lis; return Concat))

-- Tira error porque la definicion de una variable de tipo string es igual a la definicion de una variable de tipo int
-- deberiamos determinar una forma de chequear de que tipo es la variable 
strexp2 :: Parser' StringExp
strexp2 = try (do whiteSpace lis
                  strvar <- identifier lis
                  whiteSpace lis
                  return (VariableStr strvar))
          <|> try (do whiteSpace lis
                      s <- between (char '\"') (char '\"') (many $ noneOf "\"")
                      whiteSpace lis
                      return (Str s))

-- Primero parsea or y baja un orden sintactico con chainl 
boolexp :: Parser' Bexp
boolexp = chainl1 boolexp2 $ try (do reserved lis "or"
                                     return Or)
-- por segundo parsea and y baja un orden sintactico
boolexp2 :: Parser' Bexp
boolexp2 = chainl1 boolexp3 $ try (do reserved lis "and"
                                      return And)

boolexp3 :: Parser' Bexp
boolexp3 = try (parens lis boolexp)
           <|> notp
           <|> intcomp
           <|> bvalue

-- Comparacion entera
intcomp :: Parser' Bexp
intcomp = try $ do t <- intexp
                   op <- compop
                   t2 <- intexp
                   return (op t t2)


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
         <|> try (do n <- integer lis
                     return (Const n)
                  <|> do str <- identifier lis
                         return (Variable str))

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

cmdparser :: Parser' Cmd
cmdparser = try (do reserved lis "if"
                    whiteSpace lis
                    cond <- parens lis boolexp
                    whiteSpace lis
                    cmd <- braces lis cmdparse
                    whiteSpace lis
                    option (If cond cmd Pass) (try (do reserved lis "else"; whiteSpace lis; cmd2 <- braces lis cmdparse; whiteSpace lis
                                                       return (If cond cmd cmd2))))
                --     case pm of 
                --             Just p -> return (If cond cmd p)
                --             Nothing -> return (If cond cmd Pass))
            <|> try (do l <- reserved lis "pass"
                        return Pass)
            <|> try (do reserved lis "for"
                        whiteSpace lis
                        f <- parens lis forp
                        whiteSpace lis
                        cmd <- braces lis cmdparse
                        whiteSpace lis
                        return (For f cmd))
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
                            PCadena -> LetStr str <$> strexp
                            PEntero -> Let str <$> intexp)

updatePState :: String -> Types -> [(String, Types)] -> [(String, Types)]
updatePState str tipo []         = [(str, tipo)]
updatePState str tipo ((x,y):xs) = if str == x then (x, tipo):xs else (x,y):updatePState str tipo xs

lookforPState :: String -> [(String, Types)] -> Types
lookforPState str []          = error $ "Variable no encontrada en el estado: " ++ str
lookforPState str ((x, y):xs) = if str == x then y else lookforPState str xs

-- lookforPState :: String -> 

-- Hay que ver como resolver el error en la asignacion general
-- Esto actualmente falla: 
{- 

int a = 0;

a = 0

Let a (Const 0)

LetStr a (Str "")

string resultado = "hola";
n = resultado & " mundo"; 

debido a que solamente ejecuta el parser de int y luego falla 

-}

genAssignExp :: Parser' GenExpType
genAssignExp = try (do ExpStr <$> strexp) <|> (do fail "fallo en chainl"; ExpInt <$> intexp)

forp :: Parser' Forcond
forp = do def <- forDefParser
          char ';'
          cond <- forCondParser
          char ';'
          Forc def cond <$> forIncParser

forDefParser :: Parser' Definicion
forDefParser = try (do optional $ try (reserved lis "int")
                       str <- identifier lis
                       reservedOp lis "="
                       Def2 str <$> intexp)
                <|> try (do optional $ try (reserved lis "string")
                            str <- identifier lis
                            reservedOp lis "="
                            DefS str <$> strexp)

forCondParser :: Parser' Condicion
forCondParser = try (do Cond <$> boolexp)

forIncParser :: Parser' Definicion
forIncParser = try (do str <- identifier lis
                       reservedOp lis "="
                       Def2 str <$> intexp)

totParser :: Parser' a -> Parser' a
totParser p = do whiteSpace lis
                 t <- p
                 eof
                 return t

-- parseComm :: SourceName -> String -> Either ParseError Cmd
-- parseComm = parse (totParser cmdparse)