module Parser where
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Ast
import Text.Parsec.Char

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentLine   = "#"
                                  , reservedNames = ["true","false","pass","if",
                                                     "then","else","end",
                                                     "for", "or", "and", "not"]
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
                                                       ]
                                   }
                                 )

-- Primero parsea or y baja un orden sintactico con chainl 
boolexp :: Parser Bexp
boolexp = chainl1 boolexp2 $ try (do reserved lis "or"
                                     return Or)
-- por segundo parsea and y baja un orden sintactico
boolexp2 :: Parser Bexp
boolexp2 = chainl1 boolexp3 $ try (do reserved lis "and"
                                      return And)

boolexp3 :: Parser Bexp
boolexp3 = try (parens lis boolexp)
           <|> notp
           <|> intcomp
           <|> bvalue

-- Comparacion entera
intcomp :: Parser Bexp
intcomp = try $ do t <- intexp
                   op <- compop
                   t2 <- intexp
                   return (op t t2)

-- Operador de comparacion

compop = try (do reservedOp lis "=="
                 return Eq)
         <|> try (do reservedOp lis "<"
                     return Less)
         <|> try (do reservedOp lis "<="
                     return LessEq)
         <|> try (do reservedOp lis ">"
                     return Greater)
         <|> try (do reservedOp lis ">="
                     return GreaterEq)

-- Valor booleano
bvalue :: Parser Bexp
bvalue = try (do reserved lis "true"
                 return Btrue)
         <|> try (do reserved lis "false"
                     return Bfalse)
-- Not
notp :: Parser Bexp
notp = try $ do reservedOp lis "not"
                Not <$> boolexp3

intexp :: Parser Iexp
intexp = chainl1 term sumap

term :: Parser Iexp
term = chainl1 factor multp

factor :: Parser Iexp
factor = try (parens lis intexp)
         <|> try (do reservedOp lis "-"
                     f <- factor
                     return (Uminus f))
         <|> (do n <- integer lis
                 return (Const n)
              <|> do str <- identifier lis
                     return (Variable str))


sumap = do try (reservedOp lis "+")
           return Plus
        <|> do try (reservedOp lis "-")
               return Minus


multp = do try (reservedOp lis "*")
           return Times
        <|> do try (reservedOp lis "/")
               return Div

-- Parser de comandos

cmdparse :: Parser Cmd
cmdparse = chainl1 cmdparser (try (do reservedOp lis ";"
                                      return Seq))

cmdparser :: Parser Cmd
cmdparser = try (do reserved lis "if"
                    whiteSpace lis
                    char '('
                    cond <- boolexp
                    char ')'
                    whiteSpace lis
                    char '{'
                    whiteSpace lis
                    cmd <- cmdparse
                    whiteSpace lis
                    char '}'
                    whiteSpace lis
                    reserved lis "else"
                    whiteSpace lis
                    char '{'
                    whiteSpace lis
                    cmd2 <- cmdparse
                    whiteSpace lis
                    char '}'
                    whiteSpace lis
                    return (If cond cmd cmd2)
                    )
            <|> try (do l <- reserved lis "pass"
                        return Pass)
            <|> try (do reserved lis "for"
                        whiteSpace lis
                        char '('
                        f <- forp
                        char ')'
                        whiteSpace lis
                        char '{'
                        whiteSpace lis
                        cmd <- cmdparser
                        char '}'
                        whiteSpace lis
                        return (For f cmd)
                        )
            <|> try (do str <- identifier lis
                        r <- reservedOp lis "="
                        Let str <$> intexp)

forp :: Parser Forcond
forp = do def <- forDefParser
          char ';'
          cond <- forCondParser
          char ';'
          Forc def cond <$> forIncParser

forDefParser :: Parser Definicion
forDefParser = try (do str <- identifier lis
                       reservedOp lis "="
                       Def2 str <$> intexp)

forCondParser :: Parser Condicion
forCondParser = try (do Cond <$> boolexp)

forIncParser :: Parser Definicion
forIncParser = try (do str <- identifier lis 
                       reservedOp lis "="
                       Def2 str <$> intexp
                        )
totParser :: Parser a -> Parser a
totParser p = do whiteSpace lis
                 t <- p --let r = whiteSpace lis
                 eof
                 return t

parseComm :: SourceName -> String -> Either ParseError Cmd
parseComm = parse (totParser cmdparse)