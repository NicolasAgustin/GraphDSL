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
{-
    definicion del lenguaje
    definimos cuales seran los tokens de nuestro lenguaje
    un token es la unidad minima en que dividimos nuestro lenguaje
-}
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentLine   = "#"
                                  , reservedNames = ["true","false","pass","if",
                                                     "then","else","end", "color",
                                                     "for", "or", "and", "not", "string", "int", "str", "to", "while",
                                                     "print", "input", "write", "read", "node", "edge", "above", "below",
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
                                                       , "%"
                                                       ]
                                   }
                                 )

-- Tipo para definir el estado de Parsec
type Parser' = Parsec String [(String, Types)]
{-
    Definimos el tipo Parsec' 
    indicando el tipo de stream como String 
    y el estado como una lista de (String, Types)
-}

{- EXPRESIONES DE NODO -}
nodexp :: Parser' Nodexp
nodexp = try (do whiteSpace lis
                 n1 <- braces lis nodexp
                 {-
                    braces parsea el parser nodexp 
                    entre llaves
                 -}
                 reserved lis "<-"
                 n2 <- braces lis nodexp
                 return (RightTo n1 n2))
         <|> try (do whiteSpace lis
                     n1 <- braces lis nodexp
                     reserved lis "->"
                     n2 <- braces lis nodexp
                     return (LeftTo n1 n2))
         <|> try (do whiteSpace lis
                     n1 <- braces lis nodexp
                     reserved lis "<->"
                     n2 <- braces lis nodexp
                     return (LeftRight n1 n2))
         <|> try (do whiteSpace lis
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
{-
    chainl1 parsea ocurrencias del primer parser (try strexp2) en este caso, separadas por el segundo parser
    (try (do whiteSpace lis; reserved lis "&";whiteSpace lis; return Concat))

    chainl1 devuelve una asociacion hacia la izquierda y elimina el problema de la recursion izquierda

    Si tenemos la siguiente gramatica
    

    E → E + T|T
    T → T * F|F
    F → (E)|id

    se traduciria al siguiente parser
    el cual, evidentemente posee una recursion en t <- E
    el parser se llama infinitamente

    E = do 
          t <- E
          char '+'
          t' <- T
        <|> T

-}

-- Segundo orden sintactico 
strexp2 :: Parser' StringExp
strexp2 = try (do whiteSpace lis
                  strvar <- identifier lis
                  {-
                    identifier parsea un identificador de variable
                    hay que pasarle la definicion de lenguaje
                  -}
                  whiteSpace lis
                  return (VariableStr strvar))
          <|> try (do whiteSpace lis
                      s <- between (char '\"') (char '\"') (many $ noneOf "\"")
                      {-
                        between p1 p2 p
                        parsea p entre p1 y p2
                        con many parseamos cero o mas ocurrencias de noneOf
                        noneOf parsea si el caracter actual no esta en la lista provista por "\""
                        es decir, con esto parseamos todo lo que no sean comillas
                        si bien trabajamos con listas, funciona porque una lista de caracteres es una string
                      -}
                      whiteSpace lis
                      return (Str s))
          <|> try (do whiteSpace lis
                      reserved lis "str"
                      {-
                        reserved parsea una palabra reservada
                        en base a la definicion de lenguaje que le pasemos
                      -}
                      n <- parens lis intexp
                      {-
                        parens parsea el parser intexp pero entre parentesis
                      -}
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

{-
    en boolexp y boolexp2 parseamos una o mas ocurrencias separadas por
    or o por and
-}

-- Tercer orden sintactico
boolexp3 :: Parser' Bexp
boolexp3 = try (parens lis boolexp)
           <|> notp             -- Not
           <|> intcomp          -- Comparacion entera
           <|> strcomp          -- Comparacion de strings
           <|> bvalue           -- Valores booleanos
{-
    con este parser determinamos que tipo de
    expresion booleana estamos realizando
-}


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
                      {-
                        La comparacion entre strings la realizamos 
                        por valor en caso del eq y noteq 
                        y por su longitud para < > <= >=

                        en el caso de strcomp tenemos dos casos
                        intentamos primero el caso de eq y noteq 
                        ya que si lo hacemos despues,
                        puede pasar que el parser de compop suceda,
                        en dicho caso va a retornar Eq o NotEq pero
                        estas operaciones actuan sobre enteros
                        por lo que seria un error de parseo

                        Primero intentamos parsear por EqStr y NotEqStr
                        ya que son los dos casos diferentes,
                        los otros casos son iguales debido a que las strings
                        son comparadas en base a su longitud
                      -}

-- Comparacion entera
intcomp :: Parser' Bexp
intcomp = try (do t <- intexp           -- Primer operando
                  op <- compop          -- Operacion
                  t2 <- intexp          -- Segundo operando
                  return (op t t2))

{-
    tanto compop y compopStr 
    retornan una funcion
    esto para no repetir codigo
    ya que las funciones de compop 
    se pueden reutilizar
-}

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
                Not <$> boolexp

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
                     {-
                        reservedOp parsea operaciones reservadas
                     -}
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
        <|> try (do reservedOp lis "%"
                    return Mod)

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
                    {-
                        option intenta parsear y si falla 
                        sin consumir input retorna (If cond cmd Pass)
                        esto debido a que el else es opcional
                    -}
            <|> try (do whiteSpace lis
                        reserved lis "GRAPH"            -- Definicion de grafo
                        -- Parseamos el nombre para el grafo (.pdf final) y la distancia entre nodos 
                        (name, distancia) <- parens lis (do whiteSpace lis; n <- strexp; char ','; dist <- intexp; return (n, dist))
                        msize <- brackets lis intexp
                        {-
                            brackets parsea intexp entre corchetes
                        -}
                        cmd <- cmdparse
                        reserved lis "END"
                        return (Graph name distancia msize cmd))
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
                        reserved lis "node"
                        -- parseNodeCmd 
                        (n_id, x, y) <- between (whiteSpace lis) (whiteSpace lis) parseNodeCmd        -- Parseamos nodo id, posiciones, tag
                        return (LetNodeCoord n_id x y))
            <|> try (do reserved lis "color"    -- color (COLOR) {id}
                        whiteSpace lis
                        color <- strexp
                        node <- braces lis nodexp
                        return (Color color node))
            <|> try (do whiteSpace lis
                        reserved lis "edge"
                        edge_color <- optionMaybe strexp
                        {-
                            optionMaybe intenta parsear 
                            y retorna un maybe
                            esto porque puede o no haber color de arista
                            y puede o no tener un peso o tag
                        -}
                        maybe_tag <- optionMaybe strexp
                        nexp <- nodexp          -- Parseamos la expresion de nodo (ConstNode, LeftTo, RightTo, LeftRight, etc)
                        return (Set edge_color maybe_tag nexp))
            <|> try (do tipo <- reserved lis "int"
                        str <- identifier lis
                        reservedOp lis "="
                        def <- intexp
                        modifyState (updatePState str PEntero)          -- Agregamos la variable con su tipo para saber a que 
                        return (Let str def))                           --      parser llamar
                        {-
                            agregamos al estado una tupla 
                            con el nombre de la variable que se definio y su tipo
                            ya que no encontramos una forma de poder realizar lo siguiente
                            var = intexp | strexp
                            por lo que para decidir a que parser debemos llamar
                            buscamos la variable en el estado y llamamos al parser
                            correspondiente
                        -}
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

{-
    parseamos {"id"}<x, y>
-}
parseNodeCmd :: Parser' (StringExp, Iexp, Iexp)
parseNodeCmd = do whiteSpace lis 
                  node_id <- braces lis strexp -- node {"id"}<0,0>
                  char '<'
                  x <- intexp
                  char ','
                  y <- intexp
                  char '>'
                  return (node_id, x, y)

parseNodeVarParens :: Parser' Nodexp
parseNodeVarParens = try (do parens lis nodexp)
                     <|> try nodexp
{-
    no se usa
-}

-- Parser para intentar parsear lo mismo con parentesis y sin
parseStrParens :: Parser' StringExp
parseStrParens = try (do parens lis strexp)
                 <|> try strexp
{- no se usa -}

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
{-
    str: variable que queremos agregar al estado
    tipo: tipo con el que se definio la variable
    ((x,y):xs): estado del parser
        donde x es string e y es un tipo
    
    si encontramos la variable actualizamos el tipo de la misma por el tipo que le pasamos
    en caso contrario llamamos recursivamente a updatePState con el resto de la lista

    cuando llegamos a la lista vacia insertamos la nueva variable al estado
-}


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
