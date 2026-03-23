-- ParserADT.hs
-- Implementación de los combinadores monádicos para el Parser Top-Down (Recursive Descent).
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module ParserADT
    ( Parser(..)
    , satisfy
    , eof
    , (<|>)
    , pMap
    , pFail
    ) where

import Control.Applicative (Alternative(..))

-- Un Parser es una función que toma una lista de tokens 't' y devuelve :
-- * O bien, un Error (String)
-- * O bien, una tupla con el valor parseado 'a' (por ejemplo, el nodo del AST) y el resto de los tokens.
newtype Parser t a = Parser 
  { runParser :: [t] -> Either String (a, [t]) }

-- Nota: Un newtype SOLO puede envolver exactamente una (y solo una) variable o función. 
-- Un newtype es solo un truco en tiempo de compilación para disfrazar un solo valor,
-- a coste cero.
---------------------------------------------------------------------------------------------------------
-- PARTE 1: LAS CLASES MATEMÁTICAS FUNDAMENTALES (Para que Haskell nos deje usar `do`)
---------------------------------------------------------------------------------------------------------

-- 1. Implementación de Functor: 
-- Modifica el resultado exitoso de un parser aplicando una función 'f', dejando intacto el resto de tokens.
-- Usamos pattern matching con un `case` en `runParser p input`. 
--    Si sale Left err, devuelve Left err. 
--    Si sale Right (a, rest), devuelve Right (f a, rest).
instance Functor (Parser t) where
    fmap f p = Parser $ \input -> case (runParser p input ) of
        Left err        -> Left err
        Right (a, rest) -> Right (f a, rest)

-- Nota: Al darle Parser t le estamos diciendo: 
-- "Esta es la carcasa, un parser que come tokens tipo t. Transforma el siguiente tipo que reciba esa carcasa (la a)"


-- 2. Implementación de Applicative:
-- pure: crea un parser que consume 0 tokens y siempre tiene éxito devolviendo 'x'.

-- (<*>): Permite encadenar dos parsers aplicando la función resultante del primero al valor resultante del segundo. 
-- Primero corre p1. Si falla, propaga el error. 
--     Si p1 tiene éxito (f, rest1), usa 'rest1' como input para correr p2. 
--     Si p2 tiene éxito (a, rest2), devuelve (f a, rest2).
instance Applicative (Parser t) where
    pure x  = Parser $ \input -> Right (x, input) -- TODO (Debe devolver 'x' y el input sin tocar)
    
    p1 <*> p2 = Parser $ \input -> case (runParser p1 input) of
        Left err         -> Left err
        Right (f, rest1) -> case (runParser p2 rest1) of
            Left err         -> Left err
            Right (a, rest2) -> Right (f a, rest2)
    -- (Evalúa runParser p1, luego runParser p2 usando el remanente)


-- 3. Implementación de Monad:
-- (>>=) es el corazón de la notación `do`. Ejecuta un parser, toma su resultado 'a' e invoca 
-- la función 'f' para obtener el SIGUIENTE parser.
-- Igual que <*> pero `f a` te da el nuevo parser que debes correr con `runParser`.
instance Monad (Parser t) where
    return = pure
    p >>= f = Parser $ \input -> case (runParser p input) of
        Left err -> Left err
        Right (a, rest) -> runParser (f a) rest


-- 4. Implementación de Alternative:
-- Permite tomar decisiones o ramificaciones.
-- empty: Un parser que siempre falla de base.

-- (<|>): Corre el parser p1. Si va bien, perfecto. Si falla, intenta p2 SOBRE EL MISMO INPUT INICIAL.
instance Alternative (Parser t) where
    empty = pFail "Empty parser"
    p1 <|> p2 = Parser $ \input -> case (runParser p1 input) of
        Right (a1, rest1) -> Right (a1, rest1)
        Left err          -> case (runParser p2 input) of
            Left err          -> Left err
            Right (a2, rest2) -> Right (a2, rest2)
        -- TODO (case sobre p1 input. Si Left _, corre p2 input)


---------------------------------------------------------------------------------------------------------
-- PARTE 2: LOS BLOQUES DE CONSTRUCCIÓN BÁSICOS (Tus herramientas atómicas)
---------------------------------------------------------------------------------------------------------

-- Un parser que siempre falla con un mensaje. 
pFail :: String -> Parser t a
pFail err = Parser $ \_ -> Left err


-- Parsea y consume exactamente EL PRÓXIMO token si cumple el predicado `cond`.
-- Si es vacío ([]), devuelve Left "Unexpected end of file".
-- Si es (x:xs), aplica `cond x`. Si True, devuelve Right (x, xs). Si False, Left "Unexpected token".
satisfy :: (t -> Bool) -> Parser t t
satisfy cond = Parser $ \input -> case input of
    (x:xs) | cond x -> Right (x, xs)
    (x:xs)          -> Left "Unexpected token"
    []              -> Left "Unexpected end of file"


-- Parsea el final de archivo/cadena. Solo tiene éxito si no quedan más tokens por consumir.
-- Pista: Si el input es [], devuelve Right ((), []). Si quedan tokens, Left "Expected end of file".
eof :: Parser t ()
eof = Parser $ \input -> case input of
    [] -> Right ((), [])
    _  -> Left "Expected end of file"

-- Nota: El símbolo () en Haskell se llama Unit, y equivale al void de C o Java. 
--       Significa "nada de utilidad", "información inútil".

---------------------------------------------------------------------------------------------------------
-- PARTE 3: COMBINADORES ADICIONALES (Opcional, pero muy útiles)
---------------------------------------------------------------------------------------------------------

-- Versión pura de fmap
pMap :: (a -> b) -> Parser t a -> Parser t b
pMap = fmap


---------------------------------------------------------------------------------------------------------
-- SECCIÓN DE PRUEBAS LOCALES (Cargar en GHCI para testear)
---------------------------------------------------------------------------------------------------------

-- 1. Un tipo de token tonto para hacer pruebas
data TestToken = TA | TB | TC 
  deriving (Show, Eq)

-- 2. Parsers atómicos que consumen un token específico
parseA :: Parser TestToken TestToken
parseA = satisfy (== TA)

parseB :: Parser TestToken TestToken
parseB = satisfy (== TB)

-- 3. Ejemplo de Alternative (<|>): Parsea A o Parsea B
parseAorB :: Parser TestToken TestToken
parseAorB = parseA <|> parseB

-- 4. Ejemplo evaluando instancias directamente (fmap y >>=) sin notación "do"
-- Uso de Functor: Convertir el token TA a un String.
parseAMapped :: Parser TestToken String
parseAMapped = fmap (\token -> "He leido el token: " ++ show token) parseA

-- Uso de Monad (>>=): Encadenar parsers explícitamente pasando el resultado de uno al siguiente.
parseBind :: Parser TestToken String
parseBind = parseA >>= \resA -> 
            parseB >>= \resB -> 
            return ("Exito total: " ++ show resA ++ " seguido de " ++ show resB)

-- 5. Ejemplo de Monad (do block): Exactamente igual que parseBind, pero con notación "do".
parseA_then_B :: Parser TestToken String
parseA_then_B = do
    resA <- parseA
    resB <- parseB
    return ("Exito total: " ++ show resA ++ " seguido de " ++ show resB)

-- 6. Demo de AST: Construyendo una estructura de datos
-- Imagina que este es nuestro Árbol Sintáctico (AST) super simplificado.
data SimpleAST = Hoja TestToken | Nodo SimpleAST SimpleAST 
    deriving (Show, Eq)

-- Un parser que lee un TA, luego un TB, y los empaqueta en la estructura en árbol.
parseAST :: Parser TestToken SimpleAST
parseAST = do
    tokenA <- parseA
    tokenB <- parseB
    -- En vez de un String, construimos y devolvemos el nodo del árbol sintáctico.
    return (Nodo (Hoja tokenA) (Hoja tokenB))

-- PRUEBAS PARA GHCI:
-- runParser parseAMapped [TA, TB]           -- Right ("He leido el token: TA", [TB])
-- runParser parseBind [TA, TB, TC]          -- Right ("Exito total: TA seguido de TB", [TC])
-- runParser parseA_then_B [TA, TB, TC]      -- Idéntico al anterior, ¡el 'do' es magia sintáctica!
-- runParser parseAST [TA, TB, TC]           -- Right (Nodo (Hoja TA) (Hoja TB), [TC])

