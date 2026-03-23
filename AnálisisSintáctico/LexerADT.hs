-- LexerADT.hs
-- Implementación de un ADT (Abstract Data Type) para un lexer.
-- Este implementa un algoritmo genérico para que, dado un conjunto de reglas (Autómata+Constructor de Token)
-- consume una cadena y produce una lista de tokens.
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module LexerADT
    ( LexRule
    , LexError(..)
    , runLexer
    , longestMatch
    ) where

import DFA
import qualified Data.Map as M
import qualified Data.Set as S

-- Una regla léxica asocia un autómata a una función que crea un token desde el string aceptado.
type LexRule token st = (DFA st, String -> token)

-- Error léxico capturando la posición y el carácter problemático a la hora de tokenizar.
data LexError = LexError { offset :: Int, unxChar :: Char } 
  deriving (Show, Eq)

-- Función auxiliar que devuelve el string, si nos encontramos en un estado de aceptación
accepts :: Ord st => DFA st -> st -> String -> Maybe String
accepts dfa q str
    | S.member q (final dfa) = Just str
    | otherwise              = Nothing

-- Función auxiliar que devuelve Just el string consumido si nos encontramos en un estado de aceptación,
-- o el mejor resultado anterior si no estamos en aceptación. Eliminamos los if/else por guardas.
newBest :: Ord st => DFA st -> st -> String -> Maybe String -> Maybe String
newBest dfa q pref best
    | S.member q (final dfa) = Just (reverse pref)
    | otherwise              = best

-- Función auxiliar: Encuentra el prefijo más largo que un DFA particular acepta en el string.
-- Devuelve Just "str" si fue capaz de aceptar algún prefijo.
matchDFA :: Ord st => DFA st -> String -> Maybe String
matchDFA dfa input = go (initial dfa) input "" (accepts dfa (initial dfa) "")
  where
    -- Iteramos caracter a caracter. 'pref' es el string acumulado (al revés para ser O(1)). 
    -- 'best' guarda el último estado de aceptación válido que hemos visto. Es un (Maybe String): 
    -- Se interpreta como la cadena más larga aceptada hasta el momento
    go q [] pref best
        | S.member q (final dfa) = Just (reverse pref) -- Cuando hemos acabado de leer la cadena, por última vez si estamos en un estado de aceptación
        | otherwise              = best                -- Si la cadena completa no es aceptada, nos quedamos con la última cadena aceptada
    go q (x:xs) pref best =
        let best' = newBest dfa q pref best -- Miramos si estamos en un estado de actación para tomar o no un nuevo best. Este best' es antes de leer (x:xs)
        in case M.lookup (q, x) (nxt dfa) of -- Aplicamos la función de transición del dfa y miramos:
            Nothing -> best' -- El autómata no puede avanzar más, devolvemos lo mejor guardado. Esto significa que el resto de la cadena ya no se puede aceptar.
            Just q' -> go q' xs (x:pref) best' -- Paso recursivo. Añadimos x a la cadena leída, y expliramos con el nuevo estado que nos da la función de transición: q'

-- Procesa el prefijo más largo aceptado por ALGÚN autómata en la posición actual.
-- Si hay empate (dos reglas aceptan prefijos de la misma longitud), gana la regla declarada antes, priorizando.
longestMatch :: Ord st => [LexRule token st] -> String -> Maybe (token, String)
longestMatch rules input = case matches of
    [] -> Nothing
    (m:ms) -> Just (findBest m ms) -- De los tokens obtenido, aplicamos findBest para quedarnos con el mejor (más largo y prioritario)
  where
    -- Obtenemos las coincidencias de todos los DFAs que lograron leer al menos 1 caracter
    -- Buscamos los str, que matchDFA nos acepta (y que no son ε)
    -- Tomamos las reglas lexicas, y devolvemos pares de (token, string al que hace referencia ese token)
    matches = [ (constr str, str) | (dfa, constr) <- rules, Just str <- [matchDFA dfa input], not (null str) ]
    -- 'findBest' con guardas evalúa la lista de matches encontrados para quedarse con el de mayor longitud
    findBest (t, s) [] = (t, drop (length s) input) -- Nos devuelve el mejor token, junto al resto de la cadena.
        --let (t, s) = acc -- t == token, s == string
        --in (t, drop (length s) input) -- Nos devuelve el 
    findBest acc@(t1, s1) (curr@(t2, s2):rest) -- @ es un alias: acc@(t1, s1) es equivalente a let acc = (t1, t2)
        | length s2 > length s1 = findBest curr rest -- curr es estrictamente más largo, lo preferimos
        | otherwise             = findBest acc rest  -- empate o menor: nos quedamos con acc (que apareció antes)

-- Bucle principal del lexer. Consume la entrada iterativamente hasta generar todos los tokens.
-- O bien la cadena tiene un error léxico, o bien nos devuelve la lista de tokens.
runLexer :: Ord st => [LexRule token st] -> String -> Either LexError [token] -- Either es un tipo que devuelve uno entre dos valores. Se define con dos constructores derecha y izquierda (Right y Left, respectivamente)
runLexer rules input = go input 0 []
  where
    go [] _ acc = Right (reverse acc) -- Cuando hayamos procesado los tokens, devolvemos la lista ordenada (como vamos metiendo nuevos tokens delante la lista acumuladora, acc, hay que girarla para obtener la lista en el orden correcto)
    go s offset acc = 
        case longestMatch rules s of -- Empezamos buscando el primer token y la lista que queda para leer
            Nothing -> Left (LexError offset (head s)) -- Si no se encuentra nuevo token, se da el error con el valor en donde empieza el fallo (posición del string + primer carácter de este substring)
            Just (tok, rest) -> 
                let consumed = length s - length rest -- Guardamos el número de carácteres que hemos consumido
                in go rest (offset + consumed) (tok : acc) -- Guardamos el token en el acumulador, y iteramos con el resto de la cadena


------------------------------------------------------------------------------------------------------------------------------------------------
-- EJEMPLOS Y PRUEBAS DEL LEXER:

-- Un token de prueba simple
data TestToken = TWord String | TNum String 
  deriving (Show, Eq)

-- Ejemplo de DFA que solo acepta números continuos ("1", "44", "0")
dfaNum :: DFA Int
dfaNum = DFA
  { states   = S.fromList [0, 1]
  , alphabet = S.fromList ['0'..'9']
  , initial  = 0
  , final    = S.singleton 1
  , nxt      = M.fromList [ ((0, d), 1) | d <- ['0'..'9'] ] `M.union`
               M.fromList [ ((1, d), 1) | d <- ['0'..'9'] ]
  }

-- Ejemplo de DFA que solo acepta letras minúsculas seguidas ("abc", "p")
dfaWord :: DFA Int
dfaWord = DFA
  { states   = S.fromList [0, 1]
  , alphabet = S.fromList ['a'..'z']
  , initial  = 0
  , final    = S.singleton 1
  , nxt      = M.fromList [ ((0, c), 1) | c <- ['a'..'z'] ] `M.union`
               M.fromList [ ((1, c), 1) | c <- ['a'..'z'] ]
  }

-- Nuestras reglas de prueba
testRules :: [LexRule TestToken Int]
testRules =  [ (dfaNum, TNum), (dfaWord, TWord)]

-- Prueba rápida desde GHCI: 
-- > runLexer testRules "abc123z"
-- Devolverá: Right [TWord "abc",TNum "123",TWord "z"]
-- Si pones algo como "abc 123" devolverá Left (LexError 3 ' ') ya que no tenemos DFA de espacio en blanco.
