-- DFA.hs
-- Implementación de un ADT (Abstract Data Type) para autómatas finitos deterministas.
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module DFA
  ( DFA(..), -- El ADT del autómata, que importamos junto a sus componentes
    dfaNext,-- La función de transición extendida
    dfaAccepts, -- La función de aceptación de cadenas
    checkDFA, -- Función de verificación que el DFA es realmente un autómata finito determinista (teóricamente bien definido)
    dfaNextPartial,
    dfaAcceptsPartial,
    checkPartial -- Funición de verificación débil, que asegura que el DFA se comporta como un autómata.
  ) where
-- Nota: En este documento se han concebido dos perspectivas:
--       1) Por un lado, la definición más fiel a la teoria, en la que el autómata es total (esto es, el dominio de la función de trancición es exactamente estados x alfabeto).
--       2) Por otro lado, una perspectiva más operativa, en la que el ADT solo cumple las propiedades esenciales de operatividad.
-------------------------------------------------------------------------------------------------------------------------------------------------
-- DEFINCIÓN DEL AUTÓMATA

import qualified Data.Map as M
import qualified Data.Set as S

data DFA a = DFA
  { states   :: S.Set a  -- Conjunto de estados
  , alphabet :: S.Set Char -- Conjunto de símbolos del alfabeto
  , initial  :: a -- Estado inicial
  , final    :: S.Set a -- Conjunto de estados finales
  , nxt      :: M.Map (a, Char) a -- La definción de diccionario evita los problemas de univaluación que presenta una relación, pero sin tener que renunciar a la evaluación del dominio.
  }


-- Función de transición extendida \hat{δ}
dfaNext :: Ord a => DFA a -> a -> [Char] -> a
dfaNext dfa q []       = q
dfaNext dfa q (x:xs)   = dfaNext dfa ((nxt dfa) M.! (q, x)) xs -- Si la cadena no es vacía, aplica recursivamente la función a cada elemento de la función de trancición dfa_nxt
-- nota: (M.!) :: Ord k => M.Map k a -> k -> a

-- Función de transición extendida \hat{δ} para autómatas parciales
dfaNextPartial :: Ord a => DFA a -> a -> [Char] -> Maybe a
dfaNextPartial dfa q []     = Just q
dfaNextPartial dfa q (x:xs) = case (M.lookup (q, x) (nxt dfa)) of
    Nothing -> Nothing
    Just q' -> dfaNextPartial dfa q' xs
-- (M.lookup) :: Ord k => k -> M.Map k a -> Maybe a


-- Función de aceptación: f (dfa, cadena) -> devuelve un booleano (True/False)
dfaAccepts dfa input = dfaNext dfa (initial dfa) input `elem` final dfa
-- Función de acepación para autómatas parciales
dfaAcceptsPartial dfa input = maybe False (`S.member` final dfa) (dfaNextPartial dfa (initial dfa) input)
-- maybe :: b -> (a -> b) -> Maybe a -> b
-----------------------------------------------------------------------------------------------------------------------------------------------
-- VERIFICACIÓN DEL AUTÓMATA (TEÓRICO)
-- Propiedades para que el ADT sea un DFA

-- 1) Consistencia de estados:
-- El estado inicial es un elemento del conjunto de estados
checkInitial :: Ord a => DFA a -> Bool
checkInitial dfa = S.member (initial dfa) (states dfa) 
-- El conjunto de estados finales es subconjunto del conjunto de estados
checkFinal :: Ord a => DFA a -> Bool
checkFinal dfa = S.isSubsetOf (final dfa) (states dfa)

-- 2) Corrección de la función de transición:
checkTransitions :: Ord a => DFA a -> Bool
checkTransitions dfa = all (\(q, x) -> S.member ((nxt dfa) M.! (q, x)) (states dfa)) [(q, x) | q <- S.toList (states dfa), x <- S.toList (alphabet dfa)]
-- (all devuelve True si es True para todos los predicados. Es como una iteración de disyunciones para boleanos.)
-- Definición segura: Con esto, si alguna clave falta o algo raro ocurre, simplemente se considera False o se ignora, y no hay excepción.
checkTransitionsSafe :: Ord a => DFA a -> Bool
checkTransitionsSafe dfa = all (\k -> maybe True (`S.member` states dfa) (M.lookup k (nxt dfa))) (M.keys (nxt dfa))
-- maybe :: b -> (a -> b) -> Maybe a -> b

-- 3) Totalización (dom(nxt) = states x alphabet)
checkTotal :: Ord a => DFA a -> Bool
checkTotal dfa = S.fromList [(q,x)| q <- S.toList (states dfa), x <- S.toList (alphabet dfa)] == M.keysSet (nxt dfa)
--  all (\(q,x) -> not . null $ [s | (q', x', s) <- S.toList (nxt dfa), q' == q, x' == x]) [(q, x) | q <- S.toList (states dfa), x <- S.toList (alphabet dfa)]

-- Check final:
checkDFA dfa = and [checkInitial dfa, checkFinal dfa, checkTransitionsSafe dfa, checkTotal dfa]
-----------------------------------------------------------------------------------------------------------------------------------------------
-- VERIFICACIÓN DEL AUTÓMATA (OPERACIONAL)
-- Propiedades esenciales: mantenemos checkInitial y checkFinal, y añadimos checkTransitionsPartial y checkPartial

-- 2') Corrección de la función de transición para autómatas parciales
checkTransitionsPartial :: Ord a => DFA a -> Bool
checkTransitionsPartial dfa = all (\q' -> S.member q' (states dfa)) (M.elems (nxt dfa))

-- 3'Asegura que el autómata está bien definido sin exigir totalización (el dominio de nxt está en states x alphabet)
checkPartial :: Ord a => DFA a -> Bool
checkPartial dfa = all (\(q,x) -> S.member q (states dfa) && S.member x (alphabet dfa)) (M.keys (nxt dfa))

-- Check final para una función parcial 
--(esta es una definición más débil que asegura el buen funcionamiento del
-- autómata aún no siendo definido total como en la teoría)
checkDFApartial dfa = and [checkInitial dfa, checkFinal dfa, checkTransitionsPartial dfa, checkPartial dfa]
------------------------------------------------------------------------------------------------------------------------------------------------
-- EJEMPLOS:


-- Ejemplo: autómata DFA total que acepta cadenas formadas por "ab"
dfaAB :: DFA Int
dfaAB = DFA
  { states   = S.fromList [0,1,2,3]
  , alphabet = S.fromList ['a','b']
  , initial  = 0
  , final    = S.singleton 2
  , nxt      = M.fromList
      [ ((0,'a'),1), ((0,'b'),3)
      , ((1,'a'),3), ((1,'b'),2)
      , ((2,'a'),1), ((2,'b'),3)
      , ((3,'a'),3), ((3,'b'),3)
      ]
  }


-- Ejemplo: autómata DFA parcial que acepta cadenas formadas por "ab"
dfaABpartial :: DFA Int
dfaABpartial = DFA
  { states   = S.fromList [0,1]
  , alphabet = S.fromList ['a','b']
  , initial  = 0
  , final    = S.singleton 0
  , nxt      = M.fromList
      [ ((0,'a'),1)
      , ((1,'b'),0)
      ]
  }

-- Ejemplo: Autómata total que acepta las cadenas de a's y b's que empiezan y acaban por 'a'
dfaA :: DFA Int
dfaA = DFA
  { states   = S.fromList [0,1,2,3]
  , alphabet = S.fromList ['a','b']
  , initial  = 0
  , final    = S.singleton 2
  , nxt = M.fromList 
  [((0, 'a'), 2), ((0, 'b'), 3),
   ((1, 'a'), 2), ((1, 'b'), 1),
   ((2, 'a'), 2), ((2, 'b'), 1),
   ((3, 'a'), 3), ((3, 'b'), 3)]
  }



