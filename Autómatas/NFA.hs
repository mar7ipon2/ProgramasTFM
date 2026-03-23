-- NFA.hs
-- Implementación de un ADT (Abstract Data Type) para autómatas finitos no deterministas.
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module NFA
  ( NFA(..), -- El ADT del autómata, que importamos junto a sus componentes
    nfa_nxt, -- La función de transición derivada de la relación de transición
    nfaNext,-- La función de transición extendida
    nfaAccepts, -- La función de aceptación de cadenas
    checkNFA -- Función de verificación que el NFA es realmente un autómata finito no determinista
  ) where

-------------------------------------------------------------------------------------------------------------------------------------------
-- DEFINCIÓN DEL AUTÓMATA
import qualified Data.Set as S

data NFA a = NFA
  { states   :: S.Set a  -- Conjunto de estados
  , alphabet :: S.Set Char -- Conjunto de símbolos del alfabeto
  , initial  :: a -- Estado inicial
  , final    :: S.Set a -- Conjunto de estados finales
  , nxt      :: S.Set (a, Char, a) -- La relación de transición
  }

-- La función de transición derivada de relación de transición  nxt
nfa_nxt :: Ord a => NFA a -> a -> Char -> S.Set a
nfa_nxt nfa q x = S.fromList [s | (q', x', s) <- S.toList (nxt nfa), q' == q, x' == x]

-- δ* (nfa, estado, cadena) -> devuelve un conjunto de estado
nfaNext :: Ord a => NFA a -> a -> [Char] -> S.Set a
nfaNext nfa q []     = S.singleton q -- Si la cadena es vacía, devuelve el conjunto con solo el estado q
nfaNext nfa q (x:xs) = S.unions (S.map ((\ys y -> nfaNext nfa y ys) xs) (nfa_nxt nfa q x))
-- Si la cadena no es vacía, aplica recursivamente la función a cada elemento de la función de trancición nfa_nxt

-- Versión list comprehension: nfaNext nfa q (x:xs) = S.unions [ nfaNext nfa y xs | y <- S.toList (nxt nfa (q,x)) ]

-- Función de aceptación: f (nfa, cadena) -> devuelve un booleano (True/False)
nfaAccepts nfa input = not (S.null (S.intersection (final nfa) (nfaNext nfa (initial nfa) input)))
-----------------------------------------------------------------------------------------------------------------------------------------------
-- VERIFICACIÓN DEL AUTÓMATA
-- Propiedades para que el ADT sea un NFA

-- 1) Consistencia de estados:
-- El estado inicial es un elemento del conjunto de estados
checkInitial :: Ord a => NFA a -> Bool
checkInitial nfa = S.member (initial nfa) (states nfa) 
-- El conjunto de estados finales es subconjunto del conjunto de estados
checkFinal :: Ord a => NFA a -> Bool
checkFinal nfa = S.isSubsetOf (final nfa) (states nfa)

-- 2) Corrección de la función de transición:
--    comprueba que los elementos (q,x,q') de la relación nxt son de (states, alphabet, states)
checkTransitions :: Ord a => NFA a -> Bool
checkTransitions nfa = all (\(q, x, q') -> and [S.member q  (states nfa), S.member x  (alphabet nfa), S.member q' (states nfa)]) (S.toList (nxt nfa))

-- Check final:
checkNFA nfa = and [checkInitial nfa, checkFinal nfa, checkTransitions nfa]

------------------------------------------------------------------------------------------------------------------------------------------------
-- EJEMPLOS:

-- Ejemplo: Autómata que acepta las cadenas de a's y b's que empiezan y acaban por 'a'
nfaA :: NFA Int
nfaA = NFA
  { states   = S.fromList [0,1,2]
  , alphabet = S.fromList ['a','b']
  , initial  = 0
  , final    = S.singleton 2
  , nxt = S.fromList [(0, 'a', 1) , (0, 'a', 2), (1, 'a', 1), (1, 'b', 1), (1, 'a', 2)]
  }
