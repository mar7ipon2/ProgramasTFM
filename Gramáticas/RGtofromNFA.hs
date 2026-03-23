-- RGtofromNFA.hs
-- En este documento se define una función que transforma un ADT RG (Regular Grammar)
-- a un NFA (Non deterministic Finite Automata), y viceversa. 
-- Además se propone un ejemplo con el lenguaje de cadenas formadas por sílabas "ab".
-- Martí Pons Garcia
-- Lladó, 2026
------------------------------------------------------------------------------------------------------------------------------------------------------------------

module RGtofromNFA
    ( rgToNFA
    , normalizeRG
    , nfaToRG
    ) where


import qualified Data.Set as S
import qualified NFA
import qualified RGrammar as RG

------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- GRAMÁTICA REGULAR -> AFN: Paso de Gramática regular a autómata finito no determinista
------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Función para los estados: Cada símbolo no terminal es un estado
rgStates :: Ord nt => RG.RG nt t -> S.Set nt
rgStates rg = RG.nonTerminals rg

-- Función para el alfabeto: Cada símbolo terminal es un símbolo del alfabeto
rgAlphabet :: RG.RG nt t -> S.Set t
rgAlphabet rg = RG.terminals rg

-- Función estado inicial: el estado inicial es el símbolo no terminal inicial
rgInitial :: RG.RG nt t -> nt
rgInitial rg = RG.initial rg

-- Función estados finales: los estados finales vienen dados por las producciónes A -> ε
rgFinalStates :: Ord nt => RG.RG nt t -> S.Set nt
rgFinalStates rg = S.fromList [nt | (nt, RG.RGEps) <- S.toList (RG.productions rg)]

-- Función transiciones: las transiciones del autómata vienen dadas por las producciones A -> aB
-- La función calcula la lista de pares (q, x, ps), dónde:
-- q es el símbolo no terminal de la producción A -> aB (es A)
-- x es el símbolo terminal de la producción A -> aB (es 'a')
-- ps es el conjunto de símbolos no terminales de las producciones de la forma A -> aB (todos los Bs que estan en producciones A -> aB)
-- Función para las transiciones de la forma A -> aB
rgTransitions :: (Ord nt, Ord t) => RG.RG nt t -> S.Set (nt, t, nt)
rgTransitions rg = S.fromList[(q, x, b) | (q, RG.RGT x b) <- S.toList (RG.productions rg)]

-- Función final que convierte una RG a un NFA
rgToNFA :: Ord nt => RG.RG nt Char -> NFA.NFA nt
rgToNFA rg = NFA.NFA
  { NFA.states   = rgStates rg
  , NFA.alphabet = rgAlphabet rg
  , NFA.initial  = rgInitial rg
  , NFA.final    = rgFinalStates rg
  , NFA.nxt      = rgTransitions rg
  }

-- | Transforma una gramática regular eliminando las producciones estrictamente terminales (A -> a)
-- y sustituyéndolas por una transición a un nuevo estado (A -> aB) del que se puede salir con épsilon (B -> ε).
-- Requiere que el usuario proporcione un estado 'fresh' (B) que no exista previamente en la gramática.
normalizeRG :: (Ord nt, Ord t) => nt -> RG.RG nt t -> RG.RG nt t
normalizeRG fresh rg = rg
  { RG.nonTerminals = S.insert fresh (RG.nonTerminals rg)
  , RG.productions  = S.union newProds (S.singleton (fresh, RG.RGEps)) }
  where
    newProds = S.map transformProd (RG.productions rg)
    transformProd (q, RG.RGOnly a) = (q, RG.RGT a fresh)
    transformProd p                = p

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- AFN -> GRAMÁTICA REGULAR: Paso de autómata finito no determinista a Gramática regular
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Función para los no terminales: Cada estado es un símbolo no terminal
nfaNonterminals :: Ord nt => NFA.NFA nt -> S.Set nt
nfaNonterminals nfa = NFA.states nfa

-- Función para los terminales: Cada símbolo del alfabeto es un símbolo terminal
nfaTerminals :: Ord nt => NFA.NFA nt -> S.Set Char
nfaTerminals nfa = NFA.alphabet nfa

-- Función símbolo inicial: El estado inicial es el símbolo no terminal inicial
nfaInitial :: NFA.NFA nt -> nt
nfaInitial nfa = NFA.initial nfa

-- Función para las producciones A -> ε: Los símbolos A de esas producciones son los estados finales
nfaEpsilonTrans :: Ord nt => NFA.NFA nt -> S.Set (RG.Production nt Char)
nfaEpsilonTrans nfa = S.fromList [(q, RG.RGEps) | q <- S.toList(NFA.final nfa)]

-- Función para las produccioones A -> aB: Los símbolos que definen las producciones A -> aB vienen de las transiciones nxt:
-- La fucnión calcula las produciones (A, a B), dónde:
-- A y 'a' son elementos de una terna (A,a,nts) de la relación nxt.
-- B es un elemento del conjunto nts, perteneciente a la terna anterior de la relación nxt. 
nfaTrans :: Ord nt => NFA.NFA nt -> S.Set (RG.Production nt Char)
nfaTrans nfa = S.fromList [(q, RG.RGT t nt) | (q, t, nt) <- S.toList (NFA.nxt nfa)]

-- Función final que convierte un NFA a una RG
nfaToRG :: Ord nt => NFA.NFA nt -> RG.RG nt Char
nfaToRG nfa = RG.RG
  { RG.nonTerminals = nfaNonterminals nfa
  , RG.terminals    = nfaTerminals nfa
  , RG.initial      = nfaInitial nfa
  , RG.productions  = S.union (nfaEpsilonTrans nfa) (nfaTrans nfa)
  }

--------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJEMPLOS
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJEMPLO 1: Gramática Regular que genera cadenas formadas por "ab"
rgSimple :: RG.RG Int Char
rgSimple = RG.RG
  { RG.nonTerminals = S.fromList [1,2]
  , RG.terminals    = S.fromList ['a','b']
  , RG.initial      = 1
  , RG.productions  = 
    S.fromList
      [ (1, RG.RGT 'a' 2)
      , (1, RG.RGEps)
      , (2, RG.RGT 'b' 1)
      , (2, RG.RGOnly 'b')
      ]
  }

-- PRUEBAS GHCI:
-- 0) Visualización de cadenas
-- RG.prettySF show (:[]) [RG.Nonterminal 1]
-- RG.prettySF show (:[]) [RG.Terminal 'a', RG.Terminal 'b']

-- 1) Derivación inmediata:
-- RG.der1 rgSimple [RG.Nonterminal (RG.initial rgSimple)]
-- map (RG.prettySF show (:[])) (S.toList (RG.der1 rgSimple [RG.Nonterminal (RG.initial rgSimple)]))

-- 2) Derivaciones por niveles:
-- Nivel 1: RG.derLevelsk rgSimple 1 [RG.Nonterminal 1]
-- map (RG.prettySF show (:[])) (S.toList (RG.derLevelsk rgSimple 1 [RG.Nonterminal (RG.initial rgSimple)]))
-- Nivel 4: RG.derLevelsk rgSimple 4 [RG.Nonterminal (RG.initial rgSimple)]
-- map (RG.prettySF show (:[])) (S.toList (RG.derLevelsk rgSimple 4 [RG.Nonterminal 1]))

-- 3) Lenguaje (limitado):
-- RG.languageRGk 5 rgSimple
-- map (RG.prettySF show (:[])) (S.toList (RG.languageRGk 5 rgSimple))

-- 4) CONVERTIR rgSimple A UN AUTÓMATA FINITO NO DETERMINISTA:
-- nfaFromRG = rgToNFA rgSimple

-- 5) Inspeccionar el autómata
-- NFA.states nfaFromRG
-- NFA.alphabet nfaFromRG
-- NFA.initial nfaFromRG
-- NFA.final nfaFromRG
-- NFA.nxt nfaFromRG

-- 6) Validar que es un autómata finito no determinista:
-- NFA.checkNFA nfaFromRG

-- Aceptación de cadenas:
-- NFA.nfaAccepts nfaFromRG "ab"
-- NFA.nfaAccepts nfaFromRG "aba"
-- NFA.nfaAccepts nfaFromRG "abab"
-- NFA.nfaAccepts nfaFromRG ""
 

-- EJEMPLO 2: Autómata finito no determinista que acepta cadenas formadas por "ab"
nfaSimple :: NFA.NFA Int
nfaSimple = NFA.NFA 
  { NFA.states = S.fromList [0,1,2]
  , NFA.alphabet = S.fromList ['a','b']
  , NFA.initial  = 0
  , NFA.final    = S.singleton 2
  , NFA.nxt      = S.fromList
      [ (0, 'a', 1)
      , (1, 'b', 2)
      , (2, 'a', 1)
      ]
  } 

-- PRUEBAS GHCI:
-- 1) Componentes:
-- NFA.states nfaSimple
-- NFA.alphabet nfaSimple
-- NFA.initial nfaSimple
-- NFA.final nfaSimple
-- NFA.nxt nfaSimple

-- 2) Validar que es un autómata finito no determinista:
-- NFA.checkNFA nfaSimple

-- 3) Comprobar transiciones manualmente:
-- [ (q,c,qs)| (q,c,qs) <- S.toList (NFA.nxt nfaSimple)]

-- 4) Aceptación:
-- NFA.nfaAccepts nfaSimple ""
-- NFA.nfaAccepts nfaSimple "ab"
-- NFA.nfaAccepts nfaSimple "aba"
-- NFA.nfaAccepts nfaSimple "abab"
-- NFA.nfaAccepts nfaSimple "a"
-- NFA.nfaAccepts nfaSimple "abb"

-- 5) CONVERTIR nfaSimple A UNA GRAMÁTICA REGULAR:
--  rgFromNFA = nfaToRG nfaSimple

-- 6) Inspeccionar gramática:
-- RG.nonTerminals rgFromNFA
-- RG.terminals rgFromNFA
-- RG.initial rgFromNFA
-- RG.productions rgFromNFA

-- 7) Derivaciones:
-- RG.der1 rgFromNFA [RG.Nonterminal (RG.initial rgFromNFA)]
-- map (RG.prettySF show (:[])) (S.toList (RG.der1 rgFromNFA [RG.Nonterminal (RG.initial rgFromNFA)]))

-- 8) Lenguaje generado (limitado)
-- RG.languageRGk 6 rgFromNFA
-- map (RG.prettySF show (:[])) (S.toList (RG.languageRGk 6 rgFromNFA))

-- 9) Prueba final de coherencia
-- rg2 = nfaToRG (rgToNFA rgSimple)
-- map (RG.prettySF show (:[])) (S.toList (RG.languageRGk 6 rg2))
