-- FiniteAutomatonTranslate.hs
-- En este documento se define un módulo que importa DFA.hs, NFA.hs y NFAeEps.hs
-- Las funciones de este módulo sirven para hacer la conversión entre autómatas finitos.
-- Martí Pons Garcia
-- Lladó, 2026
------------------------------------------------------------------------------------------------------------------------------------------------------------------

module FiniteAutomatonTranslate
 (  dfaToNFA, -- Un autómata determinista es un caso particular de no determinista
    nfaToDFA, -- Método: Construcción por subconjuntos
    dfaToNFAe,-- Un autómata determinista es un caso particular de no determinista con transiciones ε
    nfaToNFAe,-- Un autómata no determinista es un caso particular de transiciones ε con Eps vacía
    nfaeToDFA,-- Método: Eliminación de transiciones épsilon
    nfaeToNFA -- Aplicar composición nfaeToDFA y luego dfaToNFA
 ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified DFA
import qualified NFA
import qualified NFAeEps as NFAe

--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- De DFA a NFA
-- | Traducción de DFA a NFA.
--   Todo autómata determinista es un caso particular de autómata
--   no determinista en el que cada par (q,a) tiene exactamente
--   un estado sucesor.
--
--   La función de transición del DFA:
--        δ : Q × Σ → Q
--   se convierte en la relación:
--        δ' ⊆ Q × Σ × Q
--   definida por:
--        (q,a,q') ∈ δ'  ⇔  q' = δ(q,a)
--
--   Esta transformación preserva el lenguaje reconocido.

dfaToNFA :: Ord a => DFA.DFA a -> NFA.NFA a
dfaToNFA dfa = NFA.NFA
    { NFA.states   = DFA.states dfa
    , NFA.alphabet = DFA.alphabet dfa
    , NFA.initial  = DFA.initial dfa
    , NFA.final    = DFA.final dfa
    , NFA.nxt      = transitions
    }
  where
    transitions = S.fromList[ (q, x, q')| ((q, x), q') <- M.toList (DFA.nxt dfa)]

-- Check que el dfa es realmente un nfa
checkDFAeqNFA dfa w = NFA.nfaAccepts (dfaToNFA dfa) w == DFA.dfaAccepts dfa w
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- De NFA a DFA: construcción por subconjuntos
nfaToDFA :: Ord a => NFA.NFA a -> DFA.DFA (S.Set a) -- La contrucción por subconjuntos hace que el tipo del autómata DFA sea Set a
nfaToDFA nfa = DFA.DFA
    { DFA.states   = dfaStates
    , DFA.alphabet = NFA.alphabet nfa
    , DFA.initial  = startState
    , DFA.final    = dfaFinals         
    , DFA.nxt      = dfaTransitions 
    }
  where
  -- Estado inicial del DFA
    startState = S.singleton (NFA.initial nfa) -- {q0}
  -- 1) Construcción del conjunto de estados alcanzables
  --   (clausura bajo la transición)
    dfaStates = go S.empty (S.singleton startState) -- Empezamos con el {q0} estado frontera, y ningún estado visitado
      where 
        go visited frontier
          | S.null frontier = visited -- Cuando la frontera está vacía (el grafo no se puede extender más), ya tenemos todos los estados visitados.
          | otherwise =
              let newVisited  = S.union frontier visited --- Los estados de la frontera pasan a ser estados visitados
                  next        = S.fromList [new_nxt s x | s <- S.toList frontier , x <- S.toList (NFA.alphabet nfa)] -- Buscamos los nuevos estados para la nueva frontera aplicando la transición al último nivel de la frontera
                  newFrontier = S.difference next newVisited -- Para evitar repeticiones quitamos los estados que ya habíamos visitados de la nueva frontera
              in go newVisited newFrontier
  -- new_nxt calcula la dfa_nxt:  Es decir, la función de transición (como función explícitamente)
    new_nxt s x = S.unions [ NFA.nfa_nxt nfa q x | q <- S.toList s ]
  -- Para un conjunto s, tomamos cada uno de sus elementos q y le aplicamos la función de transición del nfa
  -- Esto no da un conjunto para cada q de s. Luego Producimos el conjunto unión para determinar la salida
  -- de la nueva función de transición para el dfa.
  ----------------------------------------------------------------
  -- 2) CONJUNTO DE ESTADOS FINALES DEL DFA
  -- Un subconjunto S es final si S ∩ F ≠ ∅
    dfaFinals = S.fromList [qs | qs <- S.toList dfaStates, not (S.null (S.intersection qs (NFA.final nfa)))]
  ----------------------------------------------------------------
  -- 3) FUNCIÓN DE TRANSICIÓN DEL DFA
  -- Como la función de transición en DFA.hs es un tipo Map, tenemos que pasarlo al tipo correcto, usando new_nxt
    dfaTransitions = M.fromList[ ((qs, x), new_nxt qs x)| qs <- S.toList dfaStates, x  <- S.toList (NFA.alphabet nfa)]
    
-- Check que el nfa es realmente un dfa
checkNFAeqDFA nfa w = DFA.dfaAccepts (nfaToDFA nfa) w == NFA.nfaAccepts nfa w

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- De DFA a NFA-ε
-- Un DFA es un caso particular de NFA-ε, en el que la función de transición solo devuelve un estado, 
-- y no hay transiciones-ε (es decir Eps es vacío)
dfaToNFAe :: Ord a => DFA.DFA a -> NFAe.NFAe a
dfaToNFAe dfa = NFAe.NFAe
    { NFAe.states   = DFA.states dfa
    , NFAe.alphabet = DFA.alphabet dfa
    , NFAe.initial  = DFA.initial dfa
    , NFAe.final    = DFA.final dfa
    , NFAe.nxt      = S.fromList[ (q, x, q') | ((q,x), q') <- M.toList (DFA.nxt dfa)]
    , NFAe.eps      = S.empty
    }

-- Check que el DFA es realmente un NFA-ε
checkDFAeqNFAe dfa w = NFAe.nfaeAccepts (dfaToNFAe dfa) w == DFA.dfaAccepts dfa w

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- De NFA a NFA-ε
-- Un NFA se diferencia de un NFA-ε en que no tiene transiciones-ε (es decir Eps es vacío)
nfaToNFAe :: Ord a => NFA.NFA a -> NFAe.NFAe a
nfaToNFAe nfa = NFAe.NFAe
    { NFAe.states   = NFA.states nfa
    , NFAe.alphabet = NFA.alphabet nfa
    , NFAe.initial  = NFA.initial nfa
    , NFAe.final    = NFA.final nfa
    , NFAe.nxt      = NFA.nxt nfa
    , NFAe.eps      = S.empty
    }

-- Check que el NFA es realmente un NFA-ε
checkNFAeqNFAe nfa w = NFAe.nfaeAccepts (nfaToNFAe nfa) w == NFA.nfaAccepts nfa w

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- De NFA-ε a DFA: eliminación de transiciones-ε + construcción por subconjuntos
nfaeToDFA :: Ord a => NFAe.NFAe a -> DFA.DFA (S.Set a) 
nfaeToDFA nfae = DFA.DFA
    { DFA.states   = dfaStates
    , DFA.alphabet = NFAe.alphabet nfae
    , DFA.initial  = startState
    , DFA.final    = dfaFinals
    , DFA.nxt      = dfaTransitions 
    }
  where
  -- Estado inicial del DFA
    startState = NFAe.epsClosure nfae (NFAe.initial nfae) -- Clausure-ε de q0
  -- 1) Construcción del conjunto de estados alcanzables
  --   (clausura bajo la transición)
    dfaStates = go S.empty (S.singleton startState) -- Empezamos con el {q0} estado frontera, y ningún estado visitado
      where 
        go visited frontier
          | S.null frontier = visited -- Cuando la frontera está vacía (el grafo no se puede extender más), ya tenemos todos los estados visitados.
          | otherwise =
              let newVisited  = S.union frontier visited --- Los estados de la frontera pasan a ser estados visitados
                  next        = S.fromList [new_nxtEps s x | s <- S.toList frontier , x <- S.toList (NFAe.alphabet nfae)] -- Buscamos los nuevos estados para la nueva frontera aplicando la transición al último nivel de la frontera
                  -- nextEps     = S.fromList [NFAe.epsClosure q | q <- next] -- Miramos las transiciones ε de los nuevos estados
                  newFrontier = S.difference next newVisited -- Para evitar repeticiones quitamos los estados que ya habíamos visitados de la nueva frontera
              in go newVisited newFrontier
  -- Función de transición (sin las transiciones ε)
    new_nxt qs x = S.unions [NFAe.nfae_nxt nfae q x | q <- S.toList qs]
  -- Función de transición (añadiendo las transiciones ε)
    new_nxtEps qs x = NFAe.epsClosureSet nfae (new_nxt qs x)
  ----------------------------------------------------------------
  -- 2) CONJUNTO DE ESTADOS FINALES DEL DFA
  -- Un subconjunto S es final si S ∩ F ≠ ∅
    dfaFinals = S.fromList [qs | qs <- S.toList dfaStates, not (S.null (S.intersection qs (NFAe.final nfae)))]
  ----------------------------------------------------------------
  -- 3) FUNCIÓN DE TRANSICIÓN DEL DFA
  -- Como la función de transición en DFA.hs es un tipo Map, tenemos que pasarlo al tipo correcto, usando new_nxt
    dfaTransitions = M.fromList[ ((qs, x), new_nxtEps qs x)| qs <- S.toList dfaStates, x  <- S.toList (NFAe.alphabet nfae)]

-- Check que el NFA-ε es realmente un DFA
checkNFAeeqDFA nfae w = DFA.dfaAccepts (nfaeToDFA nfae) w == NFAe.nfaeAccepts nfae w

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- De NFA-ε a NFA: eliminación de transiciones-ε
nfaeToNFA :: Ord a => NFAe.NFAe a -> NFA.NFA a
nfaeToNFA nfae = NFA.NFA
    { NFA.states   = nfaStates
    , NFA.alphabet = NFAe.alphabet nfae
    , NFA.initial  = NFAe.initial nfae
    , NFA.final    = newFinals
    , NFA.nxt      = newTransitions
    }
  where
    -- 1) CONJUNTO DE ESTADOS
    -- Los estados son los mismos
    nfaStates = NFAe.states nfae
    -- 2) ELIMINACIÓN DE LAS TRANSICIONES ε PARA DEFINIR LA RELACIÓN DE TRANSICIÓN del NFA
    -- Función de transición (sin las transiciones ε)
    new_nxt q x = S.unions [NFAe.nfae_nxt nfae r x | r <- S.toList (NFAe.epsClosure nfae q)]
    -- Función de transición (añadiendo las transiciones ε)
    new_nxtEps q x = NFAe.epsClosureSet nfae (new_nxt q x)
    -- Nueva función de transición
    newTransitions = S.fromList [(q, x, r) | q <- S.toList nfaStates, x <- S.toList (NFAe.alphabet nfae), r <- S.toList (new_nxtEps q x)]
    -- 3) CONJUNTO DE ESTADOS FINALES DEL NFA
    newFinals = S.fromList [q | q <- S.toList (NFAe.states nfae), not(S.null (S.intersection(NFAe.epsClosure nfae q) (NFAe.final nfae)))]

-- Check que el NFA-ε es realmente un NFA
checkNFAeeqNFA nfae w = NFA.nfaAccepts (nfaeToNFA nfae) w == NFAe.nfaeAccepts nfae w
