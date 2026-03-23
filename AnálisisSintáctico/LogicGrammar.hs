-- LogicGrammar.hs
-- Definición de la gramática CFG para la lógica proposicional.
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module LogicGrammar where

import LogicTokens
import CFGrammar

-- Notas sobre la gramática para el parser de Descenso Recursivo:
-- 1. Debe ser no ambigua.
-- 2. Debe respetar la precedencia predeterminada de operadores lógicos:
--    Mayor precedencia: NOT
--    Luego: AND
--    Luego: OR
--    Luego: IMPLIES
--    Menor precedencia: IFF
-- 3. Cuidado con la recursión por la izquierda (Direct Left Recursion). 
--    Los parsers top-down entran en bucle infinito si tienes reglas como A -> A op B.

-- Conjunto de símbolos no terminales.
-- Implementamos los no terminales para cada nivel de precedencia (Not -> And -> Or -> Implies -> Iff)
-- Empezamos del que tiene menor precedencia al que más.
-- Los No Terminales con comilla sirve para implementar la eliminación de la Left Recursion
data LogicNT     =
    I    | I'   | -- Símbolos inicial, para IFF.
    Imp  | Imp' | 
    O    | O'   | 
    A    | A'   | 
    N    |        -- NOT no es binario y asocia por la derecha o directamente al átomo
    Atom          -- Los paréntesis ya no necesitan Atom'
    deriving (Show, Eq, Ord)

-- La gramática libre de contexto teórica.
-- logicCFG :: CFG LogicNT Token
-- logicCFG = CFG { ... }
logicCFG :: CFG LogicNT Token
logicCFG = CFG 
  { nonTerminals = S.fromList [I, I', Imp, Imp', O, O', A, A', N, Atom]
  -- Para TVar, al ser String infinito, en la teoría definimos un par de representantes
  , terminals    = S.fromList [TAnd, TOr, TNot, TImplies, TIff, TLParen, TRParen, TVar "p", TVar "q"]
  , initial      = I
  , productions  = S.fromList
      [ (I,    [Nonterminal Imp, Nonterminal I'])
      , (I',   [Terminal TIff, Nonterminal Imp, Nonterminal I'])
      , (I',   [])
      , (Imp,  [Nonterminal O, Nonterminal Imp'])
      , (Imp', [Terminal TImplies, Nonterminal O, Nonterminal Imp'])
      , (Imp', [])
      , (O,    [Nonterminal A, Nonterminal O'])
      , (O',   [Terminal TOr, Nonterminal A, Nonterminal O'])
      , (O',   [])
      , (A,    [Nonterminal N, Nonterminal A'])
      , (A',   [Terminal TAnd, Nonterminal N, Nonterminal A'])
      , (A',   [])
      , (N,    [Terminal TNot, Nonterminal N]) -- NOT se aplica sobre sí mismo o sobre Atom
      , (N,    [Nonterminal Atom])
      , (Atom, [Terminal TLParen, Nonterminal I, Terminal TRParen])
      , (Atom, [Terminal (TVar "p")]) -- Representante para la teoría
      , (Atom, [Terminal (TVar "q")])
      ]
  }

