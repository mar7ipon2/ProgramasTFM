-- LogicTokens.hs
-- Implementación del módulo de tokens para la lógica proposicional.
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module LogicTokens
    ( Token(..)
    , dfaAnd
    , dfaOr
    , dfaNot
    , dfaImplies
    , dfaIff
    , dfaLParen
    , dfaRParen
    , dfaVar
    ) where

import DFA
import RGrammar qualified as RG
import NFA
import RGtofromNFA
import FiniteAutomatonTranslate
import qualified Data.Set as S

-- | Vocabulario de la lógica.
data Token = 
    TAnd        -- ^ And o &
  | TOr         -- ^ Or o |
  | TNot        -- ^ ~ o !
  | TImplies    -- ^ ->
  | TIff        -- ^ <->
  | TLParen     -- ^ (
  | TRParen     -- ^ )
  | TVar String -- ^ Variables dependientes p, q, r, p1...
  deriving (Show, Eq, Ord)


-- | Función auxiliar para pasar de RG a DFA.
-- Primero normaliza la gramática usando un estado "nuevo" (fresh) para que las producciones 'A -> a' 
-- pasen a ser 'A -> aB' y 'B -> eps', donde B es un estado final.
-- Luego convierte de RG a NFA, y finalmente de NFA a DFA (con estados conjuntos por la determinización).
rgToDFA :: Ord nt => nt -> RG.RG nt Char -> DFA.DFA (S.Set nt)
rgToDFA fresh = nfaToDFA . rgToNFA . normalizeRG fresh

--------------------------------------------------------------------------------
-- GRAMÁTICAS REGULARES Y DFAS PARA CADA TOKEN
--------------------------------------------------------------------------------

-- 1) Token AND (TAnd): Acepta "&" o "And"
rgAnd :: RG.RG Int Char
rgAnd = RG.RG
  { RG.nonTerminals = S.fromList [0,1,2]
  , RG.terminals    = S.fromList ['&', 'A','n','d']
  , RG.initial      = 0
  , RG.productions  = S.fromList
      [ (0, RG.RGOnly '&')
      , (0, RG.RGT 'A' 1)
      , (1, RG.RGT 'n' 2)
      , (2, RG.RGOnly 'd')
      ]
  }
dfaAnd = rgToDFA 99 rgAnd

-- 2) Token OR (TOr): Acepta "|" o "Or"
rgOr :: RG.RG Int Char
rgOr = RG.RG
  { RG.nonTerminals = S.fromList [0,1]
  , RG.terminals    = S.fromList ['|', 'O', 'r']
  , RG.initial      = 0
  , RG.productions  = S.fromList
      [ (0, RG.RGOnly '|')
      , (0, RG.RGT 'O' 1)
      , (1, RG.RGOnly 'r')
      ]
  }
dfaOr = rgToDFA 99 rgOr

-- 3) Token NOT (TNot): Acepta "~" o "!"
rgNot :: RG.RG Int Char
rgNot = RG.RG
  { RG.nonTerminals = S.fromList [0]
  , RG.terminals    = S.fromList ['~', '!']
  , RG.initial      = 0
  , RG.productions  = S.fromList [ (0, RG.RGOnly '~'), (0, RG.RGOnly '!') ]
  }
dfaNot = rgToDFA 99 rgNot

-- 4) Token IMPLIES (TImplies): Acepta "->"
rgImplies :: RG.RG Int Char
rgImplies = RG.RG
  { RG.nonTerminals = S.fromList [0,1]
  , RG.terminals    = S.fromList ['-', '>']
  , RG.initial      = 0
  , RG.productions  = S.fromList [ (0, RG.RGT '-' 1), (1, RG.RGOnly '>') ]
  }
dfaImplies = rgToDFA 99 rgImplies

-- 5) Token IFF (TIff): Acepta "<->"
rgIff :: RG.RG Int Char
rgIff = RG.RG
  { RG.nonTerminals = S.fromList [0,1,2]
  , RG.terminals    = S.fromList ['<', '-', '>']
  , RG.initial      = 0
  , RG.productions  = S.fromList
      [ (0, RG.RGT '<' 1)
      , (1, RG.RGT '-' 2)
      , (2, RG.RGOnly '>')
      ]
  }
dfaIff = rgToDFA 99 rgIff

-- 6) Tokens Paréntesis
rgLParen, rgRParen :: RG.RG Int Char
rgLParen = RG.RG { RG.nonTerminals = S.singleton 0, RG.terminals = S.singleton '(', RG.initial = 0, RG.productions = S.singleton (0, RG.RGOnly '(') }
rgRParen = RG.RG { RG.nonTerminals = S.singleton 0, RG.terminals = S.singleton ')', RG.initial = 0, RG.productions = S.singleton (0, RG.RGOnly ')') }
dfaLParen = rgToDFA 99 rgLParen
dfaRParen = rgToDFA 99 rgRParen

-- 7) Token Variables (TVar): Acepta [a-z][a-z0-9]*
rgVar :: RG.RG Int Char
rgVar = RG.RG
  { RG.nonTerminals = S.fromList [0,1]
  , RG.terminals    = S.fromList (['a'..'z'] ++ ['0'..'9'])
  , RG.initial      = 0
  , RG.productions  = S.fromList
      (  [ (0, RG.RGT c 1) | c <- ['a'..'z'] ]
      ++ [ (0, RG.RGOnly c) | c <- ['a'..'z'] ]
      ++ [ (1, RG.RGT c 1) | c <- ['a'..'z'] ++ ['0'..'9'] ]
      ++ [ (1, RG.RGOnly c) | c <- ['a'..'z'] ++ ['0'..'9'] ]
      )
  }
dfaVar = rgToDFA 99 rgVar
