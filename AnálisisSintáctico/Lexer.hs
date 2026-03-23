-- Lexer.hs
-- Implementación del analizador léxico para lógica proposicional.
-- Instancia el motor genérico 'LexerADT' con los tokens de 'LogicTokens'.
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module Lexer
    ( lexer
    , logicRules
    ) where

import DFA
import LexerADT
import LogicTokens
import qualified Data.Map as M
import qualified Data.Set as S

-- | Punto de entrada principal para tokenizar una cadena de lógica proposicional.
lexer :: String -> Either LexError [Token]
lexer = runLexer logicRules

-- | Lista de reglas léxicas. IMPORTANTE: El orden define la prioridad en caso de empate de longitud.
-- Símbolos y palabras reservadas van antes, variables por detrás.
logicRules :: [LexRule Token (S.Set Int)]
logicRules = 
    [ (dfaAnd, \_ -> TAnd)
    , (dfaOr, \_ -> TOr)
    , (dfaImplies, \_ -> TImplies)
    , (dfaIff, \_ -> TIff)
    , (dfaNot, \_ -> TNot)
    , (dfaLParen, \_ -> TLParen)
    , (dfaRParen, \_ -> TRParen)
    , (dfaVar, TVar)
    ]
