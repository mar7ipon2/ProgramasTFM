-- Main.hs
-- Módulo principal que unifica el Analizador Léxico y el Analizador Sintáctico
-- para evaluar cadenas de Lógica Proposicional y obtener su AST.
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module Main where

import Lexer
import LexerADT (LexError(..))
import Parser
import LogicAST

-- Función orquestadora: toma un String, aplica el lexer, y si tiene éxito, le pasa los tokens al parser.
-- Devuelve el árbol AST final (Formula) o un mensaje de error detallado.
evaluateLogic :: String -> Either String Formula
evaluateLogic input = 
    case lexer (filter (`notElem` " \t\n") input) of
        Left (LexError off char) -> Left $ "Error léxico: carácter inesperado '" ++ [char] ++ "'"
        Right tokens             -> parseLogic tokens

-- Función semántica: toma un String, aplica la evaluación con evaluateLogic.
-- Devuelve la fórmula (el AST final) junto a su evaluación tautológica.
evaluateLogicTautologies :: String -> Maybe (Formula , Bool)
evaluateLogicTautologies input = case evaluateLogic input of
    Left err -> Nothing
    Right formula -> Just (formula, esTautologia formula)


tests = [ "p | ~p"                          -- Tercio excluso (Tautología)
                , "p & ~p"                  -- Contradicción (No Tautología)
                , "p -> q"                  -- Contingencia (No Tautología)
                , "(p & q) -> p"            -- Simplificación (Tautología)
                , "~(p & q) <-> (~p | ~q)"  -- Ley de De Morgan (Tautología)
                ]

-- Probar en ghci:
-- miLista = map (\n -> evaluateTautologies (tests !! n)) [0..4]
-- mapM_ print miLists
