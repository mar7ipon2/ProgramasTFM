-- LogicAST.hs
-- Definición del AST (Abstract Syntax Tree).
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module LogicAST where

import LogicTokens
import Data.List (nub)

-- | El Árbol Sintáctico Abstracto (AST) para Lógica Proposicional.
-- Aquí definiremos la estructura de nuestras fórmulas.
data Formula 
    = And Formula Formula
    | Or Formula Formula
    | Not Formula
    | Implies Formula Formula
    | Iff Formula Formula
    | Var String
    deriving (Show, Eq)


---------------------------------------------------------------------------------------------------------
-- EVALUACIÓN SEMÁNTICA (Tablas de Verdad y Tautologías)
---------------------------------------------------------------------------------------------------------

-- | Una interpretación es una lista de asociación que asigna un valor Booleano a cada nombre de variable.
type Interpretacion = [(String, Bool)]

-- | Busca el valor de una variable en una interpretación.
-- (busca c t) es el valor del primer elemento de la lista de asociación t cuya clave es c.
busca :: Eq c => c -> [(c,v)] -> v
busca c t = head [v | (c',v) <- t, c == c']

-- | Calcula el valor de verdad de una fórmula bajo una interpretación dada.
valor :: Interpretacion -> Formula -> Bool
valor i (Var x)       = busca x i
valor i (Not p)       = not (valor i p)
valor i (And p q)     = valor i p && valor i q
valor i (Or p q)      = valor i p || valor i q
valor i (Implies p q) = valor i p <= valor i q -- p -> q es equivalente a p <= q en Bool
valor i (Iff p q)     = valor i p == valor i q

-- | Extrae la lista de nombres de todas las variables que aparecen en la fórmula.
variables :: Formula -> [String]
variables (Var x)       = [x]
variables (Not p)       = variables p
variables (And p q)     = variables p ++ variables q
variables (Or p q)      = variables p ++ variables q
variables (Implies p q) = variables p ++ variables q
variables (Iff p q)     = variables p ++ variables q

-- | Genera todas las combinaciones posibles de valores de verdad para 'n' variables.
interpretacionesVar :: Int -> [[Bool]]
interpretacionesVar 0 = [[]]
interpretacionesVar n =  map (False:) bss ++ map (True:) bss
    where bss = interpretacionesVar (n-1)

-- | Genera todas las interpretaciones posibles (modelos) para una fórmula dada.
interpretaciones :: Formula -> [Interpretacion]
interpretaciones p = [zip vs i | i <- interpretacionesVar (length vs)]
    where vs = nub (variables p)

-- | Determina si una fórmula es una tautología (siempre es verdadera para cualquier interpretación).
esTautologia :: Formula -> Bool
esTautologia p = and [valor i p | i <- interpretaciones p]
