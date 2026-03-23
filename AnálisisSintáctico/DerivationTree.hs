-- DerivationTree.hs
-- Implementación de un ADT (Abstract Data Type) para los árboles de derivación generados por una CFG.
-- Martí Pons Garcia
-- Lladó, 2026
-----------------------------------------------------------------------

module DerivationTree
  ( DerTree -- El tipo de dato Arbol.
  , initialTree -- El árbol que se genera tomando solo el símbolo inicial de la gramática CFG
  , yield -- La función que devuelve la cadena final (frontier) de un árbol (digamos que sus hojas)
  , frontier -- En un árbol parcialmente expandido esta función devuelve sus hojas 
  , derChild -- Dada una gramática CFG y un árbol de derivación (pensemos como si fuera una forma sentencial), obtenemos el conjunto de árboles que se derivan (en un paso)
  , derTreeStar -- El cierre reflexivo-transitivo de la relación de derivación de árboles
  --, prettyTree -- Función para generar una visualización atractiva para los árboles de derivación
  ) where

import CFGrammar
import qualified Data.Set as S

-- Arbol de derivación: es, o bien una hoja, de tipo terminal, o bien un nodo de tipo no terminal + una lista de árboles de de. 
-- El nodo es no terminal debido a que es un árbol de derivación de CFG, por lo que solo se pueden formar hojas (más árboles) a partir de un no terminal.
data DerTree nt t = Leaf t | Node nt [DerTree nt t]
  deriving (Eq, Ord)

-- El árbol inicial consist 
initialTree :: CFG nt t -> DerTree nt t
initialTree cfg = Node (initial cfg) []


yield :: DerTree nt t -> [t] -- resultado léxico (solo relevante cuando el árbol está completamente expandido)
yield dt = case dt of
    Leaf a    -> [a]
    Node _ [] -> []
    Node _ ts -> concat (map yield ts)

frontier :: DerTree nt t -> SententialForm nt t -- proceso sintáctico
frontier dt = case dt of
    Leaf a    -> [Terminal a]
    Node a [] -> [Nonterminal a]
    Node _ ts -> concat (map frontier ts)

------------------------------------------------------------------------------------------------------------------------------------
type Derivation nt t = (SententialForm nt t, DerTree nt t)

initialDerivation :: CFG nt t -> Derivation nt t
initialDerivation cfg = ([Nonterminal (initial cfg)] , initialTree cfg)

-- Convierte un símbolo de la gramática en un árbol:
-- Si el símbolo es un terminal, se convierte en una hoja
-- Si el símbolo es no terminal, se convierte en una hoja no terminal: nodo + lista vacía de subárboles
symbolToTree :: Symbol nt t -> DerTree nt t
symbolToTree (Terminal t)     = Leaf t
symbolToTree (Nonterminal nt) = Node nt []

-- Dada una cadena de símbolos, genera hijos para una producción
-- "AbBab" -> [Node A [], Leaf b, Node B [], Leaf a, Leaf b]
rhsToSubtrees :: [Symbol nt t] -> [DerTree nt t]
rhsToSubtrees rhs = map symbolToTree rhs

--derTree1 define la relación de derivación
--deriveChild implementa la búsqueda estructural


-- Recorre el árbol y genera 
--deriveChild :: CFG nt t -> DerTree nt t -> S.Set (DerTree nt t)
--deriveChild cfg (Leaf t) = S.empty
--deriveChild cfg (Node nt []) = S.fromList [Node nt (rhsToSubtrees rhs) | (b, rhs) <- productions cfg, b == nt] -- aplicar producciones de nt → rhs
--deriveChild cfg (Node nt ts) = -- derivar en hijos
--    S.unions [rebuild nt ts i child' | (i, tChild) <- zip [0..] ts , child' <- S.toList (deriveChild cfg tChild)]
-- La lista de nuevos hijos derivados de cada elemento de ts (ts es una lista de árboles)


deriveChild :: (Ord nt, Ord t) => CFG nt t -> DerTree nt t -> S.Set (DerTree nt t)
deriveChild cfg (Leaf t) = S.empty
deriveChild cfg (Node nt []) = S.fromList [Node nt (rhsToSubtrees rhs) | (b, rhs) <- S.toList (productions cfg), b == nt] 
deriveChild cfg (Node nt tsList) = S.fromList [Node nt withChilds | withChilds <- derivedChilds tsList ]
  where
    derivedChilds [] = []
    derivedChilds (t:ts) = [t':ts | t' <- S.toList (deriveChild cfg t)] ++ map (t:) (derivedChilds ts)


derTreeStar :: (Ord nt, Ord t) => CFG nt t -> DerTree nt t -> S.Set (DerTree nt t)
derTreeStar cfg t0 = go (S.singleton t0) (S.singleton t0)
  where
    go seen frontier
      | S.null frontier = seen
      | otherwise =
          let next =
                S.unions [ derChild cfg t | t <- S.toList frontier ]
              new = next `S.difference` seen
          in go (seen `S.union` new) new
