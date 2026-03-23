-- RGrammar.hs
-- Implementación de un ADT (Abstract Data Type) para las gramáticas regulares.
-- Martí Pons Garcia
-- Lladó, 2026
------------------------------------------------------------------------------------------------------------------------------------

module RGrammar
  (RG(..), -- La gramática Regular: tiene lista de no terminales, lista de terminales, símbolo inicial y lista de producciones
   Symbol(..), -- Los símbolos son un tipo con dos constructores, o bien no terminales, o bien terminales
   SententialForm, -- Trabajamos con cadenas de símbolos, llamados formas sentenciales (no confundir con finales)
   RGProd(..), -- Usamos un nuevo tipo de dato para diferenciar los posibles destinos de una producción
   Production, -- Una producción de una gramática regular es un par (noTerminal, RGProd)
   prettySF, -- Función para visualizar mejor una forma sentencial (permite introducir las funciones show que queramos para no terminales y terminales)
   der1,  -- Función de derivación en un paso. Toma una forma sentencial y devuelve el conjunto con todas las posibles derivaciones que se pueden obtener
   derStar, -- Función de cierre reflexivo–transitivo de la relación de derivación. Devuelve todas las posibles cadenas que se puenden derivar transitivamente.
   derLevelsk, -- Función de derivación en k pasos.
   languageRG, -- Devuelve el lenguaje generado por RG
   languageRGk -- Devuelve las cadenas del lenguaje que se han generada después de k pasos.
   ) where

import qualified Data.Set as S

-- Tipo Symbol (no terminales o terminales)
data Symbol nt t = Nonterminal nt | Terminal t
  deriving (Eq, Ord)

instance (Show nt, Show t) => Show (Symbol nt t) where
  show (Nonterminal nt) = show nt
  show (Terminal t)     = show t

-- Una forma sentencial es una cadena de símbolos terminales y no terminales (vamos a usar forma sentencial en general, sin refirirnos específicamente a la cadena final)
type SententialForm nt t = [Symbol nt t]

-- Renderizado de símbolos: permite usar funciones para visualizar los tipos nt y t
prettySymbol :: (nt -> String) -> (t -> String) -> Symbol nt t -> String
prettySymbol fnt ft (Nonterminal nt) = fnt nt
prettySymbol fnt ft (Terminal t)     = ft t

-- Renderizado de formas sentenciales
prettySF :: (nt -> String) -> (t -> String) -> SententialForm nt t -> String
prettySF fnt ft = concatMap (prettySymbol fnt ft)

-------------------------------------------------------------------------------------------------------------------------------------
-- Producciones de una gramática regular por la derecha
-------------------------------------------------------------------------------------------------------------------------------------
-- Definimos RGProd para especificar las posibles cadenas que puede derivar un producción regular
data RGProd nt t = RGT t nt | RGOnly t | RGEps -- A → aB | a | ε 
  deriving (Eq, Ord)
-- Nota: Para uso teórico escribimos los tres constructores descritos, pero a la práctica sólo necesitamos RGT t nt y RGEps,
--       ya que RGOnly t se deriva de las anteriores.

-- Producción: no terminal origen + producción (RGProd)
type Production nt t = (nt, RGProd nt t)

-- Definición de gramática regular (cómo vemos en RGProd, la definimos por la derecha)
data RG nt t = RG
  { nonTerminals :: S.Set nt -- Conjunto de símbolos no terminales
  , terminals    :: S.Set t  -- Conjunto de símbolos terminales
  , initial      :: nt       -- El símbolo inicial es unn no terminal
  , productions  :: S.Set (Production nt t) -- Conjunto de producciones
  }

---------------------------------------------------------------------------------------------------------------------------------------
-- Derivaciones y lenguajes
---------------------------------------------------------------------------------------------------------------------------------------
-- Las formas sentenciales que se pueden formar en una gramática regular son una cadena de terminales con un posible no terminal al final.
-- Esta función mira si hay algún no terminal (mira al final de la lista) y, en caso afirmativo, devuelve la cadena de terminales + el no terminal
splitsLastNT :: SententialForm nt t -> Maybe ([Symbol nt t], nt)
splitsLastNT sf = 
  case reverse sf of
    (Nonterminal nt:rest) -> Just (reverse rest, nt)
    _                     -> Nothing

-- Dado que una producción regular está definida como un tipo nuevo, definimos una función para traducirla a forma sentencial, y así facilitar el uso de las producciones
prodToSF :: RGProd nt t -> SententialForm nt t
prodToSF p =
  case p of
    RGT t nt  -> [Terminal t, Nonterminal nt] 
    RGOnly t  -> [Terminal t]
    RGEps     -> []    

-- Derivación en un paso (=>^1): 
-- 1) Si es una cadena de terminales no hay derivación (conjunto vacío).
-- 2) Si hay un noterminal al final, hacemos el conjunto a partir de la lista de cadenas que cambian ese no terminal por una producción posible
der1 :: (Ord nt, Ord t) => RG nt t -> SententialForm nt t -> S.Set (SententialForm nt t)
der1 rg sf  = 
  case splitsLastNT sf of
    Nothing -> S.empty
    Just (ts, nt) -> S.fromList [ts ++ prodToSF suf | (nt', suf) <- S.toList (productions rg), nt' == nt]


-- Cierre reflexivo-transitivo de la relación de derivación (=>^*):
-- Se aplica un BFS (Breadth-First Search):
-- 1) Empezamos con los conjuntos: a) pendientes de visitar = formado por solo sf0, b) ya visitados = el vacío.
-- 2.1) Cuando nos hayamos quedo sin pendientes de visitar, la fucnión devolverá el conjunto de visitados
-- 2.2) Mientras tengamos pendientes de visitar:
--      a) Tomaremos la primera forma sentencial (sf) que encontremos en el conjunto de los pendientes
--      b) Quitaremos esta forma sentencial de los pendientes
--      c) Calcularemos el conjunto de formas sentenciales que se pueden derivar en un paso de la forma sentencial tomada sf
--      d) Miraremos cuáles de estas formas sentenciales son nuevas, tomando la diferencia con el conjunto de visitados.
-- 2.2.1) El conjunto de pendientes será la unión del que ya teníamos junto al de nuevas formas sentenciales
-- 2.2.2) El conjunto de visitados será el que ya teníamos junto a la forma sentencial que hemos usado para derivar (sf)
derStar :: (Ord nt, Ord t) => RG nt t -> SententialForm nt t -> S.Set (SententialForm nt t)
derStar rg sf0 = go (S.singleton sf0) S.empty
  where
    go pending visited
      | S.null pending = visited
      | otherwise =
          let sf       = S.findMin pending
              pending' = S.deleteMin pending
              next     = der1 rg sf
              new      = S.difference next visited
          in go (S.union pending' new) (S.insert sf visited)

-- Derivaciones por niveles (=>^k): Esta función funciona como la derStar, pero se separan bien los niveles, para poder tomar hasta el que queramos.
-- 1) Usa la función levels, que calcula la lista [L_0,L_1,..], donde cada L_i es el conjunto de formas sentenciales erivadas en i pasos
-- 2) El nivel inicial (L_0) es el singleton de la forma sentencial sf0 (S.singleton sf0)
-- 3) Se aplica recursivamente 'nextLevels'. Esta función toma de entrada un nivel k (levelk), un conjunto de formas sentenciales, y le aplica der1 a cada uno de sus elementos.
--    Para formar L_(k+1) (levelSk), une todos estos elementos. Finalemente, añadimos el nivel a la lista que calculará los siguientes niveles.
-- 4) La función derLevelsk nos devuelve solo el conjunto derivado de [L_0,L_1,..] hasta L_(k+1): [L_0,L_1,..,L_(k+1)]
derLevelsk :: (Ord nt, Ord t) => RG nt t -> Int -> SententialForm nt t -> S.Set (SententialForm nt t)
derLevelsk rg k sf0 = S.unions (take (k+1) (levels sf0))
  where
    levels sf = S.singleton sf0 : nextLevels (S.singleton sf0)
    nextLevels levelk =
      let levelSk = S.unions (S.map (der1 rg) levelk)
      in levelSk : (nextLevels levelSk)

-- Para obtener el lenguaje hay que asegurarse de tener todas las derivaciones que son solo de símbolos terminales.
-- Para ello hacemos el filtro, usando splitsLastNT, que ya es una función reconocedora de lista de terminales.
languageRG :: (Ord nt, Ord t) => RG nt t -> S.Set (SententialForm nt t)
languageRG rg = S.filter (\sf -> splitsLastNT sf == Nothing) (derStar rg [Nonterminal (initial rg)])

-- Preudolenguaje que aplica la función derLevelsk en lugar de derStar, y así podemos tomar las palabras generadas en k pasos.
languageRGk :: (Ord nt, Ord t) => Int -> RG nt t -> S.Set (SententialForm nt t)
languageRGk k rg = S.filter (\sf -> splitsLastNT sf == Nothing) (derLevelsk rg k [Nonterminal (initial rg)])

---------------------------------------------------------------------------------------------------------------------------------------
-- Ejemplo gramática que genera cadenas que empiezan y acaban en 'a'


rgA :: RG Char Char
rgA = RG
  { nonTerminals = S.fromList ['S','A']
  , terminals    = S.fromList ['a','b']
  , initial      = 'S'
  , productions  = S.fromList
      [ ('S', RGOnly 'a')
      , ('S', RGT 'a' 'A')
      , ('A', RGOnly 'a')
      , ('A', RGT 'b' 'A')
      , ('A', RGT 'a' 'A')
      ]
  }

