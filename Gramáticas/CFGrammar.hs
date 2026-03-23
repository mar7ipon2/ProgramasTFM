-- CFGrammar.hs
-- Implementación de un ADT (Abstract Data Type) para las gramáticas independientes del contexto.
-- Martí Pons Garcia
-- Lladó, 2026
------------------------------------------------------------------------------------------------------------------------------------

module CFGrammar
  (CFG(..), -- La gramática CFG: tiene lista de no terminales, lista d eterminales, símbolo inicial y lista de producciones
   Symbol(..), -- Los símbolos son un tipo con dos constructores, o bien no terminales, o bien terminales
   SententialForm, -- Trabajamos con cadenas de símbolos, llamados formas sentenciales (no confundir con finales)
   prettySF, -- Función para visualizar mejor una forma sentencial (permite introducir las funciones show que queramos para no terminales y terminales)
   der1,  -- Función de derivación en un paso. Toma una forma sentencial y devuelve el conjunto con todas las posibles derivaciones que se pueden obtener
   derStar, -- Función de cierre reflexivo–transitivo de la relación de derivación. Devuelve todas las posibles cadenas que se puenden derivar transitivamente.
   languageCFG -- Devuelve el lenguaje generado por CFG
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

-- Renderizar = convertir una estructura abstracta en una representación textual legible.
prettySymbol :: (nt -> String) -> (t -> String) -> Symbol nt t -> String
prettySymbol fnt ft (Nonterminal nt) = fnt nt
prettySymbol fnt ft (Terminal t)     = ft t
-- Esta función dice: Dime cómo convertir un no terminal a texto, dime cómo convertir un terminal a texto,
--                    y yo te convierto un símbolo (terminal o no terminal) texto

-- Definimos prettySF: una función que nos permite visualizar una forma sentencial introduciendo las funciones show, para los tipos de síbolos, que queramos.
-- Por ejemplo, si nt y t son ambons de tipo Char, lo aplicamos así: prettySF (:[]) (:[]) [Nonterminal 'S', Terminal 'a'] == "Sa"ç
prettySF :: (nt -> String) -> (t -> String) -> SententialForm nt t -> String
prettySF fnt ft = concatMap (prettySymbol fnt ft)

-- Ejmeplo de prettySF dedicada solo a cuando los no terminales y terminales son de tipo Char
prettySF' :: SententialForm Char Char -> String
prettySF' = map f
  where
    f (Nonterminal nt) = nt
    f (Terminal t)     = t

-- Hay la opción de visualización con un 
newtype SF nt t = SF { unSF :: [Symbol nt t] }

instance (Show nt, Show t) => Show (SF nt t) where
    show (SF []) = ""
    show (SF (x:xs)) = show x ++ show (SF xs)

-------------------------------------------------------------------------------------------------------------------------------------
-- Producciones de una gramática independiente del contexto
-------------------------------------------------------------------------------------------------------------------------------------

-- Definimos las producciones de una CFG
type Prod nt t = (nt , [Symbol nt t])

-- Definimos una CFG como un tipo de dato con sus componentes (símbolos (no terminal y terminales), símbolo inicial (no terminal) y producciones)
data CFG nt t = CFG
  { nonTerminals :: S.Set nt
  , terminals :: S.Set t
  , initial  :: nt
  , productions :: S.Set (Prod nt t)
  }

---------------------------------------------------------------------------------------------------------------------------------------
-- Derivaciones y lenguajes
---------------------------------------------------------------------------------------------------------------------------------------

-- Esta función devuelve una lista de 0 y 1 según los símbolos de la forma sentencial son no terminales o terminales
nonTerminalSearch :: Ord nt => CFG nt t -> SententialForm nt t -> [Int]
nonTerminalSearch cfg []     = []
nonTerminalSearch cfg (s:sf) =
    case s of 
       Nonterminal _ -> 1:(nonTerminalSearch cfg sf)
       Terminal    _ -> 0:(nonTerminalSearch cfg sf)

-- Función que separa una forma sentencial en tuplas, según encuentra símbolos no terminales
splitsNT :: SententialForm nt t -> [([Symbol nt t], nt, [Symbol nt t])]
splitsNT sf = go [] sf
  where
    go _        [] = []
    go pre (s:suf) =
        case s of
            Nonterminal a -> (reverse pre, a, suf) : go (s:pre) suf
            Terminal _    -> go (s:pre) suf

applyProduction :: Ord nt => Prod nt t -> ([Symbol nt t], nt, [Symbol nt t]) -> ([Symbol nt t], [Symbol nt t], [Symbol nt t])
applyProduction (intput, output) (as, a, bs) = (as, output, bs)

-- Definimos der1 (derivación en un paso)
der1 :: (Ord nt, Ord t) => CFG nt t -> SententialForm nt t -> S.Set (SententialForm nt t)
der1 cfg sf = S.fromList [pre ++ rhs ++ suf | (pre, a, suf) <- splitsNT sf, (b, rhs) <- S.toList (productions cfg), a == b]

-- Definimos derStar (derivación en varios pasos). Cuando aplicamos derStar al símbolo inicial esta nos genera el lenguaje entero 
-- ¡por lo que muy probablemente tiene output infinito!
-- La función calcula el cierre reflexivo–transitivo de la relación de derivación
derStar :: (Ord nt, Ord t) => CFG nt t -> SententialForm nt t -> S.Set (SententialForm nt t)
derStar cfg sf0 = go (S.singleton sf0) S.empty
  where
    go pending visited
      | S.null pending = visited --Si no hay nada por explorar, devuelve lo visitado (Es cuando termina el caso finito)
      | otherwise =
          let sf        = S.findMin pending         -- Selecciona una forma sentencial cualquiera de pending
              pending'  = S.deleteMin pending       -- La elimina de pending para no reexplorarla inmediatamente.
              next      = der1 cfg sf               -- Calcula todas las derivaciones en un paso desde sf.
              new       = S.difference next visited -- Filtra solo las formas realmente nuevas (no visitadas antes).
          in go (S.union pending' new)
                (S.insert sf visited)

derLevelsk :: (Ord nt, Ord t) => CFG nt t -> Int -> SententialForm nt t -> S.Set (SententialForm nt t)
derLevelsk cfg k sf0 = S.unions (take (k+1) (levels sf0))
  where
    levels sf = S.singleton sf0 : nextLevels (S.singleton sf0)
    nextLevels levelk =
      let levelSk = S.unions (S.map (der1 cfg) levelk)
      in levelSk : (nextLevels levelSk)


-- Para obtener el lenguaje que genera una gramática hay que tomar solo las formas sentenciales a las que se llega.
-- Por ello definimos un filtro para distinguir cuando una forma sentencial está formada solo por símbolos terminales
isTerminalSF :: SententialForm nt t -> Bool
isTerminalSF = all isTerminal
  where
    isTerminal (Terminal _) = True
    isTerminal _            = False

-- Ahora el lenguaje generado por una CFG se encuentra tomando las cadenas que se derivan del símbolo inicial con derStar, y luego filtrando las que són sólo de terminales
languageCFG :: (Ord nt, Ord t) => CFG nt t -> S.Set (SententialForm nt t)
languageCFG cfg = S.filter isTerminalSF (derStar cfg [Nonterminal (initial cfg)])

languageCFGlevels :: (Ord nt, Ord t) => CFG nt t -> Int -> S.Set (SententialForm nt t)
languageCFGlevels cfg k = S.filter isTerminalSF (derLevelsk cfg k [Nonterminal (initial cfg)])

---------------------------------------------------------------------------------------------------------------------------------------
-- Ejemplo de gramática pequeña
---------------------------------------------------------------------------------------------------------------------------------------

-- símbolos
s :: Symbol Char Char
s = Nonterminal 'S'

nT :: Char
nT = 'N'

a :: Symbol Char Char
a = Terminal 'a'

b :: Symbol Char Char
b = Terminal 'b'

-- gramática
cfgSimple :: CFG Char Char
cfgSimple = CFG
  { nonTerminals = S.fromList ['S','A']
  , terminals    = S.fromList ['a','b']
  , initial      = 'S'
  , productions  = S.fromList
      [ ('S', [Terminal 'a', Nonterminal 'A'])
      , ('A', [Terminal 'b'])
      , ('S', [Terminal 'b'])
      ]
  }

cfgwwR :: CFG Char Char
cfgwwR = CFG
  { nonTerminals = S.singleton 'S'
  , terminals    = S.fromList ['a','b']
  , initial      = 'S'
  , productions  = S.fromList
      [ ('S', [Terminal 'a', Nonterminal 'S', Terminal 'a'])
      , ('S', [Terminal 'b', Nonterminal 'S', Terminal 'b'])
      , ('S', [])
      ]
  }

-- pretty' = prettySF (:[]) (:[])
-- prettySet = \x -> map (pretty') (S.toList x)
-- set = S.fromList ([[Terminal 'a',Terminal 'b'], [Nonterminal 'S']]) 
-- prettySet set
-- lang2 = languageCFGlevels cfgwwR 2
-- prettySet lang2 //  == ["","aa","bb"]  -- Lenguaje producido con 2 niveles ("" se produce con 1 nivel, "aa" y "bb" con 2 niveles)
-- nodes2 = derLevels cfgwwR 2 [Nonterminal 'S']
-- prettySet nodes2 // == ["","S","aSa","aa","aaSaa","abSba","bSb","baSab","bb","bbSbb"] -- Las cadenas que podemos derivar con 2 iteraciones (niveles)
-- lang3 = languageCFGlevels cfgwwR 3
-- ["","aa","aaaa","abba","baab","bb","bbbb"]