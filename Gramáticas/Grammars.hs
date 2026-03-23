-- Definición de la estructura general de una gramática, con sus diferentes versiones según la jerarquía de Chomsky
-- Definición de GIC y sus derivaciones
-- Definición del NFA
import qualified Data.Set as S

-- Definición de gramática (sin especificar)
data Symbol nt t = Nonterminal nt | Terminal t
data Grammar nt t prod = Grammar
  { nonTerminals :: [nt]
  , terminals :: [t]
  , initial  :: nt
  , productions :: [prod]
  }
---------------------------------------------------------------------------------------------------------------------------------
-- DEFINICIONES de las distintsa GRAMÁTICAS
-- Definición de gramática general (tipo 0)
type GeneralProd nt t = ([Symbol nt t], [Symbol nt t])
type GeneralGrammar nt t = Grammar nt t (GeneralProd nt t)

-- Definición de gramática sensible al contexto (tipo 1)
-- ¡Importante! la gramática sensible al contexto tiene una restricción semántica: las componentes de la producción están relacionadas (α A β → α γ β)
-- Como Haskell no usa tipos dependientes, se debe dar una definicón + restricción (más adelante lo implementaré en forma de module, dónde sí imponga la propiedad)
data CSGProd nt t = CSGProd
  { leftContext  :: [Symbol nt t] -- α 
  , nonterminal  :: nt            -- A
  , rightContext :: [Symbol nt t] -- β
  , replacement  :: [Symbol nt t] -- γ
  }
type CSG nt t = Grammar nt t (CSGProd nt t)

-- Definción de gramática independiente del contexto (tipo 2)
type CFGProd nt t = (nt , [Symbol nt t])
type CFG nt t        = Grammar nt t (CFGProd nt t)

-- Definición de gramática regular (tipo 3)
data RGProd nt t = RGT t nt | RGOnly t | RGEps
type RG nt t = Grammar nt t (nt, RGProd nt t)

---------------------------------------------------------------------------------------------------------------------------------------------
-- Funciones para verificar el tipo del tipo de dato Symbol (se mira el constructor que se usa)
isNonterminal :: Symbol nt t -> Bool
isNonterminal (Nonterminal _) = True
isNonterminal _               = False

isTerminal :: Symbol nt t -> Bool
isTerminal = not.isNonterminal

der1 :: Ord nt => CFG nt t -> [Symbol nt t] -> S.Set [Symbol nt t]
der1 cfg xs = go xs [] S.empty
  where
    go (i:input) read derived
     | input == []       = derived
     | otherwise         =
       | isTerminal i    = 
        let read'        = i:read
        in go input read derived
       | otherwise       =
         


------------------------------------------------------------------------------------------------------------
-- Definir constructores cortos
t :: t -> Symbol nt t
t = Terminal

nt :: nt -> Symbol nt t
nt = Nonterminal



simbolo1 :: Symbol Int Char
simbolo1 = nt 2

simbolo2 :: Symbol Int Char
simbolo2 = t '3'

cadenaSimbolos :: [Symbol Int Char]
cadenaSimbolos = [t 'a', nt 2 , nt 3, t 'b', nt 0]
