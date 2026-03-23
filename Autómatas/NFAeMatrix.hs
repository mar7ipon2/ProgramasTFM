-- NFAeMatrix.hs
-- Implementación de un ADT (Abstract Data Type) para autómatas finitos no deterministas con transiciones épsilon.
-- Esta implementación define el autómata como NFAeEps, pero calcula la clausura épsilon usando el producto de matrices.
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module NFAeMatrix
  ( NFAe(..), -- El ADT del autómata, que importamos junto a sus componentes
    epsClosure, -- La función que calcula la clausura ε
    nfaeNext, -- La función de transición extendida
    nfaeAccepts, -- La función de aceptación de cadenas
    checkNFAe -- Función de verificación que el NFAe es realmente un autómata finito no determinista con transiciones épsilon (ε)
  ) where

-------------------------------------------------------------------------------------------------------------------------------------------
-- DEFINCIÓN DEL AUTÓMATA

import qualified Data.Set as S
import qualified Data.Matrix as M


data NFAe a = NFAe
  { states   :: S.Set a
  , alphabet :: S.Set Char
  , initial  :: a
  , final    :: S.Set a
  , nxt      :: S.Set (a, Char, a)
  , eps      :: S.Set (a, a)
  }

-- Si definimos nxt como relación, nos hará falta esta función:
-- La función de transición derivada de relación de transición  nxt
nfae_nxt :: Ord a => NFAe a -> a -> Char -> S.Set a
nfae_nxt nfae q x = S.fromList [s | (q', x', s) <- S.toList (nxt nfae), q' == q, x' == x]

-----------------------------------------------------------------------------------------------------------------------------------------
-- Implementación de la Clausura mediante matrices
infixl 7 ***
(***) :: Num a => M.Matrix a -> M.Matrix a -> M.Matrix a
(***) = M.multStd

epsElemIndexed :: Ord a => (a, a) -> S.Set a -> (Int, Int)
epsElemIndexed (x, y) states = (S.findIndex x states, S.findIndex y states)

epsIndexed :: Ord a => S.Set (a, a) -> S.Set a -> [(Int, Int)]
epsIndexed eps states = map (`epsElemIndexed` states) (S.toList eps)

-- Matriz derivada del conjunto de transiciones eps
epsMatrix :: Ord a => NFAe a -> M.Matrix Int
epsMatrix nfae = foldl (\acc (i,j) -> M.setElem 1 (i+1,j+1) acc) m0 eps0
  where
    eps0 = epsIndexed (eps nfae) (states nfae)
    n = S.size (states nfae)
    m0 = M.zero n n

-- Recordatorio (función de plegado):
--foldl :: (a -> b -> a) -> a -> [b ] -> a
--foldl f v []     =  v
--foldl f v (x:xs) =  foldl f (f v x ) xs

-- Función que transforma cualquier entero distinto de 0 en 1.
toBoolean :: Int -> Int
toBoolean 0 = 0
toBoolean _ = 1

-- Matriz de clausura: Cálculo de la matriz de estados a las que se llega mediante transiciones ε.
epsClosureMatrix :: M.Matrix Int -> M.Matrix Int
epsClosureMatrix mat = go r0
  where
    n  = M.nrows mat -- Mira la dimensión de la matriz
    idM = M.identity n -- Calcula la identidad de dim n
    r0 = fmap toBoolean (idM + mat) -- El paso inicial es la matriz identidad (las transiciones ε son reflexivas) más la matriz de transiciones épsilon
    step r = fmap toBoolean (r + (mat *** r)) -- Un nuevo paso de transiciones epsilon es "tomar la matriz de transiciones hasta el momento y sumar a la que se obtiene con al multiplicar por la de transiciones)
    go r
      | step r == r = r -- La exploración se detiene cuando los nuevos pasos no generen cambios de estado.
      | otherwise   = go (step r)

-- Encuentra el índice 0-based de un elemento en una lista
indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = go xs 0
  where
    go [] _ = error "Estado no encontrado"
    go (y:ys) i
      | x == y    = i
      | otherwise = go ys (i+1)

epsClosure :: Ord a => NFAe a -> a -> S.Set a
epsClosure nfae q =  S.fromList [ listaEstados !! j | (j, val) <- zip [0..] fila, val == 1]
-- traducimos índice de columna a estado real
  where
    listaEstados = S.toAscList (states nfae)         -- lista ordenada de estados
    cierre = epsClosureMatrix (epsMatrix nfae)       -- matriz de clausura ε
    i = indexOf q listaEstados                        -- índice 0-based de q
    fila = (M.toLists cierre) !! i                    -- fila correspondiente a q


----------------------------------------------------------------------------------------------------------------------------------------------

-- Esta función es muy simple: calcula la clausura, no de un solo estado, sinó de un conjunto de estados
epsClosureSet :: Ord a => NFAe a -> S.Set a -> S.Set a
epsClosureSet nfae qs =  S.unions (S.map (epsClosure nfae) qs)

-- Esta función es la que "consume" un símbolo, dado un conjunto de estados.
-- Básicamente calcula el conjunto de los estados a los que se llega, dado un conjunto qs, consumiendo x
move :: Ord a => NFAe a -> S.Set a -> Char -> S.Set a
move nfae qs x =  S.unions (S.map (\q -> nfae_nxt nfae q x) qs)

-- Esta es la función de transición generalizada, pues toma un conjunto de estados, y calcula el conjunto al que se puede llegar consumiendo una cadena.
-- 1) En el caso inicial, solo calcula la clausura.
-- 2) Hacemos rescursión sobre el conjunto de estados al que llegamos consumiendo x.
--    a) Calculamos la clausura del conjunto qs.
--    b) "Consumimos" x, usando move, y luego aplicamos la clausura una vez más.
nfaeNext_set :: Ord a => NFAe a -> S.Set a -> [Char] -> S.Set a
nfaeNext_set nfae qs []       = epsClosureSet nfae qs
nfaeNext_set nfae qs (x:xs)   = 
  nfaeNext_set nfae qs'' xs
  where 
    qs'  = epsClosureSet nfae qs
    qs'' = epsClosureSet nfae (move nfae qs' x)

-- Tomamos la función de transición general, y la restringuimos a un solo estado (tomando el conjunto singleton)
nfaeNext :: Ord a => NFAe a -> a -> [Char] -> S.Set a
nfaeNext nfae q xs = nfaeNext_set nfae (S.singleton q) xs

-- Versión directa sin funciones auxiliares
nfaeNext1 :: Ord a => NFAe a -> a -> [Char] -> S.Set a
nfaeNext1 nfae q []      = epsClosure nfae q
nfaeNext1 nfae q (x:xs)   = 
    S.unions (S.map (\r -> nfaeNext1 nfae r xs)
                    (S.unions (S.map (epsClosure nfae)
                                     (S.unions (S.map (\p -> nfae_nxt nfae p x) (epsClosure nfae q))))))

-- Función de aceptación
nfaeAccepts nfae input = not (S.null (S.intersection (final nfae) (nfaeNext nfae (initial nfae) input)))

---------------------------------------------------------------------------------------------------------------------------------------------
-- VERIFICACIÓN DEL AUTÓMATA
-- Propiedades para que el ADT sea un NFAe

-- 1) Consistencia de estados:
-- El estado inicial es un elemento del conjunto de estados
checkInitial :: Ord a => NFAe a -> Bool
checkInitial nfae = S.member (initial nfae) (states nfae) 
-- El conjunto de estados finales es subconjunto del conjunto de estados
checkFinal :: Ord a => NFAe a -> Bool
checkFinal nfae = S.isSubsetOf (final nfae) (states nfae)

-- 2) Corrección de la función de transición:
--    comprueba que los elementos (q,x,q') de la relación nxt son de (states, alphabet, states)
checkTransitions :: Ord a => NFAe a -> Bool
checkTransitions nfae = all (\(q, x, q') -> and [S.member q  (states nfae), S.member x  (alphabet nfae), S.member q' (states nfae)]) (S.toList (nxt nfae))

-- 3) Consistencia del conjunto de transiciones ε
checkEpsTransitions :: Ord a => NFAe a -> Bool
checkEpsTransitions nfae = S.isSubsetOf (eps nfae) (S.cartesianProduct (states nfae) (states nfae))

-- Check final
checkNFAe :: Ord a => NFAe a -> Bool
checkNFAe nfae = and [checkInitial nfae, checkFinal nfae, checkTransitions nfae, checkEpsTransitions nfae]

---------------------------------------------------------------------------------------------------------------------------------------------
-- Ejemplo: Autómata que acepta números decimales: Números que empiezan con un signo +/- y sigue una secuencia de dígitos, con la posibilidad
--          de un punto '.' para marcar la coma decimal y una nueva secuencia de dígitos 
--          (también acepta númmeros enteros, que no tienen la coma decimal)

nfaDecimalEps :: NFAe Int
nfaDecimalEps = NFAe
  { states   = S.fromList [0..5]
  , alphabet = S.fromList ('.' : '+' : '-' : ['0'..'9'])
  , initial  = 0
  , final    = S.fromList [4,5]

  , nxt = S.fromList $
        -- Desde 0
        [ (0, '+', 1), (0, '-', 1)]
     ++
        -- Desde 1
        [ (1, d, 1) | d <- ['0'..'9'] ]
     ++
        [ (1, d, 4) | d <- ['0'..'9'] ]
     ++
        [ (1, '.', 2) ]
     ++
        -- Desde 2
        [ (2, d, 3) | d <- ['0'..'9'] ]
     ++
        -- Desde 3
        [ (3, d, 3) | d <- ['0'..'9'] ]
     ++
        -- Desde 4
        [ (4, '.', 3) ]

  , eps = S.fromList[(0,1), (3,5)]
  }


