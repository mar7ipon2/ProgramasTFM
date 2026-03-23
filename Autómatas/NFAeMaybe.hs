-- NFAeEps.hs
-- Implementación de un ADT (Abstract Data Type) para autómatas finitos no deterministas con transiciones épsilon.
-- Esta implementación define el conjunto de transiciones implícitamente en NFAe, usando la mónada Maybe
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module NFAeMaybe
  ( NFAe(..), -- El ADT del autómata, que importamos junto a sus componentes
    epsClosure, -- La función que calcula la clausura ε
    nfaeNext, -- La función de transición extendida
    nfaeAccepts, -- La función de aceptación de cadenas
    checkNFAe -- Función de verificación que el NFAe es realmente un autómata finito no determinista con transiciones épsilon (ε)
  ) where

-------------------------------------------------------------------------------------------------------------------------------------------
-- DEFINCIÓN DEL AUTÓMATA

import qualified Data.Set as S

data NFAe a = NFAe
  { states   :: S.Set a
  , alphabet :: S.Set Char
  , initial  :: a
  , final    :: S.Set a
  , nxt      :: S.Set (a, Maybe Char, a)
  }


-- Si definimos nxt como relación, nos hará falta esta función:
-- La función de transición derivada de relación de transición nxt
nfae_nxt :: Ord a => NFAe a -> a -> Maybe Char -> S.Set a
nfae_nxt nfae q x = S.fromList [s | (q', x', s) <- S.toList (nxt nfae), q' == q, x' == x]


-- Caso eps1 con un NFAe definido con Maybe: Son los valores para los que el símbolo de Maybe es Nothing
eps1 :: Ord a => NFAe a -> a -> S.Set a
eps1 nfae q = S.fromList [q' | (p, Nothing, q') <- S.toList (nxt nfae), p == q]

-- Clausura ε
epsClosure :: Ord a => NFAe a -> a -> S.Set a
epsClosure nfae q0 = go S.empty q0
  where
    go visited q
      | S.member q visited = S.empty
      | otherwise =
          let visited' = S.insert q visited
          in S.insert q
               (S.unions (S.map (go visited') (eps1 nfae q)))

-- Versión epsClosureSet para caso MAYBE
epsClosureSet :: Ord a => NFAe a -> S.Set a -> S.Set a
epsClosureSet nfae qs =  S.unions (S.map (epsClosure nfae) qs)

-- Versión move para caso MAYBE
move :: Ord a => NFAe a -> S.Set a -> Char -> S.Set a
move nfae qs x = S.unions (S.map (\q -> nfae_nxt nfae q (Just x)) qs)

-- Función de transición generalizada: toma un conjunto de estados, y calcula el conjunto al que se puede llegar consumiendo una cadena.
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

-- Función auxiliar que dado un Maybe Char, mira si está en el alfabeto (se contempla la opción de ε)
checkAlphabetOrEpsilon :: Ord a => NFAe a -> Maybe Char -> Bool
checkAlphabetOrEpsilon nfae x = case x of
    Just a -> S.member a (alphabet nfae)
    Nothing -> True

-- 2) Corrección de la función de transición:
--    comprueba que los elementos (q,x,q') de la relación nxt son de (states, alphabet, states)
checkTransitions :: Ord a => NFAe a -> Bool
checkTransitions nfae = all (\(q, x, q') -> and [S.member q  (states nfae), checkAlphabetOrEpsilon nfae x, S.member q' (states nfae)]) (S.toList (nxt nfae))


-- Check final
checkNFAe :: Ord a => NFAe a -> Bool
checkNFAe nfae = and [checkInitial nfae, checkFinal nfae, checkTransitions nfae]

-------------------------------------------------------------------------------------------------------------------------------------------
-- Ejemplo: Autómata que acepta números decimales: Números que empiezan con un signo +/- y sigue una secuencia de dígitos, con la posibilidad
--          de un punto '.' para marcar la coma decimal y una nueva secuencia de dígitos 
--          (también acepta númmeros enteros, que no tienen la coma decimal)
nfaDecimalMaybe :: NFAe Int
nfaDecimalMaybe = NFAe
  { states   = S.fromList [0..5]
  , alphabet = S.fromList ('.' : '+' : '-' : ['0'..'9'])
  , initial  = 0
  , final    = S.fromList [4,5]
  , nxt = S.fromList $
        -- Transiciones normales
        [ (0, Just '+', 1), (0, Just '-', 1)]
     ++
        [ (1, Just d, 1) | d <- ['0'..'9'] ]
     ++
        [ (1, Just d, 4) | d <- ['0'..'9'] ]
     ++
        [ (1, Just '.', 2) ]
     ++
        [ (2, Just d, 3) | d <- ['0'..'9'] ]
     ++
        [ (3, Just d, 3) | d <- ['0'..'9'] ]
     ++
        [ (4, Just '.', 3) ]
     ++
        -- ε-transiciones
        [ (0, Nothing, 1), (3, Nothing, 5)]
  }

