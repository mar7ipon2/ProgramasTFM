-- NFAeEps.hs
-- Implementación de un ADT (Abstract Data Type) para autómatas finitos no deterministas con transiciones épsilon (ε).
-- Esta implementación define el conjunto de transiciones Eps explícitamente en NFAe
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module NFAeEps
  ( NFAe(..), -- El ADT del autómata, que importamos junto a sus componentes
    nfae_nxt, -- La función de transición derivada de la relación de transición
    epsClosure, -- La función que calcula la clausura ε
    epsClosureSet,
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
  , nxt      :: S.Set (a, Char, a)
  , eps      :: S.Set (a, a)
  }

-- Si definimos nxt como relación, nos hará falta esta función:
-- La función de transición derivada de relación de transición  nxt
nfae_nxt :: Ord a => NFAe a -> a -> Char -> S.Set a
nfae_nxt nfae q x = S.fromList [s | (q', x', s) <- S.toList (nxt nfae), q' == q, x' == x]


-- Esta función busca el conjunto de estados al que se llega mediente transicion épsilon desde el estado q, en un salto (no hay transitividad aquí, solo el uso de la relación)
-- Lo que hace es filtrar los pares de eps cuyo primer elemento es q, y recoge el conjunto de los segundos elementos de estos pares.
eps1 :: Ord a => NFAe a -> a -> S.Set a
eps1 nfae q = S.map snd (S.filter (\(q', _) -> q' == q) (eps nfae))

-- La clausura épsilon calcula el conjunto de estados alcanzable desde el estado actual usando solo transiciones ε.
-- Esta implementación usa un método de exploración del grafo de estados (BFS)
epsClosure :: Ord a => NFAe a -> a -> S.Set a
epsClosure nfae q0 = go S.empty q0 -- Empezamos desde el estado q (no hemos visitado ningún estado, aún)
  where
    go visited q
      | S.member q visited = S.empty -- Miramos si ya hemos visitado q, para cortar la recursión
      | otherwise =
          let visited' = S.insert q visited -- ponemos q al conjunto visitado
          in S.insert q
               (S.unions (S.map (go visited') (eps1 nfae q))) 
               -- En la última línea:
               -- 1) Calculamos todos los sucesores immediatos de q (eps1 nfae q).
               -- 2) Aplicamos recursivamente el go sobre los elementos del conjunto anterior.
               -- 3) Unimos los conjuntos que salen de esta recursión.
               -- 4) Acabamos por insertar el elemento inicial, q

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
    qs'' = move nfae qs' x

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

-- Ejemplo: Autómata que acepta una cadena indeterminada de 0 o más a's seguida de 0 o más b's, sin importar el número de estos
--          Por ejemplo, acepta las cadenas "aaab", "aa", "b", "abbbbbbb", "aabb", ε, etc.

nfaeAsBs :: NFAe Int
nfaeAsBs = NFAe
  { states   = S.fromList [0,1]
  , alphabet = S.fromList ['a','b']
  , initial  = 0
  , final    = S.fromList [0,1] -- Ambos pueden ser finales si aceptamos cadenas vacías de cada parte
  , nxt = S.fromList [(0, 'a', 0) , (1, 'b', 1)]
  , eps = S.fromList [(0,1)]
  }
