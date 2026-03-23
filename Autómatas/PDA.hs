-- PDA.hs
-- Implementación de un ADT (Abstract Data Type) para autómatas de pila.
-- En este documento vamos a definir los autómatas de pila como no deterministas, 
-- y añadiremos una función que verifica el caso particular de autómata determinista. Entonces concebimos los DPDA como caso particula de los NPDA.
-- Las transiciones ε se modelan mediente el tipo Maybe.
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module PDA
  ( PDA(..), -- El ADT del autómata, que importamos junto a sus componentes
    Config, -- Cómo se encuentra el autómata en un punto dado. Es una tupla (estado, símbolos a leer, símbolos de la pila)
    step, -- Un solo paso de transición del PDA. Aplicación de δ (nxt) una sola vez
    stepStar, -- Es la clausura reflexivo-transitiva de step (todos los estados alcanzables en cualquier número de pasos).
    accepts_by_final_state, -- La función de aceptación de cadenas por estado final
    accepts_by_empty_stack, -- La función de aceptación de cadenas por pila vacía
    checkPDA, -- Función de verificación que el PDA es realmente un autómata de pila.
    checkDeterministic -- Suponiendo que el PDA es un autómata de pila, verifica si es de hecho determinista
  ) where

-------------------------------------------------------------------------------------------------------------------------------------------
-- DEFINCIÓN DEL AUTÓMATA

import qualified Data.Set as S

-- Definir el tipo de la relación nos ahorrará escritura
type PDARel a = S.Set ((a, Maybe Char, Char), (a, String))

-- Definición del PDA (como non-deterministic Push Down Automaton)
data PDA a = PDA
  {  states         :: S.Set a
  ,  alphabet       :: S.Set Char
  ,  stack_alphabet :: S.Set Char
  ,  nxt_relation   :: PDARel a--(a, Maybe Char, Char) -> S.Set (a, String)
  ,  initial_state  :: a
  ,  initial_stack  :: Char
  ,  final          :: S.Set a
  }
-- Si definimos nxt como relación, nos hará falta esta función:
-- La función de transición derivada de relación de transición nxt
pda_nxt :: Ord a => PDARel a -> (a, Maybe Char, Char) -> S.Set (a, String)
pda_nxt rel key = S.fromList [res | (k, res) <- S.toList rel, k == key]

nxt pda = pda_nxt (nxt_relation pda)

-- Convención sobre la pila:
-- La pila se representa como un String donde la cabeza de la lista es la cima de la pila.
--
-- Cuando una transición nxt(p, a, z) contiene (q, ys), el símbolo z se desapila y la cadena ys se inserta en su lugar
-- mediante la operación (ys ++ zs), donde zs es el resto de la pila.
--
-- Esto implica que el primer símbolo de ys pasa a ser la nueva cima.


-- Configuración del autómata (estado, símbolos a leer, símbolos de la pila)
type Config a = (a, String, String)

-- Aplicación de la función de transición en un paso
step :: Ord a => PDA a -> Config a -> S.Set (Config a)
step pda (p, a:as, z:zs) = S.fromList ( [(q, as, ys ++ zs) | (q, ys) <- S.toList (nxt pda (p,Just a,z))]
  ++ [(q, a:as, ys ++ zs) | (q, ys) <- S.toList (nxt pda (p,Nothing,z))])  -- caso entrada no vacía
step pda (p, [], z:zs) = S.fromList [(q, [], ys ++ zs) | (q, ys) <- S.toList (nxt pda (p, Nothing, z))] -- caso entrada vacía
step _ _ = S.empty


-- Aplicación de la función de transición extendida (cierre transitivo-reflexivo)
stepStar :: Ord a => PDA a -> Config a -> S.Set (Config a)
stepStar pda c0 = go (S.singleton c0) S.empty
  where
    go pending visited
     | S.null pending = visited
     | otherwise      =
      let c             = S.findMin pending      -- escoger una configuración c de pending 
          pending'      = S.deleteMin pending    -- eliminar la configuración c de pending, para evitar recursión infinita
          next          = step pda c             -- obtenemos el conjunto next de configuraciones nuevas que se obtienen de aplicar step a la conf c
          new           = S.difference next visited -- new es el conjunto de las configuraciones nuevas: las de next que no estan en visited
          in go (S.union pending' new) (S.insert c visited)
          -- Pending se transforma en el conjunto sin la configuración evaluada (pending') y el de nuevas configuraciones no visitadas (new)
          -- Visited solo se llena con la configuración usada, c.

-- Funciones de aceptación
accepts_by_final_state pda input = not. S.null $ S.filter (\(q,w,_) -> w == "" && S.member q (final pda)) (stepStar pda (initial_state pda, input, [initial_stack pda]))
accepts_by_empty_stack pda input = not. S.null $ S.filter (\(_,w,z) -> w == "" && z == "") (stepStar pda (initial_state pda, input, [initial_stack pda]))

---------------------------------------------------------------------------------------------------------------------------------------------
-- VERIFICACIÓN DEL AUTÓMATA
-- Propiedades para que el ADT sea un PDA

-- 1) Consistencia de estados:
checkInitialState :: Ord a => PDA a -> Bool
checkInitialState pda = S.member (initial_state pda) (states pda) 

checkFinalStates :: Ord a => PDA a -> Bool
checkFinalStates pda = S.isSubsetOf (final pda) (states pda)

-- 2) Símbolo de pila inicial
checkInitialStack :: Ord a => PDA a -> Bool
checkInitialStack pda = S.member (initial_stack pda) (stack_alphabet pda)

-- 3) Transiciones bien formadas:
-- Verifica un par de salida (q, ys) : q es un estado, y ys es una cadena del alfabeto de la pila
checkTransitionResult :: Ord a => PDA a -> (a, String) -> Bool
checkTransitionResult pda (q', ys) = S.member q' (states pda) && all (`S.member` stack_alphabet pda) ys

checkTransitions :: Ord a => PDA a -> Bool
checkTransitions pda = all checkOne (S.toList (nxt_relation pda)) -- verifica cada uno de los elementos de la relación de transición
  where
    checkOne ((q, mx, z), res) = 
      S.member q (states pda) -- es un estado
      && inputOK mx -- mx es del alfabeto o es ε
      && S.member z (stack_alphabet pda) -- z es del alfabeto de la pila
      && checkTransitionResult pda res -- Verifica la salida mediante checkTransitionResult
    inputOK Nothing  = True
    inputOK (Just a) = S.member a (alphabet pda)

-- Check final
checkPDA :: Ord a => PDA a -> Bool
checkPDA pda = and [ checkInitialState pda, checkFinalStates pda, checkInitialStack pda, checkTransitions pda]

---------------------------------------------------------------------------------------------------------------------------------------------
-- COMPROBACIÓN DE DETERMINISMO

-- 1) Para cada triple (q,a,z), hay a lo sumo una transición
-- Para cada clave k (un triple (q,a,z)), toma su conjunto de resultados mediante pda_nxt.
-- Comprueba que tenga como máximo un resultado.
-- Esto asegura que el PDA no tiene ambigüedad sobre qué transición tomar para un (estado, símbolo de entrada, símbolo de pila) dado.
checkSingleValued :: Ord a => PDA a -> Bool
checkSingleValued pda = all (\k -> S.size (pda_nxt (nxt_relation pda) k) <= 1) keys
  where
    keys = S.map fst (nxt_relation pda)

-- 2) Si existe transición con símbolo de entrada a, no puede existir también transición ε para el mismo (q,z).
checkNoEpsilonConflict :: Ord a => PDA a -> Bool
checkNoEpsilonConflict pda = all noConflict pairs
  where
    relList = S.toList (nxt_relation pda) -- convierte la relación en lista para procesar
    pairs = S.toList $ S.fromList [ (q,z) | ((q,_,z),_) <- relList] -- extrae todas las combinaciones (estado, tope_pila) únicas
    noConflict (q,z) = not (hasSymbol q z && hasEpsilon q z) -- no debe haber conflicto
    hasSymbol q z = any (\((q',mx,z'),_) -> q'==q && z'==z && case mx of Just _ -> True; _ -> False) relList
    -- revisa si hay alguna transición que consuma un símbolo de entrada para ese (q,z)
    hasEpsilon q z = any (\((q',mx,z'),_) -> q'==q && z'==z && mx==Nothing) relList
    -- revisa si hay alguna transición ε para ese (q,z)

-- Check global determinista
checkDeterministic :: Ord a => PDA a -> Bool
checkDeterministic pda = and [checkPDA pda, checkSingleValued pda, checkNoEpsilonConflict pda]

------------------------------------------------------------------------------------------------------------------------------------------------
-- EJEMPLOS:

-- Ejemplo I: Autómata que acepta cadenas ww^R (es NPDA)
-- Aceptación por estado final
wwRpda1 :: PDA Int
wwRpda1 = PDA
  { states = S.fromList [0,1,2]
  , alphabet = S.fromList ['0','1']
  , stack_alphabet = S.fromList ['0','1','Z']
  , initial_state = 0
  , initial_stack = 'Z'
  , final = S.singleton 2
  , nxt_relation = S.fromList
      [ ((0, Just '0', 'Z'), (0, "0Z"))
      , ((0, Just '1', 'Z'), (0, "1Z"))
      , ((0, Just '0', '0'), (0, "00"))
      , ((0, Just '0', '1'), (0, "01"))
      , ((0, Just '1', '0'), (0, "10"))
      , ((0, Just '1', '1'), (0, "11"))
      , ((0, Nothing, 'Z'), (1, "Z"))
      , ((0, Nothing, '0'), (1, "0"))
      , ((0, Nothing, '1'), (1, "1"))
      , ((1, Just '0', '0'), (1, ""))
      , ((1, Just '1', '1'), (1, ""))
      , ((1, Nothing, 'Z'), (2, "Z"))
      ]
  }

-- Aceptación por pila vacía
wwRpda2 :: PDA Int
wwRpda2 = PDA
  { states = S.fromList [0,1,2]
  , alphabet = S.fromList ['0','1']
  , stack_alphabet = S.fromList ['0','1','Z']
  , initial_state = 0
  , initial_stack = 'Z'
  , final = S.empty
  , nxt_relation = S.fromList
      [ ((0, Just '0', 'Z'), (0, "0Z"))
      , ((0, Just '1', 'Z'), (0, "1Z"))
      , ((0, Just '0', '0'), (0, "00"))
      , ((0, Just '0', '1'), (0, "01"))
      , ((0, Just '1', '0'), (0, "10"))
      , ((0, Just '1', '1'), (0, "11"))
      , ((0, Nothing, 'Z'), (1, "Z"))
      , ((0, Nothing, '0'), (1, "0"))
      , ((0, Nothing, '1'), (1, "1"))
      , ((1, Just '0', '0'), (1, ""))
      , ((1, Just '1', '1'), (1, ""))
      , ((1, Nothing, 'Z'), (2, "")) -- aquí la pila se vacía
      ]
  }

-- Ejemplo II: Autómata que acepta cadenas wcw^R, con un símbolo central 'c' (es DPDA)
-- Aceptación por estado final
wcwRpda1 :: PDA Int
wcwRpda1 = PDA
  { states = S.fromList [0,1,2]
  , alphabet = S.fromList ['0','1','c']
  , stack_alphabet = S.fromList ['0','1','Z']
  , initial_state = 0
  , initial_stack = 'Z'
  , final = S.singleton 2
  , nxt_relation = S.fromList
      [ ((0, Just '0', 'Z'), (0, "0Z"))
      , ((0, Just '1', 'Z'), (0, "1Z"))
      , ((0, Just '0', '0'), (0, "00"))
      , ((0, Just '0', '1'), (0, "01"))
      , ((0, Just '1', '0'), (0, "10"))
      , ((0, Just '1', '1'), (0, "11"))
      , ((0, Just 'c', 'Z'), (1, "Z"))
      , ((0, Just 'c', '0'), (1, "0"))
      , ((0, Just 'c', '1'), (1, "1"))
      , ((1, Just '0', '0'), (1, ""))
      , ((1, Just '1', '1'), (1, ""))
      , ((1, Nothing, 'Z'), (2, "Z"))
      ]
  }

-- Aceptación por pila vacía
wcwRpda2 :: PDA Int
wcwRpda2 = PDA
  { states = S.fromList [0,1,2]
  , alphabet = S.fromList ['0','1','c']
  , stack_alphabet = S.fromList ['0','1','Z']
  , initial_state = 0
  , initial_stack = 'Z'
  , final = S.empty
  , nxt_relation = S.fromList
      [ ((0, Just '0', 'Z'), (0, "0Z"))
      , ((0, Just '1', 'Z'), (0, "1Z"))
      , ((0, Just '0', '0'), (0, "00"))
      , ((0, Just '0', '1'), (0, "01"))
      , ((0, Just '1', '0'), (0, "10"))
      , ((0, Just '1', '1'), (0, "11"))
      , ((0, Just 'c', 'Z'), (1, "Z"))
      , ((0, Just 'c', '0'), (1, "0"))
      , ((0, Just 'c', '1'), (1, "1"))
      , ((1, Just '0', '0'), (1, ""))
      , ((1, Just '1', '1'), (1, ""))
      , ((1, Nothing, 'Z'), (2, "")) -- pila vacía
      ]
  }

-- Autómata de paréntesis balanceados
-- Aceptación por estado final
balancedParensPDA :: PDA Int
balancedParensPDA = PDA
  { states = S.fromList [0,1]
  , alphabet = S.fromList ['[', ']']
  , stack_alphabet = S.fromList ['[', 'Z']
  , initial_state = 0
  , initial_stack = 'Z'
  , final = S.singleton 1
  , nxt_relation = S.fromList
      [ ((0, Just '[', 'Z'), (0, "[Z"))
      , ((0, Just '[', '['), (0, "[["))
      , ((0, Just ']', '['), (0, ""))
      , ((0, Nothing, 'Z'), (1, "Z")) -- He usado "Z" equivalente a bot, como en el dibujo del grafo. Si querías épsilon (vacío) como en las fórmulas, pon ""
      ]
  }