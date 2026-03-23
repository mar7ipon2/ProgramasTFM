-- CFGtoPDA.hs
-- En este documento se define una función que transforma un ADT CFG (Context Free Grammar)
-- a un PDA (Push-Down Automata)
-- Martí Pons Garcia
-- Lladó, 2026
-- ---------------------------------------------------------------------
import qualified Data.Set as S
import qualified PDA
import qualified CFGrammar as CFG

------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- GRAMÁTICA INDEPENDIENTE DEL CONTEXTO -> PDA: Paso de Gramática independiente del contexto a autómata de pila
------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Función auxiliar
symbolToChar :: CFG.Symbol Char Char -> Char
symbolToChar (CFG.Nonterminal nt) = nt
symbolToChar (CFG.Terminal t)     = t


-- Construcción del alfabeto de la pila (unión de no terminales y terminales) (VERSIÓN GENERAL)
stackAlphabetCFG :: (Ord nt, Ord t) => CFG.CFG nt t -> S.Set (CFG.Symbol nt t)
stackAlphabetCFG cfg = S.union nts ts
  where
    nts = S.map CFG.Nonterminal (CFG.nonTerminals cfg) -- convierte Set nt en Set (Symbol nt t)
    ts  = S.map CFG.Terminal    (CFG.terminals cfg)
-- Construcción del alfabeto de la pila (unión de no terminales y terminales) (VERSIÓN Char para el PDA)
stackAlphabetCFGChar :: CFG.CFG Char Char -> S.Set Char
stackAlphabetCFGChar cfg =
  S.union (CFG.nonTerminals cfg) (CFG.terminals cfg)


-- Transiciones epsilon a partir de las producciones (VERSIÓN GENERAL)
productionTransitions :: (Ord nt, Ord t) => CFG.CFG nt t -> S.Set (((), Maybe t, CFG.Symbol nt t),((), [CFG.Symbol nt t]))
productionTransitions cfg = S.map build (CFG.productions cfg)
  where
    build (lhs, rhs) = ( ((), Nothing, CFG.Nonterminal lhs), ((), rhs))
-- Recorremos todas las producciones "productions cfg :: Set (nt, [Symbol nt t])"
-- Y para cada producción (lhs, rhs) construimos la terna (((), Nothing, Nonterminal lhs), ((), rhs))
-- Que significa que ((q, ε, A) , (a, α)) (o en forma de función, pda_nxt (q, ε, A) = {(a, α), ..})

-- Transiciones epsilon a partir de las producciones (VERSIÓN Char para el PDA)
productionTransitionsChar :: CFG.CFG Char Char-> S.Set (((), Maybe Char, Char), ((), String))
productionTransitionsChar cfg = S.map build (CFG.productions cfg)
  where
    build (lhs, rhs) = ( ((), Nothing, lhs), ((), map symbolToChar rhs))


-- Transiciones de consumo de terminales
terminalTransitions :: CFG.CFG Char Char -> S.Set (((), Maybe Char, Char), ((), String))
terminalTransitions cfg = S.map build (CFG.terminals cfg)
  where
    build a = ( ((), Just a, a), ((), "") )
-- Para cada terminal a "(((), Just a, a), ((), ""))" definimos una transición que consume el valor a de la pila

-- Usamos productionTransitions y terminalTransitions para definir la relación de transición nxt
buildRelation :: CFG.CFG Char Char-> S.Set (((), Maybe Char, Char), ((), String))
buildRelation cfg = S.union (productionTransitionsChar cfg) (terminalTransitions cfg)


-- Función final que convierte una CFG en un PDA
cfgToPDA :: CFG.CFG Char Char -> PDA.PDA ()
cfgToPDA cfg = PDA.PDA
    { PDA.states = S.singleton ()
    , PDA.alphabet = CFG.terminals cfg
    , PDA.stack_alphabet = stackAlphabetCFGChar cfg
    , PDA.initial_state = ()
    , PDA.initial_stack = CFG.initial cfg
    , PDA.final = S.empty  -- aceptación por pila vacía
    , PDA.nxt_relation = buildRelation cfg
    }
