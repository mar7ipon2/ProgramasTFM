-- Parser.hs
-- Implementación del Analizador Sintáctico Descendente para Lógica Proposicional
-- Basado en la CFG de LogicGrammar.hs
-- Martí Pons Garcia
-- Lladó, 2026
-- ----------------------------------------------------------------------------------------------------------------------------------------

module Parser where

import ParserADT
import LogicTokens
import LogicAST

---------------------------------------------------------------------------------------------------------
-- PARSER PRINCIPAL
---------------------------------------------------------------------------------------------------------

parseRunning :: Parser Token Formula
parseRunning = do
    f <- parseI
    eof
    return f

-- | Función principal que intenta parsear una lista de tokens hasta el final.
-- Si tiene éxito, devuelve la Fórmula (el AST).
parseLogic :: [Token] -> Either String Formula
parseLogic tokens = case run of
      Left err     -> Left err
      Right (f, _) -> Right f
    where
        run = runParser parseRunning tokens

---------------------------------------------------------------------------------------------------------
-- FUNCIONES AUXILIARES ÚTILES
---------------------------------------------------------------------------------------------------------

-- | Consume un token específico exacto (ej. matchToken TAnd)
matchToken :: Token -> Parser Token Token
matchToken expected = satisfy (== expected)

-- | Consume cualquier variable y extrae su nombre
-- ESTA ES LA FUNCIÓN PRÁCTICA PARA LIDIAR CON EL CONJUNTO INFINITO DE TVar
parseVar :: Parser Token Formula
parseVar = do
    tok <- satisfy (\t -> case t of 
                            TVar _ -> True 
                            _      -> False)
    case tok of
        TVar name -> return (Var name)
        _         -> pFail "Fallo interno inesperado en parseVar"


---------------------------------------------------------------------------------------------------------
-- REGLAS DE LA GRAMÁTICA (Descenso Recursivo)
---------------------------------------------------------------------------------------------------------

-- Nivel 1: IFF (<->)
-- Regla CFG: I -> Imp I'
parseI :: Parser Token Formula
parseI = do
    leftSide <- parseImp
    parseI' leftSide
    -- 1. Primero parseamos el lado izquierdo (que como mínimo será un Imp)
    -- leftSide <- ...
    -- 2. Le pasamos ese AST parcial a parseI' para que intente expandirlo
    -- parseI' leftSide

-- Regla CFG: I' -> IFF Imp I' | vacio
parseI' :: Formula -> Parser Token Formula
parseI' leftAST = 
    (do
        _ <- matchToken TIff -- Miramos si hay un "<->"
        rightSide <- parseImp -- Parseamos el token de la derecha de "<->"
        let newAST = Iff leftAST rightSide
        -- Definimos un nuevo árbol con la parte izquierda que nos ha llegado
        -- y la parte derecha que acabamos de calcular.
        parseI' newAST
        -- Llamamos recursivamente con el nuevo árbol por si hay encadenamiento: a <-> b <-> c
    ) 
    <|> return leftAST -- Si falla (epsilon), simplemente devolvemos lo que ya teníamos ensamblado.


-- Nivel 2: IMPLIES (->)
-- Regla CFG: Imp -> O Imp'
parseImp :: Parser Token Formula
parseImp = do
    leftSide <- parseO -- Pasamos el token al siguiente nivel
    parseImp' leftSide -- Miramos si ese token se encapsula en un "->"

parseImp' :: Formula -> Parser Token Formula
parseImp' leftAST =
    ( do
        _ <- matchToken TImplies
        rightSide <- parseO
        let newAST = Implies leftAST rightSide
        parseImp' newAST  

    ) <|> return leftAST


-- Nivel 3: OR (|)
-- Regla CFG: O -> A O'
parseO :: Parser Token Formula
parseO = do
    leftSide <- parseA
    parseO' leftSide

parseO' :: Formula -> Parser Token Formula
parseO' leftAST =
    ( do
        _ <- matchToken TOr
        rightSide <- parseA
        let newAST = Or leftAST rightSide
        parseO' newAST

    ) <|> return leftAST


-- Nivel 4: AND (&)
-- Regla CFG: A -> N A'
parseA :: Parser Token Formula
parseA = do
    leftSide <- parseN
    parseA' leftSide

parseA' :: Formula -> Parser Token Formula
parseA' leftAST =
    ( do
        _ <- matchToken TAnd
        rightSide <- parseN
        let newAST = And leftAST rightSide
        parseA' newAST
        
    ) <|> return leftAST


-- Nivel 5: NOT (~)
-- Regla CFG: N -> TNot N | Atom
-- Ojo: Esta no necesita recursión por la izquierda (N') porque asocia directamente.
parseN :: Parser Token Formula
parseN = 
    ( do
        _ <- matchToken TNot
        rightSide <- parseN
        return (Not rightSide)
    ) <|> parseAtom
    -- Intenta parsear un NOT y luego otra N recursivamente
    -- <|> 
    -- Si no empieza por NOT, simplemente lee un Atom


-- Nivel 6: ATOM (Variables y Paréntesis)
-- Regla CFG: Atom -> ( I ) | TVar
parseAtom :: Parser Token Formula
parseAtom = 
    -- Opción A: Es un grupo entre paréntesis
    (do
        _ <- matchToken TLParen
        ast_interior <- parseI
        _ <- matchToken TRParen
        return ast_interior
    )
    <|> parseVar
    -- Opción B: Es una variable atómica (usar la función parseVar definida arriba)
