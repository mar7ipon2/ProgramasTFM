# ProgramasTFM: Formalización de Lenguajes Formales en Haskell

Este repositorio contiene el código fuente desarrollado como parte del Trabajo de Fin de Máster (TFM) titulado **"Formalización y verificación de lenguajes formales mediante teoría de autómatas"**.

El objetivo principal de este proyecto es ofrecer una implementación rigurosa y didáctica de los conceptos fundamentales de la teoría de lenguajes formales utilizando el lenguaje de programación funcional **Haskell**. Una vez establecida la base teórica, se ofrece una aplicación directa de los conceptos en forma de un analizador léxico y sintáctico para la lógica proposicional.

## 📂 Estructura del Proyecto

El código está organizado en tres módulos principales que reflejan los distintos niveles de la Jerarquía de Chomsky abordados en el trabajo teórico:

### 1. Autómatas (`/Autómatas`)
Contiene las implementaciones mediante Tipos de Datos Algebraicos (ADT) de diferentes máquinas de estados:
- **Autómatas Finitos Deterministas (DFA)** y **No Deterministas (NFA)**.
- **Autómatas Finitos con transiciones epsilon** (Implementaciones matriciales y con `Maybe`).
- **Autómatas de Pila (PDA)**.
- Algoritmos de traducción y equivalencia estructural entre modelos (`FiniteAutomatonTranslate.hs`).

### 2. Gramáticas (`/Gramáticas`)
Define los sistemas de reescritura formales que generan los lenguajes:
- **Gramáticas Regulares** (`RGrammar.hs`) y **Gramáticas Independientes del Contexto** (`CFGrammar.hs`).
- Traducciones para demostrar la equivalencia formal entre gramáticas generativas y autómatas reconocedores (`CFGtoPDA.hs`, `RGtofromNFA.hs`).

### 3. Análisis Sintáctico (`/AnálisisSintáctico`)
Este módulo es la aplicación directa de los dos anteriores, construyendo un *pipeline* completo de validación y evaluación para la lógica proposicional:
- **Analizador Léxico (Lexer)**: Utiliza autómatas finitos para procesar la cadena de texto y tokenizarla.
- **Analizador Sintáctico (Parser)**: Utiliza gramáticas LL(1) libres de contexto para validar la sintaxis y construir de forma determinista el Árbol de Sintaxis Abstracta (AST).
- **Evaluación Semántica**: Función capaz de procesar el AST derivado y comprobar mediante tablas de verdad si se trata de una tautología (`LogicAST.hs`).
- `Main.hs`: Punto de entrada que ensambla y ejecuta todo el proceso funcional.

## 🚀 Requisitos y Ejecución

Para ejecutar o compilar los ejemplos de este repositorio, necesitarás tener instalado:
- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) o la herramienta `ghcup` (`ghc` / `ghci`).
- Se hace amplio uso de librerías base y estándares de Haskell (ej: `containers` para `Data.Set` y `Data.Map`).

Puedes cargar el módulo principal en el intérprete interactivo GHCi fácilmente posicionándote en la carpeta correspondiente:

```bash
cd AnálisisSintáctico
ghci Main.hs
```

## 📄 Licencia y Autoría
Código desarrollado por **Martí Pons Garcia**.
