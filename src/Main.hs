module Main where

import System.Environment (getArgs)
import System.IO
import Parser
import Expr
import Tableau
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn "Bem-vindo ao sistema de Tableaux para Lógica Clássica Proposicional!"
    putStrLn "Operadores disponíveis: ~ (neg), and, or, xor, ->, <->"
    putStrLn "Digite 'f' para sair ou 'n' para inserir uma nova fórmula."
    putStrLn "Digite 'a' para carregar a fórmula de um arquivo."
    mainLoop

mainLoop :: IO ()
mainLoop = do
    putStr "Escolha a opção (f/n/a): "
    hFlush stdout
    option <- getLine
    case option of
        "f" -> putStrLn "Encerrando o programa."
        "n" -> inserirNovaFormula
        "a" -> carregarFormulaArquivo
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            mainLoop

inserirNovaFormula :: IO ()
inserirNovaFormula = do
    putStr "Digite a fórmula lógica: "
    hFlush stdout
    formula <- getLine
    processarFormula formula
    mainLoop

carregarFormulaArquivo :: IO ()
carregarFormulaArquivo = do
    putStr "Digite o nome do arquivo: "
    hFlush stdout
    fileName <- getLine
    content <- readFile fileName
    processarFormula content
    mainLoop

processarFormula :: String -> IO ()
processarFormula formula = case parseExpr formula of
    Left err  -> print err
    Right expr -> do
        putStrLn "Construindo a árvore de refutação..."
        let tab = tableau expr  -- Usar a fórmula original para construir a árvore
        putStrLn $ draw tab
        putStrLn $ if satisfiable tab
                   then "A fórmula é satisfatível."
                   else "A fórmula é insatisfatível."
