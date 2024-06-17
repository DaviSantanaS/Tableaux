module Main where

import System.Environment (getArgs)
import System.IO
import Parser
import Expr
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
        let env = Map.fromList [("p", True), ("q", True), ("r", True)]  -- Ambiente de exemplo
        print expr
        evalExpr expr env

evalExpr :: Expr -> Env -> IO ()
evalExpr expr env = do
    putStrLn $ "Avaliando a expressão com o ambiente " ++ show env
    let result = eval env expr
    putStrLn $ "Resultado: " ++ show result
    printTree expr env

printTree :: Expr -> Env -> IO ()
printTree expr env = do
    putStrLn $ show expr ++ " é " ++ if eval env expr then "satisfatível" else "insatisfatível"
    putStrLn $ formatTree expr env 0

formatTree :: Expr -> Env -> Int -> String
formatTree expr env indent =
    let padding = replicate indent ' '
        evalNode e = eval env e
        nodeValue e = if evalNode e then "T" else "F"
    in case expr of
        Atom x -> padding ++ "[" ++ x ++ " = " ++ nodeValue (Atom x) ++ "]"
        Neg p -> padding ++ "[" ++ show expr ++ " = " ++ nodeValue expr ++ "]\n" ++ formatTree p env (indent + 4)
        And p q -> padding ++ "[" ++ show expr ++ " = " ++ nodeValue expr ++ "]\n" ++ formatTree p env (indent + 4) ++ "\n" ++ formatTree q env (indent + 4)
        Or p q -> padding ++ "[" ++ show expr ++ " = " ++ nodeValue expr ++ "]\n" ++ formatTree p env (indent + 4) ++ "\n" ++ formatTree q env (indent + 4)
        Xor p q -> padding ++ "[" ++ show expr ++ " = " ++ nodeValue expr ++ "]\n" ++ formatTree p env (indent + 4) ++ "\n" ++ formatTree q env (indent + 4)
        Eqv p q -> padding ++ "[" ++ show expr ++ " = " ++ nodeValue expr ++ "]\n" ++ formatTree p env (indent + 4) ++ "\n" ++ formatTree q env (indent + 4)
        Imp p q -> padding ++ "[" ++ show expr ++ " = " ++ nodeValue expr ++ "]\n" ++ formatTree p env (indent + 4) ++ "\n" ++ formatTree q env (indent + 4)
