module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Expr

binary s f assoc = Ex.Infix (reservedOp s >> return f) assoc
prefix s f = Ex.Prefix (reservedOp s >> return f)
table = [ [prefix "~" Neg ]
        , [binary "and" And Ex.AssocLeft]
        , [binary "or" Or Ex.AssocLeft]
        , [binary "xor" Xor Ex.AssocLeft]
        , [binary "->" Imp Ex.AssocRight]
        , [binary "<->" Eqv Ex.AssocRight]
        ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

term =  parens expr
    <|> fmap Atom identifier

identifier :: Parser String
identifier = Tok.identifier lexer

parseExpr :: String -> Either ParseError Expr
parseExpr input = parse (whiteSpace >> expr) "<stdin>" input
