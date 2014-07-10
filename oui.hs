import qualified Data.Char as C
import qualified Control.Monad as M

import Text.Parsec hiding (space, whitespace, spaces)
import Text.Parsec.String

-- Data Structures
infixr 7 `CONS`
data Tree a = Tree a `CONS` Tree a | ROOT'
            | NIL | T
            | CAR | CDR | ATOMP | EQ | QUOTE | COND | LAMBDA
            | Token a
            deriving (Eq, Show, Read)

type Symbols = String
type Expression = Tree Symbols
type Boole = Expression -- NIL and T
type Function = Expression -- lambda
type Program = [Expression]

-- Parser
parseOui :: Symbols -> Either ParseError Program
parseOui = parse program "oui"

program :: Parser Program
program = do
    whitespace
    list ROOT' `sepEndBy1` spaces

list :: Expression -> Parser Expression
list root = parenthesized $ expression root where
  parenthesized = between (char '(') (char ')')

expression :: Expression -> Parser Expression
expression root = do
    whitespace
    car' <- atom <|> (list root) <|> (return root)
    cdr' <- restExpression <|> (return root)
    return $ car' `CONS` cdr' where

      restExpression = spaces >> expression root

atom :: Parser Expression
atom = fmap (readToken . uppercase) token where
  token = many1 valid
  valid = alphaNum <|> char '-'
  uppercase = map C.toUpper
  readToken t = case reads t of
                  [(x, "")] -> x
                  _ -> Token t

whitespace = many space
spaces = many1 space
space = oneOf $ ',' : ' ' : '\t' : '\r' : '\n' : []

-- Printer
type ShowExpression = Symbols -> String

showExpr :: Expression -> ShowExpression
showExpr (car' `CONS` cdr')
  | not . atomp $ car' = ('(':) . showExpr car' . (' ':) . showExpr cdr'
  | otherwise = showExpr car' . (' ':) . showExpr cdr'

showExpr ROOT' = (""++)
showExpr (Token a) = (a++)
showExpr a = shows a

showProgram :: Program -> String
showProgram [] = ""
showProgram (e:es) = showExpr e $ showProgram es

-- showProgram :: a
-- showProgram = undefined

-- F-Functions
atomp :: Expression -> Bool
atomp (_ `CONS` _) = False
atomp _ = True

car :: Expression -> Expression
car (ROOT' `CONS` ROOT') = ROOT' `CONS` ROOT'
car (car' `CONS` _) = car'

cdr :: Expression -> Expression
cdr (ROOT' `CONS` ROOT') = ROOT' `CONS` ROOT'
cdr (_ `CONS` cdr') = cdr'

eq :: Expression -> Expression -> Bool
eq x y = x == y

-- S-Functions
quote :: Expression -> Expression
quote x = x

-- Utils
truthy :: Bool -> Boole
truthy False = NIL
truthy _ = T

-- Tests
type Parsed = Bool
type TestTable = [(Symbols, Program)]
type ResultTable = [(Parsed, Parsed)]

table :: TestTable
table =
    ("()", [ROOT' `CONS` ROOT']) :
    ("(T T T)", [T `CONS` (T `CONS` (T `CONS` ROOT'))]):
    ("(())", [(ROOT' `CONS` ROOT') `CONS` ROOT']):
    ("(T)", [T `CONS` ROOT']) :
    []

test :: TestTable -> ResultTable
test = map exec where
  exec (s, p) = (parseProgram s == p, showProgram p == s)
  parseProgram = either nil id . parseOui
  nil = do return [NIL]

-- TODO
-- make tests pass
-- implement tree fold
-- eval
-- cond
-- apply
-- lamba
-- add error messages to Parser
-- add support for dotted lists
