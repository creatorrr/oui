import qualified Data.Char as C
import qualified Control.Monad as M

import Text.Parsec hiding (space, whitespace, spaces)
import Text.Parsec.String

-- Data Structures
infixr 7 `CONS`
data Tree a = Tree a `CONS` Tree a
            | NIL | T
            | CAR | CDR | ATOMP | EQ | QUOTE | COND | LAMBDA
            | Token a
            deriving (Eq, Show, Read)

type Symbols = String
type Expression = Tree Symbols
type Boole = Expression -- NIL and T
type Program = [Expression]

-- Parser
parseOui :: Symbols -> Either ParseError Program
parseOui = parse program "oui"

program :: Parser Program
program = do
    whitespace
    list NIL `sepEndBy1` spaces

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
atom = fmap (readToken . uppercase) atom' where
  atom' = do
    whitespace
    t <- token
    return t

  token = many1 valid
  valid = alphaNum <|> char '-'
  uppercase = map C.toUpper
  readToken t = case reads t of
                  [(x, "")] -> x
                  _ -> Token t

whitespace = many space
spaces = many1 space
space = oneOf $ ',' : ' ' : '\t' : '\r' : '\n' : []

-- Folder
foldTreeL :: (a -> Tree b -> a) -> a -> Tree b -> a
foldTreeL f v = foldl' where
  foldl' (car' `CONS` cdr') = (foldl' car') `f` cdr'
  foldl' a = v `f` a

foldTreeR :: (Tree a -> b -> b) -> b -> Tree a -> b
foldTreeR f v = foldr' where
  foldr' (car' `CONS` cdr') = car' `f` (foldr' cdr')
  foldr' a = a `f` v

-- Printer
type ShowExpression = Symbols -> String

-- showExpr :: Expression -> ShowExpression
-- showExpr (car' `CONS` cdr') = ('[':) . showExpr car' . (' ':) . showExpr cdr' . (']':)
--
-- showExpr NIL = (')':)
-- showExpr (Token a) = (a++)
-- showExpr a = shows a

-- showProgram :: Program -> String
-- showProgram [] = ""
-- showProgram (e:es) = ('(':) . showExpr e $ showProgram es

showProgram :: a
showProgram = undefined

-- F-Functions
atomp :: Expression -> Bool
atomp (_ `CONS` _) = False
atomp NIL = False
atomp _ = True

listp :: Expression -> Bool
listp = not . atomp

car :: Expression -> Expression
car (NIL `CONS` NIL) = NIL
car (car' `CONS` _) = car'

cdr :: Expression -> Expression
cdr (NIL `CONS` NIL) = NIL
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
    ("( )", [NIL `CONS` NIL]) :
    ("(T T T )", [T `CONS` (T `CONS` (T `CONS` NIL))]):
    ("(( ))", [(NIL `CONS` NIL) `CONS` NIL]):
    ("(T )", [T `CONS` NIL]) :
    []

test :: TestTable -> ResultTable
test = map exec where
  exec (s, p) = (parseProgram s == p, showProgram p == s)
  parseProgram = either nil id . parseOui
  nil = do return [NIL]
