import Prelude hiding (EQ)
import qualified Data.Char as C
import qualified Control.Monad as M

import Text.Parsec hiding (space, whitespace, spaces)
import Text.Parsec.String

-- Data Structures
infixr 7 `CONS`
data Tree a = Tree a `CONS` Tree a | NIL
            -- Primitives
            | T
            | CAR | CDR | ATOMP | EQ | QUOTE | COND | LAMBDA

            -- Container for user defined tokens
            | Token a
            deriving (Eq, Show, Read)

-- Synonyms for commonly used types
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
list root = do
  parenthesized $ do
    -- Try to see if empty list
    whitespace
    lookAhead $ try $ char ')'
    return root

    -- else parse expression
    <|> expression root

  where parenthesized = between (char '(') (char ')')

expression :: Expression -> Parser Expression
expression root = do
  whitespace
  car' <- atom <|> (list root) <|> (return root)
  cdr' <- restExpression <|> (return root)

  return $ car' `CONS` cdr'

  where restExpression = spaces >> expression root

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

-- Folders
foldTreeL :: (a -> Tree b -> a) -> a -> Tree b -> a
foldTreeL f v = foldl' where
  foldl' (car' `CONS` cdr') = f (foldl' car') cdr'
  foldl' a = v `f` a

foldTreeR :: (Tree a -> b -> b) -> b -> Tree a -> b
foldTreeR f v = foldr' where
  foldr' (car' `CONS` cdr') = f car' (foldr' cdr')
  foldr' a = a `f` v

-- Printer
type StringGen = Symbols -> String

showExpr :: Expression -> StringGen
showExpr (car' `CONS` cdr') = ('(':) . showExpr car' . showRest cdr' . (')':) where

  showRest :: Expression -> StringGen
  showRest (car' `CONS` cdr') = (' ':) . showExpr car' . showRest cdr'
  showRest NIL = (""++)

  -- Dotted lists support
  showRest a = ('.':) . showExpr a

showExpr (Token a) = (a++)
showExpr NIL = ("()"++)
showExpr a = shows a

showProgram :: Program -> String
showProgram = foldr showExpr ""

-- Define primitives from McCarthy's paper
-- http://www-formal.stanford.edu/jmc/recursive.pdf

-- F-Functions (Basic predicates)
atomp :: Expression -> Boole
atomp (_ `CONS` _) = NIL
atomp NIL = NIL
atomp _ = T

listp :: Expression -> Boole
listp (_ `CONS` _) = T
listp NIL = T
listp _ = NIL

car :: Expression -> Expression
car (car' `CONS` _) = car'

cdr :: Expression -> Expression
cdr (_ `CONS` cdr') = cdr'

eq :: Expression -> Expression -> Boole
eq x y = toBoole $ x == y

-- S-Functions (Functions to operate on s-expressions)
quote :: Expression -> Expression
quote x = x

when :: Expression -> Expression
when (pred `CONS` action) = if (toBool . apply) pred then (apply action) else NIL

apply :: Expression -> Expression
apply (QUOTE `CONS` cdr') = cdr'
apply (ATOMP `CONS` cdr') = atomp cdr'
apply (EQ `CONS` (a `CONS` b)) = eq a b
apply (CAR `CONS` cdr') = car cdr'
apply (CDR `CONS` cdr') = cdr cdr'

-- cond
-- lambda

-- Utils
toBoole :: Bool -> Boole
toBoole False = NIL
toBoole _ = T

toBool :: Boole -> Bool
toBool NIL = False
toBool _ = True

-- Tests
type Parsed = Bool
type TestTable = [(Symbols, Program)]
type ResultTable = [(Parsed, Parsed)]

table :: TestTable
table =
    ("()", [NIL]):
    ("(T T T)", [T `CONS` T `CONS` T `CONS` NIL]):
    ("(())", [NIL `CONS` NIL]):
    ("(T)", [T `CONS` NIL]) :
    []

test :: TestTable -> ResultTable
test = map exec where
  exec (s, p) = (parseProgram s == p, showProgram p == s)
  parseProgram = either nil id . parseOui
  nil = do return [NIL]
