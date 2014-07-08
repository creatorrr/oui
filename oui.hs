import qualified Data.Char as C
import qualified Control.Monad as M

import Text.Parsec hiding (space, whitespace, spaces)
import Text.Parsec.String

-- Data Structures
data Tree a = (Tree a) `CONS` (Tree a)
            | NIL | T
            | CAR | CDR | ATOMP | EQ | QUOTE | COND | LAMBDA
            | Token a
            deriving (Eq, Show, Read)

type Symbol = String
type Expression = Tree Symbol
type Boole = Expression -- NIL and T
type Function = Expression -- lambda
type Program = [Expression]

-- Parser
program :: Parser Program
program = do
    whitespace
    list NIL `sepEndBy1` spaces

list :: Expression -> Parser Expression
list tail = parenthesized $ expression tail where
  parenthesized = between (char '(') (char ')')

expression :: Expression -> Parser Expression
expression tail = do
    whitespace
    car' <- atom <|> (list tail) <|> (return tail)
    cdr' <- restExpression <|> (return tail)
    return $ car' `CONS` cdr' where

      restExpression = spaces >> expression tail

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

-- F-Functions
atomp :: Expression -> Boole
atomp e = truthy . isAtom $ e where
  isAtom NIL = False
  isAtom (_ `CONS` _) = False
  isAtom _ = True

car :: Expression -> Expression
car NIL = NIL
car (head `CONS` _) = head

cdr :: Expression -> Expression
cdr NIL = NIL
cdr (_ `CONS` tail) = tail

eq :: Expression -> Expression -> Boole
eq NIL empty = truthy True
eq x y = truthy $ x == y

-- S-Functions
quote :: Expression -> Expression
quote x = x

-- Utils
truthy :: Bool -> Boole
truthy False = NIL
truthy _ = T

empty = NIL `CONS` NIL :: Expression

-- Tests
type TestTable = [(String, Program)] -- [(actual, expected)]
table :: TestTable
table =
    ("()", [empty]) :
    ("(T)", [T `CONS` NIL]) :
    []

runParseTests :: TestTable -> IO Bool
runParseTests = andM . (map equate) . parseAll where
  andM = foldl liftAnd (return True)
  liftAnd = M.liftM2 (&&)
  equate (a, e) = fmap (==e) a

  parseAll = map parsePair
  parsePair (s, k) = ((parseExpr s), k)
  parseExpr input = case (parse program "oui" input) of
                      Left err -> do {print err; return [NIL]}
                      Right x -> return x

-- TODO
-- eval
-- cond
-- apply
-- lamba
-- write moar tests for parser
