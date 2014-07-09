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
type ShowExpression = Symbol -> String

showExpression :: (Eq a, Show a) => Tree a -> ShowExpression
showExpression (Token a) = shows a
showExpression (car' `CONS` NIL)
  | car' == NIL = ('(':) . (')':)
  | otherwise = ('(':) . showExpression car' . (')':)

showExpression (car' `CONS` cdr') = showExpression car' . (' ':) . showExpression cdr'
showExpression a = shows a

showProgram :: Program -> String
showProgram [] = ""
showProgram (e:es) = showExpression e $ showProgram es

-- F-Functions
atomp :: Expression -> Boole
atomp e = truthy . isAtom $ e where
  isAtom NIL = False
  isAtom (_ `CONS` _) = False
  isAtom _ = True

car :: Expression -> Expression
car NIL = NIL
car (car' `CONS` _) = car'

cdr :: Expression -> Expression
cdr NIL = NIL
cdr (_ `CONS` cdr') = cdr'

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

testParser :: TestTable -> IO Bool
testParser = andM . (map equate) . parseAll where
  andM = foldl liftAnd (return True)
  liftAnd = M.liftM2 (&&)
  equate (a, e) = fmap (==e) a

  parseAll = map parsePair
  parsePair (s, k) = ((parseProgram s), k)
  parseProgram input = case (parse program "oui" input) of
                      Left err -> do {print err; return [NIL]}
                      Right x -> return x

testPrinter :: TestTable -> Bool
testPrinter = and . (map equate) . printAll where
  equate (a, e) = a == e
  printAll = map printProgram
  printProgram (s, p) = (s, (showProgram p))

-- TODO
-- eval
-- cond
-- apply
-- lamba
-- write moar tests
-- add error messages to Parser
