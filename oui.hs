import qualified Data.Char as C
import Text.Parsec hiding (space, whitespace, spaces)
import Text.Parsec.String

-- Data Structures
data Tree a = CONS (Tree a) (Tree a)
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
    return $ CONS car' cdr' where

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
truthy :: Bool -> Boole
truthy False = NIL
truthy _ = T

atomp :: Expression -> Boole
atomp e = truthy . isAtom $ e where
  isAtom NIL = False
  isAtom (CONS _ _) = False
  isAtom _ = True

car :: Expression -> Expression
car NIL = NIL
car (CONS head _) = head

cdr :: Expression -> Expression
cdr NIL = NIL
cdr (CONS _ tail) = tail

eq :: Expression -> Expression -> Boole
eq x = truthy . (==x)

-- S-Functions
quote :: Expression -> Expression
quote x = x

-- TODO
-- eval
-- cond
-- apply
-- lamba

-- append :: Expression -> Expression -> Expression
-- append NIL = quote
-- append (CONS s rest) = append rest . CONS s
