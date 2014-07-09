import qualified Data.Char as C
import qualified Control.Monad as M

import Text.Parsec hiding (space, whitespace, spaces)
import Text.Parsec.String

-- Data Structures
infixr 7 `CONS`
data Tree a = (Tree a) `CONS` (Tree a) | ROOT'
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
type ShowExpression = Symbol -> String

-- showExpression :: Expression -> ShowExpression
-- showExpression (Token a) = (a++)
-- showExpression NIL = (""++)
-- showExpression (car' `CONS` NIL) = showExpression car' . (')':)
--
-- showExpression (car' `CONS` cdr')
  -- | atomp . car $ cdr' = showExpression car' . (' ':) . showExpression cdr'
  -- | otherwise = showExpression car' . (' ':) . ('(':) . showExpression cdr'
--
-- showExpression a = shows a
--
-- showProgram :: Program -> String
-- showProgram [] = ""
-- showProgram (e:es) = ('(':) . showExpression e $ showProgram es

showProgram :: a
showProgram = undefined

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
type TestTable = [(String, Program)] -- [(actual, expected)]
table :: TestTable
table =
    ("()", [ROOT' `CONS` ROOT']) :
    ("(T T T)", [T `CONS` (T `CONS` (T `CONS` ROOT'))]):
    ("(())", [(ROOT' `CONS` ROOT') `CONS` ROOT']):
    ("(T)", [T `CONS` ROOT']) :
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
-- add error messages to Parser
-- make tests pass
-- add support for dotted lists
