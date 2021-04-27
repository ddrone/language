{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Text
import Data.Char
import Data.Either
import Data.Foldable
import Data.Functor
import GHC.Generics
import qualified Data.Text.Lazy.IO as TextIO

data Ty
  = TySymbol
  | Arr Ty Ty
  deriving (Show, Eq)

renderType :: Ty -> String
renderType = \case
  TySymbol -> "*"
  Arr t1 t2 -> concat ["(", renderType t1, " -> ", renderType t2, ")"]

type Variable = String

data Exp r
  = Symbol String
  | Var Variable
  | Lam Variable Ty r
  | App r r
  deriving (Show, Foldable)

data Position = Position
  { startPos :: Int
  , endPos :: Int
  }
  deriving (Show, Generic)

instance ToJSON Position where

data Untyped = Untyped
  { getUntyped :: Exp Untyped
  , untypedPos :: Position
  }
  deriving (Show)

type ParseError = String

type Token = (Int, String)

-- Why not implement parsing combinators again for the n-th time?
-- TODO: chase down the origin for this type, I've learned it from Edward Kmett's stream
-- TODO: why not add start to a signature? Going to lead to clunkier primitives,
--       but would make it possible for easy error reporting
newtype Parser a = Parser { runParser :: [Token] -> Either ParseError (Int, a) }

instance Functor Parser where
  -- Congratulations on writing code even you can't understand!
  -- Which Functor instance is used in the second occurrence of fmap? Who knows!
  fmap f (Parser p) = Parser (fmap (second f) . p)

instance Applicative Parser where
  pure x = Parser (const $ pure (0, x))
  (<*>) = ap

instance Monad Parser where
  px >>= f = Parser $ \input -> do
    (x, a) <- runParser px input
    (y, b) <- runParser (f a) (drop x input)
    -- Using "WARN" might be a good idea, a la Bourbaki's "dangerous bend", but for shitty code
    -- WARN: Is this actually correct?
    pure (x + y, b)

instance Alternative Parser where
  empty = Parser (const $ Left "empty")
  pa <|> pb = Parser $ \input ->
    case runParser pa input of
      Left _ -> runParser pb input
      result@(Right _) -> result

expect :: String -> Parser Int
expect token = Parser $ \case
  [] -> Left $ concat ["expected ", token, " got EOF"]
  ((pos, t) : ts) | t == token -> pure (1, pos)
  (t : ts) -> Left $ concat ["expected ", token, " got ", snd t]

eat :: Parser (Int, String)
eat = Parser $ \case
  [] -> Left "unexpected EOF"
  (t : ts) -> pure (1, t)

leaf :: Parser Untyped
leaf = asum
  [ do start <- expect "*"
       pure (Untyped (Symbol "*") (Position start (start + 1)))
  -- TODO: the following will eat bracket as well, maybe do something about it
  , do (start, v) <- eat
       pure (Untyped (Var v) (Position start (start + length v)))
  ]

typeParser :: Parser Ty
typeParser = asum
  [ expect "*" $> TySymbol
  , do expect "("
       t1 <- typeParser
       expect "->"
       t2 <- typeParser
       expect ")"
       pure (Arr t1 t2)
  ]

parseExpr :: Parser Untyped
parseExpr = asum
  [ do start <- expect "("
       e <- compound
       beforeEnd <- expect ")"
       pure (Untyped e (Position start (beforeEnd + 1)))
  , leaf
  ]

compound :: Parser (Exp Untyped)
compound = asum
  [ do expect "fun"
       name <- snd <$> eat
       ty <- typeParser
       Lam name ty <$> parseExpr
  , do fun <- parseExpr
       App fun <$> parseExpr
  ]

parse :: Parser a -> String -> Either ParseError a
parse p input =
  let tokens = indexedWords 0 input in
  case runParser p tokens of
    Left err -> Left err
    Right (x, a) ->
      if x == length tokens
        then Right a
        else Left $ concat ["consumed ", show x, " out of ", show (length input), " tokens"]

data Typed = Typed
  { exprType :: Ty
  , expr :: Exp Typed
  , typedPos :: Position
  }
  deriving (Show)

type TypeError = String

type Env = [(String, Ty)]

typecheck :: Env -> Untyped -> Either TypeError Typed
typecheck env e =
  let pos = untypedPos e in
  case getUntyped e of
    Symbol s -> pure (Typed TySymbol (Symbol s) pos)
    Var v -> case lookup v env of
      -- TODO: some kind of string interpolation?
      Nothing -> Left $ concat ["variable ", v, " not found"]
      Just ty -> pure (Typed ty (Var v) pos)
    App eFun eArg -> do
      typedFun <- typecheck env eFun
      typedArg <- typecheck env eArg
      case exprType typedFun of
        Arr arg result ->
          if arg == exprType typedArg
            then pure (Typed result (App typedFun typedArg) pos)
            -- TODO: add an ability to point to node of a tree that causes type error
            else Left "wrong function application"
        _ -> Left "trying to apply non-function"
    Lam var ty body -> do
      typedBody <- typecheck ((var, ty) : env) body
      pure (Typed (Arr ty (exprType typedBody)) (Lam var ty typedBody) pos)

data TypeTree = TypeTree
  { range :: Position
  , treeType :: String
  , children :: [TypeTree]
  }
  deriving (Generic, Show)

instance ToJSON TypeTree where

toTypeTree :: Typed -> TypeTree
toTypeTree tree =
  let children = map toTypeTree (toList (expr tree)) in
  TypeTree (typedPos tree) (renderType (exprType tree)) children

testInput :: String -> IO ()
testInput input = do
  print $ indexedWords 0 input
  let parsed = parse parseExpr input
  case parsed of
    Left err -> putStrLn err
    Right r -> do
      print r
      let typed = typecheck [] r
      case typed of
        Left err -> putStrLn err
        Right t -> do
          print t
          let typeTree = toTypeTree t
          TextIO.writeFile "out.json" (encodeToLazyText typeTree)

indexedWords :: Int -> String -> [(Int, String)]
indexedWords n = \case
  [] -> []
  c : rest | isSpace c -> indexedWords (n + 1) rest
  input ->
    let (word, rest) = break isSpace input in
      (n, word) : indexedWords (n + length word) rest

main :: IO ()
main = do
  input <- readFile "input.lan"
  testInput input
