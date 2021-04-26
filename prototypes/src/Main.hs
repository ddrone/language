module Main where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Either

data Ty
  = TySymbol
  | Arr Ty Ty
  deriving (Show, Eq)

type Variable = String

data Exp r
  = Symbol String
  | Var Variable
  | Lam Variable Ty r
  | App r r
  deriving (Show)

newtype Untyped = Untyped { getUntyped :: Exp Untyped }
  deriving (Show)

type ParseError = String

-- Why not implement parsing combinators again for the n-th time?
-- TODO: chase down the origin for this type, I've learned it from Edward Kmett's stream
-- TODO: why not add start to a signature? Going to lead to clunkier primitives,
--       but would make it possible for easy error reporting
newtype Parser a = Parser { runParser :: [String] -> Either ParseError (Int, a) }

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

data Typed = Typed
  { exprType :: Ty
  , expr :: Exp Typed
  }
  deriving (Show)

type TypeError = String

type Env = [(String, Ty)]

typecheck :: Env -> Untyped -> Either TypeError Typed
typecheck env e = case getUntyped e of
  Symbol s -> pure (Typed TySymbol (Symbol s))
  Var v -> case lookup v env of
    -- TODO: some kind of string interpolation?
    Nothing -> Left $ concat ["variable ", v, " not found"]
    Just ty -> pure (Typed ty (Var v))
  App eFun eArg -> do
    typedFun <- typecheck env eFun
    typedArg <- typecheck env eArg
    case exprType typedFun of
      Arr arg result ->
        if arg == exprType typedArg
          then pure (Typed result (App typedFun typedArg))
          -- TODO: add an ability to point to node of a tree that causes type error
          else Left "wrong function application"
      _ -> Left "trying to apply non-function"
  Lam var ty body -> do
    typedBody <- typecheck ((var, ty) : env) body
    pure (Typed (Arr ty (exprType typedBody)) (Lam var ty typedBody))

main :: IO ()
main = do
  putStrLn "hello world"
