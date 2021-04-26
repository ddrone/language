module Main where

data Ty
  = TySymbol
  | Arr Ty Ty
  deriving (Show)

type Variable = String

data Exp r
  = Symbol String
  | Var Variable
  | Lam Variable Ty r
  | App r r
  deriving (Show)

newtype Untyped = Untyped { getUntyped :: Exp Untyped }
  deriving (Show)

data Typed = Typed
  { exprType :: Ty
  , expr :: Exp Typed
  }
  deriving (Show)

main :: IO ()
main = do
  putStrLn "hello world"
