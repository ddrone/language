module Main where

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
