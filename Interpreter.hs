module Interpreter where

import           AbsGrammar
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map               as Map
import           Data.Maybe
import           System.Exit            (exitFailure)
import           System.IO              (hGetContents, stdin)

data Val
  = IntVal Integer
  | StrVal String
  | BoolVal Bool
  | ArrVal [Val]
  | FunVal TypeOrVoid [Arg] Env Block
  | NullVal

instance Show Val where
  show (IntVal n)           = show n
  show (StrVal s)           = show s
  show (BoolVal b)          = show b
  show (ArrVal ar)          = "[" ++ show ar ++ "]"
  show (FunVal tv args _ _) = show tv ++ "(" ++ show args ++ ")"
  show NullVal              = "n0ll"

instance Eq Val where
  (IntVal n1) == (IntVal n2) = n1 == n2
  (StrVal s1) == (StrVal s2) = s1 == s2
  (BoolVal b1) == (BoolVal b2) = b1 == b2
  (ArrVal ar1) == (ArrVal ar2) = ar1 == ar2
  FunVal {} == FunVal {} = False
  NullVal == NullVal = True

instance Ord Val where
  compare (IntVal n1) (IntVal n2) = compare n1 n2
  compare (StrVal s1) (StrVal s2) = compare s1 s2
  compare (BoolVal b1) (BoolVal b2) = compare b1 b2
  compare (ArrVal ar1) (ArrVal ar2) = compare ar1 ar2

type Loc = Integer

type Store = Map.Map Loc Val

type Env = Map.Map Ident Loc

type RunTimeExcept = ExceptT String IO

type Interp = ReaderT Env (StateT Store RunTimeExcept)

run p = runExceptT (runStateT (runReaderT (execProgram p) Map.empty) Map.empty)

-- type of return value for statements
type ReturnVal = (Env, Maybe Val)

-- default return value for statements
end :: Interp ReturnVal
end = do
  env <- ask
  return (env, Nothing)

-- get next free location and reserve it in store
alloc :: Interp Loc
alloc = do
  env <- ask
  store <- get
  let next =
        (if Map.null store
           then 0
           else maximum $ Map.keys store) +
        1
  modify $ Map.insert next NullVal
  return next

-- get value associated with ident
getVar :: Ident -> Interp Val
getVar id = do
  env <- ask
  store <- get
  case Map.lookup id env of
    Just loc -> case Map.lookup loc store of
      Just val -> return val
      _ -> throwError $ "Uninitialized variable " ++ show id
    _ -> throwError $ "Undeclared variable " ++ show id

-- get location associated with ident
getLoc :: Ident -> Interp Loc
getLoc id = do
  env <- ask
  let Just loc = Map.lookup id env
  return loc

-- save value for previously declared variable
saveVar :: Ident -> Val -> Interp ()
saveVar id v = do
  env <- ask
  case Map.lookup id env of
    Just loc -> modify (Map.insert loc v)
    _ -> throwError $ "Undeclared variable " ++ show id

-- declare new variable and save its value
saveNewVar :: Ident -> Val -> Interp Env
saveNewVar id v = do
  env <- ask
  loc <- alloc
  modify (Map.insert loc v)
  let newEnv = Map.insert id loc env
  return newEnv

-- evaluate binary integer expression
evalEBinInt :: Expr -> Expr -> (Integer -> Integer -> Integer) -> Interp Val
evalEBinInt e1 e2 f = do
  i1 <- eval e1
  i2 <- eval e2
  case (i1, i2) of
    (IntVal i1', IntVal i2') -> return $ IntVal (f i1' i2')

-- evaluate binary boolean expression
evalEBinBool :: Expr -> Expr -> (Bool -> Bool -> Bool) -> Interp Val
evalEBinBool e1 e2 f = do
  b1 <- eval e1
  b2 <- eval e2
  case (b1, b2) of
    (BoolVal b1', BoolVal b2') -> return $ BoolVal (f b1' b2')

evalRelOp :: Expr -> Expr -> (Val -> Val -> Bool) -> Interp Val
evalRelOp e1 e2 f = do
  n1 <- eval e1
  n2 <- eval e2
  return $ BoolVal (f n1 n2)

eval :: Expr -> Interp Val
eval (EVar i) = getVar i
eval (ELitInt n) = return $ IntVal n
eval ELitFalse = return $ BoolVal False
eval ELitTrue = return $ BoolVal True
eval (EString s) = return $ StrVal s
eval (EAdd e1 Plus e2) = evalEBinInt e1 e2 (+)
eval (EAdd e1 Minus e2) = evalEBinInt e1 e2 (-)
eval (EAdd e1 Concat e2) = do
  s1 <- eval e1
  s2 <- eval e2
  case s1 of
    StrVal s1' -> case s2 of
      StrVal s2' -> return $ StrVal $ s1' ++ s2'
      _ -> throwError $ "Cannot concatenate string " ++ s1'
    ArrVal ar1 -> case s2 of
      ArrVal ar2 -> return $ ArrVal $ ar1 ++ ar2
      _ -> throwError $ "Cannot concatenate array " ++ show ar1
    _ -> throwError $ "Cannot concatenate " ++ show s1 ++ " with " ++ show s2
eval (EMul e1 Times e2) = evalEBinInt e1 e2 (*)
eval (EMul e1 Div e2) = do
  IntVal n <- eval e2
  if n == 0
    then throwError "Division by 0"
    else evalEBinInt e1 e2 div
eval (EMul e1 Mod e2) = do
  IntVal n <- eval e2
  if n == 0
    then throwError "Modulo by 0"
    else evalEBinInt e1 e2 mod
eval (Neg e1) = do
  IntVal i <- eval e1
  return $ IntVal (-1 * i)
eval (Not e1) = do
  BoolVal b <- eval e1
  return $ BoolVal (not b)
eval (ERel e1 LE e2) = evalRelOp e1 e2 (<=)
eval (ERel e1 LTH e2) = evalRelOp e1 e2 (<)
eval (ERel e1 GE e2) = evalRelOp e1 e2 (>=)
eval (ERel e1 GTH e2) = evalRelOp e1 e2 (>)
eval (ERel e1 EQU e2) = evalRelOp e1 e2 (==)
eval (ERel e1 NE e2) = evalRelOp e1 e2 (/=)
eval (EAnd e1 e2) = evalEBinBool e1 e2 (&&)
eval (EOr e1 e2) = evalEBinBool e1 e2 (||)
eval (ArrayGet e1 e2) = do
  ArrVal ar <- eval e1
  IntVal idx <- eval e2
  if idx < 0 || idx >= toInteger (length ar)
    then throwError ("Index out of bounds: idx=" ++ show idx ++ " while array of size " ++ show (length ar))
    else return $ ar !! fromInteger idx
eval (ArrayLen e1) = do
  ArrVal ar <- eval e1
  return $ IntVal $ toInteger $ length ar
eval (ArraySet e1 e2 e3) = do
  ArrVal ar <- eval e1
  IntVal idx <- eval e2
  val <- eval e3
  let (x, _:ys) = splitAt (fromInteger idx) ar
  let ar2 = x ++ [val] ++ ys
  return $ ArrVal ar2
eval (ArrayPush e1 e2) = do
  ArrVal ar <- eval e1
  val <- eval e2
  return $ ArrVal $ ar ++ [val]
eval (ArrayRem e1 e2) = do
  ArrVal ar <- eval e1
  IntVal idx <- eval e2
  let (x, _:ys) = splitAt (fromInteger idx) ar
  return $ ArrVal $ x ++ ys
eval (EApp id vars) = do
  presEnv <- ask
  FunVal tv args envDec b <- getVar id
  envWithArgs <- local (const envDec) (saveArgs args vars)
  (envres, res) <- local (const envWithArgs) (exec $ BStmt b)
  case tv of
    TVVoid -> return NullVal
    _ ->
      case res of
        Nothing -> throwError ("Nothing returned from the function " ++ show id)
        Just val -> return val
  where
    saveArgs [] [] = ask
    saveArgs (Arg t id:as) (v:vs) = do
      env <- saveArgs as vs
      case v of
        ExprRefE e -> do
          val <- local (const env) (eval e)
          local (const env) (saveNewVar id val)
        ExprRefR id -> do
          loc <- local (const env) (getLoc id)
          return $ Map.insert id loc env

exec :: Stmt -> Interp ReturnVal
exec Empty = end
exec (Ass id e) = do
  v <- eval e
  saveVar id v
  end
exec (Incr id) = do
  IntVal n <- getVar id
  saveVar id (IntVal (n + 1))
  end
exec (Decr id) = do
  IntVal n <- getVar id
  saveVar id (IntVal (n - 1))
  end
exec (While e b) = do
  BoolVal v <- eval e
  if v
    then do
      (env, res) <- exec $ BStmt b
      case res of
        Nothing -> exec (While e b)
        _       -> return (env, res)
    else end
exec (BStmt (Block [])) = end
exec (BStmt (Block (s:b))) = do
  (env, res) <- exec s
  case res of
    Nothing -> local (const env) (exec (BStmt (Block b)))
    _       -> return (env, res)
exec (Print e) = do
  v <- eval e
  liftIO $ print v
  end
exec (Ret e) = do
  val <- eval e
  env <- ask
  return (env, Just val)
exec VRet = do
  env <- ask
  return (env, Just NullVal)
exec (SExp e) = do
  eval e
  end
exec (Decl t []) = end
exec (Decl t (v:vars)) =
  case v of
    Init id exp -> do
      val <- eval exp
      env <- saveNewVar id val
      local (const env) (exec (Decl t vars))
    NoInit id ->
      case t of
        Array t' -> do
          env <- saveNewVar id $ ArrVal []
          local (const env) (exec (Decl t vars))
        _ -> do
          env <- saveNewVar id NullVal
          local (const env) (exec (Decl t vars))

exec (Cond e b) = do
  BoolVal v <- eval e
  if v
    then exec $ BStmt b
    else end
exec (CondElse e b1 b2) = do
  BoolVal v <- eval e
  if v
    then exec $ BStmt b1
    else exec $ BStmt b2

execTopDef :: TopDef -> Interp ReturnVal
execTopDef (FnDef tv id args b) = do
  env <- saveNewVar id NullVal
  let f = FunVal tv args env b
  local (const env) (saveVar id f)
  let Ident name = id
  if name == "main"
    then do
      (tenv, res) <- local (const env) (exec (BStmt b))
      return (env, res)
    else
      return (env, Nothing)

execProgram :: Program -> Interp ReturnVal
execProgram (Program []) = end
execProgram (Program (td:tds)) = do
  (env, res) <- execTopDef td
  local (const env) (execProgram $ Program tds)
