module TypeChecker where

import           AbsGrammar
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error.Class  (throwError)
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Control.Monad.Writer
import           Data.Either                (isLeft)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Typeable
import           ErrM                       (Err (Bad))
import           System.Exit                (exitFailure)

data FunType =
  FunType TypeOrVoid [Type]
  deriving (Eq, Show)

type TCType = Either FunType TypeOrVoid

type TCEnv = Map.Map Ident TCType

type TChecker = ReaderT TCEnv (ExceptT String (WriterT [String] Identity))

type ReturnVal = (TCEnv, TCType)

type ReturnExp = TCType

toReturn :: Type -> ReturnExp
toReturn t = Right $ TVType t

end :: TChecker (TCEnv, ReturnExp)
end = do
  env <- ask
  return (env, Right TVVoid)

runCheck :: TCEnv -> TChecker a -> (Either String a, [String])
runCheck env ev = runIdentity (runWriterT (runExceptT (runReaderT ev env)))

typeCheck :: Program -> IO ()
typeCheck p = do
  let (err, msg) = runCheck Map.empty (typeCheckProgram p)
  case err of
    Left s -> do
      putStrLn s
      mapM_ putStrLn msg
      exitFailure
    Right _ -> mapM_ putStrLn msg

assertType :: ReturnExp -> ReturnExp -> TChecker ReturnExp
assertType actual expected = do
  when (actual /= expected) $ tell ["Expected type " ++ show expected ++ " but got " ++ show actual]
  return expected

assertTypeExpr :: Expr -> TCType -> TChecker ReturnExp
assertTypeExpr e t = do
  et <- typeCheckExpr e
  assertType et t

assertResultInt :: Expr -> Expr -> TChecker ReturnExp
assertResultInt e1 e2 = do
  assertTypeExpr e1 $ Right $ TVType AbsGrammar.Int
  assertTypeExpr e2 $ Right $ TVType AbsGrammar.Int
  return $ toReturn AbsGrammar.Int

assertResultBool :: Expr -> Expr -> TChecker ReturnExp
assertResultBool e1 e2 = do
  assertTypeExpr e1 $ Right $ TVType AbsGrammar.Bool
  assertTypeExpr e2 $ Right $ TVType AbsGrammar.Bool
  return $ toReturn AbsGrammar.Bool

assertResultStr :: Expr -> Expr -> TChecker ReturnExp
assertResultStr e1 e2 = do
  assertTypeExpr e1 $ Right $ TVType Str
  assertTypeExpr e2 $ Right $ TVType Str
  return $ toReturn Str

assertResultInSameType :: Expr -> Expr -> TChecker ReturnExp
assertResultInSameType e1 e2 = do
  t1 <- typeCheckExpr e1
  t2 <- typeCheckExpr e2
  return $ toReturn AbsGrammar.Bool

getTypeFromEnv :: Ident -> TChecker ReturnExp
getTypeFromEnv id = do
  env <- ask
  case Map.lookup id env of
    Nothing  -> throwError ("Unbound variable: " ++ show id)
    Just val -> return val

getFunFromEnv :: TChecker (Ident, TCType)
getFunFromEnv = asks (Map.elemAt 0 . Map.filter isLeft)

saveTypeInEnv :: Ident -> TCType -> TChecker TCEnv
saveTypeInEnv id t = do
  env <- ask
  case Map.lookup id env of
    Nothing -> return $ Map.insert id t env
    Just val -> do
      tell ["Variable already declared " ++ show id]
      return env

typeCheckExprOrRef :: ExprOrRef -> TChecker ReturnExp
typeCheckExprOrRef er =
  case er of
    ExprRefR id -> getTypeFromEnv id
    ExprRefE e  -> typeCheckExpr e

assertArgsType :: [ExprOrRef] -> [Type] -> TChecker ReturnExp
assertArgsType [] [] = return $ Right TVVoid
assertArgsType (e:es) (t:ts) = do
  et <- typeCheckExprOrRef e
  assertType et $ toReturn t
  assertArgsType es ts
assertArgsType _ _ = throwError "Unexpected number of arguments"

typeCheckExpr :: Expr -> TChecker ReturnExp
-- ARITHMETIC AND BOOLEAN
typeCheckExpr (ELitInt _) = return $ toReturn AbsGrammar.Int
typeCheckExpr ELitTrue = return $ toReturn AbsGrammar.Bool
typeCheckExpr ELitFalse = return $ toReturn AbsGrammar.Bool
typeCheckExpr (EString _) = return $ toReturn Str
typeCheckExpr (EVar id) = getTypeFromEnv id
typeCheckExpr (EMul e1 _ e2) = assertResultInt e1 e2
typeCheckExpr (EAdd e1 o e2) =
  case o of
    Concat -> assertResultStr e1 e2
    _      -> assertResultInt e1 e2
typeCheckExpr (ERel e1 _ e2) = assertResultInSameType e1 e2
typeCheckExpr (EAnd e1 e2) = assertResultBool e1 e2
typeCheckExpr (EOr e1 e2) = assertResultBool e1 e2
typeCheckExpr (Not e1) = assertResultBool e1 ELitTrue
typeCheckExpr (Neg e1) = assertResultInt e1 (ELitInt 0)

-- FUNCTIONA
typeCheckExpr (EApp id args) = do
  t <- getTypeFromEnv id
  case t of
    Left (FunType tv argst) -> do
      assertArgsType args argst
      return $ Right tv
    _ -> do
      tell ["Expected type function of " ++ show id]
      return $ Right TVVoid
      
-- ARRAYS
typeCheckExpr (ArrayGet a e) = do
  at <- typeCheckExpr a
  case at of
    Right (TVType (Array t)) -> do
      assertResultInt e (ELitInt 0)
      return $ toReturn t
    _ -> do
      tell ["Expected type Array of " ++ show a]
      return $ toReturn AbsGrammar.Int
typeCheckExpr (ArrayLen a) = do
  at <- typeCheckExpr a
  case at of
    Right (TVType (Array t)) -> return $ toReturn AbsGrammar.Int
    _ -> do
      tell ["Expected type Array of " ++ show a]
      return $ toReturn AbsGrammar.Int
typeCheckExpr (ArraySet exp1 exp2 exp3) = do
  ar <- typeCheckExpr exp1
  assertTypeExpr exp2 (Right $ TVType AbsGrammar.Int)
  case ar of
    Right (TVType (Array t)) -> do
      assertTypeExpr exp3 (Right $ TVType t)
      return $ Right $ TVType $ Array t
    _ -> do
      tell ["First argument must be of Array type"]
      return $ Right TVVoid
typeCheckExpr (ArrayPush exp1 exp2) = do
  ar <- typeCheckExpr exp1
  case ar of
    Right (TVType (Array t)) -> do
      assertTypeExpr exp2 (Right $ TVType t)
      return $ Right $ TVType $ Array t
    _ -> do
      tell ["First argument must be of Array type"]
      return $ Right TVVoid
typeCheckExpr (ArrayRem exp1 exp2) = do
  ar <- typeCheckExpr exp1
  assertTypeExpr exp2 (Right $ TVType AbsGrammar.Int)
  case ar of
    Right (TVType (Array t)) -> return $ Right $ TVType t
    _ -> do
      tell ["First argument must be of Array type"]
      return $ Right TVVoid

-- STATEMENTS
typeCheckStmt :: Stmt -> TChecker (TCEnv, ReturnExp)
typeCheckStmt Empty = end
typeCheckStmt (BStmt (Block b)) =
  case b of
    [x] -> typeCheckStmt x
    (x:xs) -> do
      (env, res) <- typeCheckStmt x
      (newenv, res) <- local (const env) (typeCheckStmt $ BStmt $ Block xs)
      return (newenv, res)
    _ -> typeCheckStmt Empty
typeCheckStmt (Decl t vars) =
  case vars of
    [] -> end
    (v:vs) ->
      case v of
        NoInit id -> do
          env <- saveTypeInEnv id (Right $ TVType t)
          local (const env) (typeCheckStmt (Decl t vs))
        Init id exp -> do
          assertTypeExpr exp (Right $ TVType t)
          env <- saveTypeInEnv id (Right $ TVType t)
          local (const env) (typeCheckStmt (Decl t vs))
typeCheckStmt (Ass id exp) = do
  t <- getTypeFromEnv id
  assertTypeExpr exp t
  end
typeCheckStmt (Incr id) = do
  t <- getTypeFromEnv id
  assertType t (Right $ TVType Int)
  end
typeCheckStmt (Decr id) = do
  t <- getTypeFromEnv id
  assertType t (Right $ TVType Int)
  end
typeCheckStmt (Ret exp) = do
  t <- typeCheckExpr exp
  env <- ask
  return (env, t)
typeCheckStmt VRet = end
typeCheckStmt (Cond exp b) = do
  assertTypeExpr exp (Right $ TVType AbsGrammar.Bool)
  typeCheckStmt (BStmt b)
typeCheckStmt (CondElse exp b1 b2) = do
  assertTypeExpr exp (Right $ TVType AbsGrammar.Bool)
  typeCheckStmt (BStmt b1)
  typeCheckStmt (BStmt b2)
typeCheckStmt (While exp b) = do
  assertTypeExpr exp (Right $ TVType AbsGrammar.Bool)
  typeCheckStmt (BStmt b)
typeCheckStmt (Print exp) = do
  assertTypeExpr exp (Right $ TVType Str)
  end
typeCheckStmt (SExp exp) = do
  typeCheckExpr exp
  end

getTypeFromArgs :: [Arg] -> TChecker [Type]
getTypeFromArgs [] = return []
getTypeFromArgs (Arg t id:xs) = do
  ar <- getTypeFromArgs xs
  return $ t : ar

typeCheckTopDef :: TopDef -> TChecker TCEnv
typeCheckTopDef (FnDef tv id args b) = do
  targs <- getTypeFromArgs args
  envWithFun <- saveTypeInEnv id (Left $ FunType tv targs)
  envWithArgs <- local (const envWithFun) $ saveArgs args
  (env, res) <- local (const envWithArgs) (typeCheckStmt (BStmt b))
  assertType res $ Right tv
  return envWithFun
  where
    saveArgs [] = ask
    saveArgs (Arg t id:as) = do
      env <- saveArgs as
      local (const env) (saveTypeInEnv id $ Right $ TVType t)

typeCheckProgram :: Program -> TChecker TCEnv
typeCheckProgram (Program []) = ask
typeCheckProgram (Program (x:xs)) = do
  env <- typeCheckTopDef x
  local (const env) $ typeCheckProgram (Program xs)
