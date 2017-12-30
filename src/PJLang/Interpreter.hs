module PJLang.Interpreter (newEnv, eval) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldlM)

import PJLang.Ast
import PJLang.Env
import qualified PJLang.StdLib as StdLib

newEnv :: IO Env
newEnv = do
    env <- empty
    (uncurry $ setVar env) `mapM_` builtins
    return env
    where
        builtins = [
                ("print",     NativeFuncVal StdLib.print),
                ("printLine", NativeFuncVal StdLib.printLine),
                ("readLine",  NativeFuncVal StdLib.readLine)
            ]

eval :: Env -> Expr -> IOExceptEval Val

eval _env NullE = return NullVal

eval _env (BoolE bool) = return $ BoolVal bool

eval _env (IntE int) = return $ IntVal (fromIntegral int)  -- TODO(pjanczyk): fix integer types

eval _env (StringE string) = return $ StringVal string

eval env (IdentifierE name) = lift $ fromMaybe NullVal <$> getVar env name

eval env (PrefixOpE op e) = do
    v <- eval env e
    case (op, v) of
        ("+", IntVal _)   -> return v
        ("-", IntVal int) -> return $ IntVal (-int)
        _                 -> throwE $ EvalException $
            "Prefix operator `" ++ op ++
            "` cannot be applied to the type `" ++ valType v ++
            "`"

eval env (InfixOpE op lhsE rhsE)
    | op == "=" = case lhsE of
        IdentifierE name -> do
            rhsV <- eval env rhsE
            lift $ setVar env name rhsV
            return rhsV
        _                -> throwE $ EvalException $
            "The left-hand side of the operator `=` must be an identifier"
    | isInPlaceOp op = case lhsE of
        IdentifierE name -> do
            lhsV <- lift $ fromMaybe NullVal <$> getVar env name
            rhsV <- eval env rhsE
            resV <- applyInfixOp (removeInPlaceness op) lhsV rhsV
            lift $ setVar env name resV
            return rhsV
        _                -> throwE $ EvalException $
            "The left-hand side of the operator `" ++ op ++
            "` must be an identifier"
    | otherwise = do
        lhsV <- eval env lhsE
        rhsV <- eval env rhsE
        applyInfixOp op lhsV rhsV
    where
        isInPlaceOp :: Op -> Bool
        isInPlaceOp op' = op' `elem` ["^=", "*=", "/=", "%=", "+=", "-="]

        removeInPlaceness :: Op -> Op
        removeInPlaceness op' = init op'   -- remove trailing '='

        applyInfixOp :: Op -> Val -> Val -> IOExceptEval Val
        applyInfixOp "^" (IntVal l) (IntVal r) = return $ IntVal (l ^ r)
        applyInfixOp "*" (IntVal l) (IntVal r) = return $ IntVal (l * r)
        applyInfixOp "/" (IntVal l) (IntVal r) = return $ IntVal (l `quot` r)
        applyInfixOp "%" (IntVal l) (IntVal r) = return $ IntVal (l `rem` r)
        applyInfixOp "+" (IntVal l) (IntVal r) = return $ IntVal (l + r)
        applyInfixOp "-" (IntVal l) (IntVal r) = return $ IntVal (l - r)
        applyInfixOp op'  l          r         = throwE $ EvalException $
            "Infix operator `" ++ op' ++
            "` cannot be applied to the types `" ++ valType l ++
            "` and `" ++ valType r ++
            "`"

eval env (CallE calleeE argsE) = do
    calleeV <- eval env calleeE
    argsV <- eval env `mapM` argsE
    case calleeV of
        NativeFuncVal func          -> func env argsV
        UserFuncVal params bodyExpr ->
            if length argsV /= length params
                then throwE $ EvalException $
                    "Invalid number of arguments. Expected " ++ show (length params) ++
                    " argument(s)"
                else do
                    newScope <- lift empty
                    lift $ (uncurry $ setVar newScope) `mapM_` (params `zip` argsV)
                    eval newScope bodyExpr
            
        _                           -> throwE $ EvalException $
            "Only a function can be called"

eval _env (SubscriptE _lhsE _rhsE) = undefined  -- TODO(pjanczyk)

eval env (BlockE stmts) = foldlM (\_ b -> eval env b) NullVal stmts

eval env (IfElseE condExpr thenExpr maybeElseExpr) = do
    condVal <- eval env condExpr
    case condVal of
        BoolVal True  -> eval env thenExpr
        BoolVal False -> case maybeElseExpr of
            Just elseExpr  -> eval env elseExpr
            Nothing        -> return NullVal
        _             -> throwE $ EvalException $
            "The condition in `if` expression must be of type `bool`"

eval env (WhileE condExpr bodyExpr) = do
    condVal <- eval env condExpr
    case condVal of
        BoolVal True  -> do
            eval env bodyExpr
            eval env (WhileE condExpr bodyExpr)
        BoolVal False -> return NullVal
        _             -> throwE $ EvalException $
            "The condition in `while` expression must be of type `bool`"

eval _env (FuncE params bodyExpr) = return $ UserFuncVal params bodyExpr
