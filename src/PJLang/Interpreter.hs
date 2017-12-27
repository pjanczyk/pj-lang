module PJLang.Interpreter (newEnv, evalExpr) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)
import Data.Maybe (fromMaybe)

import PJLang.Ast
import PJLang.Env
import qualified PJLang.StdLib as StdLib

newEnv :: IO Env
newEnv = do
    env <- empty
    setVar env "print" (NativeFuncVal StdLib.print)
    setVar env "printLine" (NativeFuncVal StdLib.printLine)
    return env

evalExpr :: Env -> Expr -> IOExceptEval Val

evalExpr _   NullE = return NullVal

evalExpr _   (BoolE bool) = return $ BoolVal bool

evalExpr _   (IntE int) = return $ IntVal (fromIntegral int)  -- TODO(pjanczyk): fix integer types

evalExpr _   (StringE string) = return $ StringVal string

evalExpr env (IdentifierE name) = lift $ fromMaybe NullVal <$> getVar env name

evalExpr env (PrefixOpE op expr) = do
    val <- evalExpr env expr
    case (op, val) of
        ("+", IntVal _)   -> return val
        ("-", IntVal int) -> return $ IntVal (-int)
        _                 -> throwE $ EvalException $
            "Prefix operator `" ++ op ++
            "` cannot be applied to the type `" ++ valType val ++
            "`"

evalExpr env (InfixOpE "=" lExpr rExpr) = case lExpr of
        IdentifierE name -> do
            val <- evalExpr env rExpr
            lift $ setVar env name val
            return NullVal
        _                -> throwE $ EvalException $
            "The left-hand side of the operator `=` must be an identifier"

evalExpr env (InfixOpE op lExpr rExpr) = do
    lVal <- evalExpr env lExpr
    rVal <- evalExpr env rExpr
    case (op, lVal, rVal) of
        ("^", IntVal lInt, IntVal rInt) -> return $ IntVal (lInt ^ rInt)
        ("*", IntVal lInt, IntVal rInt) -> return $ IntVal (lInt * rInt)
        ("/", IntVal lInt, IntVal rInt) -> return $ IntVal (lInt `quot` rInt)
        ("%", IntVal lInt, IntVal rInt) -> return $ IntVal (lInt `rem` rInt)
        ("+", IntVal lInt, IntVal rInt) -> return $ IntVal (lInt + rInt)
        ("-", IntVal lInt, IntVal rInt) -> return $ IntVal (lInt - rInt)
        _                               -> throwE $ EvalException $
            "Infix operator `" ++ op ++
            "` cannot be applied to the types `" ++ valType lVal ++
            "` and " ++ valType rVal ++
            "`"

evalExpr env (CallE calleeExpr argsExpr) = do
    calleeVal <- evalExpr env calleeExpr
    argsVal <- evalExpr env `mapM` argsExpr
    case calleeVal of
        NativeFuncVal func -> func env argsVal   
        _                  -> throwE $ EvalException $
            "Only a native function can be called"

evalExpr env (SubscriptE lExpr rExpr) = undefined  -- TODO(pjanczyk)

evalExpr env (BlockE stmts) = foldl (\a b -> a >> evalExpr env b) (return NullVal) stmts

evalExpr env (IfElseE condExpr thenExpr maybeElseExpr) = do
    condVal <- evalExpr env condExpr
    case condVal of
        BoolVal True  -> evalExpr env thenExpr
        BoolVal False -> case maybeElseExpr of
            Just elseExpr  -> evalExpr env elseExpr
            Nothing        -> return NullVal
        _             -> throwE $ EvalException $
            "The condition in `if` expression must be of type `bool`"

evalExpr env (WhileE condExpr bodyExpr) = do
    condVal <- evalExpr env condExpr
    case condVal of
        BoolVal True  -> do
            evalExpr env bodyExpr
            evalExpr env (WhileE condExpr bodyExpr)
        BoolVal False -> return NullVal
        _             -> throwE $ EvalException $
            "The condition in `while` expression must be of type `bool`"
