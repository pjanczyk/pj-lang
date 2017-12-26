module PJLang.Interpreter where

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
    return env

evalExpr :: Env -> Expr -> IOExceptEval Val
evalExpr _   (NumLiteralE val)   = return $ IntVal (fromIntegral val)  -- TODO(pjanczyk): fix integer types
evalExpr env (IdentifierE name)  = lift $ fromMaybe NullVal <$> getVar env name
evalExpr env (ParensE expr)      = evalExpr env expr
evalExpr env (PrefixOpE op expr) = do
    val <- evalExpr env expr
    case (op, val) of
        (UnaryMinus, IntVal int) -> return $ IntVal (-int)
        (UnaryPlus,  IntVal _)   -> return val
        _                        -> throwE $ EvalException $
                                         "Operator " ++ unaryOpToString op ++ " cannot be applied to type" ++ valType val
evalExpr env (BinaryOpE op lExpr rExpr) = do
    lVal <- evalExpr env lExpr
    rVal <- evalExpr env rExpr
    case (op, lVal, rVal) of
        (BinaryPow, IntVal lInt, IntVal rInt) -> return $ IntVal (lInt ^ rInt)
        (BinaryMul, IntVal lInt, IntVal rInt) -> return $ IntVal (lInt * rInt)
        (BinaryDiv, IntVal lInt, IntVal rInt) -> return $ IntVal (lInt `quot` rInt)
        (BinaryMod, IntVal lInt, IntVal rInt) -> return $ IntVal (lInt `rem` rInt)
        (BinaryAdd, IntVal lInt, IntVal rInt) -> return $ IntVal (lInt + rInt)
        (BinarySub, IntVal lInt, IntVal rInt) -> return $ IntVal (lInt - rInt)
        _                                     -> throwE $ EvalException $
                                                      "Operator " ++ binaryOpToString op ++
                                                      " cannot be applied to types " ++ valType lVal ++
                                                      " and " ++ valType rVal
evalExpr env (CallE calleeExpr argsExpr) = do
    calleeVal <- evalExpr env calleeExpr
    argsVal <- evalExpr env `mapM` argsExpr
    case calleeVal of
        NativeFuncVal func -> func env argsVal   
        _                  -> throwE $ EvalException $
                                  "Only a native function can be called"

-- evalExpr env (SubscriptE lExpr rExpr)  -- TODO(pjanczyk)
evalExpr env (AssignE lExpr rExpr) = case lExpr of
        IdentifierE name -> do
            val <- evalExpr env rExpr
            lift $ setVar env name val
            return NullVal
        _                -> throwE $ EvalException $
                                "Left-hand side of operator = must be an identifier"
