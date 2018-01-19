module PJLang.Interpreter (ExecResult(..), newEnv, evalExpr, evalCode) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE, runExceptT)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldlM)
import Text.Parsec.Error (ParseError)

import PJLang.Ast (Expr(..), Op)
import PJLang.Env
import PJLang.Parser (buildAst)
import qualified PJLang.StdLib as StdLib

-- | Creates a default environment that contains names from the standard library
newEnv :: IO Env
newEnv = do
    globalScope <- newScope
    let env = Env [globalScope]
    setVar env `mapM_` builtins
    return env
    where
        builtins = [
                ("print",     NativeFuncVal StdLib.print),
                ("printLine", NativeFuncVal StdLib.printLine),
                ("readLine",  NativeFuncVal StdLib.readLine),
                ("toString",  NativeFuncVal StdLib.toString)
            ]

-- | Result of 'evalCode'
data ExecResult
    = ExecSuccess Val             -- ^ Success
    | ExecParseError ParseError   -- ^ Parse error
    | ExecEvalError EvalException -- ^ Runtime exception
            deriving (Show)

-- | Parses and evaluates a code
evalCode :: Env -> String -> IO ExecResult
evalCode env code = do
    case buildAst code of
        Left parseError -> return $ ExecParseError parseError
        Right ast       -> do
            result <- runExceptT $ evalExpr env ast
            return $ case result of
                Left evalException -> ExecEvalError evalException
                Right val          -> ExecSuccess val

-- | Evaluates an expression (AST)
evalExpr :: Env -> Expr -> IOExceptEval Val

evalExpr _env NullE = return NullVal

evalExpr _env (BoolE bool) = return $ BoolVal bool

evalExpr _env (IntE int) = return $ IntVal (fromIntegral int)  -- TODO(pjanczyk): fix integer types

evalExpr _env (StringE string) = return $ StringVal string

evalExpr env (IdentifierE name) = lift $ fromMaybe NullVal <$> getVar env name

evalExpr env (PrefixOpE op e) = do
    v <- evalExpr env e
    case (op, v) of
        ("+", IntVal _)   -> return v
        ("-", IntVal int) -> return $ IntVal (-int)
        ("!", BoolVal b)  -> return $ BoolVal (not b)
        _                 -> throwE $ EvalException $
            "Prefix operator `" ++ op ++
            "` cannot be applied to the type `" ++ valType v ++
            "`"

evalExpr env (InfixOpE op lhsE rhsE)
    | op == "=" = case lhsE of
        IdentifierE name -> do
            rhsV <- evalExpr env rhsE
            lift $ setVar env (name, rhsV)
            return rhsV
        _                -> throwE $ EvalException $
            "The left-hand side of the operator `=` must be an identifier"
    | isInPlaceOp op = case lhsE of
        IdentifierE name -> do
            lhsV <- lift $ fromMaybe NullVal <$> getVar env name
            rhsV <- evalExpr env rhsE
            resV <- applyInfixOp (removeInPlaceness op) lhsV rhsV
            lift $ setVar env (name, resV)
            return rhsV
        _                -> throwE $ EvalException $
            "The left-hand side of the operator `" ++ op ++
            "` must be an identifier"
    | op == "&&" = do
        lhsV <- evalExpr env lhsE
        rhsV <- if lhsV == BoolVal False
                    then return $ BoolVal False
                    else evalExpr env rhsE
        applyInfixOp op lhsV rhsV
    | op == "||" = do
        lhsV <- evalExpr env lhsE
        rhsV <- if lhsV == BoolVal True
                    then return $ BoolVal True
                    else evalExpr env rhsE
        applyInfixOp op lhsV rhsV
    | otherwise = do
        lhsV <- evalExpr env lhsE
        rhsV <- evalExpr env rhsE
        applyInfixOp op lhsV rhsV
    where
        isInPlaceOp :: Op -> Bool
        isInPlaceOp op' = op' `elem` ["^=", "*=", "/=", "%=", "+=", "-="]

        removeInPlaceness :: Op -> Op
        removeInPlaceness op' = init op'   -- remove trailing '='

        applyInfixOp :: Op -> Val -> Val -> IOExceptEval Val
        applyInfixOp "^"  (IntVal l)    (IntVal r)    = return $ IntVal (l ^ r)
        applyInfixOp "*"  (IntVal l)    (IntVal r)    = return $ IntVal (l * r)
        applyInfixOp "/"  (IntVal l)    (IntVal r)    = return $ IntVal (l `quot` r)
        applyInfixOp "%"  (IntVal l)    (IntVal r)    = return $ IntVal (l `rem` r)
        applyInfixOp "+"  (IntVal l)    (IntVal r)    = return $ IntVal (l + r)
        applyInfixOp "+"  (StringVal l) (StringVal r) = return $ StringVal (l ++ r)
        applyInfixOp "-"  (IntVal l)    (IntVal r)    = return $ IntVal (l - r)
        applyInfixOp "&&" (BoolVal l)   (BoolVal r)   = return $ BoolVal (l && r)
        applyInfixOp "||" (BoolVal l)   (BoolVal r)   = return $ BoolVal (l || r)
        applyInfixOp "=="  l             r            = return $ BoolVal (valsEq l r)
        applyInfixOp "!="  l             r            = return $ BoolVal (not (valsEq l r))
        applyInfixOp "<"  (IntVal l)    (IntVal r)    = return $ BoolVal (l < r)
        applyInfixOp ">"  (IntVal l)    (IntVal r)    = return $ BoolVal (l > r)
        applyInfixOp "<=" (IntVal l)    (IntVal r)    = return $ BoolVal (l <= r)
        applyInfixOp ">=" (IntVal l)    (IntVal r)    = return $ BoolVal (l >= r)
        applyInfixOp op'   l             r            = throwE $ EvalException $
            "Infix operator `" ++ op' ++
            "` cannot be applied to the types `" ++ valType l ++
            "` and `" ++ valType r ++
            "`"

        valsEq :: Val -> Val -> Bool
        valsEq  NullVal       NullVal      = True
        valsEq (BoolVal a)   (BoolVal b)   = a == b
        valsEq (IntVal a)    (IntVal b)    = a == b
        valsEq (StringVal a) (StringVal b) = a == b
        valsEq  _             _            = False   -- TODO(pjanczyk): NativeFuncVal, LambdaVal

evalExpr env (CallE calleeE argsE) = do
    calleeV <- evalExpr env calleeE
    argsV <- evalExpr env `mapM` argsE
    case calleeV of
        NativeFuncVal func -> func env argsV
        LambdaVal closure params bodyExpr -> do
            when (length argsV /= length params) $ throwE $ EvalException $
                    "Invalid number of arguments. Expected " ++ show (length params) ++
                    " argument(s)"
            innerScope <- lift newScope
            let env' = Env (innerScope:closure)
            lift $ scopeSetVar innerScope `mapM_` (params `zip` argsV)
            evalExpr env' bodyExpr
        _ -> throwE $ EvalException $
            "Only a function can be called"

evalExpr _env (SubscriptE _lhsE _rhsE) = undefined  -- TODO(pjanczyk)

evalExpr env (BlockE stmts) = foldlM (\_ b -> evalExpr env b) NullVal stmts

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

evalExpr (Env scopes) (LambdaE params bodyExpr) = return $ LambdaVal scopes params bodyExpr
