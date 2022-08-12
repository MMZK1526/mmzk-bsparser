-- | A module for expression parsers.

{-# LANGUAGE TupleSections #-}

module MMZK.BSParser.Expr 
  (BSAssoc(..), BSBiOpT(..), BSExprsT(..), BSBiOp, BSExprs, mkExpr) where

import           Data.Functor.Identity
import           MMZK.BSParser.Parser

data BSAssoc = ALeft | ARight | ANone
  deriving (Eq, Ord, Show)

data BSBiOpT e m a = BiOp BSAssoc (BSParserT e m (a -> a -> a))

type BSBiOp e a = BSBiOpT e Identity a

data BSExprsT e m a = EBiOp [BSBiOpT e m a]

type BSExprs e a = BSExprsT e Identity a

runBiOp :: Monad m => BSBiOpT e m a -> BSParserT e m (BSAssoc, a -> a -> a)
runBiOp (BiOp assoc p) = (assoc ,) <$> p
{-# INLINE [2] runBiOp #-}

mkExpr :: Monad m => BSParserT e m a -> [BSExprsT e m a] -> BSParserT e m a
mkExpr lit []                  = lit
mkExpr lit (exprs : exprTable) = do
  let mkSubExpr = mkExpr lit exprTable
  case exprs of
    EBiOp opPs -> do
      expr   <- mkSubExpr
      mOp    <- optional . choice $ runBiOp <$> opPs
      case mOp of
        Nothing          -> pure expr
        Just (assoc, op) -> case assoc of
          ALeft  -> let goL partialOp = do
                          expr' <- mkSubExpr
                          mOp'  <- optional . choice $ runBiOp <$> opPs
                          case mOp' of
                            Nothing           -> pure $ partialOp expr'
                            Just (ALeft, op') -> goL (op' $ partialOp expr')
                            _                 -> empty
                    in  goL (op expr)
          ARight -> let goR = do
                          expr' <- mkSubExpr
                          mOp'  <- optional . choice $ runBiOp <$> opPs
                          case mOp' of
                            Nothing            -> pure expr'
                            Just (ARight, op') -> op' expr' <$> goR
                            _                  -> empty
                    in  op expr <$> goR
          ANone  -> op expr <$> mkSubExpr
