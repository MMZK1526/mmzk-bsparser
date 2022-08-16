-- | A module for expression parsers.

{-# LANGUAGE TupleSections #-}

module MMZK.BSParser.Expr (BSAssoc(..), BSExprT(..), BSExpr, mkExpr) where

import           Control.Applicative
import           Control.Monad
import           Data.Functor.Identity
import           MMZK.BSParser.Parser (BSParserT)

data BSAssoc = ALeft | ARight | ANone
  deriving (Eq, Ord, Show)

data BSExprT e m a = BiOp BSAssoc (BSParserT e m (a -> a -> a))
                   | Pref (BSParserT e m (a -> a))
                   | Suff (BSParserT e m (a -> a))

type BSExpr e a = BSExprT e Identity a

_runExprs :: Monad m
          => [BSExprT e m a]
          -> ( [BSParserT e m (BSAssoc, a -> a -> a)]
             , [BSParserT e m (Int, a -> a)]
             , [BSParserT e m (Int, a -> a)] )
_runExprs = go 0
  where
    go _ []             = ([], [], [])
    go i (expr : exprs) = case expr of
      BiOp assoc p -> (fmap (assoc ,) p : biOps, prefs, suffs)
      Pref p       -> (biOps, fmap (i ,) p : prefs, suffs)
      Suff p       -> (biOps, prefs, fmap (i ,) p : suffs)
      where
        (biOps, prefs, suffs) = go (i + 1) exprs
{-# INLINE [2] _runExprs #-}

-- | Take a literal parser and a list of lists of expression parsers ordered
-- from the lowest to the highest precedence, producing a parser for expressions
-- with infix, prefix, and postfix operators of various precedence and
-- associativity.
--
-- For example, the following is a parser that serves as a calculator for integral arithmetics.
-- > calculator :: Parser Integer
-- > calculator = mkExpr (choice [ lexer $ L.signed L.digits
-- >                             , parens (lexer $ L.char '(') (lexer $ L.char ')')
-- >                                      calculator ])
-- >                             [ [ BiOp ALeft ((+) <$ (lexer $ L.char '+'))
-- >                               , BiOp ALeft ((-) <$ (lexer $ L.char '-')) ]
-- >                             , [ BiOp ALeft ((*) <$ (lexer $ L.char '*'))
-- >                               , BiOp ALeft (div <$ (lexer $ L.char '/')) ] ]
mkExpr :: Monad m => BSParserT e m a -> [[BSExprT e m a]] -> BSParserT e m a
mkExpr lit []                  = lit
mkExpr lit (exprs : exprTable) = do
  let (biOps, prefs, suffs) = _runExprs exprs
  let mkSubExpr             = do
        mPref <- optional $ msum prefs
        expr  <- mkExpr lit exprTable
        mSuff <- optional $ msum suffs
        return $ case (mPref, mSuff) of
          (Just (i, p), Just (j, s)) -> if i < j then p (s expr) else s (p expr)
          _                          -> maybe id snd (mPref <|> mSuff) expr
  expr <- mkSubExpr
  mOp  <- optional $ msum biOps
  case mOp of
    Nothing          -> pure expr
    Just (assoc, op) -> case assoc of
      ALeft  -> let goL partialOp = do
                      expr' <- mkSubExpr
                      mOp'  <- optional $ msum biOps
                      case mOp' of
                        Nothing           -> pure $ partialOp expr'
                        Just (ALeft, op') -> goL (op' $ partialOp expr')
                        _                 -> empty
                in  goL (op expr)
      ARight -> let goR = do
                      expr' <- mkSubExpr
                      mOp'  <- optional $ msum biOps
                      case mOp' of
                        Nothing            -> pure expr'
                        Just (ARight, op') -> op' expr' <$> goR
                        _                  -> empty
                in  op expr <$> goR
      ANone  -> op expr <$> do
        expr' <- mkSubExpr
        mOp'  <- optional $ msum biOps
        case mOp' of
          Nothing -> pure expr'
          _       -> empty
