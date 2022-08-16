-- | A module for expression parsers.

{-# LANGUAGE TupleSections #-}

module MMZK.BSParser.Expr
  ( BSAssoc(..), BSExprT(..), BSExpr, mkExpr, infixL, infixN, infixR, prefix
  , postfix ) where

import           Control.Applicative
import           Control.Monad
import           Data.Functor.Identity
import           MMZK.BSParser.Parser (BSParserT)

data BSAssoc = ALeft | ARight | ANone
  deriving (Eq, Ord, Show)

data BSExprT e m a = BiOp BSAssoc (BSParserT e m (a -> a -> a))
                   | Pref (BSParserT e m (a -> a))
                   | Post (BSParserT e m (a -> a))

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
      BiOp assoc p -> (fmap (assoc ,) p : biOps, prefs, posts)
      Pref p       -> (biOps, fmap (i ,) p : prefs, posts)
      Post p       -> (biOps, prefs, fmap (i ,) p : posts)
      where
        (biOps, prefs, posts) = go (i + 1) exprs
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
  let (biOps, prefs, posts) = _runExprs exprs
  let mkSubExpr             = do
        mPref <- optional $ msum prefs
        expr  <- mkExpr lit exprTable
        mPost <- optional $ msum posts
        return $ case (mPref, mPost) of
          (Just (i, p), Just (j, s)) -> if i < j then p (s expr) else s (p expr)
          _                          -> maybe id snd (mPref <|> mPost) expr
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

infixL :: Monad m => (a -> a -> a) -> BSParserT e m b -> BSExprT e m a
infixL = (BiOp ALeft .) . (<$)
{-# INLINE [2] infixL #-}

infixN :: Monad m => (a -> a -> a) -> BSParserT e m b -> BSExprT e m a
infixN = (BiOp ANone .) . (<$)
{-# INLINE [2] infixN #-}

infixR :: Monad m => (a -> a -> a) -> BSParserT e m b -> BSExprT e m a
infixR = (BiOp ARight .) . (<$)
{-# INLINE [2] infixR #-}

prefix :: Monad m => (a -> a) -> BSParserT e m b -> BSExprT e m a
prefix = (Pref .) . (<$)
{-# INLINE [2] prefix #-}

postfix :: Monad m => (a -> a) -> BSParserT e m b -> BSExprT e m a
postfix = (Post .) . (<$)
{-# INLINE [2] postfix #-}
