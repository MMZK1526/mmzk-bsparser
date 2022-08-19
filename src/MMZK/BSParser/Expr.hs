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
-- The first argument parses literals of the expression, while the second
-- describes how to parse for the various operators.
--
-- The former is a "BSParserT". If the expression has grammatic rules that
-- overrides operator precedency (such as parenthesis), it should be treated as
-- literal rules.
--
-- The latter is a list of list of "BSExprT", each contains a "BSParserT" that
-- produces either an infix operator (a -> a -> a) or a prefix/postfix operator
-- (a -> a). Operators listed in the same minor list have the same level of
-- precedence, while the elements of the major list represent precedences from
-- lowest to highest.
--
-- Infix operators also come with an associativity rule, which can be
-- left-associative (@ALeft@), right-associative (@ARight@), or non-associative
-- (@ANone@). Multiple operators of the same precedence level but with different
-- associativity cannot appear together without explicit ordering, which depends
-- on the grammar of the expression but is usually specified by parenthesis.
-- Similarly, more than one non-associative operators of the same precedence
-- level cannot appear together without explicit ordering.
--
-- When prefix and postfix operators apply to the same literal, if they have
-- the same precedence, the order of application favours the one that appears
-- earlier in their minor list.
--
-- When reaching a combination of a prefix operator and a literal, this parser
-- eagerly parses for the prefix operator. For example, if "-" is both a numeral
-- sign and an operator for negation, the parser will consider "-9" as the
-- application of the negation operator on the literal "9". Similarly, it
-- eagerly parses for literals before attempting to parse postfix operators.
--
-- The following example is a parser that serves as a calculator for integral
-- arithmetics:
--
-- > calculator :: Parser Integer
-- > calculator = mkExpr (choice [ lexer $ L.signed L.digits
-- >                             , parens (lexer $ L.char '(') (lexer $ L.char ')')
-- >                                      calculator ])
-- >                             [ [ infixL (+) (lexer $ L.char '+')
-- >                               , infixL (-) (lexer $ L.char '-') ]
-- >                             , [ infixL (*) (lexer $ L.char '*')
-- >                               , infixL div (lexer $ L.char '/') ]
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
