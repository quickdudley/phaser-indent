{-# LANGUAGE RankNTypes,ImpredicativeTypes,NoMonomorphismRestriction #-}
module Codec.Phaser.Indent (
  IndentPhase,
  liftPhase,
  runIndentPhase,
  currentIndent,
  blockWith,
  space
 ) where

import Data.Char

import Control.Applicative
import Control.Monad

import Unsafe.Coerce -- Useful here only because ImpredicativeTypes is
  -- imperfectly implemented in GHC

import Codec.Phaser.Core
import Codec.Phaser.Common(Position(..))

-- | A type for indentation aware parsers
newtype IndentPhase a = IndentPhase (
  forall o . Phase Position Char o () -> Phase Position Char o a
 )

instance Functor IndentPhase where
  fmap f (IndentPhase i) = IndentPhase ((fmap . fmap) f i)

instance Applicative IndentPhase where
  pure a = IndentPhase (\_ -> pure a)
  IndentPhase f <*> IndentPhase a = IndentPhase (\i -> f i <*> a i)
  IndentPhase a *> IndentPhase b = IndentPhase (\i -> a i *> b i)
  IndentPhase a <* IndentPhase b = IndentPhase (\i -> a i <* b i)

instance Monad IndentPhase where
  return = pure
  fail e = IndentPhase (\_ -> fail e)
  IndentPhase a >>= f = IndentPhase (\i -> a i >>= \a' -> let
    IndentPhase b = f a'
    in b i
   )
  (>>) = (*>)

instance Alternative IndentPhase where
  empty = IndentPhase (\_ -> empty)
  IndentPhase a <|> IndentPhase b = IndentPhase (\i -> a i <|> b i)

instance MonadPlus IndentPhase where
  mzero = empty
  mplus = (<|>)

-- | Runs a 'Phase' within an 'IndentPhase'
liftPhase :: (forall o . Phase Position Char o a) -> IndentPhase a
liftPhase p = IndentPhase (\_ -> p)

-- | Turns an 'IndentPhase' into a 'Phase'
runIndentPhase :: IndentPhase a -> Phase Position Char o a
runIndentPhase (IndentPhase a) = a (return ())

-- | Gets a 'Phase' which consumes the indentation of the current block
currentIndent :: IndentPhase (forall o . Phase Position Char o ())
currentIndent = IndentPhase (\i -> pure (unsafeCoerce i))

-- | Runs the first argument, then uses the returned 'Phase' to consume the
-- indentation for the second argument.
blockWith :: (IndentPhase (forall o . Phase Position Char o ())) ->
  IndentPhase a -> IndentPhase a
blockWith r (IndentPhase b) = r >>= \i -> IndentPhase (\_ -> b i)

-- | Consume at least one whitespace character, enforcing the current
-- indentation rule if a newline is found.
space :: IndentPhase ()
space = IndentPhase (\i -> let
  go = get >>= \c -> case c of
    '\n' -> (i *> return ()) <|> line
    _ | isSpace c -> go <|> return ()
      | otherwise -> empty
  line = (i *> go) <|>
    (munch ((&&) <$> isSpace <*> (/= '\n')) *> char '\n' *> line)
  in fail "Expecting whitespace" <|> go
 )
