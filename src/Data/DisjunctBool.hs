module Data.DisjunctBool where

import Data.Maybe (Maybe (Just, Nothing))
import Prelude

-- | Enable a third value, to indicate both
--   Both is indiciated by Nothing.
--  The dual of this is that the third value is non.
newtype DisjunctBool = DisjunctBool (Maybe Bool)
  deriving (Show, Eq)

-- ord is nothing at the top, then true then false
-- todo - use Ord instead, where -1 is false, +1 is true and 0 is both
instance Semigroup DisjunctBool where
  (<>) (DisjunctBool x) (DisjunctBool y) = DisjunctBool $ case (x, y) of
    (Just x, Just y) -> case (x, y) of
      (True, False) -> Just x
      (False, True) -> Just y
      _ -> Nothing
    (Just x, Nothing) -> Just x
    (Nothing, Just y) -> Just y
    (Nothing, Nothing) -> Nothing

instance Monoid DisjunctBool where
  mempty = DisjunctBool Nothing

-- false, true, both
disjunctBool :: a -> a -> a -> DisjunctBool -> a
disjunctBool false true both x = case x of
  DisjunctBool (Just False) -> false
  DisjunctBool (Just True) -> true
  DisjunctBool Nothing -> both
