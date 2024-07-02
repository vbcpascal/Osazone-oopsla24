module Utils.Functions where

import Control.Monad
import Data.Bitraversable (Bitraversable, bimapM)
import Data.Map (Map, filterWithKey, fromList, toList)

-- Monadic

mif :: Monad m => m Bool -> m a -> m a -> m a
mif cond tbranch fbranch = do
  b <- cond
  if b then tbranch else fbranch

mwhen :: Monad m => m Bool -> m () -> m ()
mwhen cond branch = mif cond branch $ return ()

assert :: Applicative f => String -> Bool -> f ()
assert msg cond = unless cond (error msg)

-- Pairs

firstM :: (Bitraversable t, Applicative f) => (a -> f c) -> t a b -> f (t c b)
firstM f = bimapM f pure

secondM :: (Bitraversable t, Applicative f) => (b -> f d) -> t a b -> f (t a d)
secondM = bimapM pure

mapMapM :: (Ord k, Monad m) => (a -> m b) -> Map k a -> m (Map k b)
mapMapM f = fmap fromList . mapM (secondM f) . toList

fst3 :: (t -> a) -> (t, b, c) -> (a, b, c)
fst3 f (a, b, c) = (f a, b, c)

-- Lists

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter list =
  let (first, rest) = break (== delimiter) list
  in first : case rest of
        []   -> []
        x:xs -> splitOn delimiter xs

-- Maps

filterKey :: Ord k => (k -> Bool) -> Map k a -> Map k a
filterKey f = filterWithKey (curry (f . fst))

-- Strings

beginWith :: String -> String -> Bool
beginWith _ [] = True
beginWith (x : xs) (y : ys) | x == y = xs `beginWith` ys
beginWith _ _ = False
