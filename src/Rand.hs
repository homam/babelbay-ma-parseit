module Rand
    (
    randomize,
    runEval,
    Eval
) where

import System.Random
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Control.Monad.State as S

import qualified Data.List as L

import Control.Monad.Identity
import Data.Maybe


randomize :: (RandomGen g, S.MonadState g m) => [b] -> m [b]
randomize bs = do
  rs <- mrand 10 (1::Integer) (10::Integer)
  return $ map fst $ L.sortOn snd (bs `zip` rs)


type Eval a g =  StateT (g Identity) a

runEval :: (RandomGen g) => g -> StateT g Identity a -> (a, g)
runEval g f = runIdentity $ runStateT f g

rand :: (RandomGen g, Random a, S.MonadState g m) => a -> a -> m a
rand lo hi = do
  g <- S.get
  let (i, g') = randomR (lo, hi) g
  S.put g'
  return i

mrand :: (RandomGen g, Random a, S.MonadState g m) => Int -> a -> a -> m [a]
mrand howMany lo hi = replicateM howMany $ rand lo hi

duck :: IO ()
duck = do
    g <- newStdGen
    print $ runEval g (mrand 10 (1::Integer) (10::Integer))

--

-- runEval' :: (RandomGen g) => g -> StateT g Maybe a -> (a, g)
-- runEval' g f = fromJust $ runStateT f g

-- rand :: (RandomGen g, Random a, S.MonadState g m) => a -> a -> m a
-- rand lo hi = do
--   g <- S.get
--   let (i, g') = randomR (lo, hi) g
--   S.put g'
--   return i
--
-- mrand :: (RandomGen g, Random a, S.MonadState g m) => a -> a -> m [a]
-- mrand lo hi = replicateM 10 $ rand lo hi
