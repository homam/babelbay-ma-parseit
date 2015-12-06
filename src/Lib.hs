module Lib
    (
      someFunc,
      csvFilePath,
      parseCSV,
      toChapters,
      Flashcard,
      Chapter,
      Language,
      Dic
    ) where

import Prelude hiding (words)
import qualified Data.Map as M
import Data.List.Split (splitOn, splitWhen)

import Text.XML.Light
import System.Random
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Control.Monad.State as S

import qualified Data.List as L

import Control.Monad.Identity
import Data.Maybe



type Language = String
type Dic = M.Map Language String
data Flashcard = Flashcard {
      cardIndex :: Integer
    , fcDic:: Dic
  } deriving Show
data Chapter = Chapter {
      chapterIndex :: Integer
    , dic :: Dic
    , cards :: [Flashcard]
  } deriving Show



csvFilePath :: FilePath
csvFilePath = "/Users/homam/Downloads/babelbay/final.csv"


parseCSV :: String -> [[String]]
parseCSV = map (splitOn ",") . lines


toChapter :: [[String]] -> Integer -> Chapter
toChapter matrix index = Chapter index (toDic $ drop 1 $ head matrix) (map toFlashcard $ drop 1 matrix)

toFlashcard :: [String] -> Flashcard
toFlashcard list = Flashcard (read $ head list) (toDic $ drop 1 list)

toDic :: [String] -> M.Map Language String
toDic list = M.fromList $ ["en","ar","fr","de","ru","es"] `zip` list

-- @file@ is the full content of a CSV file
toChapters :: String -> [Chapter]
toChapters file =
  let csv = parseCSV file
      schapters = drop 1 $ splitWhen ((== "") . head) csv
  in zipWith toChapter schapters [1 ..]

someFunc :: IO ()
someFunc = do
  file <- readFile csvFilePath
  let chapters = toChapters file
  let oneChapter = head chapters
  let questions = []
  print $ map (toCFlashcard "es" "en" questions) $ cards $ head chapters


-- rand :: (Random a, RandomGen g, S.MonadState g m) => a -> a -> m a
-- rand :: (Num b, RandomGen s, Random b, S.MonadState s m) => m b
-- rand lo hi = do
--   g <- S.get
--   let  (i, g') = randomR (lo, hi) g
--   S.put g
--   return i

-- randomFlashcards :: StdGen -> Chapter -> Flashcard -> (StdGen, [Flashcard])
-- randomFlashcards g chapter fc =
--   let fcindex = cardIndex fc
--       cards' = filter ((/= fcindex) . cardIndex) $ cards chapter
--   in _
--





data CFlashcard = CFlashcard {
      question :: String
    , answer :: String
  } deriving Show



toCFlashcard :: Language -> Language -> [String] -> Flashcard -> CFlashcard
toCFlashcard native target questions (Flashcard index dic) =
  let answer = dic M.! native
      question = dic M.! target
  in  CFlashcard {question = question, answer = answer}



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
