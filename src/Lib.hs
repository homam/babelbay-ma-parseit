{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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
import Rand


import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Either



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
csvFilePath = "./final.csv"


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
  let questions = cards oneChapter
  g <- newStdGen
  let (cs, _) = runEval g $ mapM (toCFlashcard "es" "en" questions) $ cards $ head chapters
  putStrLn $ showTopElement $ collElement "cards" $ map cFlashcardToXml cs


data CFlashcard = CFlashcard {
      cflashCardIndex :: Integer
    , question :: String
    , answer :: String
    , quiz :: [String]
  } deriving Show

data CChapter = CChapter {
      cchapterIndex :: Integer
    , ctitle :: String
    , ccards :: [CFlashcard]
}

data CCourse = CCourse {
      courseId :: Integer
    , ccourseKey :: String
    , ccourseIntro :: CCourseIntro
}

data CCourseIntro = CCourseIntro {
      ccourseIntroTitle :: String
    , ccourseIntroIntroTitle :: String
    , ccourseIntroFCs :: [CCourseIntroFC]
} deriving Show

data CCourseIntroFC = CCourseIntroFC {
      ccourseIntroFCQuestion :: String
    , ccourseIntroFCShortAnswer :: String
    , ccourseIntroFCLongAnswer :: [String]
} deriving Show



toCFlashcard :: (RandomGen g, S.MonadState g m) => Language -> Language -> [Flashcard] -> Flashcard -> m CFlashcard
toCFlashcard native target questions (Flashcard index dic) = do
  let answer = dic M.! native
      question = dic M.! target
  quiz <- (question :) . map ((M.! target) . fcDic) . take 3 <$> randomize (filter ((/= index) . cardIndex) questions)
  return CFlashcard {cflashCardIndex = index, question = question, answer = answer, quiz = quiz}


toCChapter :: (RandomGen g, S.MonadState g m) => Language -> Language -> Chapter -> m CChapter
toCChapter = undefined


---


data CourseIntroFlashcard = CourseIntroFlashcard {
      ciFCQuestion :: Dic
    , ciFcShortAns :: Dic
    , ciFCLongAns  :: Dic
} deriving Show


duck2 :: IO ()
duck2 = do
  csvData <- BL.readFile "./content/en-Table 1.csv"
  let xs = go CCourseIntro {ccourseIntroTitle = "", ccourseIntroIntroTitle = "", ccourseIntroFCs = []} (decode NoHeader csvData)
  write xs
  where
    write (Left err) = putStrLn err
    write (Right xs) = print xs -- writeFile "out.txt" $ foldl1 (\a b -> a ++ "\n" ++ b) xs

    go _ (Left err) = Left err
    go intro (Right v) = V.foldM (\ intro' (xs :: [String]) -> do
        -- print xs
        -- writeFile "out.txt" $ foldl1 (\a b -> a ++ "\n" ++ b) xs
        if "1-title" == xs !! 1 then
          return $ intro' {ccourseIntroTitle = xs !! 2}
        else
          return intro' -- $ xs ++ ys
        ) intro v



---

xmlDOM :: String -> String
xmlDOM txt = showTopElement $ Element
    (unqual "root")
    []
    [ Elem $ Element
      (unqual "element")
      []
      [Text $ CData CDataText txt Nothing]
      Nothing
    ]
    Nothing

textElement :: String -> String -> Element
textElement name content = Element
  (unqual name)
  []
  [
    Text $ CData CDataText content Nothing
  ]
  Nothing

collElement :: String -> [Element] -> Element
collElement name children = Element
  (unqual name)
  []
  (map Elem children)
  Nothing

cFlashcardToXml :: CFlashcard -> Element
cFlashcardToXml card = Element
  (unqual "flashCard")
  []
  [
      Elem $ textElement "FCQuestion" (question card)
    , Elem $ textElement "FCAnswer" (question card)
  ]
  Nothing
