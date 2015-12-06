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



toCFlashcard :: (RandomGen g, S.MonadState g m) => Language -> Language -> [Flashcard] -> Flashcard -> m CFlashcard
toCFlashcard native target questions (Flashcard index dic) = do
  let answer = dic M.! native
      question = dic M.! target
  quiz <- (question :) . map ((M.! target) . fcDic) . take 3 <$> randomize (filter ((/= index) . cardIndex) questions)
  return CFlashcard {cflashCardIndex = index, question = question, answer = answer, quiz = quiz}


toCChapter :: (RandomGen g, S.MonadState g m) => Language -> Language -> Chapter -> m CChapter
toCChapter = undefined


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
