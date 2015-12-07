module CSVParsers (
    Language
  , Dic
  , Flashcard
  , csvFCDic
  , csvFCIndex
  , Chapter
  , csvChapterIndex
  , csvChapterDic
  , csvChapterCards
  , toChapters
) where

import qualified Data.Map as M
import Data.List.Split (splitOn, splitWhen)


--

type Language = String

type Dic = M.Map Language String

data Flashcard = Flashcard {
    csvFCIndex :: Integer
  , csvFCDic :: Dic
} deriving Show

data Chapter = Chapter {
    csvChapterIndex :: Integer
  , csvChapterDic :: Dic
  , csvChapterCards :: [Flashcard]
} deriving Show




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
