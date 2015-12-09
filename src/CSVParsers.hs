{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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
  , DicOf
  , CSVCourseIntro
  , csvCourseTitle
  , csvCourseIntroTitle
  , csvCourseIntroFC1
  , csvCourseIntroFC2
  , CSVCourseIntroFlashcard
  , csvCiFcQuestion
  , csvCiFcShortAns
  , csvCiFcLongAns
  , toCourseIntro
) where

import qualified Data.Map as M
import Data.List.Split (splitOn, splitWhen)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Either




-- Course Contetent

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


-- Descriptions


type DicOf a = M.Map Language a

data CSVCourseIntro = CSVCourseIntro {
    csvCourseTitle :: Dic
  , csvCourseIntroTitle :: Dic
  , csvCourseIntroFC1 :: CSVCourseIntroFlashcard String
  , csvCourseIntroFC2 :: CSVCourseIntroFlashcard [String]
} deriving Show

data CSVCourseIntroFlashcard a = CSVCourseIntroFlashcard {
    csvCiFcQuestion :: Dic
  , csvCiFcShortAns :: Dic
  , csvCiFcLongAns  :: DicOf a
} deriving Show

emptyCsvCourseIntroFlashcard = CSVCourseIntroFlashcard {
    csvCiFcQuestion = M.empty
  , csvCiFcShortAns = M.empty
  , csvCiFcLongAns = M.empty
}

emptyCsvCourseIntro = CSVCourseIntro {
    csvCourseTitle = M.empty
  , csvCourseIntroTitle = M.empty
  , csvCourseIntroFC1 = emptyCsvCourseIntroFlashcard
  , csvCourseIntroFC2 = emptyCsvCourseIntroFlashcard
}


csvVectorToCourseIntro :: V.Vector [String] -> CSVCourseIntro
csvVectorToCourseIntro = go emptyCsvCourseIntro where
  fieldMap v = M.fromList [
        ("1-title", \ val -> v {csvCourseTitle = val} ) -- courseTitle
      , ("intro_title", \ val -> v {csvCourseIntroTitle = val})
      , ("intro_fc-1_q", \ val -> updateFC1 $ \ fc -> fc {csvCiFcQuestion = val})
      , ("intro_fc-1_a_short", \ val -> updateFC1 $ \ fc -> fc {csvCiFcShortAns = val})
      , ("intro_fc-1_a_long", \ val -> updateFC1 $ \ fc -> fc {csvCiFcLongAns = val})
      , ("intro_fc-2_q", \ val -> updateFC2 $ \ fc -> fc {csvCiFcQuestion = val})
      , ("intro_fc-2_a_short", \ val -> updateFC2 $ \ fc -> fc {csvCiFcShortAns = val})
      , ("intro_fc-2_a_long", \ val -> updateFC2 $ \ fc -> fc {csvCiFcLongAns = M.map (map (T.unpack . T.strip . T.pack) . filter ((>1) . length) . splitOn "*") val })
    ] where
      updateFC1 f =
        let fc = csvCourseIntroFC1 v
            fc' = f fc
        in
        v { csvCourseIntroFC1 = fc' }

      updateFC2 f =
        let fc = csvCourseIntroFC2 v
            fc' = f fc
        in
        v { csvCourseIntroFC2 = fc' }
  go = V.foldl $ \ intro (xs :: [String]) -> do
      let f = M.lookup (xs !! 1) (fieldMap intro)
      maybe intro ($ M.fromList $ ["en","es","fr","de","ru","ar"] `zip` drop 2 xs) f


toCourseIntro :: BL.ByteString -> Either String CSVCourseIntro
toCourseIntro csvData = csvVectorToCourseIntro <$> decode NoHeader csvData
