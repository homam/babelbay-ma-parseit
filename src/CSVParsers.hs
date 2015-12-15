{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module CSVParsers (
    Language
  , Dic
  , CSVFlashcard (..)
  , CSVChapter (..)
  , toCSVChapters
  , DicOf
  , CSVCourseIntro (..)
  , CSVCourseIntroFlashcard (..)
  , toCSVCourseIntro
  , CSVLanguage (..)
  , CSVCourseMeta (..)
) where

import qualified Data.Map as M
import Data.List.Split (splitOn, splitWhen)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Either
import Text.Read (readEither)


-- Course Contetent

type Language = String

type Dic = M.Map Language String

data CSVFlashcard = CSVFlashcard {
    csvFCIndex :: Integer
  , csvFCDic :: Dic
} deriving Show

data CSVChapter = CSVChapter {
    csvChapterIndex :: Integer
  , csvChapterDic :: Dic
  , csvChapterCards :: [CSVFlashcard]
} deriving Show


csvVectorToMatrix :: V.Vector [String] -> [[String]]
csvVectorToMatrix = V.toList

toCSVChapter :: [[String]] -> Integer -> CSVChapter
toCSVChapter matrix index = CSVChapter index (toDic $ drop 1 $ head matrix) (map toFlashcard $ drop 1 matrix)

toFlashcard :: [String] -> CSVFlashcard
toFlashcard list = CSVFlashcard (read $ head list) (toDic $ drop 1 list)

toDic :: [String] -> M.Map Language String
toDic list = M.fromList $ ["en","ar","fr","de","ru","es"] `zip` list

csvToChapters :: V.Vector [String] -> [CSVChapter]
csvToChapters file =
  let csv = csvVectorToMatrix file
      schapters = drop 1 $ splitWhen ((== "") . head) csv
  in zipWith toCSVChapter schapters [1 ..]

-- exported
toCSVChapters :: BL.ByteString -> Either String [CSVChapter]
toCSVChapters csvData = csvToChapters <$> decode NoHeader csvData



-- Descriptions


data CSVLanguage = Arabic | English | Spanish | German | French | Russian deriving (Read, Show, Eq, Ord, Enum)

data CSVCourseMeta = CSVCourseMeta {
    csvCourseMetaLanguage :: CSVLanguage
  , csvCourseMetaId1 :: Integer
  , csvCourseMetaId2 :: Integer
} deriving Show

type DicOf a = M.Map Language a

data CSVCourseIntro = CSVCourseIntro {
    csvCourseTitle1 :: Dic
  , csvCourseTitle2 :: Dic
  , csvCourseIntroTitle :: Dic
  , csvCourseIntroFC1 :: CSVCourseIntroFlashcard String
  , csvCourseIntroFC2 :: CSVCourseIntroFlashcard [String]
  , csvCourseMeta :: CSVCourseMeta
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

toCSVCourseMeta :: String -> Either String CSVCourseMeta
toCSVCourseMeta col = do
  (id1, id2, lang) <- readEither col :: Either String (Integer, Integer, CSVLanguage)
  return CSVCourseMeta {
      csvCourseMetaLanguage = lang
    , csvCourseMetaId1 = id1
    , csvCourseMetaId2 = id2
  }


csvVectorToCourseIntro :: [String] -> CSVCourseIntro -> V.Vector [String] -> CSVCourseIntro
csvVectorToCourseIntro = go where
  fieldMap v = M.fromList [
        ("1-title", \ val -> v {csvCourseTitle1 = val} ) -- courseTitle
      , ("2-title", \ val -> v {csvCourseTitle2 = val} )
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
  go langs = V.foldl $ \ intro (xs :: [String]) -> do
      let f = M.lookup (xs !! 1) (fieldMap intro)
      maybe intro ($ M.fromList $ langs `zip` drop 2 xs) f


-- exported
toCSVCourseIntro :: BL.ByteString -> Either String CSVCourseIntro
toCSVCourseIntro csvData = do
  vs <- V.drop 1 <$> decode NoHeader csvData
  meta <- toCSVCourseMeta $ V.head vs !! 1  -- first cell is a tuple (curseId, courseId, lang :: CSVLanguage)
  let langs = drop 2 (V.head vs) -- list of langs defined in the CSV
  let emptyCsvCourseIntro = CSVCourseIntro {
      csvCourseTitle1 = M.empty
    , csvCourseTitle2 = M.empty
    , csvCourseIntroTitle = M.empty
    , csvCourseIntroFC1 = emptyCsvCourseIntroFlashcard
    , csvCourseIntroFC2 = emptyCsvCourseIntroFlashcard
    , csvCourseMeta = meta
  }
  return $ csvVectorToCourseIntro langs emptyCsvCourseIntro vs
