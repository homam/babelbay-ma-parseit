{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Lib (
    someFunc
  , csvFilePath
) where

import Prelude hiding (words)
import qualified Data.Map as M


import Text.XML.Light
import System.Random
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Control.Monad.State as S
import Rand
import CSVParsers


import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Either



csvFilePath :: FilePath
csvFilePath = "./final.csv"


someFunc :: IO ()
someFunc = do
  file <- readFile csvFilePath
  let chapters = toChapters file
  let oneChapter = head chapters
  let questions = csvChapterCards oneChapter
  g <- newStdGen
  let (cs, _) = runEval g $ mapM (toCFlashcard "es" "en" questions) $ csvChapterCards $ head chapters
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
toCFlashcard native target questions flashcard = do
  let dic = csvFCDic flashcard
      index = csvFCIndex flashcard
      answer = dic M.! native
      question = dic M.! target
  quiz <- (question :) . map ((M.! target) . csvFCDic) . take 3 <$> randomize (filter ((/= index) . csvFCIndex) questions)
  return CFlashcard {cflashCardIndex = index, question = question, answer = answer, quiz = quiz}


toCChapter :: (RandomGen g, S.MonadState g m) => Language -> Language -> Chapter -> m CChapter
toCChapter = undefined


---

type DicOf a = M.Map Language a

data CSVCourseIntro = CSVCourseIntro {
    csvCourseIntroTitle :: Dic
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
    csvCourseIntroTitle = M.empty
  , csvCourseIntroFC1 = emptyCsvCourseIntroFlashcard
  , csvCourseIntroFC2 = emptyCsvCourseIntroFlashcard
}

duck :: IO ()
duck = do
  csvData <- BL.readFile "./content/en-Table 1.csv"
  let xs = go emptyCsvCourseIntro (decode NoHeader csvData)
  write xs
  where
    write :: Either String CSVCourseIntro -> IO ()
    write (Left err) = putStrLn err
    write (Right xs) = print xs -- writeFile "out.txt" $ foldl1 (\a b -> a ++ "\n" ++ b) xs

    fieldMap v = M.fromList [
          ("1-title", \ val -> v {csvCourseIntroTitle = val} ) -- courseTitle
        , ("intro_title", \ val -> v {csvCourseIntroTitle = val})
        , ("intro_fc-1_q", \ val ->
          updateFC1 (\ fc -> fc {csvCiFcQuestion = val})
          -- let fc = csvCourseIntroFC1 v
          --     fc' = fc {csvCiFcQuestion = val}
          -- in
          -- v { csvCourseIntroFC1 = fc' }
        )
        , ("intro_fc-1_a_short", \ val ->
          let fc = csvCourseIntroFC1 v
              fc' = fc {csvCiFcShortAns = val}
          in
          v { csvCourseIntroFC1 = fc' })
        , ("intro_fc-1_a_long", \ val ->
          let fc = csvCourseIntroFC1 v
              fc' = fc {csvCiFcLongAns = val}
          in
          v { csvCourseIntroFC1 = fc' })
        -- , ("intro_fc-1_a_long", \ val -> v {csvCiFcLongAns = val})
      ] where
        updateFC1 f =
          let fc = csvCourseIntroFC1 v
              fc' = f fc
          in
          v { csvCourseIntroFC1 = fc' }

    go _ (Left err) = Left err
    go intro (Right v) = V.foldM (\ intro' (xs :: [String]) -> do
        let f = M.lookup (xs !! 1) (fieldMap intro')
        return $ maybe intro' ($ M.fromList $ ["en","es","fr","de","ru","ar"] `zip` drop 2 xs) f
        ) intro v


duck2 :: IO ()
duck2 = do
  csvData <- BL.readFile "./content/en-Table 1.csv"
  let xs = go CCourseIntro {ccourseIntroTitle = "", ccourseIntroIntroTitle = "", ccourseIntroFCs = []} (decode NoHeader csvData)
  write xs
  where
    write (Left err) = putStrLn err
    write (Right xs) = print xs -- writeFile "out.txt" $ foldl1 (\a b -> a ++ "\n" ++ b) xs

    go _ (Left err) = Left err
    go intro (Right v) = V.foldM (\ intro' (xs :: [String]) ->
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
