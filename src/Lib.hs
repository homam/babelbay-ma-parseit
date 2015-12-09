{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving, RankNTypes #-}

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
import qualified Control.Monad.Reader as R
import Rand
import CSVParsers
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Control.Monad.Trans.Reader
import Control.Monad.Identity



csvFilePath :: FilePath
csvFilePath = "./final.csv"



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


data CCourseIntro = CCourseIntro {
    ccourseTitle :: String
  , ccourseIntroTitle :: String
  , ccourseIntroFC1 :: CCourseIntroFC String
  , ccourseIntroFC2 :: CCourseIntroFC [String]
} deriving Show

data CCourseIntroFC a = CCourseIntroFC {
    ccourseIntroFCQuestion :: String
  , ccourseIntroFCShortAnswer :: String
  , ccourseIntroFCLongAnswer :: a
} deriving Show


data CCourseMeta = CCourseMeta {
    native :: String
  , target :: String
  , courseId :: Integer
  , ccourseKey :: String
}


newtype CSVDataConversion a = CSVDataConversion {
  unCSVDataConversion :: ReaderT CCourseMeta (StateT StdGen Identity) a
} deriving (Functor, Applicative, Monad, R.MonadReader CCourseMeta, S.MonadState StdGen)

runCSVDataConversion :: CSVDataConversion a -> StdGen -> CCourseMeta -> (a, StdGen)
runCSVDataConversion k g course =
  let state = g
  in runIdentity $ runStateT (runReaderT (unCSVDataConversion k) course) state


toCFlashcard :: [Flashcard] -> Flashcard -> CSVDataConversion CFlashcard
toCFlashcard questions flashcard = do
  config <- R.ask
  let dic = csvFCDic flashcard
      index = csvFCIndex flashcard
      answer = dic M.! native config
      question = dic M.! target config
  quiz <- (question :) . map ((M.! target config) . csvFCDic) . take 3 <$> randomize (filter ((/= index) . csvFCIndex) questions)
  return CFlashcard {cflashCardIndex = index, question = question, answer = answer, quiz = quiz}

toCCourseIntro :: CSVCourseIntro -> CSVDataConversion CCourseIntro
toCCourseIntro intro = do
  config <- R.ask
  return CCourseIntro {
      ccourseTitle = csvCourseTitle intro M.! native config
    , ccourseIntroTitle = csvCourseIntroTitle intro M.! native config
    , ccourseIntroFC1 = toCCourseIntroFC (native config) (csvCourseIntroFC1 intro)
    , ccourseIntroFC2 = toCCourseIntroFC (native config) (csvCourseIntroFC2 intro)
}

toCCourseIntroFC :: String -> CSVCourseIntroFlashcard a -> CCourseIntroFC a
toCCourseIntroFC native fc =
  let question = csvCiFcQuestion fc
      shortAnswer = csvCiFcShortAns fc
      longAnswer = csvCiFcLongAns fc
  in CCourseIntroFC {
      ccourseIntroFCQuestion = question M.! native
    , ccourseIntroFCShortAnswer = shortAnswer M.! native
    , ccourseIntroFCLongAnswer = longAnswer M.! native
  }


toCChapter :: (RandomGen g, S.MonadState g m) => Language -> Language -> Chapter -> m CChapter
toCChapter = undefined



someFunc :: IO ()
someFunc = do
  file <- readFile csvFilePath
  let chapters = toChapters file
  let oneChapter = head chapters
  let questions = csvChapterCards oneChapter
  let course = CCourseMeta {native = "es", target = "en", courseId = 1, ccourseKey = "es-en"}
  g <- newStdGen
  let (cs, _) = runCSVDataConversion (mapM (toCFlashcard questions) $ csvChapterCards $ head chapters) g course
  putStrLn $ showTopElement $ collElement "cards" $ map cFlashcardToXml cs



---

duck :: IO ()
duck = do
  csvData <- BL.readFile "./content/en-Table 1.csv"
  let course = CCourseMeta {native = "es", target = "en", courseId = 1, ccourseKey = "es-en"}
  let xs = toCourseIntro csvData
  write course xs
  where
    write :: CCourseMeta -> Either String CSVCourseIntro -> IO ()
    write _ (Left err) = putStrLn err
    -- write (Right xs) = print xs -- writeFile "out.txt" $ foldl1 (\a b -> a ++ "\n" ++ b) xs
    write course (Right xs) = do
      g <- newStdGen
      print $ runCSVDataConversion (toCCourseIntro xs) g course



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
