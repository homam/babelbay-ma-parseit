{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving, RankNTypes #-}

module Lib (
    Flashcard (..)
  , Chapter
  , chapterIndex
  , chapterTitle
  , chapterCards
  , CourseIntro (..)
  , CourseIntroFlashcard (..)
  , Course
  , courseIntro
  , courseChapters
  , CourseMeta (..)
  , makeCourseMeta
  , toCourse
  , CSVDataConversion
  , runCSVDataConversion
) where

import Prelude hiding (words)
import qualified Data.Map as M
import System.Random
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import Rand
import CSVParsers
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Control.Monad.Trans.Reader
import Control.Monad.Identity




data Flashcard = Flashcard {
    flashCardIndex :: Integer
  , question :: String
  , answer :: String
  , quiz :: [String]
  , audio :: String
} deriving Show

data Chapter = Chapter {
    chapterIndex :: Integer
  , chapterTitle :: String
  , chapterCards :: [Flashcard]
} deriving Show


data CourseIntro = CourseIntro {
    courseTitle :: String
  , courseId :: Integer
  , courseLanguage :: CSVLanguage
  , courseIntroTitle :: String
  , courseIntroFC1 :: CourseIntroFlashcard String
  , courseIntroFC2 :: CourseIntroFlashcard [String]
} deriving Show

data CourseIntroFlashcard a = CourseIntroFlashcard {
    courseIntroFCQuestion :: String
  , courseIntroFCShortAnswer :: String
  , courseIntroFCLongAnswer :: a
} deriving Show

data Course = Course {
    courseIntro :: CourseIntro
  , courseChapters :: [Chapter]
} deriving Show

data CourseMeta = CourseMeta {
    native :: Language
  , target :: Language
} deriving Show

makeCourseMeta :: Language -> Language -> CourseMeta
makeCourseMeta native target = CourseMeta {
      native = native
    , target = target
  }


newtype CSVDataConversion a = CSVDataConversion {
  unCSVDataConversion :: ReaderT CourseMeta (StateT StdGen Identity) a
} deriving (Functor, Applicative, Monad, R.MonadReader CourseMeta, S.MonadState StdGen)

runCSVDataConversion :: CSVDataConversion a -> StdGen -> CourseMeta -> (a, StdGen)
runCSVDataConversion k g course = runIdentity $ runStateT (runReaderT (unCSVDataConversion k) course) g


toFlashcard :: [CSVFlashcard] -> CSVFlashcard -> CSVDataConversion Flashcard
toFlashcard questions flashcard = do
  config <- R.ask
  let dic = csvFCDic flashcard
      index = csvFCIndex flashcard
      answer = dic M.! native config
      question = dic M.! target config
  quiz <- (question :) . map ((M.! target config) . csvFCDic) . take 3 <$> randomize (filter ((/= index) . csvFCIndex) questions)
  return Flashcard {flashCardIndex = index, question = question, answer = answer, quiz = quiz, audio = target config ++ "/" ++ show index}

toCourseIntro :: CSVCourseIntro -> CSVDataConversion [CourseIntro]
toCourseIntro intro = do
  config <- R.ask
  let nativeLang = native config
  let meta = csvCourseMeta intro
  return $ map
    (\ (ctitle, cid) -> CourseIntro {
        courseTitle = ctitle intro M.! nativeLang
      , courseId = cid meta
      , courseLanguage = csvCourseMetaLanguage meta
      , courseIntroTitle = csvCourseIntroTitle intro M.! nativeLang
      , courseIntroFC1 = toCourseIntroFlashcard nativeLang (csvCourseIntroFC1 intro)
      , courseIntroFC2 = toCourseIntroFlashcard nativeLang (csvCourseIntroFC2 intro)
    })
    [(csvCourseTitle1, csvCourseMetaId1), (csvCourseTitle2, csvCourseMetaId2)] -- there are 2 different course titles and course Ids in each CSV file

toCourseIntroFlashcard :: Language -> CSVCourseIntroFlashcard a -> CourseIntroFlashcard a
toCourseIntroFlashcard native fc =
  let question = csvCiFcQuestion fc
      shortAnswer = csvCiFcShortAns fc
      longAnswer = csvCiFcLongAns fc
  in CourseIntroFlashcard {
      courseIntroFCQuestion = question M.! native
    , courseIntroFCShortAnswer = shortAnswer M.! native
    , courseIntroFCLongAnswer = longAnswer M.! native
  }


toChapter :: CSVChapter -> CSVDataConversion Chapter
toChapter chapter = do
  config <- R.ask
  let questions = csvChapterCards chapter
  cs <- mapM (toFlashcard questions) $ csvChapterCards chapter
  return Chapter {
      chapterIndex = csvChapterIndex chapter
    , chapterTitle = csvChapterDic chapter M.! native config
    , chapterCards = cs
  }

toCourse :: CSVCourseIntro -> [CSVChapter] -> CSVDataConversion [Course]
toCourse intro chapters = do
  intros' <- toCourseIntro intro
  chapters' <- mapM toChapter chapters
  -- CSV matrix has the content for 2 courses
  let half = floor $ fromIntegral (length chapters') / 2
  let (chapters1, chapters2) = splitAt half chapters'
  return $ map
    (\ (intro', chapter'') -> Course {
        courseIntro = intro'
      , courseChapters = chapter''
    })
    (intros' `zip` [chapters1, chapters2])



---

duck :: IO ()
duck = do
  csvData <- BL.readFile "./content/en-Table 1.csv"
  let xs = toCourseIntro <$> toCSVCourseIntro csvData
  g <- newStdGen
  let course = CourseMeta {native = "en", target = "ar"}
  let c = (\x -> runCSVDataConversion x g course) <$> xs
  write c
  where
    write (Left err) = putStrLn err
    write (Right c) = print c



---
