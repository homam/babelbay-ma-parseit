{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving, RankNTypes #-}

module Lib (
    someFunc
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
} deriving Show


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

data CCourse = CCourse {
    ccourseIntro :: CCourseIntro
  , ccourseChapters :: [CChapter]
} deriving Show

data CCourseMeta = CCourseMeta {
    native :: String
  , target :: String
  , courseId :: Integer
  , ccourseKey :: String
} deriving Show


newtype CSVDataConversion a = CSVDataConversion {
  unCSVDataConversion :: ReaderT CCourseMeta (StateT StdGen Identity) a
} deriving (Functor, Applicative, Monad, R.MonadReader CCourseMeta, S.MonadState StdGen)

runCSVDataConversion :: CSVDataConversion a -> StdGen -> CCourseMeta -> (a, StdGen)
runCSVDataConversion k g course = runIdentity $ runStateT (runReaderT (unCSVDataConversion k) course) g


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


toCChapter :: Chapter -> CSVDataConversion CChapter
toCChapter chapter = do
  config <- R.ask
  let questions = csvChapterCards chapter
  -- let (cs, _) = runCSVDataConversion (mapM (toCFlashcard questions) $ csvChapterCards chapter) g config
  cs <- mapM (toCFlashcard questions) $ csvChapterCards chapter
  return CChapter {
      cchapterIndex = csvChapterIndex chapter
    , ctitle = csvChapterDic chapter M.! native config
    , ccards = cs
  }

toCCourse :: CSVCourseIntro -> [Chapter] -> CSVDataConversion CCourse
toCCourse intro chapters = do
  intro' <- toCCourseIntro intro
  chapters' <- mapM toCChapter chapters
  return CCourse {
      ccourseIntro = intro'
    , ccourseChapters = chapters'
  }


someFunc :: IO ()
someFunc = do
  file <- BL.readFile "./final.csv"
  csvData <- BL.readFile "./content/en-Table 1.csv"
  let chapters = toChapters file
  -- either print (\ xs -> print $ head xs) chapters
  let intro = toCourseIntro csvData
  let course = CCourseMeta {native = "es", target = "en", courseId = 1, ccourseKey = "es-en"}
  g <- newStdGen
  -- let oneChapter = head chapters
  -- let questions = csvChapterCards oneChapter
  -- let (cs, _) = runCSVDataConversion (mapM (toCFlashcard questions) $ csvChapterCards $ head chapters) g course
  -- putStrLn $ showTopElement $ collElement "cards" $ map cFlashcardToXml cs

  -- let (cchapters, _) = runCSVDataConversion (mapM toCChapter chapters) g course
  -- print cchapters

  let c = liftM2 (\x y -> runCSVDataConversion (toCCourse x y) g course) intro chapters
  --let (ccourse, _) = runCSVDataConversion (toCCourse intro chapters) g course
  write c
  where
    write (Left err) = putStrLn err
    write (Right (c, _)) = putStrLn $ ppcTopElement prettyConfigPP $ collElement "BookSummary" $ map cChapterToXml (ccourseChapters c)




---

duck :: IO ()
duck = do
  csvData <- BL.readFile "./content/en-Table 1.csv"
  let xs = toCCourseIntro <$> toCourseIntro csvData
  g <- newStdGen
  let course = CCourseMeta {native = "es", target = "en", courseId = 1, ccourseKey = "es-en"}
  let c = (\x -> runCSVDataConversion x g course) <$> xs
  write c
  where
    write (Left err) = putStrLn err
    write (Right c) = print c



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

addAttr :: (String, String) -> Element -> Element
addAttr (name, value) elem =
  let attr = Attr {attrKey = QName { qName = name, qURI = Nothing, qPrefix = Nothing }, attrVal = value}
  in add_attr attr elem


collElement :: String -> [Element] -> Element
collElement name children = Element
  (unqual name)
  []
  (map Elem children)
  Nothing

cFlashcardToXml :: CFlashcard -> Element
cFlashcardToXml card = Element
  (unqual "QAFlashCard")
  []
  [
      Elem $ textElement "FCQuestion" (question card)
    , Elem $ collElement "FCAnswer" [textElement "text" $ answer card]
    , Elem $ collElement "quiz" [elemQuestion (answer card) (quiz card)]
  ]
  Nothing
  where
    elemQuestion :: String -> [String] -> Element
    elemQuestion title options = addAttr ("layout", "text") $ collElement "question" $ textElement "title" title : elemOptions options

    elemOptions (correct:wrongs) = addAttr ("correct", "true") (textElement "option" correct) : map (addAttr ("correct", "false") . textElement "option") wrongs

cChapterToXml :: CChapter -> Element
cChapterToXml chapter = Element
  (unqual "chapter")
  [
    Attr {attrKey = QName { qName = "key", qURI = Nothing, qPrefix = Nothing }, attrVal = show $ cchapterIndex chapter}
  ]
  ((Elem $ textElement "title" (ctitle chapter)) : map (Elem . cFlashcardToXml) (ccards chapter))
  Nothing
