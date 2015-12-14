{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RankNTypes #-}

module XMLSerialization (
  chapterToXml
) where

--
import Prelude hiding (words)
import qualified Data.Map as M
import Text.XML.Light
import System.Random
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import Lib
import CSVParsers
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Control.Monad.Trans.Reader
import Control.Monad.Identity

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

emptyElement :: String -> [(String, String)] -> Element
emptyElement name attrs = Element
  (unqual name)
  (map toAttr attrs)
  []
  Nothing

toAttr :: (String, String) -> Attr
toAttr (name, value) = Attr {attrKey = QName { qName = name, qURI = Nothing, qPrefix = Nothing }, attrVal = value}

addAttr :: (String, String) -> Element -> Element
addAttr = add_attr . toAttr


collElement :: String -> [Element] -> Element
collElement name children = Element
  (unqual name)
  []
  (map Elem children)
  Nothing

flashcardToXml :: Flashcard -> Element
flashcardToXml card = Element
  (unqual "QAFlashCard")
  []
  [
      Elem $ textElement "FCQuestion" (question card)
    , Elem $ collElement "FCAnswer" [textElement "text" $ answer card]
    , Elem $ collElement "quiz" [elemQuestion (answer card) (quiz card)]
    , Elem $ emptyElement "image" [("position", "belowShortAnswer"),("source", "http://babelbay-assets.mobileacademy.com/images/" ++ index ++ ".jpg")]
    , Elem $ emptyElement "audio" [("source", "http://babelbay-assets.mobileacademy.com/audios/" ++ index ++ ".mp3")]
  ]
  Nothing
  where
    index = show (flashCardIndex card)

    elemQuestion :: String -> [String] -> Element
    elemQuestion title options = addAttr ("layout", "text") $ collElement "question" $ textElement "title" title : elemOptions options
    elemOptions (correct:wrongs) = addAttr ("correct", "true") (textElement "option" correct) : map (addAttr ("correct", "false") . textElement "option") wrongs

chapterToXml :: Chapter -> Element
chapterToXml chapter = Element
  (unqual "chapter")
  [
    Attr {attrKey = QName { qName = "key", qURI = Nothing, qPrefix = Nothing }, attrVal = show $ chapterIndex chapter}
  ]
  ((Elem $ textElement "title" (chapterTitle chapter)) : map (Elem . flashcardToXml) (chapterCards chapter))
  Nothing


courseIntroFlashcardToXml :: Element -> CourseIntroFlashcard a -> Element
courseIntroFlashcardToXml long fc = collElement "QAFlashCard" [question, answer] where
  question = textElement "FCQuestion" (courseIntroFCQuestion fc)
  answer = collElement "FCAnswer" [short, long]
  short = addAttr ("type", "text") $ textElement "short" (courseIntroFCShortAnswer fc)

courseIntroFlashcard1AnswerToXml :: CourseIntroFlashcard String -> Element
courseIntroFlashcard1AnswerToXml fc = courseIntroFlashcardToXml (collElement "long" . return . textElement "text" . courseIntroFCLongAnswer $ fc) fc

courseIntroFlashcard2AnswerToXml :: CourseIntroFlashcard [String] -> Element
courseIntroFlashcard2AnswerToXml fc = courseIntroFlashcardToXml (collElement "long" . return . collElement "simpleList" . map (textElement "item") . courseIntroFCLongAnswer $ fc) fc



courseToXml :: String -> Course -> Element
courseToXml keyPostfix course = let intro = courseIntro course in Element
  (unqual "BookSummary")
  []
  ([
      Elem $ emptyElement "meta" [
          ("id", show $ courseId intro)
        , ("key", "Learn" ++ show (courseLanguage intro) ++ "Language" ++ keyPostfix)
        , ("viewType", "BabelbayQA")
      ]
    , Elem $ textElement "Title" (courseTitle intro)
    , Elem $ courseIntroFlashcard1AnswerToXml (courseIntroFC1 intro)
    , Elem $ courseIntroFlashcard2AnswerToXml (courseIntroFC2 intro)
  ] ++ map (Elem . chapterToXml) (courseChapters course))
  Nothing

---

someFunc :: IO ()
someFunc = do
  file <- BL.readFile "./final.csv"
  csvData <- BL.readFile "./content/en-Table 1.csv"
  let chapters = toCSVChapters file
  let intro = toCSVCourseIntro csvData
  let course = makeCourseMeta "es" "en"
  g <- newStdGen
  let c = liftM2 (\x y -> runCSVDataConversion (toCourse x y) g course) intro chapters
  write c
  where
    write :: Either String ([Course], StdGen) -> IO ()
    write (Left err) = putStrLn err
    write (Right (cs, _)) = mapM_
      (putStrLn . ppcTopElement prettyConfigPP . uncurry courseToXml)
      (["", "Two"] `zip` cs)
