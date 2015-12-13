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

cFlashcardToXml :: Flashcard -> Element
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

chapterToXml :: Chapter -> Element
chapterToXml chapter = Element
  (unqual "chapter")
  [
    Attr {attrKey = QName { qName = "key", qURI = Nothing, qPrefix = Nothing }, attrVal = show $ chapterIndex chapter}
  ]
  ((Elem $ textElement "title" (chapterTitle chapter)) : map (Elem . cFlashcardToXml) (chapterCards chapter))
  Nothing


---

someFunc :: IO ()
someFunc = do
  file <- BL.readFile "./final.csv"
  csvData <- BL.readFile "./content/en-Table 1.csv"
  let chapters = toCSVChapters file
  let intro = toCSVCourseIntro csvData
  let course = makeCourseMeta "es" "en" 1 "es-en"
  g <- newStdGen
  let c = liftM2 (\x y -> runCSVDataConversion (toCourse x y) g course) intro chapters
  write c
  where
    write (Left err) = putStrLn err
    write (Right (c, _)) = putStrLn $ ppcTopElement prettyConfigPP $ collElement "BookSummary" $ map chapterToXml (courseChapters c)