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
import Control.Monad.Trans.Except
import qualified Control.Monad.Except as C
import qualified Control.Exception.Base as B
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
  (map Elem [
      textElement "FCQuestion" (question card)
    , collElement "FCAnswer" [textElement "text" $ answer card]
    , collElement "quiz" [elemQuestion (answer card) (quiz card)]
    , emptyElement "image" [("position", "belowShortAnswer"),("source", "http://babelbay-assets.mobileacademy.com/images/" ++ index ++ ".jpg")]
    , emptyElement "audio" [("source", "http://babelbay-assets.mobileacademy.com/audios/" ++ audio card ++ ".wav")]
  ])
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
  (map Elem (textElement "title" (chapterTitle chapter) : map flashcardToXml (chapterCards chapter)))
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
courseToXml key course = let intro = courseIntro course in Element
  (unqual "BookSummary")
  []
  (map Elem $ [
      emptyElement "meta" [
          ("id", show $ courseId intro)
        , ("key", key)
        , ("viewType", "BabelbayQA")
      ]
    , textElement "Title" (courseTitle intro)
    , collElement "introduction" [
        textElement "title" (courseIntroTitle intro)
      , courseIntroFlashcard1AnswerToXml (courseIntroFC1 intro)
      , courseIntroFlashcard2AnswerToXml (courseIntroFC2 intro)
    ]
  ] ++ map chapterToXml (courseChapters course))
  Nothing

courseKey :: String -> Course -> String
courseKey keyPostfix course = "Learn" ++ show (courseLanguage $ courseIntro course) ++ "Language" ++ keyPostfix

---

data AppConfig = AppConfig CourseMeta deriving Show
data AppState = AppState StdGen deriving Show
type App m = ReaderT AppConfig (StateT AppState (ExceptT String m))

runApp :: App m a -> AppConfig -> AppState -> m (Either String (a, AppState))
-- runApp app config state = runExceptT $ (runStateT $ runReaderT app config) state
runApp app config = runExceptT . runStateT (runReaderT app config)

app :: App IO (IO ())
app = do
  (AppConfig meta) <- R.ask
  (AppState g) <- S.get
  file <- liftIO $ BL.readFile "./final.csv"
  csvData <- liftIO $ BL.readFile $ "./content/" ++ target meta ++ "-Table 1.csv"
  let chapters = toCSVChapters file
  let intro = toCSVCourseIntro csvData
  let ecs = mapSnd AppState <$> liftM2 (\x y -> runCSVDataConversion (toCourse x y) g meta) intro chapters
  let cs = mapFst (["", "Two"] `zip`) <$> ecs
  let ios = mapFst (mapM_ (uncurry (writeCourse meta) . mapB courseKey)) <$> cs
  case ios of
    (Left err) -> C.throwError "EXCEPTION"
    (Right tup) -> do
      S.put (snd tup)
      return (fst tup)

  -- runApp app (AppConfig $ makeCourseMeta "en" "es") (AppState $ mkStdGen 10) >>= handle
  -- handle (Left str) = print str
  -- handle (Right i) = fst i


  --liftIO $ print cs
  --return ((), AppState g)

-- write :: App IO ((), StdGen)
-- write = do
--   (AppConfig meta) <- R.ask
--   (AppState g) <- S.get

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

-- writeCourse :: CourseMeta -> String -> Course -> IO ()
writeCourse meta key course = do
  let fileName = key ++ "-" ++ native meta ++ ".xml"
  writeFile ("/Users/homam/dev/ma/maAssets/" ++ key ++ "/text/" ++ fileName) (ppcTopElement prettyConfigPP (courseToXml key course))

-- liftIO = lift . lift

someFunc :: IO ()
someFunc = do
  matrix <- BL.readFile "./final.csv"
  forM_ ["en","ar","fr","de","ru","es"] $ \ targetLang -> do
    let meta = makeCourseMeta "es" targetLang
    csvData <- BL.readFile $ "./content/" ++ target meta ++ "-Table 1.csv"
    let chapters = toCSVChapters matrix
    let intro = toCSVCourseIntro csvData
    g <- newStdGen
    let cs = liftM2 (\x y -> runCSVDataConversion (toCourse x y) g meta) intro chapters
    write meta cs
    where
      write :: CourseMeta -> Either String ([Course], StdGen) -> IO ()
      write _ (Left err) = putStrLn err
      write meta (Right (cs, _)) = mapM_
        -- (putStrLn . ppcTopElement prettyConfigPP . uncurry courseToXml . mapB courseKey)
        (uncurry (writeCourse meta) . mapB courseKey)
        (["", "Two"] `zip` cs)

      writeCourse :: CourseMeta -> String -> Course -> IO ()
      writeCourse meta key course = do
        let fileName = key ++ "-" ++ native meta ++ ".xml"
        writeFile ("/Users/homam/dev/ma/maAssets/" ++ key ++ "/text/" ++ fileName) (ppcTopElement prettyConfigPP (courseToXml key course))

--

mapB :: (a -> b -> c) -> (a, b) -> (c, b)
mapB f t = (uncurry f t, snd t)
