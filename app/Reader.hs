{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
module Reader (
  Reading,
  Position,
  SourceFile (..),
  saveState,
  resetState,
  showLineColumn,
  read,
  currentPosition,
  currentLine,
  currentFile,
  peek,
  skipAnyChar,
  skipWhile,
  skipWhitespace,
  readChar,
  skipChar,
  readWhile,
  readUntil,
  readUntilChar,
  stopRecording,
  startRecording,
  peek2,
)
where

import Control.Monad.Loops (whileM)
import Data.Char
import Data.Functor ((<&>))
import Data.List
import Data.Map
import Data.Maybe (fromJust, listToMaybe)
import Effectful
import Effectful.Fail
import Effectful.State.Static.Shared
import Utils
import Prelude hiding (read)

type Reading = State ReaderState

data Position = Position {index :: Int, line :: Int, column :: Int}
  deriving (Show)

showLineColumn :: Position -> String
showLineColumn pos = show pos.line ++ ":" ++ show pos.column

data ReaderState = ReaderState
  { filename :: String
  , remaining_contents :: String
  , position :: Position
  , recordings :: Map Int String
  , nextRecordingId :: Int
  }

saveState :: (Reading :> es) => Eff es ReaderState
saveState = get

resetState :: (Reading :> es) => ReaderState -> Eff es ()
resetState = put

data SourceFile = SourceFile {filename :: String, contents :: String}
  deriving (Show)

read :: forall a es. SourceFile -> Eff (Fail : Reading : es) a -> Eff (Fail : es) a
read SourceFile{..} f =
  evalState
    ReaderState
      { filename
      , remaining_contents = contents
      , position =
          Position
            { index = 0
            , line = 1
            , column = 1
            }
      , recordings = Data.Map.empty
      , nextRecordingId = 0
      }
    run
 where
  run :: Eff (Reading : Fail : es) a
  run = do
    result <- inject $ runFail f
    pos <- currentPosition
    case result of
      Left err -> fail $ err ++ " at " ++ showLineColumn pos
      Right x -> return x

currentPosition :: (Reading :> es) => Eff es Position
currentPosition = get <&> position

currentLine :: (Reading :> es) => Eff es Int
currentLine = currentPosition <&> line

currentFile :: (Reading :> es) => Eff es String
currentFile = get @ReaderState <&> \reader -> reader.filename

peek :: (Reading :> es) => Eff es (Maybe Char)
peek = get <&> listToMaybe . remaining_contents

peek2 :: (Reading :> es) => Eff es (Maybe Char)
peek2 =
  get <&> remaining_contents <&> \case
    _ : c2 : _ -> Just c2
    _ -> Nothing

skipWhile :: (Reading :> es) => (Char -> Bool) -> Eff es ()
skipWhile predicate = readWhile predicate <&> ignore

skipWhitespace :: (Reading :> es) => Eff es ()
skipWhitespace = skipWhile isSpace

readWhile :: (Reading :> es) => (Char -> Bool) -> Eff es String
readWhile predicate = whileM (peek <&> maybe False predicate) (readChar <&> fromJust)

readUntil :: (Reading :> es) => (Char -> Bool) -> Eff es String
readUntil predicate = readWhile (not . predicate)

readUntilChar :: (Reading :> es) => Char -> Eff es String
readUntilChar c = readUntil (== c)

skipAnyChar :: (Reading :> es, Fail :> es) => Eff es ()
skipAnyChar =
  readChar >>= \case
    Just _ -> return ()
    Nothing -> fail "unexpected EOF"

skipChar :: (Reading :> es, Fail :> es) => Char -> Eff es ()
skipChar expected = do
  peek >>= \case
    Just actual | actual == expected -> return ()
    Just actual -> fail $ "expected " ++ show expected ++ ", got " ++ show actual
    Nothing -> fail $ "expected " ++ show expected ++ ", got EOF"
  skipAnyChar

readChar :: (Reading :> es) => Eff es (Maybe Char)
readChar = state \reader ->
  case reader.remaining_contents of
    [] -> (Nothing, reader)
    c : remaining_contents ->
      let Position{..} = reader.position
          newPosition = case c of
            '\n' -> Position{index = index + 1, line = line + 1, column = 1}
            _ -> Position{index = index + 1, line, column = column + 1}
       in ( Just c
          , reader
              { remaining_contents
              , position = newPosition
              , recordings = Data.Map.map (\s -> s ++ [c]) reader.recordings
              }
          )

newtype Recording = Recording Int

startRecording :: (Reading :> es) => Eff es Recording
startRecording = state \reader ->
  let newId = reader.nextRecordingId
      nextRecordingId = newId + 1
   in ( Recording newId
      , reader
          { recordings = Data.Map.insert newId "" reader.recordings
          , nextRecordingId
          }
      )

stopRecording :: (Reading :> es) => Recording -> Eff es String
stopRecording (Recording recordingId) = state \reader ->
  let result = reader.recordings ! recordingId
      recordings = Data.Map.delete recordingId reader.recordings
   in (result, reader{recordings})