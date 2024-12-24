module RPP where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (void)
import Data.Attoparsec.Text qualified as Atto
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Read qualified as Text

data Block = Block
    { name :: Text
    , items :: [Either Block Text]
    }
    deriving (Show)

newtype Project = Project Block
    deriving (Show)

lineParser :: Atto.Parser Text
lineParser = Text.dropWhileEnd (== '\r') <$> Atto.takeWhile (/= '\n')

strParser :: Atto.Parser Text
strParser = quotedStr <|> Atto.takeWhile (/= ' ')
  where
    quotedStr = "\"" *> Atto.takeWhile (/= '"') <* "\""

lexP :: Atto.Parser a -> Atto.Parser a
lexP p = Atto.skipSpace *> p

firstChar :: Char -> Atto.Parser ()
firstChar c = void $ lexP $ Atto.char c

bodyParser :: Atto.Parser (Either Block Text)
bodyParser = lexP $ Atto.eitherP blockParser lineParser

blockParser :: Atto.Parser Block
blockParser = do
    firstChar '<'
    name <- lineParser
    items <- Atto.manyTill' bodyParser (firstChar '>' *> Atto.endOfLine)
    pure $ Block{name, items}

parse :: Text -> Either String Project
parse = Atto.parseOnly (fmap Project blockParser <* Atto.endOfInput)

parseFile :: FilePath -> IO (Either String Project)
parseFile fp = parse <$> Text.readFile fp

test :: IO ()
test = print $ parse body
  where
    body =
        Text.unlines
            [ "<REAPER_PROJECT 0.1 \"7.11/linux-x86_64\" 1713119490"
            , "  <NOTES 0 2"
            , "  >"
            , "  RIPPLE 0"
            , "  <TRACK {49C573C8-7E81-7BA8-0214-AC26DE957942}"
            , "    NAME MELO"
            , "  >"
            , ">"
            ]

-- high level
type Time = Double

newtype Track = Track Block

data Item = Item
    { position :: Time
    , itemLength :: Time
    , sourceOffset :: Time
    }
    deriving (Show)

getItems :: Track -> [Either Text Item]
getItems (Track blk) = mapMaybe getItem blk.items
  where
    getItem = \case
        Left b | "ITEM" == b.name -> Just $ parseItem (getValues b)
        _ -> Nothing
    parseItem values = do
        position <- getFValue "POSITION" values
        itemLength <- getFValue "LENGTH" values
        sourceOffset <- getFValue "SOFFS" values
        pure $ Item{position, itemLength, sourceOffset}

getFValue :: Text -> [(Text, Text)] -> Either Text Double
getFValue name values = case lookup name values of
    Nothing -> Left $ "Not found: " <> name
    Just txt -> case Text.double txt of
        Right (v, "") -> Right v
        e -> Left $ "Could not parse: " <> Text.pack (show e)

getValues :: Block -> [(Text, Text)]
getValues blk = mapMaybe getValue blk.items
  where
    getValue = \case
        Right txt ->
            let (k, r) = Text.span (/= ' ') txt
             in Just (k, Text.drop 1 r)
        _ -> Nothing

getTracks :: Project -> [Track]
getTracks (Project rblk) = mapMaybe getTrack rblk.items
  where
    getTrack = \case
        Left blk | "TRACK {" `Text.isPrefixOf` blk.name -> Just $ Track blk
        _ -> Nothing

data Marker = Marker Time Text deriving (Show)

markerParser :: Atto.Parser Marker
markerParser = do
    _ <- Atto.decimal @Integer
    Marker <$> lexP Atto.double <*> lexP strParser

parseMarker :: Text -> Maybe Marker
parseMarker = either (const Nothing) Just . Atto.parseOnly markerParser

getMarkers :: Project -> [Marker]
getMarkers (Project rblk) = mapMaybe getMarker (getValues rblk)
  where
    getMarker ("MARKER", txtMarker) = parseMarker txtMarker
    getMarker _ = Nothing
