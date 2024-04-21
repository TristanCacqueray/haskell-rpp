module RPP where

import Data.Attoparsec.Text qualified as Atto
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

data Block = Block
    { name :: Text
    , items :: [Either Block Text]
    }
    deriving (Show)

lineParser :: Atto.Parser Text
lineParser = Atto.takeWhile (/= '\n')

firstChar :: Char -> Atto.Parser ()
firstChar c = Atto.skipSpace *> Atto.char c >> pure ()

bodyParser :: Atto.Parser (Either Block Text)
bodyParser = Atto.skipSpace *> Atto.eitherP blockParser lineParser

blockParser :: Atto.Parser Block
blockParser = do
    firstChar '<'
    name <- lineParser
    items <- Atto.manyTill' bodyParser (firstChar '>' *> Atto.endOfLine)
    pure $ Block{name, items}

parse :: Text -> Either String Block
parse = Atto.parseOnly (blockParser <* Atto.endOfInput)

parseFile :: FilePath -> IO (Either String Block)
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
