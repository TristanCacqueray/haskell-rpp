{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read (double)
import System.Directory (removeFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, takeExtension)
import System.Process (callProcess)

import RPP

main :: IO ()
main =
    getArgs >>= \case
        ["--help"] -> usage
        [] -> usage
        [rppFile, videoFile, outFile] -> do
            RPP.parseFile rppFile >>= \case
                Left err -> putStrLn $ rppFile <> ": " <> err
                Right proj -> go proj rppFile videoFile outFile
  where
    usage = do
        putStrLn "usage: split-video RPP_FILE VIDEO_FILE OUT_FILE"
        exitFailure

go proj rppFile videoFile outFile = do
    T.writeFile ".ffmpeg-concat" (concatScript proj (T.pack videoFile))
    let tmpFile = dropExtension outFile <> "-tmp" <> takeExtension outFile
    -- split the video and remove the audio
    callProcess "ffmpeg" ["-hide_banner", "-safe", "0", "-f", "concat", "-i", ".ffmpeg-concat", "-c", "copy", "-an", tmpFile]
    -- merge with the new audio
    let audioFile = dropExtension rppFile <> ".mp3"
    callProcess "ffmpeg" ["-hide_banner", "-i", tmpFile, "-i", audioFile, "-c", "copy", outFile]
    mapM_ removeFile [".ffmpeg-concat", tmpFile]

concatScript :: RPP.Project -> T.Text -> T.Text
concatScript project videoPath = mappend "ffconcat version 1.0\n" $ mkScript $ map (either (error . show) id) $ getItems $ head $ getTracks project
  where
    mkScript :: [RPP.Item] -> T.Text
    mkScript [] = ""
    mkScript (item : next : rest) = mkFile item.sourceOffset (next.position - item.position) <> mkScript (next : rest)
    mkScript [item] = mkFile item.sourceOffset item.itemLength
    mkFile mOffset mLength = "file '" <> videoPath <> "'\n" <> mkValue "inpoint" mOffset <> mkValue "outpoint" (mOffset + mLength)
      where
        mkValue name v = name <> " " <> T.pack (show v) <> "\n"
