{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import RPP
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main =
    getArgs >>= \case
        ["--help"] -> usage
        [] -> usage
        [rppFile] ->
            RPP.parseFile rppFile >>= \case
                Left err -> putStrLn $ rppFile <> ": " <> err
                Right proj -> go proj
  where
    usage = putStrLn "usage: dump RPP_FILE"

go :: RPP.Project -> IO ()
go proj = do
    putStrLn "[+] Markers"
    mapM_ (Text.putStrLn . showMarker) $ getMarkers proj

showMarker :: RPP.Marker -> Text
showMarker (RPP.Marker pos name) = Text.pack posTxt <> " " <> name
  where
    posTxt = mconcat [hourTxt, minTxt, secTxt]
    secTxt = printf "%02d" $ sec `mod` 60
    minTxt = printf "%02d:" $ (sec `mod` 3600) `div` 60
    hour = sec `div` 3600
    hourTxt
        | hour > 0 = printf "%d:" hour
        | otherwise = ""
    sec = floor @Double @Int pos
