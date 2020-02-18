module Main where

import Lib
import Control.Exception
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy as BS


loadSettings :: IO Settings
loadSettings = do
    contents <- try (BS.readFile "settings.json") :: IO (Either SomeException BS.ByteString)
    return (either (const defaultSettings) (fromJust . decode) contents)


main :: IO ()
main = do
    settings <- loadSettings
    runApp settings imgHostMain
