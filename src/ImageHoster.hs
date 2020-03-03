{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-|
Description : Simple image hosting
Copyright   : (c) Daniel, 2020
License     : BSD-3
Maintainer  : kingdread@gmx.de
-}

module ImageHoster
    ( -- * Submodules
      module ImageHoster.Data
    , module ImageHoster.Monad
      -- * Entry points
    , imgHostMain
    , handleUpload
    , handleCleanup
      -- * Other functions
    , checkAuthorization
    , randomName
    , validChars
    , indexPage
    , getImageData
    , saveFile
    ) where

import ImageHoster.Data
import ImageHoster.Monad

import Data.ByteString.Char8 (pack)
import Control.Applicative
import Control.Monad
import Crypto.BCrypt
import Data.Aeson
import Data.FileEmbed
import Data.Maybe
import Data.Time.Clock.System
import Network.CGI
import System.Directory
import System.FilePath
import System.Random

import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BS

-- | HTML source code of the index page.
--
-- This is included at compile time from @src/index.html@.
indexPage :: String
indexPage = $(embedStringFile "src/index.html")


rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just


chooseRandom :: (RandomGen g) => [a] -> g -> (a, g)
chooseRandom inp gen = let (idx, nextGen) = randomR (0, length inp - 1) gen in
                           (inp !! idx, nextGen)


-- | Valid characters for filename creation.
validChars :: String
validChars = ['a'..'z'] ++ ['A'..'Z']


-- | Creates a random filename.
--
-- This uses the IO standard random generator to generate a name.
randomName :: App String
randomName = settings' nameLength >>= \l ->
    mapM (\_ -> liftIO $ getStdRandom (chooseRandom validChars)) [1..l]


-- | Save the given file in the correct output directory.
saveFile :: BS.ByteString
         -- ^ The file content to be saved, i.e. the image data.
         -> String
         -- ^ The file extension (including dot), or an empty string.
         -> Seconds
         -- ^ The duration until the file should expire, or 0.
         -> String
         -- ^ The user name of the user that saved the file.
         -> App String
         -- ^ Returns the file name that was used to save the file.
saveFile content extension duration username = do
    filename <- (++ extension) <$> randomName
    dir <- settings' outputDir
    let outputPath = dir </> filename
    liftIO $ BS.writeFile outputPath content
    currentTime <- liftIO getSystemTime
    let eol = currentTime { systemSeconds = systemSeconds currentTime + fromInteger duration }
        metadata = Metadata { creator = username
                            , createdAt = toInteger $ systemSeconds currentTime
                            , endOfLife = toInteger $ systemSeconds eol
                            }
    liftIO $ BS.writeFile (outputPath ++ ".meta") (encode metadata)
    return filename


-- | Check the authorization data.
checkAuthorization :: [(String, String)]
                   -- ^ The (user, password) list.
                   -> String
                   -- ^ The given username.
                   -> String
                   -- ^ The given password.
                   -> Bool
                   -- ^ The authorization result.
checkAuthorization us user password = case lookup user us of
                                        Just p -> validatePassword (pack p) (pack password)
                                        Nothing -> False


-- | Get the image data from the current request.
--
-- This first checks if some raw content is given in the @imagecontent@ field
-- (i.e. data from a pasted image), and afterwards checks @imagefile@ (i.e. the
-- file selected in the file chooser).
getImageData :: FallibleApp BS.ByteString
getImageData = fallible $ do
    rawData <- getInputFPS "imagecontent"
    case rawData of
      Just d | d /= "" -> return . rightToMaybe $ B64.decode d
      _ -> getInputFPS "imagefile"


cleanupFile :: FilePath -> App ()
cleanupFile fname = do
    dir <- settings' outputDir
    currentTime <- systemSeconds <$> liftIO getSystemTime
    content <- liftIO $ BS.readFile (dir </> fname)
    case decode content :: (Maybe Metadata) of
      Just metadata | isEternal metadata -> return ()
      Just metadata | endOfLife metadata < toInteger currentTime -> do
          logCGI ("Deleting " ++ fname)
          liftIO $ removeFile (dir </> fname)
          liftIO $ removeFile (dir </> takeBaseName fname)
      _ -> return ()


-- | Performs the cleanup by deleting expired images.
handleCleanup :: App CGIResult
handleCleanup = do
    dir <- settings' outputDir
    files <- liftIO $ getDirectoryContents dir
    let metaFiles = filter ((== ".meta") . takeExtension) files
    forM_ metaFiles cleanupFile
    output "Cleaning up"


handleUpload' :: FallibleApp CGIResult
handleUpload' = do
    us <- liftApp $ settings' users
    outdir <- liftApp $ settings' outputDir
    user <- fallible $ getInput "username"
    password <- fallible $ getInput "password"
    guard $ checkAuthorization us user password
    filecontent <- getImageData
    filename <- (fallible $ getInputFilename "imagefile") <|> return ""
    duration :: Integer <- fallible $ readInput "duration"
    savedFileName <- liftApp $ saveFile filecontent (takeExtension filename) duration user
    liftApp . redirect $ outdir </> savedFileName


-- | Performs a file upload.
handleUpload :: App CGIResult
handleUpload = runFallibleApp handleUpload' >>= \case
    Just r -> return r
    Nothing -> setStatus 400 "Invalid request" >> output "The upload failed"

-- | Main entry point of the application.
--
-- Depending on the given parameters, this either dispatches to
-- 'handleCleanup', 'handleUpload' or just returns the 'indexPage'.
imgHostMain :: App CGIResult
imgHostMain = dispatch [ (doCleanup, handleCleanup)
                       , (doUpload, handleUpload)
                       , (doDefault, output indexPage)
                       ]
    where
        dispatch :: [(App Bool, App CGIResult)] -> App CGIResult
        dispatch ((c, a):as) = do
            take_that <- c
            if take_that then a
                         else dispatch as
        dispatch [] = do
            setStatus 500 "Internal Server Error"
            output "No dispatch could be done :("

        doCleanup :: App Bool
        doCleanup = isJust <$> getInput "cleanup"
        doUpload :: App Bool
        doUpload = ("POST" ==) <$> requestMethod
        doDefault :: App Bool
        doDefault = return True
