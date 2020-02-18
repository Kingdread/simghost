{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Description : Simple image hosting
Copyright   : (c) Daniel, 2020
License     : BSD-3
Maintainer  : kingdread@gmx.de
-}

module Lib
    ( -- * Data structures
      App
    , runApp
    , Settings(..)
    , defaultSettings
    , Metadata(..)
    , Seconds
      -- * Entry points
    , imgHostMain
    , handleUpload
    , handleCleanup
      -- * Other functions
    , checkAuthorization
    , randomName
    , validChars
    , indexPage
    , ask
    , asks
    , getImageData
    , saveFile
    ) where


import Data.ByteString.Char8 (pack)
import Control.Applicative
import Control.Monad
import Control.Monad.Reader hiding (ask, asks)
import Crypto.BCrypt
import Data.Aeson
import Data.Aeson.Types
import Data.FileEmbed
import Data.Maybe
import Data.Time.Clock.System
import Data.Traversable
import GHC.Generics
import Network.CGI
import System.Directory
import System.FilePath
import System.Random

import qualified Control.Monad.Reader
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

-- | We are dealing with seconds when talking about durations, hence the type
-- alias.
type Seconds = Integer


-- | Metadata of a stored file.
data Metadata = Metadata { creator :: String
                         -- ^ Username of the creator.
                         , createdAt :: Integer
                         -- ^ Unix timestamp of the creation time.
                         , endOfLife :: Integer
                         -- ^ Unix timestamp of the expiration time.
                         } deriving (Show, Eq, Generic, FromJSON, ToJSON)

isEternal :: Metadata -> Bool
isEternal Metadata{..} = createdAt == endOfLife


-- | Application settings, should be read from a file at startup.
data Settings = Settings { outputDir :: FilePath
                         -- ^ Output directory for new images.
                         , defaultDuration :: Seconds
                         -- ^ Default lifetime of new images.
                         , nameLength :: Int
                         -- ^ Length (in characters) of the random identifier for images.
                         , users :: [(String, String)]
                         -- ^ A list of (user, password) pairs.
                         --
                         -- Passwords are expected to be hashed using bcrypt.
                         } deriving (Show)


instance FromJSON Settings where
    parseJSON = withObject "settings" $ \o -> do
        outputDir <- o .:? "outputDir" .!= "img"
        defaultDuration <- o .:? "defaultDuration" .!= 0
        nameLength <- o .:? "nameLength" .!= 20
        users <- (o .:? "users" .!= object []) >>= parseUserDict
        return Settings{..}


parseUserDict :: Value -> Parser [(String, String)]
parseUserDict =
    withObject "users" $ \o ->
        for (HM.toList o) $ \(user, pw) -> do
            password <- parseJSON pw
            return (T.unpack user, password)


instance ToJSON Settings where
    toJSON Settings{..} = object [ "outputDir" .= outputDir
                                 , "defaultTimeout" .= defaultDuration
                                 , "nameLength" .= nameLength
                                 , "users" .= formatUsers users
                                 ]
        where
            formatUsers :: [(String, String)] -> Object
            formatUsers = HM.fromList . map (\(user, pw) -> (T.pack user, toJSON pw))


-- | Return the default settings.
defaultSettings :: Settings
defaultSettings =  case fromJSON $ object [] of
                     Success a -> a
                     Error s -> error s


-- | The application monad stack.
--
-- This includes the CGI monad, IO and a reader to access the (global)
-- application settings.
type App a = CGIT (ReaderT Settings IO) a


-- | Run the given 'App' with the given 'Settings'.
runApp :: Settings -> App CGIResult -> IO ()
runApp settings app = runReaderT (runCGI (handleErrors app)) settings


-- | HTML source code of the index page.
--
-- This is included at compile time from @src/index.html@.
indexPage :: String
indexPage = $(embedStringFile "src/index.html")


-- | A lifted version of 'Control.Monad.Reader.ask'.
--
-- Provides access to the 'Settings' instance in the 'App' monad stack.
ask :: App Settings
ask = lift Control.Monad.Reader.ask


-- | A lifted version of 'Control.Monad.Reader.asks'.
--
-- Provides access to the 'Settings' instance, but applies a function before
-- returning the value:
--
-- > directory <- asks outputDir
asks :: (Settings -> a) -> App a
asks f = fmap f ask


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
randomName = asks nameLength >>= \l ->
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
    dir <- asks outputDir
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
                   -> Maybe String
                   -- ^ The given username.
                   -> Maybe String
                   -- ^ The given password.
                   -> Bool
                   -- ^ The authorization result.
checkAuthorization users (Just user) (Just password) = case lookup user users of
                                                         Just p -> validatePassword (pack p) (pack password)
                                                         Nothing -> False
checkAuthorization _ _ _ = False


-- | Get the image data from the current request.
--
-- This first checks if some raw content is given in the @imagecontent@ field
-- (i.e. data from a pasted image), and afterwards checks @imagefile@ (i.e. the
-- file selected in the file chooser).
getImageData :: App (Maybe BS.ByteString)
getImageData = do
    rawData <- getInputFPS "imagecontent"
    case rawData of
      Just d | d /= "" -> return . rightToMaybe $ B64.decode d
      _ -> getInputFPS "imagefile"


cleanupFile :: FilePath -> App ()
cleanupFile fname = do
    dir <- asks outputDir
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
    dir <- asks outputDir
    files <- liftIO $ getDirectoryContents dir
    let metaFiles = filter ((== ".meta") . takeExtension) files
    forM_ metaFiles cleanupFile
    output "Cleaning up"


-- | Performs a file upload.
handleUpload :: App CGIResult
handleUpload = do
    users <- asks users
    outdir <- asks outputDir
    user <- getInput "username"
    password <- getInput "password"
    let authorized = checkAuthorization users user password
    if authorized then do
        filecontent <- getImageData
        filename <- getInputFilename "imagefile"
        duration :: Maybe Integer <- readInput "duration"
        let finfo = (,,) <$> filecontent <*> (filename <|> Just "") <*> duration
        case finfo of
          Just (content, name, dur) -> do
              savedFileName <- saveFile content (takeExtension name) dur (fromJust user)
              redirect $ outdir </> savedFileName
          Nothing -> do
              setStatus 400 "Missing data"
              output "Invalid request"
     else do
         setStatus 401 "Unauthorized"
         output "Invalid credentials"

-- | Main entry point of the application.
--
-- Depending on the given parameters, this either dispatches to
-- 'handleCleanup', 'handleUpload' or just returns the 'indexPage'.
imgHostMain :: App CGIResult
imgHostMain = do
    cleanup <- getInput "cleanup"
    case cleanup of
      Just _ -> handleCleanup
      Nothing -> do
          method <- requestMethod
          case method of
            "POST" -> handleUpload
            _ -> output indexPage
