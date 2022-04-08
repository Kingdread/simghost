{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ImageHoster.Data
    ( Settings(..)
    , Seconds
    , Metadata(..)
    , isEternal
    , defaultSettings
    ) where

import Control.Arrow ((***))
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.KeyMap (toList, fromList)
import Data.Aeson.Key (toString, fromString)
import Data.Traversable
import GHC.Generics

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
        for (toList $ o) $ \(user, pw) -> do
            password <- parseJSON pw
            return (toString user, password)


instance ToJSON Settings where
    toJSON Settings{..} = object [ "outputDir" .= outputDir
                                 , "defaultTimeout" .= defaultDuration
                                 , "nameLength" .= nameLength
                                 , "users" .= formatUsers users
                                 ]
        where
            formatUsers :: [(String, String)] -> Object
            formatUsers = fromList . map (fromString *** toJSON)


-- | Return the default settings.
defaultSettings :: Settings
defaultSettings =  case fromJSON $ object [] of
                     Success a -> a
                     Error s -> error s
