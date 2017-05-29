{-# LANGUAGE OverloadedStrings #-}
module ADL.Config(
    Config(..),
    Verbosity(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data Config = Config
    { config_certbotPath :: T.Text
    , config_awsHostedZoneId :: T.Text
    , config_basedir :: T.Text
    , config_email :: T.Text
    , config_domains :: [T.Text]
    , config_verbosity :: Verbosity
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkConfig :: T.Text -> T.Text -> T.Text -> T.Text -> [T.Text] -> Config
mkConfig certbotPath awsHostedZoneId basedir email domains = Config certbotPath awsHostedZoneId basedir email domains Verbosity_quiet

instance AdlValue Config where
    atype _ = "config.Config"
    
    jsonGen = genObject
        [ genField "certbotPath" config_certbotPath
        , genField "awsHostedZoneId" config_awsHostedZoneId
        , genField "basedir" config_basedir
        , genField "email" config_email
        , genField "domains" config_domains
        , genField "verbosity" config_verbosity
        ]
    
    jsonParser = Config
        <$> parseField "certbotPath"
        <*> parseField "awsHostedZoneId"
        <*> parseField "basedir"
        <*> parseField "email"
        <*> parseField "domains"
        <*> parseFieldDef "verbosity" Verbosity_quiet

data Verbosity
    = Verbosity_quiet
    | Verbosity_noisy
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue Verbosity where
    atype _ = "config.Verbosity"
    
    jsonGen = genUnion (\jv -> case jv of
        Verbosity_quiet -> genUnionVoid "quiet"
        Verbosity_noisy -> genUnionVoid "noisy"
        )
    
    jsonParser
        =   parseUnionVoid "quiet" Verbosity_quiet
        <|> parseUnionVoid "noisy" Verbosity_noisy
        <|> parseFail "expected a Verbosity"