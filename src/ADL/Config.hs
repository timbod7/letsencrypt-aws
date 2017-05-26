{-# LANGUAGE OverloadedStrings #-}
module ADL.Config(
    Config(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data Config = Config
    { config_domain :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkConfig :: T.Text -> Config
mkConfig domain = Config domain

instance AdlValue Config where
    atype _ = "config.Config"
    
    jsonGen = genObject
        [ genField "domain" config_domain
        ]
    
    jsonParser = Config
        <$> parseField "domain"