{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson as JS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as LNE
import qualified Data.Text as T
import qualified Data.Text.IO as T

import ADL.Core
import ADL.Config
import Control.Applicative
import Control.Monad.Trans.AWS
import Control.Monad.IO.Class
import Control.Lens
import Data.Maybe(fromMaybe)
import Data.Monoid
import Network.AWS.Route53
import Network.AWS.S3
import Network.AWS.Data(fromText)
import System.Environment(getArgs, getEnv, getExecutablePath)
import System.Exit(exitWith,ExitCode(..))
import System.FilePath((</>))
import System.Process(callProcess)
import System.IO(stdout)


getCerts :: FilePath -> Config -> IO ()
getCerts configPath c = do
  exepath <- getExecutablePath
  let exe = (T.unpack . config_certbotPath) c
      basedir = (T.unpack . config_basedir) c
      email = (T.unpack . config_email) c
      domains = (T.unpack . T.intercalate "," . config_domains) c
      authhook = exepath <> " auth-hook " <> configPath
      cleanuphook = exepath <> " cleanup-hook " <> configPath
      args =
        [ "--manual"
        , "--config-dir", basedir </> "config"
        , "--work-dir", basedir </> "work"
        , "--logs-dir", basedir </> "logs"
        , "--preferred-challenges", "dns"
        , "--manual-auth-hook", authhook
        , "--manual-cleanup-hook", cleanuphook
        , "--manual-public-ip-logging-ok"
        , "--agree-tos"
        , "-n"
        , "-m", email
        , "-d", domains
        , "certonly"
        ]
  print args
  callProcess exe args

authHook :: Config -> IO ()
authHook c = do
  env <- mkAwsEnv
  (certbotDomain,certbotValidation,hostedZone) <- getDnsDetails c
  let batch = changeBatch (pure (change Create rset))
      rset = resourceRecordSet  ("_acme-challenge." <> certbotDomain) Txt
           & rrsTTL .~ Just 60
           & rrsResourceRecords .~ (Just (pure (resourceRecord certbotValidation)))
  runResourceT . runAWST env $ do
    resp <- send (changeResourceRecordSets hostedZone batch)
    let ready = getChange (resp ^. crrsrsChangeInfo ^. ciId)
    await resourceRecordSetsChanged ready
  return ()

cleanupHook :: Config -> IO ()
cleanupHook c = do
  env <- mkAwsEnv
  (certbotDomain,certbotValidation,hostedZone) <- getDnsDetails c
  let batch = changeBatch (pure (change Delete rset))
      rset = resourceRecordSet  ("_acme-challenge." <> certbotDomain) Txt
           & rrsTTL .~ Just 60
           & rrsResourceRecords .~ (Just (pure (resourceRecord certbotValidation)))
  runResourceT . runAWST env $ do
    send (changeResourceRecordSets hostedZone batch)
  return ()

getDnsDetails :: Config -> IO (T.Text,T.Text,ResourceId)
getDnsDetails c = do
  certbotDomain <- T.pack <$> getEnv "CERTBOT_DOMAIN"
  certbotValidation <- T.pack <$> getEnv "CERTBOT_VALIDATION"
  let hostedZone = case fromText (config_awsHostedZoneId c) of
        Left e -> error e
        Right hz -> hz
  return (certbotDomain,"\""<>certbotValidation<>"\"",hostedZone)

mkAwsEnv :: IO Env
mkAwsEnv = do
  env0 <- newEnv Discover
  l <- newLogger Debug stdout
  return (env0 & envLogger .~ l)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["get-certs", configpath] -> do
      config <- adlFromJsonFile' configpath
      getCerts configpath config
    ["auth-hook", configpath] -> do
      config <- adlFromJsonFile' configpath
      authHook config
    ["cleanup-hook", configpath] -> do
      config <- adlFromJsonFile' configpath
      cleanupHook config
    _ -> do
      T.putStrLn "Usage: letsencrypt-aws <config-file>"
      exitWith (ExitFailure 1)
