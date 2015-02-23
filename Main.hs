{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Data.Maybe
import Network.SimpleIRC
import Web.Twitter.Conduit
import Web.Twitter.Conduit.Response
import Data.Text.Encoding
import Network.HTTP.Conduit (withManager)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Web.Authenticate.OAuth as OA
import Control.Exception
import System.Process
import System.Exit
import System.Environment (getEnvironment)
import Test.QuickCheck (generate, elements)

commandPrefix = "%"

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
    let oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return (oauth, cred)
   where consumerKey = ""
         consumerSecret = ""
         accessToken = ""
         accessSecret = ""

getTWInfo :: IO TWInfo
getTWInfo = do
    (oa, cred) <- getOAuthTokens
    return $ (setCredential oa cred def)

onMessage :: EventFunc
onMessage server msg
   | B.pack (commandPrefix ++ "tweet") `B.isPrefixOf` mMsg msg = do
        twInfo <- getTWInfo
        do res <- try (withManager $ \mgr -> do call twInfo mgr $ update $ T.drop 7 (decodeUtf8 text))
           case res of
              Left (e :: TwitterError) -> sendMsg server chan "error"
              Right status -> sendMsg server chan "success"
   | B.pack (commandPrefix ++ "eval") `B.isPrefixOf` mMsg msg = do
     env <- getEnvironment
     (status,out,_) <- readProcessWithExitCode "mueval" options ""
     case status of
        ExitSuccess -> sendMsg server chan (B.pack out)
        ExitFailure{} -> return ()
   | B.pack (commandPrefix ++ "witzig") `B.isPrefixOf` mMsg msg = do
     r <- (generate . elements) ["Witzig!", "Nicht Witzig!"]
     sendMsg server chan r
   | B.pack (commandPrefix ++ "relefant") `B.isPrefixOf` mMsg msg = do
     r <- (generate . elements) ["relefant", "extrem relefant", "nicht relefant", "irrelefant"]
     sendMsg server chan r
   | otherwise = print msg
   where chan = fromJust $ mChan msg
         nick = fromJust $ mNick msg
         text = mMsg msg
         options = ["-t","5","--expression", B.unpack $ B.drop 6 text]

events = [(Privmsg onMessage)]

server :: IrcConfig
server = (mkDefaultConfig "irc.physicsporn.org" "Samyaza")
         { cChannels = ["#test"]
         , cEvents   = events
         }

main = do
   connect server False True

