{-# LANGUAGE OverloadedStrings #-}

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
        res <- withManager $ \mgr -> do
            call twInfo mgr $ update $ T.drop 7 (decodeUtf8 text)
        print res
   | B.pack (commandPrefix ++ "eval") `B.isPrefixOf` mMsg msg = do
     env <- getEnvironment
     (status,out,_) <- readProcessWithExitCode "mueval" options ""
     case status of
        ExitSuccess -> sendMsg server chan (B.pack out)
        ExitFailure{} -> return ()
   | otherwise = print msg
   where chan = fromJust $ mChan msg
         nick = fromJust $ mNick msg
         text = mMsg msg
         options = ["-t","5","--expression", B.unpack $ B.drop 6 text]

events = [(Privmsg onMessage)]

server :: IrcConfig
server = (mkDefaultConfig "irc.physicsporn.org" "Samyaza")
         { cChannels = ["#asozialesnetzwerk"]
         , cEvents   = events
         }

main = do
   connect server False True

