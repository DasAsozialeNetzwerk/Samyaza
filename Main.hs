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
   | B.pack "%tweet" `B.isPrefixOf` mMsg msg = do
        twInfo <- getTWInfo
        res <- withManager $ \mgr -> do
            call twInfo mgr $ update $ T.drop 7 (decodeUtf8 text)
        print res
   | otherwise = print msg
   where chan = fromJust $ mChan msg
         nick = fromJust $ mNick msg
         text = mMsg msg

events = [(Privmsg onMessage)]

server :: IrcConfig
server = (mkDefaultConfig "irc.physicsporn.org" "Samyaza")
         { cChannels = ["#asozialesnetzwerk"]
         , cEvents   = events
         }

main = do
   connect server False True

