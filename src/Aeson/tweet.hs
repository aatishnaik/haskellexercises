{-# LANGUAGE OverloadedStrings #-}
module Aeson.Tweet where

import Network.Wreq
import Web.Twitter.Conduit
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.List as DL
import Network.HTTP.Types.Status as HT
import Data.Aeson as DA
import Data.ByteString.Char8 as DB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Network.Wreq as NW
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import Control.Lens


{-instance FromJSON Response where
    parseJSON = withObject "User" $ \v -> Response
        <$> v .: "name"
        <*> v .: "age"-}

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = "FaPdykltzoxz65Tf9TH9ugIe4"
    , oauthConsumerSecret = "4yxJAXPQMQinymZI6CDgWNhMdLAT68zjucqvlpNeS7glDvSEtc"
    }
credential :: Credential
credential = Credential
    [ ("oauth_token", "976334890915397632-DpsHhYJiTKkQY7ip48JFcqK86xbrsBP")
    , ("oauth_token_secret", "a6C63vZ8sQmFFgQjm1KnRy4VYPKwRy1dfNcFb5nxliYMH")
    ]
twInfo :: TWInfo
twInfo = def
    { twToken = def { twOAuth = tokens, twCredential = credential }
    , twProxy = Nothing
    }

getUserDetails :: IO(Web.Twitter.Conduit.Response User)
getUserDetails = 
    do
        mgr <- newManager tlsManagerSettings
        callWithResponse twInfo mgr $ accountVerifyCredentials

getStatus :: IO ()
getStatus = do
    mgr <- newManager tlsManagerSettings
    sourceWithMaxId twInfo mgr homeTimeline
        $= CL.isolate 60
        $$ CL.mapM_ $ \status -> liftIO $ do
            T.putStrLn $ T.concat [ T.pack . show $ status ^. statusId
                                  , ": "
                                  , status ^. statusUser . userScreenName
                                  , ": "
                                  , status ^. statusText
                                  ]

userDetails recrd =
    recrd >>= \strRec -> 
        let
            sRec = show strRec
            opJson = DB.pack sRec
        in case (decode (BL.fromStrict opJson)) of
            Just val -> val
            Nothing -> ""