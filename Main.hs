{-# LANGUAGE OverloadedStrings #-}

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Web.Authenticate.OAuth
import Network.HTTP.Conduit (withManager,simpleHttp)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import Control.Monad
import Control.Lens
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Lazy as B

main :: IO ()
main =
    withManager $ \mgr -> do
        sourceWithMaxId twInfo mgr (userTimeline (ScreenNameParam "ENTERHERE") & count ?~ 200 & includeRts ?~ False & excludeReplies ?~ True)
            $$ CL.mapM_ $ \status -> liftIO $ processEntities status

processEntities status =
    processMedia $ status ^. statusExtendedEntities ^? _Just . enMedia

processMedia = maybe (return ()) process
  where process (x:xs) = do
            let url = x ^. entityBody . meMediaURL
                fileName = getFileName url
            T.putStrLn $ "Downloading: " <> fileName
            file <- simpleHttp $ T.unpack $ url <> ":orig"
            B.writeFile (T.unpack fileName) file
            process xs
        process [] = return ()

getFileName = T.reverse . T.takeWhile(/= '/') . T.reverse

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = "ENTERHERE"
    , oauthConsumerSecret = "ENTERHERE"
    }

credential :: Credential
credential = Credential
    [ ("oauth_token", "ENTERHERE")
    , ("oauth_token_secret", "ENTERHERE")
    ]

twInfo :: TWInfo
twInfo = def
    { twToken = def { twOAuth = tokens, twCredential = credential }
    , twProxy = Nothing
    }

