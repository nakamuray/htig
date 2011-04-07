-- | A type class that is able to perform HTTP requests.
module HTIG.EnumHttpClient
       ( EnumHttpClient(..)
       ) where

import Control.Arrow
import Network.HTTP.Enumerator as HE
import Network.OAuth.Http.Request as OR
import Network.OAuth.Http.Response
import qualified Network.OAuth.Http.HttpClient
import Control.Monad.Trans
import Data.CaseInsensitive (original, mk)
import qualified Data.ByteString.Char8 as BC

import qualified Paths_htig as P (version)
import Data.Version (showVersion)

data EnumHttpClient = EnumHttpClient

instance Network.OAuth.Http.HttpClient.HttpClient EnumHttpClient where

  runClient _ req = liftIO $ do
      req' <- parseUrl url
      let req'' = req' { HE.method = meth
                       , requestHeaders = hdrs
                       , requestBody = body
                       }
      rsp <- withManager $ httpLbs req''
      case statusCode rsp of
          code | 200 <= code && code < 300 -> return $ Right (fromResponse rsp)
               | otherwise                 -> return $ Left (show code)

    where
      url = case OR.method req of
          POST -> showURL (req {qString = fromList []})
          _    -> showURL req

      meth = BC.pack . show $ OR.method req

      body = case OR.method req of
          POST -> RequestBodyBS $ BC.pack . show $ qString req
          _    -> RequestBodyBS $ BC.pack ""

      hdrs = (mk $ BC.pack "User-Agent", BC.pack $ "htig-" ++ showVersion P.version)
           : (map (mk . BC.pack *** BC.pack) $ toList $ reqHeaders req)

      fromResponse rsp = RspHttp (statusCode rsp)
                                 (show $ statusCode rsp)
                                 (fromList . map (BC.unpack . original *** BC.unpack) $ responseHeaders rsp)
                                 (responseBody rsp)
