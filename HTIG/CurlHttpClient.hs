-- Copyright (c) 2009, Diego Souza
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--   * Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright notice,
--     this list of conditions and the following disclaimer in the documentation
--     and/or other materials provided with the distribution.
--   * Neither the name of the <ORGANIZATION> nor the names of its contributors
--     may be used to endorse or promote products derived from this software
--     without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-- | A type class that is able to perform HTTP requests.
module HTIG.CurlHttpClient
       ( CurlHttpClient(..)
       ) where

import Control.Arrow
import Network.Curl
import Control.Monad.Fix
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import qualified Network.OAuth.Http.HttpClient
import Control.Monad.Trans
import Data.Char (chr,ord)
import Network.URI
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

import qualified Paths_htig as P (version)
import Data.Version (showVersion)

data CurlHttpClient = CurlHttpClient

instance Network.OAuth.Http.HttpClient.HttpClient CurlHttpClient where

  runClient _ req = liftIO $ withCurlDo $ do { c <- initialize
                                             --; print opts
                                             ; setopts c opts
                                             ; rsp <- perform_with_response_ c
                                             ; case (respCurlCode rsp)
                                               of errno
                                                    | errno `elem` successCodes -> return $ Right (fromResponse rsp)
                                                    | otherwise                 -> return $ Left (show errno)
                                             }
    where httpVersion = case (version req)
                        of Http10 -> HttpVersion10
                           Http11 -> HttpVersion11
                           
          successCodes = [ CurlOK
                         , CurlHttpReturnedError
                         ]

          url = case method req of
            POST -> showURL (req {qString = fromList []})
            _ -> showURL req

          curlMethod = case (method req)
                       of GET   -> [ CurlHttpGet True ]
                          POST  -> [ CurlPost True ]
                          PUT   -> [ CurlPut True ]
                          HEAD  -> [ CurlNoBody True,CurlCustomRequest "HEAD" ]
                          other -> if (B.null.reqPayload $ req)
                                   then [ CurlHttpGet True,CurlCustomRequest (show other) ]
                                   else [ CurlPost True,CurlCustomRequest (show other) ]

          curlPostData = case method req of
            POST -> [ CurlPostFields (map postopt . toList . qString $ req) ]
            _ -> []
          postopt (k, v) = escapeURIString isUnreserved k ++ "=" ++
                           escapeURIString isUnreserved v

          curlHeaders = let headers = (map (\(k,v) -> k++": "++v).toList.reqHeaders $ req)
                        in [ CurlHttpHeaders $ "Expect: " 
                                             : ("Content-Length: " ++ (show.B.length.reqPayload $ req))
                                             : headers
                           ]

          opts = [ CurlURL (showURL req)
                 , CurlHttpVersion httpVersion
                 , CurlLowSpeedTime 60
                 , CurlLowSpeed 1
                 , CurlUserAgent $ "htig-" ++ showVersion P.version
                 , CurlHeader False
                 , CurlSSLVerifyHost 1
                 , CurlSSLVerifyPeer False
                 , CurlTimeout 30
                 ] ++ curlHeaders
                   ++ curlMethod 
                   ++ curlPostData
          
          fromResponse rsp = RspHttp (respStatus rsp) (respStatusLine rsp) (fromList.respHeaders $ rsp) (B.pack.map (fromIntegral.ord).respBody $ rsp)
