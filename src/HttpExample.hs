module HttpExample where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple

myToken :: BC.ByteString
myToken = "DxNMiwVSHZUePcEmfuJBqaEtnkNSRfXb"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest ::
     BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path =
  setRequestMethod method $
  setRequestHost host $
  setRequestHeader "token" [token] $
  setRequestPath path $
  setRequestSecure True $ setRequestPort 443 defaultRequest

buildRequestNoSsl ::
     BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequestNoSsl token host method path =
  setRequestMethod method $
  setRequestHost host $
  setRequestHeader "token" [token] $ setRequestPath path defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath
