module HttpExample where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text as T
import GHC.Generics
import Network.HTTP.Simple

myToken :: BC.ByteString
myToken = "DxNMiwVSHZUePcEmfuJBqaEtnkNSRfXb"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

-- every setRequestXXX call takes an existing request object, makes some
-- changes, and returns a new request object.
-- "defaultRequest" is the start request object.
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
