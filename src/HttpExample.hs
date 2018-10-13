module HttpExample where

import Control.Monad (forM_)
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

-- The following datatypes model the JSON data you can download from:
-- https://gist.github.com/willkurt/9dc14babbffea1a30c2a1e121a81bc0a
data NOAAResult = NOAAResult
  { uid :: T.Text
  , mindate :: T.Text
  , maxdate :: T.Text
  , name :: T.Text
  , datacoverage :: Float
  , resultId :: T.Text
  } deriving (Show, Eq)

-- Because the JSON data uses "id" instead of "resultId", you need to
-- make your own instance of FromJSON.
instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult <$> v .: "uid" <*> v .: "mindate" <*> v .: "maxdate" <*>
    v .: "name" <*>
    v .: "datacoverage" <*>
    v .: "mindate"

instance ToJSON NOAAResult where
  toJSON (NOAAResult uid mindate maxdate name datacoverage resultId) =
    object
      [ "uid" .= uid
      , "mindate" .= mindate
      , "maxdate" .= maxdate
      , "name" .= name
      , "datacoverage" .= datacoverage
      , "id" .= resultId
      ]

data Resultset = Resultset
  { offset :: Int
  , count :: Int
  , limit :: Int
  } deriving (Show, Generic)

instance FromJSON Resultset

instance ToJSON Resultset

newtype Metadata = Metadata
  { resultset :: Resultset
  } deriving (Show, Generic)

instance FromJSON Metadata

instance ToJSON Metadata

data NOAAResponse = NOAAResponse
  { metadata :: Metadata
  , results :: [NOAAResult]
  } deriving (Show, Generic)

instance FromJSON NOAAResponse

-- To make NOAAResponse an instance of ToJSON, all of its nested fields
-- have to be instances of ToJSON as well.
instance ToJSON NOAAResponse

printResults :: Either String [NOAAResult] -> IO ()
printResults (Left msg) = print msg
printResults (Right results) = do
  forM_ results (print . name)

-- This shows how to make a Sum type an instance of FormJSON and ToJSON
data IntList
  = EmptyList
  | Cons Int
         IntList
  deriving (Show, Eq, Generic)

instance FromJSON IntList

instance ToJSON IntList

intListExample :: IntList
intListExample = Cons 1 $ Cons 2 $ Cons 3 $ EmptyList

-- LC.putStrLn $ encode intListExample
-- {
--   "tag": "Cons",
--   "contents": [
--     1,
--     {
--       "tag": "Cons",
--       "contents": [
--         2,
--         {
--           "tag": "Cons",
--           "contents": [
--             3,
--             {
--               "tag": "EmptyList"
--             }
--           ]
--         }
--       ]
--     }
--   ]
-- }
--
main' :: IO ()
main' = do
  jsonData <- L.readFile "data.json"
  LC.putStrLn jsonData
  let noaaResponse = eitherDecode jsonData :: Either String NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults
