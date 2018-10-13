module JsonExample where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text as T
import GHC.Generics

data Book = Book
  { title :: T.Text
  , author :: T.Text
  , year :: Int
  } deriving (Show, Generic)

instance FromJSON Book

instance ToJSON Book

myBook :: Book
myBook = Book {author = "Will Kurt", title = "Learn Haskell", year = 2017}

myBookJSON :: LC.ByteString
myBookJSON = encode myBook

rawJSON :: LC.ByteString
rawJSON =
  "{\"author\": \"Email Ciroan\", \"title\": \"A Short History of Decay\", \"year\": 1949}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

data ErrorMessage = ErrorMessage
  { message :: T.Text
  , error :: Int
  } deriving (Show, Eq)

-- The following won't work because "error" is already defined in Haskell.
-- instance FromJSON ErrorMessage
-- instance ToJSON ErrorMessage
--
-- :t (.:)
-- Data.Aeson (.:) :: (FromJSON a) => Object -> Text -> Parser a
-- v .: "messsage" tries to parse the message field from your JSON object.
-- The applicative context here is the Parser context.
instance FromJSON ErrorMessage where
  parseJSON (Object v) = ErrorMessage <$> v .: "message" <*> v .: "error"

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) =
    object ["message" .= message, "error" .= errorCode]

-- decode sampleError :: Maybe ErrorMessage
sampleError :: LC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

-- encode anErrorMessage
anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0
