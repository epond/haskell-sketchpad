module ReaderTransformer where

import Control.Monad.Reader

type Email = String
type UserName = String
type StreetAddress = String
type APIKey = String

data User = User { username :: UserName, email :: Email} deriving (Eq, Show)

-- application configuration
apiKey :: APIKey
apiKey = "123ABC"

-- low level api functions
getUser :: UserName -> APIKey -> Maybe User
getUser username' _ = Just $ User username' "fred@mail.com"

getStreetAddress :: Email -> APIKey -> Maybe StreetAddress
getStreetAddress _ _ = Just "1 Acacia Road"

-- api functions wrapped in a Reader monad transformer
getUserReaderT :: UserName -> ReaderT APIKey Maybe User
getUserReaderT username' = do
    apikey <- ask
    lift $ getUser username' apikey

getStreetAddressReaderT :: Email -> ReaderT APIKey Maybe StreetAddress
getStreetAddressReaderT email' = do
    apikey <- ask
    lift $ getStreetAddress email' apikey