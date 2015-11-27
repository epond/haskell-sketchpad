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

-- we can compose the low level api functions but must pass APIKey each time
streetAddressDirectly :: Maybe StreetAddress
streetAddressDirectly = do
    user <- getUser "Fred" apiKey
    getStreetAddress (email user) apiKey

-- wrap api functions in a Reader monad transformer
getUserReaderT :: UserName -> ReaderT APIKey Maybe User
getUserReaderT username' = do
    apikey <- ask
    lift $ getUser username' apikey

getStreetAddressReaderT :: Email -> ReaderT APIKey Maybe StreetAddress
getStreetAddressReaderT email' = do
    apikey <- ask
    lift $ getStreetAddress email' apikey

-- now we can still compose the wrapped api functions but we only need to provide
-- APIKey at the last moment
streetAddressReader :: Maybe StreetAddress
streetAddressReader =
    let readerMaybe = do
            user <- getUserReaderT "Fred"
            getStreetAddressReaderT (email user)
    in runReaderT readerMaybe apiKey