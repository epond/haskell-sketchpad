module ReaderTransformer where

type Email = String
type UserName = String
type StreetAddress = String
type APIKey = String

data User = User { username :: UserName, email :: Email} deriving (Eq, Show)

getUser :: UserName -> APIKey -> Maybe User
getUser _ _ = Just $ User "Fred" "fred@mail.com"

getStreetAddress :: Email -> APIKey -> Maybe StreetAddress
getStreetAddress _ _ = Just "1 Acacia Road"

streetAddressDirectly :: Maybe StreetAddress
streetAddressDirectly = Nothing -- ???