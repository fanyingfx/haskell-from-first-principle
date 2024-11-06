module EqCaseGuard where

type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

type ValidatePerson a = Either [PersonInvalid] a

data PersonInvalid
  = NameEmpty
  | AgeTooLow

toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

instance Show PersonInvalid where
  show = toString

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = if age >= 0 then Right age else Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = if name /= "" then Right name else Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)
mkPerson'  :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk)(Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge
