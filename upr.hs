import System.Win32 (heapValidate)
import Data.Text.Internal.Encoding.Utf32 (validate)
import Control.Exception (ErrorCall)

-- records - kato Student String String STring String Maybe String
-- po-udobni sa kato poleta, obache e vajno da se kaje che te sa funkcii, a ne imena na poleta  

data Student = Student {
    fn :: String,
    name :: String,
    email :: String,
    phone :: Maybe String
} deriving Show

data Validation = InvalidFnErr | InvalidNameErr | InvalidEmailErr | InvalidPhoneErr deriving Show

createStudent :: String -> String -> String -> Maybe String -> Either [Maybe Validation] Student
createStudent fn name email phone = 
    let invalid = [validateFn fn, validateName name,validateEmail email,validatePh phone]
    in if null invalid 
        then Right $ Student fn name email phone
        else Left invalid

    where
        validateFn :: String -> Maybe Validation
        validateFn fn 
            | length fn == 10 = Nothing
            | otherwise = Just InvalidFnErr
        
        validateName :: String -> Maybe Validation
        validateName name  
        -- words e funkciq koqto splitva otdelnite dumi na name
            | length (words name) == 3 = Nothing
            | otherwise = Just InvalidNameErr
        
        validateEmail :: String -> Maybe Validation
        validateEmail email 
            | not (null (dropWhile (/= '@') email )) && '.' `elem` tail( dropWhile (/= '@') email) = Nothing
            | otherwise = Just InvalidEmailErr
        
        validatePh :: Maybe String -> Maybe Validation
        validatePh  = maybe Nothing validate 
            where 
                validate :: String -> Maybe Validation
                validate phone 
                    | head phone == '0' && length phone == 10 = Nothing
                    | otherwise = Just InvalidPhoneErr
