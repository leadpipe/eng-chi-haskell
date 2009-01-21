--file: ch05/SimpleJSON.hs
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: JValue -> Maybe Integer
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getArray :: JValue -> Maybe JValue
getArray (JArray a)   = Just a
getArray _            = Nothing

getArray :: JValue -> Maybe JValue
getObject (JObject o) = Just o
getObject _           = Nothing

isNull :: JValue -> Bool
isNull                = (JNull ==)

