-- write a small library in Haskell
-- JSON library
-- a JSON value can be either a string, an integer, a boolean or a special value called null
-- a JSON  is an array of JSON values
-- a JSON object is pair (string, JSON value)

module SimpleJSON 
	(
	JSONValue(..),
	getString,
	getInt,
	getDouble,
	getBool,
	isNull,
	getObject,
	getArray
	) where


data JSONValue = JString String
	   | JNumber Double
	   | JBool Bool
	   | JNull
	   | JArray [JSONValue]
	   | JObject (String, JSONValue)
	   deriving (Eq, Show, Ord)

getString:: JSONValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt:: JSONValue -> Maybe Int
getInt (JNumber d) = Just (truncate d)
getInt _ = Nothing

getDouble:: JSONValue -> Maybe Double
getDouble (JNumber d) = Just d
getDouble _ = Nothing

getBool:: JSONValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _ = Nothing

isNull:: JSONValue -> Bool
isNull v = v == JNull

getObject:: JSONValue -> Maybe (String, JSONValue)
getObject (JObject c) = Just c
getObject _ = Nothing

getArray:: JSONValue -> Maybe [JSONValue]
getArray (JArray ar) = Just ar
getArray _ = Nothing
