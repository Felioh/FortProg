data JSON = JNull
          | JBool Bool
          | JInt Int
          | JFloat Float
          | JString String
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving Show

exampleJSON :: JSON
exampleJSON = JArray [JObject 
                        [("name", JString "meier"),
                        ("besuchte_kurse", JArray [JString "Logik", JString "Programmierung", JString "Compilerbau"]),
                        ("bachelor_note", JNull), 
                        ("zugelassen", JBool True)],
                      JObject
                        [("name", JString "schmidt"), 
                        ("besuchte_kurse", JArray [JString "Programmierung", JString "Informationssysteme"]),
                        ("backelor_note", JFloat 2.7),
                        ("zugelassen", JBool False)]]


foldJSON :: k -> (Bool -> k) -> (Int -> k) -> (Float -> k) -> (String -> k) -> ([k] -> k) -> ([(String,k)] -> k) -> JSON -> k
foldJSON null bool int float string list object jason = case jason of 
                                                      JNull           -> null
                                                      JBool b         -> bool b
                                                      JInt i          -> int i
                                                      JFloat f        -> float f
                                                      JString s       -> string s
                                                      JArray js       -> list (map (foldJSON null bool int float string list object) js)
                                                      JObject sjason  -> object (map (\(s,js) -> (s,foldJSON null bool int float string list object js)) sjason)