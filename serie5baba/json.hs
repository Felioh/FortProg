data JSON = JNull
          | JBool Bool
          | JInt Int
          | JFloat Float
          | JString String
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving Show


Hallo = JArray [
            JObject[("Name", JString "meier"),
            ("besuchte_kurse", JArray[JString "Logik" , JString "Programmierung", JString "Compilerbau"]),
            ("zugelassen", JBool True)
            ]
    ]

{-   {
    "name": "meier",
    "besuchte_kurse": ["Logik", "Programmierung", "Compilerbau"],
    "bachelor_note": null,
    "zugelassen": true
  } -}


jsonExample :: JSON
jsonExample = JArray
     [ JObject [ ("name", JString "meier")
            , ("besuchte_kurse", JArray
                [ JString "Logik"
                , JString "Programmierung"
                , JString "Compilerbau"
                ]
              )
            , ("bachelor_note", JNull)
            , ("zugelassen", JBool True)
            ]

foldJSON :: element -> (Bool -> element) -> (Int -> element) -> (Float -> element) -> (String -> element) -> ([element] -> element) -> ([(String, element)] -> element) -> JSON -> element 
--WAS DAS???? ^|^ o|o
foldJSON JNull JBool JInt JFloat JString JArray JObject JSON = case JSON of 
        JNull           -> jnull
        JBool   b       -> jbool b 
        JInt    i       -> jint i
        JFloat  f       -> jfloat f
        JString s       -> jstring s
        JArray  a     -> jarray map (foldJSON JNull JBool JInt JFloat JString JArray JObject) a 
        JObject (x,y) -> jobject (map (x, foldJSON JNull JBool JInt JFloat JString JArray JObject y))

 
