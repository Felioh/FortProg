data JSON = JNull
          | JBool Bool
          | JInt Int
          | JFloat Float
          | JString String
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving Show


a = JArray  [
    JObject [
        ("name", JString "meier"), 
        ("besuchte_kurse", JArray [JString "Logik", JString "Programmierung", JString "Compilerbau"]),
        ("bachelor_note", JNull),
        ("zugelassen", JBool True)
        ], 
    JObject [
       ("name", JString "schmidt"), 
       ("besuchte_kurse", JArray [JString "Programmierung", JString "Informationssysteme"]),
       ("bachelor_note", JFloat 2.7),
       ("zugelassen", JBool False) 
    ]
    ]

-- foldJSON :: JSON a -> 
