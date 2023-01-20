data JSON = JNull
          | JBool Bool
          | JInt Int
          | JFloat Float
          | JString String
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving Show

example = JObject [ ("name: ", JString "meier"), 
                    ("besuchte kurse: ", JArray [JString "Logik", JString "Programmierung", JString "COmpilerbau"]),
                    ("bachelor_note: ", JNull),
                    ("zugelassen: ", JBool True)
                  ]

foldJSON:: jnull jbool jint jfloat jstring jarray jobject json = case json of
           JNull     -> jnull
           JBool   b -> jbool b
           JInt    i -> jint i
           JFloat  f -> jfloat f
           JString s -> jstring s
           JArray a  -> jarray (map (foldJSON jnull jbool jint jfloat jstring jarray jobject) a)
           JObject o -> jobject (map (k, foldJSON jnull jbool jint jfloat jstring jarray jobject v) o)


--funktion -> JNull -> [JSON]