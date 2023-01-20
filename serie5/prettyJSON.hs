import SimplePrelude

data JSON = JNull
          | JBool Bool
          | JInt Int
          | JFloat Float
          | JString String
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving Show


test = JArray[JObject [("name", JString "meier"),
                       ("besuchte_kurse", JArray [JString "Logik", JString "Progammierung", JString "Comilerbau"]),
                       ("bachelor_note", JNull),
                       ("zugelassen", JBool True)],
              JObject [("name", JString "schmidt"),
                       ("besuchte_kurse", JArray [JString "Programmierung", JString "Informationssysteme"]),
                       ("bachelor_note", JFloat 2.7),
                       ("zugelassen", JBool False)]]


foldJSON :: g -> (Bool -> g) -> (Int -> g) -> (Float -> g) -> (String -> g) -> ([g] -> g) -> ([(String, g)] -> g) -> JSON -> g
foldJSON jnull jbool jint jfloat jstring jarray jobject json = case json of
                                                                    JNull          -> jnull
                                                                    (JBool a)      -> jbool a
                                                                    (JInt a)       -> jint a
                                                                    (JFloat a)     -> jfloat a
                                                                    (JString a)    -> jstring a
                                                                    (JArray a)     -> jarray (map ourfold a)                        -- Was macht map? JSON -> g für jedes Element der Liste (Alg. wendet für jedes Element in der Liste die Funktion an)
                                                                    (JObject a)    -> jobject (uncurry zip (process (unzip a)))     -- unzip: [(a, b)] -> ([a], [b]); zip  ([a], [b]) -> [(a, b)] -- uncurry (a -> b -> c) -> (a, b) -> c
 where process (a, b) = (a , map ourfold b)                                                                                         -- hmm lecker curry :: ((a, b) -> c) -> a -> b -> c  curry: [a] -> [b] -> [(a, b)]
       ourfold a = foldJSON jnull jbool jint jfloat jstring jarray jobject a


prettyJSON :: JSON -> String
prettyJSON = foldJSON jnull jbool jint jfloat jstring jarray jobject
  where jnull     = "null"
        jbool a   = if a then "True" else "False"
        jint a    = showInt a
        jfloat a  = showFloat a
        jstring a = '\"' : a ++ "\""
        jarray a  = '[' : commaSep (map prettyJSON a) ++ "]"
        jobject a = '{' : commaSep (map prettyKV a) ++ "}"
        -- Transform a list of strings into a comma separated string
        commaSep :: [String] -> String
        commaSep [] = ""
        commaSep s  = foldr1 (\s1 s2 -> s1 ++ ", " ++ s2) s
        prettyKV (k, v) = '\"' : k ++ "\": " ++ prettyJSON v
