data Child = Naughty | Nice
  deriving (Show,  Eq)-- Es gab keine Show Instanz für die Kinder, deshalb konnten die nicht ausgegebenen werden

data Wish a = Wish a-- Construktor hat gefehlt

data Present a = Box a | WrappingPaper (Present a) --a Fehlte bei dem Present im Wrappingpaper
  deriving Show -- Es gab keine Show Instanz für die Geschenke, deshalb konnten die nicht ausgegebenen werden

{- instance Eq Child where  --Instanz rekursiv auf == def.
  x == y = if x == Naughty
           then if y == Naughty then True else False
           else if y == Naughty then False else True -}

wrap :: Int -> a -> Present a
wrap 0 x = Box x
wrap n x = WrappingPaper (wrap (n - 1) x) -- Klammern fehlten (falsch gesetzt)

santa :: [(Child, Wish a)] -> [(Child, Maybe (Present a))]
santa xs = snd (foldr santasLittleHelper (0, []) xs)
  where santasLittleHelper = \(c, Wish x) (n,cs) ->
          let c' = if c == Naughty then (c, Nothing) else (c, Just (wrap n x))
          in (n + 1, c':cs) -- falsch angeordnet im Tupel