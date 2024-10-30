module SingModule where


fstString :: [Char] -> [Char]
fstString x = x ++ " in ther rain     "

sndString :: [Char] -> [Char]
sndString x = x ++ " over ther rainbow"

myIf :: Bool->a->a->a
myIf cond x y = if cond then x else y 

sing :: String
sing = if x > y then fstString x else sndString y
  where
    x = "Someday"
    y = "Somewhere"