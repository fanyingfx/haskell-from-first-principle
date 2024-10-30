data Mood = Blah | Woot deriving (Show)

changeMood :: Mood -> Mood
changeMood Woot = Woot
changeMood _ = Woot