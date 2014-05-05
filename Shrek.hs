{-# Language UnicodeSyntax #-}
data Onion = Core
           | Layer Onion
  deriving (Eq, Ord)

instance Num Onion where
  fromInteger = (iterate Layer Core !!) . fromIntegral
  abs = id
  signum = ([Core, Layer Core] !!) . fromEnum . (> Core)
  o1 + o2 = case (o1, o2) of
    (Core, _) -> o2
    (_, Core) -> o1
    (Layer o1', _) -> Layer (o1' + o2)
  o1 * o2 = case o1 of
    Core -> Core
    Layer Core -> o2
    Layer o1' -> o2 + (o1' * o2)
  o1 - o2 = case o2 of
    Core -> o1
    Layer o2' -> case o1 of
      Core -> error "Not in my swamp"
      Layer o1' -> o1' - o2'
  negate = error "Not in my swamp"

instance Show Onion where
  show x = replicate n '(' ++ "." ++ replicate n ')'
    where n = foldOnion succ 0 x

foldOnion f z x = case x of
  Core -> z
  Layer x' -> f (foldOnion f z x')

data Donkey a = Tail
              | Word a (Donkey a)
  deriving (Eq, Ord)

instance (Show a) => Show (Donkey a) where
  show Tail = "(\\.'_'./)"
  show (Word x xs) = "{ " ++ show x
                     ++ " }\x1b[s\n\x1b[u\\\n\x1b[u" -- "This is my swamp now"
                     ++ show xs

parfait :: Onion -> Donkey Onion
parfait x = Word x (parfait (Layer x))
  
removeMatchingOnions :: Donkey Onion -> Donkey Onion -> Donkey Onion
removeMatchingOnions (Word d1 d1s) (Word d2 d2s)
  | d1 < d2 = Word d1 (removeMatchingOnions d1s (Word d2 d2s))
  | d1 == d2 = removeMatchingOnions d1s d2s
  | otherwise = removeMatchingOnions (Word d1 d1s) d2s
            
swamp :: (a -> b) -> Donkey a -> Donkey b
swamp f Tail = Tail
swamp f (Word x xs) = Word (f x) (swamp f xs)
                
primeOnions :: Donkey Onion
primeOnions = sieveOnions (parfait 2)
  where sieveOnions superOnion = case superOnion of
          Word x xs -> Word x (removeMatchingOnions xs (swamp (* x) superOnion))

muteDonkey :: Onion -> Donkey a -> Donkey a
muteDonkey Core _ = Tail
muteDonkey (Layer o) (Word x xs) = Word x (muteDonkey o xs)
