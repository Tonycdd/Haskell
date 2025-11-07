import Prelude hiding (Semigroup, Monoid,(<>), mconcat,mempty,mappend)
import qualified Data.Foldable as Data
Data.Foldable
-- kak definirame klasove
-- classs
class Semigroup a where 
    --definirame funkciite
    (<>) :: a -> a -> a

-- definirame Monoid, kato elementut za koito go definirame e instanciq na semiGroup
class Semigroup a => Monoid a where
    mempty :: a
    
    mconcat :: [a] -> a
    mconcat = foldr (<>) mempty

-- pravim instanciq ot Semigroup za int
instance Semigroup Int where
    (<>) :: Int -> Int -> Int
    x <> y = x + y

instance Monoid Int where
    mempty :: Int
    mempty = 0

-- trik za definirane za subirane na proizvoilen monoid
newtype Sum a = Sum { 
    getSum :: a 
    }

instance Num a => Semigroup (Sum a) where
    (<>) :: Num a => Sum a -> Sum a -> Sum a
    (Sum x) <> (Sum y) = Sum $ x + y

instance Num a => Monoid (Sum a) where
    mempty :: Num a => Sum a
    mempty = Sum 0


instance Semigroup a => Semigroup (Maybe a) where 
     (<>) :: Semigroup a => Maybe a -> Maybe a -> Maybe a
     Nothing <> Nothing = Nothing
     a@(Just x) <> Nothing = a
     Nothing <> a@(Just x) = a
     (Just x) <> (Just y) = Just $ x <> y

instance Monoid a => Monoid (Maybe a) where 
     mempty :: Monoid a => Maybe a
     mempty = Nothing

-- pravi samo instanciq za desniq argument na Either koito e pravilnata stoinost 
instance Foldable (Either a) where
    foldr::(b->c->c) -> c -> Either a b -> c
    foldr op nv (Left, _) = nv
    foldr op nv (Right x) = op x nv 
