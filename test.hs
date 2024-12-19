class MyIsEqual a where
    isEqual :: a -> a -> Bool
    isEqual x y = not (isNotEqual x y)

    isNotEqual :: a -> a -> Bool
    isNotEqual x y = not (isEqual x y)

data MyEnum = A | B | C

instance MyIsEqual MyEnum

myEqTest = isEqual A B
