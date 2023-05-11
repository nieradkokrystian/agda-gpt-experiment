data Either (A B : Set) : Set where
 left : A → Either A B
 right : B → Either A B
