data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

infix 4 _≡_
infix 3 ¬

data ⊥ : Set where


¬_ : Set → Set
¬ P = P → ⊥



data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero + n = n
suc m + n = suc (m + n)

data List (A : Set) : Set where
  nil  : List A
  cons : A → List A → List A

infixr 5 _∷_
_∷_ : ∀ {A} → A → List A → List A
x ∷ xs = cons x xs

data Bool : Set where
 true : Bool
 false : Bool

infix 4 _≟_
_≟_ : ℕ → ℕ → Bool
zero ≟ zero = true
zero ≟ suc _ = false
suc _ ≟ zero = false
suc m ≟ suc n = m ≟ n

removeAll : {A : Set} → (A → A → Bool) → A → List A → List A
removeAll _ _ nil = nil
removeAll eq x (cons y ys) with eq x y
... | true = removeAll eq x ys
... | false = y ∷ removeAll eq x ys

infix 4 _∈_

data _∈_ {A : Set} (x : A) : List A → Set where
  here : ∀ {xs} → x ∈ cons x xs
  there : ∀ {y xs} → x ∈ xs → x ∈ cons y xs

