open import Relation.Binary.PropositionalEquality
open import Relation.Nullary

open import Data.Product
open import Data.Empty
open import Data.List
open import Data.Unit
open import Data.Nat


fst⊥ : ∀ {A : Set} → ⊥ × A → ⊥ 
fst⊥ = proj₁

snd⊥ : ∀ {A : Set} → A × ⊥ → ⊥ 
snd⊥ = proj₂

elim⊥ : ∀ {A : Set} → ⊥ → A 
elim⊥ = λ ()


data State : Set where
 uninitialized : State
 counting : ℕ → State
 finished : State

data Msg : Set where
 initialize : ℕ → Msg
 increment : Msg

isFinished : State → Set
isFinished uninitialized = ⊥
isFinished (counting x) = ⊥
isFinished finished = ⊤

update : State → Msg → State
update uninitialized (initialize x) = counting x
update uninitialized increment = uninitialized
update (counting x) (initialize x₁) = counting x
update (counting x) increment = counting (suc x)
update finished x₁ = uninitialized