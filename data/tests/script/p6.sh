#!/bin/bash
echo "start P1"
echo $pwd
aga -a=P6.agda -t="removeAllCorrect : ∀ {A} (eq : A → A → Bool) → (x : A) → (xs : List A) → ∀ y →  y ∈ xs → y ≡ x → ¬ (y ∈ (removeAll eq x xs))"
