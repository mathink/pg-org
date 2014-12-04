
Set Implicit Arguments.
Unset Strict Implicit.

Require Import List.
Import ListNotations.

Let pi: list nat := [ 3; 1; 4; 1; 5; 9; 2].

Fixpoint rev (A: Type)(l: list A): list A :=
  match l with
    | [] => []
    | x::xs => rev xs ++ [x]
  end.

Compute (rev pi).

Theorem rev_rev_id:
  forall (A: Type)(l: list A),
    rev (rev l) = l.

Proof.
  intros A l.

induction l as [| x xs IHl].

simpl; reflexivity.

simpl.

Fail rewrite IHl.

Lemma rev_app:
  forall (A: Type)(l1 l2: list A),
    rev (l1 ++ l2) = rev l2 ++ rev l1.

Proof.
  intros A l1 l2; induction l1 as [| x xs IHl];
  simpl; try (rewrite app_nil_r; reflexivity).

rewrite IHl, app_assoc; reflexivity.

Qed.

Show.

rewrite rev_app.

rewrite IHl; simpl.

reflexivity. Qed.
