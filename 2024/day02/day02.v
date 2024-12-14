(** 2 December 2024 / Published on 14 December 2024 *)

(** This Coq/Rocq module contains a formalization and a solution for
the problem of the Advent of Code 2024 event, day 2
(see {{https://adventofcode.com/2024/day/1}}). *)

(** See the formalization of day 1
({{https://github.com/thierry-martinez/advent-of-code/blob/main/2024/day01/day01.v}})
for more information about Coq/Rocq and the approach. *)

(** * A Formalization *)

(** ** Part 1 *)

Require Import List.
Import ListNotations.

Fixpoint pairwise (p: nat -> nat -> Prop) (l: list nat): Prop :=
  match l with
  | [] | [_] => True
  | a :: (b :: _) as tl => p a b /\ pairwise p tl
  end.

Definition increasing: list nat -> Prop := pairwise (fun a b => a < b).

Definition decreasing: list nat -> Prop := pairwise (fun a b => a > b).

Require Import Arith.PeanoNat.

Definition distance (n m: nat): nat :=
  if n <? m then m - n else n - m.

Definition bounded_predicate (a b: nat): Prop :=
  let d := distance a b in 1 <= d <= 3.

Definition bounded_variation :=
  pairwise bounded_predicate.

Definition safe (l: list nat): Prop :=
  (increasing l \/ decreasing l) /\ bounded_variation l.

(** ** Part 2 *)

Definition exists_sublist (p: list nat -> Prop) (l: list nat): Prop :=
  exists l1 x l2, l1 ++ [x] ++ l2 = l /\ p (l1 ++ l2).

Definition quasisafe (l: list nat): Prop :=
  safe l \/ exists_sublist safe l.

(** A Solution *)

Fixpoint pairwise_bool (p: nat -> nat -> bool) (l: list nat): bool :=
  match l with
  | [] | [_] => true
  | a :: (b :: _) as tl => p a b && pairwise_bool p tl
  end.

Definition bounded (d: nat): bool :=
  (1 <=? d) && (d <=? 3).

Definition is_increasing_safe_predicate (a b: nat): bool :=
  (a <? b) && bounded (b - a).

Definition is_increasing_safe: list nat -> bool :=
  pairwise_bool is_increasing_safe_predicate.

Definition is_decreasing_safe_predicate (a b: nat): bool :=
  (b <? a) && bounded (a - b).

Definition is_decreasing_safe: list nat -> bool :=
  pairwise_bool is_decreasing_safe_predicate.

Definition is_safe (l: list nat): bool :=
  match l with
  | [] | [_] => true
  | a :: b :: _ =>
      if a <? b then
        is_increasing_safe l
      else
        is_decreasing_safe l
  end.

Fixpoint find_sublist_rec (p: list nat -> bool) (l acc: list nat): bool :=
  match l with
  | [] => false
  | hd :: tl =>
      p (rev_append acc tl) || find_sublist_rec p tl (hd :: acc)
  end.

Definition find_sublist (p: list nat -> bool) (l: list nat): bool :=
  find_sublist_rec p l [].

Definition is_quasisafe (l: list nat): bool :=
  match l with
  | [] => true
  | _ => find_sublist is_safe l
  end.

(** * A Proof That This Solution Correctly Implements The Formalization *)

Require Import Bool.

Lemma pairwise_if_pairwise_bool:
  forall pp: nat -> nat -> Prop,
  forall pb: nat -> nat -> bool,
  (forall a b, pb a b = true -> pp a b) ->
  forall l: list nat, pairwise_bool pb l = true ->
  pairwise pp l.
Proof.
  intros pp pb pp_if_pb l. induction l; intro H.
  - exact I.
  - destruct l.
    + exact I.
    + apply andb_true_iff in H. destruct H as [pb_hd pb_tl].
      * split.
        -- apply pp_if_pb. exact pb_hd.
        -- apply IHl. exact pb_tl.
Qed.

Lemma safe_if_is_increasing_safe:
  forall l: list nat, is_increasing_safe l = true -> safe l.
Proof.
  split.
  - left. apply (pairwise_if_pairwise_bool _ is_increasing_safe_predicate).
    + intros a b Hi. apply andb_true_iff in Hi. destruct Hi as [a_lt_b ba_bounded].
      apply Nat.ltb_lt. exact a_lt_b.
    + exact H.
  - apply (pairwise_if_pairwise_bool _ is_increasing_safe_predicate).
    + intros a b Hi. apply andb_true_iff in Hi. destruct Hi as [a_lt_b ba_bounded].
      apply andb_true_iff in ba_bounded. destruct ba_bounded as [ba1 ba3].
      unfold bounded_predicate. unfold distance. rewrite a_lt_b. split.
      * apply Nat.ltb_lt. exact ba1.
      * apply Nat.leb_le. exact ba3.
    + exact H.
Qed.

Lemma bounded_if_bounded_predicate_increasing:
  forall a b: nat, bounded_predicate a b -> a < b -> bounded (b - a) = true.
Proof.
  intros a b bp a_lt_b.
  unfold bounded_predicate in bp. unfold distance in bp.
  apply Nat.ltb_lt in a_lt_b. rewrite a_lt_b in bp.
  destruct bp as [bp1 bp3]. apply andb_true_iff. split.
  - apply Nat.ltb_lt. exact bp1.
  - apply Nat.leb_le. exact bp3.
Qed.

Lemma lt_not_ltb: forall a b, a < b -> b <? a = false.
Proof.
  intros a b H.
  apply Nat.ltb_ge.
  apply le_S_n.
  apply Nat.le_le_succ_r.
  exact H.
Qed.

Lemma bounded_if_bounded_predicate_decreasing:
  forall a b: nat, bounded_predicate a b -> b < a -> bounded (a - b) = true.
Proof.
  intros a b bp b_lt_a.
  unfold bounded_predicate in bp. unfold distance in bp.
  apply lt_not_ltb in b_lt_a. rewrite b_lt_a in bp.
  destruct bp as [bp1 bp3]. apply andb_true_iff. split.
  - apply Nat.ltb_lt. exact bp1.
  - apply Nat.leb_le. exact bp3.
Qed.

Lemma is_increasing_safe_if_increasing_and_bounded:
  forall l: list nat, increasing l -> bounded_variation l -> is_increasing_safe l = true.
Proof.
  intros l Hinc Hbounded. induction l.
  - reflexivity.
  - destruct l.
    + reflexivity.
    + destruct Hinc as [a_lt_n Hinc].
      destruct Hbounded as [an_bounded Hbounded].
      apply andb_true_iff. split.
      * apply andb_true_iff. split.
        -- apply Nat.ltb_lt. exact a_lt_n.
        -- apply bounded_if_bounded_predicate_increasing.
          ++ exact an_bounded.
          ++ exact a_lt_n.
      * apply IHl.
        -- exact Hinc.
        -- exact Hbounded.
Qed.

Lemma is_decreasing_safe_if_decreasing_and_bounded:
  forall l: list nat, decreasing l -> bounded_variation l -> is_decreasing_safe l = true.
Proof.
  intros l Hdec Hbounded. induction l.
  - reflexivity.
  - destruct l.
    + reflexivity.
    + destruct Hdec as [a_gt_n Hdec].
      destruct Hbounded as [an_bounded Hbounded].
      apply andb_true_iff. split.
      * apply andb_true_iff. split.
        -- apply Nat.ltb_lt. exact a_gt_n.
        -- apply bounded_if_bounded_predicate_decreasing.
          ++ exact an_bounded.
          ++ exact a_gt_n.
      * apply IHl.
        -- exact Hdec.
        -- exact Hbounded.
Qed.

Lemma safe_if_is_decreasing_safe:
  forall l: list nat, is_decreasing_safe l = true -> safe l.
Proof.
  split.
  - right. apply (pairwise_if_pairwise_bool _ is_decreasing_safe_predicate).
    + intros a b Hi. apply andb_true_iff in Hi. destruct Hi as [b_lt_a ab_bounded].
      apply Nat.ltb_lt. exact b_lt_a.
    + exact H.
  - apply (pairwise_if_pairwise_bool _ is_decreasing_safe_predicate).
    + intros a b Hi. apply andb_true_iff in Hi. destruct Hi as [b_lt_a ab_bounded].
      apply andb_true_iff in ab_bounded. destruct ab_bounded as [ab1 ab3].
      unfold bounded_predicate. unfold distance.
      apply Nat.ltb_lt in b_lt_a.
      apply lt_not_ltb in b_lt_a.
      rewrite b_lt_a. split.
      * apply Nat.leb_le. exact ab1.
      * apply Nat.leb_le. exact ab3.
    + exact H.
Qed.

Lemma empty_is_safe: safe [].
Proof.
  split.
  - left. exact I.
  - exact I.
Qed.

Lemma singleton_is_safe: forall n: nat, safe [n].
Proof.
  split.
  - left. exact I.
  - exact I.
Qed.

Lemma is_safe_iff_safe: forall l: list nat, is_safe l = true <-> safe l.
Proof.
  split; intro H.
  - destruct l.
    + exact empty_is_safe.
    + destruct l.
      * apply singleton_is_safe.
      * simpl in H. destruct (n <? n0).
        -- apply safe_if_is_increasing_safe. exact H.
        -- apply safe_if_is_decreasing_safe. exact H.
  - destruct l.
    + reflexivity.
    + destruct l.
      * reflexivity.
      * simpl. destruct H as [[Hinc | Hdec] Hbounded].
        -- replace (n <? n0) with true.
          ++ apply is_increasing_safe_if_increasing_and_bounded.
            ** exact Hinc.
            ** exact Hbounded.
          ++ destruct Hinc as [n_lt_n0 inc_l].
            apply Nat.ltb_lt in n_lt_n0. rewrite n_lt_n0. reflexivity.
        -- replace (n <? n0) with false.
          ++ apply is_decreasing_safe_if_decreasing_and_bounded.
            ** exact Hdec.
            ** exact Hbounded.
          ++ destruct Hdec as [n_gt_n0 dec_l].
            apply lt_not_ltb in n_gt_n0.
            rewrite n_gt_n0. reflexivity.
Qed.

Lemma find_sublist_rec_exists:
  forall (p: list nat -> bool) (l acc: list nat),
    find_sublist_rec p l acc = true <->
      exists_sublist (fun sublist => p (rev_append acc sublist) = true) l.
Proof.
  induction l; intro acc; split; intro H.
  - discriminate H.
  - destruct H as [l1 [x [l2 [Hl1_x_l2 Hsafe]]]].
    symmetry in Hl1_x_l2.
    apply app_cons_not_nil in Hl1_x_l2.
    contradiction.
  - unfold find_sublist_rec in H.
    refine ((if p (rev_append acc l) as b return p (rev_append acc l) = b -> _ then _ else _) eq_refl); intro Hp.
    + exists []. exists a. exists l. split.
      * reflexivity.
      * exact Hp.
    + apply orb_true_iff in H. destruct H.
      * rewrite H in Hp. discriminate Hp.
      * apply IHl in H.
        destruct H as [l1 [x [l2 [Hl1_x_l2 Hp']]]].
        exists (a :: l1). exists x. exists l2. split.
        -- simpl. simpl in Hl1_x_l2. rewrite Hl1_x_l2. reflexivity.
        -- simpl. simpl in Hp'. exact Hp'.
  - destruct H as [l1 [x [l2 [Hl1_x_l2 Hp']]]].
    apply orb_true_iff.
    destruct l1.
    + left. inversion Hl1_x_l2. rewrite H1 in Hp'.
      exact Hp'.
    + right. apply IHl. simpl in Hl1_x_l2. inversion Hl1_x_l2.
      exists l1. exists x. exists l2. split.
      * reflexivity.
      * simpl. rewrite H0 in Hp'. exact Hp'.
Qed.

Lemma is_quasisafe_iff_quasisafe:
  forall l: list nat, is_quasisafe l = true <-> quasisafe l.
Proof.
  intro l. destruct l as [|a tlb].
  - split; intro H.
    + left. exact empty_is_safe.
    + reflexivity.
  - split; intro H.
    + right. apply find_sublist_rec_exists in H. destruct H as [l1 [x [l2 [Hcons His_safe]]]].
      exists l1. exists x. exists l2. split.
      * exact Hcons.
      * apply is_safe_iff_safe. exact His_safe.
    + apply find_sublist_rec_exists. destruct H as [Hsafe | Hquasi].
      * exists []. exists a. exists tlb. split.
        -- reflexivity.
        -- destruct tlb as [|b tlc].
           ++ apply is_safe_iff_safe. exact empty_is_safe.
           ++ destruct Hsafe as [[Hinc | Hdec] Hbounded].
             ** destruct Hinc as [_ Hinc]. destruct Hbounded as [_ Hbounded].
                apply is_safe_iff_safe. split.
                --- left. exact Hinc.
                --- exact Hbounded.
             ** destruct Hdec as [_ Hdec]. destruct Hbounded as [_ Hbounded].
                apply is_safe_iff_safe. split.
                --- right. exact Hdec.
                --- exact Hbounded.
      * destruct Hquasi as [l1 [x [l2 [Hcons His_safe]]]].
        exists l1. exists x. exists l2. split.
        -- exact Hcons.
        -- apply is_safe_iff_safe. exact His_safe.
Qed.
