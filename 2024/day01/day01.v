(** 1 December 2024 / Published on 13 December 2024 *)

(** This Coq/Rocq module contains a formalization and a solution for
the problem of the Advent of Code 2024 event, day 1
(see {{https://adventofcode.com/2024/day/1}}). *)

(** Coq/Rocq is a programming language and proof assistant in which one can
    specify problems formally and then mathematically prove that a given program
    implements the specification correctly. For more information about
    Coq/Rocq, see {{https://coq.inria.fr/}}. *)

(** * A Formalization *)

(** ** Part 1 *)

(** We take the liberty to omit the formalization of the parsing step.
    Instead, we assume that the input is a list of pairs of non-negative
    numbers. Although not strictly specified, all given test inputs
    are non-negative. Thus, we represent the input as an inhabitant of
    the type [list (nat * nat)]. *)

Definition Input: Set := list (nat * nat).

(** The problem interprets such inputs as two lists [l1] and [l2] of
equal length. By using the terms defined in the standard module [List],
we have [combine l1 l2 = input], or, equivalently, [(l1, l2) = split input].
*)

Require Import List.

(** Part 1 asks us to consider the pairs of corresponding numbers
from the two lists, where each list is considered in increasing order.
For each pair of numbers in the same position
in increasing order, we consider their _distance_ between the two numbers,
defined as the absolute difference of the two numbers.
The final answer for part 1 is the total distance, that is to say the
sum of all these respective distances. *)

(** The _distance_ between two numbers [a] and [b] is defined as
[|a - b|], that is to say [b - a] if [a < b], [a - b]
otherwise. *)

(** We would then like to know, given two numbers [a] and [b], whether
[a < b] or not. We call that _deciding_ whether [a < b] or not, and,
luckily for us, this property is decidable for every pair of numbers.
We don't need to prove it: it is already done for us in the module
[PeanoNat]. *)

Require Import PeanoNat.

Definition distance (a b: nat): nat :=
  if a <? b then b - a else a - b.

(** Note the distinction between [a < b], which is the _property_ (of
type [Prop]), that _a_ is stricty less than _b_, and the Boolean [a <?
b], which is of type [bool] and is equal to [true] if and only if
(iff) [a < b].  We will use the (standard) theorem [Nat.ltb_lt], which
states that [forall n m : nat, (n <? m) = true <-> n < m]. *)

Require Import PeanoNat.

(** To consider the two lists in increasing order, we will define the
lists [sorted1] and [sorted2], which are _permutations_ of the two
lists [l1] and [l2] (that is to say, lists that contain the same
elements, including the possible repetitions, but in a different
order). We will then specify that we are interested specifically in
_sorted_ permutations with respect to the order [le] (less than or
equal to, [<=]).  In other terms, the permutations should be in
increasing order.

The concepts of permutations of lists and sorted lists are defined in
the standard modules [Permutation] and [Sorted]. *)

Require Import Permutation.
Require Import Sorted.

(** The specification of part 1 is then as follows: a number [n] is a
solution of [input] if there exist two lists [l1] and [l2] such that
[combine l1 l2 = input], and there exists two increasingly-sorted
permutations [sorted1] and [sorted2] (of [l1] and [l2] respectively],
such that [n] is the sum of the distances between respective pairs of
numbers in the two sorted permutations (again, these pairs are
constructed with [combine], and then the compution of distances
is spanned over all these pairs with [map]). *)

Definition part1_spec (input: Input) (n: nat): Prop :=
  exists l1 l2 sorted1 sorted2,
  combine l1 l2 = input /\
  Permutation l1 sorted1 /\
  Permutation l2 sorted2 /\
  Sorted le sorted1 /\
  Sorted le sorted2 /\
  n = list_sum (map (fun '(a, b) => distance a b) (combine sorted1 sorted2)).

(** ** Part 2 *)

(** Each Advent of Code problem consists in two parts, which consider
the same input, but interpret it differently. For day 1, part 2 asks
to compute, for each number [n] of the first list (including possible
repetitions), the product of [n] with the number of occurrences of [n]
in the second list (note that they are other ways of describing this
number: proving formally that these other ways are all equivalent is
left as an exercise). *)

(** There are many ways to formalize the number of occurrences of a
certain value in a list.  We use here a general approach: we will
specify how a number equals to the number of elements in a set [A]
that satisfies a predicate [p] over [A] (that is to say, a function of
type [A -> Prop]). *)

(** The number of occurrences of [x] in a list [l] will then be the
number of indices [i] (over [nat]) such that [nth_error l i = Some x],
where [nth_error] returns [Some y] if [y] is the element of index [i]
in [l] (and [None] if [i] is outside the range of [l], hence the
suffix [_error]).  *)

(** To formalize that a number equals to the number of elements that
satisfies a given predicate [p], we first formalize the fact that a
number is the smallest (non-negative) number that satisfies a
predicate [p]. That is to say, [p n] is satisfied, and for all number
[m] that satisfies [p], we have [n <= m]. *)

Definition minimum (p: nat -> Prop) (n: nat): Prop := p n /\ forall m,
  p m -> n <= m.

(** A number [n] then equals to the number of elements that satisfies
a given predicate [p] if [n] is the smallest number such that there
exists a list [l] of length [n] such that every element [a] that
satisfies [p] is in [l].  Since [n] is the smallest of such numbers,
the list [l] will be the smallest of such lists: that means that this
list is without repetition and does not contain other elements that do
not satisfy [p] (we could prove these facts as an exercise).  *)

Definition prop_count_spec {A: Set} (p: A -> Prop): nat -> Prop :=
  minimum (fun n => exists l: list A, length l = n /\ forall a, p a ->
  In a l).

(** The specification of part 2 is then as follows: a number [n] is a
solution of [input] if there exist two lists [l1] and [l2] such that
[combine l1 l2 = input], and there exists a list [lcount] of the same
length than [l1] such that every pair of respective numbers [(a, b)]
in [combine l1 lcount] is such that [b] is the number of occurrences
of [a] in [lcount]. Then, [n] is the sum of the product of such pairs
[combine l1 lcount]. *)

Definition part2_spec (input: Input) (n: nat): Prop :=
  exists l1 l2 lcount,
  combine l1 l2 = input /\
  length lcount = length l1 /\
  (forall a b, In (a, b) (combine l1 lcount) ->
    prop_count_spec (fun i => nth_error l2 i = Some a) b) /\
  n = list_sum (map (fun '(a, count) => a * count) (combine l1 lcount)).

(** A Solution *)

(** To compute the sorted permutations, we will use the merge sort
algorithm defined in the standard library. *)

Require Sorting.Mergesort.

(** Then, a solution of part 1 just splits the input in two lists [l1]
and [l2], uses merge sort to compute the respective sorted permutations
[sorted1] and [sorted2], and then sums the respective distances
(obtained again with [combine]). *)

Definition part1 (input: Input): nat :=
  let (l1, l2) := split input in
  let sorted1 := Sorting.Mergesort.NatSort.sort l1 in
  let sorted2 := Sorting.Mergesort.NatSort.sort l2 in
  list_sum (map (fun '(a, b) => distance a b) (combine sorted1 sorted2)).

(** Part 2 requires to decide whether two numbers are equal: this
is stated in the standard module [Peano_dec]. *)

Require Import Peano_dec.

(** To compute [part2], we just sum over the product of every element
[a] of [l1] with the number of occurrences of [a] in [l2]. To count
the number of occurrences of an element in a list, we use [count_occ]
from the standard library. *)

Definition part2 (input: Input): nat :=
  let (l1, l2) := split input in
  list_sum (map (fun a => a * count_occ eq_nat_dec l2 a) l1).

(** * A Proof That This Solution Correctly Implements The Formalization *)

(** We first state that being sorted does not depend on how the order
is defined, as long as the orders are equivalent.  Indeed, the
specification of [part1] uses the propositional order [<], whereas
[merge sort] uses the order induced by the computational order [<?]
(and the formalization of merge sort then used the induced
propositional order [fun x y => x <? y = true], and the following
lemma states that being sorted by either propositional order is
indeed equivalent). *)

Lemma Sorted_morphism: forall {A} (R R': A -> A -> Prop) (l: list A),
    (forall a b, R a b -> R' a b) -> Sorted R l -> Sorted R' l.
Proof.
  intros A R R' l HR HSorted. induction HSorted.
  - apply Sorted_nil.
  - apply Sorted_cons.
    + exact IHHSorted.
    + destruct H.
      * apply HdRel_nil.
      * apply HdRel_cons. apply HR. exact H.
Qed.

(** Then the following theorem states that [part1] satisfies
[part1_spec] for every possible input. *)

Theorem part1_ok: forall input, part1_spec input (part1 input).
Proof.
  intro.
  set (ll := split input).
  set (l1 := fst ll).
  set (l2 := snd ll).
  set (sorted1 := Sorting.Mergesort.NatSort.sort l1).
  set (sorted2 := Sorting.Mergesort.NatSort.sort l2).
  exists l1. exists l2. exists sorted1. exists sorted2.
  repeat split.
  - apply split_combine. unfold l1. unfold l2. rewrite <- surjective_pairing. reflexivity.
  - apply Sorting.Mergesort.NatSort.Permuted_sort.
  - apply Sorting.Mergesort.NatSort.Permuted_sort.
  - apply (Sorted_morphism (fun a b => Nat.leb a b = true)).
    + apply Nat.leb_le.
    + apply Sorting.Mergesort.NatSort.Sorted_sort.
  - apply (Sorted_morphism (fun a b => Nat.leb a b = true)).
    + apply Nat.leb_le.
    + apply Sorting.Mergesort.NatSort.Sorted_sort.
  - unfold part1. rewrite (surjective_pairing (split input)). reflexivity.
Qed.

(** For part 2, we will state that being a member [(a, b)] of the list
of pairs [combine l (map f l)] actually means that [b = f a]. *)

Lemma combine_map_elim: forall A B (a: A) (l: list A) (f: A -> B) (b: B),
  In (a, b) (combine l (map f l)) -> b = f a.
Proof.
  intros A B a l f b H.
  induction l as [|hd tl IH].
  - inversion H.
  - destruct H. 
    + inversion H. reflexivity.
    + apply IH. apply H. 
Qed.

(** We then prove that [count_occ] actually counts the number of
indices [i] such that [nth_error l i = Some a]. *)

Lemma count_occ_spec:
  forall A (eq_dec: forall (a b: A), {a = b} + {a <> b}) (a: A) (l: list A),
    prop_count_spec (fun i => nth_error l i = Some a) (count_occ eq_dec l a).
Proof.
  intros A eq_dec a l. induction l as [|hd tl IH].
  - split.
    + exists nil. split.
      * reflexivity.
      * intros n H. destruct n.
        -- rewrite nth_error_nil in H. inversion H.
        -- rewrite nth_error_nil in H. inversion H.
    + intros m H. apply le_0_n.
  - destruct IH as [IHval IHmin].
    destruct (eq_dec hd a) as [eq | neq].
    + rewrite (count_occ_cons_eq _ _ eq). split.
      * destruct IHval as [l [IHlen IHin]].  exists (0 :: map S l). split.
        -- simpl. apply eq_S. rewrite length_map. exact IHlen.
        -- intros n H. destruct n.
           ** apply in_eq.
           ** apply in_cons. apply in_map. apply IHin. exact H.
      * intros m H. destruct H as [l0 [Hlen Hin]].
        set (l0' := map pred (filter (fun n => negb (n =? 0)) l0)).
        apply (Nat.le_lt_trans _ (length l0')).
        ++ apply IHmin. exists l0'. split.
           ** reflexivity.
           ** intros n Hn. rewrite (pred_Sn n). apply in_map. apply filter_In. split.
              --- apply Hin. exact Hn.
              --- apply Bool.negb_true_iff. apply Nat.eqb_neq. intro absurd. inversion absurd.
        ++ rewrite <- Hlen.
           rewrite <- (filter_length (fun n => negb (n =? 0)) l0).
           unfold l0'.
           rewrite length_map.
           apply Nat.lt_add_pos_r.
           apply Nat.neq_0_lt_0.
           intro Hlen0.
           apply length_zero_iff_nil in Hlen0.
           apply (in_nil (a := 0)).
           rewrite <- Hlen0.
           apply filter_In. split.
           ** apply Hin. rewrite eq. reflexivity.
           ** rewrite Bool.negb_involutive. apply Nat.eqb_refl.
    + split.
      * destruct IHval as [l [IHlen IHin]]. exists (map S l). split.
           ++ rewrite length_map. rewrite (count_occ_cons_neq _ _ neq). exact IHlen.
           ++ intros n H. destruct n.
              ** simpl in H. inversion H. contradiction.
              ** apply in_map. apply IHin. exact H.
      * intros m H. destruct H as [l0 [Hlen Hin]].
        set (l0' := map pred l0).
        rewrite <- Hlen.
        rewrite (count_occ_cons_neq _ _ neq). apply IHmin. exists l0'. split.
        -- unfold l0'. apply length_map.
        -- intros n Hn. rewrite (pred_Sn n). apply in_map. apply Hin.
           apply Hn.
Qed.

(* We then state that, given two functions [f] and [g],
applying [g] to all the pairs [(a, f a)] where [a] spans over a list [l]
equals to applying [fun a => g (a, f a)] to every element of [l].
We will use that when we will use [map] over [combine l lcount],
where [lcount] will be computed with [map f l], where [f] counts
the occurrences in the other list. *)

Lemma map_combine_map: forall A B C (l: list A) (f: A -> B) (g: A * B -> C),
  map g (combine l (map f l)) = map (fun a => g (a, f a)) l.
Proof.
  intros A B C l f g.
  induction l.
  - reflexivity.
  - simpl. rewrite IHl. reflexivity.
Qed.

(** Then the following theorem states that [part2] satisfies
[part2_spec] for every possible input. *)

Theorem part2_ok: forall input, part2_spec input (part2 input).
Proof.
  intro.
  set (ll := split input).
  set (l1 := fst ll).
  set (l2 := snd ll).
  exists l1. exists l2. exists (map (count_occ eq_nat_dec l2) l1).
  split.
  - apply split_combine. unfold l1. unfold l2. rewrite <- surjective_pairing. reflexivity.
  - split.
    + apply length_map.
    + split.
      * intros a b H. apply combine_map_elim in H.
        rewrite H. apply count_occ_spec.
      * unfold part2. fold ll. rewrite (surjective_pairing ll).
        rewrite map_combine_map.
        reflexivity.
Qed.
