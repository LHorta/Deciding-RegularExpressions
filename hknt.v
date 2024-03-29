(** Coq proofs for the bisimulation up to congruence technique for NFA *)
(* Copyright 2011-2012: Damien Pous. *)

Require Import List Relations Arith Morphisms PeanoNat.
Set Implicit Arguments.

(** * Basic definitions, facts about continuous functions *)

(** reversing the arguments of a function *)
Definition swap {A} {B} {C} (f: A -> B -> C) x y := f y x.

(** finite iterations of a function  *)
Fixpoint xiter {A} (f: A -> A) x n := 
  match n with 
    | O => x
    | S n => f (xiter f x n)
  end.

(** subsets of a set [X] *)
Definition set X := X -> Prop.

(** simple notion of finiteness, using lists *)
Definition finite X := forall X': set X, exists l, forall x, X' x <-> In x l.

(** notation for set equality / inclusion *)
Notation seq := (pointwise_relation _ iff).
Notation sincl := (pointwise_relation _ Basics.impl).
Infix "==" := seq (at level 80).
Infix "<=" := sincl.

(** notation for relation equality / inclusion *)
Definition rel X Y := X -> Y -> Prop.
Notation req := (pointwise_relation _ seq).
Notation rincl := (pointwise_relation _ sincl).
Infix "===" := req (at level 80).
Infix "<==" := rincl (at level 80).

Instance preorder_sincl X: @PreOrder (set X) sincl.
Proof. firstorder. Qed.

Instance preorder_rincl X Y: @PreOrder (rel X Y) rincl.
Proof. constructor. firstorder. intros R S T H H' x. transitivity (S x); auto. Qed.

Section basics.

 Context {X Y: Type}.

 (** squaring function: R -> RR *)
 Definition square (R: rel X X) x z := exists y, R x y /\ R y z. 
 (** union of two relations *)
 Definition cup (R S: rel X Y) x y := R x y \/ S x y.
 (** union of a denumerable family of relations *)
 Definition union (R: nat -> rel X Y) x y := exists n, R n x y.
 (** omega iteration of a function on relations *)
 Definition iter f (R: rel X Y) := union (xiter f R).

 (** is a sequence of relations increasing? *)
 Definition increasing (R: nat -> rel X Y) := 
   forall n m, le n m -> R n <== R m.

 (** does a function on relations extends its argument? *)
 Definition extensive f := forall R: rel X Y, R <== f R.

 (** the finite iterations of an extensive function form an increasing sequence *)
 Lemma increasing_xiter f: extensive f -> forall R, increasing (xiter f R).
 Proof.
   intros Hf R n m le. revert R. induction le; intros R x y H.
   apply H.
   apply Hf, IHle, H.
 Qed.

End basics.
Infix "\cup" := cup (at level 40).

(** simple facts about monotone functions *)
Notation monotone f := (Proper (rincl ==> rincl) f).

Instance cup_incr {X Y}: Proper (rincl ==> rincl ==> rincl) (@cup X Y).
Proof. firstorder. Qed.

Lemma monotone_cup {X Y X' Y'} (f g: rel X Y -> rel X' Y'): 
  monotone f -> 
  monotone g ->
  monotone (fun R => f R \cup g R).
Proof. intros. intros ? ? ?. apply cup_incr; auto. Qed.

Lemma monotone_xiter {X Y} (f: rel X Y -> rel X Y): 
  monotone f -> forall n,
  monotone (fun R => xiter f R n).
Proof. intro Hf. induction n. firstorder. intros R S H. apply Hf, IHn, H. Qed.

Lemma monotone_iter {X Y} (f: rel X Y -> rel X Y): 
  monotone f -> 
  monotone (iter f).
Proof. intros Hf R S H x y [n H']. exists n. revert H'. now apply monotone_xiter. Qed.


Section continuous.

 (** we say that a function is continuous if it preserves union of increasing sequences *)
 Definition continuous {X Y X' Y'} (f: rel X Y -> rel X' Y') := 
   forall R, increasing R -> f (union R) <== union (fun n => f (R n)).

 (** the pointwise union of two continuous functions is continuous *)
 Lemma continuous_cup {X Y X' Y'} (f g: rel X Y -> rel X' Y'): continuous f -> continuous g -> 
   continuous (fun R => cup (f R) (g R)).
 Proof. 
   intros Hf Hg R HR x y [H|H].
    apply Hf in H as [n H]; trivial. exists n; left; apply H.
    apply Hg in H as [n H]; trivial. exists n; right; apply H.
 Qed.

 (** constant functions are continuous  *)
 Lemma continuous_constant {X Y X' Y'} K: @continuous X Y X' Y' (fun _ => K).
 Proof. intros R _ x y H. exists 0. apply H. Qed.

 (** the identity function is continuous *)
 Lemma continuous_id {X Y}: @continuous X Y X Y id.
 Proof. intros R _ x y H. apply H. Qed.

 (** the reverse function is continuous *)
 Lemma continuous_swap {X Y}: @continuous X Y Y X swap.
 Proof. intros R _ x y [n H]. exists n. assumption. Qed.

Fixpoint max n m : nat :=
  match n, m with
    | 0, _ => m
    | S n', 0 => n
    | S n', S m' => S (max n' m')
  end.

(*Lemma max_sn_m : forall n m : nat, n > m -> max n m -> n.*)
 
 (** the squaring function is continuous  *)
 Lemma continuous_square {X}: @continuous X X X X square.
 Proof. 
   intros R HR x z [y [[n Hxy] [m Hyz]]]. exists (max n m), y. 
   split; eapply HR; try eassumption. (* auto with arith.*)



(*my job*)
induction n.
simpl.
auto with arith.

destruct IHn.



simpl.
unfold max.
simpl.
auto with arith.
simpl.
destruct IHn.
rewrite <- IHn.

induction m.
simpl.
reflexivity.
simpl.
rewrite IHm.
simpl.
rewrite <- IHn.
eapply H.
simpl.

apply Init.Nat.max.
auto with arith.
apply_subrelation.
assumption.
auto with arith.
intros.
f_equal.
elim n.
simpl.
split.
auto with arith.
apply IHn.
simpl.
eassumption.
eapply Hxy.
split.
tauto.

auto.
eapply Hxy.

 Qed.

 (** the omega iteration of a continuous and extensive function is a 
    pre-fixpoint for that function  *)
 Lemma continuous_iter {X Y} (f: rel X Y -> rel X Y): continuous f -> extensive f -> 
   forall R, f (iter f R) <== iter f R.
 Proof.
   intros Hf Hf' R x y H.
   apply Hf in H as [n H]. 
   exists (S n). apply H. 
   apply increasing_xiter, Hf'.
 Qed.

 (** iterating a function on a pre-fixpoint *)
 Lemma fixed_iter {X Y} (f: rel X Y -> rel X Y): 
   monotone f -> forall R, f R <== R -> iter f R <== R.
 Proof.
   intros Hf R H. 
   assert (G: forall n, xiter f R n <== R).
    induction n. reflexivity. simpl. now rewrite IHn.
   intros x y [n H']. eapply G, H'. 
 Qed.
 
End continuous.


(** * Bisimulation proof method for DFA  *)

Section Alphabet.

 (** we fix an alphabet [A]  *)
 Variable A: Set.

 (** a word is just a list of letters  *)
 Definition word := list A.

 (** ** DFA *)
 Section DFA.

  (** a DFA is given by:
     - a set [E] of states,
     - an output function [o],
     - a transition function [t]
     *)
  Variables E: Type.
  Variable o: E -> Prop.
  Variable t: E -> A -> E.

  (** state reached from [x] by reading the word [w] *)
  Fixpoint read x w := 
    match w with
      | nil => x
      | a::w => read (t x a) w
    end.

  (** is a word accepted by state [x]  *)
  Definition accept x w := o (read x w).

  (** two states are language equivalent if they accept the same words *)
  Definition equiv x y := accept x == accept y.

  (** corresponding notion of language inclusion *)
  Definition incl x y := accept x <= accept y.

  (* equivalence checking can obviously be reduced to inclusion checking *)
  Fact equivalence_from_inclusion: forall x y, equiv x y <-> incl x y /\ incl y x.
  Proof. firstorder. Qed.


  (** ** Bisimulations *)

  (** small generalisation of the notion of bisimulation,
     required later for up-to techniques  *)
  Definition progress (R S: relation E) := 
    forall x y, R x y -> (o x <-> o y) /\ forall a, S (t x a) (t y a).

  (** a bisimulation is a self-progressing relation  *)
  Definition bisimulation R := progress R R.

  (** language equivalence is a bisimulation  *)
  Lemma bisimulation_equiv: bisimulation equiv.
  Proof.
    intros x y H. split. apply (H nil).
    intros a w. apply (H (a::w)).
  Qed.

  (** bisimulations are contained in language equivalence  *)
  Lemma bisimulation_in_equiv: forall R, bisimulation R -> R <== equiv.
  Proof.
    intros R HR x y H w. revert x y H. 
    induction w; intros x y H. apply HR, H.
    apply IHw, HR, H. 
  Qed.

  (** the bisimulation proof method is sound and complete for language equivalence *)
  Theorem equiv_iff_bisim: forall x y, equiv x y <-> exists R, bisimulation R /\ R x y.
  Proof.
    split. intro H. eauto using bisimulation_equiv. 
    intros [R [HR H]]. revert H. now apply bisimulation_in_equiv. 
   Qed.

  (** Up-to techniques  *)

  Definition bisimulation_up_to f R := progress R (f R).

  (** a compatible function is a function that preserves progressions *)
  Definition compatible f := forall R S, progress R S -> progress (f R) (f S).

  (** technical lemmas about progressions and unions  *)
  Instance progress_incr: Proper (rincl --> rincl ++> Basics.impl) progress.
  Proof. intros R R' HR S S' HS H x y Hxy. apply HR, H in Hxy. firstorder. Qed.

  Lemma cup_progress R S T: progress R T -> progress S T -> progress (R \cup S) T.
  Proof. firstorder. Qed.

  Lemma progress_cup R S T: progress R S \/ progress R T -> progress R (S \cup T).
  Proof. firstorder. Qed.

  Lemma union_progress R S: (forall n, progress (R n) S) -> progress (union R) S.
  Proof. firstorder. Qed.

  Lemma progress_union n R S: progress R (S n) -> progress R (union S).
  Proof. firstorder. Qed.

  (** compatible functions yield correct up-to techniques  *)
  Theorem compatible_correct: 
    forall f, compatible f ->
    forall R, bisimulation_up_to f R -> R <== equiv.
  Proof.
    intros f Hf R HR x y Hxy.
    apply equiv_iff_bisim. exists (iter f R). split.
     clear x y Hxy. 
     apply union_progress. intro n. 
     apply progress_union with (S n). 
     induction n.
      assumption.
      apply Hf, IHn.
    exists O. exact Hxy.
  Qed.

  (** compositionality properties of compatible functions  *)
  Lemma compatible_id: compatible id.
  Proof (fun R S H => H).

  Lemma compatible_comp f g: compatible f -> compatible g -> compatible (fun R => f (g R)).
  Proof. intros Hf Hg R S H. apply Hf, Hg, H. Qed.

  Lemma compatible_cup f g: compatible f -> compatible g -> compatible (fun R => f R \cup g R).
  Proof. intros Hf Hg R S H. apply cup_progress; apply progress_cup; auto. Qed.

  Lemma compatible_union f: (forall n, compatible (swap f n)) -> compatible (fun R => union (f R)).
  Proof. 
    intros Hf R S H. 
    apply union_progress. intro n.
    apply progress_union with n. 
    apply Hf, H.
  Qed.

  Lemma compatible_xiter f: compatible f -> forall n, compatible (swap (xiter f) n).
  Proof.
    intro Hf. induction n; unfold swap.
     apply compatible_id.
     apply compatible_comp; assumption.
  Qed.

  Lemma compatible_iter f: compatible f -> compatible (iter f).
  Proof. intro Hf. apply compatible_union. intro n. apply compatible_xiter, Hf. Qed.

  (** ** Up to equivalence (Hopcroft and Karp)  *)

  (** the basic functions underlying the equivalence closure function are compatible *)

  Lemma compatible_eq: compatible (fun _ => eq).
  Proof. intros _ _ _ x y <-; tauto. Qed.

  Lemma compatible_equiv: compatible (fun _ => equiv).
  Proof. intros _ _ _. exact bisimulation_equiv. Qed.

  Lemma compatible_swap: compatible swap.
  Proof. intros R S H x y Hxy. apply H in Hxy. intuition. Qed.

  Lemma compatible_square: compatible square.
  Proof.
    intros R S H x z [y [Hxy Hyz]]. 
    apply H in Hxy. 
    apply H in Hyz. 
    firstorder congruence.
  Qed.

  (** defining equivalence closure from the above functions, 
     we immediately get that this function is compatible *)

  Definition e1 (R: relation E) := eq \cup swap R \cup square R \cup R.
  Definition e := iter e1.

  Lemma compatible_e1: compatible e1. 
  Proof.
    repeat apply compatible_cup.
     apply compatible_eq.
     apply compatible_swap.
     apply compatible_square.
     apply compatible_id.
  Qed.

  Lemma compatible_e: compatible e. 
  Proof. apply compatible_iter, compatible_e1. Qed.

  (** as a corollary, we get an abstract proof of correctness for Hopcroft
     and Karp's algorithm  *)
  Corollary HK: forall R, bisimulation_up_to e R -> R <== equiv.
  Proof. apply compatible_correct, compatible_e. Qed.


  (** characterisation of [e] as the (inductively defined) reflexive,
     symmetric, and transitive closure *)
  Section e_charact.

   Lemma continuous_e1: continuous e1.
   Proof.
     repeat apply continuous_cup.
     apply continuous_constant.
     apply continuous_swap.
     apply continuous_square.
     apply continuous_id.
   Qed.

   Lemma e1e: forall R, e1 (e R) <== e R.
   Proof. 
     apply continuous_iter.
     apply continuous_e1. 
     intros R x y H. right. assumption.
   Qed.

   Variable R: relation E.

   (** alternative definition of the equivalence closure, as an inductive definition *)
   Inductive eR: relation E :=
   | eR_refl: Reflexive eR
   | eR_sym: Symmetric eR
   | eR_trans: Transitive eR
   | eR_ext: subrelation R eR.

   (** [e] and [eR] coincide  *)
   Theorem e_eR: eR === e R.
   Proof.
    intros x y; split.
     induction 1.
      exists 1. left. left. left. reflexivity. 
      apply e1e. left. left. right. assumption.
      apply e1e. left. right. eexists. split; eassumption.
      exists 0. assumption.

     destruct 1 as [n H]. revert x y H. induction n; simpl; intros x y H.
      apply eR_ext, H.
      destruct H as [[[<-|H]|[z [Hxz Hzy]]]|H].
       apply eR_refl. 
       apply eR_sym. apply IHn. apply H. 
       apply eR_trans with z; apply IHn; assumption.
       apply IHn, H.
   Qed.

  End e_charact.    

 End DFA.

 (** * Extending the above techniques to NFA  *)


 Section NFA.

  (** ** NFA *)

  (** a NFA is given by:
     - a set [E] of states,
     - an output function [o],
     - a (non-deterministic) transition function [t]
     *)
  Variable E: Type.
  Variable o: E -> Prop.
  Variable t: E -> A -> set E.

  (** ** Powerset construction *)

  (** [(E',o',t')] is the determinised NFA of [(E,o,t)]  *)
  Definition E' := set E.
  Definition o' (X: E') := exists x, X x /\ o x.
  Definition t' (X: E') a: E' := fun y => exists x, X x /\ t x a y.

  (** singleton sets *)
  Notation singleton x := (@eq E x).

  (** union of sets of states *)
  Definition plus (X Y: E'): E' := fun z => X z \/ Y z.
  Infix "+" := plus.

  Instance plus_compat: Proper (seq ==> seq ==> seq) plus.
  Proof. firstorder. Qed.
  
  (** [o'] and [t'] respect set equality *)
  Instance o'_compat: Proper (seq ==> iff) o'.
  Proof. intros X Y H. unfold o'. setoid_rewrite H. reflexivity. Qed.

  Instance t'_compat: Proper (seq ==> eq ==> seq) t'.
  Proof. intros X Y H a ? <-. unfold t'. setoid_rewrite H. reflexivity. Qed.

  Instance read_compat: Proper (seq ==> eq ==> seq) (read t').
  Proof. 
    intros X Y H w ? <-. revert X Y H. induction w; intros X Y H. assumption. 
    apply IHw, t'_compat. assumption. reflexivity. 
  Qed.

  (** [o'] and [t'] are semi-lattice homomorphisms *)
  Lemma o'_plus: forall X Y, o' (X+Y) <-> (o' X \/ o' Y).
  Proof. firstorder. Qed.
  Instance monotone_o': Proper (sincl ==> Basics.impl) o'.
  Proof. firstorder. Qed.

  Lemma t'_plus: forall X Y a, t' (X+Y) a == t' X a + t' Y a.
  Proof. firstorder. Qed.
  Instance monotone_t': Proper (sincl ==> eq ==> sincl) t'.
  Proof. firstorder congruence. Qed.

  (** thus so is the [accept] function *)
  Lemma accept_plus: forall X Y, accept o' t' (X+Y) == 
    (fun w => accept o' t' X w \/ accept o' t' Y w). 
  Proof.
    unfold accept. intros X Y w. revert X Y. induction w. apply o'_plus. 
    intros. simpl. rewrite t'_plus. apply IHw.
  Qed.

  (** we deduce that for NFA, language inclusion can easily be reduced to language equivalence *)
  Theorem inclusion_from_equivalence: forall X Y, incl o' t' X Y <-> equiv o' t' (X+Y) Y.
  Proof. unfold equiv. setoid_rewrite accept_plus. firstorder. Qed.


  (** ** Up to congruence (HKC) *)

  (** basic function underlying the congruence closure function *)
  Definition u (R: relation E'): relation E' := 
    fun X Y => exists X1, exists X2, exists Y1, exists Y2, 
      X == X1+X2 /\ Y == Y1+Y2 /\ R X1 Y1 /\ R X2 Y2.

  (** [u] is compatible *)
  Lemma compatible_u: compatible o' t' u.
  Proof.
    intros R S H X Y (X1&X2&Y1&Y2&HX&HY&H1&H2). 
    apply H in H1. apply H in H2. split. 
     rewrite HX, HY, 2o'_plus. tauto.
     intro a. do 4 eexists. rewrite HX, HY, 2t'_plus.
     split. reflexivity. split. reflexivity. firstorder.
  Qed.

  (** definition of the congruence closure from smaller and compatible functions *)
  Definition c1 R := seq \cup swap R \cup square R \cup R \cup u R.
  Definition c := iter c1.

  (** congruence closure is compatible *)
  Lemma compatible_c1: compatible o' t' c1.
  Proof.
    repeat apply compatible_cup.
     intros _ _ _ X Y H. setoid_rewrite H. firstorder.
     apply compatible_swap.
     apply compatible_square.
     apply compatible_id.
     apply compatible_u.
  Qed.

  Lemma compatible_c: compatible o' t' c.
  Proof. apply compatible_iter, compatible_c1. Qed.

  (** as a corollary, we get an abstract proof of correctness our optimised algorithm for NFA *)
  Corollary HKC: forall R, bisimulation_up_to o' t' c R -> R <== equiv o' t'.
  Proof. apply compatible_correct, compatible_c. Qed.

  (** properties about the congruence closure [c] *)
  Lemma continuous_u: continuous u.
  Proof.
    intros R HR X Y (X1&X2&Y1&Y2&HX&HY&[n H1]&[m H2]). 
    exists (max n m). do 4 eexists. 
    split. eassumption.
    split. eassumption.
    split; eapply HR; try eassumption; auto with arith.
  Qed.

  Lemma continuous_c1: continuous c1.
  Proof.
    repeat apply continuous_cup. 
    apply continuous_constant.
    apply continuous_swap.
    apply continuous_square.
    apply continuous_id.
    apply continuous_u.
  Qed.

  Lemma monotone_c1: monotone c1.
  Proof.
    repeat apply monotone_cup. 
     firstorder. 
     firstorder. 
     intros R S H X Y (Z&HX&HY). exists Z. firstorder. 
     firstorder. 
     intros R S H X Y (?&?&?&?&?&?&H1&H2). do 4 eexists. intuition eauto; firstorder.
  Qed.

  Instance monotone_c: monotone c. 
  Proof. apply monotone_iter, monotone_c1. Qed.

  Lemma c1c: forall R, c1 (c R) <== c R.
  Proof. apply continuous_iter. exact continuous_c1. clear. firstorder. Qed.

  Lemma cc: forall R, c (c R) <== c R.
  Proof. intro. apply fixed_iter. exact monotone_c1. apply c1c. Qed.

  Instance c_seq R: subrelation seq (c R).
  Proof. intros X Y H. apply c1c. repeat left. assumption. Qed.

  Instance c_equivalence R: Equivalence (c R). 
  Proof.
    split. intro. apply c_seq. reflexivity. 
    intros X Y H. apply c1c. left; left; left; right. assumption.
    intros X Y Z HXY HYZ. apply c1c. left; left; right. eexists. split; eassumption. 
  Qed.

  Instance c_ext R: subrelation R (c R).
  Proof. intros X Y H. now exists 0. Qed.

  Instance c_plus R: Proper (c R ==> c R ==> c R) plus.
  Proof. 
    intros X X' HX Y Y' HY. apply c1c. right. 
    do 4 eexists. intuition (reflexivity || eassumption).
  Qed.


  (** ** Up to congruence and language equivalence (HKC')

     Unlike in the paper, where we present the special case where we
     use similarity, we present here the general case, where we can
     use an arbitrary relation contained in language equivalence *)

  Section HKC'.

   Variable Req: rel E' E'.
   Hypothesis HReq: Req <== equiv o' t'.
 
   (** definition of the congruence closure up to the given relation *)
   Definition c1' R := Req \cup c1 R.
   Definition c' := iter c1'. (* note: it would be easier to define [c' R] as [c(R\cup Req)] *)
 
   (* the above [c'] function is not compatible by itself unless we
      impose stronger constraints on [Req], it is however always
      contained in the following compatible function *)
   Definition c1'' R := equiv o' t' \cup c1 R.
   Definition c'' := iter c1''.
 
   Lemma compatible_c'': compatible o' t' c''.
   Proof.
     apply compatible_iter.
     apply compatible_cup.
     apply compatible_equiv.
     apply compatible_c1.
   Qed.

   Lemma monotone_c1': monotone c1'.
   Proof. apply monotone_cup. firstorder. apply monotone_c1. Qed.
 
   Lemma c'_c'': forall R, c' R <== c'' R.
   Proof.
     assert (M: forall R S, R<==S -> c1' R <== c1'' S). 
      intros R S H X Y. transitivity (c1' S X Y). apply monotone_c1', H. 
      intros [H'|H']. left. now apply HReq. now right. 
     assert (G: forall n R, xiter c1' R n <== xiter c1'' R n). 
      induction n; intros R. firstorder. simpl. apply M, IHn.
     intros R X Y [n H]. exists n. revert H. apply G. 
   Qed.
 
   (** as a corollary, we get the abstract proof of correctness of HKC' *)
   Corollary HKC': forall R, bisimulation_up_to o' t' c' R -> R <== equiv o' t'.
   Proof. 
     intros R HR.
     eapply compatible_correct. apply compatible_c''. 
     intros X Y H. apply HR in H. intuition. now apply c'_c''. 
   Qed.
 
   (** characterisation of [c'] using [c] (Lemma 11 in the paper) *)
   Lemma continuous_c1': continuous c1'.
   Proof.
     apply continuous_cup. 
     apply continuous_constant.
     apply continuous_c1.
   Qed.
 
   Lemma c1'c': forall R, c1' (c' R) <== c' R.
   Proof. apply continuous_iter. apply continuous_c1'. clear. firstorder. Qed.
 
   Theorem c'c: forall R, c' R <== c (R \cup Req).
   Proof.
     intros R X Y [n H]. revert X Y H. induction n; intros X Y H. 
       apply c_ext. now left.
       destruct H as [H|[[[[H|H]|[z [Hxz Hzy]]]|H]|(x1&x2&y1&y2&Hx&Hy&H1&H2)]].
        apply c_ext. now right.
        now apply c_seq. 
        symmetry. now apply IHn.
        etransitivity; apply IHn; eassumption.
        apply IHn, H.
        rewrite Hx, Hy. apply c_plus; auto. 
   Qed.

   Theorem cc': forall R, c (R \cup Req) <== c' R.
   Proof.
     intros R X Y [n H]. revert X Y H. induction n; intros X Y H.
      destruct H. now exists 0. apply c1'c'. now left.
      destruct H as [[[[H|H]|[z [Hxz Hzy]]]|H]|(x1&x2&y1&y2&Hx&Hy&H1&H2)].
      apply c1'c'. right. repeat left. assumption. 
      apply c1'c'. right. left; left; left; right. now apply IHn. 
      apply c1'c'. right. left; left; right. eexists. split; apply IHn; eassumption. 
      now apply IHn. 
      apply c1'c'. right. right. do 4 eexists. intuition eauto. 
   Qed.

   Corollary c'_iff_c: forall R, c' R === c (R \cup Req).
   Proof. intro R. pose proof (@c'c R). pose proof (@cc' R). firstorder. Qed.

  End HKC'.

  (** ** Computing the congruence closure by rewriting *)
  Section rewriting.

    (** we fix a relation R with which to rewrite *)
    Variable R: relation E'. 

    (** single step rewriting *)
    CoInductive step: relation E' :=
    | do_step: forall X Y Z, R X Y \/ R Y X -> X <= Z -> step Z (X+Y+Z).

    (** reflexive transitive closure (multi-step rewriting) *)
    Inductive steps: relation E' :=
    | steps_seq: subrelation seq steps
    | steps_single: subrelation step steps
    | steps_trans: Transitive steps.

    (** the rewriting relation is a preorder *)
    Existing Instance steps_seq.
    Existing Instance steps_single.
    Global Instance steps_preorder: PreOrder steps.
    Proof. constructor. intro. now apply steps_seq. exact steps_trans. Qed.
    Instance steps_compat: Proper (seq ==> seq ==> iff) steps.
    Proof. 
      assert (G: Proper (seq ==> seq ==> Basics.impl) steps).
       intros X X' HX Y Y' HY H. induction H as [ | | ? ? ? H ? H']. 
        now rewrite <- HX, H, HY. 
        now rewrite <- HX, H, HY. 
        now rewrite <- HX, H, H', HY. 
      split; apply G; trivial; symmetry; assumption. 
    Qed.

    (** one can rewrite under arbitrary contexts *)
    Instance steps_plus: Proper (steps ==> steps ==> steps) plus.
    Proof. 
      assert (G: forall T, Proper (steps ==> steps) (plus T)).
       intros T X X' H. induction H. 
        now rewrite H. 
        destruct H as [X Y Z H].
         etransitivity. apply steps_single, do_step. eassumption. firstorder. 
         apply steps_seq; firstorder. 
        etransitivity; eassumption.
      intros X X' HX Y Y' HY. 
      etransitivity. apply G, HY.
      transitivity (Y'+X). apply steps_seq; firstorder. 
      etransitivity. apply G, HX. apply steps_seq; firstorder. 
    Qed.

    (** helper lemmas to rewrite easily *)
    Lemma step_lr: forall X Y, R X Y -> steps X (X+Y).
    Proof. 
      intros X Y H. etransitivity. 
      apply steps_single, do_step. left; eassumption. reflexivity. 
      apply steps_seq; firstorder. 
    Qed.

    Lemma step_rl: forall X Y, R X Y -> steps Y (Y+X).
    Proof. 
      intros X Y H. etransitivity. 
      apply steps_single, do_step. right; eassumption. reflexivity. 
      apply steps_seq; firstorder. 
    Qed.


    (** rewriting steps are contained in the congruence closure of [R]  *)
    Lemma step_correct: forall X Y, step X Y -> c R X Y.
    Proof. 
      destruct 1 as [X Y Z [H|H] HZ]. 
       rewrite <- (c_ext _ _ _ H); apply c_seq; firstorder. 
       rewrite -> (c_ext _ _ _ H); apply c_seq; firstorder. 
    Qed.

    Lemma steps_correct: forall X Y, steps X Y -> c R X Y.
    Proof. 
      induction 1.
       now apply c_seq.
       now apply step_correct. 
       etransitivity; eassumption. 
    Qed.

    (** the rewriting relation is confluent *)
    Lemma step_confluent: forall X X1 X2, step X X1 -> step X X2 -> 
      exists X1', step X1 X1' /\ exists X2', step X2 X2' /\ X1'==X2'.
    Proof.
      intros _ _ X2_ [X1 Y1 X H1 HX1] H2_. inversion_clear H2_ as [X2 Y2 X' H2 HX2].
      eexists. split. apply do_step. apply H2. clear -HX2. firstorder. 
      eexists. split. apply do_step. apply H1. clear -HX1. firstorder. 
      clear. firstorder. 
    Qed. 

    Lemma steps_confluent': forall X X1, steps X X1 -> forall X2, step X X2 -> 
      exists X1', step X1 X1' /\ exists X2', steps X2 X2' /\ X1'==X2'.
    Proof.
      induction 1 as [X X1|X X1 H1|X X' X1 H IH H1 IH1]; intros X2 H2. 
       destruct H2. 
        eexists. split. constructor. eassumption. firstorder. 
        eexists. split. reflexivity. firstorder. 
       destruct (step_confluent H1 H2) as (X1'&?&X2'&?&?).
        eexists. split. eassumption. 
        eexists. split. apply steps_single. eassumption. assumption.
       apply IH in H2 as (X2'&H2'&X2'b&H2'b&H2). apply IH1 in H2' as (X2''&H2''&X2''b&H2''b&H2').
        eexists. split. eassumption. 
        eexists. split. 2: reflexivity. now rewrite H2'b, <-H2, H2''b, <-H2'. 
    Qed. 

    Lemma steps_confluent: forall X X1 X2, steps X X1 -> steps X X2 -> 
      exists X', steps X1 X' /\ steps X2 X'.
    Proof.
      intros X X1 X2 H1. revert X2. 
      induction H1 as [X X1|X X1 H1|X X' X1 H IH H1 IH1]; intros X2 H2. 
       exists X2. split. now rewrite <-H. reflexivity. 
       destruct (steps_confluent' H2 H1) as (X2'&H2'&X1'&?&?).
        eexists. split. eassumption. rewrite H2'. now apply steps_seq. 
       apply IH in H2 as (X2'&H2'&H2). apply IH1 in H2' as (X2''&H2''&H2').
        eexists. split. eassumption. now rewrite H2.
    Qed.
    
    (** first characterisation theorem *)
    Theorem c_steps: forall X Y, c R X Y <-> exists Z, steps X Z /\ steps Y Z.
    Proof.
      split. 
      (* left to right *)
      * intros [n H]. revert X Y H. induction n; intros X Y H. 
        exists (X+Y). split. apply (step_lr H). 
        rewrite (step_rl H) at 1. apply steps_seq. firstorder. 
        destruct H as [[[[H|H]|[z [Hxz Hzy]]]|H]|(x1&x2&y1&y2&Hx&Hy&H1&H2)].
        - exists X. split. reflexivity. now apply steps_seq.
        - firstorder. 
        - destruct (IHn _ _ Hxz) as (xy&Hx&Hy1).
          destruct (IHn _ _ Hzy) as (yz&Hy2&Hz).
          destruct (steps_confluent Hy1 Hy2) as (xz&?&?).
          exists xz. split. now rewrite Hx. now rewrite Hz. 
        - apply IHn, H. 
        - destruct (IHn _ _ H1) as (xy&HX&HY).
          destruct (IHn _ _ H2) as (xy'&HX'&HY').
          exists (xy+xy'). rewrite Hx, Hy. now split; apply steps_plus. 
      (* right to left *)
      * intros (Z&HX&HY). now rewrite (steps_correct HX), (steps_correct HY). 
    Qed.

    (** sets only grow by rewriting *)
    Lemma steps_incr: forall X Y, steps X Y -> X <= Y.
    Proof. 
      induction 1 as [X Y H|? ? H|X Y Z _ IH1 _ IH2].
       intros ? ?. now apply H. 
       destruct H. clear. firstorder. 
       firstorder.
    Qed.

    (** alternative characterisation *)
    Theorem c_steps': forall X Y, c R X Y <-> 
      (exists Z, steps X Z /\ Y <= Z) /\ (exists Z, steps Y Z /\ X <= Z).
    Proof.
      intros. rewrite c_steps. split. 
      intros (Z&HX&HY). split; exists Z; auto using steps_incr. 
      intros [(X'&HX'&HY)(Y'&HY'&HX)]. exists (X'+Y'). split. 
       rewrite HX', <- HY'. apply steps_seq. firstorder. 
       rewrite HY', <- HX'. apply steps_seq. firstorder. 
    Qed.

    (** characterisation for inclusion checking *)
    Theorem c_steps_incl: forall X Y, c R (X+Y) Y <-> exists Z, steps Y Z /\ X <= Z.
    Proof.
      intros. rewrite c_steps'. split. firstorder. 
      intros (Z&HY&H). pose proof (steps_incr HY). split. 2: firstorder. 
       exists (X+Z). split. rewrite HY. reflexivity. firstorder. 
    Qed.

    (** helper lemma to rewrite inclusions  *)
    Lemma chain_incl_step X Y Z Z':
      R (X+Y) Y -> Y <= Z' -> steps Z Z' -> steps Z (X+Z').
    Proof.
      intros HR H HZ. rewrite HZ. transitivity (Y+Z'). apply steps_seq; firstorder. 
      rewrite (step_rl HR). apply steps_seq; firstorder. 
    Qed.

  End rewriting.


  (** * Coinductive presentation of the antichain algorithms *)

  (** ** Simulations *)

  Definition r2sr (R: rel E' E'): rel E E' := fun x Y => exists X, X x /\ R X Y.
  Definition sr2r (T: rel E E'): rel E' E' := fun X Y => forall x, X x -> T x Y.

  Lemma sr2r2sr T: r2sr (sr2r T) === T.
  Proof. intros x Y. compute. firstorder. exists (singleton x). intuition congruence. Qed.

  Lemma r2sr2r R: R <== sr2r (r2sr R).
  Proof. firstorder. Qed.

  (** small generalisation of the notion of (antichain) simulation,
     required later for up-to techniques  *)
  Definition s_progress (T T': rel E E') := 
    forall x Y, T x Y -> (o x -> o' Y) /\ forall a x', t x a x' -> T' x' (t' Y a).

  (** a simulation is a self-progressing relation  *)
  Definition s_simulation R := s_progress R R.

  (** language inclusion is a simulation  *)
  Lemma s_simulation_incl: s_simulation (r2sr (incl o' t')).
  Proof.
    intros x Y [X [Hx H]]. split. 
     intro Ho. apply (H nil). exists x. split; assumption. 
     intros a x' Hx'. exists (t' X a). split. exists x. tauto.
     intro w. apply (H (a::w)).
  Qed.

  (** simulations are contained in language inclusion  *)
  Lemma s_simulation_in_incl: forall T, s_simulation T -> sr2r T <== incl o' t'.
  Proof.
    intros T HT x Y H w. revert x Y H.
    induction w; intros X Y H. 
      intros (x&?&?). eapply HT; eauto.
      apply IHw. intros x' (x&Hx&Hx'). eapply HT; eauto.
  Qed.

  (** the simulation proof method is sound and complete for language inclusion *)
  Theorem incl_iff_sim: forall X Y, incl o' t' X Y <-> exists T, s_simulation T /\ sr2r T X Y.
  Proof.
    split.
     intro H. exists (r2sr (incl o' t')). split. 
      exact s_simulation_incl. 
      apply r2sr2r. assumption.
    intros [T [HT H]]. revert H. now apply s_simulation_in_incl. 
  Qed.

  (** ** Theory of simulations up to *)

  Definition s_simulation_up_to f T := s_progress T (f T).

  (** a s-compatible function is a function that preserves s-progressions *)
  Definition s_compatible f := forall T T', s_progress T T' -> s_progress (f T) (f T').

  (** technical lemmas about s-progressions and unions  *)
  Instance s_progress_incr: Proper (rincl --> rincl ++> Basics.impl) s_progress.
  Proof. intros R R' HR S S' HS H x y Hxy. apply HR, H in Hxy. firstorder. Qed.

  Lemma cup_s_progress R S T: s_progress R T -> s_progress S T -> s_progress (R \cup S) T.
  Proof. firstorder. Qed.

  Lemma s_progress_cup R S T: s_progress R S \/ s_progress R T -> s_progress R (S \cup T).
  Proof. firstorder. Qed.

  Lemma union_s_progress R S: (forall n, s_progress (R n) S) -> s_progress (union R) S.
  Proof. firstorder. Qed.

  Lemma s_progress_union n R S: s_progress R (S n) -> s_progress R (union S).
  Proof. firstorder. Qed. 

  (** s-compatible function yield correct up-to techniques  *)
  Theorem s_compatible_correct: 
    forall f, s_compatible f ->
    forall T, s_simulation_up_to f T -> sr2r T <== incl o' t'.
  Proof.
    intros f Hf T HT X Y HXY.
    apply incl_iff_sim. exists (iter f T). split.
     clear X Y HXY. 
     apply union_s_progress. intro n. 
     apply s_progress_union with (S n). 
     induction n.
      assumption.
      apply Hf, IHn.
    exists O. apply HXY. assumption.
  Qed.

  (** compositionality properties of compatible functions  *)
  Lemma s_compatible_id: s_compatible id.
  Proof (fun R S H => H).

  Lemma s_compatible_comp f g: s_compatible f -> s_compatible g -> s_compatible (fun R => f (g R)).
  Proof. intros Hf Hg R S H. apply Hf, Hg, H. Qed.

  Lemma s_compatible_cup f g: s_compatible f -> s_compatible g -> s_compatible (fun R => f R \cup g R).
  Proof. intros Hf Hg R S H. apply cup_s_progress; apply s_progress_cup; auto. Qed.


  (** ** Simulations up to upward closure (AC) *)

  (** upward closure function *)
  Definition upc (T: rel E E'): rel E E' := fun x Y => exists Y', T x Y' /\ Y' <= Y.

  (** the upward closure function is compatible  *)
  Lemma s_compatible_upc: s_compatible upc.
  Proof.
    intros T T' H x Y (Y'&Hx&HY). apply H in Hx as [Ho Ht]. split. 
     now rewrite <-HY. 
    intros a x' Hx'. apply Ht in Hx'. eexists. split. eassumption. now apply monotone_t'.
  Qed.

  (** as a corollary, we get an abstract proof of correctness for the antichain algorithm *)
  Corollary AC: forall T, s_simulation_up_to upc T -> sr2r T <== incl o' t'.
  Proof. apply s_compatible_correct, s_compatible_upc. Qed.


  (** ** Simulations up to upward closure and similarity (AC') *)

  Section AC'.

   (** we assume a relation on states which is a (branching-time) simulation *)
   Variable sim: rel E E.
   Definition branching_simulation := forall x y, sim x y -> 
     (o x -> o y) /\ (forall a x', t x a x' -> exists y', t y a y' /\ sim x' y').
   Hypothesis bsim_sim: branching_simulation.

   (** extension of the relation to larger types ([sim''] is the
   "forall-exists" relation from the TACAS'10 paper) *)
   Definition sim': rel E E' := fun x Y => exists y, Y y /\ sim x y.
   Definition sim'': rel E' E' := sr2r sim'.

   (** branching-time simulations are always correct w.r.t. language inclusion  *)
   Lemma s_simulation_sim': s_simulation sim'.
   Proof.
      intros x Y (y&Hy&Hxy). apply bsim_sim in Hxy. split. 
       intro Hx. exists y. tauto.
       intros a x' Hx. apply Hxy in Hx. firstorder. 
   Qed.

   Lemma sim''_in_incl: sim'' <== incl o' t'.
   Proof. apply s_simulation_in_incl, s_simulation_sim'. Qed.

   (** extended upward closure function, exploiting similarity *)
   Definition upc' (T: rel E E'): rel E E' := 
     sim' \cup 
     (fun x Y => exists x', sim x x' /\ exists Y', sim'' Y' Y /\ T x' Y').

   (** this function is compatible *)
   Lemma s_compatible_upc': s_compatible upc'.
   Proof.
     apply s_compatible_cup. intros _ _ _. exact s_simulation_sim'.
     intros T T' HT x Y (x'&Hx&Y'&HY&H). 
     apply bsim_sim in Hx as [Ho Ht]. 
     apply HT in H as [Ho' Ht']. 
     split.
      intro H. apply Ho, Ho' in H. revert H. apply sim''_in_incl in HY. apply (HY nil).
      intros a x1 Hx1. apply Ht in Hx1 as (x1'&Hx1'&?). exists x1'. split. assumption. 
      apply Ht' in Hx1'. eexists. split. 2: eassumption. 
      intros y' (y&Hy&H'). eapply s_simulation_sim'; eauto. 
   Qed.

   (** as a corollary, we get an abstract proof of correctness for 
      the optimised antichain algorithm *)
   Corollary AC': forall T, s_simulation_up_to upc' T -> sr2r T <== incl o' t'.
   Proof. apply s_compatible_correct, s_compatible_upc'. Qed.

  End AC'.

  (** * Relationship between AC and HKC *)

  (** ** HKC can mimick AC *)

  Definition hat (T: rel E E'): rel E' E' := 
    fun X Y => exists x, T x Y /\ X == singleton x + Y. 

  Lemma hat_step (T: rel E E'): forall x Y, T x Y -> c (hat T) (singleton x+Y) Y.
  Proof. intros. apply c_ext. eexists. split. eassumption. reflexivity. Qed.

  Lemma hat_upc: forall T, hat (upc T) <== c (hat T).
  Proof.
    intros T X Y (x&(Y'&H&HY)&HX). rewrite HX. clear HX.
    transitivity ((singleton x+Y')+Y). apply c_seq; firstorder. 
    rewrite hat_step by assumption. apply c_seq; firstorder.
  Qed.

  (** we need finiteness of E for the following lemma (one could also
     extend the congruence closure with arbitrary unions) *)

  Hypothesis Efinite: finite E.

  Lemma hat_s_progress: forall T T', s_progress T T' -> progress o' t' (hat T) (c(hat T')).
  Proof.
    intros T T' H X Y (x&HXY&HX). apply H in HXY as [Ho Ht]. clear H. split. 
    rewrite HX. firstorder. apply Ho. congruence.
    intro a. specialize (Ht a). 
    transitivity (t x a + t' Y a). apply c_seq.
    rewrite HX, t'_plus. apply plus_compat. firstorder congruence. reflexivity.
    clear X HX Ho. revert Ht. generalize (t x a). generalize (t' Y a). clear a x Y. intros Y X. 
    destruct (Efinite X) as [l Hl]. revert X Hl. 
    induction l as [|x l IH]; intros X Hl Ht. apply c_seq. clear - Hl; firstorder. 
    transitivity ((singleton x + Y) + ((fun z => In z l) + Y)). 
     apply c_seq. clear - Hl; firstorder. 
    rewrite hat_step. 2: apply Ht; firstorder. 
    rewrite IH by firstorder. apply c_seq; firstorder. 
  Qed.

  Theorem HKC_mimicks_AC: forall T, 
    s_simulation_up_to upc T -> bisimulation_up_to o' t' c (hat T).
  Proof.
    intros T HT. apply hat_s_progress in HT.
    (* rewrite <- hat_upc in HT. *)
    eapply progress_incr. 3: eassumption. firstorder. 
    rewrite hat_upc. apply cc. 
  Qed.

  (** ** AC can mimick HKC on inclusion of disjoint automata *)
  Section AC_HKC.

   (** we assume two disjoint sets of states, corresponding to two sub-automata *)
   Variables S1 S2: E'.
   Hypothesis Hdisjoint: forall x, S1 x -> S2 x -> False.
   Hypothesis HtS1: forall x a, S1 x -> t x a <= S1.
   Hypothesis HtS2: forall x a, S2 x -> t x a <= S2.

   Lemma HtS2': forall X a, X <= S2 -> t' X a <= S2.
   Proof. intros X a H x' [x [Hx Hx']]. apply HtS2 in Hx'; firstorder. Qed.

   (** [\overline R] relation from the paper *)
   Definition over (R: rel E' E'): rel E E' := 
     fun x Y => Y<=S2 /\ exists X, X x /\ S1 x /\ exists Z, Z==X+Y /\ R Z Y.
     (* (we need to make [over R] compatible w.r.t. [==] in it's first argument) *)

   (** auxiliary result about progressions  *)
   Lemma over_progress: forall R R', progress o' t' R R' -> s_progress (over R) (over R').
   Proof.
     intros S S' H x Y (HY&X&Hx&Hx1&Z&HZ&HXY). apply H in HXY as [Ho Ht]. clear H. split. 
     rewrite <- Ho, HZ, o'_plus. clear -Hx; firstorder. 
     intros a x' Hx'. specialize (Ht a). clear Ho. split. apply HtS2', HY.
     exists (t' X a). split. firstorder. split. firstorder. 
     exists (t' Z a). split. rewrite HZ. apply t'_plus. assumption. 
   Qed.

   (** we now assume a relation whose all pairs have the shape [(X+Y,Y)], with [X<=S1] and [Y<=S2] *)
   Variable R: rel E' E'.
   Hypothesis Hdisjoint_inclusion: forall Z Y, R Z Y -> Y <= S2 /\ exists X, X <= S1 /\ Z = X + Y. 

   (** auxiliary lemmas  *)
   Lemma steps_cap_S2: forall Y Y', steps R Y Y' -> forall y, Y' y -> S2 y -> Y y.
   Proof.
     induction 1 as [Y Y' H|Y Y' H|Y Y' Y'' H IH H' IH']; intros y Hy. 
     - firstorder. 
     - destruct H as [U V Y [HUV|HUV] HY]; 
        destruct (Hdisjoint_inclusion HUV) as (HV2&X'&HX'1&->); firstorder. 
     - auto. 
   Qed.

   Lemma over_c_aux x: S1 x -> forall Y Y', steps R Y Y' -> Y' x ->  
     Y x \/ upc (over R) x (fun y => Y y /\ S2 y).
   Proof.
     intro Hx1. induction 1 as [Y Y' H|Y Y' H|Y Y' Y'' H IH H' IH']; intros HY'x. 
     - left. firstorder. 
     - destruct H as [U V Y [HUV|HUV] HY]. 
       * left. apply Hdisjoint_inclusion in HUV as (HV2&X'&HX'1&->). firstorder. 
       * destruct (Hdisjoint_inclusion HUV) as (HV2&X'&HX'1&->). 
         destruct HY'x as [[?|[?|HX']]|?]; try (left; trivial; apply HY; assumption).
         right. exists U. split. 2: firstorder. split. assumption. 
         exists X'. split. assumption. split. assumption. 
         eexists. split. 2: eassumption. reflexivity. 
     - apply IH' in HY'x as [?|Hx(* (Z&HZ&HZ') *)]. now apply IH.
       right. generalize (steps_cap_S2 H). clear -Hx. firstorder. 
   Qed.
       
   (** lemma 9 in the paper  *)
   Lemma over_c: over (c R) <== upc (over R).
   Proof.
     intros x Y (HY2&X&Hx&Hx1&Z&HZ&H). rewrite HZ in H. clear Z HZ. 
     apply c_steps_incl in H as (Y'&H&HXY). apply HXY in Hx. clear HXY X.
     destruct (over_c_aux Hx1 H Hx) as [Hx'|Hx']. elim (Hdisjoint Hx1). firstorder. 
     assert (I: (fun y => Y y /\ S2 y) <= Y) by firstorder. 
     unfold upc. setoid_rewrite <-I. assumption. 
   Qed.

   (** AC can mimick HKC on inclusion of disjoint automata *)
   Theorem AC'_mimicks_HKC'_on_disjoint_inclusions: 
     bisimulation_up_to o' t' c R -> s_simulation_up_to upc (over R).
   Proof. intros HR. apply over_progress in HR. now rewrite over_c in HR. Qed.
   
  End AC_HKC.

  (*[not yet] ** HKC is better than AC on language equivalence *)
  (*[not yet] ** HKC is better than AC on language inclusion of NFA sharing states *)

  (** * Relationship between AC' and HKC' *)
  
  (** ** HKC' can mimick AC' *)

  Section HKC'_AC'.

   Variable sim: rel E E.

   Lemma hat_upc': forall T, hat (upc' sim T) <== c' (hat (sim' sim)) (hat T).
   Proof.
     intros T X Y (x&H&HX). apply cc'. destruct H as [H|(y&H&Y'&Hy&HY')].
      apply c_ext. right. econstructor; eauto.
      rewrite HX. clear X HX. apply c_steps_incl. 
      exists (singleton x + (singleton y + (Y' + Y))). split. 2: firstorder. 
      apply chain_incl_step with (singleton y). right. exists x. firstorder. firstorder. 
      apply chain_incl_step with Y'. left. eexists; split. eassumption. reflexivity. firstorder. 
      destruct (Efinite Y') as [l Hl]. clear -Hy Hl. revert Y' Hy Hl. 
      induction l as [|x l IH]; intros X H Hl. apply steps_seq. firstorder. 
      transitivity (singleton x+((fun z => In z l)+Y)). 
       apply chain_incl_step with Y. right. 2:firstorder. destruct (H x); firstorder. 
       apply IH. firstorder. tauto. 
      apply steps_seq. clear - Hl; firstorder.
   Qed.

   Theorem HKC'_mimicks_AC': forall T, 
     s_simulation_up_to (upc' sim) T -> bisimulation_up_to o' t' (c' (hat (sim' sim))) (hat T).
   Proof.
     intros T HT. apply hat_s_progress in HT.
     eapply progress_incr. 3: eassumption. firstorder. 
     rewrite hat_upc'. rewrite c'c at 1. rewrite <- cc'. apply cc. 
   Qed.
 
  End HKC'_AC'.

  (*[not yet] ** HKC' is better than AC' on language inclusion of disjoint NFA *)

 End NFA.

End Alphabet.
