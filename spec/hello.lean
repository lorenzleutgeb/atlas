import data.real.basic

-- Syntax

def Name : Type := string

inductive CmpOp
  | Lt | Le | Eq | Ne | Gt | Ge

instance cmpop_inhabited : inhabited CmpOp :=
inhabited.mk CmpOp.Eq

inductive Expr
  | Var : Name -> Expr
  | Leaf : Expr
  | Node : Expr -> Name -> Expr -> Expr
  | Cmp : Name -> CmpOp -> Name -> Expr
  | Ite : Name -> Expr -> Expr -> Expr
  --| App : Name -> List Name -> Expr
  | Let : Name -> Expr -> Expr -> Expr

-- Static Semantics

inductive Typ
  | Tree | Bool | Base

def Idxr : Type := ((Name → nat) × nat)

--abbrev Anno (a : Type) [Ord a] := (Name → a) × ((Name → Nat) → Nat → a)
--abbrev Anno (a : Type) [Ord a] := ((List Nat) → a) × ((List Nat) → a)

--abbrev ATyp a [Ord a] := Typ × Anno a

-- TODO: How to best model the context? An AssocList might work, but to define the typing rules
-- they appear too low-level/cumbersome.

inductive styprel : (Name → Typ) → Expr → Typ → Prop
  | typ_true : ∀ ctx, styprel ctx (Expr.Var "true") (Typ.Bool)
  | typ_false : ∀ ctx, styprel ctx (Expr.Var "false") (Typ.Bool)
  | typ_var : ∀ ctx x, (ctx x) == t → styprel ctx (Expr.Var x) t
  | typ_leaf : ∀ ctx, styprel ctx Expr.Leaf Typ.Tree
  | typ_node : ∀ ctx t1 t2 d,
    (styprel ctx t1 Typ.Tree) → (styprel ctx t2 Typ.Tree) → styprel ctx (Expr.Ite d t1 t2) Typ.Tree
  | typ_cmp : ∀ ctx t1 t2 op, ctx t1 == Typ.Base → ctx t2 == Typ.Base →
     styprel ctx (Expr.Cmp t1 op t2) Typ.Bool
  | typ_ite : ∀ ctx c t e1 e2, styprel ctx e1 t → styprel ctx e2 t → styprel ctx (Expr.Var c) Typ.Bool →
                 styprel ctx (Expr.Ite c e1 e2) t
  --| typ_let : forall ctx 

def styp (ctx : Ctx) (e : Expr) : Option Typ := match e with
  | Expr.Leaf => Option.some Typ.Tree
  -- | Expr.Val x => Option.some Typ.Base
  | Expr.Var x => if let some t := ctx.find? x then Option.some t else Option.none
  | Expr.Cmp l op r => Option.some Typ.Bool
  | Expr.Node l d r => Option.none
  | Expr.Ite c e1 e2 => Option.none
  | Expr.Let x e1 e2 => Option.none
  /-
  | Expr.Ite c t f => if ctx.find? c == (Option.some Typ.Bool) && tt == tf then tt else Option.none
    where
      tt := styp ctx l
      -/

theorem typgood : forall ctx1 ctx2 e ty, styp ctx1 e == some ty → styprel ctx2 e ty := sorry

-- Dynamic Semantics

inductive Tree (a : Type) [Ord a] where
  | Leaf : Tree a
  | Node : Tree a -> a -> Tree a -> Tree a

def log' (x : ℚ) : ℚ := id (min 1 x)

def rk (t : Tree) : ℚ := match t with
  | Expr.Leaf -> 1
  | Expr.Tree l d r -> (rk l) + (rk d)

inductive Val (a : Type) [Ord a] where
  | Tree : Tree a -> Val a
  | Base : a -> Val a
  | Bool : Bool -> Val a

abbrev Env a [Ord a] := Std.AssocList Name (Val a)

def eval {a : Type} [Ord a] (env : Env a) (e : Expr) : (Option (Val a)) := match e with
  | Expr.Leaf => Option.some (Val.Tree Tree.Leaf)
  | Expr.Var x => env.find? x
  | Expr.Cmp l op r => Option.none
  | Expr.Node l d r => Option.none
  | Expr.Ite c e1 e2 => Option.none
  | Expr.Let x e1 e2 => Option.none