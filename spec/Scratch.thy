theory Scratch
imports Complex_Main "HOL-Library.tree" splay
begin

fun is_tree :: "value \<Rightarrow> bool" where
"is_tree (ValTree _) = True" |
"is_tree _ = False"

fun tree_only :: "'b \<Rightarrow> ((base Tree.tree) \<Rightarrow> 'b) \<Rightarrow> (value \<Rightarrow> 'b)" where
"tree_only z f = (\<lambda>x. case x of ValTree t \<Rightarrow> f t | _ \<Rightarrow> z)"

fun tree_count :: "(value list) \<Rightarrow> nat" where
"tree_count vs = sum_list (map (tree_only 0 (%x. 1)) vs)"

fun catOptions :: "(('a option) list) \<Rightarrow> ('a list)" where
"catOptions [] = []" |
"catOptions ((Some x)#xs) = (x#(catOptions xs))" |
"catOptions (None#xs) = catOptions xs"

fun tree_filter :: "(value list) \<Rightarrow> ((base Tree.tree) list)" where
"tree_filter vs = catOptions (map (tree_only None Some) vs)"

fun rk :: "'a Tree.tree \<Rightarrow> real" where
"rk \<langle>\<rangle> = 1" |
"rk \<langle>l, _, r\<rangle> = rk l + rk r + log 2 (size1 l) + log 2 (size1 r)"

fun p1 :: "nat \<Rightarrow> nat \<Rightarrow> ('a Tree.tree) \<Rightarrow> real" where
"p1 a b t = log 2 (a * (size1 t) + b)"

fun pn :: "(nat list) \<Rightarrow> nat \<Rightarrow> (('a Tree.tree) list) \<Rightarrow> real" where
"pn as b vs = log 2 ((sum_list (map (%(a, t). a * (size1 t)) (zip as vs))) + b)"

lemma p1_pn: "p1 a b t = pn [a] b [t]"
  by simp

fun extend :: "'b \<Rightarrow> ('a \<rightharpoonup> 'b) \<Rightarrow> ('a \<Rightarrow> 'b)" where
"extend z m = (\<lambda>x. case m x of Some y \<Rightarrow> y | None \<Rightarrow> z)"

fun \<Phi>1 :: "(nat * (((nat * nat) * nat) list)) \<Rightarrow> 'a Tree.tree  \<Rightarrow> real" where
"\<Phi>1 (qp, qs) t = rk t * qp + sum_list (map (\<lambda>((a, b), q). p1 a b t * q) qs)"

type_synonym "annotation" = "((nat list) * ((((nat list) * nat) * nat) list))"

fun \<Phi> :: "annotation \<Rightarrow> 'a Tree.tree list \<Rightarrow> real" where
"\<Phi> (qps, qss) ts =
    (sum_list (map (\<lambda>(qp, t). rk t * qp) (zip qps ts)))
 +  (sum_list (map (\<lambda>((as, b),q). pn as b ts * q) qss))"

lemma phi1_phi: "\<Phi>1 (qp,[((a, b), q)]) t = \<Phi> ([qp], [(([a], b), q)]) [t]"
  by simp

type_synonym "map_annotation" = "(ident \<rightharpoonup> nat) * ((ident \<rightharpoonup> nat) * nat)"

fun \<Phi>c :: "annotation \<Rightarrow> context \<Rightarrow> environment \<Rightarrow> real" where
"\<Phi>c a c e = \<Phi> a (tree_filter (catOptions (map (\<lambda>x. e (fst x)) c)))"

fun \<Phi>v :: "annotation \<Rightarrow> value \<Rightarrow> real" where
"\<Phi>v a (ValTree t) = \<Phi> a [t]" |
"\<Phi>v _ _ = 0"

theorem soundness:
"\<lbrakk> typing s (gamma, q) 1 e (alpha, qp) ; reduction prog env l e v \<rbrakk> \<Longrightarrow>
(\<Phi>c q gamma env) - (\<Phi>v qp v) \<ge> l
"
  sorry

theorem soundness_cf:
"\<lbrakk> typing s (gamma, q) 0 e (alpha, qp) ; reduction prog env l e v \<rbrakk> \<Longrightarrow>
(\<Phi>c q gamma env) \<ge> (\<Phi>v qp v)
"
  sorry

end
