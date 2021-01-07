(*
Avaneesh Kolluri
2/12/2020
I pledge my honor that I have abided by the Stevens Honor System.
*)

(* Problem 1 *)
type dTree = Leaf of int | Node of char*dTree*dTree

(* Problem 2 *)
let tLeft = Node('w', Node('x', Leaf(2), Leaf(5)), Leaf(8))
let tRight = Node('w', Node('x',Leaf(2),Leaf(5)), Node('y',Leaf(7),Leaf(5)))

let graph = (['x';'y';'z'],[([0;0;0] ,  0);([0;0;1] ,  1);([0;1;0] ,  1);([0;1;1] ,  0);([1;0;0] ,  1);([1;0;1] ,  0);([1;1;0] ,  0);([1;1;1] ,  1)])

(* Problem 3 *)
let max x y = 
	if x>y then x else y

let rec dTree_height tree : int =
	match tree with
	|Leaf(x) -> 0
	|Node(i,j,k) -> max (1 + dTree_height j) (1 + dTree_height k)

let rec dTree_size tree : int =
	match tree with
	|Leaf(x) -> 1
	|Node(i,j,k) -> 1 + dTree_size j + dTree_size k

let rec add_at_end l i =
  match l with
  |[] -> [i]
  | h :: t -> h :: (add_at_end t i)

let rec dTree_pathsHelp tree lst : int list list = 
	match tree with
	|Leaf(x) -> [lst]
	|Node(i,j,k) -> (dTree_pathsHelp j (add_at_end lst 0)) @ (dTree_pathsHelp k (add_at_end lst 1))

let dTree_paths tree : int list list = 
	dTree_pathsHelp tree []

let rec dTree_is_perfect tree : bool = 
	match tree with
	|Leaf(x) -> true
	|Node(i,j,k) -> (dTree_height j = dTree_height k) && dTree_is_perfect j && dTree_is_perfect j

let rec dTree_map f g t : dTree = 
	match t with
	|Leaf(x) -> Leaf(g x)
	|Node(i,j,k) -> Node(f i, dTree_map f g j, dTree_map f g k)

(* Problem 4 *)
let rec list_to_tree lst : dTree = 
	match lst with
	|[] -> Leaf(0)
	|h::t -> Node(h,list_to_tree t,list_to_tree t)

(* Problem 5 *)
(* The following is considering that f is a full gaph given to us.
let rec help tree f x y: dTree = 
	match x with
	|[] -> Leaf(y)
	|h::t -> if h=0 then let Node(i,j,k) = tree in Node(i,help j f t y,k)
									else let Node(i,j,k) = tree in Node(i,j,help k f t y)

let rec replace_leaf_at_help tree f : dTree = 
	match f with
	|[] -> tree
	|(x, y)::t -> replace_leaf_at_help (help tree f x y) t

let replace_leaf_at tree f : dTree = 
	replace_leaf_at_help tree (snd f)
*)

let rec help lst tree a : dTree = 
	match lst, tree with
	|[], Leaf x -> Leaf a
	|[h], Node(i,l,r) -> if (h = 0) then Node (i, help [] l a, r) else Node(i, l, help [] r a)
	|h::t, Node(i,l,r) -> if (h = 0) then Node(i, help t l a, r) else Node(i, l, help t r a)

let rec replace_leaf_at_help tree f : dTree = 
	match f with
	|[] -> tree
	|(lst, x)::t -> replace_leaf_at_help (help lst tree x) t

let replace_leaf_at tree f : dTree =
	replace_leaf_at_help tree f


(* Problem 6 *)
let bf_to_dTree graph : dTree = 
	replace_leaf_at (list_to_tree (fst graph)) (snd graph)




