(*
Name: Beier (Benjamin) Liu
Date: 6/5/2018

Remark:
*)


(*===================================================================================================
File content:
Solution to homework 3
===================================================================================================*)

(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
		  | Unit
		  | Tuple of valu list
		  | Constructor of string * valu

fun g f1 f2 p =
	let
	val r = g f1 f2
	in
	case p of
		Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
	end

(**** for the challenge problem only ****)

datatype typ = Anything
		 | UnitT
		 | IntT
		 | TupleT of typ list
		 | Datatype of string

(**** you can put all your code here ****)

(*Q1 --function only_capitals that takes a string list and returns a string list that has only the strings in the argument that start with an uppercase letter.*)
val only_capitals = List.filter (fn s=>Char.isUpper(String.sub(s, 0)))

(*Q2 --function longest_string1 that takes a string list and returns the longest string in the list. If the list is empty, return ""*)
val longest_string1 = foldl (fn (prev, curr)=>if String.size prev>String.size curr then prev else curr) ""

(*Q3 --function longest_string2 that is exactly like longest_string1 except in the case of ties it returns the string closest to the end of the list*)
val longest_string2 = foldl (fn (prev, curr)=>if String.size prev>=String.size curr then prev else curr) ""

(*Q4 --functions longest_string_helper, longest_string3, and longest_string4*)
fun longest_string_helper f str_ls=
		foldl (fn (prev, curr)=> if f(String.size prev, String.size curr) then prev else curr) "" str_ls

val longest_string3=longest_string_helper (fn (x,y) => (x>y))

val longest_string4=longest_string_helper (fn (x,y) => (x>=y))

(*Q5 --function longest_capitalized that takes a string list and returns the longest string in the list that begins with an uppercase letter, or "" if there are no such strings*)
val longest_capitalized=longest_string3 o only_capitals

(*Q6 --function rev_string that takes a string and returns the string that is the same characters in reverse order*)
val rev_string=String.implode o rev o String.explode

(*Q7 --function first_answer of type ('a -> 'b option) -> 'a list -> 'b (notice the 2 arguments are curried)*)
fun first_answer f ls =
	let
		fun aux(x, xs')=
			case f(x) of
				NONE => first_answer f xs'
				| SOME v => v
	in
		case ls of
			[] => raise NoAnswer
			| x::xs' => aux(x, xs')
	end

(*Q8 --function all_answers of type ('a -> 'b list option) -> 'a list -> 'b list option (notice the 2 arguments are curried)*)
fun all_answers f ls =
	let
		fun aux(x, xs', acc)=
			case f(x) of
				NONE => all_answers f xs'
				| SOME lst => SOME (lst @ acc)
(*		fun helper f ls acc =
			case (ls,acc) of
				([],[]) => SOME []
				| ([], _::_) => NONE
				| (x::xs',_) => aux(x, xs', acc)*)
	in
		(*helper f ls []*)
		case ls of
			[] => NONE
			| x::xs' => aux(x, xs', [])
	end

(*Q9 --Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard patterns it contains.
Use g to define a function count_wild_and_variable_lengths that takes a pattern and returns the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables in the variable patterns it contains. (Use String.size. We care only about variable names; the constructor names are not relevant.)
Use g to define a function count_some_var that takes a string and a pattern (as a pair) and returns the number of times the string appears as a variable in the pattern. We care only about variable names; the constructor names are not relevant.*)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(*fun count_wildcards p =
	case p of
		Wildcard => 1
		| ConstructorP (_, pat) => count_wildcards pat
		| TupleP (x::xs') => count_wildcards x + count_wildcards (TupleP xs')
		| _ => 0*)

(*	let
		fun helper p ls =
			case p of
				ConstructorP(_, pat) => if (all_answers (fn s => if s="ConstructorP" then SOME [s] else NONE) ls)=NONE
										then helper pat ls
										else 1+helper pat ("ConstructorP"::ls)
				| TupleP(x::xs') => if (all_answers (fn s => if s="TupleP" then SOME [s] else NONE) ls)=NONE
										then helper (TupleP(xs')) ls
										else 1+helper (TupleP(xs')) ("TupleP"::ls)
				| _ => 1
	in*)
	(*end*)
(*	case p of
		ConstructorP(_, pt) => 1+count_wildcards(pt)
		| TupleP =>
		| _ => 1*)

val count_wild_and_variable_lengths = g (fn _ => 1) String.size
(*fun count_wild_and_variable_lengths p =
	let
		fun count_variable_lengths p =
			case p of
				Variable s=> String.size s
				| ConstructorP(s, _) => String.size s
				| TupleP(x::xs') => count_variable_lengths x + count_variable_lengths (TupleP xs')
				| _ => 0
	in
		 count_wildcards p + count_variable_lengths p
	end*)
fun count_some_var (str, p) = g (fn _ => 0) (fn x => if String.isSubstring str x
													then 1
													else 0) p
(*fun count_some_var(s, p)=
	case p of
		Variable sr => if sr=s then 1 else 0
		| ConstructorP(sr, _) => if sr=s then 1 else 0
		| TupleP(x::xs') => count_some_var(s, x) + count_some_var(s, (TupleP xs'))*)

(*Q10 --function check_pat that takes a pattern and returns true if and only if all the variables appearing in the pattern are distinct from each other*)
fun check_pat p =
	let
	fun filterString pat acc = case pat of
					Variable x => x :: acc
					 | ConstructorP (_, p) => filterString p acc
					 | TupleP ps =>
					   List.foldl
					   (fn (p, acc) => (filterString p []) @ acc) [] ps
					 | _ => []
	in
	let
		val strList = filterString p []
		fun checkDuplicate remList =
			case remList of
			[] => true
			| x :: xs => if List.exists (fn item => item = x) xs
						then false
						else checkDuplicate xs
	in
		checkDuplicate strList
	end
	end

(*Q11 --function match that takes a valu * pattern and returns a (string * valu) list option, namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does*)
fun match (v, p) =
    case p of
	Wildcard => SOME []
      | UnitP => (case v of Unit => SOME []
			  | _ => NONE)
      | Variable str => SOME [(str, v)]
      | ConstP i => (case v of Const j => if i = j then SOME [] else NONE
			     | _ => NONE)
      | TupleP plst => (case v of
			    Tuple vlst => if List.length plst = List.length vlst
					  then all_answers match (ListPair.zip (vlst, plst))
					  else NONE
			  | _ => NONE)
      | ConstructorP (str, pt) => (case v of
				       Constructor (vstr, vval) => if str = vstr
								   then match (vval, pt)
								   else NONE
				     | _ => NONE)


(*Q12 --function first_match that takes a value and a list of patterns and returns a (string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where lst is the list of bindings for the first pattern in the list that matches.*)
fun first_match v plst =
    SOME (first_answer (fn p => match (v, p)) plst)
    handle NoAnswer => NONE

(*testing phrase*)
(*val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []*)
