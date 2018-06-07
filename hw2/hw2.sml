(*
Name: Beier (Benjamin) Liu
Date: 6/1/2018

Remark:
*)


(*===================================================================================================
File content:
Solutions to homework 2
===================================================================================================*)

(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

(*Qa --function all_except_option, which takes a string and a string list. Return NONE if the string is not in the list, else return SOME lst where lst is identical to the argument list except the string is not in it.*)
fun all_except_option(str: string, str_ls: string list)=
	(*
	============================================================================================
	Arguments:
	str	-- string, targeted string to be compared with the string list
	str_ls 	--string list, a string of list

	Returns:
	option 	--if str in the str_ls return a string list; o/w return NONE
	============================================================================================
	*)
	let
		fun is_in_list(str: string, str_ls: string list)=
			case str_ls of
				[] => false
				| hd_::tl_ => if not (same_string(str, hd_))
							then is_in_list(str, tl_)
							else true
		fun remove_str_from_ls(str: string, str_ls: string list)=
			case str_ls of
				[] => []
				| hd_::tl_ => if not (same_string(str, hd_))
							then hd_::remove_str_from_ls(str, tl_)
							else remove_str_from_ls(str, tl_)
	in
		if not (is_in_list(str, str_ls))
		then NONE
		else SOME (remove_str_from_ls(str, str_ls))
	end

(*Qb --function get_substitutions1, which takes a string list list (a list of list of strings, the substitutions) and a string s and returns a string list.*)
fun get_substitutions1(str_ls_ls: string list list, s: string)=
	(*
	============================================================================================
	Arguments:
	str_ls_ls	-- string list list, a list of lists of strings
	s 	--string, targeted string to be compared with the string list

	Returns:
	a list of string
	============================================================================================
	*)
	case str_ls_ls of
		[] => []
		| hd_::tl_ => case all_except_option(s, hd_) of
					NONE => get_substitutions1(tl_, s)
					| SOME lst => lst @ get_substitutions1(tl_, s)

(*Qc --function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive local helper function.*)
fun get_substitutions2(str_ls_ls: string list list, s: string)=
	(*
	============================================================================================
	Arguments:
	str_ls_ls	-- string list list, a list of lists of strings
	s 	--string, targeted string to be compared with the string list

	Returns:
	a list of string
	============================================================================================
	*)
	let
	 	fun aux(str_ls_ls: string list list, s: string, acc: string list)=
	 		case str_ls_ls of
	 			[] => acc
	 			| hd_::tl_ => case all_except_option(s, hd_) of
	 						NONE => aux(tl_, s, acc)
	 						| SOME lst => aux(tl_, s, acc @ lst)
	 in
	 	aux(str_ls_ls, s, [])
	 end

(*Qd --function similar_names, which takes a string list list of substitutions (as in parts (b) and (c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full names (type {first:string,middle:string,last:string} list).*)
type fullname_type={first:string, middle:string, last:string}
fun similar_names(str_ls_ls: string list list, fullname: fullname_type)=
	(*
	============================================================================================
	Arguments:
	str_ls_ls	-- string list list, a list of lists of strings
	fullname 	-- fullname_type, the first name is the targeted string to be compared with the string list

	Returns:
	a list of {first:string, last:string, middle:string}
	============================================================================================
	*)
	let
		fun get_all_fullname(str_ls: string list, mid: string, lat: string)=
			case str_ls of
				[] => []
				| hd_::tl_ => {first=hd_, last=lat, middle=mid}::get_all_fullname(tl_, mid, lat)
	in
		case fullname of
		{first=x,middle=y,last=z} => get_all_fullname(x::get_substitutions1(str_ls_ls, x), y, z)
	end


(*2a --function card_color, which takes a card and returns its color*)
fun card_color(cd: card)=
	(*
	============================================================================================
	Arguments:
	cd 	--card, the card whose color will be returned

	Returns:
	color, the color of the card
	============================================================================================
	*)
	case cd of
		(Clubs, _) => Black
		| (Spades, _) => Black
		| (Hearts, _) => Red
		| (Diamonds, _) => Red

(*2b --function card_value, which takes a card and returns its value (numbered cards have their number as the value, aces are 11, everything else is 10)*)
fun card_value(cd: card)=
	(*
	============================================================================================
	Arguments:
	cd 	--card, the card whose value will be returned

	Returns:
	int, the value of the card
	============================================================================================
	*)
	case cd of
		(_, Num x) => x
		| (_, Ace) => 11
		| (_, _) => 10


(*2c --function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a list that has all the elements of cs except c. If c is in the list more than once, remove only first one. If c is not in the list, raise the exception e*)
fun remove_card(cs: card list, c: card, e: exn)=
	(*
	============================================================================================
	Arguments:
	cs 	--card list, one list of cards
	c 	--card, one card, whose value will be compared with cs
	e 	--exn, an exception to be raised when no card in cs matches card c

	Returns:
	card list, the card list after removing the card c
	============================================================================================
	*)
	let
		fun is_in_list(c: card, cs: card list)=
			case cs of
				[] => false
				| hd_::tl_ => if not (c=hd_)
							then is_in_list(c, tl_)
							else true
		fun remove_card_from_ls(c: card, cs: card list, cnt: int)=
			case cs of
				[] => []
				| hd_::tl_ => if (c=hd_ andalso cnt>0)
							then remove_card_from_ls(c, tl_, cnt-1)
							else hd_::remove_card_from_ls(c, tl_, cnt)
	in
		if not (is_in_list(c, cs))
		then raise e
		else remove_card_from_ls(c, cs, 1)
	end

(*2d --function all_same_color, which takes a list of cards and returns true if all the cards in the list are the same color*)
fun all_same_color(cs)=
	(*
	============================================================================================
	Arguments:
	cs 	--card list, one list of cards

	Returns:
	boolean 	--whether the card list all share the same color
	============================================================================================
	*)
	let
		fun is_same_color(x, y)=
			case (card_color(x), card_color(y)) of
				(Black, Black) => true
				| (Red, Red) => true
				| (Red, Black) => false
				| (Black, Red) => false
	in
		case cs of
			x::(y::xs') => is_same_color(x,y) andalso all_same_color(y::xs')
			| x::[] => true
			| [] => true
	end

(*2e --function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally defined helper function that is tail recursive.*)
fun sum_cards(cs)=
	(*
	============================================================================================
	Arguments:
	cs 	--card list, one list of cards

	Returns:
	int, sum of the value in the card list
	============================================================================================
	*)
	let
		fun aux(cs, acc)=
			case cs of
				[] => acc
				| x::xs' => aux(xs', acc+card_value(x))
	in
		aux(cs, 0)
	end



(*2f --function score, which takes a card list (the held-cards) and an int (the goal) and computes the score as described above*)
fun score(cs, goal)=
	(*
	============================================================================================
	Arguments:
	cs 	--card list, one list of cards
	goal --some pre-set integer

	Returns:
	int 	--compute the score of the game
	============================================================================================
	*)
	let
		val sum = sum_cards(cs)
		val preliminary_score= if sum>goal then 3*(sum-goal) else goal-sum
		val sore=if all_same_color(cs) then preliminary_score div 2 else preliminary_score
	in
		sore
	end

(*2g --function officiate, which "runs a game." It takes a card list (the card-list) a move list (what the player "does" at each point), and an int (the goal) and returns the score at the end of the game after processing (some or all of) the moves in the move list in order.*)
fun officiate(cs, ms, goal)=
	(*
	============================================================================================
	Arguments:
	cs 	--card list, one list of cards
	ms 	--move list, one list of type move
	goal 	--int, pre-set integer

	Returns:
	int 	--the score of the play
	============================================================================================
	*)
	let
(*		fun is_same_card(x, y)=
			let
				val (x1, x2) = x
				val (y1, y2) = y
			in
				if x1=y1 andalso x2=y2 then true else false
			end*)
		(*depreciated method*)

		fun drop_card(c: card, hs: card list)=
			case hs of
				[] => raise IllegalMove
				|x::xs' => if x=c
						then xs'
						else x::drop_card(c, xs')


		fun make_a_move(cs, hs, ms)=
			if sum_cards(hs)<=goal
			then case (cs,hs,ms) of
				([],_,_) => score(hs, goal)
				| (_,_,[]) => score(hs, goal)
				| (hdcs::tlcs, hs, Draw::tlms) => make_a_move(tlcs, hdcs::hs, tlms)
				| (hdcs::tlcs, hs, Discard c::tlms) => make_a_move(cs, drop_card(c, hs), tlms)
			else score(hs, goal)
(*			if null cs
			then score(hs, goal)
			else if sum_cards(hs)>goal
			then score(hs, goal)
			else if null ms
			then score(hs, goal)
			else
				case hd ms of
					Draw => make_a_move(tl cs, hd cs::hs, tl ms)
					| Discard c=> make_a_move(cs, drop_card(c, hs), tl ms*)
	in
		make_a_move(cs, [], ms)
	end


(*test starts here *)
val test1 = all_except_option ("string", ["string"]) = SOME []

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false)
              handle IllegalMove => true)
