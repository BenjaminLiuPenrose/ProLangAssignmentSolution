(*
Name: Beier (Benjamin) Liu
Date: 6/1/2018

Remark:
*)


(*===================================================================================================
File content:
Solutions to homework 1
===================================================================================================*)
exception UndefinedDateError

type myDate = int*int*int

(*check whether the date is a reasonable one. you may ignore it.*)
fun is_resonable(x: myDate) =
	(*
	================================================================================================
	Arguments:
	x	-- myDate

	Returns:
	boolean
	================================================================================================*)
	if #1 x<1
	then false
	else if #2 x>12 orelse #2 x<1
	then false
	else if #3 x>31 orelse #3 x<1
	then false
	else true

(*Q1 --function evaluates to true if the frst argument is a date that comes before the second argument*)
fun is_older (x: myDate, y: myDate) =
	(*
	===============================================================================================
	Arguments:
	x	-- myDate
	y 	-- myDate

	Returns:
	boolean
	==============================================================================================*)
	if is_resonable(x) andalso is_resonable(y) then
		if #1 x < #1 y
		then true
		else if #1 x = #1 y andalso #2 x < #2 y
		then true
		else if #1 x = #1 y andalso #2 x = #2 y andalso #3 x < #3 y
		then true
		else false
	else raise UndefinedDateError

(*Q2 --function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month.*)
fun number_in_month(dates_ls: myDate list, m: int) =
	(*
	===============================================================================================
	Arguments:
	dates_ls	-- list of myDate
	m  	-- int

	Returns:
	int
	==============================================================================================*)
	let
		fun is_in_month(x: myDate, m: int)=
			if is_resonable(x) then if #2 x=m then 1 else 0
			else raise UndefinedDateError
	in
		if dates_ls=[] then 0
		else is_in_month(hd dates_ls, m)+number_in_month(tl dates_ls, m)
	end

(*Q3 --function number_in_months that takes a list of dates and a list of months (i.e., an int list) and returns the number of dates in the list of dates that are in any of the months in the list of months.*)
fun number_in_months(dates_ls: myDate list, months_ls: int list) =
	(*
	===============================================================================================
	Arguments:
	dates_ls	-- list of myDate
	months_ls  	-- list of int

	Returns:
	list of int
	==============================================================================================*)
	if months_ls=[] then 0
	else number_in_month(dates_ls, hd months_ls)+number_in_months(dates_ls, tl months_ls)

(*Q4 --function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month.*)
fun dates_in_month(dates_ls: myDate list, m: int) =
	(*
	===============================================================================================
	Arguments:
	dates_ls	-- list of myDate
	m  	-- int

	Returns:
	list of int
	==============================================================================================*)
	let
		fun is_in_month(x: myDate, m: int)=
			if is_resonable(x) then if #2 x=m then x else (0,0,0)
			else raise UndefinedDateError
	in
		if dates_ls=[] then []
		else
			let val tmp=is_in_month(hd dates_ls, m)
			in
				if tmp=(0,0,0) then dates_in_month(tl dates_ls, m)
				else tmp::dates_in_month(tl dates_ls, m)
			end
	end

(*Q5 --function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in the list of months*)
fun dates_in_months(dates_ls: myDate list, months_ls: int list) =
	(*
	===============================================================================================
	Arguments:
	dates_ls	-- list of myDate
	months_ls  	-- list of int

	Returns:
	list of int
	==============================================================================================*)
	let
		fun merge_ls(ls1: myDate list, ls2: myDate list)=
			ls1@ls2
	in
		if months_ls=[] then []
		else merge_ls(dates_in_month(dates_ls, hd months_ls), dates_in_months(dates_ls, tl months_ls))
	end

(*Q6 --function get_nth that takes a list of strings and an int n and returns the nth element of the
list*)
fun get_nth(str_ls: string list, n: int)=
	(*
	===============================================================================================
	Arguments:
	str_ls	-- list of string
	n  	-- int

	Returns:
	string
	==============================================================================================*)
	if n=1 then hd str_ls
	else get_nth(tl str_ls, n-1)

(*Q7 --function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example)*)
fun date_to_string(x: myDate)=
	(*
	===============================================================================================
	Arguments:
	x	-- myDate

	Returns:
	string
	==============================================================================================*)
	let
		val months_ls = ["January","February","March","April","May","June","July","August","September", "October", "November", "December"];
		val month_letter=get_nth(months_ls, #2 x)
	in
		month_letter^" "^Int.toString(#3 x)^", "^Int.toString(#1 x)
	end

(*Q8 --function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.*)
fun number_before_reaching_sum(sum: int, int_ls: int list)=
	(*
	===============================================================================================
	Arguments:
	sum	-- int
	int_ls  	-- int list

	Returns:
	int
	==============================================================================================*)
	let
		fun aux(sum: int, int_ls: int list, cnt: int)=
			if sum<=0 then cnt
			else aux(sum-hd int_ls, tl int_ls, cnt+1)
	in
		aux(sum, int_ls, 0)-1
	end

(*Q9 --function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.)*)
fun what_month(day_of_year: int)=
	(*
	===============================================================================================
	Arguments:
	day_of_year  	-- int

	Returns:
	string
	==============================================================================================*)
	let
		val ls=[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
		val month_ls = ["January","February","March","April","May","June","July","August","September", "October", "November", "December"];
	in
		(*get_nth(month_ls, number_before_reaching_sum(day_of_year, ls))*)
		number_before_reaching_sum(day_of_year, ls)+1
	end

(*Q10 --function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1*)
fun month_range(day1: int, day2: int)=
	(*
	===============================================================================================
	Arguments:
	day1	-- int
	day2  	-- int

	Returns:
	list of string
	==============================================================================================*)
	if day1>day2 then []
	else what_month(day1)::month_range(day1+1, day2)

(*Q11 --function oldest that takes a list of dates and evaluates to an (int*int*int) option.*)
fun oldest(dates_ls: myDate list)=
	(*
	===============================================================================================
	Arguments:
	dates_ls	-- list of myDate

	Returns:
	myDate
	==============================================================================================*)
	if null dates_ls
	then NONE
	else
		let val tmp=oldest(tl dates_ls)
		in if is_older(hd dates_ls, valOf tmp)
			then SOME (hd dates_ls)
			else tmp
		end


fun is_equal (x: myDate, y: myDate) =
	(*
	===============================================================================================
	Arguments:
	x	-- myDate
	y 	-- myDate

	Returns:
	boolean
	==============================================================================================*)
	if is_resonable(x) andalso is_resonable(y) then
		if #1 x <> #2 y
		then false
		else if #2 x <> #2 y
		then false
		else if #3 x <> #3 y
		then false
		else true
	else raise UndefinedDateError

fun remove_dup(ls: int list)=
	let fun delete(x: int, ls: int list)=
			if ls=[]
			then []
			else
				if x=hd ls
				then delete(x, tl ls)
				else hd ls::delete(x, tl ls)
	in
		if ls=[]
		then []
		else hd ls::remove_dup(delete(hd ls, tl ls))
	end

fun number_in_months_challenge(dates_ls: myDate list, months_ls: int list)=
	let val months_ls=remove_dup(months_ls)
	in number_in_months(dates_ls, months_ls)
	end

fun dates_in_months_challenge(dates_ls: myDate list, months_ls: int list)=
	let val months_ls=remove_dup(months_ls)
	in dates_in_months(dates_ls, months_ls)
	end

fun reasonable_date(date: myDate)=
	let
		val (y, m, d)=date;
		val is_leap_year= ((y mod 400)=0) orelse ((y mod 4)=0 andalso (y mod 100)<>0);
		val month_date_ls=
			if is_leap_year
			then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
			else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		fun get_nth(ls: int list,n: int)=
			if n=1 then hd ls
			else get_nth(tl ls, n-1)
	in
		if y<1
		then false
		else if m<1 orelse m>12
		then false
		else if d<1 orelse d>get_nth(month_date_ls, m)
		then false
		else true
	end


(*Testing Phrase*)
(*val test1 = is_older ((1,2,3),(2,3,4)) = true;

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1;

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3;

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)];

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)];

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there";

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013";

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3;

val test9 = what_month 70 = 3;

val test10 = month_range (31, 34) = [1,2,2,2];*)

(*val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31);*)
