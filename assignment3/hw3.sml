(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer


(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals lst =
  List.filter (fn x => Char.isUpper(String.sub(x, 0))) lst

fun longest_string1 lst =
  List.foldl (fn (x, y) => if String.size x > String.size y
			   then x
			   else y)  "" lst

fun longest_string2 lst =
  List.foldl (fn (x, y) => if String.size x >= String.size y
			   then x
			   else y) "" lst

fun longest_string_helper cmp lst =
  List.foldl (fn (x, y) => if cmp (String.size x, String.size y)
			   then x
			   else y) "" lst

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = (List.foldl o (fn () => fn (x, y) => if Char.isUpper (String.sub (x, 0))
						     andalso String.size x > String.size y
						  then x
						  else y)) () ""

fun rev_string str =
  (String.implode o rev o String.explode) str

fun first_answer f lst =
  case lst of
      [] => raise NoAnswer
    | x::xs' => case f x of
		    SOME v => v
		  | NONE => first_answer f xs'

fun all_answers f lst =
  let fun assist (rest, acc) = 
      case rest of
	  [] => SOME acc
	| x::xs' => case f x of
			SOME b_lst => assist (xs', acc @ b_lst)
		      | NONE => NONE
  in
      assist (lst, [])
  end

(*****************************************************************)
      
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
      
fun count_wildcards p =
  g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
  g (fn () => 1) (fn x => String.size x) p

fun count_some_var (str, p) =
  g (fn () => 0) (fn x => if str = x then 1 else 0) p

fun check_pat p =
  let fun all_str pa =
	case pa of
	    Variable s => [s]
	  | TupleP ps => List.foldl (fn (pa, i) => i @ all_str pa) [] ps
	  | ConstructorP (_, pa) => all_str pa
	  | _ => []
      fun check lst =
	case lst of
	    [] => true
	  | x::xs' => not (List.exists (fn next => next = x) xs') andalso check xs'
  in
      (check o all_str) p
  end

fun match (v, p) =
  case (v, p) of
      (_, Wildcard) => SOME []
    | (v, Variable s) => SOME [(s, v)]
    | (Unit, UnitP) => SOME []
    | (Const _, ConstP _) => SOME []
    | (Tuple v_l, TupleP p_l) => if List.length v_l = List.length p_l
				 then all_answers match (ListPair.zip (v_l, p_l))
				 else NONE
    | (Constructor (s2, v), ConstructorP (s1, p)) => if s1 = s2
						     then match (v, p)
						     else NONE
    | (_, _) => NONE

fun first_match v p_lst =
  SOME (first_answer (fn p => match (v, p)) p_lst)
       handle NoAnswer => NONE
