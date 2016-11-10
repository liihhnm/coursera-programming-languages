(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, lst) =
  let fun assist (lst1, lst2) =
      case lst2 of
	  [] => NONE
	| x::xs' => if same_string(str, x) then SOME(lst1 @ xs')
		    else assist(lst1 @ [x], xs')
  in
      assist([], lst)
  end

fun get_substitutions1 (lst, name) =
  case lst of
      [] => []
    | x::xs' => let val tem = all_except_option(name, x)
		in case tem of
		       NONE => get_substitutions1(xs', name)
		     | SOME subs => subs @ get_substitutions1(xs',name)
		end

fun get_substitutions2 (lst, name) =
  let fun assist (rest, answer) =
	case rest of
	    [] => answer
	  | x::xs' => let val tem = all_except_option(name, x)
		      in case tem of
			     NONE => assist(xs', answer)
			   | SOME subs => assist(xs', answer @ subs)
		      end
  in
      assist(lst, [])
  end

fun similar_names (lst, {first = f, middle = m, last = l}) =
  let fun assist (lst1, lst2) = 
	case lst2 of
	    [] => lst1
	  | x::xs' => assist(lst1 @ [{first = x, middle = m, last = l}], xs')
  in
      assist ([{first = f, middle = m, last = l}], get_substitutions2 (lst, f))
  end
  
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

	      (* put your solutions for problem 2 here *)

fun card_color (s, r) =
  case s of
      Spades => Black
    | Clubs  => Black
    | _ => Red

fun card_value (s, r) =
  case r of
      Num i => i
    | Ace => 11
    | _ => 10

fun remove_card (lst, c, e) =
  case lst of
      [] => raise e
    | x::xs' => if x = c then xs'
		else x::remove_card(xs', c, e)

fun all_same_color (lst) =
  case lst of
      [] => true
    | x::[] => true
    | head::(neck::rest) => (card_color head = card_color neck
			     andalso all_same_color (neck::rest))

fun sum_cards (lst) =
  let fun assist (rest, sum) =
	case rest of
	    [] => sum
	  | x::xs' => assist(xs', sum + card_value x)
  in
      assist (lst, 0)
  end

fun score (lst, goal) =
  let val sum = sum_cards (lst)
  in
      let val current = if sum > goal then 3 * (sum - goal)
		    else goal - sum
      in
	  if all_same_color lst then current div 2
	  else current
      end
  end

fun officiate (cl, ml, goal) =
  let fun assist (cl, ml, hl) =
	case ml of
	    [] => score (hl, goal)
	  | (Discard c)::xs' => let val current_hl = remove_card (hl, c, IllegalMove)
				in
				    assist (cl, xs', hl)
				end
	  | Draw::xs' => case cl of
			     [] => score (hl, goal)
			   | head::rest => let val current_hl = head::hl
					       val current_s = sum_cards current_hl
					   in
					       if current_s > goal then score (current_hl, goal)
					       else assist (rest, xs', current_hl)
					   end
  in
      assist (cl, ml, [])
  end

