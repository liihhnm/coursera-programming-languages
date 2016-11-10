(*
fun is_order(date1 : int * int * int, date2 : int * int * int) =
  if (#1 date1) > (#1 date2) then false
  else if (#1 date1) < (#1 date2) then true
  else
      if (#2 date1) > (#2 date2) then false
      else if (#2 date1) < (#2 date2) then true
      else
	  if (#3 date1) > (#3 date2) then false
	  else if (#3 date1) < (#3 date2) then true
	  else false
*)
fun is_order(date1 : int * int * int, date2 : int * int * int) =
    let 
        val y1 = #1 date1
        val m1 = #2 date1
        val d1 = #3 date1
        val y2 = #1 date2
        val m2 = #2 date2
        val d2 = #3 date2
    in
        y1 < y2 orelse (y1=y2 andalso m1 < m2)
                orelse (y1=y2 andalso m1=m2 andalso d1 < d2)
    end
	
fun number_in_month(dates : (int * int * int) list, month : int) =
  if null dates   then 0
  else
      if #2 (hd dates) = month then 1 + number_in_month(tl dates, month)
      else 0 + number_in_month(tl dates, month)

fun number_in_months(dates : (int * int * int) list, months: int list) =
  if null months then 0
  else
      number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int * int * int) list, month : int) =
  if null dates then []
  else
      if #2 (hd dates) = month then (hd dates)::dates_in_month(tl dates, month)
      else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int * int * int) list, months : int list) =
  if null months then []
  else
      dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(s_lst : string list, n : int) =
  if n = 1 then hd s_lst
  else get_nth(tl s_lst, n - 1)

fun date_to_string(date : int * int * int) =
  let val months = ["January", "February", "March", "April", "May",
		"June", "July", "August", "September", "October",
		"November", "December"]
  in get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum : int, numbers : int list) =
  if sum > hd numbers
  then 1 + number_before_reaching_sum(sum - hd numbers, tl numbers)
  else 0

fun what_month(day : int) =
  let val days_number = [31, 28, 31, 30, 31, 30,
			 31, 31, 30, 31, 30, 31]
      fun what_month2(day : int, days_number : int list) =
	if day <= hd days_number then 1
	else 1 + what_month2(day - hd days_number, tl days_number)
  in what_month2(day, days_number)
  end

fun month_range(day1 : int, day2 : int) =
  if day1 > day2 then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates : (int * int * int) list) =
  if null dates then NONE
  else
      let val tl_ans = oldest(tl dates)
      in if isSome tl_ans andalso is_order(valOf tl_ans, hd dates)
	 then tl_ans
	 else SOME (hd dates)
      end

(*fun number_in_months_challenge()= *)
