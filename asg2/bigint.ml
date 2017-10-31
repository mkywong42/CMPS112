(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let listlen   = List.length
    let radix     = 10
    let radixlen  =  1
    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))
    
    (* function to recursively trim zeros*)
    let trimzeros list =
	let rec trimzeros' list' = match list' with
	    | []   -> []
	    | [0]  -> []
	    | car::cdr ->
	         let cdr' = trimzeros' cdr
		 in match car, cdr' with
                    | 0, [] -> []
		    | car, cdr' -> car::cdr'
        in trimzeros' list
;;
    (* function to compare two Bigint numbers *)

    let rec cmp list1 list2 =
        if (listlen list1) > (listlen list2) then 1
	else if (listlen list1) < (listlen list2) then -1
	else match (list1, list2) with
	   | [], []                    -> 0
	   | list1, []                 -> 1
	   | [], list2                 -> -1 
	   | car1::cdr1, car2::cdr2    -> (* must reverse Bigints before comparing *)
                let r1 = reverse list1
		in let r2 = reverse list2 
                   in if r1 > r2 then 1
                      else if r2 > r1 then -1
                      else 0 

    (* recursive function to add two Bigint numbers *)

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> trimzeros (add' list1 [carry] 0)
        | [], list2, carry   -> trimzeros (add' [carry] list2 0)
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry (* add carried digit also *)
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix) (* sum/radix = carry *)

    (* recursive function to subtract two Bigint numbers *)
    let rec sub' list1 list2 borrow = match (list1, list2, borrow) with
	| list1, [], borrow      -> trimzeros (sub' list1 [borrow] 0)
	| [], list2, 0           -> list2
        | list1, [], 0		 -> list1
        | [], list2, borrow	 -> trimzeros (sub' [borrow] list2 0)
	| car1::cdr1, car2::cdr2, borrow ->
 
         if (car1 - borrow) < car2 (* if digit1 < digit2, must borrow *)
            then let diffr = (10 + car1) - (car2 + borrow)
               in diffr mod radix :: trimzeros ((sub' cdr1 cdr2 1))
         else let diffr = car1 - car2 - borrow
	      in diffr mod radix :: trimzeros ((sub' cdr1 cdr2 0))

        
    (* add function that checks the signs before passing the values to recursion *)
    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
           then Bigint (neg1, trimzeros (add' value1 value2 0))
        else if (neg1 = Neg && neg2 = Pos) then(
           if (cmp value1 value2) = 1
              then Bigint (neg1, trimzeros (sub' value2 value1 0))
           else
              Bigint (neg2, trimzeros (sub' value2 value1 0))
        )
        else if (neg1 = Pos && neg2 = Neg) then(
           if (cmp value1 value2) = 1
              then Bigint (neg1, trimzeros (sub' value1 value2 0))
           else
              Bigint (neg2, trimzeros (sub' value2 value1 0))
        )
        else zero

    (* sub function that checks the signs before passing the correct values to recursion *)
    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
	if (neg1 = Pos && neg2 = Pos) then(
	   if (cmp value1 value2) = 1
              then Bigint (Pos, trimzeros (sub' value1 value2 0))
           else Bigint (Neg, trimzeros (sub' value2 value1 0))
        )
        else if (neg1 = Neg && neg2 = Neg) then(
           if(cmp value1 value2) = 1
              then Bigint (Neg, trimzeros (sub' value1 value2 0))
           else Bigint (Pos, trimzeros (sub' value2 value1 0))
        )
        else
           Bigint (neg1, trimzeros (add' value1 value2 0))
    
    (* function to just double a number by 2. Helps with egyptian mul & div *)
    let double number = add' number number 0
   
    (* taken from sample code arithmetic but modified for lists *)
    let rec mul' (multiplier, powerof2, multiplicand') =
       if (cmp powerof2 multiplier) = 1
          then multiplier, [0]
       else let remainder, product =
                mul' (multiplier, double powerof2, double multiplicand')
            in if (cmp remainder powerof2) = -1
                  then remainder, product
               else (sub' remainder powerof2 0), (add' product multiplicand' 0)

    (* taken from sample code arithmetic. modified for lists *)
    let emul (multiplier, multiplicand) = 
       let _, product = mul' (multiplier, [1], multiplicand)
       in product
    ;;
    
    (* taken from sample code arithmetic, but modified to check signs *)
    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
       if neg1 = neg2 then(
          if (cmp value1 value2) = 1
             then Bigint (Pos, emul (value1, value2))
          else Bigint (Pos, emul (value2, value1))
       )
       else
          (if (cmp value1 value2) = 1
             then Bigint (Neg, emul (value1, value2))
          else Bigint (Neg, emul (value2, value1)))
    
    (* taken from sample code arithmetic but modified for lists *)
    let rec divrem' (dividend, powerof2, divisor') =
	if (cmp divisor' dividend) = 1
	   then [0], dividend
        else let quotient, remainder =
                 divrem' (dividend, double powerof2, double divisor')
             in if (cmp remainder divisor') = -1
                   then quotient, remainder
                else (add' quotient powerof2 0), (sub' remainder divisor' 0)

    let divrem (dividend, divisor') = divrem' (dividend, [1], divisor')

    let ediv (dividend, divisor) =
	let quotient, _ = divrem (dividend, divisor)
	in quotient

    (* taken from sample code arithetic, ut modified to check signs *)
    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) = match (value1, value2) with
        | [0], value2 -> zero
        | value1, [0] -> (eprintf "dc: division by zero\n%!"; Bigint(Pos, [0]))
        | value1, value2 ->
        if neg1 = neg2 then(
           Bigint (Pos, ediv (value1, value2))
        )
        else Bigint (Neg, ediv (value1, value2))

    let erem (dividend, divisor) =     
       let _, remainder = divrem (dividend, divisor)
       in remainder

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
       if neg1 = neg2 then(
          Bigint (Pos, erem (value1, value2))
       )
       else Bigint (Neg, erem (value1, value2))
          
    (* multiply value to result until exponent-- becomes 0 *)
    let rec pow' (value, exponent, result) =
        if (cmp exponent [0]) = 0 (* base case exponent = 0, return result *)
           then result
        else pow' (value, sub' exponent [1] 0, emul(value, result)) (* continuously multiply value and result *)
  
    (* function that does value1^value2 *)
    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) = match (value1, value2) with
        | value1, [0]    -> Bigint (Pos, [1]) (* anything to the power of zero is 1 *)
        | [0], value2    -> zero (* 0 to the power of anything is zero *)
        | value1, value2 ->
        if (neg1 = Pos && neg2 = Pos) then(
           Bigint (Pos, pow' (value1, value2, [1]))
        )
        else if neg2 = Neg then zero
        else 
	   let mod2 = (erem (value2, [2])) in (* if exponent is odd n%2 = 1? *)
           if (cmp mod2 [1]) = 0
              then Bigint (Neg, (pow' (value1, value2, [1]))) (* negative *)
           else Bigint (Pos, (pow' (value1, value2, [1]))) (* else, even exponent means positive *)

end

