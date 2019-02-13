let print_int_list  l =
 let rec list l i =
  if i < List.length l then (
   Printf.printf "%d " (List.nth l i);
   list l (i+1))
  else Printf.printf "\n"
in list l 0;;

let print_string_list  l =
 let rec list l i =
  if i < List.length l then (
   Printf.printf "%s " (List.nth l i);
   list l (i+1))
  else Printf.printf "\n"
in list l 0;;


let first (a,b) = a;;
let second (a,b) = b;;

let (>>) a f = f a;;

(*string -> word list*)
let text_all_words t =
 let letter_check place = (*is character letter or numeral*)
  let p = int_of_char(t.[place]) in
   if ((65 <= p) && (p <= 90)) ||((97 <= p) && (p <= 122)) (*ENG*)
   || ((192 <= p) && (p <= 223)) || ((224 <= p) && (p <= 255)) (*RUS*) then true 
   else false
 in
     
  let word_take place = 
   let rec w_t p s = 
    if p < String.length t then (
     if letter_check p then w_t (p + 1) (s ^ String.make 1 t.[p])
     else (p + 1,s) )
	else (p,s)
   in w_t place ""
  in 
   let rec all_words place l = 
    if place < String.length t then (
     let com = word_take place in
      if String.length (second com) = 0 then all_words (place + 1) l 
     else all_words (first com) ((second com) :: l) )
    else l
in all_words 0 [];;

let string_eq l p =
   let rec s_e n = 
    if n < String.length p then (
     if p.[n] = l.[n] then s_e (n + 1)
     else false)
   else true
   in
  if String.length l <> String.length p then false 
  else  s_e 0;;
  
let list_index l s = 
 let rec index l i = 
  match l with 
   l1 :: ls -> if string_eq l1 s then i else index ls (i + 1)
   |_ -> failwith "1"
 in index l 0;;  
  
let frequency l = 
let word_list = ref [] in
let fr_array = Array.init (List.length l) (fun _ -> 0) in 
 let exist t s = List.mem s t in
  for i = 0 to List.length l - 1 do
   let word = List.nth l i in
     if exist !word_list word then (let place = list_index !word_list word in
	   fr_array.(place) <- fr_array.(place) + 1) 
       else (word_list:= !word_list @ [word]; let place = List.length !word_list - 1 in
	   fr_array.(place) <- fr_array.(place) + 1)
  done; (!word_list, fr_array);; 

let make_very_good input = 
 let array = frequency(text_all_words (input)) in
 let list = array >> second >> Array.to_list >> List.filter (fun x -> x <> 0) in
  (first array, list);;	 
	 
let read_text input = 
let s = ref "" in
try 
 let rec read f =
  s:= (!s ^ input_line f ^ " "); read f
 in read input
with End_of_file -> !s;;
 
let take_string file value =
let j = ref value in
let s = ref "" in
 while !j <> 32 do
  s := !s ^ (String.make 1 (char_of_int !j));
  j := input_byte file ;
 done;
!s;; 
 
let read_freq input = 
let word_list = ref [] in
let repeat_list = ref [] in
 let rec read f =
  let value = input_byte f in
   if (47 < value) && (value < 58) then (repeat_list := (int_of_string (take_string f value)) :: !repeat_list; read f)
   else if value = 32 then read f
   else (word_list := (take_string f value) :: !word_list; read f)
 in
try
read input 
with End_of_file -> (!word_list, !repeat_list);;

let write_freq pair output = 
let word_list = first (pair) in
let repeat_list = second (pair) in 
 let write f =
  for i = 0 to List.length word_list - 1 do 
   output_string f ((List.nth word_list i) ^ " " ^ (string_of_int(List.nth repeat_list i)) ^ " ");
  done;
 in 
try 
 write output;
with End_of_file -> ();;  

let input = open_in "autor_1";;
let output= open_out "base_1";;
 write_freq (make_very_good (read_text input)) output;;
close_in input;;
close_out output;;

let input = open_in "autor_2";;
let output= open_out "base_2";;
 write_freq (make_very_good (read_text input)) output;;
close_in input;;
close_out output;;

