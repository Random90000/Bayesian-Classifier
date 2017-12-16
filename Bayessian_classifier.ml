module StrMap = Map.Make(String);;

(* Input: file name; output: Reverse list of words*)
let rec rfread_words f rst words   =
 let letter_check p = (*проверить, является ли данное место в строке буквой или цифрой*)
   ((65 <= p) && (p <= 90)) || ((97 <= p) && (p <= 122)) (*английские*)
   || ((192 <= p) && (p <= 223)) || ((224 <= p) && (p <= 255)) (*русские*)
   || ((48 <= p) && (p <= 57)) (*цифры-нужны нам, т.к. мы этой же функцией читаем базу*)
 in
	try let c = input_char f in
		if letter_check(Char.code c) then (*both cases are tail recurive*)
			let cs = String.make 1 c in 
			rfread_words f false (match words with 
				h::t when not rst -> (h^cs)::t
				| lst -> cs::lst)
		else	rfread_words f true words
	with _-> words
;;
let read_words fname = 
	let f = open_in fname in
	let words = List.rev (rfread_words f false []) in
	close_in f;
	words
;;
(* Input: list of words; output: word->frequency map*)
let rec direct_frequency frq = function
	w::tail ->
		let n = try StrMap.find w frq with _ -> 0 in
		direct_frequency (StrMap.add w (n+1) frq) tail
	| [] -> frq
;;
(* Input: list of [word frequency] pairs as a plain String list;
   output: word->frequency map*)
let rec db_frequency frq = function
	w::c::tail -> (db_frequency (StrMap.add w (int_of_string   c) frq) tail)
	| [] -> frq
	| _ -> raise (Invalid_argument "A pair expected")
;;
(* Let it accept both databases and sourse texts *)
let frequency words = 
 try db_frequency StrMap.empty words
 with |_ -> direct_frequency StrMap.empty words 
;;
let smoothed_prob smooth frq len word =
	let count = try StrMap.find word frq with _ -> 0 in
	( float_of_int count +. smooth) /. ( float_of_int len +. smooth)
;;
let bayesian_text_prob_log smooth known_frq new_frq =
	let total_known = StrMap.fold (fun _ v acc -> acc + v) known_frq 0 in
	let total_new = StrMap.fold (fun _ v acc -> acc + v) new_frq 0 in
	(
		(* сумма логарифмов вероятностей слова в тексте-образце
			по каждому слову в исследуемом тексте *)
		StrMap.fold (fun word new_count ->
			(+.) (( float_of_int new_count)  *. log(smoothed_prob smooth known_frq total_known word))
		) new_frq 0.
	) /. (float_of_int total_new)  (*делим на число слов в исследуемом тексте для нормировки, 
	    что не влияет на последующую оценку наибольшего сходства*)
;;
let list_max lst = 
	List.fold_left (
	fun (i,v,l) x -> if x > v then (l,x,l+1) else (i,v,l+1)
	) (0,(List.hd lst),0) lst
;;
let print_classify smooth known_files new_file =
	let new_frq = frequency (read_words new_file) in
	(*StrMap.iter(Format.printf "%s -> %d\n")  new_frq;*)
	let estimated_prob = List.map ( 
			fun f -> bayesian_text_prob_log smooth (frequency (read_words f)) new_frq 
			) known_files in
	List.iter2 (Format.printf "Log probability of similarity to %s : %f\n") known_files estimated_prob;
	let (i,_,_) = list_max estimated_prob in
	Format.printf "The most likely source is %s\n" (List.nth known_files i)
;;
(*Можно читать данные о частотности слов из баз*)
print_classify 1. ["base_1.ml"; "base_2.ml"] "new_text.ml";;

(*.. или сразу считать в тексте *)
print_classify 1. ["autor_1.ml"; "autor_2.ml"] "new_text.ml";; 
