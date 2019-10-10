(** [format_assoc_list fmt_key fmt_val fmt lst] formats an association 
    list [lst] as a dictionary.  The [fmt_key] and [fmt_val] arguments
    are formatters for the key and value types, respectively.  The
    [fmt] argument is where to put the formatted output. *)

module type Engine = sig
  type idx
  val index_of_dir : string -> idx
  val words : idx -> string list
  val to_list : idx -> (string * string list) list
  val or_not  : idx -> string list -> string list -> string list
  val and_not : idx -> string list -> string list -> string list
  val format : Format.formatter -> idx -> unit
end

module Make = 
  functor (S:DictionarySet.Set with type Elt.t = string)
    -> functor (D:Dictionary.Dictionary with type Key.t = string
                                         and type Value.t = S.t) 
    -> struct
      (* TODO: replace [unit] with a type of your own design. *)
      (** AF: TODO: document the abstraction function.
          RI: TODO: document any representation invariants. *)
      type idx = D.t

      let index_of_dir d =
        (* failwith "Unimplemented" *)

        let valid_file_name = Str.regexp "^.*\\.txt$" in
        (* let valid_preword = Str.regexp "\\S+" in
           let valid_word = Str.regexp "[A-Za-z0-9(\\S*?)[A-Za-z0-9]|[A-Za-z0-9]" in  *)
        (* let whitespace = Str.regexp "\\s+" in  *)
        let boundary_character = Str.regexp "[A-Z-a-z0-9]" in 


        let rec iter_dir dir acc =
          try 
            let f = (Unix.readdir dir) in 
            if Str.string_match valid_file_name f 0 then
              iter_dir dir (f::acc) else iter_dir dir acc
          with End_of_file -> Unix.closedir dir; acc
        in

        let preword_to_word p = 
          try
            let len = Stdlib.String.length p in 
            let n1 = Str.search_forward boundary_character p 0 in 
            let n2 = Str.search_backward boundary_character p len in 
            let sub_len = (n2 - n1) + 1 in 
            Stdlib.String.sub p n1 sub_len 
          with
          | Not_found -> ""
        in 

        let rec break_and_insert file prewords dict = 
          match prewords with
          | [] -> dict
          | h::t ->
            let next_word = Stdlib.String.lowercase_ascii (preword_to_word h) in 
            if D.member next_word dict then 
              let curr_file_set = dict |> D.find next_word |> Option.get in 
              let new_file_set = S.insert file curr_file_set in 
              let new_dict = D.insert next_word new_file_set dict in 
              break_and_insert file t new_dict
            else 
              let init_file_set = S.empty in 
              let new_file_set = S.insert file init_file_set in 
              let new_dict = D.insert next_word new_file_set dict in 
              break_and_insert file t new_dict
        in 


        let rec iter_file f in_chan dict =
          try 
            let line = input_line in_chan in 
            let prewords = (Stdlib.String.split_on_char ' ' line) in (*Str.split whitespace line in  *)
            let all_words = List.map preword_to_word prewords in 
            let words = List.filter (fun x -> not (x = "")) all_words in 
            let updated_dict = break_and_insert f words dict in 
            iter_file f in_chan updated_dict
          with End_of_file -> close_in in_chan; dict
        in  

        let rec iter_file_list lst dict = 
          match lst with
          | [] -> dict
          | h::t -> let new_dict = iter_file h (open_in (d^Filename.dir_sep^h)) dict in 
            iter_file_list t new_dict
        in

        try 
          let dir_handle = Unix.opendir d in 
          let file_list = iter_dir dir_handle [] in 
          iter_file_list file_list D.empty
        with
          Unix.Unix_error (Unix.ENOENT, "opendir", d) -> raise (Not_found)

      let words idx = 
        (* failwith "Unimplemented" *)

        let rec get_words idx (acc:string list) = 
          let tmp = D.choose idx in 
          match tmp with 
          | None -> acc
          | Some s -> let k = (fst s:string) in get_words (D.remove k idx) (k::acc)
        in 

        get_words idx []

      let to_list idx =
        (*failwith "Unimplemented"*)
      (* D.to_list idx  *)
        let rec get_pairs idx acc = 
          let tmp = D.choose idx in 
          match tmp with 
          | None -> acc
          | Some s -> let k = (fst s) in 
                      let v = S.to_list (snd s) in 
                      get_pairs (D.remove k idx) ((k,v)::acc)
        in 

        get_pairs idx []

      let or_not idx ors nots =
        (* failwith "Unimplemented" *)

        let rec acc_ors ors_lst idx acc =
          match ors_lst with 
          | [] -> acc 
          | h::t -> if D.member h idx then (
                      match D.find h idx with 
                      | None -> acc_ors t idx acc 
                      | Some s -> let new_acc = s |> S.to_list |> List.append acc |> List.sort_uniq compare in 
                                  acc_ors t (D.remove h idx) new_acc
                      )
                    else acc_ors t idx acc 
        in 

        let rec rem_nots nots_lst acc =
          match nots_lst with 
          | [] -> acc 
          | h::t -> if List.mem h acc then 
                    rem_nots t (List.filter (fun x -> x <> h) acc)
                    else rem_nots t acc 
        in 

        rem_nots nots (acc_ors ors idx [])

      let and_not idx ands nots =
        failwith "Unimplemented"

      let format fmt idx =
        (* format_list format_string fmt idx *)
        D.format fmt idx


    end