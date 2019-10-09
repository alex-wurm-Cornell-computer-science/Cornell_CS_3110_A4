(** [format_assoc_list fmt_key fmt_val fmt lst] formats an association 
    list [lst] as a dictionary.  The [fmt_key] and [fmt_val] arguments
    are formatters for the key and value types, respectively.  The
    [fmt] argument is where to put the formatted output. *)
let format_list format_val fmt lst =
  Format.fprintf fmt "[";
  List.iter (fun v -> Format.fprintf fmt "%a;"
                format_val v) lst;
  Format.fprintf fmt "]"

let format_string fmt x =
  Format.fprintf fmt "%s" x

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
        (* let rec add_file dir_handle acc = 
           try 
            let file_name = Unix.readdir dir_handle in 
            let r = Str.regexp "^.*\\.txt$" in
            if Str.string_match r file_name 0 then
              add_file dir_handle (file_name::acc)
            else add_file dir_handle acc
           with | End_of_file -> acc
           in *)

        (*          
        match dir_handle with
        | Unix.Unix_error (Unix.ENOENT) ("opendir") (d) -> raise (Not_found)
        | dir_handle ->  *)
(* 
        let rec iter_dir d acc =
          try 
            let f = (Unix.readdir d) in 
            let  = Str.regexp "^.*\\.txt$" in
            if Str.string_match r f 0 then
              iter_dir d (f::acc) else iter_dir d acc
          with End_of_file -> Unix.closedir d; acc;
        in

        let dir_handle = Unix.opendir d in
        iter_dir dir_handle [] *)

        let valid_file_name = Str.regexp "^.*\\.txt$" in
        let valid_preword = Str.regexp "\\S+" in
        let valid_word = Str.regexp "(\\w)|\\w(\\S+?)\\w" in 


        let rec iter_dir dir acc =
          try 
            let f = (Unix.readdir dir) in 
            if Str.string_match valid_file_name f 0 then
              iter_dir dir (f::acc) else iter_dir dir acc
          with End_of_file -> Unix.closedir dir; acc;
        in

        try
          let dir_handle = Unix.opendir d in 
          (* iter_dir dir_handle [] *)
        with Unix.Unix_error (Unix.ENOENT, "opendir", d) -> raise (Not_found)

        let file_list = iter_dir d [] in 
        let idx_dict = D.empty in 

        let words_of_file f =
          
        let rec words_from_files files acc =
          match files with
            | [] -> acc
            | h::t -> 


      let words idx = 
        failwith "Unimplemented"
         
        (* let word = Str.regexp "^\\w\\S\\w$" in 

         let rec read_words wlist acc = 
          match wlist with
          | [] -> acc
          | h::t -> if Str.string_match h word 0 then read_words t (h :: acc) else

        in
       *)

      let to_list idx =
        failwith "Unimplemented"

      let or_not idx ors nots =
        failwith "Unimplemented"

      let and_not idx ands nots =
        failwith "Unimplemented"

      let format fmt idx =
        format_list format_string fmt idx
    end