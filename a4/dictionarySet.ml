open Dictionary

(** [format_elt_list fmt_key fmt lst] formats an element 
    list [lst] as a dictionary. The [fmt_key] argument
    is a formatter for the key type. The
    [fmt] argument is where to put the formatted output. *)
let format_elt_list format_elt fmt lst =
  Format.fprintf fmt "[";
  List.iter (fun (k,v) -> Format.fprintf fmt "%a; "
                format_elt k) lst;
  Format.fprintf fmt "]"

module type ElementSig = sig
  type t
  include Dictionary.KeySig with type t := t
end

module Unit = struct
  type t = unit
  let format fmt d =
      Format.fprintf fmt "()"
end

module type Set = sig
  module Elt : ElementSig
  type elt = Elt.t
  module Un = Unit
  type un = Un.t
  (* module D = Dictionary.DictionaryMaker(Elt)(Un).t *)
  type t
  val rep_ok : t  -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : elt -> t -> t
  val member : elt -> t -> bool
  val remove : elt -> t -> t
  val union : t -> t -> t
  val intersect : t -> t -> t
  val difference : t -> t -> t
  val choose : t -> elt option
  val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> elt list
  val format : Format.formatter -> t -> unit
end

module Make =
  functor (E : ElementSig) ->
  functor (DM : DictionaryMaker) ->
  struct
    module Elt = E
    module Un = Unit
    type elt = Elt.t
    type un = Un.t 

    (* TODO: change type [t] to something involving a dictionary *)
    (** AF: TODO: document the abstraction function.
        RI: TODO: document any representation invariants. *)

    module Dict = DM(Elt)(Un)

    type t = Dict.t

    let compare x y =
    match Elt.compare (fst x) (fst y) with
    | LT -> -1
    | EQ -> 0
    | GT -> 1

    let rep_ok s =
      (*failwith "Unimplemented"*)
      s

    let empty =
      (* TODO: replace [()] with a value of your rep type [t]. *)
      Dict.empty

    let is_empty s =
      (*failwith "Unimplemented"*)
      Dict.is_empty s

    let size s =
      (*failwith "Unimplemented"*)
      Dict.size s

    let insert x s =
      (*failwith "Unimplemented"*)
      Dict.insert x () s

    let member x s =
      (*failwith "Unimplemented"*)
      Dict.member x s 

    let remove x s =
      (*failwith "Unimplemented"*)
      Dict.remove x s

    let choose s =
      (*failwith "Unimplemented"*)
      let tup = Dict.choose s in 
      match tup with
      | None -> None
      | Some (a,b) -> Some a

    let fold f init s =
      (*failwith "Unimplemented"*)
      Dict.fold (fun k v acc -> f k acc) init s
      (* List.fold_left (fun acc (x,()) -> f x acc) init *)
    
    let union s1 s2 =
      (* failwith "Unimplemented" *)
      
      let rec unite_sets d1 d2 = 
        
        let l2 = Dict.to_list d2 in 

        match l2 with
        | [] -> Dict.empty
        | h::t -> let new_d1 = Dict.insert (fst h) (snd h) d1 in
                  let new_d2 = Dict.remove (fst h) d2 in 
                  unite_sets new_d1 new_d2
      in 

      unite_sets s1 s2

    let intersect s1 s2 =
      failwith "Unimplemented"

      (* let rec intersect_sets d1 d2 =

        let l1 = Dict.to_list d1 in 
        let l2 = Dict.to_list d2 in 

        match l2 with
        | [] -> Dict.empty
        | h::t -> if List.mem (fst h) l1 && not (List.mem (fst h) l2) 
                  then let new_d1 = Dict.remove (fst h) d1 in
                        intersect_sets new_d1 d2
                  else  *)


    let difference s1 s2 =
      failwith "Unimplemented"

    let to_list s =
      (*failwith "Unimplemented"*)
      Dict.to_list s |> List.map fst

    let format fmt d =
      (* Format.fprintf fmt "<unimplemented>"  *)
      d |> Dict.to_list |> format_elt_list Elt.format fmt
  end
