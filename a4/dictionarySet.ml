open Dictionary

module type ElementSig = sig
  type t
  include Dictionary.KeySig with type t := t
end

module Unit = struct
  type t = unit
  (* include Dictionary.ValueSig with type t := t *)
end

module type Set = sig
  module Elt : ElementSig
  type elt = Elt.t
  (* module Un : UnitSig *)
  (* type un = Un.t *)
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

    type t = DM(Elt)(Un).t

    let rep_ok s =
      (*failwith "Unimplemented"*)
      s

    let empty =
      [] (* TODO: replace [()] with a value of your rep type [t]. *)

    let is_empty s =
      (*failwith "Unimplemented"*)
      match s with
      | [] -> true
      | h::t -> false

    let size s =
      (*failwith "Unimplemented"*)
      List.length s

    let insert x s =
      (*failwith "Unimplemented"*)
      let exists = List.mem_assoc x s in

      if exists then s 
      else (x,()) :: s

    let member x s =
      (*failwith "Unimplemented"*)
      List.mem_assoc x s 

    let remove x s =
      (*failwith "Unimplemented"*)
      let exists = List.mem_assoc x s in

      if exists then List.remove_assoc x s 
      else s

    let choose s =
      (*failwith "Unimplemented"*)
      let l = size s in 
      let n = Random.int l in 
      
      match s with
      | [] -> None
      | h::t -> Some (List.nth s n)

    let fold f init s =
      (*failwith "Unimplemented"*)
      List.fold_left (fun acc (x,()) -> f x acc) init s

    let union s1 s2 =
      failwith "Unimplemented"

    let intersect s1 s2 =
      failwith "Unimplemented"

    let difference s1 s2 =
      failwith "Unimplemented"

    let to_list s =
      (*failwith "Unimplemented"*)
      List.sort compare s

    let format fmt d =
      Format.fprintf fmt "<unimplemented>" 
  end
