open Dictionary;;
open ListDictionary;;
open DictionarySet;;
open Engine;;

#directory "_build";;

#load_rec "listDictionary.cmo";;
#load_rec "dictionarySet.cmo";;
#load_rec "engine.cmo";;

module String = struct
  type t = string
  let compare x y =
    match Stdlib.compare x y with
    | x when x<0 -> LT
    | 0 -> EQ
    | _ -> GT
  let format fmt x =
    Format.fprintf fmt "%s" x
end;;

module S = DictionarySet.Make(String)(ListDictionary.Make)
module D = ListDictionary.Make(String)(DictionarySet.Make(String)(ListDictionary.Make))
module E = Engine.Make(S)(D);;

#install_printer E.format;;
#install_printer S.format;;
#install_printer D.format;;
