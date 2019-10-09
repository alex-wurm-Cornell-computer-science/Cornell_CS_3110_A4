open OUnit2
open ListDictionary

(* of course add whatever code you want *)

open Dictionary

module Int = struct
  type t = int
  let compare x y =
    match Stdlib.compare x y with
    | x when x<0 -> LT
    | 0 -> EQ
    | _ -> GT
  let format fmt x =
    Format.fprintf fmt "%d" x
end;;

(* The next line creates a dictionary that maps ints to ints. *)
module IntIntDictionary = ListDictionary.Make(Int)(Int)


let make_is_empty
    (name : string)
    (input : IntIntDictionary.t)
    (expected_output : bool): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.is_empty input))

let make_size
    (name : string)
    (dict : IntIntDictionary.t)
    (expected_output : int): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.size dict))

let make_insert
    (name : string)
    (key : int)
    (value : int)
    (dict : IntIntDictionary.t)
    (expected_output : IntIntDictionary.t): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.insert key value dict))

let make_remove
    (name : string)
    (key : int)
    (dict : IntIntDictionary.t)
    (expected_output : IntIntDictionary.t): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.remove key dict))

let make_find
    (name : string)
    (key : int)
    (dict : IntIntDictionary.t)
    (expected_output : int option): test = 
  name >:: (fun _ -> 
      assert_equal expected_output (IntIntDictionary.find key dict))

let make_member
    (name : string)
    (key : int)
    (dict : IntIntDictionary.t)
    (expected_output : bool): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.member key dict))

let make_choose
    (name : string)
    (dict : IntIntDictionary.t)
    (expected_output : (int * int) option): test = 
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.choose dict))

let make_to_list
    (name : string)
    (dict : IntIntDictionary.t)
    (expected_output : (int * int) list): test = 
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.to_list dict))

let d_empty = IntIntDictionary.empty
let d_add_1 = IntIntDictionary.insert 1 1 (IntIntDictionary.empty)
let d_remove_1 = IntIntDictionary.remove 1 d_add_1
let d_add_2a = IntIntDictionary.insert 6 2 d_remove_1
let d_add_2b = IntIntDictionary.insert 12 3 d_add_2a
let d_add_existing = IntIntDictionary.insert 6 7 d_add_2b

let tests = [
  make_is_empty "is_empty: created empty listdict" d_empty true;
  make_size "size: created empty listdict" d_empty 0;

  make_insert "insert: added one key*val pair" 1 1 d_empty d_add_1;
  make_is_empty "is_empty: added one key*val pair" d_add_1 false;
  make_size "size: added one key*val pair" d_add_1 1;

  make_remove "remove: removed one key*val pair" 1 d_add_1 d_empty;
  make_is_empty "is_empty: removed one key*val pair" d_remove_1 true;
  make_size "size: removed one key*val pair" d_remove_1 0;


]



let suite = "search test suite" >::: tests

let _ = run_test_tt_main suite
