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

let make_fold
    (name : string)
    (func : IntIntDictionary.key -> IntIntDictionary.value -> 'a -> 'a)
    (init : int)
    (dict : IntIntDictionary.t)
    (expected_output : int): test =
  name >:: (fun _ ->
      assert_equal expected_output (IntIntDictionary.fold func init dict))

let d_empty = IntIntDictionary.empty
let d_add_1 = IntIntDictionary.insert 1 1 (IntIntDictionary.empty)
let d_choose_1 = (1,1)
let d_remove_1 = IntIntDictionary.remove 1 d_add_1
let d_add_2a = IntIntDictionary.insert 6 2 d_remove_1
let d_choose_2a = (6,2)
let d_add_2b = IntIntDictionary.insert 12 3 d_add_2a
let d_add_existing = IntIntDictionary.insert 6 7 d_add_2b

let tests = [
  make_is_empty "is_empty: created empty listdict" d_empty true;
  make_size "size: created empty listdict" d_empty 0;
  make_choose "choose: created empty listdict" d_empty None;

  make_insert "insert: added one key*val pair" 1 1 d_empty d_add_1;
  make_is_empty "is_empty: added one key*val pair" d_add_1 false;
  make_size "size: added one key*val pair" d_add_1 1;
  make_choose "choose: from dict with one element" d_add_1 (Some d_choose_1);
  make_fold "fold: sum of dict with one element" (fun k v acc -> k*2 + v*3 + acc) 0 d_add_1 5;

  make_remove "remove: removed one key*val pair" 1 d_add_1 d_empty;
  make_is_empty "is_empty: removed one key*val pair" d_remove_1 true;
  make_size "size: removed one key*val pair" d_remove_1 0;
  make_choose "choose: from newly empty listdict" d_remove_1 None;
  make_fold "fold: sum of dict with empty listdict" (fun k v acc -> k*2 + v*3 + acc) 0 d_remove_1 0;

  make_insert "insert: add one of two key*val pairs" 6 2 d_remove_1 d_add_2a;
  make_is_empty "is_empty: add one of two key*val pairs" d_add_2a false;
  make_size "size: add one of two key*val pairs" d_add_2a 1;
  make_choose "choose: add one of two key*val pairs" d_add_2a (Some d_choose_2a);
  make_fold "fold: sum of dict with one element" (fun k v acc -> k*2 + v*3 + acc) 0 d_add_2a 18;

  make_insert "insert: add two of two key*val pairs" 12 3 d_add_2a d_add_2b;
  make_is_empty "is_empty: add two of two key*val pairs" d_add_2b false;
  make_size "size: add two of two key*val pairs" d_add_2b 2;
  make_find "find: add two of two key*val pairs" 12 d_add_2b (Some 3);
  make_member "member: add two of two key*val pairds" 12 d_add_2b true;
  make_find "find: looking for nonexisting element" 10 d_add_2b None;
  make_member "member: looking for nonexisting element" 10 d_add_2b false;
  make_fold "fold: sum of dict with one element" (fun k v acc -> k*2 + v*3 + acc) 0 d_add_2b 51;
  make_insert "insert: adding element with existing key" 6 7 d_add_2b d_add_existing;
  make_is_empty "is_empty: add two of two key*val pairs" d_add_existing false;
  make_size "size: add two of two key*val pairs" d_add_existing 2;
  make_fold "fold: sum of dict with one element" (fun k v acc -> k*2 + v*3 + acc) 0 d_add_existing 66;
]

let suite = "search test suite" >::: tests

let _ = run_test_tt_main suite
