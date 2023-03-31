open Kcas
type node = {
  key : int;
  height : int;
  next : node Loc.t array;
}

type t = { head : node }

let max_height = 10

let get_random_level () =
  let rec count_level cur_level =
    if cur_level == max_height || Random.bool () then cur_level else count_level (cur_level + 1)
  in
  count_level 1

let null_node = {key=Int.max_int; height=0; next=[||]}

let create () =
  let tail =
    {
      key = Int.max_int;
      height = max_height;
      next = Array.make (max_height+1) (Loc.make null_node);
    }
  in
  let head =
    {
      key = Int.min_int;
      height = max_height;
      next = Array.init (max_height+1) (fun _ -> Loc.make tail);
    }
  in
  { head }

let create_node key =
  let h = get_random_level () in
  { key; height = h; next = Array.init (h+1) (fun _ -> Loc.make null_node) }

let rec find_in key node level preds succs = Tx.(
  let* next_node = get node.next.(level) in
  if
    key > next_node.key
  then find_in key next_node level preds succs
  else (
    preds.(level) <- node;
    succs.(level) <- next_node;
    if level == 0 then return (key == next_node.key)
    else find_in key node (level - 1) preds succs)
)

let find slist key =
  let rec find_in node level = Tx.(
    let* next_node = get node.next.(level) in
    if
      key > next_node.key
    then find_in next_node level 
    else (
      if level == 0 then return (key == next_node.key)
      else find_in node (level - 1))
  ) in 
  let h = slist.head in
  find_in h max_height

let add slist key = Tx.(
  let new_node = create_node key in
  let preds = Array.make (max_height+1) null_node in
  let succs = Array.make (max_height+1) null_node in
  let h = slist.head in
  let* pres = find_in key h max_height preds succs in
  let height = new_node.height in
  let rec assign index =
    set new_node.next.(index) succs.(index) >>
    set preds.(index).next.(index) new_node >>
    if index == 0 then return true else assign (index - 1)
  in
  if not pres then assign height else return false
)

let remove slist key = Tx.(
  let preds = Array.make (max_height+1) null_node in
  let succs = Array.make (max_height+1) null_node in
  let h = slist.head in
  let* pres = find_in key h max_height preds succs in
  let rec reassign key index =
    let prev_node = preds.(index) in
    let curr_node = succs.(index) in
    if curr_node.key == key then
      prev_node.next.(index) <- curr_node.next.(index);
    if index == 0 then return true else reassign key (index - 1)
  in
  if pres then reassign key max_height else return false
)