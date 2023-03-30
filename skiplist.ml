type 'a node = {
  key : int;
  value : 'a option;
  height : int;
  next : 'a node option array;
}

type 'a t = { head : 'a node }

let max_height = 10

let get_random_level () =
  let rec count_level cur_level =
    if cur_level == max_height-1 || Random.float 1.0 <= 0.5 then cur_level else count_level (cur_level + 1)
  in
  count_level 1

let create () =
  let tail =
    {
      key = Int.max_int;
      value = None;
      height = max_height;
      next = Array.make max_height None;
    }
  in
  let head =
    {
      key = Int.min_int;
      value = None;
      height = max_height;
      next = Array.make max_height (Some tail);
    }
  in
  { head }

let create_node key value =
  let h = get_random_level () in
  { key; value = Some value; height = h; next = Array.make h None }

let rec find_in key node level preds succs =
  (* print_int node.key; *)
  (* print_newline (); *)
  match node.next.(level) with
  | Some next_node ->
      if
        (* print_int next_node.key; *)
        (* print_newline (); *)
        key > next_node.key
      then find_in key next_node level preds succs
      else (
        preds.(level) <- Some node;
        succs.(level) <- Some next_node;
        if level == 0 then key == next_node.key
        else find_in key node (level - 1) preds succs)
  | None -> false

let find key slist =
  let preds = Array.make max_height None in
  let succs = Array.make max_height None in
  let h = slist.head in
  find_in key h (max_height - 1) preds succs

let add key value slist =
  let new_node = create_node key value in
  let preds = Array.make max_height None in
  let succs = Array.make max_height None in
  let h = slist.head in
  let pres = find_in key h (max_height - 1) preds succs in
  let height = new_node.height in
  let rec assign index =
    new_node.next.(index) <- succs.(index);
    (Option.get preds.(index)).next.(index) <- Some new_node;
    if index == 0 then () else assign (index - 1)
  in
  if not pres then assign (height - 1) else ()

let remove key value slist =
  let preds = Array.make max_height None in
  let succs = Array.make max_height None in
  let h = slist.head in
  let pres = find_in key h (max_height - 1) preds succs in
  let rec reassign key index =
    let prev_node = Option.get preds.(index) in
    let curr_node = Option.get succs.(index) in
    if curr_node.key == key then
      prev_node.next.(index) <- curr_node.next.(index);
    if index == 0 then () else reassign key (index - 1)
  in
  if pres then reassign key (max_height - 1) else ()
