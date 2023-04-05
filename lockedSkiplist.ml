type node = {
  key:int;
  height:int;
  next: node array;
  lock:Mutex.t;
  mutable marked:bool;
  mutable fully_linked:bool;
}

type t = {
  head: node
}

let max_height = 10

let null_node = {key=Int.max_int; height=0; next=[||];lock=Mutex.create ();marked=false;fully_linked=false;}

let create_node key height =
  {key;height;next=Array.make (height+1) null_node;lock=Mutex.create ();marked=false;fully_linked=false;}

let get_random_level () =
  let rec count_level cur_level =
    if cur_level == max_height || Random.bool () then cur_level
    else count_level (cur_level + 1)
  in
  count_level 1

let create () = 
  let tail = create_node Int.max_int max_height in 
  let head = create_node Int.min_int max_height in 
  Array.iteri (fun index  _ -> head.next.(index) <- tail) head.next;
  {head;}

let find_in sl key preds succs = 
  let lfound = ref (-1) in 
  let pred = sl.head in 
  let rec set_level level pred = 
    let rec find_curr_node pred curr = 
      if key > curr.key then 
        let pred = curr in 
        let curr = pred.next.(level) in 
        find_curr_node pred curr 
      else
        pred,curr
    in
    let curr = pred.next.(level) in 
    let pred, curr = find_curr_node pred curr in 
    if !lfound == (-1) && key == curr.key then lfound := level;
    preds.(level) <- pred;
    succs.(level) <- curr;
    if level > 0 then set_level (level-1) pred 
    else ()
  in 
  set_level max_height pred;
  !lfound
  
let add sl key = 
  let height = get_random_level () in 
  let preds = Array.make (max_height + 1) null_node in 
  let succs = Array.make (max_height + 1) null_node in 
  let rec repeat () = 
    let lfound = find_in sl key preds succs in 
    if lfound != -1 then 
      let nodeFound = succs.(lfound) in 
      if not nodeFound.marked then 
        let rec ensure_fully_linked () = 
          if nodeFound.fully_linked then () 
          else ensure_fully_linked ()
        in 
        ensure_fully_linked ();
        false 
      else 
        repeat ()
    else
      let highestLocked = ref (-1) in 
      let finallyBlock () = 
        for level = 0 to !highestLocked do 
          try Mutex.unlock preds.(level).lock with _ -> ()
        done
      in 
      let tryBlock () = 
        let rec lockLevels level valid =
          if not (valid && level<=height) then valid 
          else 
            let pred = preds.(level) in 
            let succ = succs.(level) in 
            Mutex.try_lock pred.lock |> ignore;
            highestLocked := level;
            let valid = (not pred.marked) && (not succ.marked) && pred.next.(level) == succ in 
            lockLevels (level+1) valid 
        in
        let valid = lockLevels 0 true in 
        if not valid then 
          (finallyBlock ();
          repeat ())
        else 
          let new_node = create_node key height in 
          for level = 0 to height do 
            new_node.next.(level) <- succs.(level)
          done;
          for level = 0 to height do 
            preds.(level).next.(level) <- new_node
          done;
          new_node.fully_linked <- true;
          finallyBlock ();
          true 
      in
      tryBlock () 
  in
  repeat () 


let remove sl key = 
  let victim = ref null_node in 
  let isMarked = ref false in 
  let height = ref (-1) in 
  let preds = Array.make (max_height + 1) null_node in 
  let succs = Array.make (max_height + 1) null_node in 
  let rec repeat () = 
    let lfound = find_in sl key preds succs in 
    if lfound != -1 then victim := succs.(lfound);
    if !isMarked || (lfound !=(-1) && !victim.fully_linked && !victim.height == lfound && not !victim.marked) then 
      let alreadyMarked = ref false in
      (if not !isMarked then 
        (height := !victim.height;
        Mutex.try_lock !victim.lock |> ignore;
        if !victim.marked then 
          (try Mutex.unlock !victim.lock with _ -> ();
          alreadyMarked := true)
      else
        ((!victim).marked <- true;
        isMarked := true)));
      if !alreadyMarked then false 
      else 
        let highestLocked = ref (-1) in 
        (* Printf.printf "have I reached here!%d" !height; *)
        let finallyBlock () = 
          for level = 0 to !highestLocked do
            try Mutex.unlock preds.(level).lock with _ -> ();
          done 
        in 
        let tryBlock () = 
          let rec lockLevels level valid = 
            if not (valid && (level <= !height)) then valid 
            else 
              let pred = preds.(level) in 
              Mutex.try_lock pred.lock |> ignore;
              highestLocked := level;
              let valid = not pred.marked && pred.next.(level) == !victim in 
              lockLevels (level+1) valid 
          in
          let valid = lockLevels 0 true in 
          if not valid then (
            finallyBlock ();
            repeat ()
          )
          else
            let rec unlockLevel level = 
              if level < 0 then () 
              else (
                preds.(level).next.(level) <- !victim.next.(level);
                unlockLevel (level-1)
              )
            in
            unlockLevel !height;
            finallyBlock ();
            true
          in 
          tryBlock ()
      else
        false
  in
  repeat ()

         


let find sl key  = 
  let preds = Array.make (max_height+1) null_node in 
  let succs = Array.make (max_height+1) null_node in 
  let lfound = find_in sl key preds succs in 
  lfound != (-1) && succs.(lfound).fully_linked && not (succs.(lfound).marked)