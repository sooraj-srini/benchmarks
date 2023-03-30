(* A write heavy workload with threads with 50% adds and 50% removes. *)
let write_heavy_workload num_elems num_threads = 
  let sl = Skiplist.create () in
  (* let elems = Array.init num_elems (fun _ -> Random.int 10000) in  *)
  let push = (fun () -> Domain.spawn ( fun () ->
    let start_time = Unix.gettimeofday () in 
    for i = 0 to (num_elems - 1) do ( 
      if (i/2) < num_elems/2 then 
        Skiplist.add (i) 1 sl |> ignore
    else
        Skiplist.remove (i - (num_elems/2)) sl |> ignore)
    done;
    start_time
  )) in 
  let threads = List.init num_threads (fun _ -> push ()) in 
  let start_time_threads = List.map (fun domain -> Domain.join domain) threads in 
  let end_time = Unix.gettimeofday () in 
  let time_diff = end_time -. (List.nth start_time_threads 0) in 
  time_diff

(* A regular workload with 90% reads, 9% adds and 1% removes. *)
let read_heavy_workload num_elems num_threads = 
  let sl = Skiplist.create () in
  let elems = Array.init num_elems (fun _ -> Random.int 10000) in 
  let push = (fun () -> Domain.spawn ( fun () ->
    let start_time = Unix.gettimeofday () in 
    for i = 0 to (num_elems - 1) do ( 
      Domain.cpu_relax ();
      if i mod 1000 < 90 then 
        Skiplist.add elems.(i) 1 sl |> ignore
      else if i mod 1000 >= 90 && i mod 1000 < 100 then 
        Skiplist.remove elems.(i) sl |> ignore
      else 
        Skiplist.find elems.(i) sl |> ignore
    )
    done;
    start_time
  )) in 
  let threads = List.init num_threads (fun _ -> push ()) in 
  let start_time_threads = List.map (fun domain -> Domain.join domain) threads in 
  let end_time = Unix.gettimeofday () in 
  let time_diff = end_time -. (List.nth start_time_threads 0) in 
  time_diff
  
  
  let moderate_heavy_workload num_elems num_threads = 
    let sl = Skiplist.create () in
    let elems = Array.init num_elems (fun _ -> Random.int 10000) in 
    let push = (fun () -> Domain.spawn ( fun () ->
      let start_time = Unix.gettimeofday () in 
      for i = 0 to (num_elems - 1) do ( 
        Domain.cpu_relax ();
        let prob = Random.float 1.0 in 
        if prob < 0.2 then 
          Skiplist.add (Random.int 10000) 1 sl |> ignore
        else if prob >= 0.2 && prob < 0.3 then 
          Skiplist.remove (Random.int 10000) sl |> ignore
        else 
          Skiplist.find elems.(i) sl |> ignore
      )
      done;
      start_time
    )) in 
    let threads = List.init num_threads (fun _ -> push ()) in 
    let start_time_threads = List.map (fun domain -> Domain.join domain) threads in 
    let end_time = Unix.gettimeofday () in 
    let time_diff = end_time -. (List.nth start_time_threads 0) in 
    time_diff
    


let bench ~workload_type ~num_elems ~num_threads () =
  let workload =
    if workload_type = "read_heavy" then 
      read_heavy_workload
    else if workload_type = "moderate_heavy" then 
      moderate_heavy_workload
    else 
      write_heavy_workload
    in
  let results = ref [] in
  for i = 1 to 10 do
    let time = workload num_elems num_threads in
    if i > 1 then results := time :: !results
  done;
  let results = List.sort Float.compare !results in
  let median_time = List.nth results 4 in
  let median_throughput = Float.of_int num_elems /. median_time in
  Benchmark_result.create_generic ~median_time ~median_throughput ("sequential_skiplist_"^workload_type)