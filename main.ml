let benchmark_list =
  [
    Bench_atomic_skiplist.bench ~workload_type:"read_heavy" ~num_elems:2_000_000 ~num_threads:12;
    Bench_atomic_skiplist.bench ~workload_type:"moderate_heavy" ~num_elems:2_000_000 ~num_threads:12;
    Bench_atomic_skiplist.bench ~workload_type:"balanced_heavy" ~num_elems:2_000_000 ~num_threads:12;
    Bench_atomic_skiplist.bench ~workload_type:"write_heavy" ~num_elems:2_000_000 ~num_threads:12;
    (* Bench_skiplist.bench ~workload_type:"read_heavy" ~num_elems:2_000_000 ~num_threads:1;
    Bench_skiplist.bench ~workload_type:"moderate_heavy" ~num_elems:2_000_000 ~num_threads:1;
    Bench_skiplist.bench ~workload_type:"balanced_heavy" ~num_elems:2_000_000 ~num_threads:1;
    Bench_skiplist.bench ~workload_type:"write_heavy" ~num_elems:2_000_000 ~num_threads:1; *)
    (* Bench_compskiplist.bench ~workload_type:"read_heavy" ~num_elems:200_000 ~num_threads:12; *)
    (* Bench_compskiplist.bench ~workload_type:"moderate_heavy" ~num_elems:200_000 ~num_threads:12; *)
    (* Bench_compskiplist.bench ~workload_type:"balanced_heavy" ~num_elems:200_000 ~num_threads:12; *)
    (* Bench_compskiplist.bench ~workload_type:"write_heavy" ~num_elems:2_000 ~num_threads:2; *)
    Bench_locked_skiplist.bench ~workload_type:"read_heavy" ~num_elems:2_000_000 ~num_threads:12;
    Bench_locked_skiplist.bench ~workload_type:"moderate_heavy" ~num_elems:2_000_000 ~num_threads:12;
    Bench_locked_skiplist.bench ~workload_type:"balanced_heavy" ~num_elems:2_000_000 ~num_threads:12;
    Bench_locked_skiplist.bench ~workload_type:"write_heavy" ~num_elems:2_000_000 ~num_threads:12;
    Bench_coarse_skiplist.bench ~workload_type:"read_heavy" ~num_elems:2_000_000 ~num_threads:12;
    Bench_coarse_skiplist.bench ~workload_type:"moderate_heavy" ~num_elems:2_000_000 ~num_threads:12;
    Bench_coarse_skiplist.bench ~workload_type:"balanced_heavy" ~num_elems:2_000_000 ~num_threads:12;
    Bench_coarse_skiplist.bench ~workload_type:"write_heavy" ~num_elems:2_000_000 ~num_threads:12;
  ]

let () =
  let results =
    (* todo: should assert no stranded domains between tests. *)
    List.map (fun f -> f ()) benchmark_list
    |> List.map Benchmark_result.to_json
    |> String.concat ", "
  in
  let output =
    Printf.sprintf {| {"name": "lockfree", "results": [%s]}|} results
    (* Cannot use Yojson rewriters as of today none works on OCaml 5.1.0.
       This at least verifies that the manually crafted JSON is well-formed.

       If the type grow, we could switch to running ppx manually on 5.0.0 and
       pasting in its output. *)
    |> Yojson.Basic.prettify
  in
  Printf.printf "%s" output
