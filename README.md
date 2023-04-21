# Introduction 

This is a collection of all the benchmarks performed on different implementations of concurrent skiplists. This is part of my UGRC-I project.  

# Overview

The main directory contains all the different implementations used:
- `atomicskiplist.ml`
- `coarseskiplist.ml`
- `compskiplist.ml`
- `lockedSkiplist.ml`

All the `bench_*` files present in the main directory contain the benchmarks performed on each of the above implementations. They have a common function `bench` which has the signature:
```
val bench: workload_type:string ->num_elems:int -> num_threads:int -> unit -> Benchmark_result.t
```

The `main.ml` file runs all the benchmarks and compiles them into a `JSON` file. 

# Results

The results of the benchmarks can be seen in the `bench/` directory. 