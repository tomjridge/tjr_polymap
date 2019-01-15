(* Polymorphic map ops courtesy of extlib *)

(* following needs polymorphic map - 
   batteries? no, only Pervasives.compare
   extlib? yes, allows parameterization by compare 

FIXME can also use first class modules to produce polymorphic ops
*)

module Private = struct
  type ('k,'v) t = (*ExtLib.*) ('k,'v)PMap.t
  let mem : 'a -> ('a, 'b) t -> bool = PMap.mem
  let map: ('v -> 'u) -> ('k,'v) t -> ('k,'u) t = PMap.map
  (* FIXME bindings and cardinal should be in ExtLib; also others *)
  let bindings: ('k,'v) t -> ('k * 'v) list = (fun m -> 
      let bs = ref [] in
      PMap.iter (fun k v -> bs:=(k,v)::!bs) m;
      List.rev !bs)
  let empty: ('k -> 'k -> int) -> ('k,'v) t = PMap.create

  (* FIXME this is inefficient *)
  let cardinal: ('k,'v) t -> int = (fun m ->
      let x = ref 0 in
      PMap.iter (fun _k _v -> x:=!x+1) m;
      !x)
  let is_empty: ('k,'v) t -> bool = fun t -> cardinal t = 0
  let iter: ('k -> 'v -> unit) -> ('k,'v) t -> unit = PMap.iter
  let find: 'k -> ('k,'v) t -> 'v = PMap.find
  let find_opt: 'k -> ('k,'v) t -> 'v option = fun k t -> 
    try 
      Some(find k t)
    with Not_found -> None
  let remove: 'k -> ('k,'v) t -> ('k,'v) t = PMap.remove
  let add: 'k -> 'v -> ('k,'v) t -> ('k,'v) t = PMap.add
  let foldi: ('a -> 'b -> 'c -> 'c) -> ('a , 'b) t -> 'c -> 'c = PMap.foldi
end


(* maintain the cardinality *)
type ('k,'v) t = (*ExtLib.*) { card:int; map: ('k,'v)PMap.t }

let lift: (('k,'v) PMap.t -> 'a) -> ('k,'v) t -> 'a = fun f m -> f m.map
  
let mem : 'a -> ('a, 'b) t -> bool = fun x -> lift (PMap.mem x)
let map: ('v -> 'u) -> ('k,'v) t -> ('k,'u) t = fun f t -> { t with map=PMap.map f t.map }
(* FIXME bindings and cardinal should be in ExtLib; also others *)
let bindings: ('k,'v) t -> ('k * 'v) list = (fun m -> 
    let bs = ref [] in
    PMap.iter (fun k v -> bs:=(k,v)::!bs) m.map;
    List.rev !bs)
let empty: ('k -> 'k -> int) -> ('k,'v) t = fun comp -> { card=0; map=PMap.create comp }

(* FIXME this is inefficient *)
let cardinal: ('k,'v) t -> int = fun t -> t.card
let is_empty: ('k,'v) t -> bool = fun t -> t.card = 0
let iter: ('k -> 'v -> unit) -> ('k,'v) t -> unit = fun f -> lift (PMap.iter f)
let find: 'k -> ('k,'v) t -> 'v = fun k -> lift (PMap.find k)
let find_opt: 'k -> ('k,'v) t -> 'v option = fun k t -> 
  try 
    Some(find k t)
  with Not_found -> None
let remove: 'k -> ('k,'v) t -> ('k,'v) t = fun k t -> 
  (* this assumes that k is present *)
  let card = if mem k t then t.card -1 else t.card in
  { card; map=PMap.remove k t.map }
let add: 'k -> 'v -> ('k,'v) t -> ('k,'v) t = fun k v t -> 
  let card = if mem k t then t.card else t.card+1 in
  { card; map=PMap.add k v t.map }
let foldi: ('a -> 'b -> 'c -> 'c) -> ('a , 'b) t -> 'c -> 'c = 
  fun f t c ->
    PMap.foldi f t.map c

let empty_int_map () = empty (Pervasives.compare : int -> int -> int)

let empty_string_map () = empty (Pervasives.compare : string -> string -> int)

(** NOTE later bindings take precedence *)
let from_bindings ~compare kvs =
  empty compare |> fun map0 ->
  List.fold_left
    (fun a (k,v) -> add k v a)
    map0
    kvs


(** NOTE m2 takes precedence *)
let union m1 m2 = 
  foldi (fun k v m -> add k v m) m2 m1
