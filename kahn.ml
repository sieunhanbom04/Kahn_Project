open Unix

module type S = sig
  type 'a process
  type 'a in_port
  type 'a out_port

  val new_channel: unit -> 'a in_port * 'a out_port
  val put: 'a -> 'a out_port -> unit process
  val get: 'a in_port -> 'a process

  val doco: unit process list -> unit process

  val return: 'a -> 'a process
  val bind: 'a process -> ('a -> 'b process) -> 'b process

  val run: 'a process -> 'a
end

module Lib (K : S) = struct

  let ( >>= ) x f = K.bind x f

  let delay f x =
    K.bind (K.return ()) (fun () -> K.return (f x))

  let par_map f l =
    let rec build_workers l (ports, workers) =
      match l with
      | [] -> (ports, workers)
      | x :: l ->
          let qi, qo = K.new_channel () in
          build_workers
            l
            (qi :: ports,
             ((delay f x) >>= (fun v -> K.put v qo)) :: workers)
    in
    let ports, workers = build_workers l ([], []) in
    let rec collect l acc qo =
      match l with
      | [] -> K.put acc qo
      | qi :: l -> (K.get qi) >>= (fun v -> collect l (v :: acc) qo)
    in
    let qi, qo = K.new_channel () in
    K.run
      ((K.doco ((collect ports [] qo) :: workers)) >>= (fun _ -> K.get qi))

end

(*Implementation with processes communicating through network*)

module Net: S = struct
  type 'a process = (unit -> 'a)

  type 'a in_port = in_channel
  type 'a out_port = out_channel

  let port = ref 10000

  let create_addr () = port := (!port + 1) mod 10000 + 10000;
                      ADDR_INET(inet_addr_any, !port)

  let rec try_to_connect s addr =
    try
      connect s addr;
    with _ -> try_to_connect s addr

  let new_channel () =
    let addr = create_addr () in
    let s1 = socket PF_INET SOCK_STREAM 0 in
    let s2 = socket PF_INET SOCK_STREAM 0 in
    Unix.setsockopt s2 SO_REUSEADDR true;
    let (fd_in, fd_out) = Unix.pipe () in
    let (fd_in1, fd_out1) = Unix.pipe () in
    let input_channel = Unix.in_channel_of_descr fd_in in
    let output_channel = Unix.out_channel_of_descr fd_out1 in
    match fork () with
    | 0 -> try_to_connect s1 addr;
           exit 0;
           input_channel, output_channel

    | pid_child -> bind s2 addr; listen s2 20;
           let fd_client, addr_client = accept s2 in
           let ch_in = Unix.in_channel_of_descr fd_client in
           let ch1 = Unix.out_channel_of_descr fd_out in
           let ch_out = Unix.out_channel_of_descr s1 in
           ch_in, ch_out

    (*let new_channel () =
      let (fd_in, fd_out) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      let input_channel = Unix.in_channel_of_descr fd_in in
      let output_channel = Unix.out_channel_of_descr fd_out in
      input_channel, output_channel*)

  (*let new_channel () =
    let addr = create_addr () in
    let s1 = socket PF_INET SOCK_STREAM 0 in
    let s2 = socket PF_INET SOCK_STREAM 0 in
    let (fd_in, fd_out) = Unix.pipe () in
    let input_channel = Unix.in_channel_of_descr fd_in in
    let output_channel = Unix.out_channel_of_descr fd_out in
    match fork () with
    | 0 ->
        try_to_connect s1 addr;
            exit 0 ;
          print_endline "child hasn't exited";
             input_channel, output_channel

    | pid_child -> bind s2 addr; listen s2 20;
           let fd_client, addr_client = accept s2 in
           let ch_in = Unix.in_channel_of_descr fd_client in
           let ch_out = Unix.out_channel_of_descr s1 in
           ch_in, ch_out*)

  let put value out_channel () =
    Marshal.to_channel out_channel value [Marshal.Compat_32];
    flush out_channel

  let rec get in_channel () =
    try
      let value = Marshal.from_channel in_channel
      in
      value
    with End_of_file -> ();
    get in_channel ()

  let run e = e ()

  let doco process_list () =
    let rec process_handle l =
      match l with
      | [] -> ()
      | processA :: [] -> run processA
      | processA :: rest -> ( match fork () with
                              | 0 -> run processA
                              | _ -> process_handle rest
                            )
    in process_handle process_list

  let return v = (fun () -> v)

  let bind e e' () =
    let v = e () in
    e' v ()

end

(*Implementation with processes communicating through pipes*)

module Pr: S = struct
  type 'a process = (unit -> 'a)

  type 'a in_port = in_channel
  type 'a out_port = out_channel

  let new_channel () = let (fd_in, fd_out) = Unix.pipe () in
    let in_channel = Unix.in_channel_of_descr fd_in in
    let out_channel = Unix.out_channel_of_descr fd_out in
    in_channel, out_channel

  let put value out_channel () =
    Marshal.to_channel out_channel value [Marshal.Compat_32];
    flush out_channel

  let rec get in_channel () =
    try
      let value = Marshal.from_channel in_channel
      in
      value
    with End_of_file -> ();
    get in_channel ()

  let run e = e ()

  let doco process_list () =
    let rec process_handle l =
      match l with
      | [] -> ()
      | processA :: [] -> run processA
      | processA :: rest -> ( match fork () with
                              | 0 -> run processA
                              | _ -> process_handle rest
                            )
    in process_handle process_list

  let return v = (fun () -> v)

  let bind e e' () =
    let v = e () in
    e' v ()

end

(*Implementation with sequential simulation*)

module Seq: S = struct
  type 'a process = ('a -> unit) -> unit

  (*type 'a channel = { q : 'a Queue.t; mutable ('a -> unit) option }*)
  type 'a channel = { mutable v:'a option;
  		                mutable read: ('a -> unit) option;
  		                mutable write: ((unit -> unit) * 'a) option }
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let queue_Process = Queue.create ()

  let enqueue p = Queue.push p queue_Process

  let dequeue () = Queue.pop queue_Process

  exception Stop

  let run (f : 'a process) : 'a  =
    print_endline "start";
    enqueue (fun () -> f (fun x -> ()));
    (*try*)
    while true do
      (*print_endline "shit";*)
      dequeue () ()
    done;
    raise Stop


  let doco (l : (unit process) list) : (unit process) =
    (fun x -> (*print_endline "LOL";*) List.iter (fun f -> enqueue (fun () -> (f (fun () -> ())))) l)

  let new_channel () =
    let q = { v=None; read=None; write=None } in
    q, q

  let put v out k =
    match out with
    | { v=Some v'; read=_; write=None } -> out.write <- Some (k,v)

    | { v=None; read=Some r; write=None } ->
          out.read <- None; enqueue (fun () -> r v); k ()

    | { v=None; read=None; write=None } -> out.v <- Some v; k ()

    | _ -> failwith "failed put_mvar"

  let get inp k =
    match inp with
    | { v=Some v; read=None; write=None } -> inp.v <- None; k v

    | { v=Some v; read=None; write=Some(c,v') } ->
        inp.v <- Some v'; inp.write <- None; enqueue c; k v

    | { v=None; read=None; write=_ } -> inp.read <- Some(k)

    | _ -> failwith "failed take_mvar"


  let return v = fun k -> k v

  let bind e e' = fun k -> e (fun v -> e' v k)
end

(*Original implementation by Thread*)

module Th: S = struct
  type 'a process = (unit -> 'a)

  type 'a channel = { q: 'a Queue.t ; m: Mutex.t; }
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let new_channel () =
    let q = { q = Queue.create (); m = Mutex.create (); } in
    q, q

  let put v c () =
    Mutex.lock c.m;
    Queue.push v c.q;
    Mutex.unlock c.m;
    Thread.yield ()

  let rec get c () =
    try
      Mutex.lock c.m;
      let v = Queue.pop c.q in
      Mutex.unlock c.m;
      v
    with Queue.Empty ->
      Mutex.unlock c.m;
      Thread.yield ();
      get c ()

  let doco l () =
    let ths = List.map (fun f -> Thread.create f ()) l in
    List.iter (fun th -> Thread.join th) ths

  let return v = (fun () -> v)

  let bind e e' () =
    let v = e () in
    Thread.yield ();
    e' v ()

  let run e = e ()
end
