type 'a t = ('a -> unit) -> unit

let runq = Queue.create ()
let enqueue t  = Queue.push t runq
let dequeue () = Queue.take runq

let return a : 'a t = fun k -> k a

let skip       = return ()
let yield () k = enqueue k
let halt  ()   = return ()

let (>>=) (t:'a t) (k2:'a -> 'b t) : 'b t = fun k -> t (fun r -> k2 r k)

let spawn (t:unit -> unit t) = enqueue (fun () -> t () (fun () -> ()))

exception Stop
let stop () = raise Stop

let start (f : unit t) : unit  =
  print_endline "start";
  enqueue (fun () -> f (fun () -> ()));
  try
    while true do
      print_endline "shit";
      dequeue () ()
    done
  with Queue.Empty | Stop -> ()

type 'a mvar = { mutable v:'a option;
		 mutable read: ('a -> unit) option;
		 mutable write: ((unit -> unit) * 'a) option }

let make_mvar () = { v=None; read=None; write=None }

let doco (l : (unit t) list) : (unit t) =
  (fun x -> print_endline "LOL"; List.iter (fun f -> enqueue (fun () -> (f (fun () -> ())))) l)

let put_mvar out v k =
  match out with
  | { v=Some v'; read=_; write=None } -> print_endline "dkm_put1"; out.write <- Some (k,v)

  | { v=None; read=Some r; write=None } -> print_endline "dkm_put2";
        out.read <- None; enqueue (fun () -> r v); k ()

  | { v=None; read=None; write=None } -> print_endline "dkm_put3";
                                          out.v <- Some v; k ()

  | _ -> failwith "failed put_mvar"

let take_mvar inp k =
  match inp with
  | { v=Some v; read=None; write=None } -> print_endline "dkm_get1";
                                            inp.v <- None; k v

  | { v=Some v; read=None; write=Some(c,v') } -> print_endline "dkm_get2";
      inp.v <- Some v'; inp.write <- None; enqueue c; k v

  | { v=None; read=None; write=_ } -> print_endline "dkm_get3";
                                        inp.read <- Some(k)

  | _ -> failwith "failed take_mvar"


type 'a fifo = { q : 'a Queue.t; mutable w: ('a -> unit) option }
let make_fifo () = { q=Queue.create (); w=None }

let take_fifo f k =
  if Queue.length f.q = 0 then
    f.w <- Some k
  else
    k (Queue.take f.q)

let put_fifo f v =
  Queue.add v f.q;
  match f.w with
  | Some k -> enqueue (fun () -> k (Queue.take f.q)); f.w <- None
  | None -> ()
