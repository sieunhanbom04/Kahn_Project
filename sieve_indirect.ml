open Lwc_cont
open Main

let rec integers out i : (unit -> unit) -> unit =
  put_mvar out i >>= fun () -> integers out (i+1)

let rec output inp : (unit -> unit) -> unit =
  take_mvar inp >>= fun v ->
  if !print then (Printf.printf "%i \n" v; flush stdout);
  if v < !last then output inp else (stop (); halt())

let rec filter n inp out : (unit -> unit) -> unit =
  take_mvar inp >>= fun v ->
  (if v mod n <> 0 then put_mvar out v else skip) >>=
  fun () -> filter n inp out

let rec sift inp out : (unit -> unit) -> unit =
  take_mvar inp >>= fun v ->
  put_mvar out v >>= fun () ->
  let mid = make_mvar () in
  doco [filter v inp mid; sift mid out ]

let sieve : (unit -> unit) -> unit =
  let mi = make_mvar () in
  let mo = make_mvar () in
  doco [integers mi 2; sift mi mo; output mo]
  (*spawn (integers mi 2);
  spawn (sift mi mo);
  spawn (output mo);*)

let example : (unit -> unit) -> unit =
  return (make_mvar ()) >>= fun mi -> doco [integers mi 2; output mi]

let () = start example
