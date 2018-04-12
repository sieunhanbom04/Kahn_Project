module Eratosthene (K : Kahn.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib

  let filter (prime : int) (qi : int K.in_port) (qo : int K.out_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun n -> (if (n mod prime != 0)
                                then
                                  ((K.put n qo) >>= (fun () -> loop ()))
                                else
                                  loop ()))
    in loop ()

  let rec sift (qi : int K.in_port) (qo : int K.out_port) : unit K.process =
    (K.get qi) >>=
    (fun prime -> (K.put prime qo) >>=
    (fun () -> (delay K.new_channel ()) >>=
      (fun (q_in, q_out) -> K.doco [ filter prime qi q_out; sift q_in qo ; ])))

  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v -> Format.printf "%d@." v; loop ())
    in
    loop ()

  let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2

  let double_channel_init () =
    let t1 = K.new_channel () in
    let t2 = K.new_channel () in
    t1, t2

  let main : unit K.process =
  (delay double_channel_init ()) >>=
  (fun ((q_in1, q_out1), (q_in2, q_out2)) -> K.doco [ integers q_out1 ; sift q_in1 q_out2; output q_in2 ; ])

  let counter_test_positive x : unit K.process =
    let rec loop i : unit K.process =
      print_endline (string_of_int(i)); loop (i+1)
    in
    loop x

  let counter_test_negative x : unit K.process=
    let rec loop i : unit K.process =
      print_endline (string_of_int(i)); loop (i-1)
    in loop x

end

module E = Eratosthene(Kahn.Th)

let () = E.K.run (E.main)
