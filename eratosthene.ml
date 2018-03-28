(*
let input_int = input_binary_int

let output_int = output_binary_int

let generate k output =
  let rec gen m =
    output_int output m;
    if m < k then gen (m+1)
    else ()
  in
  gen 2

let print_prime n = print_int n; print_newline ()

let read_first_primes input count =
  let rec read_primes first_primes count =
    if count <= 0 then first_primes
    else
    let n = input_int input in
    if List.exists (fun m -> n mod m = 0) first_primes then
      read_primes first_primes count
    else begin
      print_prime n;
      read_primes (n :: first_primes) (count - 1)
    end
  in
  read_primes [] count

let rec filter input =
  try
    let first_primes = read_first_primes input 1000 in
    let (fd_in, fd_out) = Unix.pipe () in
    match Unix.fork () with
    | 0 -> close fd_out;
      filter (Unix.in_channel_of_descr fd_in)
    | p -> close fd_in;
      let output = (Unix.out_channel_of_descr fd_out) in
      while true do
        let n = input_int input in
        if List.exists (fun m -> n mod m = 0) first_primes then ()
        else output_int output n
      done
  with End_of_file -> ();;




let sieves () =
  let len = try int_of_string Sys.argv.(1) with _ -> max_int in
  let (fd_in, fd_out) = pipe () in
  match fork () with
  | 0 -> close fd_out;
         filter (Unix.in_channel_of_descr fd_in)
  | p -> close fd_in;
         generate len (Unix.out_channel_of_descr fd_out);;

handle_unix_error sieves ();;
*)

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
end

module E = Eratosthene(Kahn.Th)

let () = E.K.run E.main
