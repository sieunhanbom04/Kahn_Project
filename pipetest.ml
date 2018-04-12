open Unix
open Printf

let output, input = Unix.pipe ()

let write_pid input =
  try
    let m = "(" ^ (string_of_int(Unix.getpid ())) ^ ")"
    in ignore (Unix.write input m 0 (String.length m));
      (*Unix.close input*)
  with
    Unix.Unix_error(n, f, arg) -> Printf.printf "%s(%s) : %s\n" f arg (Unix.error_message n);;

    match Unix.fork () with
    | 0 -> for i = 0 to 5 do
          match Unix.fork () with
          | 0 ->  write_pid input; exit 0
          | _ -> ()
          done;
          (*Unix.close input*)
    | _ -> (*Unix.write input "tung" 0 (String.length "tung");*) Unix.close input;
          let s = ref "" and buff = String.create 6
          in while true do
          match Unix.read output buff 0 6 with
          | 0 -> Printf.printf "My grandchildren are %s\n" !s; exit 0
          | n -> s := !s ^ (String.sub buff 0 n) ^ "."
          done
          (*Unix.write input "tung" 0 (String.length "tung")*);;

(*let () = write_pid input*)
