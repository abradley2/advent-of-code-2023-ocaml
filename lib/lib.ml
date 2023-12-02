let get_arg_string () = Sys.argv |> Array.to_list |> String.concat " "

exception MissingArg of string

let get_part () =
  let part = ref None in
  Arg.parse
    [("--part", Arg.Int (fun v -> part := Some v), "part to run")]
    (fun _ -> ())
    (get_arg_string ()) ;
  match !part with
  | Some v -> v
  | None -> raise (MissingArg "--part")

type problem = Problem of string

let dead_ends_to_string dead_ends =
  let open Bark in
  let rec loop dead_ends lines =
    match dead_ends with
    | [] -> lines |> List.rev |> String.concat "\nor "
    | x :: xs ->
        let problem =
          match x.problem with
          | Problem v -> v
        in
        loop xs (Printf.sprintf "Expecting: %s, at col: %d, row: %d" problem x.col x.row :: lines)
  in
  loop dead_ends []

let repeat times el =
  let rec repeat elist =
    if List.length elist |> Int.equal (abs times) then elist else repeat (el :: elist)
  in
  repeat []

let from_opt default_val opt =
  match opt with
  | Some v -> v
  | None -> default_val
