open Base
open Lib

type instruction = Forward of int | Left of int | Right of int

module Facing = struct
  type t = North | West | East | South
end

let turn_parser =
  let open Bark in
  succeed (fun d v -> d v)
  |= one_of
       [ token (Token ("L", Problem "Left")) |> map (fun _ v -> Left v)
       ; token (Token ("R", Problem "Right")) |> map (fun _ v -> Right v) ]
  |= Bark.int (Problem "Velocity Int")

let input_parser =
  let open Bark in
  loop [] (fun state ->
      one_of
        [ succeed (fun n -> Loop (n :: state))
          |= turn_parser
          |. chomp_while (fun c -> Char.is_whitespace c || Char.equal c ',')
        ; endd (Problem "End") |> map (fun _ -> Done (List.rev state)) ] )

module Visited = Stdlib.Set.Make (struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    if x1 = x2 && y1 = y2 then 0 else if compare x1 x2 <> 0 then compare x1 x2 else compare y1 y2
end)

let unfold_instructions instruction_list =
  let rec unfold_instructions instruction_list unfolded =
    match instruction_list with
    | [] -> unfolded
    | instruction :: next -> (
      match instruction with
      | Left v ->
          unfold_instructions next (List.append unfolded (Left 1 :: repeat (v - 1) (Forward 1)))
      | Right v ->
          unfold_instructions next (List.append unfolded (Right 1 :: repeat (v - 1) (Forward 1)))
      | Forward v -> unfold_instructions next (List.append unfolded (repeat v (Forward 1))) )
  in
  unfold_instructions instruction_list []

let solve instruction_list ~return_on_repeat =
  let rec run_instruction instruction_list facing ~x ~y prev_visited =
    let visited = Visited.add (x, y) prev_visited in
    if Visited.mem (x, y) prev_visited && return_on_repeat then (x, y)
    else
      match instruction_list with
      | [] -> (x, y)
      | instruction :: next -> (
        match (instruction, facing) with
        | Forward v, Facing.North -> run_instruction next facing ~x ~y:(y - v) visited
        | Forward v, Facing.East -> run_instruction next facing ~x:(x + v) ~y visited
        | Forward v, Facing.South -> run_instruction next facing ~x ~y:(y + v) visited
        | Forward v, Facing.West -> run_instruction next facing ~x:(x - v) ~y visited
        | Left v, Facing.North -> run_instruction next Facing.West ~x:(x - v) ~y visited
        | Left v, Facing.East -> run_instruction next Facing.North ~x ~y:(y - v) visited
        | Left v, Facing.South -> run_instruction next Facing.East ~x:(x + v) ~y visited
        | Left v, Facing.West -> run_instruction next Facing.South ~x ~y:(y + v) visited
        | Right v, Facing.North -> run_instruction next Facing.East ~x:(x + v) ~y visited
        | Right v, Facing.East -> run_instruction next Facing.South ~x ~y:(y + v) visited
        | Right v, Facing.South -> run_instruction next Facing.West ~x:(x - v) ~y visited
        | Right v, Facing.West -> run_instruction next Facing.North ~x ~y:(y - v) visited )
  in
  let x, y = run_instruction instruction_list Facing.North ~x:0 ~y:0 Visited.empty in
  abs x + abs y

let () =
  let input = Stdio.In_channel.read_all "./day_00/input.txt" in
  Bark.run input_parser input
  |> Result.map ~f:(fun instructions_list ->
         match get_part () with
         | 1 ->
             solve instructions_list ~return_on_repeat:false |> Int.to_string |> Stdio.print_endline
         | 2 ->
             unfold_instructions instructions_list
             |> solve ~return_on_repeat:true |> Int.to_string |> Stdio.print_endline
         | _ -> Stdio.print_endline "Invalid part number" )
  |> Result.map_error ~f:(fun e -> dead_ends_to_string e |> Stdio.print_endline)
  |> Stdlib.Result.fold ~ok:Fn.id ~error:Fn.id
