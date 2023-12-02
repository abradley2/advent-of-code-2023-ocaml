open Lib

type turn = L | R

module Facing = struct
  type t = North | East | South | West

  let dirs = [North; East; South; West]

  let right dir =
    List.find_index (( = ) dir) dirs
    |> Option.map (fun idx -> List.nth_opt dirs (idx + 1))
    |> Option.join |> Option.value ~default:North

  let left dir =
    List.find_index (( = ) dir) dirs
    |> Option.map (fun idx -> if idx <= 0 then Some West else List.nth_opt dirs (idx - 1))
    |> Option.join |> Option.value ~default:West
end

let turn_parser =
  let open Bark in
  succeed (fun d v -> (Some d, v))
  |= one_of
       [ token (Token ("L", Problem "Left")) |> map (Fun.const L)
       ; token (Token ("R", Problem "Right")) |> map (Fun.const R) ]
  |= Bark.int (Problem "Velocity Int")

let input_parser =
  let open Bark in
  loop [] (fun state ->
      one_of
        [ succeed (fun n -> Loop (n :: state))
          |= turn_parser
          |. chomp_while (fun c -> Char.equal c ' ' || Char.equal c ',')
        ; endd (Problem "End") |> map (fun _ -> Done (List.rev state)) ] )

module Visited = Set.Make (struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    if x1 = x2 && y1 = y2 then 0 else if compare x1 x2 <> 0 then compare x1 x2 else compare y1 y2
end)

exception InvalidInput of string

let unfold_instructions instruction_list =
  let rec unfold_instructions instruction_list unfolded =
    match instruction_list with
    | [] -> unfolded
    | (Some t, v) :: next -> (
      match t with
      | L -> unfold_instructions next (List.append unfolded ((Some L, 0) :: repeat v (None, 1)))
      | R -> unfold_instructions next (List.append unfolded ((Some R, 0) :: repeat v (None, 1))) )
    | _ -> raise (InvalidInput "All instructions must have a turn")
  in
  unfold_instructions instruction_list []

let solve instruction_list ~return_on_repeat =
  let rec run_instruction instruction_list facing ~x ~y prev_visited =
    match instruction_list with
    | [] -> (x, y)
    | (turn_opt, v) :: next -> (
        let visited = if v <> 0 then Visited.add (x, y) prev_visited else prev_visited in
        if Visited.mem (x, y) prev_visited && return_on_repeat then (x, y)
        else
          let next_facing =
            match turn_opt with
            | Some L -> Facing.left facing
            | Some R -> Facing.right facing
            | None -> facing
          in
          match next_facing with
          | Facing.North -> run_instruction next next_facing ~x ~y:(y - v) visited
          | Facing.East -> run_instruction next next_facing ~x:(x + v) ~y visited
          | Facing.South -> run_instruction next next_facing ~x ~y:(y + v) visited
          | Facing.West -> run_instruction next next_facing ~x:(x - v) ~y visited )
  in
  let x, y = run_instruction instruction_list Facing.North ~x:0 ~y:0 Visited.empty in
  abs x + abs y

let () =
  let input = Stdio.In_channel.read_all "./day_00/input.txt" in
  Bark.run input_parser input
  |> Result.map (fun instructions_list ->
         match get_part () with
         | 1 ->
             unfold_instructions instructions_list
             |> solve ~return_on_repeat:false |> Int.to_string |> Stdio.print_endline
         | 2 ->
             unfold_instructions instructions_list
             |> solve ~return_on_repeat:true |> Int.to_string |> Stdio.print_endline
         | _ -> Stdio.print_endline "Invalid part number" )
  |> Result.map_error (fun e -> dead_ends_to_string e |> Stdio.print_endline)
  |> Result.fold ~ok:Fun.id ~error:Fun.id
