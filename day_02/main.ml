open Lib

type color = Blue | Red | Green

type block_count = BlockCount of int * color

type round = Round of block_count list

type id = Id of int

type game = Game of id * round list

let game_parser =
  let open Bark in
  succeed (fun game_id round_list -> Game (Id game_id, round_list))
  |. keyword (Token ("Game", Problem "Expecting int for game number"))
  |. chomp_while (Char.equal ' ')
  |= int (Problem "Expecting int for game number")

let block_count_parser =
  let open Bark in
  succeed (fun num color -> BlockCount (num, color))
  |= int (Problem "Expecting int for cube color")
  |. spaces
  |= one_of
       [ keyword (Token ("red", Problem "Expecting Red/Green/Blue")) |> map (fun _ -> Red)
       ; keyword (Token ("blue", Problem "Expecting Red/Green/Blue")) |> map (fun _ -> Blue)
       ; keyword (Token ("green", Problem "Expecting Red/Green/Blue")) |> map (fun _ -> Green) ]

let game_round_parser =
  let open Bark in
  loop [] (fun game_round ->
      block_count_parser
      |> and_then (fun block_count ->
             one_of
               [ succeed (Loop (block_count :: game_round))
                 |. token (Token (",", Problem "Expecting comma delimiter"))
                 |. chomp_while (Char.equal ' ')
               ; Done (Round (block_count :: game_round |> List.rev)) |> succeed ] ) )

let game_rounds_parser =
  let open Bark in
  loop [] (fun game_rounds ->
      game_round_parser
      |> and_then (fun game_round ->
             one_of
               [ succeed (Loop (game_round :: game_rounds))
                 |. token (Token (";", Problem "Expecting semicolon delimiter"))
                 |. chomp_while (Char.equal ' ')
               ; Done (game_round :: game_rounds) |> succeed ] ) )

let input_parser =
  let open Bark in
  loop [] (fun lines ->
      succeed (fun to_game game_rounds -> to_game game_rounds)
      |= game_parser
      |. token (Token (":", Problem "Expecting colon delimiter between game and rounds"))
      |. chomp_while (Char.equal ' ')
      |= game_rounds_parser
      |> and_then (fun line ->
             one_of
               [ succeed (Loop (line :: lines))
                 |. token (Token ("\n", Problem "Expecting newline delimiter"))
               ; Done (line :: lines) |> succeed ] ) )

let run_round (Round round) =
  let rec run_round ~red ~green ~blue round =
    if red < 0 || green < 0 || blue < 0 then false
    else
      match round with
      | [] -> true
      | BlockCount (n, Red) :: next -> run_round ~red:(red - n) ~green ~blue next
      | BlockCount (n, Green) :: next -> run_round ~red ~green:(green - n) ~blue next
      | BlockCount (n, Blue) :: next -> run_round ~red ~green ~blue:(blue - n) next
  in
  run_round ~red:12 ~green:13 ~blue:14 round

let play_game (Game (id, rounds)) =
  let rec play_game rounds =
    match rounds with
    | [] -> Some id
    | round :: next -> if run_round round == false then None else play_game next
  in
  play_game rounds

let solve_part_one parsed_input =
  let score_game game =
    match play_game game with
    | Some (Id v) -> v
    | None -> 0
  in
  parsed_input |> List.fold_left (fun acc game -> acc + score_game game) 0 |> Int.to_string

let run_round_pt2 (Round round) =
  let rec run_round_pt2 ~red ~blue ~green round =
    match round with
    | [] -> (red, blue, green)
    | BlockCount (n, Red) :: next -> run_round_pt2 ~red:(max red n) ~green ~blue next
    | BlockCount (n, Green) :: next -> run_round_pt2 ~red ~green:(max green n) ~blue next
    | BlockCount (n, Blue) :: next -> run_round_pt2 ~red ~green ~blue:(max blue n) next
  in
  run_round_pt2 ~red:0 ~green:0 ~blue:0 round

let play_game_pt2 (Game (_, rounds)) =
  let rec play_game (r, b, g) rounds =
    match rounds with
    | [] -> r * b * g
    | round :: next ->
        let r', b', g' = run_round_pt2 round in
        play_game (max r r', max b b', max g g') next
  in
  play_game (0, 0, 0) rounds

let solve_part_two parsed_input =
  parsed_input |> List.fold_left (fun acc game -> acc + play_game_pt2 game) 0 |> Int.to_string

let () =
  let part = get_part ()
  and input = Stdio.In_channel.read_all "day_02/input.txt" |> Bark.run input_parser in
  match (part, input) with
  | _, Error dead_ends -> dead_ends_to_string dead_ends |> print_endline
  | 1, Ok parsed_input -> print_endline ("Part 1: " ^ solve_part_one parsed_input)
  | 2, Ok parsed_input -> print_endline ("Part 2: " ^ solve_part_two parsed_input)
  | _ -> print_endline "Invalid part"
