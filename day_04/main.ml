open Lib

type card = Card of (int * int list)

let nums_parser =
  let open Bark in
  loop [] (fun nums ->
      one_of
        [ succeed (fun num -> Loop (num :: nums))
          |. chomp_while (Char.equal ' ')
          |= int (Problem "Expecting int value")
          |. chomp_while (Char.equal ' ')
        ; succeed (Done (List.rev nums)) ] )

let card_parser =
  let open Bark in
  succeed (fun card_id winnums -> Card (card_id, winnums))
  |. keyword (Token ("Card", Problem "Expecting Card keyword"))
  |. chomp_while (Char.equal ' ')
  |= int (Problem "Expecting Card Id int")
  |. token (Token (":", Problem "Expecting colon delimiter after Card Id"))
  |. chomp_while (Char.equal ' ')
  |= nums_parser

let line_parser =
  let open Bark in
  succeed (fun card hand -> (card, hand))
  |= card_parser
  |. chomp_while (Char.equal ' ')
  |. token (Token ("|", Problem "Expecting pipe delimiter between card and hand"))
  |. chomp_while (Char.equal ' ')
  |= nums_parser

let input_parser =
  let open Bark in
  loop [] (fun lines ->
      one_of
        [ succeed (fun line -> Loop (line :: lines))
          |= line_parser
          |. chomp_while (fun c -> c == '\n')
        ; succeed (Done (List.rev lines)) ] )

module IntSet = Set.Make (Int)

type card_result = {match_count: int; score: int}

let process_card (Card (_, win_nums)) my_nums =
  let win_set = IntSet.of_list win_nums and my_set = IntSet.of_list my_nums in
  let match_count = IntSet.inter win_set my_set |> IntSet.to_list |> List.length in
  {match_count; score= (if match_count = 0 then 0 else pow 2 (match_count - 1))}

let process_input_pt_1 parsed_input =
  let rec process_input_pt_1 parsed_input ~results =
    match parsed_input with
    | [] -> List.rev results
    | (card, hand) :: next -> process_input_pt_1 next ~results:(process_card card hand :: results)
  in
  process_input_pt_1 parsed_input ~results:[]

module SuperSet = struct
  include Set.Make (Int)
  include List
end

let rec process_card_result ~idx ~indexed_card_results card_result =
  let my_result = card_result.match_count in
  let next = Array.sub indexed_card_results (idx + 1) card_result.match_count in
  let next_result =
    next
    |> Array.mapi (fun i -> process_card_result ~idx:(idx + i + 1) ~indexed_card_results)
    |> Array.fold_left ( + ) 0
  in
  my_result + next_result

let process_input_pt_2 (card_results : card_result list) =
  let indexed_card_results = Array.of_list card_results in
  let rec process_input_pt_2 card_results ~idx ~count =
    match card_results with
    | [] -> count
    | card_result :: next ->
        process_input_pt_2 next ~idx:(idx + 1)
          ~count:(count + process_card_result ~indexed_card_results card_result ~idx)
  in
  process_input_pt_2 card_results ~idx:0 ~count:(List.length card_results)

let () =
  let input = Stdio.In_channel.read_all "day_04/input.txt" and part = get_part () in
  match (part, Bark.run input_parser input) with
  | 1, Ok parsed_input ->
      let results = process_input_pt_1 parsed_input in
      let count = results |> List.map (fun v -> v.score) |> List.fold_left ( + ) 0 in
      Printf.printf "Part One: %d\n" count
  | 2, Ok parsed_input ->
      let card_results = process_input_pt_1 parsed_input in
      let count = process_input_pt_2 card_results in
      Printf.printf "Part One: %d\n" count
  | _, Ok _ -> print_endline "Invalid part"
  | _, Error dead_ends -> print_endline (dead_ends_to_string dead_ends)
