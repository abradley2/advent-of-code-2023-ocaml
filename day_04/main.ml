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

let process_card (Card (_, win_nums)) my_nums =
  let win_set = IntSet.of_list win_nums and my_set = IntSet.of_list my_nums in
  let intersect = IntSet.inter win_set my_set |> IntSet.to_list |> List.length in
  if intersect = 0 then 0 else pow 2 (intersect - 1)

let process_input_pt_1 parsed_input =
  let rec process_input_pt_1 parsed_input ~results =
    match parsed_input with
    | [] -> results
    | (card, hand) :: next -> process_input_pt_1 next ~results:(process_card card hand :: results)
  in
  process_input_pt_1 parsed_input ~results:[] |> List.fold_left ( + ) 0

let () =
  let input = Stdio.In_channel.read_all "day_04/input.txt" and part = get_part () in
  match (part, Bark.run input_parser input) with
  | 1, Ok parsed_input -> Printf.printf "Part One: %d\n" (process_input_pt_1 parsed_input)
  | 2, Ok _ -> print_endline "Part 2"
  | _, Ok _ -> print_endline "Invalid part"
  | _, Error dead_ends -> print_endline (dead_ends_to_string dead_ends)
