open Lib

let is_digit = Fun.flip List.mem ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

let is_symbol c = if (not (is_digit c)) && c <> '.' then Some c else None

let dirs ~y_origin ~x_origin =
  List.map
    (fun y_mod -> List.map (fun x_mod -> (y_origin + y_mod, x_origin + x_mod)) [-1; 0; 1])
    [-1; 0; 1]
  |> List.concat

type ctx = Ctx of (int * int) list * string

module CoordSet = Set.Make (struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    if Int.equal x1 x2 && Int.equal y1 y2 then 0
    else if not (Int.equal x1 x2) then compare x1 x2
    else compare y1 y2
end)

let is_part_adjacent ctx_list (gear_row, gear_col) =
  let gear_set = CoordSet.of_list (dirs ~y_origin:gear_row ~x_origin:gear_col) in
  let rec is_part_adjacent ctx_list matches =
    match ctx_list with
    | [] -> matches
    | Ctx (coords, digit_string) :: next_ctx_list -> (
        let value = int_of_string digit_string in
        let has_match =
          if CoordSet.of_list coords |> CoordSet.inter gear_set |> CoordSet.is_empty |> not then
            Some value
          else None
        in
        match has_match with
        | Some m -> is_part_adjacent next_ctx_list (m :: matches)
        | None -> is_part_adjacent next_ctx_list matches )
  in
  is_part_adjacent ctx_list []
  |> fun res ->
  match res with
  | [x; y] -> Some (x * y)
  | _ -> None

let is_adjacent_to ~comparator all_lines (Ctx (row_col_list, digit_str)) =
  let rec is_adjacent_to row_col_list =
    match row_col_list with
    | (row, col) :: next -> (
      match
        dirs ~y_origin:row ~x_origin:col
        |> List.find_opt (fun (row, col) ->
               all_lines |> get_opt row
               |> opt_and_then (get_opt col)
               |> opt_and_then comparator
               |> Option.map (fun _ -> true)
               |> Option.value ~default:false )
        |> Option.map (fun _ -> int_of_string digit_str)
      with
      | None -> is_adjacent_to next
      | some -> some )
    | [] -> None
  in
  is_adjacent_to row_col_list

let is_symbol_adjacent = is_adjacent_to ~comparator:is_symbol

let rec process_line all_lines chars ~line_idx ~char_idx ~values ~ctx =
  let add_to_ctx char =
    match ctx with
    | Ctx (coords, char_list) -> Ctx ((line_idx, char_idx) :: coords, str_snoc char char_list)
  in
  match chars with
  | [] -> values
  | char :: next_chars -> (
      let next_char_is_digit =
        match next_chars with
        | next_char :: _ -> if is_digit next_char then Some next_char else None
        | _ -> None
      in
      match (is_digit char, next_char_is_digit) with
      (* if we are on a digit but the next is not a digit, add it to values and reset context *)
      | true, None ->
          process_line all_lines ~line_idx next_chars ~char_idx:(char_idx + 1)
            ~ctx:(Ctx ([], ""))
            ~values:(List.cons (add_to_ctx char) values)
      (* if we are on a digit but there's a digit next, add it to context and keep reading *)
      | true, Some _ ->
          process_line all_lines next_chars ~line_idx ~char_idx:(char_idx + 1)
            ~ctx:(add_to_ctx char) ~values
      (* if we aren't on a digit keep going *)
      | false, _ ->
          process_line all_lines next_chars ~line_idx ~char_idx:(char_idx + 1) ~ctx ~values )

let process_input_pt1 all_lines =
  let rec process_input_pt1 lines ~line_idx ~values =
    match lines with
    | [] ->
        values
        |> List.map (is_symbol_adjacent all_lines)
        |> List.fold_left
             (fun acc cur -> Option.map (Fun.flip List.cons acc) cur |> Option.value ~default:acc)
             []
        |> List.fold_left ( + ) 0
        |> fun total -> (total, values)
    | chars :: next_lines ->
        let new_values =
          process_line all_lines chars ~line_idx ~char_idx:0 ~ctx:(Ctx ([], "")) ~values:[]
        in
        process_input_pt1 next_lines ~line_idx:(line_idx + 1)
          ~values:(List.append new_values values)
  in
  process_input_pt1 (all_lines |> Array.map Array.to_list |> Array.to_list) ~line_idx:0 ~values:[]

let process_input_pt2 all_lines ctx_list =
  let rec process_input_pt2 lines ~line_idx ~all_values =
    match lines with
    | [] -> all_values
    | line :: next_lines ->
        let rec process_line chars ~char_idx ~values =
          match chars with
          | [] -> values
          | char :: next_chars -> (
            match char with
            | '*' -> (
              match is_part_adjacent ctx_list (line_idx, char_idx) with
              | Some v ->
                  process_line next_chars ~char_idx:(char_idx + 1) ~values:(List.cons v values)
              | None -> process_line next_chars ~char_idx:(char_idx + 1) ~values )
            | _ -> process_line next_chars ~char_idx:(char_idx + 1) ~values )
        in
        let values = process_line line ~char_idx:0 ~values:[] in
        process_input_pt2 next_lines ~line_idx:(line_idx + 1)
          ~all_values:(List.append values all_values)
  in
  process_input_pt2 all_lines ~line_idx:0 ~all_values:[] |> List.fold_left ( + ) 0

let () =
  let part = get_part () in
  let input =
    Stdio.In_channel.read_all "day_03/input.txt"
    |> String.split_on_char '\n' |> List.to_seq
    |> Seq.map (fun line -> String.to_seq line |> Array.of_seq)
    |> Array.of_seq
  in
  let pt_1_total, ctx_list = process_input_pt1 input in
  match part with
  | 1 -> print_endline ("Part one: " ^ Int.to_string pt_1_total)
  | 2 ->
      let input = input |> Array.to_list |> List.map Array.to_list in
      print_endline ("Part two: " ^ Int.to_string (process_input_pt2 input ctx_list))
  | _ -> print_endline "Unknown Part"
