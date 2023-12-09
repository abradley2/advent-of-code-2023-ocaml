open Lib

module Range = struct
  type dest = Dest of int

  type src = Src of int

  type len = Len of int

  type t = dest * src * len

  let in_src_range sut ((Dest dest_val, Src src_val, Len len) : t) =
    if sut >= src_val && sut < src_val + len then Some (Base.List.range dest_val len) else None

  let make dest src len : t = (dest, src, len)
end

module String_map = Map.Make (String)

module Range_map = struct
  type src_key = Src_key of string

  type dst_key = Dst_key of string

  module Src_map = Map.Make (struct
    type t = src_key

    let compare (Src_key a) (Src_key b) = compare a b
  end)

  type t = (dst_key * Range.t list) Src_map.t
end

module Pair_set = Set.Make (struct
  type t = string * int

  let compare (src_key1, src_val1) (src_key2, src_val2) =
    compare (src_key1 ^ string_of_int src_val1) (src_key2 ^ string_of_int src_val2)
end)

module Int_set = Set.Make (Int)
module String_set = Set.Make (String)
module String_map = Map.Make (String)

let seeds_parser =
  let open Bark in
  succeed (fun v -> v)
  |. token (Token ("seeds:", Problem "Expecting title for seed list"))
  |. chomp_while (Char.equal ' ')
  |= loop [] (fun seeds ->
         one_of
           [ succeed (fun seed -> Loop (seed :: seeds))
             |. chomp_while (Char.equal ' ')
             |= int (Problem "Expecting Seed int")
             |. chomp_while (Char.equal ' ')
           ; succeed (Done (List.rev seeds)) ] )

let is_var v = (not (Char.equal '-' v)) && (not (Char.equal v '\n')) && not (Char.equal v ' ')

let map_to_parser =
  let open Bark in
  succeed (fun from_val to_val -> (from_val, to_val))
  |= variable ~start:is_var ~inner:is_var ~reserved:String_set.empty
       ~expecting:(Problem "Expecting \"from\" variables")
  |. token (Token ("-to-", Problem "Expecting hyphen"))
  |= variable ~start:is_var ~inner:is_var ~reserved:String_set.empty
       ~expecting:(Problem "Expecting \"to\" variables")
  |. spaces
  |. keyword (Token ("map:", Problem "Expecting title for map list"))
  |. token (Token ("\n", Problem "Expecting newline"))

let range_parser =
  let open Bark in
  succeed (fun dest src len -> Range.make dest src len)
  |= (int (Problem "Expecting int for destination") |> map (fun v -> Range.Dest v))
  |. spaces
  |= (int (Problem "Expecting int for source") |> map (fun v -> Range.Src v))
  |. spaces
  |= (int (Problem "Expecting int for len") |> map (fun v -> Range.Len v))

let ranges_parser =
  let open Bark in
  loop [] (fun ranges ->
      one_of
        [ succeed (fun range -> Loop (range :: ranges)) |= range_parser
        ; succeed (Done (List.rev ranges))
          |. (token (Token ("\n\n", Problem "Expecting double newline delimiter")) |> backtrackable)
        ; succeed (Loop ranges) |. token (Token ("\n", Problem "Expecting newline"))
        ; succeed (Done (List.rev ranges)) ] )

let input_parser =
  let open Bark in
  succeed (fun seeds a -> (seeds, a))
  |= seeds_parser |. spaces
  |= loop [] (fun groups ->
         one_of
           [ map_to_parser
             |> and_then (fun map_to ->
                    ranges_parser |> map (fun ranges -> Loop ((map_to, ranges) :: groups)) )
           ; endd (Problem "Expecting EOF") |> map (fun _ -> Done (List.rev groups)) ] )

let run_through_ranges src_key cur_val input (_, (ranges : Range.t list)) =
  let src_matches =
    List.map
      (fun range -> Range.in_src_range cur_val range |> Option.value ~default:[cur_val])
      ranges
    |> List.flatten
  in
  let next = String_map.find_opt src_key input in
  match next with
  | None -> Done src_matches
  | Some (next_dest, next_ranges) -> Loop (next_dest, next_ranges, src_matches)

let run_pairs pairs input =
  let rec run_pairs pairs ~results =
    match pairs with
    | [] -> results
    | (src_key, pair_val) :: next -> (
        Printf.printf "%s, %d\n" src_key pair_val ;
        let next_result =
          run_through_ranges src_key pair_val input (String_map.find src_key input)
        in
        match next_result with
        | Done next_result -> run_pairs next ~results:(List.append results next_result)
        | Loop (next_src_key, _, next_vals) ->
            let next_pairs = List.map (fun next_val -> (next_src_key, next_val)) next_vals in
            let deduped = Pair_set.of_list (List.append next next_pairs) |> Pair_set.to_list in
            run_pairs deduped ~results )
  in
  run_pairs pairs ~results:[]

let format_input (input : ((string * string) * Range.t list) list) =
  let rec format_input input ~formatted =
    match input with
    | [] -> formatted
    | ((src_val, dest_val), range) :: next ->
        Printf.printf "%s -> %s\n" src_val dest_val ;
        format_input next ~formatted:(String_map.add src_val (dest_val, range) formatted)
  in
  format_input input ~formatted:String_map.empty

let () =
  let input = Stdio.In_channel.read_all "day_05/input.txt" in
  match Bark.run input_parser input with
  | Ok (seeds, range_map_list) ->
      let formatted = format_input range_map_list in
      let results = run_pairs (List.map (fun pair_val -> ("seed", pair_val)) seeds) formatted in
      let smallest = List.fold_left (fun acc cur -> min acc cur) (List.hd results) results in
      print_endline (string_of_int smallest)
  | Error dead_ends -> print_endline (dead_ends_to_string dead_ends)
