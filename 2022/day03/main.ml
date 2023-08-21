let input = [%blob "input.txt"]
let lines = Str.(split (regexp "\n")) input

module CharSet = Set.Make (Char)

let to_charset str = String.to_seq str |> CharSet.of_seq

let get_priority char =
  match char with
  | 'a' .. 'z' -> Char.code char - Char.code 'a' + 1
  | 'A' .. 'Z' -> Char.code char - Char.code 'A' + 27
  | _ -> failwith "invalid char"
;;

let find_common sets =
  List.fold_left CharSet.inter (List.hd sets) (List.tl sets) |> CharSet.choose
;;

let () =
  let part1 =
    lines
    |> List.map (fun line ->
      let length = String.length line in
      let left = String.sub line 0 (length / 2) |> to_charset in
      let right = String.sub line (length / 2) (length / 2) |> to_charset in
      find_common [ left; right ] |> get_priority)
    |> List.fold_left ( + ) 0
  in
  Printf.printf "Part 1: %d\n" part1
;;

let () =
  let rec find_common_by_3 accum lines =
    match lines with
    | [] -> accum
    | a :: b :: c :: tail ->
      find_common_by_3 (find_common (List.map to_charset [ a; b; c ]) :: accum) tail
    | _ -> failwith "unknown error"
  in
  Printf.printf
    "Part 2: %d\n"
    (lines |> find_common_by_3 [] |> List.map get_priority |> List.fold_left ( + ) 0)
;;
