let input = [%blob "input.txt"]

type shape =
  | Rock
  | Paper
  | Scissor

let score_from_char = function
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissor
  | _ -> failwith "char unsupported"
;;

let score_from_shape = function
  | Rock -> 1
  | Paper -> 2
  | Scissor -> 3
;;

let get_winner = function
  | Rock -> Paper
  | Paper -> Scissor
  | Scissor -> Rock
;;

let get_loser = function
  | Rock -> Scissor
  | Scissor -> Paper
  | Paper -> Rock
;;

type outcome =
  | Win
  | Lose
  | Draw

let score_from_outcome = function
  | Win -> 6
  | Draw -> 3
  | Lose -> 0
;;

let outcome_from_char = function
  | 'X' -> Lose
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> failwith "char unsupported"
;;

let get_outcome one other =
  match one, other with
  | _ when score_from_shape one = score_from_shape other -> Draw
  | Rock, Scissor | Paper, Rock | Scissor, Paper -> Win
  | _ -> Lose
;;

let score scorer =
  input |> Str.(split (regexp "\n")) |> List.map scorer |> List.fold_left ( + ) 0
;;

let () =
  let part1 =
    score (fun line ->
      let theirs = score_from_char (String.get line 0) in
      let mine = score_from_char (String.get line 2) in
      score_from_shape mine + score_from_outcome (get_outcome mine theirs))
  in
  Printf.printf "Part 1: %d\n" part1;
  let part2 =
    score (fun line ->
      let theirs = score_from_char (String.get line 0) in
      let outcome = outcome_from_char (String.get line 2) in
      let mine =
        match outcome with
        | Win -> get_winner theirs
        | Draw -> theirs
        | Lose -> get_loser theirs
      in
      score_from_shape mine + score_from_outcome outcome)
  in
  Printf.printf "Part 2: %d\n" part2
;;
