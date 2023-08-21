let input = [%blob "input.txt"]

let sum_of_ints list =
  let rec aux total = function
    | [] -> total
    | hd :: tail -> aux (total + hd) tail
  in
  aux 0 list
;;

let elf_from_string str =
  str |> Str.(split (regexp "\n")) |> List.map int_of_string |> List.fold_left ( + ) 0
;;

let max list =
  let rec aux curr = function
    | [] -> curr
    | hd :: tail -> aux (if curr > hd then curr else hd) tail
  in
  aux 0 list
;;

let rec take n = function
  | [] -> []
  | hd :: tail -> if n == 0 then [] else hd :: take (n - 1) tail
;;

let () =
  let elves =
    List.(
      input
      |> Str.(split (regexp "\n\n"))
      |> map elf_from_string
      |> sort Stdlib.compare
      |> rev)
  in
  Format.printf "Part 1: %d\n" (List.hd elves);
  Format.printf "Part 2: %d\n" (elves |> take 3 |> sum_of_ints)
;;
