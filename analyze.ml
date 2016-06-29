
module M = Map.Make(String)

let read_list input =
  let aux = function
    | `String "no" -> false
    | `String "yes" -> true
    | _ -> assert false
  in
  match List.map aux input with
  | a :: r -> a, Array.of_list r
  | _ -> assert false

let read_input json =
  let rec aux acc = function
    | (test, `List l) ->
      let a = read_list l in
      M.add test a acc
    | _ -> assert false
  in
  match json with
  | `Assoc l -> List.fold_left aux M.empty l
  | _ -> assert false

let print_res fmt b =
  Format.fprintf fmt "%5s"
    (Format.sprintf "%B" b)

let print_expect expected fmt res =
  let color =
    if expected = res then "green" else "Red"
  in
  CCFormat.with_color color print_res fmt res

let print_array fmt (expected, a) =
  Format.fprintf fmt "@[<h>(%a) %a@]"
    (CCFormat.with_color "White" print_res) expected
    (CCFormat.array ~start:"" ~sep:" " ~stop:"" (print_expect expected)) a

let print fmt m =
  let aux test a =
    Format.fprintf fmt "%s:\t %a@." test print_array a
  in
  M.iter aux m

let filter m =
  let aux _ (expect, ares) =
    Array.exists (fun res -> expect <> res) ares
  in
  M.filter aux m

let proba_correct (expect, ares) =
  let p = Array.fold_left (fun p res -> if res = expect then p + 1 else p) 0 ares in
  2 * p > Array.length ares

let proba m =
  let aux _ arg acc = if proba_correct arg then acc + 1 else acc in
  let p = M.fold aux m 0 in
  (float p) /. (float @@ M.cardinal m)

let probas m =
  let acc = Array.make (Array.length (snd @@ snd (M.choose m))) 0 in
  let aux _ (expect, ares) =
    Array.iteri (fun i res -> if res = expect then acc.(i) <- acc.(i) + 1) ares
  in
  M.iter aux m;
  let n = float @@ M.cardinal m in
  Array.map (fun p -> (float p /. n)) acc

let () =
  (* setup *)
  Format.set_margin 1000;
  CCFormat.set_color_default true;
  (* reading *)
  let m = read_input (Yojson.Safe.from_file Sys.argv.(1)) in
  (* treat input *)
  print Format.std_formatter (filter m);
  let p = probas m in
  Format.printf "@[<h>probas      : %a@]@."
    (CCFormat.array ~start:"" ~sep:" " ~stop:"" CCFormat.float) p;
  Format.printf "proba (mean): %f\t%f@." (proba m) ((Array.fold_left (+.) 0. p) /. (float @@ Array.length p))


