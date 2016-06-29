
module Main = struct

  (* Compare floats *)
  let cmp a b =
    if abs_float (a -. b) < 0.000001 then 0
    else if a < b then -1
    else 1

  let (=) a b = (cmp a b == 0)
  let (>) a b = (cmp a b == 1)
  let (<) a b = (cmp a b == -1)
  let (<=) a b = (match cmp a b with 0 | -1 -> true | _ -> false)
  let (>=) a b = (match cmp a b with 0 | 1 -> true | _ -> false)

  (** Points *)
  type point = {
    x : float;
    y : float;
  }

  let point_of_yojson = function
    | `List [`Float x; `Float y] ->
      Result.Ok { x; y; }
    | _ -> Result.Error "bad point"

  (* Distance on points *)
  let dist a b =
    sqrt ((a.x -. b.x) ** 2. +. (a.y -. b.y) ** 2.)

  let eq_points a b =
    a.x = b.x && a.y = b.y

  let quad a =
    if a.x >= 0. then
      (if a.y >= 0. then 0 else 3)
    else
      (if a.y >= 0. then 1 else 2)

  let dist_line (a, b) =
    if eq_points a b then (fun c -> dist a c)
    else
      let d = dist a b in
      (fun c ->
        let p = (b.y -. a.y) *. c.x -.
                (b.x -. a.x) *. c.y +.
                b.x *. a.y -.
                b.y *. a.x
        in
        (abs_float p) /. d
      )

  let area a b c =
    let p = dist a b in
    let q = dist b c in
    let r = dist c a in
    let t = (p +. q +. r) /. 2. in
    sqrt (t *. (t *. p) *. (t -. q) *. (t -. r))

  (* A missing constant in ocaml stdlib, :p *)
  let pi = 4.0 *. atan 1.0

  (* Angles for triple of points *)
  let angle a c b =
    let p = dist a b in
    let q = dist b c in
    let r = dist c a in
    acos (
      (q *. q +. r *. r -. p *. p)
      /. ( 2. *. q *. r)
    )

  (* Minimum radius of a circlr containing 3 points *)
  let min_radius a b c =
    let alpha = angle b a c in
    let beta = angle a b c in
    let gamma = angle b c a in
    if alpha > pi /. 2. then dist b c /. 2.
    else if beta > pi /. 2. then dist a c /. 2.
    else if gamma > pi /. 2. then dist a b /. 2.
    else dist b c /. (2. *. sin (angle b a c))


  (** Types and input functions *)
  type params = {
    radius1 : float [@key "RADIUS1"];
    radius2 : float [@key "RADIUS2"];
    quads   : int   [@key "QUADS"];
    dist    : float [@key "DIST"];
    a_pts   : int   [@key "A_PTS"];
    b_pts   : int   [@key "B_PTS"];
    c_pts   : int   [@key "C_PTS"];
    d_pts   : int   [@key "D_PTS"];
    e_pts   : int   [@key "E_PTS"];
    f_pts   : int   [@key "F_PTS"];
    g_pts   : int   [@key "G_PTS"];
    k_pts   : int   [@key "K_PTS"];
    n_pts   : int   [@key "N_PTS"];
    q_pts   : int   [@key "Q_PTS"];
    epsilon : float [@key "EPSILON"];
    length1 : float [@key "LENGTH1"];
    length2 : float [@key "LENGTH2"];
    area1   : float [@key "AREA1"];
    area2   : float [@key "AREA2"];
  } [@@deriving of_yojson]

  type lcm = string array array

  let lcm_of_yojson = function
    | `Assoc l ->
      let res = Array.make_matrix 15 15 "" in
      let aux = function
        | (t, `List u) ->
          let i = int_of_string t in
          List.iteri
            (fun j s -> res.(i).(j) <- s)
            (List.map (function `String s -> s | _ -> assert false) u)
        | _ -> assert false
      in
      List.iter aux l;
      Result.Ok res
    | _ -> Result.Error "bad lcm"

  type input = {
    random_seed : int;
    params      : params [@key "PARAMETERS"];
    points      : point array;
    puv         : bool array [@key "PUV"];
    lcm         : lcm [@key "LCM"];
  } [@@deriving of_yojson {strict=false}]

  (* Launch Interpretor conditions *)
  let lic0 params points =
    params.length1 > 0. &&
    try
      for i = 0 to Array.length points - 2 do
        let a = points.(i) in
        let b = points.(i + 1) in
        if dist a b > params.length1 then
          raise Exit
      done;
      false
    with Exit ->
      true

  let lic1 params points =
    params.radius1 > 0. &&
    try
      for i = 0 to Array.length points - 3 do
        let radius = min_radius points.(i) points.(i + 1) points.(i + 2) in
        if radius > params.radius1 then raise Exit
      done;
      false
    with Exit ->
      true

  let lic2 params points =
    0. <= params.epsilon && params.epsilon < pi &&
    try
      for i = 0 to Array.length points - 3 do
        let a = points.(i) in
        let b = points.(i + 1) in
        let c = points.(i + 2) in
        if not (eq_points a b) && not (eq_points b c) then begin
          let d = angle a b c in
          if d < pi -. params.epsilon || d > pi +. params.epsilon then
            raise Exit
        end
      done;
      false
    with Exit ->
      true

  let lic3 params points =
    0. < params.area1 &&
    try
      for i = 0 to Array.length points - 3 do
        let s = area points.(i) points.(i + 1) points.(i + 2) in
        if s > params.area1 then
          raise Exit
      done;
      false
    with Exit ->
      true

  let lic4 params points =
    Pervasives.(2 <= params.q_pts && params.q_pts <= Array.length points) &&
    Pervasives.(1 <= params.quads && params.quads <= 3) &&
    try
      for i = 0 to Array.length points - params.q_pts do
        let quads = Array.make 4 0 in
        for j = i to i + params.q_pts - 1 do
          quads.(quad points.(j)) <- 1
        done;
        let s = Array.fold_left (+) 0 quads in
        if Pervasives.(s > params.quads) then raise Exit
      done;
      false
    with Exit ->
      true

  let lic5 params points =
    try
      for i = 0 to Array.length points - 2 do
        if points.(i + 1).x -. points.(i).x < 0. then raise Exit
      done;
      false
    with Exit ->
      true

  let lic6 params points =
    Pervasives.(3 <= params.n_pts && params.n_pts <= Array.length points) &&
    0. <= params.dist &&
    try
      for i = 0 to Array.length points - params.n_pts do
        let f = dist_line (points.(i), points.(i + params.n_pts - 1)) in
        for j = i + 1 to i + params.n_pts - 2 do
          if f points.(j) > params.dist then raise Exit
        done
      done;
      false
    with Exit ->
      true

  let lic7 params points =
    Pervasives.(1 <= params.k_pts && params.k_pts <= Array.length points - 2) &&
    try
      for i = 0 to Array.length points - (params.k_pts + 2) do
        if dist points.(i) points.(i + params.k_pts + 1) > params.length1 then raise Exit
      done;
      false
    with Exit ->
      true

  let lic8 params points =
    Pervasives.(1 <= params.a_pts && 1 <= params.b_pts) &&
    Pervasives.(params.a_pts + params.b_pts <= Array.length points - 3) &&
    try
      for i = 0 to Array.length points - (params.a_pts + params.b_pts + 3) do
        let radius = min_radius
            points.(i)
            points.(i + params.a_pts + 1)
            points.(i + params.a_pts + params.b_pts + 2)
        in
        if radius > params.radius1 then raise Exit
      done;
      false
    with Exit ->
      true

  let lic9 params points =
    Pervasives.(1 <= params.c_pts && 1 <= params.d_pts) &&
    Pervasives.(params.c_pts + params.d_pts <= Array.length points - 3) &&
    try
      for i = 0 to Array.length points - (params.c_pts + params.d_pts + 3) do
        let a = points.(i) in
        let b = points.(i + params.c_pts + 1) in
        let c = points.(i + params.c_pts + params.d_pts + 2) in
        if not (eq_points a b) && not (eq_points b c) then begin
          let d = angle a b c in
          if d < pi -. params.epsilon || d > pi +. params.epsilon then
            raise Exit
        end
      done;
      false
    with Exit ->
      true

  let lic10 params points =
    Pervasives.(1 <= params.e_pts && 1 <= params.f_pts) &&
    Pervasives.(params.e_pts + params.f_pts <= Array.length points - 3) &&
    try
      for i = 0 to Array.length points - (params.e_pts + params.f_pts + 3) do
        let s = area
            points.(i)
            points.(i + params.e_pts + 1)
            points.(i + params.e_pts + params.f_pts + 2) in
        if s > params.area1 then
          raise Exit
      done;
      false
    with Exit ->
      true

  let lic11 params points =
    Pervasives.(1 <= params.g_pts && params.g_pts <= Array.length points - 2) &&
    try
      for i = 0 to Array.length points - (params.g_pts + 2) do
        if points.(i + params.g_pts + 1).x -. points.(i).x < 0. then raise Exit
      done;
      false
    with Exit ->
      true

  let lic12 params points =
    0. <= params.length2 &&
    try
       for i = 0 to Array.length points - (params.k_pts + 2) do
         if dist points.(i) points.(i + params.k_pts + 1) < params.length2 then raise Exit
       done;
       false
     with Exit -> true

  let lic13 params points =
    0. <= params.radius2 &&
    try
      for i = 0 to Array.length points - (params.a_pts + params.b_pts + 3) do
        let radius = min_radius
            points.(i)
            points.(i + params.a_pts + 1)
            points.(i + params.a_pts + params.b_pts + 2)
        in
        if radius <= params.radius2 then raise Exit
      done;
      false
    with Exit ->
      true

  let lic14 params points =
    0. <= params.area2 &&
    try
      for i = 0 to Array.length points - (params.e_pts + params.f_pts + 3) do
        let s = area
            points.(i)
            points.(i + params.e_pts + 1)
            points.(i + params.e_pts + params.f_pts + 2) in
        if s < params.area2 then
          raise Exit
      done;
      false
    with Exit ->
      true

  let cmv input =
    let p = lic7 input.params input.points in
    let q = lic8 input.params input.points in
    let r = lic10 input.params input.points in
    let lic = [|
      lic0;
      lic1;
      lic2;
      lic3;
      lic4;
      lic5;
      lic6;
      (fun _ _ -> p);
      (fun _ _ -> q);
      lic9;
      (fun _ _ -> r);
      lic11;
      (fun x y -> p && lic12 x y);
      (fun x y -> q && lic13 x y);
      (fun x y -> r && lic14 x y);
    |] in
    Array.map (fun f -> f input.params input.points) lic

  let pum cmv lcm =
    Array.init 15 (fun i ->
        Array.init 15 (fun j ->
            match lcm.(i).(j) with
            | "NOTUSED" -> true
            | "ANDD" -> cmv.(i) && cmv.(j)
            | "ORR" -> cmv.(i) || cmv.(j)
            | _ -> assert false
          ))

  let array_for_all a =
    let open Pervasives in
    let rec aux a i =
      if i >= Array.length a then true
      else a.(i) && aux a (i + 1)
    in
    aux a 0

  let fuv puv pum =
    Array.init 15 (fun i ->
        (not puv.(i)) || (array_for_all pum.(i))
      )

  let launch fuv =
    array_for_all fuv

  let decide input =
    let cmv = cmv input in
    Array.iteri (fun i b -> Format.printf "%d : %b@." i b) cmv;
    let pum = pum cmv input.lcm in
    let fuv = fuv input.puv pum in
    launch fuv

end

let () =
  match Main.input_of_yojson (Yojson.Safe.from_file Sys.argv.(1)) with
  | Result.Ok input ->
    if Main.decide input then
      Format.printf "YES@."
    else
      Format.printf "NO@."
  | Result.Error msg ->
    Format.printf "Error during parsing:\n%s@." msg;
    exit 1

