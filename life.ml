(* UCSD CSE 130 : Programming Languages
 *
 * OCaml demo using Conway's Game of Life
 *
 * ztatlock@cs.ucsd.edu
 *)

(* pipe function as infix operator *)
let (|>) x f = f x

(* dimensions of the universe *)
let uH = 30
let uW = 70

(* cell states *)
let dead = 0
let live = 1

(* universe represented as 2d int matrix *)
let mk_univ () =
  Array.make_matrix uH uW dead

(* addresses of neighbors for cell @ (r, c) *)
let neighbors (r, c) =
  let up  = (r + uH - 1) mod uH in
  let dn  = (r + uH + 1) mod uH in
  let lft = (c + uW - 1) mod uW in
  let rht = (c + uW + 1) mod uW in
  [ (up, lft); (up, c); (up, rht)
  ; (r,  lft);          (r,  rht)
  ; (dn, lft); (dn, c); (dn, rht)
  ]

(* given a universe, row i, col j, and cell @ (i, j)
 * determine the next state of that cell *)
let step_cell u i j cell =
  let islive = cell <> 0 in
  let ns =
    (i, j) |> neighbors
           |> List.map (fun (r, c) -> u.(r).(c))
           |> List.fold_left (+) 0
  in
  if islive && 1 < ns && ns < 4 then
    live
  else if not islive && ns = 3 then
    live
  else
    dead

let step_row u i r =
  Array.mapi (step_cell u i) r

(* get the next universe *)
let step u =
  Array.mapi (step_row u) u

let clear () =
  print_string
    ( "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    ^ "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    ^ "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    )

let row_str r =
  let aux acc cell =
    match cell with
    | 0 -> acc ^ "."
    | _ -> acc ^ "o"
  in
  Array.fold_left aux "" r

let univ_str u =
  let aux acc r =
    acc ^ (row_str r) ^ "\n"
  in
  Array.fold_left aux "" u

let nap () =
  let rec loop i =
    if i > 1000000 then
      ()
    else
      loop (i + 1)
  in
  loop 0

(* briefly show the user this universe *)
let display u =
  clear ();
  print_string (univ_str u);
  flush stdout;
  nap ();
  u

let rec main u =
  u |> display
    |> step
    |> main

(* famous life patterns *)

let glider r c u =
  u.(r + 0).(c + 1) <- live;
  u.(r + 1).(c + 2) <- live;
  u.(r + 2).(c + 0) <- live;
  u.(r + 2).(c + 1) <- live;
  u.(r + 2).(c + 2) <- live;
  u

let acorn r c u =
  u.(r + 0).(c + 1) <- live;
  u.(r + 1).(c + 3) <- live;
  u.(r + 2).(c + 0) <- live;
  u.(r + 2).(c + 1) <- live;
  u.(r + 2).(c + 4) <- live;
  u.(r + 2).(c + 5) <- live;
  u.(r + 2).(c + 6) <- live;
  u

let _ =
  () |> mk_univ
     |> acorn 15 35
     |> main

