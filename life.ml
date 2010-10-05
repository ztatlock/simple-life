let h = 45
let w = 75

let (|>) x f = f x

let range n =
  let rec loop n =
    if n < 0 then
      []
    else
      n :: (loop (n - 1))
  in
  loop (n - 1)


let mkboard () =
  Array.init h (fun _ ->
    Array.init w (fun _ ->
      0))

let get board (r, c) =
  Array.get (Array.get board r) c

let set board (r, c) s =
  Array.set (Array.get board r) c s

let print board =
  List.iter
    (fun r ->
       List.iter
         (fun c ->
           if get board (r, c) = 0 then
             print_string " "
           else
             print_string "o")
         (range w);
       print_string "\n")
    (range h)

let neighbors (r, c) =
  let up = (r + h - 1) mod h in
  let dn = (r +     1) mod h in
  let lt = (c + w - 1) mod w in
  let rt = (c +     1) mod w in
  [ (up, lt); (up, c); (up, rt)
  ; (r,  lt);          (r,  rt)
  ; (dn, lt); (dn, c); (dn, rt)
  ]

let live_neighbors board coords =
  coords
    |> neighbors
    |> List.map (get board) 
    |> List.fold_left (+) 0

let step board =
  let nb = mkboard () in
  List.iter
    (fun r ->
      List.iter
        (fun c ->
          let ln = live_neighbors board (r, c) in
          if 2 < ln && ln < 5 then
            set nb (r, c) 1
          else
            set nb (r, c) 0)
        (range w))
    (range h);
  nb

let sleep () =
  let rec loop n =
    if n <= 0 then
      ()
    else begin
      loop (n - 1);
      loop (n - 1)
    end
  in
  loop 22
  

let clear () =
  List.iter
    (fun _ -> print_endline "")
    (range 80)

let rec loop b =
  let b' = step b in
  clear ();
  print b;
  flush stdout;
  sleep ();
  loop b'


let b = mkboard ()

let _ =
  List.iter
    (fun (r, c) -> set b (r, c) 1)
    [ (1, 1)
    ; (1, 3)
    ; (2, 2)
    ; (2, 3)
    ; (3, 2)
    ; (3, 3)
    ; (3, 1)
    ; (5, 6)
    ; (5, 5)
    ; (5, 7)
    ; (5, 8)
    ; (6, 5)
    ]

let _ = loop b

