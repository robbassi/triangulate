type point = { x : int; y : int }
type line = { a : point; b : point; }
type intersection = None | At of point

(* util *)
let make_line a b = {a;b}

let string_of_point = function
  | {x;y} -> Printf.sprintf "(%d,%d)" x y

let string_of_line = function
  | ({a;b} : line) ->
    Printf.sprintf "%s->%s"
      (string_of_point a)
      (string_of_point b)

let print_lines lines =
  List.iter (fun l -> Printf.printf "%s\n" (string_of_line l)) lines

(* busy loop *)
let idle () = 
  while true do
    ignore ()
  done

(* build config string based on max_x/max_y of all points *)
let suggest_bounds points =
  let max_x = ref 0 in
  let max_y = ref 0 in
  List.iter (fun p ->
      if p.x > !max_x then max_x := p.x;
      if p.y > !max_y then max_y := p.y)
    points;
  Printf.sprintf " %dx%d" (!max_x + 10) (!max_y + 10)

let coefficients (line : line) =
  (line.a.y - line.b.y,
   line.b.x - line.a.x,
   -(line.a.x * line.b.y - line.b.x * line.a.y))


(* check if two lines intersect *)
let intersect l1 l2 =
  let is_origin p = (l1.a = p || l1.b = p) && (l2.a = p || l2.b = p) in
  let is_within_bounds p =
    let distance p1 p2 =
      sqrt(float(p1.x - p2.x) ** 2. +. float(p1.y - p2.y) ** 2.)
    in
    (distance l1.a p) +. (distance p l1.b) -. (distance l1.a l1.b) < 1.0 &&
    (distance l2.a p) +. (distance p l2.b) -. (distance l2.a l2.b) < 1.0
  in
  let (l10,l11,l12) = coefficients l1 in
  let (l20,l21,l22) = coefficients l2 in
  let d = l10 * l21 - l11 * l20 in
  let dx = l12 * l21 - l11 * l22 in
  let dy = l10 * l22 - l12 * l20 in
  if d <> 0 then
    let x = dx / d in
    let y = dy / d in
    let intersection_point = {x;y} in
    if not (is_within_bounds intersection_point) || is_origin intersection_point then
      None
    else
      At intersection_point
  else
    None

(* triangulate all the points *)
let triangulate points =
  let rec first_pass points =
    match points with
    | _ :: [] | [] -> []
    | p1 :: p2 :: tl -> {a=p1;b=p2} :: first_pass (p2 :: tl)
  in
  let overlaps line lines =
    List.exists
      (fun l -> 
          match intersect l line with
          | None -> false
          | At p -> true)
      lines
  in
  let attach_from p lines =
    List.fold_left
      (fun accum p2 ->
         let new_line = {a=p;b=p2} in
         if p <> p2 && not (overlaps new_line accum) then
           new_line :: accum
         else
           accum)
      lines
      points
  in
  let attach_all lines =
    List.fold_left
      (fun accum p -> attach_from p accum)
      lines
      points
  in

  (* triangulate *)
  points
  |> List.sort (fun p1 p2 -> p1.x - p2.x)
  |> first_pass
  |> attach_all

(* main entry point *)
let run () =
  let open Graphics in
  let points = ref [] in
  while true do
    let lines = ref (triangulate !points) in

    (* create window that can fit all the points *)
    !points
    |> suggest_bounds
    |> open_graph;

    (* draw points *)
    List.iter (fun {x;y} ->
        draw_circle x y 3)
      !points;

    (* draw lines *)
    List.iter (fun ({a;b} : line) ->
        moveto a.x a.y;
        lineto b.x b.y)
      !lines;

    (* draw labels *)
    (* List.iter (fun {x;y} -> *)
    (*     moveto (x - 5) (y - 5); *)
    (*     draw_string (Printf.sprintf "(%d,%d)" x y)) *)
    (*   !points; *)

    let status = wait_next_event [Button_up] in
    points := !points @ [{x=status.mouse_x;y=status.mouse_y}]
  done

let test_intersect () =
  let open Printf in
  let test_points = [
    {a={x=1;y=1};b={x=10;y=1}}, {a={x=1;y=2};b={x=10;y=2}};
    {a={x=10;y=0};b={x=0;y=10}}, {a={x=0;y=0};b={x=10;y=10}};
    {a={x=(-5);y=(-5)};b={x=0;y=0}}, {a={x=1;y=1};b={x=10;y=10}};
    {a={x=10;y=10};b={x=10;y=100}}, {a={x=10;y=100};b={x=50;y=75}};
    {a={x=10;y=10};b={x=10;y=100}}, {a={x=15;y=15};b={x=100;y=15}};
    {a={x=50;y=75};b={x=60;y=10}}, {a={x=10;y=10};b={x=100;y=100}};
  ] in
  List.iter (fun (l1,l2) ->
      printf "checking lines [%s, %s]:\n\t"
        (string_of_line l1)
        (string_of_line l2);
      match intersect l1 l2 with
      | At p ->
        printf "they intersect at %d,%d\n" p.x p.y;
      | None -> printf "no intersection\n")
    test_points
   
let () =
  (* test_intersect (); *)
  run()
