module Point = struct
  type t = { x : int; y : int }

  let distance p1 p2 = sqrt(float(p1.x - p2.x) ** 2. +. float(p1.y - p2.y) ** 2.)
end

module Line = struct
  open Point

  type t = { a : Point.t; b : Point.t; }
  type intersection = None | At of Point.t
  
  let coefficients line =
    (line.a.y - line.b.y, line.b.x - line.a.x, -(line.a.x * line.b.y - line.b.x * line.a.y))

  let is_within_bounds p l1 l2 =
    (distance l1.a p) +. (distance p l1.b) -. (distance l1.a l1.b) < 1.0 &&
    (distance l2.a p) +. (distance p l2.b) -. (distance l2.a l2.b) < 1.0

  (* check if two lines intersect *)
  let intersection l1 l2 =
    let is_origin p =
      (l1.a = p || l1.b = p) && (l2.a = p || l2.b = p)
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
      if not (is_within_bounds intersection_point l1 l2) || is_origin intersection_point then
        None
      else
        At intersection_point
    else
      None

  let overlaps l1 l2 =
    match intersection l1 l2 with
    | None -> false
    | At p -> true
end

module Points2D = struct
  
  (* triangulate all the points *)
  let triangulate points =

    (* connect points left to right, based on x value *)
    let rec first_pass points =
      match points with
      | _ :: [] | [] -> []
      | p1 :: p2 :: tl -> Line.{a=p1;b=p2} :: first_pass (p2 :: tl)
    in

    (* attach point p to lines, if there is no overlap *)
    let attach_from p lines =
      List.fold_left
        (fun accum p2 ->
           let new_line = Line.{a=p;b=p2} in
           if p <> p2 && not (List.exists (Line.overlaps new_line) accum) then
             new_line :: accum
           else
             accum)
        lines
        points
    in

    (* attach remaining points, given a set of starting lines *)
    let attach_remaining lines =
      List.fold_left
        (fun accum p -> attach_from p accum)
        lines
        points
    in

    (* triangulate *)
    points
    |> List.sort Point.(fun p1 p2 -> p1.x - p2.x)
    |> first_pass
    |> attach_remaining
end

module Canvas = struct
  (* A drawing surface *)
end

(* util *)

(* busy loop *)
let idle () = 
  while true do
    ignore ()
  done

(* build config string based on max_x/max_y of all points *)
let suggest_bounds points =
  let max_x = ref 0 in
  let max_y = ref 0 in
  List.iter
    Point.(fun p ->
        if p.x > !max_x then max_x := p.x;
        if p.y > !max_y then max_y := p.y)
    points;
  Printf.sprintf " %dx%d" (!max_x + 10) (!max_y + 10)

(* main entry point *)
let run () =
  let open Graphics in
  let open Point in
  let open Line in
  let points = ref [] in
  while true do
    let lines = ref (Points2D.triangulate !points) in

    (* create window that can fit all the points *)
    !points
    |> suggest_bounds
    |> open_graph;

    (* draw points *)
    List.iter
      (fun {x;y} ->
        draw_circle x y 3)
      !points;

    (* draw lines *)
    List.iter
      (fun {a;b} ->
          moveto a.x a.y;
          lineto b.x b.y)
      !lines;

    let status = wait_next_event [Button_up] in
    points := !points @ [{x=status.mouse_x;y=status.mouse_y}]
  done
   
let () =
  run()
