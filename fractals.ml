(*****************************************************************************)
(*#load "graphics.cma" ;; (* à ajouter si 4.02.3 *)*)
#use "topfind";;
#require "graphics";;
open Graphics ;;

open Random;; (* loads the Random module *)
self_init();; (* initializes the random generator *)
(*
** The function int: int -> int can now be called.
** int bound returns a random integer between
** 0 (inclusive) and bound (exclusive).
*)

(*****************************************************************************)
(*                                                                           *)
(*   The funcions in the next section are needed by most of the fractals     *)
(*                                                                           *)
(*****************************************************************************)

(*
** draw_line (x, y) (z, t)
** Draw a line from point (x,y)
** to point (z,t) with type :
** val draw_line : int * int -> int * int -> unit = <fun>
*)
let draw_line (x, y) (z, t) =
  moveto x y ;
  lineto z t ;;

(*
** draw_triangle p1 p2 p3
** Draw a triangle by drawing lines between
** all three points p1 p2 p3. Type :
** val draw_triangle : int * int -> int * int -> int * int -> unit = <fun>
*)
let draw_triangle p1 p2 p3 =
  draw_line p1 p2;
  draw_line p1 p3;
  draw_line p2 p3;;

(*
** square p1 p2 p3 p4
** Draw a square by drawing lines between
** all four points p1 p2 p3 p4. Type :
** val square : int * int -> int * int -> int * int -> int * int -> unit = <fun>
*)
let draw_square p1 p2 p3 p4 =
  draw_line p1 p2;
  draw_line p2 p3;
  draw_line p3 p4;
  draw_line p4 p1;;

(*
** ftoi x
** Transform a float into a int by rounding it. Type :
** val ftoi : float -> int = <fun>
*)
let ftoi x = int_of_float (Float.round x) ;;

(*
** itof x
** Transform a int into a float. Type :
** val itof : int -> float = <fun>
*)
let itof x = float_of_int x ;;

(*****************************************************************************)

(*
** mountain n (x,y) (z,t)
** displays a moutain of order n
** between (x,y) and (z,t)
*)

let mountain n (x, y) (z, t) =
  if n < 0 then invalid_arg "[Error] n cannot be negative !";
  let rec aux n (x, y) (z, t) =
    if n <= 0 then draw_line (x,y) (z,t)
    else
      let m = (x + z) / 2 and
          h = (y + t) / 2 + int(abs(z - x)/5 + 20) in
        begin
          aux (n-1) (x,y) (m,h);
          aux (n-1) (m,h) (z,t);
        end in
  aux n (x, y) (z, t) ;;

(*open_graph " 900x500" ;*)
(*clear_graph ();*)
(*mountain 9 (10,200) (800,200) ;;*)

(*****************************************************************************)

(*
** dragon n (x,y) (z,t)
** displays a dragon of order n
** starting from (x,y) and (z,t)
*)

let dragon n (x, y) (z, t) =
  if n < 0 then invalid_arg "[Error] n cannot be negative !";
  let rec aux n (x, y) (z, t) =
    if n <= 0 then draw_line (ftoi x, ftoi y) (ftoi z, ftoi t)
    else
      let u = (x+.z)/.2. +. (t-.y)/.2. and
          v = (y+.t)/.2. -. (z-.x)/.2. in
      begin
        aux (n-1) (x, y) (u,v);
        aux (n-1) (z, t) (u,v);
      end
  in aux n (itof x, itof y) (itof z, itof t) ;;

(*open_graph " 600x600" ;*)
(*clear_graph () ;*)
(*dragon 19 (150, 150) (350, 350) ;;*)

(*
** dragon_bonus n (x,y) (z,t)
** displays a dragon of order n
** starting from (x,y) and (z,t)
*)

let dragon_bonus n (x, y) (z, t) =
  if n < 0 then invalid_arg "[Error] n cannot be negative !";
  let rec aux n (x, y) (z, t) c =
    if n <= 0 then draw_line (ftoi x, ftoi y) (ftoi z, ftoi t)
    else
      let u = (x+.z)/.2. +. (t-.y)/.2. and
          v = (y+.t)/.2. -. (z-.x)/.2. in
      begin
        (* Change l'ordre des appels recursif en fonction du cote (gauche | droite) *)
        if c = 'D' then (
          aux (n-1) (x, y) (u,v) 'D';
          aux (n-1) (z, t) (u,v) 'G';)
        else (
          aux (n-1) (z, t) (u,v) 'D';
          aux (n-1) (x, y) (u,v) 'G';)
      end
  in aux n (itof x, itof y) (itof z, itof t) 'D' ;;

(*open_graph " 600x600" ; *)
(*clear_graph () ; *)
(*dragon_bonus 19 (150, 150) (350, 350) ;; *)

(*****************************************************************************)

(*
** koch_curve n (x,y) (z,t)
** displays a koch curve of order n
** from starting positions (x,y) and (z,t)
*)

(*
                 (mx,my)
                    .
                   /\
                  /  \
  (x,y) ._______./    \._______. (z,t)
            (x1,y1)  (x2,y2)
*)

let koch_curve n (x,y) (z,t) =
  if n < 0 then invalid_arg "[Error] n cannot be negative !";
  let fsquare x = x *. x in
  let pi = 4.0 *. atan 1.0 in
  let rec aux n (x,y) (z,t) =
    if n <= 0 then draw_line (ftoi x, ftoi y) (ftoi z, ftoi t)
    else (
      let l = sqrt(fsquare(z-.x) +. fsquare(t-.y)) /. 3. in
      let difx = z -. x and
          dify = t -. y in
      let x1 = x +. (difx *. (1. /. 3.)) and
          y1 = y +. (dify *. (1. /. 3.)) and
          x2 = x +. (difx *. (2. /. 3.)) and
          y2 = y +. (dify *. (2. /. 3.)) in
      let mx = x1 +. (l *. (cos (pi/.3. +. (atan2 dify difx)))) and
          my = y1 +. (l *. (sin (pi/.3. +. (atan2 dify difx)))) in
      aux (n-1) (x,y) (x1,y1);
      aux (n-1) (x1, y1) (mx, my);
      aux (n-1) (mx,my) (x2, y2);
      aux (n-1) (x2,y2) (z,t);
    ) in
  aux n (itof x, itof y) (itof z, itof t) ;;

(*open_graph " 1020x300" ;*)
(*clear_graph () ;*)
(*koch_curve 6 (5,5) (1000,5) ;;*)

(*
** koch_snowflake n (x,y) (z,t)
** displays a koch snowflake of order n
** from starting position (x,y) and size d
*)

let koch_snowflake n (x,y) d =
  if n < 0 then invalid_arg "[Error] n cannot be negative !";
  let triangle_height = ftoi ((sqrt 3. /. 2.) *. itof d) in
  begin
    koch_curve n (x,y) (x+d/2, y - triangle_height);
    koch_curve n (x-d/2, y - triangle_height) (x,y);
    koch_curve n (x+d/2, y - triangle_height) (x-d/2, y - triangle_height);
  end ;;

(*open_graph " 800x800" ;*)
(*clear_graph () ;*)
(*koch_snowflake 6 (400,700) 400;;*)

(*****************************************************************************)

(*
** carpet n (x,y)
** displays a Sierpinski carpet of order n
** from starting point (x,y)
*)

let carpet n (x,y) =
  if n < 0 then invalid_arg "[Error] n cannot be negative !";
  let rec aux (x,y) l =
    if l >= 3. then
      let l3 = l /. 3. in begin
        (* Place a white square in the center *)
        fill_rect (ftoi (x+.l3)) (ftoi (y+.l3)) (ftoi l3-1) (ftoi l3-1);
        aux (x, y) l3;
        aux (x +. l3, y) l3;
        aux (x +. l3 *. 2., y) l3;
        aux (x +. l3 *. 2., y +. l3) l3;
        aux (x +. l3 *. 2., y +. l3 *. 2.) l3;
        aux (x +. l3, y +. l3 *. 2.) l3;
        aux (x, y +. l3 *. 2.) l3;
        aux (x, y +. l3) l3;
      end
  in begin
    set_color black;
    fill_rect x y (n-1) (n-1);
    set_color white;
    aux (itof x, itof y) (itof n);
    set_color black
  end;;

(*open_graph " 300x300" ;*)
(*clear_graph () ;*)
(*carpet 243 (10,10) ;;*)

(*****************************************************************************)

(*
** sierpinski n (x,y) size
** displays a sierpinski triangle
** from (x,y) with size size
*)

(*             _
              / \
             /   \
            /     \
  (x2,y2) ./_______\. (x3,y3)
          /\       /\
         /  \     /  \
        /    \   /    \
      ./______\./______\
  (x,y)     (x1,y1)
*)

let sierpinski n (x,y) size =
  if n < 0 || size < 0 then invalid_arg "[Error] n or size cannot be negative !";
  let rec aux n (x,y) l =
    let triangle_height = (sqrt 3. /. 2.) *. l in
    let l2 = l /. 2. in
    if n > 0 && l >= 1. then
      let (x1, y1) = (x +. l2, y) and
          (x2, y2) = (x +. l2 /. 2., y +. triangle_height /. 2.) and
          (x3, y3) = (x +. l2 +. l2 /. 2., y +. triangle_height /. 2.) in
        begin
          (* draw the center reversed triangle *)
          draw_triangle (ftoi x1, ftoi y1) (ftoi x2, ftoi y2) (ftoi x3, ftoi y3);
          aux (n-1) (x,y) l2;
          aux (n-1) (x1,y1) l2;
          aux (n-1) (x2,y2) l2;
        end in
  begin
    let triangle_height = ftoi ((sqrt 3. /. 2.) *. itof size) in
    draw_triangle (x,y) (x+size, y) (x+size/2, y + triangle_height);
    aux n (itof x, itof y) (itof size);
  end ;;

(*open_graph " 1100x600" ;*)
(*clear_graph ();*)
(*sierpinski 10 (5, 5) 300;;*)

(*****************************************************************************)

(*
** four_circles r (x,y) limit
** displays a circle of radius r from starting position (x,y)
** and with the drawing limit limit
*)

let four_circles r (x,y) limit =
  if r < 0 || limit < 0 then invalid_arg "[Error] r or limit cannot be negative !";
  let rec aux r (x,y) l =
    if r >= (itof limit) then begin
        draw_circle (ftoi x) (ftoi y) (ftoi r);
        let new_r = r /. 2. in
        aux new_r (x-.new_r, y) l;
        aux new_r (x+.new_r, y) l;
        aux new_r (x, y-.new_r) l;
        aux new_r (x, y+.new_r) l;
      end in
  aux (itof r) (itof x, itof y) limit ;;

(*open_graph " 600x600" ;*)
(*clear_graph ();*)
(*four_circles 200 (300,300) 10;;*)

(*****************************************************************************)

(*
** arrow r (x,y) orient limit
** displays an arrow of radius r from starting position (x,y)
** with orientation orient and with the drawing limit limit
*)

let rec arrow r (x,y) orient limit =
  if r < 0 || limit < 0 then invalid_arg "[Error] r or limit cannot be negative !";
  if r > limit then
    begin
      fill_circle x y r;
      let r2 = r/2 in
      match orient with
        | 'N' -> ( arrow r2 (x,y+r+r2) 'N' limit;
                  arrow r2 (x-r-r2,y) 'W' limit;
                  arrow r2 (x+r+r2,y) 'E' limit; )

        | 'S' -> ( arrow r2 (x,y-r-r2) 'S' limit;
                  arrow r2 (x-r-r2,y) 'W' limit;
                  arrow r2 (x+r+r2,y) 'E' limit; )

        | 'E' -> ( arrow r2 (x,y+r+r2) 'N' limit;
                  arrow r2 (x,y-r-r2) 'S' limit;
                  arrow r2 (x+r+r2,y) 'E' limit; )

        | 'W' -> ( arrow r2 (x,y+r+r2) 'N' limit;
                  arrow r2 (x-r-r2,y) 'W' limit;
                  arrow r2 (x,y-r-r2) 'S' limit; )

        | _   -> invalid_arg "orient must be either 'N' , 'S' , 'E' or 'W'"
    end ;;

(*open_graph " 600x600" ;*)
(*clear_graph ();*)
(*arrow 100 (300,300) 'N' 0;;*)

(*****************************************************************************)

(*
** pytagora_tree n (x,y) size
** displays a pytagora tree of order n
** with starting position (x,y)
** and size n
*)

let pytagora_tree n (x,y) size =
  if n < 0 || size < 0 then invalid_arg "[Error] n or size cannot be negative !";
  let rec aux n (x1,y1) (x2,y2) =
    if n > 0 then
      let difx = x2 -. x1 and
          dify = y2 -. y1 in
      (* p3 = top of the triangle *)
      let x3 = x1 +. 0.5 *. (difx -. dify) and
          y3 = y1 +. 0.5 *. (dify +. difx) in
      begin
        let dx = x3 -. x1 and dy = y3 -. y1 in
        (* p4 p5 = left square outer point*)
        let x4 = x3 -. dy and y4 = y3 +. dx and
            x5 = x1 -. dy and y5 = y1 +. dx in begin
            draw_square (ftoi x1, ftoi y1) (ftoi x3, ftoi y3) (ftoi x4, ftoi y4) (ftoi x5, ftoi y5);
            aux (n-1) (x5, y5) (x4, y4);
          end;
        let dx = x3 -. x2 and dy = y3 -. y2 in
        (* p6 p7 = right square outer point*)
        let x6 = x2 +. dy and y6 = y2 -. dx and
            x7 = x3 +. dy and y7 = y3 -. dx in begin
            draw_square (ftoi x2, ftoi y2) (ftoi x6, ftoi y6) (ftoi x7, ftoi y7) (ftoi x3, ftoi y3);
            aux (n-1) (x7, y7) (x6, y6);
          end;
      end
  in begin
      draw_rect x y size size;
      aux n (itof x, itof (y+size)) (itof (x+size), itof (y+size));
    end;;

(*open_graph " 1000x800" ;*)
(*clear_graph ();*)
(*pytagora_tree 15 (450,5) 100;;*)

(*****************************************************************************)

(*
** vicsek_star n (x,y) size
** displays a vicsek star of order n
** from starting position (x,y) and of size size
*)

let vicsek_star n (x,y) size =
  if n < 0 || size < 0 then invalid_arg "[Error] n or size cannot be negative !";
  let rec aux n (x,y) l =
    if n = 0 || l < 3 then fill_rect x y (l-1) (l-1)
    else
      let l3 = ftoi (itof l /. 3.) in begin
        aux (n-1) (x, y) l3;
        aux (n-1) (x + l3*2, y) l3;
        aux (n-1) (x + l3, y + l3) l3;
        aux (n-1) (x, y + l3*2) l3;
        aux (n-1) (x + l3*2, y + l3*2) l3;
      end
in aux n (x, y) size;;

(*open_graph " 510x510" ;*)
(*clear_graph ();*)
(*vicsek_star 5 (5,5) 500;;*)

(*****************************************************************************)
