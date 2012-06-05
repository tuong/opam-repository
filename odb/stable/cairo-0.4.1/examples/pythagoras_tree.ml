(* Example inspired from
   http://www.ffconsultancy.com/products/fsharp_for_visualization/demo6.html
*)

open Cairo

let pi = 4. *. atan 1.


let rotate t = RotateTransform(t / pi * 180.).Value

let branches m =
  [ m * rotate(pi/2. - asin(4. / 5.)) * scale(4. / 5., 4. / 5.) *
      translate(0., 1.);
    m * translate(-1., 0.) * rotate(-pi/2. + asin(3. / 5.)) *
      scale(3. / 5., 3. / 5.) * translate(1., 1.) ]

let square cr = rectangle cr 0. 0. 1. 1.

let shape brush m =
  Shape([ Interior brush;
          Stroke(Brushes.DarkGreen, { width = 0.01 }) ],
        [ Contour.map (fun p -> p * m) square ])

let trunks brush ms = Group(List.map (shape brush) ms)

let rec tree n ms =
  if n=0 then [trunks Brushes.Green ms] else
    let ms' = List.collect branches ms
    trunks Brushes.BurlyWood ms :: tree (n-1) ms'

let () =
  let view = View(Group[])
  for n = 1 to 12 do
    view.Scene <- Group(tree n [Matrix.Identity])
  Run()
