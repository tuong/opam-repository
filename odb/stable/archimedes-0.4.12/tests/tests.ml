(*                                                              -*-tuareg-*- *)
open Printf

(* A list of couples (test_name, description, function_to_run). *)
let alltests = [ ("Arrows1", Arrows1.description, Arrows1.draw);
  ("Arrows2", Arrows2.description, Arrows2.draw);
  ("Axes1", Axes1.description, Axes1.draw);
  ("Axes_box", Axes_box.description, Axes_box.draw);
  ("Axes_cross", Axes_cross.description, Axes_cross.draw);
  ("Axes_dates", Axes_dates.description, Axes_dates.draw);
  ("Axes_ortho", Axes_ortho.description, Axes_ortho.draw);
  ("Axes_ylog", Axes_ylog.description, Axes_ylog.draw);
  ("Backend_line", Backend_line.description, Backend_line.draw);
  ("Backend_path", Backend_path.description, Backend_path.draw);
  ("Backend_text", Backend_text.description, Backend_text.draw);
  ("Clip", Clip.description, Clip.draw);
  ("Demo_zoom", Demo_zoom.description, Demo_zoom.draw);
  ("Layout1", Layout1.description, Layout1.draw);
  ("Layout2", Layout2.description, Layout2.draw);
  ("Layout_borders", Layout_borders.description, Layout_borders.draw);
  ("Layout_custom", Layout_custom.description, Layout_custom.draw);
  ("Layout_grid", Layout_grid.description, Layout_grid.draw);
  ("Layout_sync", Layout_sync.description, Layout_sync.draw);
  ("Markers", Markers.description, Markers.draw);
  ("Piecharts", Piecharts.description, Piecharts.draw);
  ("Plot_array", Plot_array.description, Plot_array.draw);
  ("Plot_fill", Plot_fill.description, Plot_fill.draw);
  ("Plot_functions", Plot_functions.description, Plot_functions.draw);
  ("Plot_sampler", Plot_sampler.description, Plot_sampler.draw);
  ("Plot_sqrt", Plot_sqrt.description, Plot_sqrt.draw);
  ("Plot_stack", Plot_stack.description, Plot_stack.draw);
  ("Text", Text.description, Text.draw);
  ("Text_data", Text_data.description, Text_data.draw);
  ("Text_labels", Text_labels.description, Text_labels.draw);
  ("Vector_field", Vector_field.description, Vector_field.draw);
  ("Viewport_autofit", Viewport_autofit.description, Viewport_autofit.draw);
  ("Viewport_path", Viewport_path.description, Viewport_path.draw);
  ("Viewport_two", Viewport_two.description, Viewport_two.draw);
  ("Xy_param", Xy_param.description, Xy_param.draw) ]

let backends = ref []
let cairo out ext () =
  backends := (fun fname -> ["cairo"; out; sprintf "%s.%s" fname ext]) :: !backends
let tikz () =
  backends := (fun fname -> ["tikz"; sprintf "%s.tex" fname]) :: !backends
let graphics () =
  backends := (fun _ -> ["graphics"; "hold"]) :: !backends

let list_tests () =
  List.iter (fun (name,_,_) -> Format.printf "%s@ " name) alltests;
  exit 0

let specs = Arg.align [
  "--ps",  Arg.Unit(cairo "PS" "ps"), " activate PS (cairo) output";
  "--pdf", Arg.Unit(cairo "PDF" "pdf"), " activate PDF (cairo) output";
  "--png", Arg.Unit(cairo "PNG" "png"), " activate PNG (cairo) output";
  "--svg", Arg.Unit(cairo "SVG" "svg"), " activate SVG (cairo) output";
  "--tex", Arg.Unit tikz, " activate LaTeX (TikZ) output";
  "--graphics", Arg.Unit graphics, " activate graphics output (done if no \
    option is given)";
  "--tests", Arg.Unit list_tests, " list all possible tests and stop";
]

let tests =
  let usage = "tests [option1] [option2] ...\n\
    where an option is a test name or one of the following:" in
  let tests = ref [] in
  Arg.parse specs (fun t -> tests := t :: !tests) usage;
  if !backends = [] then cairo "PNG" "png" ();
  if !tests = [] then alltests
  else (
    let add acc t =
      try List.find (fun (name,_,_) -> name = t) alltests :: acc
      with Not_found -> printf "Test not found: %s\n" t; acc in
    List.fold_left add [] !tests
  )

let () =
  let fail_exn = ref 0 in
  List.iter (fun (name, description, test) ->
    List.iter (fun b ->
      Format.printf "@[<2>%s@;- %s@]@." name description;
      try test(b name)
      with e ->
        let bt = Printexc.get_backtrace () in
        incr fail_exn;
        Format.printf "  %s@.%s" (Printexc.to_string e) bt;
    ) !backends
  ) tests;
  if !fail_exn > 0 then
    printf "WARNING: %i test%s failed with an exception.\n"
      !fail_exn (if !fail_exn > 1 then "s" else "")
