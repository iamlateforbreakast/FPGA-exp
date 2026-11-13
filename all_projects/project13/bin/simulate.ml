open Project13_lib

let () =
  let board = Option.value (Sys.getenv_opt "BOARD") ~default:"nano20k" in
  let ok =
    match board with
    | "nano20k" -> Sim_models.run_sdram () = `Pass
    | "nano4k" -> Sim_models.run_hyperram () = `Pass
    | other ->
      failwith (Printf.sprintf "Unknown BOARD '%s'; expected nano20k or nano4k" other)
  in
  if not ok then exit 1
