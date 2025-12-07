(* Helper function to run a test with timing *)
let run_timed name test_fn =
  let start_time = Sys.time () in
  Printf.printf "Running %s... " name;
  flush stdout;
  test_fn ();
  let end_time = Sys.time () in
  let duration = end_time -. start_time in
  Printf.printf "✓ (%.3f seconds)\n" duration;
  flush stdout

let () =
  Printf.printf "\n=== Running CtxProof Tests ===\n\n";
  flush stdout;

  let total_start = Sys.time () in

  run_timed "Test_fof_positive" Test_fof_positive.run;
  run_timed "Test_fof_negative" Test_fof_negative.run;
  run_timed "Test_line" Test_line.run;
  run_timed "Test_proof" Test_proof.run;
  run_timed "Test_logic_ops" Test_logic_ops.run;

  let total_end = Sys.time () in
  let total_duration = total_end -. total_start in

  Printf.printf "\n=== All tests passed! ===\n";
  Printf.printf "Total time: %.3f seconds\n\n" total_duration;
  flush stdout