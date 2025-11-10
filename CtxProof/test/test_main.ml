let () =
  Test_fof_positive.run();
  Test_fof_negative.run();
  Test_line.run();
  Test_logic_ops.run();
  
  print_endline "All tests passed!" 