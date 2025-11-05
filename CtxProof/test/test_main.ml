let () =
  Test_fof_positive.run();
  Test_fof_negative.run();
  Test_line.run();
  
  print_endline "All tests passed!" 