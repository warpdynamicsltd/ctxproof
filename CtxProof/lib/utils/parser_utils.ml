let position p =
  let line = p.Lexing.pos_lnum in
  let col = p.Lexing.pos_cnum - p.Lexing.pos_bol in
  (line, col)

let location_to_string sp =
  let sl, sc = position sp in
  (Printf.sprintf "at line %d, character %d" sl (sc + 1))