eval $(opam env)
dune build
dune test
cp _build/install/default/bin/ctxproof ../ctxproof