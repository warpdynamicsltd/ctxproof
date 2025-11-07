eval $(opam env)
dune build
dune test
rm -f ../ctxproof
cp _build/install/default/bin/ctxproof ../ctxproof