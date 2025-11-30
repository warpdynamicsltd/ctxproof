eval $(opam env)
dune build || exit 1
dune test || exit 1
rm -f ../ctxproof
cp _build/install/default/bin/ctxproof ../ctxproof