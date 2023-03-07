build: hpack
	cabal --ghc-options='${GHC_OPTIONS}' build

hpack:
	hpack .

test: hpack
	cabal --ghc-options='${GHC_OPTIONS}' test

run: hpack
	cabal --ghc-options='${GHC_OPTIONS}' run

clean: hpack
	cabal clean

format-haskell: hpack
	find app/ src/ test/ -name "*.hs" -exec fourmolu -i {} +

format-nix:
	alejandra --quiet .

format: format-nix format-haskell

ghcid: hpack
	ghcid -c "cabal --ghc-options='${GHC_OPTIONS}' repl"

hlint: hpack
	hlint .

repl: hpack
	cabal repl

ghci: repl

.PHONY: build hpack test run clean format-haskell format-nix format ghcid hlint repl ghci
