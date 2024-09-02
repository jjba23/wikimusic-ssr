.PHONY: all test clean

fmt:
	find . -name '*.hs' -type f -exec ormolu --mode inplace {} \;
	find . -name '*.nix' -exec nixfmt {} \;
	-statix check
	-deadnix -f
	-npm install stylelint-config-recommended --save-dev
	-stylelint  "resources/css/*.css"
ghcid-dev:
	ghcid --warnings --restart=./wikimusic-ssr.cabal --test "WikiMusic.SSR.Boot.boot"
dev: fmt
	make ghcid-dev

# watchexec -r -e css "make ghcid-dev" ?????
test:
	nix run .#test
push-cache:
	nix path-info --recursive | cachix push wikimusic-ssr
	nix flake --extra-experimental-features 'nix-command flakes' \
		--accept-flake-config archive --json | jq --raw-output '.path, (.inputs | to_entries [] .value.path)' | cachix push wikimusic-ssr
push-cache-arm:
	nix --system aarch64-linux path-info --recursive | cachix push wikimusic-ssr
	nix --system aarch64-linux flake --extra-experimental-features 'nix-command flakes' \
		--accept-flake-config archive --json | jq --raw-output '.path, (.inputs | to_entries [] .value.path)' | cachix push wikimusic-ssr

cabal-release:
	cabal sdist -o .

build:
	nix build --extra-experimental-features 'nix-command flakes' --accept-flake-config --system x86_64-linux
build-arm:
	nix build --extra-experimental-features 'nix-command flakes' --accept-flake-config --system aarch64-linux

