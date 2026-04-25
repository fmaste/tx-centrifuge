{

  description = "tx-centrifuge: pull-based transaction generator for testing and benchmarking";

  # Without this `nix build` compiles the whole CHaP closure from source:
  # nixConfig applies only from the top-level flake, the node's copy of this
  # does not reach us.
  # Needs a trusted user, or `--accept-flake-config`.
  nixConfig =
    { extra-substituters = [ "https://cache.iog.io" ];
      extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    }
  ;

  # One cardano-node pin decides everything.
  # Here there is no haskell.nix, no nixpkgs of our own, no crypto overlay.
  # It's cardano-node's `legacyPackages`, iohkNix.overlays.crypto and its
  # instantiated project that does the heavy work. We even take libsodium-vrf
  # and the compiler from the same place the node builds with.
  #
  # To have a new pin:
  # > nix build  --override-input cardano-node \
  #     path:$HOME/cardano-node/releases.d/11.0.1
  # Or from a consuming flake, the same choice declaratively:
  # > inputs.tx-centrifuge.inputs.cardano-node.follows = "cardano-node";
  inputs =
    { cardano-node.url = "github:IntersectMBO/cardano-node/master";
      # For `lib` only.
      # Following the node's brings no extra fetching and avoids disagreements.
      nixpkgs.follows = "cardano-node/nixpkgs";
    }
  ;

  outputs = { self, nixpkgs, cardano-node }:
    let
      inherit (nixpkgs) lib;

      systems =
        [ "x86_64-linux"  "aarch64-linux"
          "x86_64-darwin" "aarch64-darwin"
        ]
      ;
      forAllSystems = f:
        lib.genAttrs systems (system: f cardano-node.legacyPackages.${system})
      ;

      ############################################################################
      # `nix build`: the node's own project, with our tree grafted into its src.
      ############################################################################

      # Builds our executable by adding to cardano-node's source a copy of the
      # tx-centrifuge files that exist here in this repo, as it had always been
      # one of its packages. It then compiles ours the way it compiles its own:
      # same GHC, same library versions, same flags.
      #
      # It has to be a copy and not a reference. cabal only treats a package as
      # something to build when the package sits inside the project's own source
      # directory. Named from outside, as a source-repository-package or as an
      # absolute path, cabal files it under prebuilt dependencies instead and
      # there is no executable to pick up.
      #
      # Cost: the node's local packages are rebuilt rather than substituted,
      # since their sources now sit at a different store path. The expensive part
      # of the closure comes from CHaP and still substitutes from cache.iog.io.
      getCardanoNodeExe = cardanoNodePkgs:
        let
          # Generate a derivation / store path holding a cardano-node checkout
          # in which tx-centrifuge is one of the project's packages.
          # Work by appending ${self} to the working tree, dirty or not.
          src = cardanoNodePkgs.runCommand "cardano-node-with-tx-centrifuge"
            { # `\n` in a replacement and the `0,/re/` range are both GNU-only,
              # and stdenv's sed is BSD on darwin.
              nativeBuildInputs = [ cardanoNodePkgs.gnused ];
            }
            ''
            cp -R ${cardano-node} $out
            chmod -R u+w $out
            cp -R ${self} $out/bench/tx-centrifuge
            chmod -R u+w $out/bench/tx-centrifuge
            # Add ourselves to the first `packages:` stanza, in place. Not a
            # second `packages:` field and not cabal.project.local: haskell.nix
            # parses this file to decide which directories the plan derivation
            # needs, so a package it cannot see here is a package cabal will not
            # find there. The keyword is replaced rather than the whole line, so
            # a stanza that already carries an entry on that line keeps it: the
            # entries are whitespace separated either way.
            sed -i \
              '0,/^packages:/s|^packages:|packages:\n  bench/tx-centrifuge|' \
              $out/cabal.project
            # An assertion. If a future cabal.project spells its packages in a
            # way the line above does not match, the graft fails here instead of
            # yielding a plan that silently lacks tx-centrifuge.
            test "$(grep -cE '^  bench/tx-centrifuge($|[[:blank:]])' $out/cabal.project)" = 1
            ''
          ;
          # The above derivation only produces data. It compiles nothing.
          # A haskell.nix project is a module-system evaluation, and appendModule adds one more module to it.
          project = cardanoNodePkgs.cardanoNodeProject.appendModule
            { src = lib.mkForce src;
              modules =
                [ { package-keys = [ "tx-centrifuge" ];
                    # -Wwarn undoes the `-Werror` the node applies to local
                    # packages, which we now are. Later flags win.
                    packages.tx-centrifuge.configureFlags = [ "--ghc-option=-Wwarn" ];
                  }
                ]
              ;
            }
          ;
        in
          project.hsPkgs.tx-centrifuge.components.exes.tx-centrifuge
      ;

      ############################################################################
      # `nix develop`: generate ./cabal.project from the node's cabal.project.
      ############################################################################

      getCardanoNodeCabalProject = cardanoNodePkgs:
        let
          # Taken from the node checkout rather than CHaP: the executable needs
          # the node library, the test-suite needs tx-generator, and both reach
          # the two trace-* packages that live only in that repo. Everything else
          # resolves from CHaP/Hackage at the index-state the checkout pins.
          nodePackages =
            [ "cardano-node"
              "trace-forward"
              "trace-resources"
              "bench/tx-generator"
            ]
          ;
          # The node project's local-package stanzas are the only thing dropped:
          # their paths are relative to that checkout, and the four above are
          # re-added as absolute paths. Everything that decides a version
          # survives verbatim, CHaP's repository stanza and root keys included.
          #
          # A stanza runs until the first line that is neither blank nor
          # indented. Column-0 comments are held back, so a dropped stanza takes
          # its own header comment with it instead of leaving it stranded.
          dropLocalPackages = text:
            let
              re = pattern: line: builtins.match pattern line != null;
              isHeader = re "(packages|extra-packages|optional-packages)[[:blank:]]*:.*";
              isHeld = re "(--.*)?";
              isIndented = re "[[:blank:]].*";
              step = acc: line:
                if isHeld line then
                  acc // { hold = acc.hold ++ [ line ]; }
                else if isHeader line then
                  acc // { hold = [ ]; skip = true; }
                else if acc.skip && isIndented line then
                  acc
                else
                  { skip = false; hold = [ ]; kept = acc.kept ++ acc.hold ++ [ line ]; }
              ;
              end = lib.foldl' step { skip = false; hold = [ ]; kept = [ ]; }
                (lib.splitString "\n" text);
            in
              end.kept ++ end.hold
          ;
          projectLines =
            [ "-- GENERATED by the tx-centrifuge flake; do not edit."
              "-- Pins come from ${cardano-node}/cabal.project"
              "-- Regenerate with `nix develop`. Local tweaks go in cabal.project.local."
              ""
              "packages:"
              "  ."
            ]
            ++ map (p: "  ${cardano-node}/${p}") nodePackages
            ++
            [ ""
              "-- tx-centrifuge never calls sd_notify; keeps libsystemd out of the build."
              "package cardano-node"
              "  flags: -systemd"
              ""
              "package cardano-git-rev"
              "  flags: -systemd"
              ""
            ]
            ++ dropLocalPackages (builtins.readFile "${cardano-node}/cabal.project")
          ;
        in
          cardanoNodePkgs.writeText "cabal.project"
            (lib.concatStringsSep "\n" projectLines)
      ;

    in

      { packages = forAllSystems
          (pkgs:
            { tx-centrifuge = getCardanoNodeExe pkgs;
              default = getCardanoNodeExe pkgs;
              # `nix build .#cabal-project && cat result` to read the pins the
              # dev shell will use, without entering it.
              cabal-project = getCardanoNodeCabalProject pkgs;
            }
          )
        ;
        devShells = forAllSystems
          (pkgs:
            let
              # The compiler and cabal the node itself builds with, rather than
              # a nixpkgs GHC that may differ in patch level. Same source as
              # nix/pkgs.nix: `haskell-nix.cabal-install.${compiler-nix-name}`.
              compilerName = pkgs.cardanoNodeProject.args.compiler-nix-name;
            in
              { default = pkgs.mkShell
                  { name = "tx-centrifuge";
                    nativeBuildInputs =
                      [ pkgs.haskell-nix.compiler.${compilerName}
                        pkgs.haskell-nix.cabal-install.${compilerName}
                        pkgs.pkg-config
                        # cabal clones the node project's source-repository-packages.
                        pkgs.git
                        # protoc, wanted at configure time by the custom Setup.hs
                        # of proto-lens-protobuf-types. A build tool, so it goes
                        # here rather than in buildInputs.
                        pkgs.protobuf
                      ]
                    ;
                    # Everything the plan needs from the system has to be
                    # listed by hand here, and it announces itself in one of
                    # three ways, each later than the last:
                    buildInputs =
                      [ # IOG's VRF fork, from the node's own crypto overlay.
                        pkgs.libsodium-vrf
                        pkgs.secp256k1
                        pkgs.libblst
                        pkgs.lmdb
                        pkgs.liburing
                        pkgs.snappy
                        pkgs.zlib
                        pkgs.ncurses
                      ]
                    ;
                    # Refuses to clobber a cabal.project it did not write, so a
                    # hand-rolled one survives a stray `nix develop`.
                    shellHook =
                      ''
                      if [ -e cabal.project ] && ! head -n1 cabal.project | grep -q '^-- GENERATED'; then
                        echo "tx-centrifuge: ./cabal.project is not generated, leaving it alone" >&2
                      else
                        install -m 0644 ${getCardanoNodeCabalProject pkgs} cabal.project
                      fi
                      echo "tx-centrifuge: pins from ${cardano-node}"
                      ''
                    ;
                  }
                ;
              }
          )
        ;
      }
    ;

}

