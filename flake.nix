{
  # Requires nix version 2.7 or later

  # $ nix develop
  # $ nix build    [see result/bin when completed]
  # $ nix develop .#orgmode-daedalus.llvm_9.default
  # $ nix develop .#orgmode-daedalus.llvm_9.ghc98
  # $ nix develop .#orgmode-daedalus.ghc98.llvm_9
  # $ nix run
  # $ nix run .#orgmode-daedalus

  description = "The orgmode-daedalus parser/printer library";

  nixConfig.bash-prompt-suffix = "orgmode-daedalus.env} ";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    levers = {
      url = "github:kquick/nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    alex-tools-src = {
      url = "github:GaloisInc/alex-tools";
      flake = false;
    };
    bv-sized-src = {
      url = "github:GaloisInc/bv-sized";
      flake = false;
    };
    daedalus-src = {
      url = "github:galoisinc/daedalus";
      flake = false;
    };
    diagnose-src = {
      url = "github:mesabloo/diagnose";
      flake = false;
    };
    kvitable = {
      url = "github:kquick/kvitable";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
      inputs.microlens-src.follows = "microlens-src";
      inputs.named-text.follows = "named-text";
      inputs.parameterized-utils-src.follows = "parameterized-utils-src";
      inputs.sayable.follows = "sayable";
      inputs.tasty-checklist.follows = "tasty-checklist";
    };
    language-rust-src = {
      url = "github:GaloisInc/language-rust";
      flake = false;
    };
    microlens-src = {
      url = "github:stevenfontanella/microlens";
      flake = false;
    };
    named-text = {
      url = "github:kquick/named-text";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
      inputs.sayable.follows = "sayable";
      inputs.microlens-src.follows = "microlens-src";
      inputs.parameterized-utils-src.follows = "parameterized-utils-src";
      inputs.tasty-checklist.follows = "tasty-checklist";
    };
    parameterized-utils-src = {
      url = "github:GaloisInc/parameterized-utils";
      flake = false;
    };
    sayable = {
      url = "github:kquick/sayable";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
    };
    tasty-checklist = {
      url = "github:kquick/tasty-checklist";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
      inputs.microlens-src.follows = "microlens-src";
      inputs.parameterized-utils-src.follows = "parameterized-utils-src";
    };
    tasty-sugar = {
      url = "github:kquick/tasty-sugar";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
      inputs.microlens-src.follows = "microlens-src";
      inputs.named-text.follows = "named-text";
      inputs.kvitable.follows = "kvitable";
      inputs.parameterized-utils-src.follows = "parameterized-utils-src";
      inputs.sayable.follows = "sayable";
      inputs.tasty-checklist.follows = "tasty-checklist";
    };
  };

  outputs = { self, nixpkgs, levers
            , alex-tools-src
            , bv-sized-src
            , daedalus-src
            , diagnose-src
            , kvitable
            , language-rust-src
            , microlens-src
            , named-text
            , parameterized-utils-src
            , sayable
            , tasty-checklist
            , tasty-sugar
            }:
    rec
      {
        devShells = levers.haskellShells
          { inherit nixpkgs;
            flake = self;
            ghcvers = system: [ "ghc910" ];
            # additionalPackages = pkgs: [ pkgs.? ];
          };

        packages = levers.eachSystem (system:
          let mkHaskell = levers.mkHaskellPkg { inherit nixpkgs system; };
              pkgs = import nixpkgs { inherit system; };
          in rec
            {
              default = orgmode-daedalus;
              alex-tools = mkHaskell "alex-tools" alex-tools-src {};
              bv-sized = mkHaskell "bv-sized" bv-sized-src {
                inherit parameterized-utils;
              };
              daedalus = mkHaskell "daedalus" daedalus-src {
                adjustDrv = args: pkgs.haskell.lib.dontCheck;
                inherit daedalus-core daedalus-utils daedalus-value daedalus-vm
                  rts-hs rts-hs-data parameterized-utils alex-tools;
              };
              daedalus-core = mkHaskell "daedalus-core" "${daedalus-src}/daedalus-core" {
                inherit bv-sized daedalus-utils daedalus-value
                  rts-hs rts-hs-data rts-vm-hs parameterized-utils;
              };
              daedalus-vm = mkHaskell "daedalus-vm" "${daedalus-src}/daedalus-vm" {
                inherit daedalus-core daedalus-utils daedalus-value
                  language-rust
                  rts-vm-hs;
              };
              daedalus-utils = mkHaskell "daedalus-utils" "${daedalus-src}/daedalus-utils" {
                inherit alex-tools;
              };
              daedalus-value = mkHaskell "daedalus-value" "${daedalus-src}/daedalus-value" {
                inherit daedalus-utils rts-hs rts-hs-data;
              };
              language-rust = mkHaskell "language-rust" language-rust-src {};
              microlens = mkHaskell "microlens" "${microlens-src}/microlens" {};
              rts-hs = mkHaskell "rts-hs" "${daedalus-src}/rts-hs" {
                inherit rts-hs-data;
              };
              rts-hs-data = mkHaskell "rts-hs-data" "${daedalus-src}/rts-hs-data" {};
              rts-vm-hs = mkHaskell "rts-vm-hs" "${daedalus-src}/rts-vm-hs" {
                inherit rts-hs-data;
              };
              diagnose = mkHaskell "diagnose" diagnose-src {
                configFlags = [ "-fmegaparsec-compat"
                                # "--allow-newer=containers"
                                #"--jailbreak"
                              ];
                # diagnose has an upper limit on containers that disallows GHC
                # 9.10 or later.  This constraint is not necessary.  The
                # pkgs.haskell.lib.doJailbreak is too macro and doesn't quite do
                # it, so this more directly removes bounds from containers:
                #
                adjustDrv = args: drv:
                  drv.overrideAttrs (p: {
                    preConfigure = ''
                      sed -i -e 's/[ \t]containers[ \t.><=^0-9&*]*/ containers/' diagnose.cabal
                      '';
                  });
              };
              parameterized-utils = mkHaskell "parameterized-utils"
                parameterized-utils-src { inherit microlens; };
              orgmode-daedalus = mkHaskell "orgmode-daedalus" self {
                inherit
                  daedalus
                  diagnose
                  named-text
                  parameterized-utils
                  rts-hs-data
                  rts-vm-hs
                  sayable
                  tasty-checklist
                  tasty-sugar
                ;
              };
            });
      };
}
