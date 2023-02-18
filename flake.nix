{
  description = "A purescript todomvc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    ps-tools.follows = "purs-nix/ps-tools";

    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";

    utils.url = "github:numtide/flake-utils";

    npmlock2nix = {
      flake = false;
      url = "github:nix-community/npmlock2nix";
    };
  };

  outputs = { nixpkgs, utils, ... }@inputs:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };

          mergeShells = envs: pkgs.mkShell (builtins.foldl'
            (a: v: {
              buildInputs = a.buildInputs ++ v.buildInputs;
              nativeBuildInputs = a.nativeBuildInputs ++ v.nativeBuildInputs;
              propagatedBuildInputs = a.propagatedBuildInputs ++ v.propagatedBuildInputs;
              propagatedNativeBuildInputs = a.propagatedNativeBuildInputs ++ v.propagatedNativeBuildInputs;
              shellHook = a.shellHook + "\n" + v.shellHook;
            })
            (pkgs.mkShell { })
            envs);

          npmlock2nix = import inputs.npmlock2nix { inherit pkgs; };
          ps-tools = inputs.ps-tools.legacyPackages.${system};
          purs-nix = inputs.purs-nix {
            inherit system;
          };
          ps =
            purs-nix.purs
              {
                dependencies =
                  with purs-nix.ps-pkgs;
                  [
                    console
                    effect
                    prelude
                    halogen
                  ];

                dir = ./.;
              };
          pursShell = pkgs.mkShell
            {
              packages =
                with pkgs;
                [
                  entr
                  nodejs
                  (ps.command {
                    bundle = {
                      esbuild = {
                        outfile = "./public/bundle.js";
                      };
                    };
                  })
                  ps-tools.for-0_15.purescript-language-server
                  purs-nix.esbuild
                  purs-nix.purescript
                  nodePackages.purs-tidy
                ];

              shellHook = ''
                zsh
                exit 0
              '';
            };
          nodeShell = npmlock2nix.v2.shell {
            src = ./.;
          };
        in
        {
          packages.default = ps.bundle { };
          formatter = pkgs.nixpkgs-fmt;
          devShells.default = mergeShells [ pursShell nodeShell ];

        });
}
