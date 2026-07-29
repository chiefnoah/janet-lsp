{
  description = "Language server for the Janet programming language";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { self, nixpkgs }:
    let
      forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
      project = builtins.replaceStrings [ "\n" ] [ " " ] (builtins.readFile ./project.janet);
      versionMatch = builtins.match ".*:version \"([^\"]+)\".*" project;
      version = builtins.elemAt versionMatch 0;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          janet-lsp = pkgs.callPackage ./nix/package.nix {
            src = self;
            inherit version;
            commit = self.shortRev or self.dirtyShortRev or "unknown";
          };
        in
        {
          default = janet-lsp;
          inherit janet-lsp;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.janet
              pkgs.jpm
            ];
            shellHook = ''
              export JANET_TREE="$PWD/jpm_tree"
              export JANET_PATH="$JANET_TREE/lib"
            '';
          };
        }
      );
    };
}
