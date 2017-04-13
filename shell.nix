with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
	matching = self.callPackage ./. {};
      };
   };

in modifiedHaskellPackages.matching.env
