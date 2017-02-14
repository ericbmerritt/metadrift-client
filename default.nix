{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  compiler = "ghc801";

  aeson-diff = pkgs.haskell.lib.dontCheck pkgs.haskell.packages.${compiler}.aeson-diff;
  haskellPackages = pkgs.haskell.packages.${compiler};
in
    with haskellPackages; mkDerivation {
    pname = "metadrift-client";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [ wreq  aeson text aeson-diff optparse-applicative
                              optparse-generic split bytestring http-conduit
                              http-types yaml data-lens data-lens-template
                              containers MissingH time cryptohash byteable
                              base64-bytestring case-insensitive old-locale ];

     executableHaskellDepends = [ text hindent yaml filepath ];
     buildDepends = [ ];
     testHaskellDepends = [ doctest hlint ];
     homepage = "https://github.com/ericbmerritt/metadrift-client";
     description = "metadrift client package";
     license = nixpkgs.stdenv.lib.licenses.gpl3;
}
