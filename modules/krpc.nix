{ lib, buildPythonPackage, fetchPypi, setuptools, certifi, protobuf }:

buildPythonPackage rec {
  pname = "krpc";
  version = "0.5.3";

  src = fetchPypi {
    inherit pname version;
    # PASTE YOUR HASH HERE
    hash = "sha256-YOUR_GENERATED_HASH_HERE"; 
  };

  nativeBuildInputs = [ setuptools ];
  propagatedBuildInputs = [ certifi protobuf ];

  doCheck = false; # Tests require a connection to a running KSP game

  meta = with lib; {
    description = "Control Kerbal Space Program via RPC";
    homepage = "https://krpc.github.io/krpc/";
    license = licenses.mit;
  };
}
