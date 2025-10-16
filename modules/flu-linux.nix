{ pkgs ? import <nixpkgs> {} }:

let
  myLinuxSrc = pkgs.fetchFromGitHub {
    owner = "Flu";
    repo = "flu-linux";
    rev = "7ea30958b3054f5e488fa0b33c352723f7ab3a2a";
    sha256 = "0000000000000000000000000000000000000000000000000000";
  };
  
  flu-linux = pkgs.buildLinux {
    name = "flu-linux";
    src = myLinuxSrc;
    config = "./.config";
    modules = [ ];
  };
in {
  kernelPackages = {
    kernel = flu-linux;
    headers = flu-linux;
    modules = [];
  };
}
