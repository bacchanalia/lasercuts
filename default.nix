let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  flake-compat = with lock.nodes.flake-compat.locked;
    fetchTarball { inherit url; sha256 = narHash; };
in
(import flake-compat { src = ./.; }).defaultNix
