let
  user1 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAdJcJ2IxX5QdkZSXiQgCwOnBVsU2oXJCmUmPxUxhixW";
  users = [ user1 ];
  systems = [  ];
  urls = [ "mcUrl.age" "gcUrl.age" "muwhpc.age" "cemm.age" "wireguard_server_private_key.age" ];
in builtins.listToAttrs (map (url: { name = url; value = { inherit url; publicKeys = users; }; }) urls)

