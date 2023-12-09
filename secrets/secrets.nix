let
  user1 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID6pr4scU4r6j2BSTUve5TURkECF2Jy+GUr4QvRbB2cq";
  users = [ user1 ];
  systems = [  ];
in
{
  "mcUrl.age".publicKeys = users;
  "gcUrl.age".publicKeys = users;
}
