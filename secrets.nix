# Auto-generated from README.org
let
  systems = {
    pie = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHutBETZO1KDA1anRT/3GDKaeFaEmdAqh7FZ6TKnYNDT";
  };
  users = {
    yubikey1 = "age1yubikey1qvqpv4f2kvwxzdf4rm349za00jayxhk23ads42l92sjywlcwce20yhm6pm2";
    bayraktar = "age1kmlska3klt5ca98chxm0xu2q2rc5x2c3agkut6303ajg35rreqes7qg3yd";
  };
in {
  "secrets/wireless_secrets.age".publicKeys = [
    users.yubikey1 users.bayraktar  # for editing
    systems.pie
  ];
}
