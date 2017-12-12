var child_process = require('child_process');

function getStdout(cmd) {
  var stdout = child_process.execSync(cmd);
  return stdout.toString().trim();
}

exports.host = "imap.gmail.com";
exports.port = 993;
exports.tls = true;
exports.username = "rasen.dubi@gmail.com";
exports.password = getStdout("pass imap.gmail.com/rasen.dubi@gmail.com");
exports.onNotify = "mbsync sync-gmail";
// exports.onNotifyPost = {
//   "mail": "emacsclient  -e '(gnus-group-get-new-news)'",
//   "update": "emacsclient  -e '(gnus-group-get-new-news)'",
//   "expunge": "emacsclient  -e '(gnus-group-get-new-news)'",
// };
exports.boxes = [ "INBOX" ];
