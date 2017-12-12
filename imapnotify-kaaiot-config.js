var child_process = require('child_process');

function getStdout(cmd) {
  var stdout = child_process.execSync(cmd);
  return stdout.toString().trim();
}

exports.host = "imap.gmail.com";
exports.port = 993;
exports.tls = true;
exports.username = "ashmalko@kaaiot.io";
exports.password = getStdout("pass imap.gmail.com/ashmalko@kaaiot.io");
exports.onNotify = "mbsync sync-kaaiot";
// exports.onNotifyPost = {
//   "mail": "emacsclient  -e '(gnus-group-get-new-news)'",
//   "update": "emacsclient  -e '(gnus-group-get-new-news)'",
//   "expunge": "emacsclient  -e '(gnus-group-get-new-news)'",
// };
exports.boxes = [ "INBOX" ];
