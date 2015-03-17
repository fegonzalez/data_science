// Hide email address from bots
// idName = id of <div> or <span> tag you where you want the email address to appear
// username = username@harding.edu
function DisplayEmail(idName, username)
{
    var end = "harding.edu";
    var addr = username + "@" + end;
    document.getElementById(idName).innerHTML = "<a href=mailto:" + addr + ">" + addr + "</a>";
}