function getCookie(cname) {
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for(var i = 0; i <ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0)==' ') {
            c = c.substring(1);
        }
        if (c.indexOf(name) == 0) {
            return c;
        }
    }
    return "";
} 
document.write('<script type="text/javascript">\n var '+unof.cv.tools.LWZ().decompress(localStorage.cookieCMParams) +'\n</script>');