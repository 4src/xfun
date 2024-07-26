NR==1 {  
  print "<!DOCTYPE html>"
  print "<html lang=\"en\">"
  print "<head>"
  print "<title>"FILENAME"</title>"
  print "<meta charset=\"UTF-8\">"
  print "<link rel=\"icon\" type=\"image/x-icon\" href=\"favicon.ico\">"
  print "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/vs.min.css\">"
  print "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js\"></script>"
  print "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/python.min.js\"></script>"
  print "<script>hljs.highlightAll();</script>"
  print "<link rel=\"stylesheet\" href=\"style.css\">"
  print "</head><body>"
  print "<small><p align=\"right\"><a href=\"home\">home</a> :: <a href=\"issues\">issues</a> :: <a href=\"license\">license</a>"
  print ":: <a href=\"home\">home</a> :: <a href=\"issues\">issues</a> :: <a href=\"license\">license</a></p></small>"
  print "<h1>"FILENAME"<img align=right width=200 src=logo.png></h1>"
}
BEGIN     { Start=1 }
NR< 3     { next }
/^###/    { if (!Start) printf "\n</code></pre>"
            Start = 0
            slurp($2 ".html")
            printf "<pre><code>" ; next }
1
END { print "</body></html>" }

function slurp(s) {
  while ((getline < s)> 0) { print $0 }
  close(s) } 
