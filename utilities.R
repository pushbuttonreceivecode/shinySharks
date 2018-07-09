#utilities...

#R does not have a "mode" function...
#oddly enough (for stats)
statsMode = function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#bootstrap wrappers...
danger = function(x){
  p(tags$em(x), class = "bg-danger", style="color:#000; font-weight:bold; padding:5px;")
}

warn = function(x){
  p(tags$b(tags$em(x)), class = "bg-warning", style="padding:5px;")
}

info = function(x){
  p(tags$b(x), class = "bg-info", style="padding:5px;")
}

infolink = function(x, label, link){
  p(tags$b(x), a(label, href=link), class = "bg-info", style="padding:5px;")
}
