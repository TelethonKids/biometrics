function navbar() {
  var x = document.getElementById("toc").innerHTML.replace(/(\r\n\t|\n|\r\t)/gm, "").replace(/>\s+</g, "><");
  document.getElementById("toc").innerHTML = "";
  var img = document.getElementById("nav_logo").innerHTML;
	var parser = new DOMParser();
	var xmlDoc = parser.parseFromString(x, "text/html");
	x = xmlDoc.getElementsByTagName("ul")[0].childNodes;
	var out = "<nav class='navbar navbar-expand-lg navbar-toggleable bg-dark navbar-dark sticky-top'>" +
    "<a class='navbar-brand' href='https://www.telethonkids.org.au' target='_blank'>"+ img +"</a>" +
    "<button class='navbar-toggler navbar-toggler-right' type='button' data-toggle='collapse' data-target='#navbarNavDropdown' aria-controls='navbarNavDropdown' aria-expanded='false' aria-label='Toggle navigation'>" +
      "<span class='navbar-toggler-icon'></span>" +
    "</button>" +
    "<div class='collapse navbar-collapse' id='navbarNavDropdown'>" +
    "<ul class='navbar-nav mr-auto'>";
	for (var i = 0; i < x.length ; i++) {
	  if (x[i].childNodes.length == 1) {
	      out += "<li class='nav-item'><a class='nav-link' href='" + x[i].childNodes[0].getAttribute("href") + "'>" + x[i].childNodes[0].childNodes[0].nodeValue + "</a></li>";
	  } else {
		  y = x[i].getElementsByTagName("ul")[0].childNodes;
      out += "<li class='nav-item dropdown'>" +
    		"<a class='nav-link dropdown-toggle' href='#' id='navbarDropdownMenuLink' role='button' data-toggle='dropdown' aria-haspopup='true' aria-expanded='false'>" +
    		  x[i].childNodes[0].childNodes[0].nodeValue +
    		"</a>" +
    		"<div class='dropdown-menu' aria-labelledby='navbarDropdownMenuLink'>" +
        "<a class='dropdown-item' href='" + x[i].childNodes[0].getAttribute("href") + "'>" + x[i].childNodes[0].childNodes[0].nodeValue + "</a>" +
        "<div class='dropdown-divider'></div>";
		    for (var j = 0; j < y.length; j++) {
		      out += "<a class='dropdown-item' href='" + y[j].childNodes[0].getAttribute("href") + "'>" + y[j].childNodes[0].childNodes[0].nodeValue + "</a>";
		    }
    		out += "</div></li>";
      }
  	}
	out += "</ul></div></nav>";
  document.getElementById("nav").innerHTML = out;
	return;
}
