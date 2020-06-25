document.addEventListener('DOMContentLoaded', function() {
	var selector = document.getElementById("btnAddNetwork");
		selector.setAttribute("data-step", "1");
		selector.setAttribute("data-intro", "Press this button to upload your network file");
		selector.setAttribute("data-position", "bottom");
		selector = document.getElementById("btnAddNetwork2");
		selector.setAttribute("data-step", "2");
		selector.setAttribute("data-intro", "Press this button to upload your annotation file");
		selector.setAttribute("data-position", "bottom");
		selector = document.getElementById("btnAddExpression");
		selector.setAttribute("data-step", "3");
		selector.setAttribute("data-intro", "Press this button to upload your expression file");
		selector.setAttribute("data-position", "bottom");

	
	document.getElementById("introButton").onclick = function(){
		introJs().start();
		//console.log("SKATA");
	};
}, false);
