$(document).ready(function(){ 

var color = d3.scale.linear().domain([-2, 4]).range(["#252525", "#cccccc"]),
    groupHullColor1 = "#e7c7c7";
	groupHullColor2 = "#ffa700";
	groupHullColor3 = "#9bc0ff";

var force = d3.layout.force()
    .charge(-120)
    .linkDistance(30)
    .size([width, height]);

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);


var theGraphData = {
"nodes":[
    {"id":0,"propertyValue":3,'x': 50, 'y': 50, 'fixed': true},
    {"id":1,"propertyValue":1,'x': 60, 'y': 160, 'fixed': true},
    {"id":2,"propertyValue":1.5,'x': 150, 'y': 150, 'fixed': true},
    {"id":3,"propertyValue":3,'x': 200, 'y': 300, 'fixed': true},
    {"id":4,"propertyValue":2,'x': 500, 'y': 300, 'fixed': true},
    {"id":5,"propertyValue":1,'x': 550, 'y': 350, 'fixed': true},
    {"id":6,"propertyValue":1.5,'x': 550, 'y': 150, 'fixed': true},
    {"id":7,"propertyValue":1,'x': 700, 'y': 200, 'fixed': true},
    {"id":8,"propertyValue":.75,'x': 400, 'y': 50, 'fixed': true},
],
"links":[
  {"source":2,"target":0,"value":1},
  {"source":2,"target":1,"value":8},
  {"source":8,"target":2,"value":10},
  {"source":3,"target":2,"value":6},
  {"source":4,"target":2,"value":1},
  {"source":3,"target":4,"value":1},
  {"source":3,"target":5,"value":6},
  {"source":4,"target":5,"value":1},
  {"source":4,"target":6,"value":1},
  {"source":6,"target":7,"value":1}
]
}

graph = theGraphData
	
var group1 = [[0,1,2]],
	group2 = [[3,4,5]],
	group3 = [[6,7]];

var groupNodes1 = group1.map(function(group1,index){
    	return group1.map(function(member){return graph.nodes[member] });
	});
var groupNodes2 = group2.map(function(group2,index){
    	return group2.map(function(member){return graph.nodes[member] });
	});
var groupNodes3 = group3.map(function(group3,index){
    	return group3.map(function(member){return graph.nodes[member] });
	});

var groupPath = function(d) {
    var fakePoints = [];  
    if (d.length == 1 || d.length == 2) {     
      fakePoints = [ [d[0].x + 0.001, d[0].y - 0.001],[d[0].x - 0.001, d[0].y + 0.001],[d[0].x - 0.001, d[0].y + 0.001]]; }     
    d.forEach(function(element) { fakePoints = fakePoints.concat([   
           [(element.x), (element.y + (2 + (4 * element.propertyValue)))],
           [(element.x + 0.7071 * (2 + (4 * element.propertyValue))), (element.y + 0.7071 * (2 + (4 * element.propertyValue)))],
           [(element.x + (2 + (4 * element.propertyValue))), (element.y)],
           [(element.x + 0.7071 * (2 + (4 * element.propertyValue))), (element.y - 0.7071 * (2 + (4 * element.propertyValue)))],
           [(element.x), (element.y - (2 + (4 * element.propertyValue)))],
           [(element.x - 0.7071 * (2 + (4 * element.propertyValue))), (element.y - 0.7071 * (2 + (4 * element.propertyValue)))],
           [(element.x - (2 + (4 * element.propertyValue))), (element.y)],
           [(element.x - 0.7071 * (2 + (4 * element.propertyValue))), (element.y + 0.7071 * (2 + (4 * element.propertyValue)))]
    ]); })
    return "M" + d3.geom.hull( fakePoints ).join("L") + "Z";
};

var groupHullFill1 = function(d, i) { return groupHullColor1; };
var groupHullFill2 = function(d, i) { return groupHullColor2; };
var groupHullFill3 = function(d, i) { return groupHullColor3; };

force
    .nodes(graph.nodes)
    .links(graph.links)
    .start();

var link = svg.selectAll(".link")
    .data(graph.links)
  .enter().append("line")
    .attr("class", "link")
    .style("stroke-width", function(d) { return Math.sqrt(d.value); });

var node = svg.selectAll(".node")
    .data(graph.nodes)
  .enter().append("circle")
    .attr("class", "node")
    .attr("r", function(d) { return 2 + (4 * d.propertyValue); })
    .style("fill", function(d) { return color(d.propertyValue); })
    .style("stroke-width", 1.5)
    .call(force.drag);

node.append("title")
    .text(function(d) { return d.name; });

force.on("tick", function() {
    
    link.attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });

    node.attr("cx", function(d) { return d.x; })
        .attr("cy", function(d) { return d.y; });
      
    svg.selectAll("path").remove()
    
    svg.selectAll("path#group1")
      .data(groupNodes1)
        .attr("d", groupPath)
      .enter().insert("path", "circle")
        .style("fill", groupHullFill1)
        .style("stroke", groupHullFill1)
        .style("stroke-width", 35)
        .style("stroke-linejoin", "round")
        .style("opacity", .2)
    	.attr("ID","group")
        .attr("d", groupPath);
		
	svg.selectAll("path#group2")
      .data(groupNodes2)
        .attr("d", groupPath)
      .enter().insert("path", "circle")
        .style("fill", groupHullFill2)
        .style("stroke", groupHullFill2)
        .style("stroke-width", 35)
        .style("stroke-linejoin", "round")
        .style("opacity", .2)
    	.attr("ID","group")
        .attr("d", groupPath);
		
	svg.selectAll("path#group3")
      .data(groupNodes3)
        .attr("d", groupPath)
      .enter().insert("path", "circle")
        .style("fill", groupHullFill3)
        .style("stroke", groupHullFill3)
        .style("stroke-width", 35)
        .style("stroke-linejoin", "round")
        .style("opacity", .2)
    	.attr("ID","group")
        .attr("d", groupPath);
});

    Shiny.onInputChange("count", n);
  )};