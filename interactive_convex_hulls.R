convex_hulls<- function(){
  qual_col_pals<-c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D",
                   "#666666","#7FC97F","#BEAED4","#FDC086","#FFFF99","#386CB0","#F0027F","#BF5B17","#A6CEE3","#1F78B4",
                   "#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#B15928","#FBB4AE","#B3CDE3",
                   "#CCEBC5","#DECBE4","#FED9A6","#FFFFCC","#E5D8BD","#FDDAEC","#F2F2F2","#B3E2CD","#FDCDAC","#CBD5E8","#F4CAE4","#E6F5C9",
                   "#FFF2AE","#F1E2CC","#CCCCCC","#E41A1C","#377EB8","#4DAF4A","#984EA3","#FFFF33","#A65628","#F781BF","#999999","#66C2A5",
                   "#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3","#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3",
                   "#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#095F02")
  
  g <- fetchFirstSelectedStoredIgraph_annotations_tab()
  if (is.null(g)) 
    return()
  dataset1<- get.edgelist(g)
  # print(dataset1)
  
  my_network<- as.data.frame(get.edgelist(g))
  my_network<- data.frame(from = my_network$V1, to = my_network$V2)
  
  gName <- SelectedStoredNets()$name
  
  annoation_graph <- fetchFirstSelectedStoredGroups2_annotations_tab()
  if (is.null(annoation_graph)) 
    return()
  annotName <- SelectedStoredAnnots()$name
  annoation_graph <- as.data.frame(annoation_graph)
  groups<-annoation_graph
  
  annotation1<- groups
  
  groups<- data.frame(V1 = groups$Annotations, stri_split_fixed(groups$Nodes, ",",  simplify = TRUE))
  groups<-mutate_all(groups, funs(na_if(.,"")))
  number_of_groups<-dim(groups)[1]
  #paste("Number of nodes in", gName, " is ", " and the annotation file is : ", annotName)
  
  x <- list()
  for (i in 1:number_of_groups) {
    group_i<- groups[i,]
    group_i<- group_i[,-1]
    group_i <- group_i[!is.na(group_i)]
    x[[i]]<- (group_i)
  }
  
  GO <- list()
  for (i in 1:number_of_groups) {
    GO[[i]]<-rep(groups[i,1], length(x[[i]]))
  }
  
  column1<-my_network$from
  column2<-my_network$to
  node_names<-unique(union(column1, column2))
  tt<-unlist(x)
  nodes_with_NA_groups<-setdiff(node_names,tt)
  
  members <- data_frame(id=unlist(x),group = unlist(GO))
  members_with_NA_groups <- data_frame(id=unlist(x),group = unlist(GO))
  # print(members_with_NA_groups)
  
  # members <- unique(rbind(as.matrix(dataset1[,1]), as.matrix(dataset1[,2])))
  # print(members)
  
  dataset1 <- as.matrix(dataset1)
  annotation1 <- as.matrix(annotation1)
  
  nrowdat <- nrow(dataset1)
  nrowannot <- nrow(annotation1)
  
 
  source("convex_hullInput.R", local = T)
  lay<-convexInput()
  # print(lay)
  
  
  fileConn <- file("output.html", "w")
  
  if (length(s)==0)
  {
    s<-c(1:nrowannot)
  }
  
  if (length(s)) {
    s<-sort(s)#-----------------------------------
    x<- length(s)
    # print(s)
    # print(x)
    ccc<-group_pal_rows(length(x))
    tmp_selected_colors<- c()
    
      tmp_selected_colors<- c(tmp_selected_colors, ccc[s[i]])
      group_color <- tmp_selected_colors
      group_color_fill <- adjustcolor(group_color, alpha.f = 0.2)
      
  cat(sprintf(paste("<!DOCTYPE html>
  <meta charset=\"utf-8\">
              
  <!-- Load d3.js -->
  <!--<script type=\"text/javascript\" src=\"d3.js\"></script>-->
  <script src=\"https://cdnjs.cloudflare.com/ajax/libs/d3/3.4.11/d3.min.js\"></script>
  
  <style>
  .node {
    stroke: #fff;
    stroke-width: 1.5px;
  }
  
  .nodelabel {
  font-family: \"arial\";
  font-size: ",scaling_labels_convex(), "px;
  }
  
  .link {
    stroke: #999;
    stroke-opacity: .6;
  }
  

  </style>
  	
  <body>
  	<script>
  	//arrow keys for svg pan
  	document.onkeydown = function(e) {
		  e = e || window.event;
		  switch(e.which || e.keyCode) {
		  case 37: // left
			var gg = document.getElementsByTagName(\"svg\")[0];
			var transform_attribute = gg.getAttribute(\"transform\"); //example: \"translate(-141.0485937362168,-100.78113920140399) scale(0.5612310558128654)\"
			var transform_attribute_array = transform_attribute.split(\"(\");
      var transform_attribute_array2 = transform_attribute_array[1].split(\",\");
			var scale_x = parseFloat(transform_attribute_array2[0]) - 10
			var set_transform = transform_attribute_array[0].concat(\"(\", scale_x, \",\", transform_attribute_array2[1], \"(\", transform_attribute_array[2]); 
			gg.setAttribute(\"transform\", set_transform);
			//alert(\"left pressed\");
			break;
			case 38: // up
			var gg = document.getElementsByTagName(\"svg\")[0];
			var transform_attribute = gg.getAttribute(\"transform\"); //example: \"translate(-141.0485937362168,-100.78113920140399) scale(0.5612310558128654)\"
			var transform_attribute_array = transform_attribute.split(\"(\");
			var transform_attribute_array2 = transform_attribute_array[1].split(\",\");
			var transform_attribute_array3 = transform_attribute_array2[1].split(\")\");
			var scale_y = parseFloat(transform_attribute_array3[0]) - 10
			var set_transform = transform_attribute_array[0].concat(\"(\", transform_attribute_array2[0], \",\", scale_y, \") scale(\", transform_attribute_array[2]); 
			gg.setAttribute(\"transform\", set_transform);
			//alert(\"up pressed\");
			break;
			
			case 39: // right
			var gg = document.getElementsByTagName(\"svg\")[0];
			var transform_attribute = gg.getAttribute(\"transform\"); //example: \"translate(-141.0485937362168,-100.78113920140399) scale(0.5612310558128654)\"
			var transform_attribute_array = transform_attribute.split(\"(\");
			var transform_attribute_array2 = transform_attribute_array[1].split(\",\");
			var scale_x = parseFloat(transform_attribute_array2[0]) + 10
			var set_transform = transform_attribute_array[0].concat(\"(\", scale_x, \",\", transform_attribute_array2[1], \"(\", transform_attribute_array[2]); 
			gg.setAttribute(\"transform\", set_transform);
			//alert(\"right pressed\");
			break;
			
			case 40: // down
			var gg = document.getElementsByTagName(\"svg\")[0];
			var transform_attribute = gg.getAttribute(\"transform\"); //example: \"translate(-141.0485937362168,-100.78113920140399) scale(0.5612310558128654)\"
			var transform_attribute_array = transform_attribute.split(\"(\");
			var transform_attribute_array2 = transform_attribute_array[1].split(\",\");
			var transform_attribute_array3 = transform_attribute_array2[1].split(\")\");
			var scale_y = parseFloat(transform_attribute_array3[0]) + 10
			var set_transform = transform_attribute_array[0].concat(\"(\", transform_attribute_array2[0], \",\", scale_y, \") scale(\", transform_attribute_array[2]); 
			gg.setAttribute(\"transform\", set_transform);
			//alert(\"down pressed\");
			break;
			
			default: return; // exit this handler for other keys
			}
			e.preventDefault(); // prevent the default action (scroll / move caret)
		}
  	
  	var width = 1000,
  		height = 600;
            
            
            // The color functions: in this example I'm coloring all the convex hulls at the same layer the same to more easily see the result.
		var color = d3.scale.category20();
              color(0);
              color(1);
              color(2);
              color(3);
              color(4);
              color(5);
              color(6);
              color(7);
              color(8);
              color(9);
			        color(10);
              color(11);
              color(12);
              color(13);
              color(14);
              color(15);
              color(16);
              color(17);
              color(18);
              color(19);
          ",sep="")), file = fileConn)
  
  for(i in 1:x){
    cat(sprintf(
      paste("groupHullColor", s[i], " = '", qual_col_pals[s[i]], "' ;\n", sep = "")), file = fileConn)
  }
  
  cat(sprintf("var force = d3.layout.force()
    .charge(-120)
    .linkDistance(30)
    .size([width, height]);

var zoomFlag = 0;

var zoomFlag = 0;
		var svg = d3.select(\"body\").append(\"svg\")
			.attr(\"width\", width)
			.attr(\"height\", height)
			.attr(\"transform\", \"translate(5.684341886080802e-14,5.684341886080802e-14) scale(0.9999999999999999)\") //TODO

			.call(d3.behavior.zoom().on(\"zoom\", function () {
			svg.attr(\"transform\", \"translate(\" + d3.event.translate + \")\" + \" scale(\" + d3.event.scale + \")\")
			//alert(d3.event.scale)
			if (!zoomFlag && d3.event.scale < 0.6){
				var nodelabels = g.selectAll(\".nodelabel\")
				nodelabels.text(\"\")
				zoomFlag = 1
			}
			if (zoomFlag && d3.event.scale > 0.6){
				var nodelabels = g.selectAll(\".nodelabel\")
				.text(function(d){return d.name;});
				zoomFlag = 0
			}
		  }))
		  
  .on(\"dblclick.zoom\", null)
  .on(\"mousedown.zoom\", null)
  .append(\"g\");
  
var g = d3.select(\"g\");

// propertyValue depicts the size of the node while value the edge width, 'fixed': true to disable bouncy physics
var theGraphData = {
\"nodes\":[\n"), file= fileConn)
  
  
  # members_with_NA_groups$id #<- parseMembers()# to do
  if(length(nodes_with_NA_groups)>0){
    for (i in 1:length(nodes_with_NA_groups))
    {
      members_with_NA_groups[nrow(members_with_NA_groups)+1,1] <- nodes_with_NA_groups[i]
    }
    members_with_NA_groups<-unique(members_with_NA_groups)
  }
  # print(members_with_NA_groups)
  nodes <- unique(members_with_NA_groups$id)
  # print(nodes)
  

  minx<-min(lay[,1])
  maxx<-max(lay[,1])
  miny<-min(lay[,2])
  maxy<-max(lay[,2])
  
  
  #### Expressions ####
  
  if (!is.null(getStoredExpressionChoices())){
    # expression<-read.delim("string_expression_colors.txt", header = F)
    expression<-fetchFirstSelectedStoredExpression()
    colnames(expression) <- c("id", "color")
    express_order<- as.data.frame(members_with_NA_groups)
    express_order<- as.data.frame(unique(express_order$id))
    colnames(express_order) <- "id"
    expression<-inner_join(express_order, expression, by = "id")
    expression$color<- as.character(expression$color)
    expression$color[which(expression$color=="blue")] <- "0"
    expression$color[which(expression$color=="orange")] <- "2"
    expression$color[which(expression$color=="green")] <- "4"
    expression$color[which(expression$color=="red")] <- "6"
    expression$color[which(expression$color=="purple")] <- "8"
    expression$color[which(expression$color=="gray")] <- "15"
    # print(expression)
  }
  
  if (is.null(getStoredExpressionChoices())){
      expression<- as.data.frame(members_with_NA_groups)
      expression<- as.data.frame(unique(expression$id))
      expression$color <- rep(c("15"))
      colnames(expression) <- c("id", "color")
  }
  
  
  
  ###################
  
  
  

    for (i in 1:length(nodes)){
      coor_x<-mapper(lay[i,1], minx, maxx, 25, 575)
      coor_y<-mapper(lay[i,2], miny, maxy, 25, 575)
      #sample(0:600, 1)
      if(expression_colors == T){
    cat(sprintf(paste("{\"id\":", i-1, ",name:\"", nodes[i],"\",\"propertyValue\":",scaling_nodes_convex(), ",'x':", coor_x*scaling_coordinates_convex() , ", 'y':", coor_y*scaling_coordinates_convex(), ", 'fixed': true, \"color_value\":", expression$color[i], "},\n",sep="")), file = fileConn)
      }
      
      if(expression_colors == F){
    cat(sprintf(paste("{\"id\":", i-1, ",name:\"", nodes[i],"\",\"propertyValue\":",scaling_nodes_convex(), ",'x':", coor_x*scaling_coordinates_convex() , ", 'y':", coor_y*scaling_coordinates_convex(), ", 'fixed': true, \"color_value\":", 15, "},\n",sep="")), file = fileConn)
      }
    }
  
  cat(sprintf("],
  \"links\":[\n"), file= fileConn)
  for (i in 1:nrowdat){
    cat(sprintf(paste("{\"source\":", which(nodes %in% dataset1[i,1])-1, ",\"target\":", which(nodes %in% dataset1[i,2])-1, ",\"value\":1},\n",sep="")), file = fileConn
    )}
  
  cat(sprintf(
    "]
}

graph = theGraphData

// The data for grouping nodes.
// The groups are not partitions, some nodes belong to more than one group.
var " 
  ), file = fileConn)
  
  for(i in 1:x){
    genes <- strsplit(annotation1[s[i], 2], ",")$Nodes
    
    if(i<x){
      cat(sprintf(paste("group",s[i]," = [[", sep="")), file = fileConn)
      for(j in 1:length(genes)){
        if(j<length(genes)){
        cat(sprintf(paste(which(nodes %in% genes[j])-1, ",", sep="")), file = fileConn)}
        if(j==length(genes)){
          cat(sprintf(paste(which(nodes %in% genes[j])-1, sep="")), file = fileConn)}
      }
      cat(sprintf("]],\n"), file = fileConn)
      
    }
    if(i==x)
    {
      cat(sprintf(paste("group",s[i]," = [[", sep="")), file = fileConn)
      for(j in 1:length(genes)){
        if(j<length(genes)){
          cat(sprintf(paste(which(nodes %in% genes[j])-1, ",", sep="")), file = fileConn)}
          if(j==length(genes)){
            cat(sprintf(paste(which(nodes %in% genes[j])-1, sep="")), file = fileConn)}
        
      }
      cat(sprintf("]];\n"), file = fileConn)      
    }
  }      

  ########################################
 
  
  for (i in 1:x){
    cat(sprintf(paste("var groupNodes", s[i], " = group", s[i], 
                      ".map(function(group", s[i], ",index){
  return group", s[i],".map(function(member){return graph.nodes[member] });
  });\n", sep="")), file = fileConn)
  }

  cat(sprintf("\nvar groupPath = function(d) {
    var fakePoints = [];  
    if (d.length == 1 || d.length == 2) {     // This adjusts convex hulls for groups with fewer than 3 nodes by adding virtual nodes.
       fakePoints = [ [d[0].x + 0.001, d[0].y - 0.001],[d[0].x - 0.001, d[0].y + 0.001],[d[0].x - 0.001, d[0].y + 0.001]]; }     
    d.forEach(function(element) { fakePoints = fakePoints.concat([   // \"0.7071\" is the sine and cosine of 45 degree for corner points.
           [(element.x), (element.y + (2 + (4 * element.propertyValue)))],
           [(element.x + 0.7071 * (2 + (4 * element.propertyValue))), (element.y + 0.7071 * (2 + (4 * element.propertyValue)))],
           [(element.x + (2 + (4 * element.propertyValue))), (element.y)],
           [(element.x + 0.7071 * (2 + (4 * element.propertyValue))), (element.y - 0.7071 * (2 + (4 * element.propertyValue)))],
           [(element.x), (element.y - (2 + (4 * element.propertyValue)))],
           [(element.x - 0.7071 * (2 + (4 * element.propertyValue))), (element.y - 0.7071 * (2 + (4 * element.propertyValue)))],
           [(element.x - (2 + (4 * element.propertyValue))), (element.y)],
           [(element.x - 0.7071 * (2 + (4 * element.propertyValue))), (element.y + 0.7071 * (2 + (4 * element.propertyValue)))]
    ]); })
    return \"M\" + d3.geom.hull( fakePoints ).join(\"L\") + \"Z\";
};\n"), file = fileConn)
  
  for (i in 1:x){
    cat(sprintf(paste("\nvar groupHullFill", s[i]," = function(d, i) { return groupHullColor", s[i],"; };",sep="")
    ), file = fileConn)
  }

  #####
  
  cat(sprintf(paste("\nforce
.nodes(graph.nodes)
.links(graph.links)
.start();

var link = g.selectAll(\".link\")
    .data(graph.links)
	.enter().append(\"line\")
    .attr(\"class\", \"link\")
    .style(\"stroke-width\", function(d) { return Math.sqrt(d.value); });

var node = g.selectAll(\".node\")
    .data(graph.nodes)
	.enter().append(\"circle\")
    .attr(\"class\", \"node\")
    .attr(\"r\", function(d) { return 2 + (4 * d.propertyValue); })
	  .style(\"fill\", function(d) { return color(d.color_value); })
    .style(\"stroke-width\", 1.5)
    .call(force.drag);
  
//node.append(\"title\")
//  .text(function(d) { return d.name; });
    
var nodelabels = svg.selectAll(\".nodelabel\") 
  .data(graph.nodes)
  .enter()
  .append(\"text\")
  .attr({\"x\":function(d){return d.x;},
        \"y\":function(d){return d.y;},
        \"class\":\"nodelabel\",
        \"stroke\":\"black\"})
  .text(function(d){return d.name;});

force.on(\"tick\", function() {
    // this updates the links, but they are UNDER the convex hulls because the hulls are recreated every tick
    link.attr(\"x1\", function(d) { return d.source.x; })
        .attr(\"y1\", function(d) { return d.source.y; })
        .attr(\"x2\", function(d) { return d.target.x; })
        .attr(\"y2\", function(d) { return d.target.y; });

    node.attr(\"cx\", function(d) { return d.x; })
        .attr(\"cy\", function(d) { return d.y; });
        
    nodelabels.attr(\"x\", function(d) { return d.x; }) 
        .attr(\"y\", function(d) { return d.y+3; }); 
                  
    // this updates the convex hulls
    g.selectAll(\"path\").remove()
    
                  ", sep="")), file = fileConn)
  for (i in 1:x){
    cat(sprintf(paste("\n
  g.selectAll(\"path#group", s[i],"\")
  .data(groupNodes", s[i], ")
  .attr(\"d\", groupPath)
  .enter().insert(\"path\", \"circle\")
  .style(\"fill\", groupHullFill",s[i], ")
  .style(\"stroke\", groupHullFill", s[i], ")
  .style(\"stroke-width\", 35)
  .style(\"stroke-linejoin\", \"round\")
  .style(\"opacity\", .2)
  .attr(\"ID\",\"group\")
  .attr(\"d\", groupPath);
", sep="")), file = fileConn)}
  
  cat(sprintf("\n });
 

	</script>
</body>\n"), file = fileConn)
  

  close(fileConn)
 
  }#if (length(s)) 
}#function
