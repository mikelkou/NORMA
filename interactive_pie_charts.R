pie_charts<- function(){
  
  qual_col_pals<-c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D",
                   "#666666","#7FC97F","#BEAED4","#FDC086","#FFFF99","#386CB0","#F0027F","#BF5B17","#A6CEE3","#1F78B4",
                   "#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#B15928","#FBB4AE","#B3CDE3",
                   "#CCEBC5","#DECBE4","#FED9A6","#FFFFCC","#E5D8BD","#FDDAEC","#F2F2F2","#B3E2CD","#FDCDAC","#CBD5E8","#F4CAE4","#E6F5C9",
                   "#FFF2AE","#F1E2CC","#CCCCCC","#E41A1C","#377EB8","#4DAF4A","#984EA3","#FFFF33","#A65628","#F781BF","#999999","#66C2A5",
                   "#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3","#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3",
                   "#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#095F02")
  
  g <- fetchFirstSelectedStoredIgraph()
  if (is.null(g)) 
    return()
  dataset1<- get.edgelist(g)
  # print(dataset1)
  
  my_network<- as.data.frame(get.edgelist(g))
  my_network<- data.frame(from = my_network$V1, to = my_network$V2)
  
  gName <- SelectedStoredNets()$name
  
  annoation_graph <- fetchFirstSelectedStoredGroups()
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
  
  source("pie_charts(stableInput).R", local = T)
  lay<-pie_chartsInput()
  
  
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
  
  fileConn <- file("output2.html", "w")
  cat(sprintf("<!DOCTYPE html>
<head>
  <meta charset=\"utf-8\">
  <script src=\"https://cdnjs.cloudflare.com/ajax/libs/d3/3.4.11/d3.min.js\"></script>
  <style>
    .node {
            stroke: white;
			stroke-width: 3;
        }

	.nodelabel {
	  font-family: \"arial\";
	}

	.link {
	  stroke: #999;
	  stroke-opacity: .6;
	}
  </style>
</head>

<body>
  <script>
    //arrow keys for svg pan
		document.onkeydown = function(e) {
			e = e || window.event;
			switch(e.which || e.keyCode) {
			case 37: // left
			var gg = document.getElementsByTagName(\"g\")[0];
			var transform_attribute = gg.getAttribute(\"transform\"); //example: \"translate(-141.0485937362168,-100.78113920140399) scale(0.5612310558128654)\"
			var transform_attribute_array = transform_attribute.split(\"(\");
			var transform_attribute_array2 = transform_attribute_array[1].split(\",\");
			var scale_x = parseFloat(transform_attribute_array2[0]) - 10
			var set_transform = transform_attribute_array[0].concat(\"(\", scale_x, \",\", transform_attribute_array2[1], \"(\", transform_attribute_array[2]); 
			gg.setAttribute(\"transform\", set_transform);
			//alert(\"left pressed\");
			break;

			case 38: // up
			var gg = document.getElementsByTagName(\"g\")[0];
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
			var gg = document.getElementsByTagName(\"g\")[0];
			var transform_attribute = gg.getAttribute(\"transform\"); //example: \"translate(-141.0485937362168,-100.78113920140399) scale(0.5612310558128654)\"
			var transform_attribute_array = transform_attribute.split(\"(\");
			var transform_attribute_array2 = transform_attribute_array[1].split(\",\");
			var scale_x = parseFloat(transform_attribute_array2[0]) + 10
			var set_transform = transform_attribute_array[0].concat(\"(\", scale_x, \",\", transform_attribute_array2[1], \"(\", transform_attribute_array[2]); 
			gg.setAttribute(\"transform\", set_transform);
			//alert(\"right pressed\");
			break;
			
			case 40: // down
			var gg = document.getElementsByTagName(\"g\")[0];
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
		
		var graph = { 	\"nodes\":[\n"), file = fileConn)
		
  if(length(nodes_with_NA_groups)>0){
    for (i in 1:length(nodes_with_NA_groups))
    {
      members_with_NA_groups[nrow(members_with_NA_groups)+1,1] <- nodes_with_NA_groups[i]
    }
    members_with_NA_groups<-unique(members_with_NA_groups)
  }
  # print(members_with_NA_groups)
  nodes <- unique(members_with_NA_groups$id)
  # nodes_no_NAs<- na.omit(nodes)
  
  annots <- na.omit(members_with_NA_groups)
  annots<- unique(annots$group)

  members_with_zeros<- as.matrix(members_with_NA_groups)
  members_with_zeros[is.na(members_with_zeros)] <- 0
  members_with_zeros<- as.data.frame(members_with_zeros)
  print(members_with_zeros)
  
  groupss <- members_with_zeros %>% 
    group_by(id) %>% 
    summarise_all(funs(trimws(paste(., collapse = ','))))
  groupss <- inner_join(members_with_zeros, groupss, by = "id")
   # groupss<- na.omit(groupss)
  # print(groupss)
  groupss <-data.frame(groupss[,1],groupss[,3])
  groupss<-groupss[!duplicated(groupss[,1]), ]
  colnames(groupss)<- c("V1", "V2")
  groupss <- as.data.frame(groupss)
  groupss_as_charachter<- as.character(groupss$V2)
  
  
  Groupss <- strsplit(groupss_as_charachter, ",")
  # print(groupss)
  # print("----")
  # print(Groupss)
  # print(nodes)
  # print(length(nodes))

  # minx<-min(lay[,1])
  # maxx<-max(lay[,1])
  # miny<-min(lay[,2])
  # maxy<-max(lay[,2])
  
  for (i in 1:length(nodes)){
    # coor_x<-mapper(lay[i,1], minx, maxx, 25, 575)
    # coor_y<-mapper(lay[i,2], miny, maxy, 25, 575)
    cat(sprintf(paste("{\"id\":", i-1, ",name:\"", nodes[i],"\",\"propertyValue\":3,'x':", sample(0:600, 1) , ", 'y':", sample(0:600, 1), ", 'fixed': true,\"proportions\": [\n",sep="")), file = fileConn)
    print(length(Groupss[[i]]))
   for (j in 1:length(Groupss[[i]])) {
       if(j<length(Groupss[[i]])){
       cat(sprintf(paste("{\"group\":", which(annots %in% Groupss[[i]][j]), "," , "\"value\": 1},\n")), file = fileConn)
       }
     if(Groupss[[i]]=="0"){
       cat(sprintf(paste("{\"group\": 0," , "\"value\": 1}]},\n")), file = fileConn)
     }
     if(j==length(Groupss[[i]]) & Groupss[[i]]!="0"){
       cat(sprintf(paste("{\"group\":", which(annots %in% Groupss[[i]][j]), "," , "\"value\": 1}]},\n")), file = fileConn)
       }
   }
  }

  cat(sprintf("],\n
						\"links\":[\n"), file = fileConn)
  for (i in 1:nrowdat){
    cat(sprintf(paste("{\"source\":", which(nodes %in% dataset1[i,1])-1, ",\"target\":", which(nodes %in% dataset1[i,2])-1, "},\n",sep="")), file = fileConn
    )}
  cat(sprintf(
    "]
    };\n "), file = fileConn)
		
  cat(sprintf("var width = 960,
			height = 500,
			radius = 25,
			color = d3.scale.category10();
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

		var pie = d3.layout.pie()
			.sort(null)
			.value(function(d) { return d.value; });

		var arc = d3.svg.arc()
			.outerRadius(radius)
			.innerRadius(0);
		
		var zoomFlag = 0;
		
		var svg = d3.select(\"body\").append(\"svg\")
			.attr(\"width\", width)
			.attr(\"height\", height)
			//.style(\"border\", \"4px solid black\")
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
		  .append(\"g\")
			.attr(\"transform\", \"translate(5.684341886080802e-14,5.684341886080802e-14) scale(0.9999999999999999)\");
		  
		var g = d3.select(\"g\");

		var force = d3.layout.force()
			.charge(-120)
			.linkDistance(100)
			.size([width, height]);

		force.nodes(graph.nodes)
			 .links(graph.links)
			 .start();

		var link = svg.selectAll(\".link\")
			.data(graph.links)
			.enter().append(\"line\")
			.attr(\"class\", \"link\");

		var node = svg.selectAll(\".node\")
			.data(graph.nodes)
			.enter().append(\"g\")
			.attr(\"class\", \"node\")
			.style(\"stroke\", function(d) { return color(d.color_value); })
			.call(force.drag);
			
		var nodelabels = svg.selectAll(\".nodelabel\") 
			.data(graph.nodes)
			.enter()
			.append(\"text\")
			.attr({\"x\":function(d){return d.x;},
				  \"y\":function(d){return d.y;},
				  \"class\":\"nodelabel\",
				  \"stroke\":\"black\"})
			.text(function(d){return d.name;});
			   
		node.selectAll(\"path\")
			.data(function(d, i) {return pie(d.proportions); })
			.enter()
			.append(\"svg:path\")
			.attr(\"d\", arc)
			.attr(\"fill\", function(d, i) { return color(d.data.group); });;

		force.on(\"tick\", function() {
			link.attr(\"x1\", function(d) { return d.source.x; })
			.attr(\"y1\", function(d) { return d.source.y; })
			.attr(\"x2\", function(d) { return d.target.x; })
			.attr(\"y2\", function(d) { return d.target.y; });

			node.attr(\"x\", function(d) { return d.x; })
				.attr(\"y\", function(d) { return d.y; })
				.attr(\"transform\", function(d) { return \"translate(\" + d.x + \",\" + d.y + \")\"});
				
			nodelabels.attr(\"x\", function(d) { return d.x; }) 
				.attr(\"y\", function(d) { return d.y; });
		});
  </script>
</body>
              "), file = fileConn)
  
  close(fileConn)
  }#if (length(s)) 
}#function