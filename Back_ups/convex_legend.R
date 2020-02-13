convex_legend <- function(){
g <- fetchFirstSelectedStoredIgraph()
if (is.null(g)) 
  return()
my_network<- as.data.frame(get.edgelist(g))
my_network<- data.frame(from = my_network$V1, to = my_network$V2)

gName <- SelectedStoredNets()$name

annoation_graph <- fetchFirstSelectedStoredGroups()
if (is.null(annoation_graph)) 
  return()
annotName <- SelectedStoredAnnots()$name
annoation_graph <- as.data.frame(annoation_graph)
groups<-annoation_graph

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
if(length(nodes_with_NA_groups)>0){
  for (i in 1:length(nodes_with_NA_groups))
  {
    #print(nodes_with_NA_groups[i])
    members_with_NA_groups[nrow(members_with_NA_groups)+1,1] <- nodes_with_NA_groups[i]
  }
  members_with_NA_groups<-unique(members_with_NA_groups)
}

edge <- data_frame(from = my_network$from, to = my_network$to, group = NA) #edge --> not edges

within_group_edges <- members %>%
  split(.$group) %>%
  map_dfr(function (grp) {
    if(length(grp$id)>=2){
      id2id <- combn(grp$id, 2)
      data_frame(from = id2id[1,],
                 to = id2id[2,],
                 group = unique(grp$group))
    }
  })

group_ids <- lapply(members_with_NA_groups %>% split(.$group), function(grp) { grp$id })
virt_group_nodes <- length(members_with_NA_groups$id) + 1:number_of_groups
names(virt_group_nodes) <- c(letters[1:number_of_groups])
edges_virt <- data_frame(from = edge$from, to = edge$to, weight = 5, group = edge$group)

within_virt <- members %>% split(.$group) %>% map_dfr(function (grp) {
  group_name <- unique(grp$group)
  virt_from <- rep(virt_group_nodes[group_name], length(grp$id))
  if(length(grp$id)>=2){
    id2id <- combn(grp$id, 2)
    data_frame(
      from = c(id2id[1,], virt_from),
      to = c(id2id[2,], grp$id),            # also connects from virtual_from node to each group node
      weight = c(rep(0.1, ncol(id2id)),     # weight between group nodes
                 rep(50, length(grp$id))),
      to_be_deleted = c(rep(T, ncol(id2id)),     # weight between group nodes
                        rep(T, length(grp$id))), # weight that 'ties together' the group (via the virtual group node)
      group = group_name
    )
  }
})


edges_virt <-bind_rows(mutate_all(edges_virt, as.character), mutate_all(within_virt, as.character)) # vgazei 38,39,40
virt_group_na <- virt_group_nodes[is.na(names(virt_group_nodes))]
non_group_nodes <- (members_with_NA_groups %>% filter(is.na(group)))$id
nodes_virt <- data_frame(id = 1:(length(members_with_NA_groups$id) + length(virt_group_nodes)),
                         is_virt = c(rep(FALSE, length(members_with_NA_groups$id)),
                                     rep(TRUE, length(virt_group_nodes))))

nodes_virt[1:length(members_with_NA_groups$id),]<- paste(members_with_NA_groups$id) #replace with the right names from our network
nodes_virt<- unique(nodes_virt)
g_virt <- graph_from_data_frame(edges_virt,
                                directed = FALSE,
                                vertices = nodes_virt)

# use "auto layout"
lay <- layout_nicely(g_virt)

# remove virtual group nodes from graph
g_virt <- delete_vertices(g_virt,which(nodes_virt$is_virt == T ))

# remove virtual group nodes' positions from the layout matrix
tmp<-which(nodes_virt$is_virt == T )

lay <- lay[-tmp, ]
g_virt=delete.edges(g_virt, which(E(g_virt)$to_be_deleted==T))

#Plot
group_color <- c(group_palette(length(group_ids)), "white")
group_color_fill <- adjustcolor(group_color, alpha.f = 0.3)

n<- length(unique(members_with_NA_groups$group))

legendDataFrame <- data.frame(stringsAsFactors=FALSE, Group = unique(members_with_NA_groups$group), Colour = group_color[1:n])

# identify(input$plot_hover$x,input$plot_hover$y, labels = members_with_NA_groups$id, n= length(members_with_NA_groups$id), plot = TRUE, atpen = T)
# plot(g_virt,vertex.size=30, edge.color = "grey50", identify(plot_hover$x, plot_hover$y))


# paste0(input$plot_hover$x, " " , input$plot_hover$y)
# str(shiny:::scaleCoords(input$plot_hover$x, input$plot_hover$y,  = ))


# p <- plot_ly(members_with_NA_groups, x = ~id, y = ~group, type = 'scatter', mode = 'markers',
#              hoverinfo = 'text', text = ~paste('Label ', members_with_NA_groups$id))



# outfile <- tempfile(fileext = ".png")
# png(outfile)
# # s = input$chooseGroups_rows_selected
# source("convex_hullInput.R", local = T)
# convexInput()
# dev.off()


# vs <- V(g_virt)
# es <- as.data.frame(get.edgelist(g_virt))
# 
# Nv <- length(vs)
# Ne <- length(es[1]$V1)
# 
# Xn <- lay[,1]
# Yn <- lay[,2]
# 
# 
# 
# edge_shapes <- list()
# for(i in 1:Ne) {
#   v0 <- es[i,]$V1
#   v1 <- es[i,]$V2
#   
#   edge_shape = list(
#     type = "line",
#     line = list(color = "#030303", width = 0.3),
#     x0 = Xn[v0],
#     y0 = Yn[v0],
#     x1 = Xn[v1],
#     y1 = Yn[v1]
#   )
#   
#   edge_shapes[[i]] <- edge_shape
# }
# 
# axis <- list(title = " ", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
# 
# pen <- png::readPNG("Selected_Groups2.png")
# 
# plot_ly(x = ~Xn, y = ~Yn, mode = "markers", text = names(vs), hoverinfo = "text") %>%
#   layout(title = '',
#          shapes = edge_shapes,
#          xaxis = axis,
#          yaxis = axis,
#          images = list(
#            source = raster2uri(as.raster(pen)),
#            x = 0.5, y = 0.5,
#            sizex = 0.2, sizey = 0.1,
#            xref = "x", yref = "y",
#            # sizing = "stretch",
#            opacity = 0.8,
#            layer = "below",
#            xanchor = "left", yanchor = "bottom"
#          ),
#          margin = list(t = 50)
#   )
# 

# p <- layout(
#   network,
#   title = '',
#   shapes = edge_shapes,
#   xaxis = axis,
#   yaxis = axis
# )
# 
# p

members_with_NA_groups

}