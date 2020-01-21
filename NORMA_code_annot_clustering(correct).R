#install.packages('Rcpp')
library(Rcpp)
library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(RColorBrewer)
library(data.table)
library(networkD3)
library(stringi)
library(reshape2)
#Example-1
network_file<-"C:\\Users\\mikae\\Desktop\\MSc_Molecular_Biomedicine\\Master_Thesis_Pavlopoulos\\Application_note_2\\App\\string_interactions.txt"
GO_file<-"C:\\Users\\mikae\\Desktop\\MSc_Molecular_Biomedicine\\Master_Thesis_Pavlopoulos\\Application_note_2\\App\\string_interactions_groups_comma_duplicate.txt"

#Example-2
#network_file<-"PAP_example.txt"
#GO_file<-"PAP_david.txt" #without spaces between annotation's words

#Example-3
#network_file<-"PAP_example.txt"
#GO_file<-"PAP_david_tmp.txt" #with spaces between annotation's words

#choose_file
#network_file<- choose.files()
#GO_file<- choose.files()

  
my_network<- read.delim(network_file, header= T)
m<- as.matrix(my_network) # coerces the data set as a matrix
g=graph.edgelist(m,directed=FALSE) # turns the edgelist into a 'graph object'

groups<- read.delim(GO_file, header= F, sep = "\t", na.strings=c("","NA")) #replace spaces with NAs
groups<- data.frame(V1 = groups$V1, 
           stri_split_fixed(groups$V2, ",", 
                            simplify = TRUE))
groups<-mutate_all(groups, funs(na_if(.,"")))

number_of_groups<-dim(groups)[1]

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
node_names
tt<-unlist(x)
nodes_with_NA_groups<-setdiff(node_names,tt)
nodes_with_NA_groups

members <- data_frame(id=unlist(x),group = unlist(GO))
members_with_NA_groups <- data_frame(id=unlist(x),group = unlist(GO))
members
if(length(nodes_with_NA_groups)>0){
for (i in 1:length(nodes_with_NA_groups))
{
  #print(nodes_with_NA_groups[i])
  members_with_NA_groups[nrow(members_with_NA_groups)+1,1] <- nodes_with_NA_groups[i]
}
  members_with_NA_groups<-unique(members_with_NA_groups)
}



edge <- data_frame(from = my_network$from, to = my_network$to, group = NA) #edge --> not edges

members
members_with_NA_groups

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

# group_ids <- lapply(members_with_NA_groups %>% split(.$group), function(grp) { grp$id })
# group_ids

group_order<-(as.list(unique(members_with_NA_groups$group)))
EE <- new.env(hash = TRUE)
for(i in 1: length(group_order))
{
  group_name_as_key<-group_order[[i]]
  EE[[ as.character(group_name_as_key) ]]<-i
}
for(i in 1: length(group_order))
{
  group_name_as_key<-group_order[[i]]
  index<-EE[[ as.character(group_name_as_key) ]]
}


group_ids_tmp <- lapply(members_with_NA_groups %>% split(.$group), function(grp) { grp$id })
group_ids<-c()
for(i in 1: length(group_ids_tmp))
{
  group_name_as_key<-names(group_ids_tmp[i])
  index<-EE[[group_name_as_key]]
  group_ids<-c(group_ids, group_ids_tmp[index])
}
####################################
#############################################################

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

# non_group_nodes
# virt_group_na
# #edges_na_group_virt <- data_frame(from = non_group_nodes,
#                                   to = rep(virt_group_na,
#                                            length(non_group_nodes)),
#                                   weight = 10,
#                                   group = NA)

#edges_virt <-bind_rows(mutate_all(edges_virt, as.character), mutate_all(edges_na_group_virt, as.character))

nodes_virt <- data_frame(id = 1:(length(members_with_NA_groups$id) + length(virt_group_nodes)),
                         is_virt = c(rep(FALSE, length(members_with_NA_groups$id)),
                                     rep(TRUE, length(virt_group_nodes))))

nodes_virt[1:length(members_with_NA_groups$id),]<- paste(members_with_NA_groups$id) #replace with the right names from our network
nodes_virt<- unique(nodes_virt)
edges_virt     
nodes_virt


g_virt <- graph_from_data_frame(edges_virt, 
                                directed = FALSE,
                                vertices = nodes_virt)

# use "auto layout"
lay <- layout_nicely(g)
# print(lay)

# lay <- layout_choices()

#  layout_nicely(g_virt)
# lay <- layout_(g_virt, with_fr(), normalize())

# remove virtual group nodes from graph
g_virt <- delete_vertices(g_virt,which(nodes_virt$is_virt == T ))

# remove virtual group nodes' positions from the layout matrix
tmp<-which(nodes_virt$is_virt == T )

lay <- lay[-tmp, ]
g_virt=delete.edges(g_virt, which(E(g_virt)$to_be_deleted==T))

#############
group_palette<- function(n){
  
  if(n>=8){
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    nn<-  col_vector[1:n]
    nnames<-c(nn, "white")
  }
  else
  {  
    qual_col_pals = brewer.pal(n = 8, name = "Dark2")
    nnames<-c(qual_col_pals[1:n], "white")
  }
}


#Plot
#par(mar = rep(0.1, 4))   # reduce margins
group_color <- group_palette(100)

# group_color <- c(brewer.pal(length(group_ids), 'Dark2'), "white")
group_color_fill <- adjustcolor(group_color, alpha.f = 0.5)



#####

m<-structure(list(vertex = c(members_with_NA_groups$id), affilation = c(members_with_NA_groups$group)), 
             .Names = c("vertex", "affilation"), class = "data.frame", row.names = c(NA, -length(members_with_NA_groups$id)))
sorted_vertices<-unique(m$vertex)
oneAffil <- m$affilation[!duplicated(sorted_vertices)]
am <- acast(m, formula = vertex ~ affilation, fill = 0)
am_backup<-am
for (i in 1:length(sorted_vertices))
{
  index<-(which(rownames(am) ==sorted_vertices[i]))
  am[i,]<-(cbind(am_backup[index,]))
}
rownames(am)<-sorted_vertices

colnames(am) <-(unique(members_with_NA_groups$group))
rows<-dim(am)[[1]]
columns<-dim(am)[[2]]
sums<-rep(0,rows)

for (i in 1:rows) {
  for (j in 1:columns) {
    if(am[i,j]>0)
    {
      am[i,j]<-1        
    }
  }
}

for (j in 1:columns) {
  for (i in 1:rows) {
    sums[[i]]<-sums[[i]]+am[i,j]
  }
}

for (i in 1:rows) {
  if(sums[[i]]==0)
  {
    am[i,as.integer(columns)]<-1
  }
}
values <- lapply(seq_len(nrow(am)), function(i) {am[i,]
  
})





##########################
### Choose specific groups ####
if (length(x)) {
#  if(x==1)
#    return()
  # am[am == 0] <- 0.5
  #values <- lapply(seq_len(nrow(am[,1:x])), function(i) {am[i, 1:x]
   print (x)
  })
}

x<-2
# am[am == 0] <- NA
values <- lapply(seq_len(nrow(am[,1:x])), function(i) {am[i, 1:x]
  
  
})


class(values)

n<- length(unique(members_with_NA_groups$group))

legendDataFrame <- data.frame(stringsAsFactors=FALSE, Group = unique(members_with_NA_groups$group), Colour = group_color[1:n])

groups


g <- make_ring(10)
values <- lapply(1:10, function(x) sample(1:10,3))
if (interactive()) {
  plot(g, vertex.shape="pie", vertex.pie=values,
       vertex.pie.color=list(heat.colors(5)),
       vertex.size=seq(10,30,length=10), vertex.label=NA)
}


ncolor<- nrow(my_network)-1
####


group_color<- c("black", "white")

rp<- NULL
# recordPlot()




visIgraph(g_virt) %>%
  visNodes(size = 25, shape = "circle") %>%

  # visInteraction(keyboard = TRUE)
  visInteraction( dragNodes = F) %>%
  visEvents(hoverNode = "function(nodes) {
        Shiny.onInputChange('current_node_id', nodes);
      ;}") %>%
  visClusteringByGroup(g_virt, group_ids) %>%
  visGroups(g_virt, groupname = )

x11()
visIgraph(g_virt) %>%
  visNodes(size = 20, shape = "circle",  scaling = list(max = 20, min =20 )) %>%
  visInteraction(keyboard = F, dragNodes = T) 
# %>%
#   visLayout(lay)

visNetwork(g_virt,largest,1)

nodes <- data.frame((members_with_NA_groups$id), group= members_with_NA_groups$group, replace = TRUE)
edges <- data.frame(from = my_network$from, to = my_network$to)

visNetwork(nodes, edges) %>%
  visGroups(groupname = "a", color = "red", shape = "database") %>%
  visGroups(groupname = "b", color = "yellow", shape = "triangle") %>%
  visClusteringByGroup(groups = c("c"), label = "Group : ", 
                       shape = "ellipse", color = "blue", force = TRUE) %>%
  visLegend()

library(visNetwork)
tkplot(g_virt, vertex.color = 'grey97',
       edge.color = "grey",
       mark.groups = group_ids, mark.col = group_color_fill,
       mark.border = group_color)

tk_off()


# rp=recordPlot()


for(i in 1:x)
{
  tmp_selected_colors<- c(tmp_selected_colors, ccc[s[i]])
}
V(g_virt)$color <-rainbow(11)
group_color <- sapply(V(g_virt)$color, function(y){ifelse(y %in% group_ids, 1, V(g_virt)$color)})

vis
a<- plot(g_virt, layout=  lay,
     vertex.shape="pie",
     vertex.pie=values,
     vertex.pie.color= list(V(g_virt)$color),
     vertex.size = 5,
     edge.color = "black",
     mark.groups = group_ids[c(1,2)], mark.col = group_color_fill,
     mark.border = group_color)
dev.off()
# )

set.seed(123)
par(mar = c(0,0,0,0)) 
plot(g_virt, layout=lay,
     # vertex.shape="pie",
     # vertex.pie=values,
     # vertex.pie.color= list(group_color),
     vertex.size = ifelse(V(g_virt)$name %in% s_labels_pies, 15, 9),
     edge.color = "black",
     mark.groups = group_ids, mark.col = group_color_fill,
     mark.border = group_color, edge.color = "grey50", vertex.label = ifelse(V(g_virt)$name %in% s_labels_pies, V(g_virt)$name, "")
)
# dev.off()
# )


x<- length(group_ids)
group_ids[c(1:x)]

plot(g_virt, layout=  lay,
     # vertex.shape="pie",
     # vertex.pie=values,
     # vertex.pie.color= list(group_color),
     # vertex.size = 9,
     edge.color = "black",
     mark.groups = group_ids, mark.col = group_color_fill,
     mark.border = group_color)
zoom::zm()
111112inout.zoom()


legend('topleft', legend = names(group_ids), col = group_color,
       pch = 15,  pt.cex = 1.5, cex = 0.8, 
       text.col = "black", horiz = FALSE,  title="Groups", incect(0, 0, 0))


##############################################################

p<-plot(g_virt, layout = lay, vertex.color = 'grey97', vertex.size = 15,
        edge.color = 'grey50',
        mark.groups = group_ids, mark.col = group_color_fill,
        mark.border = group_color, vertex.label.family='Arial')

figure() %>%
  ly_points(g_virt, lay[,1], lay[,2], data = members_with_NA_groups,
            
            hover = list(lay[,1], lay[,2]))







G <- upgrade_graph(g_virt)
L <- lay

vs <- V(g_virt)
es <- as.data.frame(get.edgelist(g_virt))

Nv <- length(vs)
Ne <- length(es[1]$V1)

Xn <- lay[,1]
Yn <- lay[,2]




edge_shapes <- list()
for(i in 1:Ne) {
  v0 <- es[i,]$V1
  v1 <- es[i,]$V2
  
  edge_shape = list(
    type = "line",
    line = list(color = "#030303", width = 0.3),
    x0 = Xn[v0],
    y0 = Yn[v0],
    x1 = Xn[v1],
    y1 = Yn[v1]
  )
  
  edge_shapes[[i]] <- edge_shape
}

axis <- list(title = " ", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)

pen <- png::readPNG("Selected_Groups.png")

plot_ly(x = ~Xn, y = ~Yn, mode = "markers", text = names(vs), hoverinfo = "text") %>%
  layout(title = '',
         shapes = edge_shapes,
         xaxis = axis,
         yaxis = axis,
    images = list(
      source = raster2uri(as.raster(pen)),
      x = 0.5, y = 0.5,
      sizex = 0.2, sizey = 0.1,
      xref = "x", yref = "y",
      # sizing = "stretch",
      opacity = 0.8,
      layer = "below",
      xanchor = "left", yanchor = "bottom"
    ),
    
    margin = list(t = 50)
  )




# network <- plot_ly(x = ~Xn, y = ~Yn, mode = "markers", text = names(vs), hoverinfo = "text") %>%
#   layout(
#     network,
#     title = '',
#     shapes = edge_shapes,
#     xaxis = axis,
#     yaxis = axis,
#     
#     images = list(
#       list(
#         source = raster2uri(as.raster(pen)),
#         xref = "x",
#         yref = "y",
#         x = 0.9,
#         y = 3.1,
#         sizex = 2,
#         sizey = 2,
#         sizing = "stretch",
#         opacity = 0.4,
#         layer = "below"
#       )
#     )
#   )
# 
# network
# 
# 
# outfile <- tempfile(fileext = ".png")
# png(outfile)
# s = input$chooseGroups_rows_selected
# source("convex_hullInput.R", local = T)
# convexInput()
# dev.off()
# 
# 
# image_file <- "/Users/mikae/Downloads/convex_hull.png"
# txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
# 
# 
# G <- upgrade_graph(g_virt)
# L <- lay

vs <- V(g_virt)
es <- as.data.frame(get.edgelist(g_virt))

Nv <- length(vs)
Ne <- length(es[1]$V1)

Xn <- lay[,1]
Yn <- lay[,2]

network <- plot_ly(x = ~Xn, y = ~Yn, mode = "markers", text = names(vs), hoverinfo = "text") %>%
  layout(
    images = list(
      list(
        source =  "https://images.plot.ly/language-icons/api-home/r-logo.png",
             xref = "x",
             yref = "y",
             x = 0.9,
             y = 3.1,
             sizex = 2,
             sizey = 2,
             sizing = "stretch",
             opacity = 0.4,
             layer = "below"
        )
      )
    )



edge_shapes <- list()
for(i in 1:Ne) {
  v0 <- es[i,]$V1
  v1 <- es[i,]$V2
  
  edge_shape = list(
    type = "line",
    line = list(color = "#030303", width = 0.3),
    x0 = Xn[v0],
    y0 = Yn[v0],
    x1 = Xn[v1],
    y1 = Yn[v1]
  )
  
  edge_shapes[[i]] <- edge_shape
}

axis <- list(title = " ", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)

# p <- layout(
#   network,
#   # source= outfile,
#   title = '',
#   shapes = edge_shapes,
#   xaxis = axis,
#   yaxis = axis
# )


p




























##################################
ggg<- data.frame(my_network)
my_network<- as.data.frame(get.edgelist(g))
my_network<- data.frame(from = my_network$V1, to = my_network$V2)
print(my_network)
if (length(my_network$from) < 1000){
  simpleNetwork(my_network,
                linkDistance = 200, charge = -30, fontSize = 12, fontFamily = 'Arial',
                linkColour = "#666", nodeColour = "#3182bd", opacity = 0.8, zoom = T)}
if (length(my_network$V1) > 1000){
  simpleNetwork(my_network,linkDistance = 50, charge = -5, fontSize = 9, fontFamily = 'Arial',
                linkColour = "#666", nodeColour = "#3182bd", opacity = 0.8, zoom = T)
}


##########################################

ggg <- igraph_to_networkD3(g_virt, group = members$group[!duplicated(members_with_NA_groups$id)])

forceNetwork(Links = ggg$links, Nodes = ggg$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', linkDistance = 200, charge = -30, fontSize = 9, fontFamily = "serif",
             linkColour = "#666", opacity = 0.8, zoom = T)

#############################################



m<-structure(list(vertex = c(members_with_NA_groups$id), affilation = c(members_with_NA_groups$group)), 
             .Names = c("vertex", "affilation"), class = "data.frame", row.names = c(NA, -length(members_with_NA_groups$id)))
sorted_vertices<-unique(m$vertex)
oneAffil <- m$affilation[!duplicated(sorted_vertices)]
am <- acast(m, formula = vertex ~ affilation, fill = 0)
am_backup<-am
for (i in 1:length(sorted_vertices))
{
  index<-(which(rownames(am) ==sorted_vertices[i]))
  am[i,]<-(cbind(am_backup[index,]))
}
rownames(am)<-sorted_vertices


colnames(am) <-(unique(members_with_NA_groups$group))
rows<-dim(am)[[1]]
columns<-dim(am)[[2]]
sums<-rep(0,rows)

  for (i in 1:rows) {
    for (j in 1:columns) {
        if(am[i,j]>0)
          {
          am[i,j]<-1        
          }
    }
  }

for (j in 1:columns) {
  for (i in 1:rows) {
      sums[[i]]<-sums[[i]]+am[i,j]
  }
}

for (i in 1:rows) {
  if(sums[[i]]==0)
  {
    am[i,as.integer(columns)]<-1
  }
}
values <- lapply(seq_len(nrow(am)), function(i) {am[i,]
  
})
group_color<- c(group_color, "white")
if (interactive()) {
  plot(g_virt, vertex.shape="pie", vertex.pie=values,
       vertex.pie.color=list(group_color),
       vertex.size=seq(as.integer(rows),30,length=as.integer(rows)))
}



class(values)
values

group_palette(4)


geom_point() +
  
  coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)

ranges <- reactiveValues(x = NULL, y = NULL)




lalala2 <-lalala[order(match(lalala[,1],nodes)),]

lalala2<-unique(lalala2)
lalala2<-lalala[!duplicated(lalala$id), ]
lalala2 <-data.frame(lalala[,1],lalala[,3])
