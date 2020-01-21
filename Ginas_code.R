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
#network_file<-"C:\\Users\\mikae\\Desktop\\MSc_Molecular_Biomedicine\\Master_Thesis_Pavlopoulos\\Application_note_2\\App\\string_interactions.txt"
#GO_file<-"C:\\Users\\mikae\\Desktop\\MSc_Molecular_Biomedicine\\Master_Thesis_Pavlopoulos\\Application_note_2\\App\\string_interactions_groups_comma_duplicate.txt"

#Example-2
#network_file<-"PAP_example.txt"
#GO_file<-"PAP_david.txt" #without spaces between annotation's words

#Example-3
#network_file<-"PAP_example.txt"
#GO_file<-"PAP_david_tmp.txt" #with spaces between annotation's words

#choose_file
network_file<- choose.files()
GO_file<- choose.files()


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

group_ids <- lapply(members_with_NA_groups %>% split(.$group), function(grp) { grp$id })
group_ids
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
# lay <- layout_nicely(g)
# print(lay)

lay <- layout_nicely(g_virt)
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
group_color <- brewer.pal(length(group_ids), 'Dark2')
# group_color <- brewer.pal(8, 'Dark2')
# group_color <- c(group_color, "purple", "grey")

# group_color <- c(brewer.pal(length(group_ids), 'Dark2'), "white")
group_color_fill <- adjustcolor(group_color, alpha.f = 0.2)


members_with_NA_groups
#####
m<-structure(list(vertex = members_with_NA_groups$id, affilation = c(members_with_NA_groups$group)), 
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

edge_col <- adjustcolor("grey30", alpha.f = 0.3)



gina
group_color <- brewer.pal(8, 'Dark2')
group_color <- c(group_color, brewer.pal(8, 'Set2'))
# group_color
group_color_fill <- adjustcolor(group_color, alpha.f = 0.2)

gina<-read.delim("gina_matrix.txt", header = T)

cg<- paste0("("(gina$group),"")

par(mar = c(0,0,0,0)) 
plot(g_virt, layout=lay,
     vertex.shape="pie",
     vertex.pie=values,
     vertex.pie.color= cg,
     vertex.frame.color = cg,
     vertex.frame.width = 20,
     vertex.size = 10,
     edge.color = edge_col, vertex.label.color = "black",
     mark.groups = group_ids, mark.col = group_color_fill,
     mark.border = group_color)


legend('topleft', legend = names(group_ids), col = group_color,
       pch = 15,  pt.cex = 1.5, cex = 0.8, 
       text.col = "black", horiz = FALSE,  title="Groups", incect(0, 0, 0))
