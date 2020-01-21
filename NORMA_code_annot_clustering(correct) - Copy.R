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
lay <- layout_nicely(g_virt)
lay_fr <- layout.fruchterman.reingold(g_virt)
lay_auto <- layout.auto(g_virt)
lay_drl <- layout.drl(g_virt)

# remove virtual group nodes from graph
g_virt <- delete_vertices(g_virt,which(nodes_virt$is_virt == T ))

# remove virtual group nodes' positions from the layout matrix
tmp<-which(nodes_virt$is_virt == T )

lay <- lay[-tmp, ]
g_virt=delete.edges(g_virt, which(E(g_virt)$to_be_deleted==T))

#Plot
#par(mar = rep(0.1, 4))   # reduce margins

group_color <- brewer.pal(length(group_ids), 'Dark2')
group_color_fill <- paste0(group_color, '20')



plot(g_virt, layout = lay, vertex.color = 'white', vertex.size = 9,
     edge.color = rgb(0.5, 0.5, 0.5, 0.2),
     mark.groups = group_ids, mark.col = group_color_fill,
     mark.border = group_color)


legend('topright', legend = names(group_ids), col = group_color,
       pch = 15, bty = "n",  pt.cex = 1.5, cex = 0.8, 
       text.col = "black", horiz = FALSE)

