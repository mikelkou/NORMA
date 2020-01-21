expressionInput <- function(){
  set.seed(123)
  
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
  par(mar = c(0, 2, 0, 0))
 
  expression<-fetchFirstSelectedStoredExpression()
  # print(expression)
  
  
  # expression<- read.delim("string_expression_colors.txt", header = F)
  colnames(expression) <- c("id", "color")
  express_order<- as.data.frame(members_with_NA_groups)
  express_order<- as.data.frame(unique(express_order$id))
  colnames(express_order) <- "id"
  expression<-inner_join(express_order, expression, by = "id")
  # expression
  
  
  cg<- paste0("("(expression$color),"")
  # print(cg)
  cg_fill<- adjustcolor(cg, alpha.f = 0.5)
  edge_col <- adjustcolor("grey30", alpha.f = 0.3)
  
  
  # plot(g_virt, layout=lay,
  #      # vertex.shape="pie",
  #      # vertex.pie=values,
  #      # vertex.pie.color= list(group_color),
  #      vertex.color = "white",
  #      vertex.frame.color = cg,
  #      vertex.frame.width = 20,
  #      vertex.size = 10,
  #      edge.color = edge_col, vertex.label.color = "black",
  #      mark.groups = group_ids, mark.col = group_color_fill,
  #      mark.border = group_color)
  
  
  m<-structure(list(vertex = expression$id, affilation = c(expression$color)), 
               .Names = c("vertex", "affilation"), class = "data.frame", row.names = c(NA, -length(expression$id)))
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
  
  colnames(am) <-(unique(expression$group))
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
  
  par(mar = c(0,0,0,0)) 
  
  group_color <- c(group_palette(length(group_ids)), "white")
  group_color_fill <- adjustcolor(group_color, alpha.f = 0.2)
  
  group_color<- brewer.pal(8, 'Dark2')
  group_color_fill <- adjustcolor(group_color, alpha.f = 0.3)
  plot(g_virt, layout=lay,
       vertex.shape="pie",
       vertex.pie=values,
       vertex.pie.color= cg_fill,
       vertex.frame.color = cg,
       vertex.frame.width = 20,
       vertex.size = 10,
       edge.color = edge_col, vertex.label.color = "black",
       mark.groups = group_ids, mark.col = group_color_fill,
       mark.border = group_color) 
  
}