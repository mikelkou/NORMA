convexInput <- function(){
  set.seed(123)
  g <- fetchFirstSelectedStoredIgraph_annotations_tab()
  if (is.null(g)) 
    return()

  my_network<- as.data.frame(get.edgelist(g))
  my_network<- data.frame(from = my_network$V1, to = my_network$V2)
  
  gName <- SelectedStoredNets()$name

  annoation_graph <- fetchFirstSelectedStoredGroups2_annotations_tab()
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
  
  
  #### sort by group as in file #####
  
  group_order<-(as.list(unique(members_with_NA_groups$group)))
  EE <- new.env(hash = TRUE)
  EE_positions <- new.env(hash = TRUE)
  for(i in 1: length(group_order))
  {
    group_name_as_key<-group_order[[i]]
    EE[[ as.character(group_name_as_key) ]]<-i
    EE_positions[[ as.character(i) ]]<-group_order[[i]]
  }
  for(i in 1: length(group_order))
  {
    group_name_as_key<-group_order[[i]]
    index<-EE[[ as.character(group_name_as_key) ]]
  }
  
  
  group_ids_tmp <- lapply(members_with_NA_groups %>% split(.$group), function(grp) { grp$id })
  group_ids<-c()
  # print(group_ids_tmp)
  for(i in 1: length(group_ids_tmp))
  {
    group_ids<-c(group_ids, group_ids_tmp[ EE_positions[[ as.character(i) ]]])
  }

  
  
  
  #print(group_ids)

  ####################################
  
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
  # lay <- layout_nicely(g_virt)
  lay <- layout_choices(g_virt, lay)
  

  # remove virtual group nodes from graph
  g_virt <- delete_vertices(g_virt,which(nodes_virt$is_virt == T ))
  
  # remove virtual group nodes' positions from the layout matrix
  tmp<-which(nodes_virt$is_virt == T )
  
  lay <- lay[-tmp, ]
  
  return(lay)
  
  # print(lay)
  #g_virt=delete.edges(g_virt, which(E(g_virt)$to_be_deleted==T))
  
  #Plot
#  group_color <- c(group_palette(length(group_ids)), "white")
#  group_color_fill <- adjustcolor(group_color, alpha.f = 0.2)
#  par(mar = c(0, 2, 0, 0))
  
  
  ##### Expression ###
  # if (!is.null(fetchFirstSelectedStoredExpression())){
  # expression<-fetchFirstSelectedStoredExpression()
  # colnames(expression) <- c("id", "color")
  # express_order<- as.data.frame(members_with_NA_groups)
  # express_order<- as.data.frame(unique(express_order$id))
  # colnames(express_order) <- "id"
  # expression<-inner_join(express_order, expression, by = "id")
  # # print(expression)
  # cg<- as.character(expression$color)
  # cg_fill<- adjustcolor(cg, alpha.f = 0.5)}
  # 
  # 
  # else {
  #   expression<- as.data.frame(members_with_NA_groups)
  #   expression<- as.data.frame(unique(expression$id))
  #   expression$color <- rep(c("white"))
  #   colnames(expression) <- c("id", "color")
  #   cg<- as.character(expression$color)
  #   cg_fill<- adjustcolor(cg, alpha.f = 0.5)
  # }
  # 
  # 
  # ex_m<-structure(list(vertex = expression$id, affilation = c(expression$color)), 
  #              .Names = c("vertex", "affilation"), class = "data.frame", row.names = c(NA, -length(expression$id)))
  # ex_sorted_vertices<-unique(ex_m$vertex)
  # ex_oneAffil <- ex_m$affilation[!duplicated(ex_sorted_vertices)]
  # ex_am <- acast(ex_m, formula = vertex ~ affilation, fill = 0)
  # ex_am_backup<-ex_am
  # for (i in 1:length(ex_sorted_vertices))
  # {
  #   ex_index<-(which(rownames(ex_am) ==ex_sorted_vertices[i]))
  #   ex_am[i,]<-(cbind(ex_am_backup[ex_index,]))
  # }
  # rownames(ex_am)<-ex_sorted_vertices
  # 
  # colnames(ex_am) <-(unique(expression$group))
  # ex_rows<-dim(ex_am)[[1]]
  # ex_columns<-dim(ex_am)[[2]]
  # ex_sums<-rep(0,ex_rows)
  # 
  # for (i in 1:ex_rows) {
  #   for (j in 1:ex_columns) {
  #     if(ex_am[i,j]>0)
  #     {
  #       ex_am[i,j]<-1        
  #     }
  #   }
  # }
  # 
  # for (j in 1:ex_columns) {
  #   for (i in 1:ex_rows) {
  #     ex_sums[[i]]<-ex_sums[[i]]+ex_am[i,j]
  #   }
  # }
  # 
  # for (i in 1:ex_rows) {
  #   if(ex_sums[[i]]==0)
  #   {
  #     ex_am[i,as.integer(ex_columns)]<-1
  #   }
  # }
  # ex_values <- lapply(seq_len(nrow(ex_am)), function(i) {ex_am[i,]
  #   
  # })
  
# 
#   if (length(my_network$from) <= 50){
#     if (label_selected == T & expression_colors == F){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, 
#            vertex.color = 'grey97',
#            vertex.size = 12,
#            edge.color = 'grey50',
#            vertex.label.color = "black",
#            mark.groups = group_ids, mark.col = group_color_fill,
#            mark.border = group_color)
#     }
#     else if (label_selected == F & expression_colors == F){
#       set.seed(123)
#       
#     plot(g_virt, layout = lay, 
#          vertex.color = 'grey97',
#          vertex.size = 12,
#          edge.color = 'grey50',
#          mark.groups = group_ids, mark.col = group_color_fill,
#          mark.border = group_color, vertex.label= NA)}
#     
#     else if (label_selected == T & expression_colors == T){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, 
#            vertex.color = cg_fill, 
#            vertex.size = 12,
#            edge.color = 'grey50',
#            vertex.pie=ex_values,
#            vertex.pie.color= cg_fill,
#            vertex.frame.color = cg_fill,
#            vertex.frame.width = 20,
#            vertex.label.color = "black",
#            mark.groups = group_ids, mark.col = group_color_fill,
#            mark.border = group_color)
#     }
#       
#       else if (label_selected == F & expression_colors == T){
#       set.seed(123)
#       
#         plot(g_virt, layout = lay, 
#              vertex.color = cg_fill,
#              vertex.size = 12,
#              edge.color = 'grey50',
#              vertex.pie=ex_values,
#              vertex.pie.color= cg_fill,
#              vertex.frame.color = cg_fill,
#              vertex.frame.width = 20,
#              mark.groups = group_ids, mark.col = group_color_fill,
#              mark.border = group_color, vertex.label= NA)}
# 
#     
#     
#   } else
#     withProgress(min = 0, max = 1, {
#       incProgress(message = "Processing data into plot", 
#                   detail = "This may take a while...", amount = .1)
#       if (label_selected == T & expression_colors == F){
#         set.seed(123)
#         
#         plot(g_virt, layout = lay, 
#              vertex.color = 'grey97', 
#              vertex.size = 10,
#              edge.color = 'grey50',
#              vertex.label.color = "black",
#              mark.groups = group_ids, mark.col = group_color_fill,
#              mark.border = group_color)
#       }
#       
#       
#       if (label_selected == T & expression_colors == T){
#         set.seed(123)
#         
#         plot(g_virt, layout = lay, 
#              vertex.color = cg_fill,
#              edge.color = 'grey50',
#              vertex.pie=ex_values,
#              vertex.pie.color= cg_fill,
#              vertex.frame.color = cg_fill,
#              vertex.frame.width = 20,
#              vertex.size = 10,
#              vertex.label.color = "black",
#              mark.groups = group_ids, mark.col = group_color_fill,
#              mark.border = group_color)
#       }        
#       else if (label_selected == F & expression_colors == F){
#         set.seed(123)
#         
#         plot(g_virt, layout = lay, vertex.color = 'grey97', vertex.size = 10,
#              edge.color = 'grey50',
#              mark.groups = group_ids, mark.col = group_color_fill,
#              mark.border = group_color, vertex.label= NA)}
#       
#       else if (label_selected == F & expression_colors == T){
#         set.seed(123)
#         
#         plot(g_virt, layout = lay, 
#              vertex.color = cg_fill,
#              edge.color = 'grey50',
#              vertex.pie=ex_values,
#              vertex.pie.color= cg_fill,
#              vertex.frame.color = cg_fill,
#              vertex.frame.width = 20,
#              vertex.size = 10,
#              mark.groups = group_ids, mark.col = group_color_fill,
#              mark.border = group_color, vertex.label= NA)}
#         
#     })
#   
#   if (length(s)) {
#     set.seed(123)
#     s<-sort(s)#-----------------------------------
#     x<- length(s)
#     ccc<-group_pal_rows(length(group_ids))
#     tmp_selected_colors<- c()
#     for(i in 1:x)
#     {
#       tmp_selected_colors<- c(tmp_selected_colors, ccc[s[i]])
#     # print(s)
#     #  print(s[i])
#     #  print(EE_positions[[  as.character(s[i])  ]])
#     }
# 
#     group_color <- tmp_selected_colors
#     group_color_fill <- adjustcolor(group_color, alpha.f = 0.2)
#     
#     # Labels for chosen groups #
#     s_labels<- c()
#     for(i in 1:x){
#       s_labels <- c(s_labels, group_ids[s[i]]) 
#       s_labels <- unlist(s_labels)
#     }
#     ###
#     
#     
#     if (label_selected == T & expression_colors == F & some_labels ==F){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, vertex.color = 'grey97', vertex.size = 10,
#            edge.color = 'grey50',
#            vertex.label.color = "black",
#            mark.groups = group_ids[s], mark.col = group_color_fill,
#            mark.border = group_color)
#     }
#     
#     
#     if (label_selected == T & expression_colors == T & some_labels ==F){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, 
#            vertex.color = cg_fill,
#            edge.color = 'grey50',
#            vertex.pie=ex_values,
#            vertex.pie.color= cg_fill,
#            vertex.frame.color = cg_fill,
#            vertex.frame.width = 20,
#            vertex.size = 10,
#            vertex.label.color = "black",
#            mark.groups = group_ids[s], mark.col = group_color_fill,
#            mark.border = group_color)
#     }        
#     else if (label_selected == F & expression_colors == F & some_labels ==F){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, vertex.color = 'grey97', vertex.size = 10,
#            edge.color = 'grey50',
#            mark.groups = group_ids[s], mark.col = group_color_fill,
#            mark.border = group_color, vertex.label= NA)}
#     
#     else if (label_selected == F & expression_colors == T & some_labels ==F){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, 
#            vertex.color = cg_fill,
#            edge.color = 'grey50',
#            vertex.pie=ex_values,
#            vertex.pie.color= cg_fill,
#            vertex.frame.color = cg_fill,
#            vertex.frame.width = 20,
#            vertex.size = 10,
#            mark.groups = group_ids[s], mark.col = group_color_fill,
#            mark.border = group_color, vertex.label= NA)}
#     
#     
#     #### Some labels selection ####
#     ###
#     # vertex.label = ifelse(V(g_virt)$name %in% group_ids[[s]], V(g_virt)$name, "")
#     ###
#     
#     if (label_selected == T & expression_colors == F & some_labels ==T){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, vertex.color = 'grey97', vertex.size = ifelse(V(g_virt)$name %in% s_labels, 18, 9),
#            edge.color = 'grey50',
#            vertex.label.color = "black",
#            mark.groups = group_ids[s], mark.col = group_color_fill,
#            mark.border = group_color, vertex.label = ifelse(V(g_virt)$name %in% s_labels, V(g_virt)$name, ""))
#     }
#     
#     
#     if (label_selected == T & expression_colors == T & some_labels ==T){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, 
#            vertex.color = cg_fill,
#            edge.color = 'grey50',
#            vertex.pie=ex_values,
#            vertex.pie.color= cg_fill,
#            vertex.frame.color = cg_fill,
#            vertex.frame.width = 20,
#            vertex.size = ifelse(V(g_virt)$name %in% s_labels, 18, 9),
#            vertex.label.color = "black",
#            mark.groups = group_ids[s], mark.col = group_color_fill,
#            mark.border = group_color, vertex.label = ifelse(V(g_virt)$name %in% s_labels, V(g_virt)$name, ""))
#     }        
#     else if (label_selected == F & expression_colors == F & some_labels ==T){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, vertex.color = 'grey97', vertex.size = ifelse(V(g_virt)$name %in% s_labels, 18, 9),
#            edge.color = 'grey50',
#            mark.groups = group_ids[s], mark.col = group_color_fill,
#            mark.border = group_color, vertex.label = ifelse(V(g_virt)$name %in% s_labels, V(g_virt)$name, ""))}
#     
#     else if (label_selected == F & expression_colors == T & some_labels ==T){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, 
#            vertex.color = cg_fill,
#            edge.color = 'grey50',
#            vertex.pie=ex_values,
#            vertex.pie.color= cg_fill,
#            vertex.frame.color = cg_fill,
#            vertex.frame.width = 20,
#            vertex.size = ifelse(V(g_virt)$name %in% s_labels, 18, 9),
#            mark.groups = group_ids[s], mark.col = group_color_fill,
#            mark.border = group_color, vertex.label = ifelse(V(g_virt)$name %in% s_labels, V(g_virt)$name, ""))}
#     
#     
#     
#     
#     
#     
#     ## Downloading the selected groups ##
#     
#     Cairo(600, 600, file="Selected_Groups.png", type="png", bg="white")
#     if (label_selected == T & expression_colors == F){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, vertex.color = 'grey97', vertex.size = 10,
#            edge.color = 'grey50',
#            vertex.label.color = "black",
#            mark.groups = group_ids[s], mark.col = group_color_fill,
#            mark.border = group_color)
#     }
#     
#     
#     if (label_selected == T & expression_colors == T){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, 
#            vertex.color = cg_fill,
#            edge.color = 'grey50',
#            vertex.pie=ex_values,
#            vertex.pie.color= cg_fill,
#            vertex.frame.color = cg_fill,
#            vertex.frame.width = 20,
#            vertex.size = 10,
#            vertex.label.color = "black",
#            mark.groups = group_ids[s], mark.col = group_color_fill,
#            mark.border = group_color)
#     }        
#     else if (label_selected == F & expression_colors == F){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, vertex.color = 'grey97', vertex.size = 10,
#            edge.color = 'grey50',
#            mark.groups = group_ids[s], mark.col = group_color_fill,
#            mark.border = group_color, vertex.label= NA)}
#     
#     else if (label_selected == F & expression_colors == T){
#       set.seed(123)
#       
#       plot(g_virt, layout = lay, 
#            vertex.color = cg_fill,
#            edge.color = 'grey50',
#            vertex.pie=ex_values,
#            vertex.pie.color= cg_fill,
#            vertex.frame.color = cg_fill,
#            vertex.frame.width = 20,
#            vertex.size = 10,
#            mark.groups = group_ids[s], mark.col = group_color_fill,
#            mark.border = group_color, vertex.label= NA)}
# 
#     # legend('topleft', legend = names(group_ids[s]), col = group_color,
#     #        pch = 15, bty = "n",  pt.cex = 1.5, cex = 0.8,
#     #        text.col = "black", horiz = FALSE,  title="Groups", inset=c(-0.01,0))
# 
#     dev.off()
  # }

  
}


  
