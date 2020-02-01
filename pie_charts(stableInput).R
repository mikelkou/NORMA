pie_chartsInput <- function() {
  set.seed(123)
  
  g <- fetchFirstSelectedStoredIgraph_annotations_tab()
  if (is.null(g))
    return()
  my_network <- as.data.frame(get.edgelist(g))
  my_network <- data.frame(from = my_network$V1, to = my_network$V2)
  
  gName <- SelectedStoredNets()$name
  annoation_graph <- fetchFirstSelectedStoredGroups2_annotations_tab()
  
  if (is.null(annoation_graph))
    return()
  annotName <- SelectedStoredAnnots()$name
  annoation_graph <- as.data.frame(annoation_graph)
  groups <- annoation_graph
  groups <- data.frame(V1 = groups$Annotations,
               stri_split_fixed(groups$Nodes, ",",  simplify = TRUE))
  groups <- mutate_all(groups, funs(na_if(., "")))
  number_of_groups <- dim(groups)[1]
  #paste("Number of nodes in", gName, " is ", " and the annotation file is : ", annotName)
  
  x <- list()
  for (i in 1:number_of_groups) {
    group_i <- groups[i, ]
    group_i <- group_i[, -1]
    group_i <- group_i[!is.na(group_i)]
    x[[i]] <- (group_i)
  }
  
  GO <- list()
  for (i in 1:number_of_groups) {
    GO[[i]] <- rep(groups[i, 1], length(x[[i]]))
  }
  
  column1 <- my_network$from
  column2 <- my_network$to
  node_names <- unique(union(column1, column2))
  tt <- unlist(x)
  nodes_with_NA_groups <- setdiff(node_names, tt)
  
  members <- data_frame(id = unlist(x), group = unlist(GO))
  members_with_NA_groups <-
    data_frame(id = unlist(x), group = unlist(GO))
  if (length(nodes_with_NA_groups) > 0) {
    for (i in 1:length(nodes_with_NA_groups))
    {
      members_with_NA_groups[nrow(members_with_NA_groups) + 1, 1] <-
        nodes_with_NA_groups[i]
    }
    members_with_NA_groups <- unique(members_with_NA_groups)
  }
  
  edge <-
    data_frame(from = my_network$from,
               to = my_network$to,
               group = NA) #edge --> not edges
  
  within_group_edges <- members %>%
    split(.$group) %>%
    map_dfr(function (grp) {
      if (length(grp$id) >= 2) {
        id2id <- combn(grp$id, 2)
        data_frame(from = id2id[1, ],
                   to = id2id[2, ],
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
  for(i in 1: length(group_ids_tmp))
  {
    group_ids<-c(group_ids, group_ids_tmp[ EE_positions[[ as.character(i) ]]])
  }
  
  
  ####################################
  # print(group_ids)
  ####################################
  

  virt_group_nodes <-
    length(members_with_NA_groups$id) + 1:number_of_groups
  names(virt_group_nodes) <- c(letters[1:number_of_groups])
  edges_virt <-
    data_frame(
      from = edge$from,
      to = edge$to,
      weight = 5,
      group = edge$group
    )
  
  within_virt <-
    members %>% split(.$group) %>% map_dfr(function (grp) {
      group_name <- unique(grp$group)
      virt_from <- rep(virt_group_nodes[group_name], length(grp$id))
      if (length(grp$id) >= 2) {
        id2id <- combn(grp$id, 2)
        data_frame(
          from = c(id2id[1, ], virt_from),
          to = c(id2id[2, ], grp$id),
          # also connects from virtual_from node to each group node
          weight = c(rep(0.1, ncol(id2id)),     # weight between group nodes
                     rep(50, length(grp$id))),
          to_be_deleted = c(rep(T, ncol(id2id)),     # weight between group nodes
                            rep(T, length(grp$id))),
          # weight that 'ties together' the group (via the virtual group node)
          group = group_name
        )
      }
    })
  
  
  edges_virt <-
    bind_rows(mutate_all(edges_virt, as.character),
              mutate_all(within_virt, as.character)) # vgazei 38,39,40
  virt_group_na <- virt_group_nodes[is.na(names(virt_group_nodes))]
  non_group_nodes <-
    (members_with_NA_groups %>% filter(is.na(group)))$id
  nodes_virt <-
    data_frame(id = 1:(
      length(members_with_NA_groups$id) + length(virt_group_nodes)
    ),
    is_virt = c(rep(
      FALSE, length(members_with_NA_groups$id)
    ),
    rep(TRUE, length(virt_group_nodes))))
  
  nodes_virt[1:length(members_with_NA_groups$id), ] <-
    paste(members_with_NA_groups$id) #replace with the right names from our network
  nodes_virt <- unique(nodes_virt)
  
  g_virt <- graph_from_data_frame(edges_virt,
                                  directed = FALSE,
                                  vertices = nodes_virt)
  
  # use "auto layout"
  # lay <- layout_nicely(g_virt)
  lay <- layout_choices(g_virt, lay)
  
  
  # remove virtual group nodes from graph
  g_virt <- delete_vertices(g_virt, which(nodes_virt$is_virt == T))
  
  # remove virtual group nodes' positions from the layout matrix
  tmp <- which(nodes_virt$is_virt == T)
  
  lay <- lay[-tmp,]
  
  return(lay)
  
  
  # g_virt = delete.edges(g_virt, which(E(g_virt)$to_be_deleted == T))
  # 
  # #Colors
  # group_color <- c(group_palette(length(group_ids)), "white")
  # group_color_fill <- adjustcolor(group_color, alpha.f = 0.3)
  # 
  # #Nodes_pies
  # m <-
  #   structure(
  #     list(
  #       vertex = c(members_with_NA_groups$id),
  #       affilation = c(members_with_NA_groups$group)
  #     ),
  #     .Names = c("vertex", "affilation"),
  #     class = "data.frame",
  #     row.names = c(NA,-length(members_with_NA_groups$id))
  #   )
  # unique_groups<-(unique(m$affilation))
  # sorted_vertices <- unique(m$vertex)
  # oneAffil <- m$affilation[!duplicated(sorted_vertices)]
  # 
  # for (i in 1: length(unique_groups))
  # {
  #   EE_positions[[ as.character(unique_groups[i]) ]]<-i
  # }
  # 
  # am <- acast(m, formula = vertex ~ affilation, fill = 0)
  # am_backup<-am
  # for (i in 1:length(sorted_vertices))
  # {
  #  index <- (which(rownames(am) == sorted_vertices[i]))
  #  am[i, ] <- (cbind(am_backup[index, ]))
  # }
  # 
  # rownames(am) <- sorted_vertices
  # colnames(am) <- (unique(members_with_NA_groups$group))
  # 
  # rows <- dim(am)[[1]]
  # columns <- dim(am)[[2]]
  # sums <- rep(0, rows)
  # 
  # am_temporary<-am
  # 
  # for (i in 1:rows) {
  #   for (j in 1:columns) {
  #       am[i, j] <- 0
  #   }
  # }
  #     
  # 
  # for (i in 1:rows) {
  #   for (j in 1:columns) {
  #     if (am_temporary[i, j] > 0)
  #     {
  #       index<-EE_positions[[  as.character(j)   ]]
  #       #print(paste0(j,"-----",index, ">>>>>>" , am[i, j], ">>>>>>",  am_temporary [i,  index ]))
  #       am[i, index] <- am_temporary [i,  j ]
  #     }
  #   }
  # }
  # 
  # 
  # 
  # for (i in 1:rows) {
  #   for (j in 1:columns) {
  #     if (am[i, j] > 0)
  #     {
  #       am[i, j] <- 1
  #     }
  #   }
  # }
  # 
  # for (j in 1:columns) {
  #   for (i in 1:rows) {
  #     sums[[i]] <- sums[[i]] + am[i, j]
  #   }
  # }
  # 
  # for (i in 1:rows) {
  #   if (sums[[i]] == 0)
  #   {
  #     am[i, as.integer(columns)] <- 1
  #   }
  # }
  # 
  # values <- lapply(seq_len(nrow(am)), function(i) {
  #   am[i, ]
  # })
  # 
  # ##### Expression ###
  # if (!is.null(fetchFirstSelectedStoredExpression())){
  #   expression<-fetchFirstSelectedStoredExpression()
  #   colnames(expression) <- c("id", "color")
  #   express_order<- as.data.frame(members_with_NA_groups)
  #   express_order<- as.data.frame(unique(express_order$id))
  #   colnames(express_order) <- "id"
  #   expression<-inner_join(express_order, expression, by = "id")
  #   
  #   cg<- as.character(expression$color)
  #   cg_fill<- adjustcolor(cg, alpha.f = 0.5)}
  # 
  # 
  # else {
  #   expression<- as.data.frame(members_with_NA_groups)
  #   expression<- as.data.frame(unique(expression$id))
  #   expression$color <- rep(c("black"))
  #   colnames(expression) <- c("id", "color")
  #   cg<- as.character(expression$color)
  # }
  # # expression<-fetchFirstSelectedStoredExpression()
  # # colnames(expression) <- c("id", "color")
  # # express_order<- as.data.frame(members_with_NA_groups)
  # # express_order<- as.data.frame(unique(express_order$id))
  # # colnames(express_order) <- "id"
  # # expression<-inner_join(express_order, expression, by = "id")
  # # 
  # # cg<- as.character(expression$color)
  # # 
  # 
  # par(mar = c(0, 2, 0, 0))
  # 
  # #Plot
  # if (length(my_network$from) <= 50) {
  #   if (label_selected_pies == T & expression_colors_pies == F){
  #     set.seed(123)
  #     
  #    plot(
  #     g_virt,layout = lay,
  #     vertex.shape = "pie",
  #     vertex.pie = values,
  #     vertex.pie.color = list(group_color),
  #     vertex.size = 20,
  #     vertex.label.color = "black",
  #     edge.color = "grey50"
  #    )}
  #   else if (label_selected_pies == F & expression_colors_pies == F){
  #     set.seed(123)
  #     
  #     plot(
  #       g_virt, layout = lay,
  #       vertex.shape = "pie",
  #       vertex.pie = values,
  #       vertex.pie.color = list(group_color),
  #       vertex.size = 20,
  #       edge.color = "grey50", vertex.label= NA
  #     )
  #   }
  #   
  #   else if (label_selected_pies == T & expression_colors_pies == T){
  #     set.seed(123)
  #     
  #     plot(
  #       g_virt, layout = lay,
  #       vertex.shape = "pie",
  #       vertex.pie = values,
  #       vertex.frame.color = cg,
  #       vertex.pie.color = list(group_color),
  #       vertex.size = 20,
  #       vertex.label.color = "black",
  #       edge.color = "grey50"
  #     )}
  #     else if (label_selected_pies == F & expression_colors_pies == T){
  #       set.seed(123)
  #       plot(
  #         g_virt, layout = lay,
  #         vertex.shape = "pie",
  #         vertex.pie = values,
  #         vertex.frame.color = cg,
  #         vertex.pie.color = list(group_color),
  #         vertex.size = 20,
  #         edge.color = "grey50", vertex.label= NA
  #       )}
  #       
  #     
  # 
  # } else
  #   withProgress(min = 0, max = 1, {
  #     incProgress(message = "Processing data into plot",
  #                 detail = "This may take a while...",
  #                 amount = .1)
  #     if (label_selected_pies == T & expression_colors_pies == F){
  #       set.seed(123)
  #       
  #     plot(
  #       g_virt, layout = lay,
  #       vertex.shape = "pie",
  #       vertex.pie = values,
  #       vertex.pie.color = list(group_color),
  #       vertex.size = 9,
  #       vertex.label.color = "black",
  #       edge.color = "grey50"
  #     )}
  #     else if (label_selected_pies == F & expression_colors_pies == F){
  #       set.seed(123)
  #       
  #       plot(
  #         g_virt, layout = lay,
  #         vertex.shape = "pie",
  #         vertex.pie = values,
  #         vertex.pie.color = list(group_color),
  #         vertex.size = 9,
  #         edge.color = "grey50", vertex.label=NA
  #       )}
  #     
  #     if (label_selected_pies == T & expression_colors_pies == T){
  #       set.seed(123)
  #       plot(
  #         g_virt, layout = lay,
  #         vertex.shape = "pie",
  #         vertex.frame.color = cg,
  #         vertex.pie = values,
  #         vertex.pie.color = list(group_color),
  #         vertex.size = 9,
  #         vertex.label.color = "black",
  #         edge.color = "grey50"
  #       )}
  #     
  #     else if (label_selected_pies == F & expression_colors_pies == T){
  #       set.seed(123)
  #       plot(
  #         g_virt, layout = lay,
  #         vertex.shape = "pie",
  #         vertex.pie = values,
  #         vertex.pie.color = list(group_color),
  #         vertex.frame.color = cg,
  #         vertex.size = 9,
  #         edge.color = "grey50", vertex.label=NA
  #       )}
  #   })
  #     
  # 
  # #----------------------------------------------------------selection
  # if (length(s)) {
  #   set.seed(123)
  #   s<-sort(s)#-----------------------------------
  #   if (length(s) == number_of_groups) {
  #     values <- lapply(seq_len(nrow(am)), function(i) {
  #       am[i, ]
  #     })
  #   }
  #   else {
  #     all_groups <- c(1:number_of_groups)
  #     choices <- c()
  #     for (i in 1:length(all_groups)) {
  #       if (!is.element(i, s)) {
  #         choices <- c(choices, i)
  #       }
  #     }
  #     
  #     
  #     #-------------------------------
  #   
  #     # mm <- am[, -sort(choices)]
  #     
  #     # print(colnames(am)[ncol(am)])
  #     
  #     if (is.na(colnames(am)[ncol(am)]) == T) {
  #       mm <- am[, -sort(choices)]
  #       }
  #     else {
  #       mm <- am[, -sort(choices)]
  #       am[, as.integer(columns)] <- 0
  #       Na <- data.frame(am[, as.integer(columns)])
  #       names(Na) <- "Na"
  #       mm <- cbind(mm, Na)
  #       mm<- as.matrix(mm)
  #       }
  #     
  #     # print(mm)
  #     
  #     #------------------
  #     # mm <- am[, -sort(choices)]
  #     
  #     rowss <- dim(mm)[[1]]
  #     columnss <- dim(mm)[[2]]
  #     sumss <- rep(0, rowss)
  # 
  # 
  #     for (j in 1:(columnss - 1)) {
  #       for (i in 1:rowss) {
  #         sumss[[i]] <- sumss[[i]] + mm[i, j]
  #       }
  #     }
  #     for (i in 1:rowss) {
  #       mm[i, as.integer(columnss)] <- 0
  #       if (sumss[[i]] == 0)
  #       {
  #         mm[i, as.integer(columnss)] <- 1
  #       }
  #     }
  #     values <- lapply(seq_len(nrow(mm)), function(i) {
  #       mm[i, ]
  #     })
  #  
  #   }
  #   
  #   x<- length(s)
  #   ccc<-group_pal_rows(length(group_ids))
  #   tmp_selected_colors<- c()
  #   for(i in 1:x)
  #   {
  #     tmp_selected_colors<- c(tmp_selected_colors, ccc[s[i]])
  #   }
  # 
  #   group_color <- c(tmp_selected_colors,"white")
  #   
  #   
  #   # Labels for chosen groups #
  #   s_labels_pies<- c()
  #   for(i in 1:x){
  #     s_labels_pies <- c(s_labels_pies, group_ids[s[i]]) 
  #     s_labels_pies <- unlist(s_labels_pies)
  #   }
  #   ###
  #   
  #   if (label_selected_pies == T & expression_colors_pies == F & some_labels_pies ==F){
  #     set.seed(123)
  #     
  #     plot(
  #       g_virt, layout = lay,
  #       vertex.shape = "pie",
  #       vertex.pie = values,
  #       vertex.pie.color = list(group_color),
  #       vertex.size = 12,
  #       vertex.label.color = "black",
  #       edge.color = "grey50"
  #     )}
  #   else if (label_selected_pies == F & expression_colors_pies == F & some_labels_pies ==F){
  #     set.seed(123)
  #     
  #     plot(
  #       g_virt, layout = lay,
  #       vertex.shape = "pie",
  #       vertex.pie = values,
  #       vertex.pie.color = list(group_color),
  #       vertex.size = 12,
  #       edge.color = "grey50", vertex.label= NA
  #     )
  #   }
  #   
  #   else if (label_selected_pies == T & expression_colors_pies == T & some_labels_pies ==F){
  #     set.seed(123)
  #     
  #     plot(
  #       g_virt, layout = lay,
  #       vertex.shape = "pie",
  #       vertex.pie = values,
  #       vertex.frame.color = cg,
  #       vertex.pie.color = list(group_color),
  #       vertex.size = 12,
  #       vertex.label.color = "black",
  #       edge.color = "grey50"
  #     )}
  #   else if (label_selected_pies == F & expression_colors_pies == T & some_labels_pies ==F){
  #     set.seed(123)
  #     plot(
  #       g_virt, layout = lay,
  #       vertex.shape = "pie",
  #       vertex.pie = values,
  #       vertex.frame.color = cg,
  #       vertex.pie.color = list(group_color),
  #       vertex.size = 12,
  #       edge.color = "grey50", vertex.label= NA
  #     )}
  #   
  #   ###chosen labels
  #   if (label_selected_pies == T & expression_colors_pies == F & some_labels_pies ==T){
  #     set.seed(123)
  #     
  #     plot(
  #       g_virt, layout = lay,
  #       vertex.shape = "pie",
  #       vertex.pie = values,
  #       vertex.pie.color = list(group_color),
  #       vertex.size = ifelse(V(g_virt)$name %in% s_labels_pies, 18, 9),
  #       vertex.label.color = "black",
  #       edge.color = "grey50", vertex.label = ifelse(V(g_virt)$name %in% s_labels_pies, V(g_virt)$name, "")
  #     )}
  #   else if (label_selected_pies == F & expression_colors_pies == F & some_labels_pies ==T){
  #     set.seed(123)
  #     
  #     plot(
  #       g_virt, layout = lay,
  #       vertex.shape = "pie",
  #       vertex.pie = values,
  #       vertex.pie.color = list(group_color),
  #       vertex.size = 12,
  #       edge.color = "grey50", vertex.label = ifelse(V(g_virt)$name %in% s_labels_pies, V(g_virt)$name, "")
  #     )
  #   }
  #   
  #   else if (label_selected_pies == T & expression_colors_pies == T & some_labels_pies ==T){
  #     set.seed(123)
  #     
  #     plot(
  #       g_virt, layout = lay,
  #       vertex.shape = "pie",
  #       vertex.pie = values,
  #       vertex.frame.color = cg,
  #       vertex.pie.color = list(group_color),
  #       vertex.size = ifelse(V(g_virt)$name %in% s_labels_pies, 18, 9),
  #       vertex.label.color = "black",
  #       edge.color = "grey50", vertex.label = ifelse(V(g_virt)$name %in% s_labels_pies, V(g_virt)$name, "")
  #     )}
  #   else if (label_selected_pies == F & expression_colors_pies == T & some_labels_pies ==T){
  #     set.seed(123)
  #     plot(
  #       g_virt, layout = lay,
  #       vertex.shape = "pie",
  #       vertex.pie = values,
  #       vertex.frame.color = cg,
  #       vertex.pie.color = list(group_color),
  #       vertex.size = ifelse(V(g_virt)$name %in% s_labels_pies, 18, 9),
  #       edge.color = "grey50", vertex.label = ifelse(V(g_virt)$name %in% s_labels_pies, V(g_virt)$name, "")
  #     )}
  # }
  #     
}
