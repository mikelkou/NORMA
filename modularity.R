modularity<- function(){}


  net<- g_virt
  # Community detection (by optimizing modularity over partitions):
  clp <- cluster_optimal(net)
  # Community detection returns an object of class "communities" 
  
  plot(clp, net, vertex.color = "grey90",
       edge.color = 'grey80',
       vertex.size = 10,
       vertex.label.color = "black",)
  
  
  


#Instead of convex hulls, colors of communities in nodes
V(net)$community <- clp$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])


plot(net, vertex.color= "white", font.label.color= "black",mark.groups = c("BCL2L1","MDM4","CHEK2","CDKN2A","ATM","CDKN1A"), 
     mark.col=c("#C5E5E7","#ECD89A"), mark.border=NA)


plot(net, layout = lay, 
                vertex.color = "grey90",
                edge.color = 'grey80',
                vertex.size = 10,
                vertex.label.color = "black",
     mark.groups = list(c("BCL2L1","MDM4"),c("CHEK2","CDKN2A","ATM","CDKN1A")), 
     mark.col=c("#C5E5E7","#ECD89A"), mark.border=NA)
