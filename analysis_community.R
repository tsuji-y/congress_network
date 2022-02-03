# 分割結果の図示----------------------------------------------------------------
g <- edge_list_byCon[[1]]
V(g)$name <- V(g)$state
rw <- walktrap.community(g, modularity=TRUE)
l <- as.dendrogram(rw)
plot(l, main = "80 congress votes network")
plot(l[[1]], main = "80 congress votes network (maximum block1)")


# ネットワークのプロット--------------------------------------------------------
plot(g, 
     vertex.size = 5,
     vertex.label.cex=1,
     vertex.color = ifelse(V(g)$party == 100, "blue", "red"), 
     vertex.label = ifelse(V(g)$south, "s", "o"),
     vertex.label.color="black",
     layout = layout.fruchterman.reingold,
     main = "40 congress network"
     )
