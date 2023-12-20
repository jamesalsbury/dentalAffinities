getMDS <- function(mat, metric = TRUE) {
  if (metric) {
    np <- cmdscale(as.dist(mat), k = 2)
  } else {
    np <- isoMDS(as.dist(mat), k = 2)$points
  }
  df <- data.frame(site = rownames(np), x = np[,1], y = np[,2])
  xlim <- c(min(c(df[,2], df[,3]), na.rm=TRUE), max(c(df[,2], df[,3]), na.rm=TRUE))
  ggplot(df, aes(x,y)) +
    geom_point(size = 2) +
    scale_x_continuous(expand = c(0.05,0.05), limits = xlim) +
    scale_y_continuous(expand = c(0.05,0.05), limits = xlim) +
    coord_fixed(xlim = xlim, ylim = xlim) +
    geom_text_repel(aes(x = x, 
                        y = y, 
                        label = site)) + 
    xlab("") + ylab("") + theme_classic() +
    ggtitle("MDS diagram")
}

getClust <- function(mat) {
  grupy <- agnes(as.dist(mat), method = "ward")
  dg <- as.dendrogram(grupy)
  ggdendrogram(dg) +
    ggtitle("Dendrogram - Ward Method") + theme_classic() + xlab("") + ylab("") + 
    theme(axis.text.x = element_text(angle=90, hjust=1))
}