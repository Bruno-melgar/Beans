rm(list = ls())     # clear objects  
graphics.off() 
########################################
########## Suelen Dendrogram ###########
########################################


# Packages ----------------------------------------------------------------

inst <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("readxl","tidyverse","cluster","fpc","ggplot2","reshape2","purrr","dendextend", "ape")
inst(packages)




# Dataframe ---------------------------------------------------------------
(data <- read_excel("Desktop/BeansDF.xlsx", 
                   sheet = "Dataframe"))
summary(data)

#Wrangling
df<-data[,-c(3)]
nombres<-data[, "Code"]
df$Class<- paste(df$Sample, df$Micro)
df1 <- df %>% 
  relocate(Class, .before = Code)
df2<-df1[,-c(1,2)]

#----- Dissimilarity Matrix -----#
#library -> cluster 
# to perform different types of hierarchical clustering
# package functions used: daisy(), diana(), clusplot()
gower.dist <- daisy(df2[ ,3:49], metric = c("gower"))

#------------ DIVISIVE CLUSTERING ------------#
divisive.clust <- diana(as.matrix(gower.dist), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

#------------ AGGLOMERATIVE CLUSTERING ------------#
# I am looking for the most balanced approach
# Complete linkages is the approach that best fits this demand - I will leave only this one here, don't want to get it cluttered
# complete
aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative, complete linkages")

#library -> fpc
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# I am capping the maximum amout of clusters by 7
# I want to choose a reasonable number, based on which I will be able to see basic differences between customer groups as a result
stats.df.divisive <- cstats.table(gower.dist, divisive.clust, 9)
stats.df.divisive

stats.df.aggl <-cstats.table(gower.dist, aggl.clust.c, 9) #complete linkages looks like the most balanced approach
stats.df.aggl

capture.output(stats.df.aggl, file = "aggl.csv")
# Elbow
# Divisive clustering
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Agglomerative clustering,provides a more ambiguous picture
# Create separate data frame for the single point
single_point <- data.frame(cluster.number = 4, within.cluster.ss = 1.13)

# Plot the full data set
plot1 <- ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering - Elbow method") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

# Add the single point with different color and shape
plot1 + geom_point(data = single_point, color = "red", shape = 21, size= 4)


# Silhouette
# Create separate data frame for the single point
single_point2 <- data.frame(cluster.number = 4, avg.silwidth = 0.34)

# Plot the full data set 2
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

plot2 <- ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering - Silhouette method") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

plot2 + geom_point(data = single_point2, color = "red", shape = 21, size= 4)

# dendrogram ploting
# library -> dendextend
dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = 4, value =   c("gold3", "darkslategray4", "darkslategray3","gold1", "darkslategray4", "darkslategray3","gold3", "darkslategray4")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("black")) %>% 
  set("labels") %>% 
  set("labels_cex", 0.4) %>% 
  set("labels_col", value = c("darkslategray", "darkslategray", "darkslategray"), k=3)
ggd1 <- as.ggdend(dendro.col)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 4")

# Radial
colors = c("gold3", "darkslategray4", "darkslategray3","gold1", "darkslategray4", "darkslategray3","gold3", "darkslategray4")
clus4 = cutree(dendro, 4)
par(mar = c(0.5,0.5,0.5,0.5), bg = "transparent")
plot(as.phylo(dendro), type = "fan", tip.color = colors[clus4], cex = 0.9)









#Hard labeling
labels(dendro)
labels(dendro) <- c("CFA-3",
                    "CFA-1",
                    "CFA-2",
                    "LPFA 72h -1",
                    "LPFA 72h -2",
                    "LPFA 72h -3",
                    "LPFA 120h -1",
                    "LPFA 120h -2",
                    "LPFA 120h -3",
                    "PPFA 72h -2",
                    "PPFA 72h -1",
                    "PPFA 72h -3",
                    "LRFA 120h -1",
                    "LRFA 120h -2",
                    "LRFA 120h -3",
                    "LRFA 72h -1",
                    "LRFA 72h -2",
                    "LBFA 72h -1",
                    "LBFA 72h -2",
                    "LBFA 72h -3",
                    "LBFA 120h -1",
                    "LBFA 120h -2",
                    "LBFA 120h -3",
                    "CBA-1",
                    "CBA-2",
                    "CBA-3",
                    "LRFA 72h -3",
                    "PPFA 120h -1",
                    "PPFA 120h -2",
                    "PPFA 120h -3",
                    "CF-2",
                    "CF-1",
                    "CF-3",
                    "LBF 72h -2",
                    "LBF 72h -1",
                    "LBF 72h -3",
                    "PPF 120h -1",
                    "PPF 120h -2",
                    "PPF 120h -3",
                    "CB-1",
                    "CB-2",
                    "CB-3",
                    "LBF 120h -3",
                    "LBF 120h -1",
                    "LBF 120h -2",
                    "PPF 72h -3",
                    "PPF 72h -1",
                    "PPF 72h -2",
                    "LPF 72h -2",
                    "LPF 72h -1",
                    "LPF 72h -3",
                    "LRF 72h -2",
                    "LRF 72h -1",
                    "LRF 72h -3",
                    "LRF 120h -1",
                    "LRF 120h -2",
                    "LPF 120h -2",
                    "LPF 120h -3",
                    "LPF 120h -1",
                    "LRF 120h -3",
                    "PSFA-2",
                    "PSFA-1",
                    "PSFA-3",
                    "MBFA-1",
                    "MBFA-2",
                    "MBFA-3",
                    "MBF-1",
                    "MBF-2",
                    "MBF-3",
                    "EUF-2",
                    "EUF-1",
                    "EUF-3",
                    "PSF-3",
                    "PSF-1",
                    "PSF-2",
                    "EUFA-1",
                    "EUFA-2",
                    "EUFA-3",
                    "161FA-2",
                    "161FA-1",
                    "161FA-3",
                    "161F-1",
                    "161F-2",
                    "161F-3",
                    "134FA-1",
                    "134FA-2",
                    "134FA-3",
                    "102FA-3",
                    "102FA-1",
                    "102FA-2",
                    "102F-1",
                    "102F-2",
                    "102F-3",
                    "134F-3",
                    "134F-1",
                    "134F-2")
labels(dendro)
