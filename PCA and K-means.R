rm(list = ls())     # clear objects  
graphics.off() 
########################################
###### Suelen PCA and clusters #########
########################################


# Packages ----------------------------------------------------------------

inst <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr", 
              "ggplot2", "ggpubr", "broom", "AICcmodavg", "ggcorrplot", 
              "rgl", "fpc","cluster", "readxl", "magrittr","multipanelfigure")
inst(packages)




# Dataframe ---------------------------------------------------------------

(data <- read_excel("Desktop/SuelenDF2.xlsx", 
                   sheet = "PCA prep"))
summary(data)




# Manipulation ------------------------------------------------------------

(agg<-aggregate(.~Code, data, FUN=mean, na.rm= TRUE, na.action = NULL))

## Indexar nambres de primer columna como nombres de fila -----------------
(df <- agg %>%
  remove_rownames() %>%
  column_to_rownames(var = "Code"))

## Double check missing values --------------------------------------------
sapply(df, is.finite)
which(is.na(df))




# PCA Analysis ------------------------------------------------------------
## Prep -------------------------------------------------------------------

(df.pca<-prcomp(df, center = TRUE, scale = TRUE)) # Data Scaling
plot(df.pca, type="lines")
p1 <- fviz_eig(df.pca, 
               return_ggplot = TRUE,
               addlabels = TRUE, 
               barfill = "darkslategray4", 
               barcolor = "gold3",
               linecolor = "#FC4E07")
p1 +  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
summary(df.pca)

## Individuals plot -------------------------------------------------------
fviz_pca_ind(df.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("gold3", "darkslategray4", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

## Variables plot ---------------------------------------------------------
fviz_pca_var(df.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("gold3", "darkslategray4", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

## Biplot -----------------------------------------------------------------
fviz_pca_biplot(df.pca, repel = TRUE,
                     col.var = "darkslategray4", # Variables color
                     col.ind = "gold3"  # Individuals color
)




# K-means clustering ------------------------------------------------------
## Dataframe prep ---------------------------------------------------------
(dfsc <- scale(df))
obs_order <- c("CF",	"CFA",	"CB",	"CBA",	"102F",	"134F",	"161F",	"EUF",	
               "MBF",	"PSF",	"102AF",	"134AF",	"161AF",	"EUAF",	"MBAF",	
               "PSAF",	"LPF72h",	"LRF72h",	"LBF72h",	"PPF72h",	"LPF120h",	
               "LRF120h",	"LBF120h",	"PPF120h",	"LPAF72h",	"LRAF72h",	
               "LBAF72h",	"PPAF72h",	"LPAF120h",	"LRAF120h",	"LBAF120h",	
               "PPAF120h")
dfsc <- dfsc[obs_order,]

## Distance Matrix --------------------------------------------------------
m.distancia <- get_dist(dfsc, method = "euclidean")
p2 <- fviz_dist (m.distancia, 
           gradient = list (low = "gold3", mid = "white", high = "darkslategray4"),  
           order = FALSE)
p2 +  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Cluster estimation (Elbow, silhouette o gap_stat methods) --------------
fviz_nbclust(dfsc, kmeans , method = "wss")
fviz_nbclust (dfsc, kmeans, method = "gap_stat")
p3 <- fviz_nbclust (dfsc, kmeans, method = "silhouette")

## Silhouette stylish plot ------------------------------------------------
p4 <- p3 +  ggtitle("Optimal number of clusters - silhouette method") +
  labs(x = "Num.of clusters k", y = "Average silhouette width") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
single_point <- data.frame(x= 3, y = 0.226)
p4 + geom_point(data = single_point, aes(x = x, y = y), color = "red", shape = 21, size= 4)

## Clustering with different numbers of K-means clusters ------------------
## K2 ---------------------------------------------------------------------
k2<-kmeansruns(dfsc, krange=2, runs=100)
fviz_cluster(k2, data=dfsc) +
  scale_colour_manual(values = c("gold3", "darkslategray4")) +
  scale_fill_manual(values = c("gold3", "darkslategray4")) 

## K3 ---------------------------------------------------------------------
k3<-kmeansruns(dfsc, krange=3, runs=100)
p5 <- fviz_cluster(k3, data=dfsc) +
  scale_colour_manual(values = c("darkslategray3", "gold3", "darkslategray4")) +
  scale_fill_manual(values = c("darkslategray3", "gold3", "darkslategray4"))
p5 + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14))

## K4 ---------------------------------------------------------------------
k4<-kmeansruns(dfsc, krange=4, runs=100)
fviz_cluster(k4, data=dfsc)




# Boxplot singular response effect of the clusters ------------------------
## Protein response  ------------------------------------------------------
prot<-df %>% 
  ggplot(aes(x=factor(k3$cluster), y= Protein, fill= factor(k3$cluster))) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  xlab("Clusters") +
  labs(fill="Cluster") +
  scale_colour_manual(values = c("darkslategray3", "gold3", "darkslategray4")) +
  scale_fill_manual(values = c("darkslategray3", "gold3", "darkslategray4")) +
  theme_minimal()
protm<-prot + theme(axis.title.x = element_blank())
protm

## Phytates response ------------------------------------------------------
phyt<-df %>% 
  ggplot(aes(x=factor(k3$cluster), y= Phytates, fill= factor(k3$cluster))) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  xlab("Clusters") +
  labs(fill="Cluster") +
  scale_colour_manual(values = c("darkslategray3", "gold3", "darkslategray4")) +
  scale_fill_manual(values = c("darkslategray3", "gold3", "darkslategray4")) +
  theme_minimal()
phytm<-phyt + theme(axis.title.x = element_blank())

## Rafinose response ------------------------------------------------------
raf<-df %>% 
  ggplot(aes(x=factor(k3$cluster), y= Rafinose, fill= factor(k3$cluster))) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  xlab("Clusters") +
  labs(fill="Cluster") +
  scale_colour_manual(values = c("darkslategray3", "gold3", "darkslategray4")) +
  scale_fill_manual(values = c("darkslategray3", "gold3", "darkslategray4")) +
  theme_minimal()
rafm<-raf + theme(axis.title.x = element_blank())

## Estaquiose response ----------------------------------------------------
est<-df %>% 
  ggplot(aes(x=factor(k3$cluster), y= Estaquiose, fill= factor(k3$cluster))) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  xlab("Clusters") +
  labs(fill="Cluster") +
  scale_colour_manual(values = c("darkslategray3", "gold3", "darkslategray4")) +
  scale_fill_manual(values = c("darkslategray3", "gold3", "darkslategray4")) +
  theme_minimal()
estm<-est + theme(axis.title.x = element_blank())

## Antioxidant response (DPPH) --------------------------------------------
dpp<-df %>% 
  ggplot(aes(x=factor(k3$cluster), y= DPPH, fill= factor(k3$cluster))) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  xlab("Clusters") +
  labs(fill="Cluster") +
  scale_colour_manual(values = c("darkslategray3", "gold3", "darkslategray4")) +
  scale_fill_manual(values = c("darkslategray3", "gold3", "darkslategray4")) +
  theme_minimal()
dppm<-dpp + theme(axis.title.x = element_blank())

## α-linolenic acid (Omega-3) response ------------------------------------
c18<-df %>% 
  ggplot(aes(x=factor(k3$cluster), y= `C18:3n3`, fill= factor(k3$cluster))) +
  geom_boxplot() +
  geom_point() +
  xlab("Clusters") +
  labs(fill="Cluster") +
  scale_colour_manual(values = c("darkslategray3", "gold3", "darkslategray4")) +
  scale_fill_manual(values = c("darkslategray3", "gold3", "darkslategray4")) +
  theme_minimal()
c18m<-c18 + theme(axis.title.x = element_blank())

## Multi-response plot ----------------------------------------------------
theme_set(theme_minimal())
figboxs<-multi_panel_figure(columns = 3, rows = 2, panel_label_type = "upper-roman")
figboxs %<>%
  fill_panel(protm, column = 1, row = 1) %<>%
  fill_panel(dppm, column = 2, row = 1) %<>%
  fill_panel(c18m, column = 3, row = 1) %<>%
  fill_panel(phytm, column = 1, row = 2) %<>%
  fill_panel(rafm, column = 2, row = 2) %<>%
  fill_panel(estm, column = 3, row = 2)
figboxs




