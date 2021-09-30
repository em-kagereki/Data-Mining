setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4")
library(factoextra)
library(dplyr)
library(tidyverse)
data<-read.csv("dataBeforeEDA.csv")









data <-data[1:200,]
data<-data %>% 
  mutate(across(everything(), .fns=~replace_na(.,0)))

#data.pca <-prcomp(data,scale=TRUE)
res.pca <-prcomp(data)

fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

### New Data:
data2<-read.csv("dataBeforeEDA.csv")
data2 <- data2[201:300,]
ind.sup.coord <- predict(res.pca, newdata = data2)
dataForAnalysis<-ind.sup.coord[, 1:4]


