setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4/codes")

#data<-read.csv("dataBeforeEDA.csv")


source("EDA.R")



### THis code is meant  to augment the data with 0's

dat4 <-df <- data.frame(matrix(ncol = ncol(data3), nrow = 0))
x<-colnames(data3)
df <- data.frame(matrix(ncol = ncol(data3), nrow = 1))
colnames(df) <- x
df[is.na(df)] <- 0



dfList <- split( data3 , f = data3$SUBJECT_ID )

for(i in 1:length(dfList))
{
  miniData <-dfList[[i]]
  if (nrow(miniData)<4){
    diff<-4-nrow(miniData)
    temp<-data.frame(matrix(ncol = ncol(miniData), nrow = diff))
    temp[is.na(temp)] <- 0
    colnames(temp) <- x
    temp$HADM_ID<-miniData[1,1]
    miniData1<-rbind(temp,miniData)  
    df <-rbind(df,miniData1)
  }else{
    df <-rbind(df,miniData)
  }
}





















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


