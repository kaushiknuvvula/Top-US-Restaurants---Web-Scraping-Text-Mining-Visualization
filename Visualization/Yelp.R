install.packages("mice")
library(mice)

data <- read.csv("data_yelp.csv",header = TRUE)

imp=mice(data, seed=1234)
fit= with(imp,lm(data$Review_Count ~data$No_of_Days ))
pooled = pool(fit)
summary(pooled)

mice_imputed_data = complete(imp, action=3)

install.packages("xlsx")
library(xlsx)
write.xlsx(mice_imputed_data, "C:/Users/Kaushik Nuvvula/Desktop/mydata.xlsx")

data <- read.csv("data_yelp.csv",header = TRUE)

distance <- dist(data, method = "euclidean")
#hcluster <- hclust(distance, method = "ward.D") #using wards method to calculate cluster
#plot(hcluster, hang = 0, label = F, main = "Cluster Dendrogram") 

fit.average <- hclust(distance, method="ward.D") 
#plot(fit.average, hang=1, cex=.8, main="Cluster Dendogram")
#dendogram = rect.hclust(fit.average, k=5)

#install.packages("dendextend")
# Using Hierarchical Clustering we can see that there are 5 clusters if we look at the dendogram
library(dendextend)
dendogram = color_labels(fit.average, k = 2)
plot(dendogram)

Cluster_No =cutree(fit.average,2)
yelp_clus = cbind(data,Cluster_No)

write.xlsx(yelp_clus, "C:/Users/Kaushik Nuvvula/Desktop/mydata_clus.xlsx")




