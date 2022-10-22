# This code for Part 1

library("tidyverse")
library("ggplot2")
library("DT")
library(dbscan)
library("maps")
library("mapdata")
cases <- read_csv("D:/Spring 2021/Data Mining/Project folders/Project 2/Project 2 Final data/Mahsa data_cleaned_AllData.csv")
cases <- cases %>% mutate_if(is.character, factor)
dim(cases)
summary(cases)
 

cases

cases_TX_scaled <- cases%>% select(
  pop_density,
  median_income,
  family_households,
  median_age ,
  median_income,
  unemployed_pop,
  commuters_by_public_transportation,
  
) %>% 
  scale() %>% as_tibble()

summary(cases_TX_scaled)

set.seed(1234)
ks <- 2:10
WSS <- sapply(ks, FUN = function(k) {
  kmeans(cases_TX_scaled , centers = k, nstart = 5)$tot.withinss
})



ggplot(as_tibble(ks, WSS), aes(ks, WSS)) + geom_line() +
  geom_vline(xintercept = 5, color = "red", linetype = 2)


km <- kmeans(cases_TX_scaled, centers = 4, nstart = 10)
km

ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"), 
                    cols = colnames(km$centers)), 
       aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))



cases_clustered <- cases_TX_scaled  %>% add_column(cluster = factor(km$cluster))
cases_clustered

#ggplot(cases_clustered , aes(x = median_income, y = median_age , color = cluster)) + geom_point()

summary(cases_clustered)

#############################################
cases <- read_csv("D:/Spring 2021/Data Mining/Project folders/Project 2/Project 2 Final data/Mahsa data_cleaned_AllData.csv")
cases <- cases %>% mutate_if(is.character, factor)
dim(cases)
summary(cases)

counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion))


cases_TX_scaled <- cases %>% mutate(county = county_name %>% 
                                      str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
summary(cases_TX_scaled)
counties_TX_clust <- counties_TX %>% left_join(cases_TX_scaled)

ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = distancing_response )) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")



counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion))



counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion))


cases_TX <- cases_TX_scaled %>% mutate(county = county_name %>% 
                                         str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

counties_TX_clust <- counties_TX %>% left_join(cases_TX_scaled %>% 
                                                 add_column(cluster = factor(km$cluster)))


ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")


#***************
#Hierarchical Clustering########################################################
cases <- read_csv("D:/Spring 2021/Data Mining/Project folders/Project 2/Project 2 Final data/Mahsa data_cleaned_AllData.csv")
cases <- cases %>% mutate_if(is.character, factor)
dim(cases)
summary(cases)
#cases<-cases[-1]


cases

cases_TX_scaled <- cases%>% select(
  pop_density,
  median_income,
  family_households,
  median_age ,
  median_income,
  unemployed_pop,
  commuters_by_public_transportation,
  
) %>% 
  scale() %>% as_tibble()

summary(cases_TX_scaled)

d <- dist(cases_TX_scaled)
#We cluster using complete link
hc <- hclust(d, method = "complete")
plot(hc)
library("ggdendro")
ggdendrogram(hc, labels = FALSE, theme_dendro = FALSE)
clusters <- cutree(hc, k = 5)
cluster_complete <- cases_TX_scaled%>%
  add_column(cluster = factor(clusters))
cluster_complete
#ggplot(cluster_complete, aes(x, y, color = cluster)) +
#  geom_point()
#ggplot(cases_TX_scaled%>% add_column(cluster = factor(cutree(hc, k = 8))),
#       aes(x, y, color = cluster)) + geom_point()

#Clustering with single link
hc_single <- hclust(d, method = "single")
plot(hc_single)
rect.hclust(hc_single, k = 5)
#cluster_single <- cases_TX_scaled %>%
#  add_column(cluster = factor(cutree(hc_single, k = 4)))
#ggplot(cluster_single, aes(x, y, color = cluster)) + geom_point()



#Density-based clustering with DBSCAN###########################################
library(dbscan)
cases_TX_scaled <- cases%>% select(
  pop_density,
  median_income,
  family_households,
  median_age ,
  median_income,
  unemployed_pop,
  commuters_by_public_transportation,
  
) %>% 
  scale() %>% as_tibble()
kNNdistplot(cases_TX_scaled, k = 3)
abline(h = .32, col = "red")
db <- dbscan(cases_TX_scaled, eps = .32, minPts = 4)
db
str(db)
#ggplot(ruspini_scaled %>% add_column(cluster = factor(db$cluster)),
#       aes(x, y, color = cluster)) + geom_point()

#Partitioning Around Medoids (PAM)##############################################
library(cluster)
cases_TX_scaled <- cases%>% select(
  pop_density,
  median_income,
  family_households,
  median_age ,
  median_income,
  unemployed_pop,
  commuters_by_public_transportation,
  
) %>% 
  scale() %>% as_tibble()
d <- dist(cases_TX_scaled)
str(d)
p <- pam(d, k = 4)
p

cases_TX_scaled <- cases_TX_scaled %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(cases_TX_scaled[p$medoids, ], rownames = "cluster")
medoids

#ggplot(ruspini_clustered, aes(x = x, y = y, color = cluster)) + geom_point() +
#  geom_point(data = medoids, aes(x = x, y = y, color = cluster), shape = 3, size = 10)



#Gaussian Mixture Models########################################################
library(mclust)
cases_TX_scaled <- cases%>% select(
  pop_density,
  median_income,
  family_households,
  median_age ,
  median_income,
  unemployed_pop,
  commuters_by_public_transportation,
  
) %>% 
  scale() %>% as_tibble()
m <- Mclust(cases_TX_scaled)
summary(m)
plot(m, what = "classification")


m <- Mclust(cases_TX_scaled, G=4)
summary(m)
plot(m, what = "classification")
#Spectral clustering############################################################
library("kernlab")
cluster_spec <- specc(as.matrix(cases_TX_scaled), centers = 5)
cluster_spec
summary(cases_TX_scaled)
#ggplot(ruspini_scaled %>% add_column(cluster = factor(cluster_spec)),
#       aes(x, y, color = cluster)) + geom_point()



#Fuzzy C-Means Clustering#######################################################
library("e1071")
cases_TX_scaled <- cases%>% select(
  pop_density,
  median_income,
  family_households,
  median_age ,
  median_income,
  unemployed_pop,
  commuters_by_public_transportation,
  
) %>% 
  scale() %>% as_tibble()
summary(cases_TX_scaled)
cases_TX_scaled

cluster_cmeans <- cmeans(as.matrix(cases_TX_scaled), centers = 5)
cluster_cmeans


#library("scatterpie")
#ggplot()  +
#  geom_scatterpie(data = cbind(cases_TX_scaled), cluster_cmeans$membership),
#                  aes(x = x, y = y), cols = colnames(cluster_cmeans$membership), legend_name = "Membership") + coord_equal()






###############Internal Cluster Validation######################################


#Compare the Clustering Quality#################################################

fpc::cluster.stats(d, km$cluster)
sapply(
  list(
    km = km$cluster,
    hc_compl = cutree(hc, k = 4),
    hc_single = cutree(hc_single, k = 4)
  ),
  FUN = function(x)
    fpc::cluster.stats(d, x))[c("within.cluster.ss", "avg.silwidth"), ]
sapply(
  list(
    km = km$cluster,
    hc_compl = cutree(hc, k = 4),
    hc_single = cutree(hc_single, k = 4)
  ),
  FUN = function(x)
    fpc::cluster.stats(d, x))[c("within.cluster.ss", "avg.silwidth"), ]
#Silhouette plot################################################################
library(cluster)
plot(silhouette(km$cluster, d))


#Find Optimal Number of Clusters for k-means####################################
#ggplot(cases_TX_scaled), aes(x, y)) + geom_point()

set.seed(1234)
ks <- 2:10
#Within Sum of Squares

WSS <- sapply(ks, FUN = function(k) {
  kmeans(cases_TX_scaled, centers = k, nstart = 5)$tot.withinss
})

ggplot(as_tibble(ks, WSS), aes(ks, WSS)) + geom_line() +
  geom_vline(xintercept = 4, color = "red", linetype = 2)


#Average Silhouette Width
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(cases_TX_scaled, centers=k, nstart = 5)$cluster)$avg.silwidth
})

best_k <- ks[which.max(ASW)]
best_k
ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = best_k, color = "red", linetype = 2)
#Dunn Index
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(cases_TX_scaled, centers=k, nstart=5)$cluster)$dunn
})

best_k <- ks[which.max(DI)]
ggplot(as_tibble(ks, DI), aes(ks, DI)) + geom_line() +
  geom_vline(xintercept = best_k, color = "red", linetype = 2)


#Gap Statistic
library(cluster)
k <- clusGap(cases_TX_scaled, FUN = kmeans,  nstart = 10, K.max = 10)
k
plot(k)
#Visualizing the Distance Matrix
#ggplot(cases_TX_scaled, aes(x, y, color = factor(km$cluster))) + geom_point()

d <- dist(cases_TX_scaled)
as.matrix(d)[1:5, 1:5]
library(seriation)
pimage(d)
pimage(d, order=order(km$cluster))


dissplot(d, labels = km$cluster, options=list(main="k-means with k=4"))

dissplot(d, labels = kmeans(cases_TX_scaled, centers = 3)$cluster)


dissplot(d, labels = kmeans(cases_TX_scaled, centers = 9)$cluster)




