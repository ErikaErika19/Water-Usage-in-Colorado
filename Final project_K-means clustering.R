#Data
data2005  <- read_xlsx("C:/Users/erikl/OneDrive - Colorado School of Mines/MNGN 598C/Assignment/Geospatial Big Data Life Cycle-Term Project/Data/Recap (To use)/WaterUsageCO2005.xlsx")

#Determine optimal number of clusters
set.seed(20)
data.matrix(data2005)
test1 <- scale (na.omit(data.matrix(data2005)[-1]))
head(data2005)

#Decide Cluster
wssplot <- function(test1, nc=20, seed=123){
  wss <- (nrow(test1)-1)*sum(apply(test1,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(test1,center=i)$withinss)}
  plot(1:nc, wss, type ="b", 
       xlab="Number of Clusters",
       ylab="within groups sum of squares")}
wssplot(test1,nc=7)

#wss
fviz_nbclust(test1, kmeans, method = "wss") + 
  geom_vline (xintercept = 4, linetype = 2) + 
  labs (subtitle = "withinss")

#gapstat
fviz_nbclust(test1, kmeans, nstart = 20, method = "gap_stat", nboot=50)+
  labs (subtitle = "Gap-Stat")

#silhouette
fviz_nbclust(test1, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette")

#Clustering
clusters <- kmeans(data2005[,2:11], 4)
data2005$clusters <- as.factor(clusters$cluster)
str (clusters)


locations_geo_df <- geocode(data2005$Location)
df.map_locations <-cbind(data2005, locations_geo_df)

write.xlsx(df.map_locations,"C:/Users/erikl/OneDrive - Colorado School of Mines/MNGN 598C/Assignment/Geospatial Big Data Life Cycle-Term Project/Data/Recap (To use)/WaterCO2005.xlsx")
testdata<-read_xlsx("C:/Users/erikl/OneDrive - Colorado School of Mines/MNGN 598C/Assignment/Geospatial Big Data Life Cycle-Term Project/Data/Recap (To use)/WaterCO2005.xlsx")
clusteredmap <- get_map ("Colorado", zoom = 7)
df.map_locations$longitude <- unlist(df.map_locations$longitude)
df.map_locations$latitude <- unlist(df.map_locations$latitude)
ggmap(clusteredmap) + geom_point(aes(x=lon[],y=lat[], colour=as.factor(clusters)), data = df.map_locations)

#2015
#Data
data2015  <- read_xlsx("C:/Users/erikl/OneDrive - Colorado School of Mines/MNGN 598C/Assignment/Geospatial Big Data Life Cycle-Term Project/Data/Recap (To use)/WaterUsageCO2015.xlsx")

#Determine optimal number of clusters
set.seed(20)
data.matrix(data2015)
test1 <- scale (na.omit(data.matrix(data2015)[-1]))
head(data2015)

#Decide Cluster
wssplot <- function(test1, nc=20, seed=123){
  wss <- (nrow(test1)-1)*sum(apply(test1,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(test1,center=i)$withinss)}
  plot(1:nc, wss, type ="b", 
       xlab="Number of Clusters",
       ylab="within groups sum of squares")}
wssplot(test1,nc=7)

#wss
fviz_nbclust(test1, kmeans, method = "wss") + 
  geom_vline (xintercept = 4, linetype = 2) + 
  labs (subtitle = "withinss")

#gapstat
fviz_nbclust(test1, kmeans, nstart = 20, method = "gap_stat", nboot=50)+
  labs (subtitle = "Gap-Stat")

#silhouette
fviz_nbclust(test1, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette")

#Clustering
clusters <- kmeans(data2015[,2:11], 4)
data2015$clusters <- as.factor(clusters$cluster)
str (clusters)


locations_geo_df <- geocode(data2015$Location)
df.map_locations <-cbind(data2015, locations_geo_df)

write.xlsx(df.map_locations,"C:/Users/erikl/OneDrive - Colorado School of Mines/MNGN 598C/Assignment/Geospatial Big Data Life Cycle-Term Project/Data/Recap (To use)/WaterCO2015.xlsx")
testdata<-read_xlsx("C:/Users/erikl/OneDrive - Colorado School of Mines/MNGN 598C/Assignment/Geospatial Big Data Life Cycle-Term Project/Data/Recap (To use)/WaterCO2015.xlsx")
clusteredmap <- get_map ("Colorado", zoom = 7)
df.map_locations$longitude <- unlist(df.map_locations$longitude)
df.map_locations$latitude <- unlist(df.map_locations$latitude)
ggmap(clusteredmap) + geom_point(aes(x=lon[],y=lat[], colour=as.factor(clusters)), data = df.map_locations)

