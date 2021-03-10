#Unsupervised Self-Organizing Map
library (kohonen)

#Data
data2015  <- read_xlsx("C:/Users/erikl/OneDrive - Colorado School of Mines/MNGN 598C/Assignment/Geospatial Big Data Life Cycle-Term Project/Data/Recap (To use)/WaterUsageCO2015.xlsx")
str (data2015)
data2015scale <- scale(data2015[,-1])
summary(data2015scale)

#SOM
set.seed(111)
g <- somgrid(xdim = 4, ydim = 1, topo = "rectangular")
map <- som (data2015scale, 
            grid = g,
            alpha = c(0.05, 0.01),
            radius = 1)
plot (map, type = 'changes')
plot (map)


#Further analysis of map
map$unit.classif
head(data2015)
map$codes

plot (map, type = 'codes',palette.name = rainbow,
      main = "Mapping of Water Usage Data")
plot (map, type = 'count')
plot (map, type = 'mapping')
plot (map, type = 'dist.neighbours')

#2005
#Data
data2005  <- read_xlsx("C:/Users/erikl/OneDrive - Colorado School of Mines/MNGN 598C/Assignment/Geospatial Big Data Life Cycle-Term Project/Data/Recap (To use)/WaterUsageCO2005.xlsx")
str (data2005)
data2005scale <- scale(data2005[,-1])
summary(data2005scale)

#SOM
set.seed(191)
g <- somgrid(xdim = 4, ydim = 1, topo = "rectangular")
map <- som (data2005scale, 
            grid = g,
            alpha = c(0.05, 0.01),
            radius = 1)
plot (map, type = 'changes')
plot (map)


#Further analysis of map
map$unit.classif
head(data2005)
map$codes

plot (map, type = 'codes',palette.name = rainbow,
      main = "Mapping of Water Usage Data")
plot (map, type = 'count')
plot (map, type = 'mapping')
plot (map, type = 'dist.neighbours')

