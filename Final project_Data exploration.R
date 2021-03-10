#read the data
w2005 <- readOGR("C:/Users/erikl/OneDrive - Colorado School of Mines/MNGN 598C/Assignment/Geospatial Big Data Life Cycle-Term Project/Finale/WaterUsageCO/WaterUsageCO2005.shp")

#make choropleth
tm_shape(w2005) + tm_fill("TotalWithd", palette = "Reds", style = "quantile", legend.hist = TRUE, title = "Total Water Withdrawals 2005, Mgal/d") + tm_legend(outside = TRUE, text.size = 0.8, hist.width = 2) + tm_borders(alpha=.4)

# Calculate Queen's neighbours
nb2005 <- poly2nb(w2005, queen = TRUE)
nb2005
plot(w2005, border = 'lightgrey')
plot(nb2005, coordinates(w2005), add=TRUE, col='red')

# Calculate Rook's neighbours
nbr2005 <- poly2nb(w2005, queen = FALSE)
nbr2005

# compares different types of neighbours
plot(w2005, border = 'lightgrey')
plot(nb2005, coordinates(w2005), add=TRUE, col='red')
plot(nbr2005, coordinates(w2005), add=TRUE, col='blue')

# Convert the neighbour data to a listw object
list_nb2005 <- nb2listw(nb2005, style="W", zero.policy=TRUE)
list_nb2005
lag2005 <- lag.listw(list_nb2005,w2005$TotalWithd)


#Computing the Moran's I statistic: the hard way
# global spatial autocorrelation
moran.test(w2005$TotalWithd, list_nb2005)
mplot2005 <- moran.plot(w2005$TotalWithd, listw = nb2listw(nb2005, style="W", zero.policy=TRUE))
title(main="Spatial Autocorrelation")

# Create a regression model
m2005 <- lm(lag2005 ~ w2005$TotalWithd)
coef(m2005)[2]

n <- 599L   # Define the number of simulations
I.r <- vector(length=n)  # Create an empty vector
for (i in 1:n){
  # Randomly shuffle income values
  x <- sample(w2005$TotalWithd, replace=FALSE)
  # Compute new set of lagged values
  x.lag <- lag.listw(list_nb2005, x)
  # Compute the regression slope and store its value
  M.r    <- lm(x.lag ~ x)
  I.r[i] <- coef(M.r)[2]
}

# Plot the histogram of simulated Moran's I values
# then add our observed Moran's I value to the plot
hist(I.r, main=NULL, xlab="Moran's I", las=1)
abline(v=coef(m2005)[2], col="red")

#Computing a pseudo p-value from an MC simulation
N.greater <- sum(coef(m2005)[2] > I.r)
p <- min(N.greater + 1, n + 1 - N.greater) / (n + 1)
p

#Running a local spatial autocorrelation
#Creates a local moran output
local <- localmoran(x = w2005$TotalWithd,listw = nb2listw(nb2005, style="W", zero.policy=TRUE))
local

# binds results to our polygon shapefile
moran.map <- cbind(w2005, local)

# maps the results
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile",legend.hist = TRUE,title = "Local Moran Statistic")+ tm_legend(outside = TRUE, text.size = 0.8, hist.width = 2) + tm_borders(alpha=.4)


#2015
#read the data
w2015 <- readOGR("C:/Users/erikl/OneDrive - Colorado School of Mines/MNGN 598C/Assignment/Geospatial Big Data Life Cycle-Term Project/Finale/WaterUsageCO/WaterUsageCO2015.shp")

#make choropleth
tm_shape(w2015) + tm_fill("TotalWithd", palette = "Reds", style = "quantile", legend.hist = TRUE, title = "Total Water Withdrawals 2015, Mgal/d") + tm_legend(outside = TRUE, text.size = 0.8, hist.width = 2) + tm_borders(alpha=.4)

# Calculate Queen's neighbours
nb2015 <- poly2nb(w2015, queen = TRUE)
nb2015
plot(w2015, border = 'lightgrey')
plot(nb2015, coordinates(w2005), add=TRUE, col='red')

# Calculate Rook's neighbours
nbr2015 <- poly2nb(w2015, queen = FALSE)
nbr2015

# compares different types of neighbours
plot(w2015, border = 'lightgrey')
plot(nb2015, coordinates(w2015), add=TRUE, col='red')
plot(nbr2015, coordinates(w2015), add=TRUE, col='blue')

# Convert the neighbour data to a listw object
list_nb2015 <- nb2listw(nb2015, style="W", zero.policy=TRUE)
list_nb2015
lag2015 <- lag.listw(list_nb2015,w2015$TotalWithd)


#Computing the Moran's I statistic: the hard way
# global spatial autocorrelation
moran.test(w2015$TotalWithd, list_nb2015)
mplot2015 <- moran.plot(w2015$TotalWithd, listw = nb2listw(nb2015, style="W", zero.policy=TRUE))
title(main="Spatial Autocorrelation")

# Create a regression model
m2015 <- lm(lag2015 ~ w2015$TotalWithd)
coef(m2015)[2]

n <- 599L   # Define the number of simulations
I.r <- vector(length=n)  # Create an empty vector
for (i in 1:n){
  # Randomly shuffle income values
  x <- sample(w2015$TotalWithd, replace=FALSE)
  # Compute new set of lagged values
  x.lag <- lag.listw(list_nb2015, x)
  # Compute the regression slope and store its value
  M.r    <- lm(x.lag ~ x)
  I.r[i] <- coef(M.r)[2]
}

# Plot the histogram of simulated Moran's I values
# then add our observed Moran's I value to the plot
hist(I.r, main=NULL, xlab="Moran's I", las=1)
abline(v=coef(m2015)[2], col="red")

#Computing a pseudo p-value from an MC simulation
N.greater <- sum(coef(m2015)[2] > I.r)
p <- min(N.greater + 1, n + 1 - N.greater) / (n + 1)
p

#Running a local spatial autocorrelation
#Creates a local moran output
local <- localmoran(x = w2015$TotalWithd,listw = nb2listw(nb2015, style="W", zero.policy=TRUE))
local

# binds results to our polygon shapefile
moran.map <- cbind(w2015, local)

# maps the results
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile",legend.hist = TRUE,title = "Local Moran Statistic")+ tm_legend(outside = TRUE, text.size = 0.8, hist.width = 2) + tm_borders(alpha=.4)
