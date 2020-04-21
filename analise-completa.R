whiskies <- read.csv("C:/whisky.txt", row.names = 1, stringsAsFactors = FALSE)

# I first went ahead and ensured that the dataset had no missing observations. 
# I generated a subset of the data that included only the 12 flavor variables, rescaled for comparability using scale().
sum(is.na(whiskies))  # no missing observations

## [1] 0
whiskies_k <- scale(whiskies[2:13])  # rescale selected vars for kmeans

# K-means clustering assigns each observation membership to one of k clusters in such a way that minimizes the distance between each observation and it's cluster's mean. 
# K-means clustering requires us to specify the number of clusters. 
# Below, we iterate through kmeans() with clusters argument varying from 1 to maxCluster and plot the within groups sum of squares for each iteration.
ssPlot <- function(data, maxCluster = 9) {
  # Initialize within sum of squares
  SSw <- (nrow(data) - 1) * sum(apply(data, 2, var))
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:maxCluster, SSw, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
}
ssPlot(whiskies_k)

# Naturally, the within groups sum of squares decreases as we increase the number of clusters. However, there is a trend of diminishing marginal returns as we increase the number of clusters. 
# I select the number of clusters based on the point at which the marginal return of adding one more cluster is less than was the marginal return for adding the clusters prior to that.
fit <- kmeans(whiskies_k, 4)  # 4 cluster solution 

# append cluster assignment
whiskies <- data.frame(whiskies, fit$cluster)
whiskies$fit.cluster <- as.factor(whiskies$fit.cluster)

# Cluster centers can inform on how taste profiles differ between clusters.
fit$centers

##      Body Sweetness    Smoky Medicinal Tobacco   Honey   Spicy   Winey
## 1 -0.6255   0.10478 -0.39342   -0.1825 -0.3606 -0.3720 -0.3647 -0.5765
## 2 -0.2541   0.05944 -0.04039    0.1214  2.7407 -0.2862  0.3606  0.2036
## 3  0.5113   0.05944  0.04733   -0.3071 -0.3606  0.7438  0.3220  0.7721
## 4  1.7163  -1.10234  2.46845    2.8149  1.7069 -1.2630  0.3606 -0.5111
##     Nutty   Malty   Fruity   Floral
## 1 -0.2395 -0.2673  0.06586  0.29654
## 2 -0.3632  0.5792 -0.38788  0.15866
## 3  0.4297  0.3624  0.13698 -0.07171
## 4 -0.3632 -0.7455 -0.81553 -1.79062


# Based on these centers, I anticipate that my love for the full bodied, smoky and medicinal lies in cluster 4.
subset(whiskies, fit.cluster == 4)

##    Distillery Body Sweetness Smoky Medicinal Tobacco Honey Spicy Winey
## 4      Ardbeg    4         1     4         4       0     0     2     0
## 22   Caol Ila    3         1     4         2       1     0     2     0
## 24  Clynelish    3         2     3         3       1     0     2     0
## 58  Lagavulin    4         1     4         4       1     0     1     2
## 59   Laphroig    4         2     4         4       1     0     0     1
## 78   Talisker    4         2     3         3       0     1     3     0
##    Nutty Malty Fruity Floral   Postcode Latitude Longitude fit.cluster
## 4      1     2      1      0  \tPA42 7EB   141560    646220           4
## 22     2     1      1      1  \tPA46 7RL   142920    670040           4
## 24     1     1      2      0   \tKW9 6LB   290250    904230           4
## 58     1     1      1      0   PA42 7DZ   140430    645730           4
## 59     1     1      0      0   PA42 7DU   138680    645160           4
## 78     1     2      2      0   IV47 8SR   137950    831770     

# I identified the most representative whisky of each cluster by seeking out the observation closest to the center based on all 12 variables.

whiskies_r <- whiskies[c(2:13, 17)]
# extract just flavor variables & cluster
candidates <- by(whiskies_r[-13], whiskies_r[13], function(data) {
  # we apply this function to observations for each level of fit.cluster
  dists <- sapply(data, function(x) (x - mean(x))^2)
  # for each variable, calc each observation's deviation from average of the
  # variable across observations
  dists <- rowSums(dists)
  # for each observation, sum the deviations across variables
  rownames(data)[dists == min(dists)]
  # obtain the row number of the smallest sum
})

candidates <- as.numeric(unlist(candidates))

whiskies[candidates, ]

##      Distillery Body Sweetness Smoky Medicinal Tobacco Honey Spicy Winey
## 42 Glenallachie    1         3     1         0       0     1     1     0
## 70 RoyalBrackla    2         3     2         1       1     1     2     1
## 1     Aberfeldy    2         2     2         0       0     2     1     2
## 4        Ardbeg    4         1     4         4       0     0     2     0
##    Nutty Malty Fruity Floral   Postcode Latitude Longitude fit.cluster
## 42     1     2      2      2   AB38 9LR   326490    841240           1
## 70     0     2      3      2   IV12 5QY   286040    851320           2
## 1      2     2      2      2  \tPH15 2EB   286580    749680           3
## 4      1     2      1      0  \tPA42 7EB   141560    646220   



# The dataset contains coordinates that I used to investigate how flavor profiles differ geographically. 
# The dataset's Latitude and Longitude variables are coordinates defined according to Great Britain's Ordnance Survey National Grid reference system. 
# I converted the coordinates to standard latitude and longitude in order to plot them using ggmap.

library(maptools)

## Loading required package: sp Checking rgeos availability: TRUE

library(rgdal)

## rgdal: version: 0.8-14, (SVN revision 496) Geospatial Data Abstraction
## Library extensions to R successfully loaded Loaded GDAL runtime: GDAL
## 1.10.1, released 2013/08/26 Path to GDAL shared files:
## C:/Revolution/R-Enterprise-6.2/R-2.15.3/library/rgdal/gdal GDAL does not
## use iconv for recoding strings. Loaded PROJ.4 runtime: Rel. 4.8.0, 6 March
## 2012, [PJ_VERSION: 480] Path to PROJ.4 shared files:
## C:/Revolution/R-Enterprise-6.2/R-2.15.3/library/rgdal/proj



whiskies.coord <- data.frame(whiskies$Latitude, whiskies$Longitude)
coordinates(whiskies.coord) = ~whiskies.Latitude + whiskies.Longitude

proj4string(whiskies.coord) = CRS("+init=epsg:27700")  # Specify that our coords are in osgb grid coord

whiskies.coord <- spTransform(whiskies.coord, CRS("+init=epsg:4326"))  # spTransform to convert osgb grid to lat/lon

whiskies <- cbind(whiskies, whiskies.coord)

# Alternatively, the ggmap package ships with a geocode function which uses Google Maps to determine the lat/lon based on a character string specifying the location.

library("ggmap")

## Loading required package: ggplot2

whiskies <- cbind(whiskies, geocode(paste(whiskies$Location, "Scotland", sep = " ,")))


whiskyMap <- qmap(location = "Scotland", zoom = 6, legend = "topleft", maptype = "terrain", 
                  color = "bw", darken = 0.5)

## Map from URL :
## http://maps.googleapis.com/maps/api/staticmap?center=Scotland&zoom=6&size=%20640x640&scale=%202&maptype=terrain&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
## Information from URL :
## http://maps.googleapis.com/maps/api/geocode/json?address=Scotland&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms

whiskyMap + geom_point(data = whiskies, aes(x = whiskies.Latitude, y = whiskies.Longitude, 
                                            colour = fit.cluster, size = 2))

# I zoomed in and examine which Distilleries lie within the Islay region.

whiskyMap <- qmap(location = "Islay", zoom = 10, legend = "topleft", maptype = "terrain", 
                  color = "bw", darken = 0.5)

## Map from URL :
## http://maps.googleapis.com/maps/api/staticmap?center=Islay&zoom=10&size=%20640x640&scale=%202&maptype=terrain&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
## Information from URL :
## http://maps.googleapis.com/maps/api/geocode/json?address=Islay&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms

whiskyMap + geom_point() + geom_text(data = whiskies, aes(x = whiskies.Latitude, 
                                                          y = whiskies.Longitude, label = Distillery, color = fit.cluster, face = "bold"))

## Warning: Removed 78 rows containing missing values (geom_text).

# The results indicate that there is a lot of variation in flavor profiles within the different scotch whisky regions. Note that initial cluster centers are chosen at random. 
# In order to replicate the results, you will need to run the following code before your analysis.

set.seed(1)

# Further data analysis would be required to determine whether proximity to types of water sources or terrain types drive common flavor profiles. 
#This could be done by obtaining shape files and adding them as an additional layer to the ggmap plot.

#For me, I have identified my next to - try single malt. Talisker is still within the familiar realm of cluster 4 but a little more malty, fruity and spicy. 
#Sounds like the perfect holiday mix.
