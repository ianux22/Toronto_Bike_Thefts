# Tasks : 
#   1 - Semplici analisi descrittive su zone più colpite e quando
#   2 - inferenza su cosa incide
#   3 - previsione 2019

#### 1. Libraries #####

library(ggpubr)
library(opendatatoronto)
library(dplyr)
library(rgdal)
library(sf)
library(tibble)
library(ggplot2)
library(viridisLite)
library(viridis)
library(RColorBrewer)
library(BBmisc)
library(cluster)
library(dendextend)
library(purrr)
library(factoextra)

#### 2. Load Shapefile's data ####

package <- show_package("4def3f65-2a65-4a4f-83c4-b2a4aed72d46")
package

# get all resources for this package
resources <- list_package_resources("4def3f65-2a65-4a4f-83c4-b2a4aed72d46")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()
data
rm(datastore_resources, package, resources)

#### 3. Load Shapefile ####
# Read this shape file with the rgdal library. 
shp <- readOGR(dsn = "C:/Users/leopa/Google Drive/Projects/Toronto Shapefile/Neighbourhoods", 
               layer="Neighbourhoods")
head(shp@polygons)
names(shp@data)
summary(shp@polygons)
shp@polygons

### Info for plottting are in 16th variable "geometry"

data_tbl <- as_tibble(data)

class(data_tbl)
data_tbl
Map_TO = ggplot(data = data_tbl, aes(geometry = geometry)) + geom_sf() +
  geom_text(data=data_tbl, aes(LONGITUDE, LATITUDE, 
                               label = AREA_LONG_CODE), size = 2) +
   scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
   ggtitle("Toronto Neighborhoods map")
   
Map_TO

hoods <- data_tbl[,c(6,7)]
names <- hoods$AREA_NAME

#### 4. Work on Bike data ####

# Load Bike's data

bikes_data <- read.csv("C:/Users/leopa/Google Drive/Projects/Toronto Bicycle Thefts/Bicycle Thefts Data.csv")

# add column of 1 for counting

bikes_data$count <- 1

bike_2014 <- subset(bikes_data, Occurrence_Year=='2014')
bike_2015 <- subset(bikes_data, Occurrence_Year=='2015')
bike_2016 <- subset(bikes_data, Occurrence_Year=='2016')
bike_2017 <- subset(bikes_data, Occurrence_Year=='2017')
bike_2018 <- subset(bikes_data, Occurrence_Year=='2018')
bike_2019 <- subset(bikes_data, Occurrence_Year=='2019')


bike_2014 <- aggregate(bike_2014$count, 
                       list(id = bike_2014$Hood_ID), 
                       sum)
colnames(bike_2014)[2]<- '2014'

bike_2015 <- aggregate(bike_2015$count, 
                       list(id = bike_2015$Hood_ID), 
                       sum)
colnames(bike_2015)[2]<- '2015'

bike_2016 <- aggregate(bike_2016$count, 
                       list(id = bike_2016$Hood_ID), 
                       sum)
colnames(bike_2016)[2]<- '2016'

bike_2017 <- aggregate(bike_2017$count, 
                       list(id = bike_2017$Hood_ID), 
                       sum)
colnames(bike_2017)[2]<- '2017'

bike_2018 <- aggregate(bike_2018$count, 
                       list(id = bike_2018$Hood_ID), 
                       sum)
colnames(bike_2018)[2]<- '2018'

bike_2019 <- aggregate(bike_2019$count, 
                       list(id = bike_2019$Hood_ID), 
                       sum)
colnames(bike_2019)[2]<- '2019'

# vector with IDs NEighborhood

id_area <- as.vector(data$AREA_LONG_CODE)
id_area <- sort(as.numeric(id_area))

id_2014 <- as.vector(bike_2014$id)
id_2014 <- sort(id_2014)

id_2015 <- as.vector(bike_2015$id)
id_2015 <- sort(id_2015)

id_2016 <- as.vector(bike_2016$id)
id_2016 <- sort(id_2016)


id_2017 <- as.vector(bike_2017$id)
id_2017 <- sort(id_2017)

id_2018 <- as.vector(bike_2018$id)
id_2018 <- sort(id_2018)

id_2019 <- as.vector(bike_2019$id)
id_2019 <- sort(id_2019)


missing_2014 <- setdiff(id_area, id_2014)
missing_2015 <- setdiff(id_area, id_2015)
missing_2016 <- setdiff(id_area, id_2016)
missing_2017 <- setdiff(id_area, id_2017)
missing_2018 <- setdiff(id_area, id_2018)
missing_2019 <- setdiff(id_area, id_2019)

missing_2014 <- data.frame(missing_2014)
missing_2014$'2014' <- 0
colnames(missing_2014)[1]<- 'id'
bike_2014 <- rbind(bike_2014, missing_2014)

missing_2015 <- data.frame(missing_2015)
missing_2015$'2015' <- 0
colnames(missing_2015)[1]<- 'id'
bike_2015 <- rbind(bike_2015, missing_2015)

missing_2016 <- data.frame(missing_2016)
missing_2016$'2016' <- 0
colnames(missing_2016)[1]<- 'id'
bike_2016 <- rbind(bike_2016, missing_2016)

missing_2017 <- data.frame(missing_2017)
missing_2017$'2017' <- 0
colnames(missing_2017)[1]<- 'id'
bike_2017 <- rbind(bike_2017, missing_2017)

missing_2018 <- data.frame(missing_2018)
missing_2018$'2018' <- 0
colnames(missing_2018)[1]<- 'id'
bike_2018 <- rbind(bike_2018, missing_2018)

missing_2019 <- data.frame(missing_2019)
missing_2019$'2019' <- 0
colnames(missing_2019)[1]<- 'id'
bike_2019 <- rbind(bike_2019, missing_2019)

rm(missing_2014, missing_2015, 
   missing_2016, missing_2017,
   missing_2018, missing_2019)

rm(id_2014, id_2015, id_2016, id_2017,
   id_2018, id_2019)

data_tbl$AREA_LONG_CODE <- as.numeric(data_tbl$AREA_LONG_CODE)
data_tbl <- data_tbl[order(data_tbl$AREA_LONG_CODE) , ]

bike_2014 <- bike_2014[order(bike_2014$id) , ]
bike_2015 <- bike_2015[order(bike_2015$id) , ]
bike_2016 <- bike_2016[order(bike_2016$id) , ]
bike_2017 <- bike_2017[order(bike_2017$id) , ]
bike_2018 <- bike_2018[order(bike_2018$id) , ]
bike_2019 <- bike_2019[order(bike_2019$id) , ]

data_tbl$y_2014 <- bike_2014$"2014"
data_tbl$y_2015 <- bike_2015$"2015"
data_tbl$y_2016 <- bike_2016$"2016"
data_tbl$y_2017 <- bike_2017$"2017"
data_tbl$y_2018 <- bike_2018$"2018"
data_tbl$y_2019 <- bike_2019$"2019"

rm(bike_2014, bike_2015, bike_2016,
   bike_2017, bike_2018, bike_2019)

#### 5. Plots thefts per year ####

data_tbl <- as_tibble(data_tbl)

class(data_tbl)
data_tbl

Map_2014 = ggplot(data = data_tbl, aes(geometry = geometry)) +
   geom_sf(data = data_tbl, aes(fill = y_2014)) +
   # scale_fill_viridis(option="viridis", name = "Number of thefts") +
   # scale_fill_gradient(low='grey', high='red') +
   scale_fill_gradient(low ='#FFFFFF' , high = '#FF3300', name="N of Thefts") +
   geom_text(data=data_tbl, aes(LONGITUDE, LATITUDE, 
                                label = y_2014), size = 2)+
   ggtitle("2014 Toronto bikes' thefts") +
   theme(text = element_text(size=20))


Map_2014

Map_2015 = ggplot(data = data_tbl, aes(geometry = geometry)) +
   geom_sf(data = data_tbl, aes(fill = y_2015)) +
   # scale_fill_viridis(option="viridis", name = "Number of thefts") +
   # scale_fill_gradient(low='grey', high='red') +
   scale_fill_gradient(low ='#FFFFFF' , high = '#FF3300', name="N of Thefts") +
   geom_text(data=data_tbl, aes(LONGITUDE, LATITUDE, 
                                label = y_2015), size = 2) +
   ggtitle("2015 Toronto bikes' thefts") +
   theme(text = element_text(size=20))
   

   

Map_2015

Map_2016 = ggplot(data = data_tbl, aes(geometry = geometry)) +
   geom_sf(data = data_tbl, aes(fill = y_2016)) +
   # scale_fill_viridis(option="viridis", name = "Number of thefts") +
   # scale_fill_gradient(low='grey', high='red') +
   scale_fill_gradient(low ='#FFFFFF' , high = '#FF3300', name="N of Thefts") +
   geom_text(data=data_tbl, aes(LONGITUDE, LATITUDE, 
                                label = y_2016), size = 2) +
   ggtitle("2016 Toronto bikes' thefts") +
   theme(text = element_text(size=20))

Map_2016

Map_2017 = ggplot(data = data_tbl, aes(geometry = geometry)) +
   geom_sf(data = data_tbl, aes(fill = y_2017)) +
   # scale_fill_viridis(option="viridis", name = "Number of thefts") +
   # scale_fill_gradient(low='grey', high='red') +
   scale_fill_gradient(low ='#FFFFFF' , high = '#003300', name="N of Thefts") +
   geom_text(data=data_tbl, aes(LONGITUDE, LATITUDE, 
                                label = y_2017), size = 2) +
   ggtitle("2017 Toronto bikes' thefts") +
   theme(text = element_text(size=20))

Map_2017

Map_2017

Map_2018 = ggplot(data = data_tbl, aes(geometry = geometry)) +
   geom_sf(data = data_tbl, aes(fill = y_2018)) +
   # scale_fill_viridis(option="viridis", name = "Number of thefts") +
   # scale_fill_gradient(low='grey', high='red') +
   scale_fill_gradient(low ='#FFFFFF' , high = '#CC6600', name="N of Thefts") +
   geom_text(data=data_tbl, aes(LONGITUDE, LATITUDE, 
                                label = y_2018), size = 2) +
   ggtitle("2018 Toronto bikes' thefts") +
   theme(text = element_text(size=20))

Map_2018

Map_2019 = ggplot(data = data_tbl, aes(geometry = geometry)) +
   geom_sf(data = data_tbl, aes(fill = y_2019)) +
   # scale_fill_viridis(option="viridis", name = "Number of thefts") +
   # scale_fill_gradient(low='grey', high='red') +
   scale_fill_gradient(low ='#FFFFFF' , high = '#3300CC', name="N of Thefts") +
   geom_text(data=data_tbl, aes(LONGITUDE, LATITUDE, 
                                label = y_2019), size = 2) +
   ggtitle("2019 Toronto bikes' thefts") +
   theme(text = element_text(size=20))

Map_2019



#### 6. Hierarchical clustering ####

# Normalize variables

data_tbl$sum <- (data_tbl$y_2014 + data_tbl$y_2015 +
                    data_tbl$y_2016 + data_tbl$y_2017 +
                    data_tbl$y_2018 + data_tbl$y_2019)

data_norm <- data_tbl[,c(17:23)]

data_norm <- normalize(data_norm)

# methods to assess best method for clustering 
#
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
   agnes(data_norm, method = x)$ac
}

map_dbl(m, ac)
##   average    single  complete      ward 

# Elbow Method

fviz_nbclust(data_norm, FUN = hcut, method = "wss")

# Silohuette method 

fviz_nbclust(data_norm, FUN = hcut, method = "silhouette")

# Gap Statistic

gap_stat <- clusGap(data_norm, FUN = hcut, nstart = 25, K.max = 10, B = 1000)
fviz_gap_stat(gap_stat)


# # Cut agnes() tree into 4 groups
# hc_a <- agnes(data_norm, method = "ward")
# cutree(as.hclust(hc_a), k = 4)
# 
# # Cut diana() tree into 4 groups
# hc_d <- diana(data_norm)
# cutree(as.hclust(hc_d), k = 4)

##### NB hclust wants the dist matrix 

dist_m <- dist(data_norm, method = "euclidean")
hc_w <- hclust(dist_m, method = "ward.D2" )
sub_grp <- cutree(hc_w, k = 4)
table(sub_grp)

data_tbl <- data_tbl %>%
   mutate(cluster = sub_grp) 

rm(data_norm, dist_m,
   m, sub_grp)   

data_tbl$cluster <- as.factor(data_tbl$cluster)

#to show and count clusters obs

data_tbl %>%
   group_by(cluster) %>%
   get_summary_stats(sum, type = "mean_sd")

# Plot Clusters

Map_Clust = ggplot(data = data_tbl, aes(geometry = geometry)) +
   geom_sf(data = data_tbl, aes(fill = cluster)) +
   # scale_fill_viridis(option="viridis", name = "Number of thefts") +
   # scale_fill_gradient(low='grey', high='red') +
   scale_fill_brewer(palette = "Dark2", name="Groups",
                     breaks=c("1", "2", "3", "4"),
                     labels=c("Low risk", "Mid-low risk", 
                              "Mid-hig risk", "High Risk")) +
   geom_text(data=data_tbl, aes(LONGITUDE, LATITUDE, 
                                label = AREA_LONG_CODE), size = 2)+
   ggtitle("Dangerous and safest zone") + 
   theme(text = element_text(size=20))

Map_Clust

rm(ac)

# Anova test for groups



#### 7. Analyzing Thefts' hours ####

bikes_data$Occurrence_Time <- as.Date(bikes_data$Occurrence_Time)

bikes_data$hour <- substr(bikes_data$Occurrence_Time, 1, 2)
bikes_data$hour <- as.factor(bikes_data$hour)
summary(bikes_data$hour)

bikes_data$hour <- as.numeric(bikes_data$hour)

ggplot(data=aggregate(bikes_data$count, 
                      list(id = bikes_data$hour), 
                      sum), aes(x=as.factor(id), y=x)) +
   geom_bar(stat="identity", fill="steelblue")+
   geom_text(aes(label=x), vjust=1.6, color="white", size=3.5)+
   theme_minimal() + 
   ggtitle("Distribution of Thefts during the day") + 
   xlab("Number of thefts") + 
   ylab("Hour of the day") + 
   theme(text = element_text(size=20))



# Creo 4 classi per la facia oraria

bikes_data[bikes_data$hour >= 1 & bikes_data$hour < 9, "time_zone"] <- 1
bikes_data[bikes_data$hour > 8 & bikes_data$hour < 14, "time_zone"] <- 2
bikes_data[bikes_data$hour > 13 & bikes_data$hour < 19, "time_zone"] <- 3
bikes_data[bikes_data$hour > 18 & bikes_data$hour < 25, "time_zone"] <- 4
bikes_data$time_zone <- as.factor(bikes_data$time_zone)



ggplot(data=aggregate(bikes_data$count, 
                      list(id = bikes_data$time_zone), 
                      sum), aes(x=as.factor(id), y=x)) +
   geom_bar(stat="identity", fill="steelblue")+
   geom_text(aes(label=x), vjust=1.6, color="white", size=3.5)+
   theme_minimal() + 
   ggtitle("Distribution of Thefts during the months") + 
   xlab("Number of thefts") + 
   ylab("Month") 



#### 8. Thefts' months #####

bikes_data$Occurrence_Month <- as.numeric(bikes_data$Occurrence_Month)

ggplot(data=aggregate(bikes_data$count, 
                      list(id = bikes_data$Occurrence_Month), 
                      sum), aes(x=as.factor(id), y=x)) +
   geom_bar(stat="identity", fill="steelblue")+
   geom_text(aes(label=x), vjust=1.6, color="white", size=3.5)+
   theme_minimal() + 
   ggtitle("Distribution of Thefts during the months") + 
   xlab("Number of thefts") + 
   ylab("Month") + theme(text = element_text(size=20))


#### 
#### 9. Closer look to other variales ####

# let's have a look to other variables

bikes_data$Premise_Type <- as.factor(bikes_data$Premise_Type)
levels(bikes_data$Premise_Type)

Premise_type_info <- bikes_data %>%
   group_by(Premise_Type) %>%
   get_summary_stats(count, type = "mean_sd")

ggplot(data=Premise_type_info, aes(x=Premise_Type, y=n)) +
   geom_bar(stat="identity", fill="steelblue")+
   geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
   theme_minimal()

bikes_data$Bike_Make <- as.factor(bikes_data$Bike_Make)
levels(bikes_data$Bike_Make)

Bike_Make_info <- bikes_data %>%
   group_by(Bike_Make) %>%
   get_summary_stats(count, type = "mean_sd")

Bike_Make_info

#no useful info, variable is a mess

bikes_data$Bike_Type <- as.factor(bikes_data$Bike_Type)
levels(bikes_data$Bike_Type)

Bike_Bike_Type_info <- bikes_data %>%
   group_by(Bike_Type) %>%
   get_summary_stats(count, type = "mean_sd")

Bike_Bike_Type_info

ggplot(data=Bike_Bike_Type_info, aes(x=Bike_Type, y=n)) +
   geom_bar(stat="identity", fill="steelblue")+
   geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
   theme_minimal()

# ok

bikes_data$Bike_Colour <- as.factor(bikes_data$Bike_Colour)
levels(bikes_data$Bike_Colour)

Bike_Colour_info <- bikes_data %>%
   group_by(Bike_Colour) %>%
   get_summary_stats(count, type = "mean_sd")

Bike_Colour_info

ggplot(data=Bike_Colour_info, aes(x=Bike_Colour, y=n)) +
   geom_bar(stat="identity", fill="steelblue")+
   geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
   theme_minimal()

# all a mess

rm(Bike_Bike_Type_info, Bike_Colour_info,
   Bike_Make_info)

###


#### 10. Trying to predict 2019  with 2018 ####
####   10.1 Preparing Dataset ####

hood_cl <- data_tbl[,c(6,24)]
colnames(hood_cl)[1]<- 'Hood_ID'

bikes_data<-merge(x=bikes_data,y=hood_cl,by="Hood_ID",all.x=TRUE)

bike_train <- subset(bikes_data, Occurrence_Year < 2019)
bike_test <- subset(bikes_data, Occurrence_Year == 2019)


bike_train <- aggregate(bike_train$count, 
                        list(cluster = bike_train$cluster,
                             Year    = bike_train$Occurrence_Year,
                             Month   = bike_train$Occurrence_Month),
                        sum)

bike_train$Year <- bike_train$Year - 2013
colnames(bike_train)[4]<- 'Total'

bike_test <- aggregate(bike_test$count, 
                       list(cluster = bike_test$cluster,
                            Year    = bike_test$Occurrence_Year,
                            Month   = bike_test$Occurrence_Month),
                       sum)

bike_test$Year <- bike_test$Year - 2013
colnames(bike_test)[4]<- 'Total'

#### 10.2 XdgBoost

###### Create Matrix ####


library(Matrix)
library(xgboost)

# One Hot Encoding


train_m     <- sparse.model.matrix(Total ~.-1, data=bike_train)
train_z     <- sparse.model.matrix(Year ~.-1, data=bike_train)
train_label <- train_z[,"Total"]
rm(train_z)

train_matrix <- xgb.DMatrix(data= as.matrix(train_m), 
                            label= train_label)

test_m     <- sparse.model.matrix(Total ~.-1, data=bike_test)
test_z     <- sparse.model.matrix(Year ~.-1, data=bike_test)
test_label <- test_z[,"Total"]
rm(test_z)

test_matrix <- xgb.DMatrix(data= as.matrix(test_m), 
                           label= test_label)

###### Create Model ####

watchlist = list(train=train_matrix, test=test_matrix)

mod_1 <-  xgb.train(data=train_matrix, 
                    max.depth=6, 
                    eta=0.1, 
                    nrounds=1000, 
                    eval.metric = "rmse",
                    watchlist = watchlist,
                    objective = "reg:squarederror")

e_1 <- data.frame(mod_1$evaluation_log)
plot(e_1$iter, e_1$train_rmse)

p_1 <- round(predict(mod_1, test_m))

xgb_1_plot <- cbind(as.data.frame(as.matrix(test_m)),
                    test_label)

xgb_1_plot$xgb_1 <-round(p_1)

((sum(p_1))) - (sum(test_label))

(sum(p_1)) / (sum(test_label))

###### GRAFICO ###########

#1 = predicted, 0 = true values

xgb_1_plot_pred <- xgb_1_plot[,-c(7)]
xgb_1_plot_pred$pred <- 1
names(xgb_1_plot_pred)[names(xgb_1_plot_pred) == "xgb_1"] <- "values"

xgb_1_plot_T <- xgb_1_plot[,-c(8)]
xgb_1_plot_T$pred <- "0"
xgb_1_plot_T$pred <- as.numeric(xgb_1_plot_T$pred)
names(xgb_1_plot_T)[names(xgb_1_plot_T) == "test_label"] <- "values"


xgb_1_gg <- rbind(xgb_1_plot_T, xgb_1_plot_pred)
rm(xgb_1_plot_T, xgb_1_plot_pred)

xgb_1_gg$id <- seq(1,96)

xgb_c1 <- subset(xgb_1_gg, cluster1 == "1")
xgb_c1 <- xgb_c1[,-c(2:4)]
colnames(xgb_c1)[1]<- 'cluster'

xgb_c2 <- subset(xgb_1_gg, cluster2 == "1")
xgb_c2$cluster2[xgb_c2$cluster2 == "1"] <- 2
xgb_c2 <- xgb_c2[,-c(1,3,4)]
colnames(xgb_c2)[1]<- 'cluster'

xgb_c3 <- subset(xgb_1_gg, cluster3 == "1")
xgb_c3$cluster3[xgb_c3$cluster3 == "1"] <- 3
xgb_c3 <- xgb_c3[,-c(1,2,4)]
colnames(xgb_c3)[1]<- 'cluster'

xgb_c4 <- subset(xgb_1_gg, cluster4 == "1")
xgb_c4$cluster4[xgb_c4$cluster4 == "1"] <- 4
xgb_c4 <- xgb_c4[,-c(1,2,3)]
colnames(xgb_c4)[1]<- 'cluster'

xgb_plot <- rbind(xgb_c1, xgb_c2,
                  xgb_c3, xgb_c4)

rm(xgb_c1, xgb_c2,
   xgb_c3, xgb_c4)

xgb_plot$cluster <- as.factor(xgb_plot$cluster)

# PLOT for summer 2019 prediction

library(ggpubr)

a <- ggplot(data=subset(xgb_plot, Month >= 6 & Month < 9 & pred == "0"), 
            aes(x=as.factor(Month), y=values, fill=cluster)) +
   geom_bar(stat="identity", position ="dodge") +
   ggtitle("TRUE number of thefts happened in 2019") + 
   xlab("month") +
   geom_text(aes(label=values), color="black", size=4,
             position = position_dodge(width = 1), vjust=1.5)

b <- ggplot(data=subset(xgb_plot, Month >= 6 & Month < 9 & pred == "1"), 
            aes(x=as.factor(Month), y=values, fill=cluster)) +
   geom_bar(stat="identity", position ="dodge") +
   ggtitle("XDGBoost PREDICTED number of thefts happened in 2019") + 
   xlab("month") +
   geom_text(aes(label=values), color="black", size=4,
             position = position_dodge(width = 1), vjust=1.5)

figure <- ggarrange(a, b,
                    labels = c("", ""),
                    ncol = 1, nrow = 2)
figure

N_summary <- data_tbl[,c(7,17:24)]

write.csv(N_summary,'N_summary.csv')



####  10.3 Random Forest ####