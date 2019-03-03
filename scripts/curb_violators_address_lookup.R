library(readr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(ggmap)

library(geosphere)
# library(data.table) -- Need to install
library(sp)

register_google(key = read_file("~/git_p/knxhack/eric_google_api_key.txt"))
#########

### Intake new data -- ! Would like to have to this arranged so that a user can upload a csv up new curb violators
curb_violators_df <- read_csv("~/git_p/knxhack/curbside_violators_master_FAKE_DATA_011719.csv")
names(curb_violators_df) <- c("date","house_num","street","over_flow","not_out","not_at_curb","details","na_1","na_2")
cv_df <- curb_violators_df %>% select_("date","house_num","street","over_flow","not_out","not_at_curb","details") %>% 
  mutate( over_flow = ifelse(is.na(over_flow),0,1)
          ,not_out = ifelse(is.na(not_out),0,1)
          ,not_at_curb = ifelse(is.na(not_at_curb),0,1)
  )

### Add address field to new dataset
cv_df <- cv_df %>% mutate(address = paste0(house_num," ",street," KNOXVILLE TN")) %>%
  mutate(address = gsub("#","",address))


### Find and create a list of addresses already geocoded
cv_hist_geocoded <- read_csv("~/git_p/knxhack/curbside_violators_geocoded.csv")
cv_hist_address_list <- cv_hist_geocoded %>% select_("address","lat","lon","geoAddress") %>% distinct()

cv_addresses_to_geocode_list <- cv_df %>% 
  anti_join(cv_hist_address_list,by=c("address"="address")) %>%
  select_("address") %>% 
  distinct()

### Pass non-geocodded addresses to google api
geocoded <- data.frame(stringsAsFactors = FALSE)

for(i in 19805:nrow(cv_addresses_to_geocode_list)){
  result <- geocode( as.character(cv_addresses_to_geocode_list$address[i]), output = "latlona", source = "google")
  if(T %in% is.na(result)){
    print(paste0("The following address could not be matched: ",as.character(cv_addresses_to_geocode_list$address[i])))
  }else{
    cv_addresses_to_geocode_list$lon[i] <- as.numeric(result[1])
    cv_addresses_to_geocode_list$lat[i] <- as.numeric(result[2])
    cv_addresses_to_geocode_list$geoAddress[i] <- as.character(result[3])
  }
}
options(warn=0)

### Create a list of all geocodded addresses
cv_addresses_to_geocode_list <- cv_addresses_to_geocode_list %>% mutate_all(as.character) 
cv_address_list <- cv_hist_address_list %>% 
  mutate_all(as.character) %>%
  union(cv_addresses_to_geocode_list) %>%
  distinct()

cv_address_list <- cv_address_list %>% filter(!geoAddress == "")

### Join historical geocodded addresses to the address entries queried from the unmatched data

### Create new df containing geocodded address field
cv_df_geo <- cv_df %>% 
  left_join(cv_address_list,by=c("address")) %>% filter(!geoAddress == "")

### Write newly geocodded data
#write_csv(cv_df_geo,"~/git_p/knxhack/curbside_violators_geo.csv")

#########
##### Currently using square geo-fence
long_range <- c(-84.44,-83.51)
lat_range <- c(36.22,36.25)

cv_geo_s <- cv_df_geo %>% mutate(lat = as.numeric(lat)
                                 ,lon = as.numeric(lon)) %>% 
  filter(lat>0 & lon < -80) %>%
  filter( lat >= lat_range[1] & lat <= lat_range[2] &
            lon >= long_range[1] & lon <= long_range[2]) %>% data.frame()

# cv_geo_s <- cv_geo_s[which(cv_geo_s$lat>0 |cv_geo_s$lon < -80),]


########## Cluster Violators (Step 1) ########## 
k.max <- 5 # Maximal number of clusters
cv_geo_s_agg <- cv_geo_s %>% 
  group_by(address, lat, lon, geoAddress) %>% 
  summarise( n = n()
             ,over_flow_n = sum(over_flow)
             ,not_out_n = sum(not_out)
             ,not_at_curb_n = sum(not_at_curb)
             ,address_violation_n = (over_flow_n + not_out_n + not_at_curb_n)
  )

# wss <- sapply(1:k.max,
#               function(k){kmeans(scale(cv_geo_s_agg$not_at_curb), k, nstart=10 )$tot.withinss})
# plot(1:k.max, wss,
#      type="b", pch = 19, frame = FALSE,
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")
# abline(v = 5, lty =2)

##### Preform Clustering -- Enhance later
k <- kmeans(scale(cv_geo_s_agg[c('over_flow_n','not_out_n','not_at_curb_n')]), k.max, nstart=10)

##### Bind cluster assignment to each row
# cv_geo_s_agg <- cbind(cv_geo_s_agg, k_v_cluster)
# colnames(cv_geo_s_agg)[colnames(cv_geo_s_agg)=="k$cluster"] <- "v_cluster"

cv_geo_s_agg <- data.frame(cv_geo_s_agg, v_cluster = k$cluster)




##### Identify worst offending cluster -- This method is simplistic and will need to be enhanced once more fields are added to clusthering alg
##### lpc - denotes lowest preforming cluster 
##### v denotes values cluster
cv_geo_s_cluster_agg <- cv_geo_s_agg %>% group_by(v_cluster) %>% summarize(
  cluster_address_count = n()
  ,avg_n = format(round(mean(n),8),scientific=F)
  ,avg_address_violation_n = mean(address_violation_n)
  ,over_flow_curb_score = mean(over_flow_n)
  ,not_out_score = mean(not_out_n)
  ,not_at_curb_score = mean(not_at_curb_n)
)

### Manually removing the very worst for now. -- This method should be replaced by statistically extreme outliers 
cv_geo_s_cluster_very_bad <- cv_geo_s_cluster_agg %>% filter(avg_address_violation_n >= 5) 
cv_geo_s_cluster_agg <- cv_geo_s_cluster_agg %>% filter(!v_cluster %in% cv_geo_s_cluster_very_bad$v_cluster)

lpc <- cv_geo_s_cluster_agg[which(cv_geo_s_cluster_agg$avg_address_violation_n == max(cv_geo_s_cluster_agg$avg_address_violation_n)),]$v_cluster
cv_geo_s_lpc <- cv_geo_s_agg[which(cv_geo_s_agg$v_cluster==lpc),]

########## Distance Clustering (Step 2) ##########
##### Define distance threshold (m)
d=400

##### Setup lat/lng for distance clustering
x<-cv_geo_s_lpc[which(cv_geo_s_lpc$v_cluster==lpc),]$lon
y<-cv_geo_s_lpc[which(cv_geo_s_lpc$v_cluster==lpc),]$lat

##### use the distm function to generate a geodesic distance matrix in meters
xy <- SpatialPointsDataFrame(
  matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
mdist <- distm(xy)

##### cluster all points using a hierarchical clustering approach so that we can cut the hight of each cluster based on distance
hc <- hclust(as.dist(mdist), method="complete")



##### View dendogram 
# library(dendextend)
# plot(hc, labels=F)
#hc_plot <- as.dendrogram(hc)
# hc_plot %>% get_nodes_attr("height")
# hc_plot %>% nnodes
# hc_plot %>% head

# plot(hc_plot, hang=-1, labels=FALSE)
#plot(hc_plot ,xlim = c(1, 50))

#max(hc_plot %>% labels)

#hc_plot_2 <- cut(hc_plot ,h=d)
#plot(hc_plot_2$upper[[1]], xaxt="n", xlab="")

#twr <- _s_lpc_dist_agg[which(_s_lpc_dist_agg$dist_cluster==703),]
#plot(hc_plot_2$lower[[703]], xaxt="n", xlab="" ,ylab="Distance (m)" ,main="Hierarchal Clustering Example", las=1)

########

##### define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
xy$clust <- cutree(hc, h=d)


##### Bind cluster assignments to s lpc dataframe
cv_geo_s_lpc <- data.frame(cv_geo_s_lpc , clust=xy$clust) 
colnames(cv_geo_s_lpc)[colnames(cv_geo_s_lpc)=="clust"] <- "dist_cluster"


##### Create some descriptive aggregations for each cluster
cv_geo_s_lpc <- cv_geo_s_lpc %>% 
  group_by(dist_cluster) %>% 
  mutate(
    cluster_address_count = n()
    ,avg_n = format(round(mean(n),8),scientific=F)
    ,avg_address_violation_n = mean(address_violation_n)
    ,over_flow_curb_score = mean(over_flow_n)
    ,not_out_score = mean(not_out_n)
    ,not_at_curb_score = mean(not_at_curb_n)
    ,dist_clust_center_lon = mean(lon)
    ,dist_clust_center_lat = mean(lat)
  )

##### Calculate distance from cluster center to make sure everything worked out correctly with distance clustering
data.table::setDT(cv_geo_s_lpc)[ , dist_from_center_m := distGeo(matrix(c(lon, lat), ncol = 2),
                                                                 matrix(c(dist_clust_center_lon, dist_clust_center_lat), ncol = 2))]
max(cv_geo_s_lpc$dist_km)


##### Create a summary table for each distance cluster
cv_geo_s_lpc_dist_agg <- cv_geo_s_lpc %>% 
  group_by(dist_cluster) %>% 
  summarise(
    cluster_address_count = n()
    ,avg_n = format(round(mean(n),8),scientific=F)
    ,avg_address_violation_n = mean(address_violation_n)
    ,over_flow_curb_score = mean(over_flow_n)
    ,not_out_score = mean(not_out_n)
    ,not_at_curb_score = mean(not_at_curb_n)
    ,dist_clust_center_long = mean(lon)
    ,dist_clust_center_lat = mean(lat)
  )


##### Creat a new field that ranks each cluster by total obvservations
cv_geo_s_lpc_dist_agg <- cv_geo_s_lpc_dist_agg[with(cv_geo_s_lpc_dist_agg, order( cluster_address_count , decreasing=TRUE )),] %>% 
  mutate(dist_cluster_rank = row_number())

##### Create df for top 30 clusters by total obvservation to highlight 
cv_geo_s_hc<-cv_geo_s_lpc[which(cv_geo_s_lpc$dist_cluster %in% head(cv_geo_s_lpc_dist_agg$dist_cluster,20) ),]




########## Visualize on map ########## 

##### Setup color pallets
conpal <- colorNumeric("GnBu", cv_geo_s_agg$address_violation_n , na.color = "black", alpha = FALSE,reverse = FALSE)
# conpal <-colorNumeric(colorRamp(c("#23ee00","#bdbdbd", "#0088ba"), interpolate = "spline"), as.numeric(mni_140m_s$weighted_rsrp_rsrq_score) , na.color = "black", alpha = F,reverse = F)
# conpal <-colorNumeric(colorRamp(c("Green","Grey", "Blue"), interpolate = "spline"), as.numeric(mni_140m_s$weighted_rsrp_rsrq_score) , na.color = "black", alpha = F,reverse = F)
conpal2 <- colorNumeric(colorRamp(c("#d68888", "#6f0000"), interpolate = "spline"), as.numeric(cv_geo_s_hc$avg_address_violation_n) , na.color = "black", alpha = F,reverse = F)

##### Create icons for distance cluster center
##### Setup Icons
##### this is modified from: https://github.com/rstudio/leaflet/blob/master/inst/examples/icons.R#L24
pchIcons = function(pch = 1, width = 30, height = 30, bg = "transparent", col = "black", ...) {
  n = length(pch)
  files = character(n)
  # create a sequence of png images
  for (i in seq_len(n)) {
    f = tempfile(fileext = '.png')
    png(f, width = width, height = height, bg = bg)
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = pch[i], col = col[i], cex = min(width, height) / 8, ...)
    dev.off()
    files[i] = f
  }
  files
}

shapes = c(3) # base R plotting symbols (http://www.statmethods.net/advgraphs/parameters.html)
iconFiles = pchIcons(shapes, 20, 20, col = c("Navy"), lwd = 2)

# rock bridge, tn 37022, usa
cv_geo_s <- cv_geo_s %>% filter(!geoAddress == 'rock bridge, tn 37022, usa')
cv_geo_s_agg <- cv_geo_s_agg %>% filter(!geoAddress == 'rock bridge, tn 37022, usa')
##### Setup Visualizaton 
leaflet(cv_geo_s ,options = leafletOptions(zoomControl = F)) %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite",group="Black and White") %>%
  addTiles(group="Open Street Map") %>%
  addProviderTiles("Esri.WorldImagery",group="Satellite") %>%
  addProviderTiles("CartoDB.DarkMatter",group="Dark") %>%
  # setView(-89.36596, 43.08632, zoom = 14) %>% 
  
  addCircleMarkers(lng=as.numeric(cv_geo_s_agg$lon), lat=as.numeric(cv_geo_s_agg$lat),group = "Curb Violators"
                   ,radius=5 
                   ,color = 'purple' 
                   # ,opacity = .5
                   # ,color=~conpal(cv_geo_s_agg$address_violation_n)
                   ,opacity = .8
                   ,weight = 5
                   ,fill = T,fillOpacity = .5
                   ,popup = paste0("<b>address: </b>",cv_geo_s_agg$address,"<br/><b>geoAddress: </b>",cv_geo_s_agg$geoAddress,'<br/><b>address_violation_n: </b>',cv_geo_s_agg$address_violation_n)) %>%
  
  addCircleMarkers(lng=as.numeric(cv_geo_s_hc$lon), lat=as.numeric(cv_geo_s_hc$lat)
                   ,radius=7
                   ,color=~conpal2(as.numeric(cv_geo_s_hc$avg_address_violation_n))
                   ,opacity = .9
                   ,fill = T,fillOpacity = 0.6
                   ,popup=paste0("<b>Cluster ID: </b>",cv_geo_s_hc$dist_cluster,'<br/><b>address_violation_n: </b>',round(cv_geo_s_hc$address_violation_n,4),'<br/><b>Dist Clust Rank: </b>',cv_geo_s_hc$address_violation_n,"<br/><b>Distance From Cluster Center (m): </b>",round(cv_geo_s_hc$dist_from_center_m,2))
                   ,group = "Cluster") %>%
  
  addMarkers(lng=as.numeric(cv_geo_s_hc$dist_clust_center_lon), lat=as.numeric(cv_geo_s_hc$dist_clust_center_lat)
             ,popup = paste0("<b>Cluster ID: </b>",cv_geo_s_hc$dist_cluster,"<br/><b>Number of Points in Cluster: </b>",cv_geo_s_hc$cluster_address_count,"<br/><b>Average address_violation_n: </b>",round(cv_geo_s_hc$avg_address_violation_n,4),"<br/><b>Cluster Center Lat: </b>", round(cv_geo_s_hc$dist_clust_center_lat,5),"<br/><b>Cluster Center Long: </b>",round(cv_geo_s_hc$dist_clust_center_lon,5))
             ,group = "Cluster Center"
             ,icon = ~icons(iconUrl = iconFiles)) %>%
  
  addLayersControl(
    baseGroups = c("Black and White","Open Street Map","Satellite","Dark"),
    overlayGroups = c("Curb Violators","Cluster","Cluster Center"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  # hideGroup(c("")) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions())



