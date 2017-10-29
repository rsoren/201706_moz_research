#
# silent_transfers_02_choose_sample.R
#
# Reed Sorensen
# August 2017
#

library(dplyr)
library(XML)
library(rvest)


source("district_ids.R")
df_districts <- read.table(text = txt2, stringsAsFactors = FALSE)
names(df_districts) <- c("Distrito", "Serial", "CP", "CD")

source("silent_transfers/unidade_sanitaria_data.R") # creates 'txt'
df <- read.table(text = txt, stringsAsFactors = FALSE)
names(df) <- c("CP","CD","CUUS","CT","CMB","US","CC","CNE","LAT","LON")

df2 <- df %>%
  select_(.dots = c("CP", "CD", "US", "LAT", "LON")) %>%
  left_join(df_districts[, c("Distrito", "CP", "CD")], by = c("CP", "CD"))

df2$Distrito <- gsub("[^a-zA-Z1-9_.]", "-", df2$Distrito)


D_Beira <- c(
  df2[df2$Distrito == "Cidade_Da_Beira", "US"]
)
D_Nhamatanda <- c(
  "Tica", 
  "Muda", 
  "Lamego", 
  "Hosp._Rural_De_Nhamatanda",
  "Siluvo_Pedreira",
  "Nharuchonga",  
  "Jasse", 
  "Siluvo" )
D_Dondo <- c(
  "Mutua", 
  "Mafambisse", 
  "Bloco_9", 
  "Samora_Machel", 
  "Maxarote", 
  "Dondo", 
  "Canhandula" ) #  NOTE: "Ceramica" excluded b/c not in dataset
D_Cidade_De_Chimoio <- c(
  "1_Maio", 
  "Eduardo_Mondlane", 
  "Nhamaonha", 
  "7_De_Abril" )
D_Gondola <- c(
  df2[df2$Distrito == "Gondola", "US"]
)
  # "Inchope", 
  # "Gondola" ) # NOTE: will add to this list after the map
D_Manica <- c(
  df2[df2$Distrito == "Manica", "US"]
)



# check that all specified facilities are in the dataset
district_name <- "Nhamatanda"
d_tmp <- get(paste0("D_", district_name))
table(d_tmp %in% df2$US)
d_tmp[!d_tmp %in% df2$US]
subset(df2, Distrito == district_name)



df3 <- df2 %>%
  mutate(
    include = 0,
    include = ifelse(US %in% 
        c(D_Beira, D_Nhamatanda, D_Dondo, D_Cidade_De_Chimoio), 1, include),
    d_color = ifelse(include == 1, "blue", "gray"),
    d_color = ifelse(Distrito == "Gondola", "green", d_color),
    d_color = ifelse(Distrito == "Manica", "brown", d_color)
  )
  




locs <- c(32.615379, -19.935049, 35.025358, -18.727954)


library(ggmap)
# moz_map <- get_map(
#   location = c(33.812948, -19.287639),
#   zoom = 8,
#   maptype = "roadmap"
# )
# 
# saveRDS(moz_map, "silent_transfers/moz_map.RDS")
moz_map <- readRDS("silent_transfers/moz_map.RDS")

## -- 
# moz_map2 <- get_map(
#   location = c(33.812948, -19.287639),
#   zoom = 8,
#   maptype = "toner"
# )
# saveRDS(moz_map2, "silent_transfers/moz_map2.RDS")
moz_map2 <- readRDS("silent_transfers/moz_map2.RDS")




source("silent_transfers/beira_corridor_points.R")

km2lat <- function(x) x / 111.12

library(rgeos)
library(sp)
library(raster)
library(geosphere)


# SpatialPoints()

line1 <- Line(as.matrix(route3[,1:2]))
line2 <- Lines(list(line1), ID = "A")
line3 <- raster::spLines(line1)
line4 <- SpatialLinesDataFrame(
  sl = line3, 
  data = df %>% dplyr::select(lon = LON, lat = LAT) %>% mutate(x = 1)
)
line4_fort <- fortify(line4)

poly1 <- gBuffer(spgeom = line4, width = 50)
poly2 <- fortify(poly1)

# convert line to object of class 'SpatialPolygons'
# https://stackoverflow.com/questions/42630703/create-buffer-around-spatial-data-in-r

# x1 = 33
# x2 = 34
# y1 = 19.5
# y2 = 19.6
# 
# myPolygon = Polygon(cbind(c(x1,x1,x2,x2,x1),c(y1,y2,y2,y1,y1)))
# myPolygons = Polygons(list(myPolygon), ID = "A")
# SpPolygon = SpatialPolygons(list(myPolygons))
# spdf = matrix(data = c(0))
# rownames(spdf) = "A"
# spp = SpatialPolygonsDataFrame(SpPolygon, data= as.data.frame(spdf))




library(sp)

poly1 <- rbind(
  line4_fort[,c("long","lat")] %>% mutate(lat=lat+0.00001),
  line4_fort[rev(1:nrow(line4_fort)),c("long","lat")] %>% mutate(lat=lat-0.00001)
)
poly2 <- Polygons(list(Polygon(poly1)),1)
poly3 <- SpatialPolygons(list(poly2))
poly3.df <- SpatialPolygonsDataFrame(Sr = poly3,  data = data.frame(A=1))

buf1 <- gBuffer(spgeom = poly3.df, width = km2lat(21))
buf2 <- gBuffer(spgeom = poly3.df, width = km2lat(18))

ggmap(moz_map) +
  geom_point(data = df3, aes(x = LON, y = LAT), 
    color = "darkblue", size = 2.5, alpha = 0.75) +
  geom_polygon(data = fortify(buf1), aes(x = long, y = lat),
    color = "black", fill = NA) +
    # fill = "darkblue", alpha = 0.35) +
  geom_polygon(data = fortify(buf2), aes(x = long, y = lat),
    fill = "darkred", alpha = 0.15) +
  geom_polygon(data = fortify(poly3.df), aes(x = long, y = lat), 
    color = "black", size = 1) +
  ggsave(
    filename = "tmp.jpg",
    device = "jpeg",
    width = 12, height = 8, units = "in" )
  
df4 <- df3

slotNames(buf1)
tmp <- slot(buf1, "polygons")[[1]]
tmp2 <- slot(tmp, "Polygons")[[1]]
tmp3 <- slot(tmp2, "coords")

library(SDMTools)
df4$in_buffer <- pnt.in.poly(
  pnts = df3[, c("LON", "LAT")],
  poly.pnts = tmp3)[,3]



geosphere::dist2Line( p = c(33.4994, -19.1236),  line = line3 )

buf1 <- gBuffer(line3, width = 0.4)


tmp <- getSpatialPolygonsLabelPoints(buf1)
library(maptools)

d <- route3
coordinates(d) <- ~ lon + lat
projection(d) <- "+init=epsg:4326"

d_mrc <- spTransform(d, CRS = CRS("+init=epsg:4326"))
d_mrc <- spTransform(d, CRS = CRS("+init=epsg:3857"))
d_mrc_bff <- gBuffer(d_mrc, width = 0.4)
library(scales)
plot(moz_map)
plot(d_mrc_bff, col = alpha("blue", .35), add = TRUE)
points(d_mrc, cex = 2, pch = 20)


library(rgdal)
tmp <- rgdal::readOGR("silent_transfers/MOZ_rds/MOZ_roads.shp")


