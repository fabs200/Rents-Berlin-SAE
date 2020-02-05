
# prepare data for map --------------------------------------------------------------

# dependent variables: rent_unweighted, rent_unweighted_by_bezirk, 
#                      rent_weighted, rent_weighted_by_bezirk

library(rgdal)
library(ggplot2)

# paths to shapefiles
mapspath <- paste0(berlindata, "LOR_SHP_EPSG_3068")
map <- "Bezirksregion_EPSG_3068"

# reading in shapefile
shapefile <- readOGR(dsn = mapspath, layer = map)

# extract shapefile-infos with bezirk-name
shapefile_help <- data.frame(shapefile@plotOrder, shapefile@data$SCHLUESSEL, 
                             shapefile@data$BZR_NAME)
colnames(shapefile_help) <- c("id", "schluessel", "bezirk_name")
shapefile_help$id <- shapefile_help$id - 1
shapefile_help$bezirk_name <- as.character(shapefile_help$bezirk_name)

# convert shapefile
shapefile_df <- fortify(shapefile)
shapefile_df$id <- as.numeric(shapefile_df$id)

# Testing the map: create a random variable for tracing districts
shapefile_df$random <- sample(100, size = nrow(shapefile_df), replace = TRUE)

# merge fortified shapefile with shapefile_help
df_rent_map <- merge(x = shapefile_df, y = shapefile_help, by = "id", all.x = TRUE)

# Testing the map: create a test-variable 'test_value' for tracing districts
df_rent_map$test_value <- 0

# 2 ids seemed to be no valid areas - remove
describe(df_rent_map$id)
length(unique(df_rent_map$id))
trash_id <- which(with(df_rent_map, id == 138 | id == 139))
df_rent_map <- df_rent_map[ -trash_id, ]
df_rent_map <- df_rent_map[order(df_rent_map$id, df_rent_map$piece), ]
describe(df_rent_map$id)
length(unique(df_rent_map$id))

# assign bezirke manually
# "Mitte": 1, "Friedrichshain-Kreuzberg": 2, "Pankow": 3, 
# "Charlottenburg-Wilmersdorf": 4, "Spandau": 5, "Steglitz-Zehlendorf": 6,
# "Tempelhof-Schöneberg": 7, "Neukölln": 8, "Treptow-Köpenick": 9,
# "Marzahn-Hellersdorf": 10, "Lichtenberg": 11, "Reinickendorf": 12

df_rent_map$bezirk_name <- ""
df_rent_map$bezirk <- 0
# NOTE: column 10: bezirk_name, 11: test_value, 12: bezirk

for (i in 0:length(unique(df_rent_map$id))) {
  
  if (i >= 0 & i <= 9) {
    print("Mitte")
    df_rent_map[which(df_rent_map$id == i),10] <- "Mitte"
    df_rent_map[which(df_rent_map$id == i),11] <- 100
    df_rent_map[which(df_rent_map$id == i),12] <- 1
  }
  
  if (i >= 10 & i <= 17) {
    print("Friedrichshain-Kreuzberg")
    df_rent_map[which(df_rent_map$id == i),10] <- "Friedrichshain-Kreuzberg"
    df_rent_map[which(df_rent_map$id == i),11] <- 20
    df_rent_map[which(df_rent_map$id == i),12] <- 2
  }
  
  if ((i >= 18 & i <= 32) | (i == 137)) {
    print("Pankow")
    df_rent_map[which(df_rent_map$id == i),10] <- "Pankow"
    df_rent_map[which(df_rent_map$id == i),11] <- 10
    df_rent_map[which(df_rent_map$id == i),12] <- 3
  }
  
  if (i >= 33 & i <= 49) {
    print("Charlottenburg-Wilmersdorf")
    df_rent_map[which(df_rent_map$id == i),10] <- "Charlottenburg-Wilmersdorf"
    df_rent_map[which(df_rent_map$id == i),11] <- 40
    df_rent_map[which(df_rent_map$id == i),12] <- 4
  }
  
  if (i >= 50 & i <= 58) {
    print("Spandau")
    df_rent_map[which(df_rent_map$id == i),10] <- "Spandau"
    df_rent_map[which(df_rent_map$id == i),11] <- 90
    df_rent_map[which(df_rent_map$id == i),12] <- 5
  }
  
  if (i >= 59 & i <= 66) {
    print("Steglitz-Zehlendorf")
    df_rent_map[which(df_rent_map$id == i),10] <- "Steglitz-Zehlendorf"
    df_rent_map[which(df_rent_map$id == i),11] <- 60
    df_rent_map[which(df_rent_map$id == i),12] <- 6
  }
  
  if (i >= 67 & i <= 73) {
    print("Tempelhof-Schöneberg")
    df_rent_map[which(df_rent_map$id == i),10] <- "Tempelhof-Schöneberg"
    df_rent_map[which(df_rent_map$id == i),11] <- 140
    df_rent_map[which(df_rent_map$id == i),12] <- 7
  }
  
  if (i >= 74 & i <= 83) {
    print("Neukölln")
    df_rent_map[which(df_rent_map$id == i),10] <- "Neukölln"
    df_rent_map[which(df_rent_map$id == i),11] <- 80
    df_rent_map[which(df_rent_map$id == i),12] <- 8
  }
  
  if (i >= 84 & i <= 103) {
    print("Treptow-Köpenick")
    df_rent_map[which(df_rent_map$id == i),10] <- "Treptow-Köpenick"
    df_rent_map[which(df_rent_map$id == i),11] <- 90
    df_rent_map[which(df_rent_map$id == i),12] <- 9
  }
  
  if (i >= 104 & i <= 112) {
    print("Marzahn-Hellersdorf")
    df_rent_map[which(df_rent_map$id == i),10] <- "Marzahn-Hellersdorf"
    df_rent_map[which(df_rent_map$id == i),11] <- 100
    df_rent_map[which(df_rent_map$id == i),12] <- 10
  }
  
  if (i >= 113 & i <= 125) {
    print("Lichtenberg")
    df_rent_map[which(df_rent_map$id == i),10] <- "Lichtenberg"
    df_rent_map[which(df_rent_map$id == i),11] <- 110
    df_rent_map[which(df_rent_map$id == i),12] <- 11
  }
  
  if (i >= 126 & i <= 136) {
    print("Reinickendorf")
    df_rent_map[which(df_rent_map$id == i),10] <- "Reinickendorf"
    df_rent_map[which(df_rent_map$id == i),11] <- 120
    df_rent_map[which(df_rent_map$id == i),12] <- 12
  }
}

# check assignment
table(df_rent_map$bezirk_name, df_rent_map$id)

## now we have an identifier to which we can merge soep-data: df_map$id
  # First: Aggregate df_rent_unit by bezirk_name
df_rent_agg_by_bezirk <- aggregate(df_rent_unit, 
                                   by = list(df_rent_unit$bezirk_name), 
                                   FUN = mean, na.rm = FALSE)
names(df_rent_agg_by_bezirk)[names(df_rent_agg_by_bezirk) == "Group.1"] <- "bezirk_name"

df_rent_agg_by_bezirk <- subset(df_rent_agg_by_bezirk, select = c(1,7,16:19,39, 41:44))
df_rent_agg_by_bezirk <- df_rent_agg_by_bezirk[order(df_rent_agg_by_bezirk$bezirk),]

table(df_rent_agg_by_bezirk$bezirk_name, df_rent_agg_by_bezirk$bezirk)
names(df_rent_agg_by_bezirk)

  # Second: Merge df_rent_agg_by_bezirk to df_map (with example variables)
df_rent_map <- plyr::join(df_rent_map, df_rent_agg_by_bezirk, by = 'bezirk', type = 'left', match = 'all')
df_rent_map[,13] <- NULL # redundant

# check
df_rent_map %>% 
  group_by(bezirk_name) %>% 
  summarise(rent_unweighted = mean(rent_unweighted), sample = mean(samplesize_in_bezirk))

  # Third: Merge df_rent_results to df_map
df_rent_map <- plyr::join(df_rent_map, df_rent_results, by = 'bezirk', 
                          type = 'left', match = 'all')
df_rent_map[,35] <- NULL # redundant

# clean up
rm(list = c("i", "trash_id", "shapefile_df", "shapefile_help", "df_rent_agg_by_bezirk"))

print("-- preparation of maps finished --")

###
