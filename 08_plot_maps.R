
# Plot Maps -------------------------------------------------------------------------

# NOTE: below are 5 Graphs: 2 test maps (variables: "random", "test_value") and 
#       4 maps with rent_unweighted, rent_unweighted_by_bezirk, rent_weighted_by_bezirk 




# Define map style ------------------------------------------------------------------

# function mapStyle, saves place
mapStyle <- function() {
  theme_void() + 
    theme(
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      axis.text.x = element_blank(), axis.text.y = element_blank(),
      axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
      panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), 
      legend.title = element_blank(),
      legend.justification=c(.5,.1), # c(a,b): a=1 left
      legend.position=c(.45,-.025), # <- c(x,y): x=1 right, y=0 bottom
      legend.direction = "horizontal", 
      #legend.position = "right",
      plot.margin = margin(0.1, 0.1, 6 ,0.1, "pt"), # t, r, b, l Dimensions of each margin. (To remember order, think trouble).
      plot.title = element_text(size = 11, color = "#1c5074", hjust = 0.5, vjust = -2.5, face = "bold"), 
      legend.key.height = unit(.2, "cm"), legend.key.width = unit(1.1, "cm")
    ) 
}

# if we want the legend thin on the very right:
#legend.justification=c(0,0), legend.position=c(.958,.1), # <- c(x,y): x=1 right, y=0 bottom
# legend.justification=c(0,0), legend.position=c(.958,.1), # <- c(x,y): x=1 right, y=0 bottom
# legend.direction = "vertical", 
#legend.key.height = unit(.9, "cm"), legend.key.width = unit(0.2, "cm")



# Plot Maps -------------------------------------------------------------------------
# random, test_value, rent_unweighted, Direct, eblupFH, eblupBHF
## test map #1: random
map <- ggplot(df_rent_map, aes(long, lat, group = group, fill = random)) + 
      geom_polygon() + coord_equal() +
      labs(fill = "random variable") + #ggtitle("Districts") +
      theme_bw() + scale_fill_gradient(low = "green", high = "darkred") + 
      mapStyle() + labs(title ="random")
print(map)

# save test map #1
pdf(paste0(projectpath,"/output/01_map_test_random.pdf"), width = 4, height = 3.25)
print(map)
dev.off()

## test map #2: test_value for "bezirk"
map <- ggplot(df_rent_map, aes(long, lat, group = group, fill = test_value)) + 
  geom_polygon() + coord_equal() +
  labs(fill = "test value") + ggtitle("Districts") +
  theme_bw() + scale_fill_gradient(low = "green", high = "darkred") + mapStyle()
#print(map)

# save test map #2
pdf(paste0(projectpath,"/output/02_map_test_bezirk.pdf"), width = 4, height = 3.25)
print(map)
dev.off()

## Map #3: rent_unweighted
map <- ggplot(df_rent_map, aes(long, lat, group = group, fill = rent_unweighted)) + 
  geom_polygon() + coord_equal() +
  #labs(fill = "rent (unweighted)") + #ggtitle("Districts") +
  theme_bw() + scale_fill_gradient(low = "green", high = "darkred") + mapStyle()
#print(map)

# save test map #3
pdf(paste0(projectpath,"/output/03_map_rent_unweighted.pdf"), width = 4, height = 3.25)
print(map)
dev.off()

## Map #4: Direct
map <- ggplot(df_rent_map, aes(long, lat, group = group, fill = Direct)) + 
  geom_polygon() + coord_equal() +
  labs(fill = "Direct") + #ggtitle("Direct") +
  theme_bw() + 
  scale_fill_gradient(low = "green", high = "darkred") + 
  mapStyle() +
  labs(title ="HT")
print(map)

# save test map #4
pdf(paste0(projectpath,"/output/04_map_rent_Direct.pdf"), 
    width = 4, height = 3.25)
print(map)
dev.off()

## Map #5: FH
map <- ggplot(df_rent_map, aes(long, lat, group = group, fill = eblup_FH)) + 
  geom_polygon() + coord_equal() +
  labs(fill = "eblup FH") + #ggtitle("eblup FH") +
  theme_bw() + 
  scale_fill_gradient(low = "green", high = "darkred") + 
  mapStyle() +
  labs(title ="FH")
print(map)

# save test map #5
pdf(paste0(projectpath,"/output/05_map_rent_eblupFH.pdf"), 
    width = 4, height = 3.25)
print(map)
dev.off()

## Map #6: BHF
map <- ggplot(df_rent_map, aes(long, lat, group = group, fill = eblup_BHF)) + 
  geom_polygon() + coord_equal() +
  labs(fill = "eblup BHF") + #ggtitle("eblup BHF") +
  theme_bw() + 
  scale_fill_gradient(low = "green", high = "darkred") + 
  mapStyle() +
  labs(title ="BHF")
print(map)

# save test map #6
pdf(paste0(projectpath,"/output/06_map_rent_eblupBHF.pdf"), 
    width = 4, height = 3.25)
print(map)
dev.off()


# clean up
rm(list = c("map", "shapefile"))


print("-- all plots saved --")

###
