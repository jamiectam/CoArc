require(ggplot2)
require(png)
require(jpeg)
require(grid)
require(ggimage)

#read in image for background
img <- jpeg("underwater.jpg")
#read in background file
ggplot(data = cod_adult_2j3klno, aes(x = distance, y = number)) +
  annotation_custom(rasterGrob(img, 
                               width = unit(1,"npc"),
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point() +
  xlab("Year") +
  ylab("Estimated biomass (t/km^2") +
  ylim(0, 45)

#read in point image
capelin_w_ts$image <- "capelin.jpg"
#plot graph


ggplot(data = bees, aes(x = distance, y = number)) +
  annotation_custom(rasterGrob(img, 
                               width = unit(1,"npc"),
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_image(aes(image = image), size = 0.15) +
  xlab("Distance (km)") +
  ylab("Number of Bees") +
  ylim(0, 45)

#graph with no grey background
capelin_graph<-ggplot(data = capelin_w_ts, aes(x = year, y = capelin_weight)) +
  # annotation_custom(rasterGrob(img, 
  #                              width = unit(1,"npc"),
  #                              height = unit(1,"npc")), 
  #                   -Inf, Inf, -Inf, Inf) +
  geom_image(aes(image = image), size = 0.15) +
  xlab("Year") +
  ylab("Estimated biomass (t/km^2") +
  ylim(0, 45) +
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))