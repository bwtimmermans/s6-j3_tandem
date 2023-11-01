# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/ggmap_buoy_map.R")
   require(ggplot2)
   require(ggmap)
   require(ggforce)
   require(grid)
   require(gridExtra)
   #require(PBSmapping)
   require(data.table)

# ================================================================= #
# Load buoys.
   source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/load_buoy_locs.R")

   str_region <- "PAC_ALL"
   buoy_list_PAC_NS_lab <- buoy_list_PAC_NS[-c(7:14,20,22,30,33,35,46,48,51,52)][c(1:6,15:18,21:29,31,32,34)]
   buoy_list <- c(buoy_list_PAC_NS_lab,buoy_list_PAC_OS[-c(1:4)])

   dd_temp <- NULL
   for (b_idx in 1:length(buoy_list)) {
      eval(parse(text=paste("dd_temp <- rbind(buoy_",rev(buoy_list)[b_idx],",dd_temp)",sep="")))
   }
   dd <- dd_temp
   dd$situ[25] <- "OS_bias"

# ================================================================= #
# Edit here
# ----------------------------------------------------------------- #

# Nudge data S6VT.
   x_nudge <- rep(3,dim(dd)[1])
   y_nudge <- rep(-0.5,dim(dd)[1])
# Nudge data WISE 2023.
   x_nudge <- rep(2.0,5)
   y_nudge <- rep(-0.5,5)

# Distance to coast.
   #vec_dist_coast <- c(67.8,6.3,332,9.7,178.4,5.2,350,31.8,538,20.9,495,31)
   vec_dist_coast <- c(68,6,332,10,178,5,350,32,538,21,495,31)
# Area rectangles and labels.
   df_rect <- data.frame(x1=c(-71,-81,-83.5,-97,-131,-132), x2=c(-65,-73,-75,-91.5,-120,-121), y1=c(40.5,31,27,24.5,36,44.5), y2=c(45,35.5,30,30,40,48), r=c("#1","#2","#3","#4","#5","#6"))

   #mapWorld <- map_data("world")
   #setnames(mapWorld, c("X","Y","PID","POS","region","subregion"))
   #worldmap = clipPolys(mapWorld, xlim=c(-179,129),ylim=c(-75,75), keepExtra=TRUE)
   #worldmap = clipPolys(mapWorld, xlim=c(-50,200),ylim=c(-75,75), keepExtra=TRUE)

#-------------------------------------------------------------------------------------------------#
# Using Google Maps.
   map1 <- get_map(location = c(lon = -140,lat = 48), zoom = 4, maptype = "satellite")
# Add transparency.
# https://stackoverflow.com/questions/38126102/set-opacity-of-background-map-with-ggmap
   map1_attributes <- attributes(map1)
   map1_transparent <- matrix(adjustcolor(map1,alpha.f = 0.8),nrow = nrow(map1))
   attributes(map1_transparent) <- map1_attributes

# Create plot.
   #p2 <- ggplot(data = dd) + #scale_x_continuous(limits = c(-100, -50), expand = c(0,0)) + scale_y_continuous(limits = c(10, 50), expand = c(0,0)) +
   p2 <- ggmap(map1_transparent) +
	 ylab("Latitude\n") + xlab("\nLongitude") +
         #coord_map(xlim = c(175,230), ylim = c(-15,56), projection = "lambert", lat0 = 10, lat1 = 25) +
# Pacific.
         #coord_map(xlim = c(148,232), ylim = c(-15,58), projection = "bonne", lat0 = 20) +
# Pacific (East).
         #coord_map(xlim = c(190,238), ylim = c(15,58), projection = "bonne", lat0 = 40) +
          #coord_map(xlim = c(140,240), ylim = c(-15,58), projection = "lagrange") +
          #geom_polygon(data = worldmap, aes(X,Y,group=PID)) +
         #geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), color = "#000000", fill = NA, linewidth = 0.35) +
          #geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), color = "#000000", fill = NA, size = 0.15) +
# Buoy symbols.
         geom_point(data = dd, aes(x=buoy_lon, y=buoy_lat, fill=situ), size=9, shape=23, stroke=2) +
         #scale_fill_viridis_d() +
         scale_fill_manual(values = c('blue','yellow','red')) +
         geom_label(data = dd[dd$name %in% buoy_list_PAC_OS[-c(1:4)],], aes(x=buoy_lon, y=buoy_lat, label=name), label.padding = unit(0.30, "lines"), size = 8, nudge_x = x_nudge, nudge_y = y_nudge) +
         geom_label(data = dd[dd$name %in% buoy_list_PAC_NS_lab,], aes(x=buoy_lon, y=buoy_lat, label=name), label.padding = unit(0.30, "lines"), size = 5, nudge_x = x_nudge, nudge_y = y_nudge) +
## Area boxes (in blue).
#         geom_rect(data=df_rect,mapping=aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2), color="blue", alpha=0.0) +
#         geom_text(data=df_rect,aes(x=x1+0.9*(x2-x1),y=y1+0.2*(y2-y1),label=r),size=6) +
# Theme stuff.
         theme(axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.x = element_text(size = 30),
               axis.text.y = element_text(size = 30),
               legend.text = element_text(size = 30,margin = margin(b = 10, t = 10, unit = "pt")),
               legend.title = element_text(size = 30) )

 
   fig_file_name <- paste("./figures/maps/buoys_",str_region,".png",sep="")
   mat_lay <- cbind(c(1,1,1),matrix(rep(2,9),ncol=3))
   png(fig_file_name,width = 1200, height = 1100)
   #grid.arrange(p2,p1,layout_matrix=mat_lay)
   plot(p2)
   dev.off()
   system(paste("okular",fig_file_name,"&> /dev/null &"))

