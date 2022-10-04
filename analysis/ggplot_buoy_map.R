# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/ggplot_buoy_map.R")
   require(ggplot2)
   require(grid)
   require(gridExtra)
   require(PBSmapping)
   require(data.table)

# ================================================================= #
# Load buoys.
   source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/load_buoy_locs.R")

   dd <- NULL
   for (b_idx in 1:length(buoy_list)) {
      eval(parse(text=paste("dd <- rbind(buoy_",rev(buoy_list)[b_idx],",dd)",sep="")))
   }

# ================================================================= #
# Edit here
# ----------------------------------------------------------------- #

# Nudge data S6VT.
   x_nudge <- rep(3,dim(dd)[1])
   y_nudge <- rep(-0.5,dim(dd)[1])

# Distance to coast.
   #vec_dist_coast <- c(67.8,6.3,332,9.7,178.4,5.2,350,31.8,538,20.9,495,31)
   vec_dist_coast <- c(68,6,332,10,178,5,350,32,538,21,495,31)
# Area rectangles and labels.
   df_rect <- data.frame(x1=c(-71,-81,-83.5,-97,-131,-132), x2=c(-65,-73,-75,-91.5,-120,-121), y1=c(40.5,31,27,24.5,36,44.5), y2=c(45,35.5,30,30,40,48), r=c("#1","#2","#3","#4","#5","#6"))

## Get time series durations.
#   path_buoy_data <- "/home/ben/research/waves/buoy_data/NDBC_complete_records/"
#   lab_range <- "Buoy   [Duration]      Dist to coast (km)\n"
#   for (b_idx in 1:length(buoy_list)) {
#   #for (b_idx in 1:5) {
#      data_file <- list.files(path = path_buoy_data, pattern = paste("^",buoy_list[b_idx],"*",sep="") )
#      range_start <- system(paste("sed -n 2p ",path_buoy_data,data_file," | cut -c2-5",sep=""),intern = TRUE)
#      range_end <- system(paste("tail -1 ",path_buoy_data,data_file," | cut -c2-5",sep=""),intern = TRUE)
#      if (b_idx == length(buoy_list)) {
#         lab_range <- paste(lab_range,paste(buoy_list[b_idx]," [",range_start,"-",range_end,"]  ",vec_dist_coast[b_idx],sep=""),sep="")
#      } else {
#         lab_range <- paste(lab_range,paste(buoy_list[b_idx]," [",range_start,"-",range_end,"]  ",vec_dist_coast[b_idx],"\n",sep=""),sep="")
#      }
#   }

## Mapping.
#   #myMap <- get_map(location =  c(-75,20), zoom = 4, maptype = "terrain")
#   #png("./test.png",width = 4000, height = 2400)
#   p1 <- ggplot(data = dd) + #scale_x_continuous(limits = c(-100, -50), expand = c(0,0)) + scale_y_continuous(limits = c(10, 50), expand = c(0,0)) +
#	 ylab("Latitude\n") + xlab("\nLongitude") +
#         coord_map(xlim = c(-98,-65), ylim = c(21,45), projection = "lambert", lat0 = 28, lat1 = 35) +
#	 geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), color = "#000000", fill = NA, size = 0.35) +
#	 geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), color = "#000000", fill = NA, size = 0.15) +
## Buoy symbols.
#         geom_point(aes(x=lon, y=lat), size=4, shape=23, fill="yellow") +
#         geom_label(aes(x=lon, y=lat, label = name), label.padding = unit(0.30, "lines"), size = 4, nudge_x = x_nudge, nudge_y = y_nudge) +
#         geom_label(aes(x=-100, y=37.5, label = lab_range), label.padding = unit(0.40, "lines"), size = 5, hjust=0) +
## Area boxes (in blue).
#         geom_rect(data=df_rect,mapping=aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2), color="blue", alpha=0.0) +
#         geom_text(data=df_rect,aes(x=x1+0.9*(x2-x1),y=y1+0.2*(y2-y1),label=r),size=6) +
## Theme stuff.
#         #theme(axis.title.x = element_text(size = rel(1.8), angle = 00), axis.title.y = element_text(size = rel(1.8), angle = 90))
#         theme(axis.title.x=element_blank(),
#               axis.title.y=element_blank(),
#               axis.text.x = element_text(size = 10),
#               axis.text.y = element_text(size = 10),
#	       #t,r,b,l
#               plot.margin=unit(c(0,1,0,2),"cm") )

   #mapWorld <- map_data("world")
   #setnames(mapWorld, c("X","Y","PID","POS","region","subregion"))
   #worldmap = clipPolys(mapWorld, xlim=c(-179,129),ylim=c(-75,75), keepExtra=TRUE)
   ##worldmap = clipPolys(mapWorld, xlim=c(-50,200),ylim=c(-75,75), keepExtra=TRUE)

   p2 <- ggplot(data = dd) + #scale_x_continuous(limits = c(-100, -50), expand = c(0,0)) + scale_y_continuous(limits = c(10, 50), expand = c(0,0)) +
	 ylab("Latitude\n") + xlab("\nLongitude") +
         #coord_map(xlim = c(175,230), ylim = c(-15,56), projection = "lambert", lat0 = 10, lat1 = 25) +
         coord_map(xlim = c(148,232), ylim = c(-15,58), projection = "bonne", lat0 = 20) +
         #coord_map(xlim = c(140,240), ylim = c(-15,58), projection = "lagrange") +
         #geom_polygon(data = worldmap, aes(X,Y,group=PID)) +
         geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), color = "#000000", fill = NA, size = 0.35) +
         #geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), color = "#000000", fill = NA, size = 0.15) +
# Buoy symbols.
         geom_point(aes(x=buoy_lon, y=buoy_lat, fill=situ), size=4, shape=23) +
         scale_fill_viridis_d() +
         #geom_label(aes(x=buoy_lon, y=buoy_lat, label=name), label.padding = unit(0.30, "lines"), size = 4, nudge_x = x_nudge, nudge_y = y_nudge) +
## Area boxes (in blue).
#         geom_rect(data=df_rect,mapping=aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2), color="blue", alpha=0.0) +
#         geom_text(data=df_rect,aes(x=x1+0.9*(x2-x1),y=y1+0.2*(y2-y1),label=r),size=6) +
# Theme stuff.
         theme(axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10) )
 
   fig_file_name <- paste("./buoys_",str_region,".pdf",sep="")
   mat_lay <- cbind(c(1,1,1),matrix(rep(2,9),ncol=3))
   pdf(fig_file_name,width = 16, height = 12)
   #grid.arrange(p2,p1,layout_matrix=mat_lay)
   plot(p2)
   dev.off()
   system(paste("okular",fig_file_name,"&> /dev/null &"))

