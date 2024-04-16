# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/ggplot_map_REMSEN.R")
# Code to plot ggmap with satellite tracks, bias and correlation, in 2 columns.

# Set seasons to plot.
   vec_ggplot_season <- 1:2

# Sampling radius.
# https://stackoverflow.com/questions/34183049/plot-circle-with-a-certain-radius-around-point-on-a-map-in-ggplot2
   func_make_circles <- function(centers, radius, nPoints = 100) {
      # centers: the data frame of centers with ID
      # radius: radius measured in kilometer
      #
      meanLat <- mean(centers$latitude)
      # length per longitude changes with lattitude, so need correction
      radiusLon <- radius / 111 / cos(meanLat/57.3)
      radiusLat <- radius / 111
      circleDF <- data.frame(buoy_ID = rep(centers$buoy_ID, each = nPoints))
      angle <- seq(0,2*pi,length.out = nPoints)

      circleDF$lon <- unlist(lapply(centers$longitude, function(x) x + radiusLon * cos(angle)))
      circleDF$lat <- unlist(lapply(centers$latitude, function(x) x + radiusLat * sin(angle)))
      return(circleDF)
   }
# Circle for sampling radius, 25, 50 km.
   df_circles <- NULL
   df_circles_25 <- NULL
   df_circles_50 <- NULL
   for (b_idx in 1:length(Bidx)) {
      df_buoy_test <- data.frame(buoy_ID=buoy_list[b_idx_list[Bidx[b_idx]]],df_buoy_data[b_idx_list[Bidx[b_idx]],c(1,2)])
      colnames(df_buoy_test) <- c("buoy_ID","latitude","longitude")
      df_circles <- rbind(df_circles,func_make_circles(df_buoy_test, buoy_radius))
      df_circles_25 <- rbind(df_circles_25,func_make_circles(df_buoy_test, 25))
      df_circles_50 <- rbind(df_circles_50,func_make_circles(df_buoy_test, 50))
   }

#-------------------------------------------------------------------------------------------------#
# Data frame for idealised J3 tracks (from linear model).
   df_plot_J3 <- NULL
   for (b_idx in 1:length(Bidx)) {
      for (kk in 1:length( list_B_list_tr_lm_seq[[b_idx]] )) {
         df_plot_J3 <- rbind(df_plot_J3,data.frame(pred_lon=list_B_list_tr_lm_seq[[b_idx]][[kk]][,1]-360,pred_lat=list_B_list_tr_lm_seq[[b_idx]][[kk]][,2],tr_id=paste0(buoy_list[b_idx_list[b_idx]],"_",kk)))
      }
   }

#-------------------------------------------------------------------------------------------------#
# Data frame for sat bin statistics.
   df_plot_cor <- NULL
   for (b_idx in 1:length(Bidx)) {
      for (kk in 1:length( list_B_list_tr_lm_seq[[b_idx]] )) {
         plot_angle <- 1.2*(180/pi)*atan( (list_B_list_tr_lm_seq[[b_idx]][[kk]][1001,2] - list_B_list_tr_lm_seq[[b_idx]][[kk]][1,2]) / (list_B_list_tr_lm_seq[[b_idx]][[kk]][1001,1] - list_B_list_tr_lm_seq[[b_idx]][[kk]][1,1]) )

         for (season_idx in vec_ggplot_season) {
            if ( !all( is.na( mat_list_cor[[S_idx,b_idx]][[season_idx]][[kk]] ) ) ) {
# Bias and correlation.
# Bias plot. Loop over seasons.
               df_DD <- data.frame(vec_dist_bins_centre,bias=mat_list_bias[[S_idx,b_idx]][[season_idx]][[kk]],cor=mat_list_cor[[S_idx,b_idx]][[season_idx]][[kk]],lon=NA,lat=NA,season=lab_season[season_idx],plot_angle=plot_angle)

               temp_idx <- which( df_DD[,1] == mat_list_bin_coords[[S_idx,b_idx]][[kk]][1,1] )
               df_DD[temp_idx:(temp_idx-1+length(mat_list_bin_coords[[S_idx,b_idx]][[kk]][,3])),4:5] <- mat_list_bin_coords[[S_idx,b_idx]][[kk]][,2:3]
               df_plot_cor <- rbind( df_plot_cor,cbind(df_DD[!is.na(df_DD$cor),],tr_id=paste0(buoy_list[b_idx_list[b_idx]],"_",kk)) )
               #df_plot_cor <- rbind( df_plot_cor,cbind(df_DD[!(is.na(df_DD$cor) | is.na(df_DD$bias)),],tr_id=paste0(buoy_list[b_idx_list[b_idx]],"_",kk)) )
            } else {
               print(paste("Buoy ID:",buoy_idx,"[",buoy_list[buoy_idx],"] No correlation data for track ID:",kk))
            }
         }
      }
   }
   df_plot_cor$lon <- df_plot_cor$lon-360

#-------------------------------------------------------------------------------------------------#
# ggmap plot showing bin correlations (or other statistics).
#-------------------------------------------------------------------------------------------------#
   require(ggplot2)
   require(viridis)
   require(grid)
   require(gridExtra)
#-------------------------------------------------------------------------------------------------#
# Load ggmap.
      require(ggmap)
      #register_google(key = "XXX", write = TRUE)
      #https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
# Using Google Maps.
      if ( flag_OS ) {
         if ( length(Bidx < 3) ) {
            ggplot_zoom <- 7
         } else {
            ggplot_zoom <- 5
         }
      } else {
         if ( length(Bidx < 3) ) {
            ggplot_zoom <- 8
         } else {
            ggplot_zoom <- 5
         }
      }
      ggmap_lon <- range(df_buoy_data[b_idx_list[Bidx],2])[1] + ( range(df_buoy_data[b_idx_list[Bidx],2])[2] - range(df_buoy_data[b_idx_list[Bidx],2])[1] ) / 2
      ggmap_lat <- range(df_buoy_data[b_idx_list[Bidx],1])[1] + ( range(df_buoy_data[b_idx_list[Bidx],1])[2] - range(df_buoy_data[b_idx_list[Bidx],1])[1] ) / 2
      map1 <- get_map(location = c(lon = ggmap_lon, lat = ggmap_lat), zoom = ggplot_zoom, maptype = "satellite")
# Add transparency.
# https://stackoverflow.com/questions/38126102/set-opacity-of-background-map-with-ggmap
      map1_attributes <- attributes(map1)
      map1_transparent <- matrix(adjustcolor(map1,alpha.f = 0.6),nrow = nrow(map1))
      attributes(map1_transparent) <- map1_attributes

#=================================================================================================#
# Set up parameters for plotting variables.
   list_plot <- list()
   vec_plot_leg_title <- c("Bias (m)","Cor")
   list_plot_colour_scale <- list(scale_colour_gradient2(low = "blue", mid="white", high = "red"),scale_colour_viridis_c(option = "plasma"))
   plot_title <- paste0(buoy_list[buoy_idx],"; ",buoy_radius," km sampling; ",dist_bin_width," km bin size")

   fig_file_name <- paste0("./figures/test_sampling3/",buoy_list[buoy_idx],"/ggmap_","M",m_limit,"_",vec_tandem_labs[S_idx],"_",buoy_list[buoy_idx],"_",buoy_radius,"km_season",paste(vec_ggplot_season,collapse=""),"_BIAS_COR1.png")
   system(paste0("if [ ! -d ./figures/test_sampling3/",buoy_list[buoy_idx]," ]; then mkdir ./figures/test_sampling3/",buoy_list[buoy_idx]," &> /dev/null; fi"))

   #df_plot_cor$cor[df_plot_cor$cor < 0.85] <- NA
# Use outlier detection to provide continuous colour scale.
# https://statsandr.com/blog/outliers-detection-in-r/
   df_plot_cor$cor[ which(df_plot_cor$cor %in% boxplot.stats(df_plot_cor$cor)$out) ] <- NA

# Data frame update for plotting.
   list_plot_var <- list()
   list_plot_var[[1]] <- df_plot_cor$bias
   list_plot_var[[2]] <- df_plot_cor$cor

# Loop over bias and correlation to generate 4 panels.
   for ( i_plot_idx in 1:2 ) {

# Details for variables.
      legend_title <- vec_plot_leg_title[i_plot_idx]
      ggplot_colour_scale <- list_plot_colour_scale[[i_plot_idx]]
# Data frame update for plotting.
      df_plot_cor$plot_var <- list_plot_var[[i_plot_idx]]

#-------------------------------------------------------------------------------------------------#
# Create plot.
      list_plot[[i_plot_idx]] <- ggmap(map1_transparent) + 
# Buoy symbol(s).
         geom_point(data=df_buoy_data[b_idx_list[Bidx],],aes(x=buoy_lon, y=buoy_lat), colour="black", fill="yellow", shape=23, size=20, stroke=5) +
# Colour scale.
         ggplot_colour_scale +
# Sampling radius.
         geom_polygon(data = df_circles, aes(lon, lat, group = buoy_ID), col="black", linewidth=3, alpha = 0) +
         geom_polygon(data = df_circles_25, aes(lon, lat, group = buoy_ID), col="grey", linewidth=2, alpha = 0) +
         geom_polygon(data = df_circles_50, aes(lon, lat, group = buoy_ID), col="grey", linewidth=2, alpha = 0) +
# Satellite tracks.
         geom_point(data=df_plot_J3, aes(x=pred_lon, y=pred_lat), col="white", size=1, shape=0) +
# Bin correlation points.
         #geom_point(data=df_plot_cor, aes(x=lon, y=lat, col=cor), fill="black", shape=1, size=10, stroke=5) +
         geom_text(data=df_plot_cor, label = "\u25AE", size=18, aes(x=lon, y=lat, col=plot_var, angle=plot_angle)) +
         facet_wrap(~season, ncol=1) +
# Buoy label.
         geom_label(aes(x=df_buoy_data$buoy_lon[b_idx_list[Bidx]], y=df_buoy_data$buoy_lat[b_idx_list[Bidx]], label=paste(buoy_list[buoy_idx])), label.padding = unit(0.30, "lines"), size = 20, nudge_x = 1.5, nudge_y = 0.7) +
# ERA5 grid cells.
         #geom_point(data = df_plot_ERA5, aes(x=ERA5_lon, y=ERA5_lat, col="4) ERA5 (0.5 deg)"), size=18, shape=10, stroke=3) +
         #geom_segment(data = df_plot_ERA5_box1, aes(x=lon_1, y=lat_1, xend=lon_2, yend=lat_2, col="5) ERA5 bilinear"), linewidth=2) +
# Title.
	 #ggtitle(paste0(buoy_list[buoy_idx],"; ",buoy_radius," km sampling; ",dist_bin_width," km bin size")) +
# Theme stuff.
         theme(panel.background = element_rect(fill = "gray30"),
               panel.grid.major = element_line(linewidth = 2),
               plot.title = element_text(size = 70,hjust = 0.5),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               axis.text.x = element_text(size = 40),
               axis.text.y = element_text(size = 40),
               legend.text = element_text(size = 40,margin = margin(b = 10, t = 10, unit = "pt")),
               #legend.title = element_blank() ) + labs(fill = "50km") +
               legend.title = element_text(size = 50),

               strip.text = element_text(size = 60, margin = margin(25,0,25,0)),
               strip.background = element_rect(fill = "white"),
               panel.spacing.x = unit(1, "lines"),
               panel.spacing.y = unit(2, "lines") ) +
         labs(colour = legend_title) +
         guides(colour=guide_colourbar(barwidth = 5, barheight = 20))
   }
#-------------------------------------------------------------------------------------------------#

   #mat_lay <- cbind(c(1,1,1),matrix(rep(2,9),ncol=3))
   mat_lay <- matrix(c(1,2),ncol=2)
   png(fig_file_name,width=3000,height=1400*length(vec_ggplot_season))
   grid.arrange(list_plot[[1]],list_plot[[2]],layout_matrix=mat_lay,top=textGrob(plot_title,gp=gpar(fontsize=100)))
   #plot(p2)
   dev.off()
   system(paste("okular",fig_file_name,"&> /dev/null &"))

