# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/ggplot_scatter_REMSEN.R")
# Code to plot 4-panel scatter plot of tandem data for publication.

# Loop over four combinations.
   list_multi_plot <- list()

   vec_flag_S6SAR <- c(FALSE,TRUE,FALSE,TRUE)
   vec_flag_S6SAR_correction1 <- c(FALSE,FALSE,FALSE,TRUE)
   vec_flag_J3BUOY <- c(FALSE,FALSE,TRUE,FALSE)

   for ( i_plot_idx in 1:4 ) {

# Setup for S6 LRM or S6 SAR. Default is J3 / S6 LRM.
      flag_S6SAR <- vec_flag_S6SAR[i_plot_idx]
      flag_S6SAR_correction1 <- vec_flag_S6SAR_correction1[i_plot_idx]
      flag_J3BUOY <- vec_flag_J3BUOY[i_plot_idx]

      tandem_idx <- c(1,2)
      if ( flag_S6SAR ) {
         tandem_idx[2] <- 3
      } else if ( flag_J3BUOY ) {
         tandem_idx[2] <- 4
      }

# Direct calculation of median (from older code).
      lab_pairing <- "DIRECTPAIR"

# Calculate medians (more tracks for J3 than S6 due to missing data!).
      vec_buoy_hs_coloc_ALL1 <- NULL
      vec_Q50_J3_ALL_BUOY <- NULL
      vec_Q50_J3_ALL <- NULL
      vec_Q50_S6LRM_ALL <- NULL
      vec_Q50_S6SAR_ALL <- NULL
      Lvec_qual_numval_J3_ALL <- NULL

      for (b_idx in 1:length(Bidx)) {
         mat_list_1Hz_hs <- list_B_mat_list_1Hz_hs[[b_idx]]
         mat_list_1Hz_numval <- list_B_mat_list_1Hz_numval[[b_idx]]
         mat_list_breaks_master <- list_B_mat_list_breaks_master[[b_idx]]
         mat_list_mean_time <- list_B_mat_list_mean_time[[b_idx]]
         vec_buoy_time_num <- list_B_vec_buoy_time_num[[b_idx]]
         vec_buoy_hs <- list_B_vec_buoy_hs[[b_idx]]

         for ( m_idx in 1:m_limit ) {
# Identify matching time slots across missions.
# Buoys.
            #mat_slot_J3B <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { abs(list_mean_time[[1]][x] - vec_buoy_time_num) < 1800 } )
            mat_list_slot_J3B <- sapply( X=1:length(mat_list_mean_time[[m_idx,1]]),FUN=function(x) { abs(mat_list_mean_time[[m_idx,1]][x] - vec_buoy_time_num) < 1800 & abs(mat_list_mean_time[[m_idx,1]][x] - vec_buoy_time_num) == min( abs(mat_list_mean_time[[m_idx,1]][x] - vec_buoy_time_num) ) } )

            vec_buoy_hs_coloc <- rep(NA,length(mat_list_mean_time[[m_idx,1]]))
            vec_buoy_hs_coloc[which(mat_list_slot_J3B,arr.ind=T)[,2]] <- vec_buoy_hs[which(mat_list_slot_J3B,arr.ind=T)[,1]]
            vec_buoy_hs_coloc_ALL1 <- c(vec_buoy_hs_coloc_ALL1,vec_buoy_hs_coloc)

            vec_Q50_J3_BUOY <- sapply( X=1:length(mat_list_breaks_master[[m_idx,1]]),FUN=function(x) { median( mat_list_1Hz_hs[[m_idx,1]][ mat_list_breaks_master[[m_idx,1]][[x]] ] , na.rm=T ) } )
            vec_Q50_J3_ALL_BUOY <- c(vec_Q50_J3_ALL_BUOY,vec_Q50_J3_BUOY[apply(X=mat_list_slot_J3B,MAR=2,FUN=any)])
# S6 LRM.
            if ( tandem_idx[2] == 2 ) {
               mat_slot_J3S6L <- sapply( X=1:length(mat_list_mean_time[[m_idx,1]]),FUN=function(x) { abs(mat_list_mean_time[[m_idx,1]][x] - mat_list_mean_time[[m_idx,2]]) < 40 } )
# J3.
               vec_Q50_J3 <- sapply( X=1:length(mat_list_breaks_master[[m_idx,1]]),FUN=function(x) { median( mat_list_1Hz_hs[[m_idx,1]][ mat_list_breaks_master[[m_idx,1]][[x]] ] , na.rm=T ) } )
               vec_Q50_J3_ALL <- c(vec_Q50_J3_ALL,vec_Q50_J3[apply(X=mat_slot_J3S6L,MAR=2,FUN=any)])

               vec_Q50_S6LRM <- sapply( X=1:length(mat_list_breaks_master[[m_idx,2]]),FUN=function(x) { median( mat_list_1Hz_hs[[m_idx,2]][ mat_list_breaks_master[[m_idx,2]][[x]] ] , na.rm=T ) } )
               vec_Q50_S6LRM_ALL <- c(vec_Q50_S6LRM_ALL,vec_Q50_S6LRM[apply(X=mat_slot_J3S6L,MAR=1,FUN=any)])
# J3 QC.
#               Lvec_qual_numval_J3 <- sapply( X=1:length(mat_list_breaks_master[[m_idx,1]]),FUN=function(x) { any( mat_list_1Hz_numval[[m_idx,1]][ mat_list_breaks_master[[m_idx,1]][[x]] ] < 9 ) } )
#               Lvec_qual_numval_J3_ALL <- c(Lvec_qual_numval_J3_ALL,Lvec_qual_numval_J3[apply(X=mat_slot_J3S6L,MAR=2,FUN=any)])
            }
# S6 SAR.
            if ( flag_S6SAR ) {
               mat_slot_J3S6H <- sapply( X=1:length(mat_list_mean_time[[m_idx,1]]),FUN=function(x) { abs(mat_list_mean_time[[m_idx,1]][x] - mat_list_mean_time[[m_idx,3]]) < 40 } )
# J3.
               vec_Q50_J3 <- sapply( X=1:length(mat_list_breaks_master[[m_idx,1]]),FUN=function(x) { median( mat_list_1Hz_hs[[m_idx,1]][ mat_list_breaks_master[[m_idx,1]][[x]] ] , na.rm=T ) } )
               vec_Q50_J3_ALL <- c(vec_Q50_J3_ALL,vec_Q50_J3[apply(X=mat_slot_J3S6H,MAR=2,FUN=any)])

               vec_Q50_S6SAR <- sapply( X=1:length(mat_list_breaks_master[[m_idx,3]]),FUN=function(x) { median( mat_list_1Hz_hs[[m_idx,3]][ mat_list_breaks_master[[m_idx,3]][[x]] ] , na.rm=T ) } )
               vec_Q50_S6SAR_ALL <- c(vec_Q50_S6SAR_ALL,vec_Q50_S6SAR[apply(X=mat_slot_J3S6H,MAR=1,FUN=any)])
            }
         }
      }
# Set Get paired data.
      if ( tandem_idx[2] == 2 ) {
         vec_sat1_paired <- vec_Q50_J3_ALL
         vec_sat2_paired <- vec_Q50_S6LRM_ALL
      } else if ( tandem_idx[2] == 3 ) {
         vec_sat1_paired <- vec_Q50_J3_ALL
         vec_sat2_paired <- vec_Q50_S6SAR_ALL
      } else if ( tandem_idx[2] == 4 ) {
         vec_sat1_paired <- vec_Q50_J3_ALL_BUOY
         vec_sat2_paired <- vec_buoy_hs_coloc_ALL1[!is.na(vec_buoy_hs_coloc_ALL1)]
      }
# Old QC scheme.
      #vec_sat1_paired <- vec_Q50_J3_ALL[!Lvec_qual_numval_J3_ALL]
      #vec_sat2_paired <- vec_Q50_S6LRM_ALL[!Lvec_qual_numval_J3_ALL]
      vec_data_diff <- vec_sat1_paired-vec_sat2_paired
# Create initial data frame for plotting.
      df_plot_tandem <- data.frame(
				   J3_sat_hs=vec_sat1_paired,
				   S6_sat_hs=vec_sat2_paired )

#=================================================================================================#
# Fit linear model correction to S6SAR.
      if ( flag_S6SAR & flag_S6SAR_correction1 ) {
# Model.
         lm_J3mS6SAR = lm(J3_sat_hs ~ S6_sat_hs + I(S6_sat_hs^(1/2)), data=df_plot_tandem)
         #vec_hs_QC <- predict.lm(object=lm_J3mS6SAR,newdata=data.frame(S6SAR=vec_hs))
         Lvec_hs_outlier <- abs(lm_J3mS6SAR$residuals) > 3*sqrt(var(lm_J3mS6SAR$residuals))

         vec_data_diff <- lm_J3mS6SAR$residuals[!Lvec_hs_outlier]
         vec_sat2_paired <- lm_J3mS6SAR$fitted.values[!Lvec_hs_outlier]
         vec_sat1_paired <- vec_sat1_paired[!Lvec_hs_outlier]
# Plot labels.
         plot_y_lab <- paste("(D) LM[J3 ~ S6 HR] Residuals")
         file_name_lab <- "S6_SAR_LM"
      } else if ( flag_S6SAR & ! flag_S6SAR_correction1 ) {
         plot_title_lab <- paste0("Hs anomaly, region ",str_region)
         plot_y_lab <- paste("(C) J3 - S6 HR (m)")
         file_name_lab <- "S6_SAR"
      } else if ( flag_J3BUOY ) {
         plot_title_lab <- paste0("Hs anomaly, region ",str_region)
         plot_y_lab <- paste("(B) J3 - Buoy (m)")
         file_name_lab <- "BUOY"
      } else {
         plot_title_lab <- paste0("Hs anomaly, region ",str_region)
         plot_y_lab <- paste("(A) J3 - S6 LR (m)")
         file_name_lab <- "S6_LRM"
      }
      plot_title_lab <- paste0("Hs anomaly, region ",str_region,", ",buoy_radius,"km")

# New DF for plotting.
# Save df_plot for multipanel plot.
      list_multi_plot[[i_plot_idx]] <- data.frame(vec_J3=vec_sat1_paired,vec_data=vec_sat2_paired,J3_data_diff=vec_data_diff,y_lab=plot_y_lab)
   }

# Generate the master df for plotting.
   df_multi_plot <- NULL
   df_multi_plot_legend <- NULL

   for ( i_plot_idx in 1:4 ) {
      df_multi_plot <- rbind( df_multi_plot, list_multi_plot[[i_plot_idx]] )

# Recover original df for statistics.
      df_plot_tandem1 <- list_multi_plot[[i_plot_idx]]
# Regression.
      lm_hs <- lm(vec_data ~ vec_J3,data=df_plot_tandem1)
# RMSD
      hs_rmsd <- sqrt( mean( ( df_plot_tandem1$vec_J3 - df_plot_tandem1$vec_data )^2 ,na.rm=T ) )
# RMSE
      #if ( flag_S6SAR & flag_S6SAR_correction1 ) {
      if ( i_plot_idx == 4 ) {
         #hs_rmse <- sqrt(mean(lm_J3mS6SAR$residuals[!Lvec_hs_outlier]^2))
         hs_rmse <- sqrt(mean(df_plot_tandem1$J3_data_diff^2))
      } else {
         hs_rmse <- sqrt(mean(lm_hs$residuals^2))
      }
# Correlation.
      hs_cor <- cor(df_plot_tandem1$vec_J3,df_plot_tandem1$vec_data,use="pairwise.complete.obs")
# Bias.
      hs_bias <- mean(df_plot_tandem1$vec_J3) - mean(df_plot_tandem1$vec_data)
      #high_bias <- 0.75
      #hs_high_bias <- quantile(vec_sat1_paired,probs=high_bias) - quantile(vec_sat2_paired,probs=high_bias)
# Label for plotting.
      label=paste0("Mean bias (J3 mean = ",format(mean(df_plot_tandem1$vec_J3),digits=2),"): ",format(hs_bias,digits=2),
             "\nCorrelation: ",format(hs_cor,digits=3),
	     "\nRMSD: ",format(round(hs_rmsd,3),nsmall=2),
	     "\nRMSE: ",format(round(hs_rmse,3),nsmall=2),
	     "\nN: ",sum(!is.na(df_plot_tandem1$vec_J3)) )
      df_multi_plot_legend <- rbind( df_multi_plot_legend, data.frame(label=label,y_lab=list_multi_plot[[i_plot_idx]]$y_lab[1]) )
   }

# https://r-graph-gallery.com/2d-density-plot-with-ggplot2
# https://stats.stackexchange.com/questions/12392/how-to-compare-two-datasets-with-q-q-plot-using-ggplot2
      require(ggplot2)
      fig_scatter_filename <- paste0("./figures/test_sampling3/OS/ggplot_scatter_anomaly_MULTI2x2-",file_name_lab,"_",buoy_radius,"km_",lab_pairing,".png")
# Data for Q-Q plot.
      #qq_data <- as.data.frame(qqplot(df_plot_tandem$J3_sat_hs, df_plot_tandem$S6_sat_hs, plot.it=FALSE))

      p1 <- ggplot(df_multi_plot, aes(x=vec_J3, y=J3_data_diff) ) +
         geom_hex(bins = 70) +
         xlim(0,10) + ylim(-1.5,1.5) +
         ggtitle(plot_title_lab) + xlab(paste(vec_tandem_labs[tandem_idx[1]],"(m)")) + ylab("Residual (m)") +
         geom_abline(slope = 0, intercept = 0, colour = "red", linewidth = 5) +
#         #geom_label(x=1, y=1.5, hjust=0, label=paste0("Correlation:",format(hs_cor,digits=3),"\n","RMSD: ",format(round(hs_rmsd,3),nsmall=2),"\n","RMSE:",format(round(hs_rmse,3),nsmall=2)), size=24, label.padding = unit(2.0, "lines")) +
         geom_text(data=df_multi_plot_legend, aes(label=label), x=0, y=1.15, hjust=0, lineheight=1, size=20) +
#         geom_text(x=0, y=1.15, hjust=0, lineheight = 1,
#	       	label=paste0("Mean bias (J3 mean = ",format(mean(df_plot_tandem1$vec_J3),digits=2),"): ",format(hs_bias,digits=2),
#      		             "\nCorrelation: ",format(hs_cor,digits=3),
#			     "\nRMSD: ",format(round(hs_rmsd,3),nsmall=2),
#			     "\nRMSE: ",format(round(hs_rmse,3),nsmall=2),
#			     "\nN: ",sum(!is.na(df_plot_tandem1$vec_J3))), size=20) +
         scale_fill_continuous(type = "viridis") +
         facet_wrap(~y_lab,ncol=2) +
         theme(
            plot.title = element_text(size = 110, hjust = 0.5, margin = margin(t = 50, r = 0, b = 50, l = 0)),
            axis.title.x=element_text(size = 100),
            axis.title.y=element_text(size = 100, margin = margin(t = 0, r = -20, b = 0, l = 0)),
            #panel.grid.minor = element_blank(),
            #panel.grid.major = element_blank(),
            #panel.background = element_rect(fill = "black"),

            strip.text = element_text(size = 80, margin = margin(25,25,25,25)),
            strip.background = element_rect(fill = "white"),
            panel.spacing.x = unit(3, "lines"),
            panel.spacing.y = unit(3, "lines"),
            axis.text.y = element_text(size = 80, margin = unit(c(5, 5, 5, 5), "mm")),
            axis.text.x = element_text(size = 80, margin = unit(c(5, 5, 5, 5), "mm")),
            axis.ticks.x = element_blank(),

            legend.position = "right",
            legend.margin = margin(0,75,0,0),
            legend.key.width = unit(1.5, "inch"),
            legend.key.height = unit(2, "inch"),
            legend.title = element_text(size = 75, margin = margin(25,0,0,0)),
            legend.title.align = 0.5,
            legend.text = element_text(size = 60, margin = margin(0,0,0,25))
         )
#      #geom_point(data = qq_data, aes(x=x, y=y), color = "orange", size = 6)

      png(fig_scatter_filename, width = 3000, height = 3000)
      plot(p1)
      dev.off()
      system(paste("okular",fig_scatter_filename,"&> /dev/null &"))

