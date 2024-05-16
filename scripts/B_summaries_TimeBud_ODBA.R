############################################################
############################################################
########### Get summaries per bird of Time and DBA #########
############################################################
############################################################

#Loading data (hmms classes, axxy, and interpolated gps) already pre-processed to get summaries 

hmmData<-readRDS('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 3 prop dive/hmm_predictionsv2.RDS')

######################################################
#### 4) Join HMM class and DBAe calcs
######################################################

#calculate the sampling time per BIRD, to merge later with the next summary!!!  
#NEEDS TO BE DONE< the previous method below may be better for this
samptime <- hmmData |>
  dplyr::rename(dep_id = ID)|>
  dplyr::group_by(dep_id) |> 
  dplyr::summarise(
    timestart = head(time, n=1),
    timeend = tail(time, n=1),
    SampTime = as.numeric(- difftime(timestart, timeend), units = 'hours')) # sampling time ba22sed on the tracking device
    #all sampling times match "time.total" from the dlw sheet except the the 100 hours bird dep which has a 74 h samp time, because the device shut down, but bird is eliminated after!
  
# some diftimes have been calculated with another bird
#all_birds[which(all_birds$diftime <0),]

#all_birds[34160:34170, ]          

sum1 <- hmmData |>
  dplyr::rename(dep_id = ID)|>
  dplyr::group_by(behaviour, dep_id) |>
  dplyr::arrange(time)|>
  dplyr::summarise(
    TotalTimeDay = dplyr::n()/60,
    TotalDBADay = sum(vedba.sum, na.rm = TRUE)
  )


sum1_1 <- sum1|>
  dplyr::select("behaviour","dep_id","TotalTimeDay") |>
  tidyr::pivot_wider(names_from = behaviour, values_from = TotalTimeDay) |> 
  dplyr::rename(TCol = Colony, TFly = Commuting, TFor = Foraging, TRest = Resting)
  
sum1_2 <- sum1 |>
  dplyr::select("behaviour","dep_id","TotalDBADay")|> 
  tidyr::pivot_wider(names_from = behaviour, values_from = TotalDBADay)|>
  dplyr::rename(DBACol = Colony, DBAFly = Commuting, DBAFor = Foraging, DBARest = Resting)

merged_summed <- merge(sum1_1, sum1_2, by="dep_id", all=TRUE)

merged_summed <- merge(merged_summed, samptime, by="dep_id", all=TRUE)


#extract variable names
#vars1 <- merged_summed |> select(c("TotalTimeDay", "TotalODBADay")) |> names()

vars1 <- c("TCol", "TFly",  "TFor", "TRest", "DBACol", "DBAFly", "DBAFor", "DBARest")

merged_summed2<-merged_summed |> 
  dplyr::mutate(
    dplyr::across(all_of(vars1), ~.x/SampTime, .names = paste0("p","{col}"))#total time and ODBA of each activty divided by total Sampling time
  )


##
##
##for plotting only

merged_for_plots<-merge(sum1, samptime, by="dep_id", all=TRUE)
names(merged_for_plots)

vars1 <- c( "TotalTimeDay","TotalDBADay")

merged_for_plots<-merged_for_plots |> 
  dplyr::mutate(
    dplyr::across(all_of(vars1), ~.x/SampTime, .names =paste0("p","{col}"))#total time and ODBA divided by total Sampling time
  )


A <- ggplot2::ggplot(merged_for_plots, ggplot2::aes(x = behaviour, y = TotalTimeDay, group = behaviour, color =  behaviour) )+
  ggplot2::geom_boxplot()+
  ggplot2::theme_bw()+
  ggplot2::theme(legend.position = "none")+
  ggplot2::xlab(NULL)+
  ggplot2::theme(axis.text.x=ggplot2::element_blank())

B <- ggplot2::ggplot(merged_for_plots, ggplot2::aes(x = behaviour, y = TotalDBADay, group = behaviour, color =  behaviour) )+
  ggplot2::geom_boxplot()+
  ggplot2::theme_bw()+
  ggplot2::xlab(NULL)+
  ggplot2::theme(legend.position = "none")+
  ggplot2::theme(axis.text.x=ggplot2::element_blank())



C <- ggplot2::ggplot(merged_for_plots, ggplot2::aes(x = behaviour, y = pTotalTimeDay, group = behaviour, color =  behaviour) )+
  ggplot2::geom_boxplot()+
  ggplot2::theme_bw()+
  ggplot2::theme(legend.position = "none")+
  ggplot2::xlab(NULL)#+
  #ggplot2::theme(axis.text.x = ggplot2::element_blank())

D <- ggplot2::ggplot(merged_for_plots, ggplot2::aes(x = behaviour, y = pTotalDBADay, group = behaviour, color =  behaviour) )+
  ggplot2::geom_boxplot()+
  ggplot2::theme_bw()+
  ggplot2::theme(legend.position = "none")+
  #ggplot2::theme(axis.text.x=ggplot2::element_blank())+
  ggplot2::xlab(NULL)


cowplot::plot_grid(A, B, C, D, nrow = 2)


#save merged_summed2 for stats

#write.csv(merged_summed2, "data/merged_summed4.csv")

#merged_summed2.csv is first version with OBDA values calculated to 60 secs
# merged_summed3.csv is the new version with values calculated to 2 secs

#"merged_summed4.csv" new HMMs at 1 min, DBA at 3 sec window, summed over 1 min