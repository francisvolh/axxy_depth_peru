library(dplyr)
library(tidyr)
library(ggplot2)


########################################################################
### 1) Group HMM Classifications for behaviour models (dba 60 sec window) , it was run in sets of three
########################################################################


#LUMPED HMMS FILES CAN BE LOADED IN LINE 63 !!!!!!!!!!!!!!!!!!!!!!!

#summarize behaviours from PEBOs (or GUCOs)
getwd()

#list.files(choose.dir())

setwd("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run")

#create a list of names of pre produced outputs from HMM and axxy data
fn2<-list.files(pattern = ".csv")

idx <- grep("PEBO", fn2) # to select only summary files from HMM

fn2 <- fn2[idx]


#merge all birds HMM result data 
all_birds<-NULL #object to bind all birds into


system.time({
  
  for (i in 1:length(fn2)) {
    
    set1<-read.csv(fn2[i]) #read each bird file
    
    binding1<-NULL
    
    for (j in unique(set1$dep_id)) {
      
      bird1 <- dplyr::filter(set1, dep_id == j)
      
      bird1$diftime <- c(NA, as.numeric( difftime( bird1$time[2:nrow(bird1)], bird1$time[1:(nrow(bird1) - 1)], units = 'hours')))
      
      binding1 <- rbind(binding1, bird1)
      
    }
    #produce a diff time column
    
    #bind all data
    all_birds <-rbind(all_birds, binding1)
    
    #sum1<-pivot_wider(sum1, names_from = HMM, values_from = TotalTime)
    #sum1$dep_id <- bird1$dep_id[1]
    #sum1$totalSamp <- as.numeric(- difftime( bird1$time[1], bird1$time[(nrow(bird1))], units = 'hours'))
    #summary_budget <-rbind(summary_budget, sum1)
  }
  
})

#lumped HMM runs
write.csv(all_birds, "all_birds_HMMS.csv")
saveRDS(all_birds, "all_birds_HMMS.RDS")


##########################################################################
### 2) Group DBA calculations for DBA energetics (2 sec calculations) 
##########################################################################

setwd("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/dba only fourth run")

#create a list of names of pre produced outputs from HMM and axxy data
fn2<-list.files(pattern = ".csv")

idx <- grep("PEBO", fn2) # to select only summary files from HMM

fn2 <- fn2[idx]


#merge all birds HMM result data 
all_birds<-NULL #object to bind all birds into


system.time({
  
  for (i in 1:length(fn2)) {
    
    set1<-read.csv(fn2[i]) #read each bird file
    
    binding1<-NULL
    
    for (j in unique(set1$dep_id)) {
      
      bird1 <- dplyr::filter(set1, dep_id == j)
      
      bird1$diftime <- c(NA, as.numeric( difftime( bird1$time[2:nrow(bird1)], bird1$time[1:(nrow(bird1) - 1)], units = 'hours')))
      
      binding1 <- rbind(binding1, bird1)
      
    }
    #produce a diff time column
    
    #bind all data
    all_birds <-rbind(all_birds, binding1)
    
    #sum1<-pivot_wider(sum1, names_from = HMM, values_from = TotalTime)
    #sum1$dep_id <- bird1$dep_id[1]
    #sum1$totalSamp <- as.numeric(- difftime( bird1$time[1], bird1$time[(nrow(bird1))], units = 'hours'))
    #summary_budget <-rbind(summary_budget, sum1)
  }
  
})

#lumped HMM runs
write.csv(all_birds, "all_birds_DBAe.csv")
saveRDS(all_birds, "all_birds_DBAe.RDS")
all_DBAe <- all_birds
rm(all_birds)


######################################################
#### 3) Join HMM class and DBAe calcs
######################################################
setwd("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run")

all_birds<-readRDS("all_birds_HMMS.RDS")
all_birds$odba <- NULL

all_DBAe<-readRDS("all_birds_DBAe.RDS")
all_DBAe <- all_DBAe[, c("dep_id" , "time", "odba") ]

all_DBAe<- merge(all_birds, all_DBAe, by = c("dep_id", "time"))

all_birds<-all_DBAe
######################################################
#### 4) Join HMM class and DBAe calcs
######################################################

#calculate the sampling time per BIRD, to merge later with the next summary!!!  
#NEEDS TO BE DONE< the previous method below may be better for this
samptime <- all_birds %>% 
  group_by(dep_id) %>% 
  summarise(
    timestart = head(time, n=1),
    timeend = tail(time, n=1),
    SampTime = as.numeric(- difftime(timestart, timeend), units = 'hours')) # sampling time ba22sed on the tracking device
    #all sampling times match "time.total" from the dlw sheet except the the 100 hours bird dep which has a 74 h samp time, because the device shut down, but bird is eliminated after!
  
# some diftimes have been calculated with another bird
#all_birds[which(all_birds$diftime <0),]

#all_birds[34160:34170, ]          

sum1 <- all_birds %>% 
  group_by(HMM, dep_id) %>% 
  summarise(
    TotalTimeDay = sum(diftime, na.rm = TRUE),
    TotalDBADay = sum(odba, na.rm = TRUE)
  )


sum1_1 <- sum1 %>%
  select("HMM","dep_id","TotalTimeDay") %>% 
  pivot_wider(names_from = HMM, values_from = TotalTimeDay) %>% 
  rename(TCol = Colony, TFly = Flying, TFor = Foraging, TRest = Resting)
  
sum1_2 <- sum1 %>%
  select("HMM","dep_id","TotalDBADay") %>% 
  pivot_wider(names_from = HMM, values_from = TotalDBADay) %>% 
  rename(DBACol = Colony, DBAFly = Flying, DBAFor = Foraging, DBARest = Resting)

merged_summed <- merge(sum1_1, sum1_2, by="dep_id", all=TRUE)

merged_summed <- merge(merged_summed, samptime, by="dep_id", all=TRUE)


#extract variable names
#vars1 <- merged_summed %>% select(c("TotalTimeDay", "TotalODBADay")) %>% names()

vars1 <- c("TCol", "TFly",  "TFor", "TRest", "DBACol", "DBAFly", "DBAFor", "DBARest")

merged_summed2<-merged_summed %>% 
  mutate(
    across(all_of(vars1), ~.x/SampTime, .names = paste0("p","{col}"))#total time and ODBA of each activty divided by total Sampling time
  )


##
##
##for plotting only

merged_for_plots<-merge(sum1, samptime, by="dep_id", all=TRUE)
names(merged_for_plots)

vars1 <- c( "TotalTimeDay","TotalDBADay")

merged_for_plots<-merged_for_plots %>% 
  mutate(
    across(all_of(vars1), ~.x/SampTime, .names =paste0("p","{col}"))#total time and ODBA divided by total Sampling time
  )


A<-ggplot(merged_for_plots, aes(x = HMM, y = TotalTimeDay, group = HMM, color =  HMM) )+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  xlab(NULL)+
  theme(axis.text.x=element_blank())

B<-ggplot(merged_for_plots, aes(x = HMM, y = TotalDBADay, group = HMM, color =  HMM) )+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")


C<-ggplot(merged_for_plots, aes(x = HMM, y = pTotalTimeDay, group = HMM, color =  HMM) )+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  xlab(NULL)+
  theme(axis.text.x=element_blank())

D<-ggplot(merged_for_plots, aes(x = HMM, y = pTotalDBADay, group = HMM, color =  HMM) )+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")


cowplot::plot_grid(A, B, C, D, nrow = 2)


#save merged_summed2 for stats

#write.csv(merged_summed2, "merged_summed3.csv")

#merged_summed2.csv is first version with OBDA values calculated to 60 secs
# merged_summed3.csv is the new version with values calculated to 2 secs

