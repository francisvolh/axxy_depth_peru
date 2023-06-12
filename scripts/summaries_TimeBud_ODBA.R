library(dplyr)
library(tidyr)
library(ggplot2)

#summarize behaviours from PEBOs (or GUCOs)
getwd()

#list.files(choose.dir())

setwd("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_first run")

#create a list of names of pre produced outputs from HMM and axxy data
fn2<-list.files(pattern = ".csv")



#merge all birds HMM result data 
all_birds<-NULL #object to bind all birds into


system.time({
  
  for (i in 1:length(fn2)) {
    
    bird1<-read.csv(fn2[i]) #read each bird file
    
    #produce a diff time column
    bird1$diftime <- c(NA, as.numeric( difftime( bird1$time[2:nrow(bird1)], bird1$time[1:(nrow(bird1) - 1)], units = 'hours')))
    
    #bind all data
    all_birds <-rbind(all_birds, bird1)
    
    #sum1<-pivot_wider(sum1, names_from = HMM, values_from = TotalTime)
    #sum1$dep_id <- bird1$dep_id[1]
    #sum1$totalSamp <- as.numeric(- difftime( bird1$time[1], bird1$time[(nrow(bird1))], units = 'hours'))
    #summary_budget <-rbind(summary_budget, sum1)
  }
  
})


#calculate the sampling time per BIRD, to merge later with the next summary!!!  
#NEEDS TO BE DONE< the previous method below may be better for this
samptime <- all_birds %>% 
  group_by(dep_id) %>% 
  summarise(
    timestart = head(time, n=1),
    timeend = tail(time, n=1),
    SampTime = as.numeric(- difftime(timestart, timeend), units = 'hours'))
  
             

sum1 <- all_birds %>% 
  group_by(HMM, dep_id) %>% 
  summarise(
    TotalTimeDay = sum(diftime, na.rm = TRUE),
    TotalODBADay = sum(odba, na.rm = TRUE)
  )

merged_summed <- merge(sum1, samptime, by="dep_id", all=TRUE)


#extract variable names
vars1 <- merged_summed %>% select(c("TotalTimeDay", "TotalODBADay")) %>% names()

merged_summed2<-merged_summed %>% 
  mutate(
    across(all_of(vars1), ~.x/SampTime, .names ="{col}")#total time and ODBA of each activty divided by total Sampling time
  )

names(merged_summed2)

A<-ggplot(merged_summed2, aes(x = HMM, y = TotalTimeDay, group = HMM, color =  HMM) )+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")+
  xlab(NULL)+
  theme(axis.text.x=element_blank())

B<-ggplot(merged_summed2, aes(x = HMM, y = TotalODBADay, group = HMM, color =  HMM) )+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")


cowplot::plot_grid(A, B, nrow = 2)




#get summary of time budgets, not perfect when trying to add OBDA as it is

summary_budget<-NULL

system.time({
  
  for (i in 1:length(fn2)) {
    
    bird1<-read.csv(fn2[i])
    
    bird1$diftime <- c(NA, as.numeric( difftime( bird1$time[2:nrow(bird1)], bird1$time[1:(nrow(bird1) - 1)], units = 'hours')))
    
    
    sum1 <- bird1 %>% 
      group_by(HMM) %>% 
      summarise(
        TotalTime = sum(diftime, na.rm = TRUE),
        TotalODBA = sum(odba, na.rm = TRUE)
      )
    
    sum1<-pivot_wider(sum1, names_from = HMM, values_from = TotalTime)
    
    sum1$dep_id <- bird1$dep_id[1]
    
    sum1$totalSamp <- as.numeric(- difftime( bird1$time[1], bird1$time[(nrow(bird1))], units = 'hours'))
    
    summary_budget <-rbind(summary_budget, sum1)
  }
  
})

head(summary_budget)
vars <- summary_budget %>% select(-dep_id) %>% names()

summary_budget2<-summary_budget %>% 
  mutate(
    across(vars, ~.x/totalSamp, .names ="daily_{col}")
  )

head(summary_budget2)

ggplot(summary_budget) +
  geom_boxplot(c)

ggplot(stack(summary_budget[,c("daily_Colony", 
                               "daily_Flying", 
                               "daily_Foraging", 
                               "daily_Resting")]), 
       aes(x = ind, y = values, group = ind, color =  ind)
       ) +
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "none")
  
