
DEE<-read.csv(file.choose())

head(DEE)
head(merged_summed2)

 merge.test<- merge(merged_summed2, DEE, by.x="dep_id", by.y="Bird")
 
 
 merge.test %>% 
   filter(HMM=="Flying") %>% 
 ggplot(aes(DEE.KJ.d.g, TotalTimeDay ))+
   geom_point()
 
 forreg1 <- merge.test %>% 
   filter(HMM=="Flying")
 summary(lm(data=forreg1, DEE.KJ.d.g ~ TotalTimeDay)) 
 
 forreg2 <- merge.test %>% 
   filter(HMM=="Colony")
 summary(lm(data=forreg2, DEE.KJ.d.g ~ TotalTimeDay)) 
 
 write.csv(merge.test, "merge.test.DLW.Boobies.csv")
 