
init00<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/dlw machine results/2021-10-15/initials.csv")

head(init00)

summary_init<-init00 %>% 
  group_by(birdID) %>% 
  summarise(
    initValD = mean (DHr),
    initialValO = mean (O18O16r)
  )


boxes<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/boxes_DLW_2019_Boobies.csv")
head(boxes)

boxes_Init<-merge(boxes, summary_init, by.x="BirdBox", by.y="birdID", all=TRUE)

plot(boxes_Init$mass01,boxes_Init$initValD)
abline(lm(boxes_Init$initValD~boxes_Init$mass01))

plot(boxes_Init$mass02,boxes_Init$initValD)
abline(lm(boxes_Init$initValD~boxes_Init$mass02))
