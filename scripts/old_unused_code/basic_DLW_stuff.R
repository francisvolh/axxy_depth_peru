library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)

init00<-read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/dlw machine results/2021-10-15 INITIALS - BOXES/initials.csv")

head(init00)

summary_init<-init00 %>% 
  group_by(birdID) %>% 
  summarise(
    initValD = mean (DHr),
    initialValO = mean (O18O16r)
  ) %>% 
  mutate(
    initValD = initValD*1000000,
    initialValO = initialValO*1000000
  )


boxes<-read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/boxes_DLW_2019_Boobies.csv")
head(boxes)

boxes_Init<-merge(boxes, summary_init, by.x="BirdBox", by.y="birdID", all=TRUE)


boxes_Init <- boxes_Init %>% 
  rowwise() %>% 
  mutate(
    Avg.Mass = mean(c(mass01, mass02))
  )

#plot(boxes_Init$mass01,boxes_Init$initValD)
#abline(lm(boxes_Init$initValD~boxes_Init$mass01))
#dlw.lm1<-lm(boxes_Init$initValD~boxes_Init$mass01)
#summary(
#  lm(boxes_Init$initValD~boxes_Init$mass01)
#  )

#sqrt(summary(dlw.lm1)$adj.r.squared)


#plot(boxes_Init$mass02,boxes_Init$initValD)
#abline(lm(boxes_Init$initValD~boxes_Init$mass02))

#summary(
#  lm(boxes_Init$initValD~boxes_Init$mass02)
#)

#dlw.lm2 <- lm(boxes_Init$initValD~boxes_Init$mass02)

#sqrt(summary(dlw.lm2)$adj.r.squared)

plot <- ggscatter(data = boxes_Init, 
          x = "Avg.Mass", y = "initialValO" ,
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          #greyed out 2 next lines because adding after
          #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor 
          #cor.coeff.args = list(method = "pearson", #label.x = 3, 
           #                    label.sep = "\n"),
          xlab = "Body mass (g)",
          ylab = "Deuterium (ppm)"
) 
plot +  stat_cor(method = "pearson", label.x = 1400, label.y = 2650)


pivoted <- boxes_Init[ , c("BirdBox", "initValD", "initialValO", "Avg.Mass")] %>% 
  pivot_longer(!c(BirdBox, Avg.Mass),
               names_to = "Isotope",
               values_to = "Values")


## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(boxes_Init$Avg.Mass, boxes_Init$initValD, pch=16, axes=FALSE, #ylim=c(0,1), 
     xlab="", ylab="", 
     type="p",col="black" #, main="Dilution and body mass relationship"
     )
axis(2, #ylim=c(0,1),
     col="black",las=1)  ## las=1 makes horizontal labels
mtext("Deuterium (ppm)",side=2,line=2.5)
box(lwd =2.5)
abline(lm(boxes_Init$initValD~boxes_Init$Avg.Mass))
reg_deut <- summary(lm(boxes_Init$initValD~boxes_Init$Avg.Mass))

lm.out1 <- lm(initValD ~  Avg.Mass , data = boxes_Init)

newx1 <-  seq(min(boxes_Init$Avg.Mass), max(boxes_Init$Avg.Mass), by = 0.05)

conf_interval1 <- predict(lm.out1, newdata=data.frame(Avg.Mass=newx1), interval="confidence",
                          level = 0.95)

matlines(newx1, conf_interval1[,2:3], col = "black", lty=2)


## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(boxes_Init$Avg.Mass, boxes_Init$initialValO, pch=15, axes=FALSE, #ylim=c(0,1), 
     xlab="", ylab="", 
     type="p",col="red")
## a little farther out (line=4) to make room for labels
mtext("Oxygen 18 (ppm)",side=4,col="red",line=4) 
axis(4, #ylim=c(0,7000), 
     col="black",col.axis="red",las=1)

## Draw the time axis
axis(1,pretty(range(boxes_Init$Avg.Mass),10))
mtext("Body mass (g)",side=1,col="black",line=2.5)  

abline(lm(boxes_Init$initialValO~boxes_Init$Avg.Mass), col = "red")
reg_o18 <- summary(lm(boxes_Init$initialValO~boxes_Init$Avg.Mass))


## Add Legend
legend("topright",
       bty = "n",
       legend=c(paste("Deuterium r =", round(sqrt(reg_deut$r.squared),2) ),
                paste("Oxygen 18 r =", round(sqrt(reg_o18$r.squared),2) )),
       text.col=c("black","red"),pch=c(16,15),col=c("black","red"))


boxes_Init$predictedD<-predict(lm(boxes_Init$initValD~boxes_Init$Avg.Mass), 
                                 newdata = boxes_Init[ , c("initValD")])
boxes_Init$predictedO<-predict(lm(boxes_Init$initialValO~boxes_Init$Avg.Mass), 
                               newdata = boxes_Init[ , c("initialValO")])


lm.out2 <- lm(initialValO ~  Avg.Mass , data = boxes_Init)

newx2 <-  seq(min(boxes_Init$Avg.Mass), max(boxes_Init$Avg.Mass), by = 0.05)

conf_interval2 <- predict(lm.out2, newdata=data.frame(Avg.Mass=newx2), interval="confidence",
                          level = 0.95)


matlines(newx2, conf_interval2[,2:3], col = "red", lty=2)


#version 2 of graph 
s<-ggscatter(data = pivoted, 
             x = "Avg.Mass", y = "Values" ,
             color = "black", shape = 21, #size = 3, # Points color, shape and size
             add = "reg.line",  # Add regressin line
             add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
             conf.int = TRUE, # Add confidence interval
             cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
             cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                   label.sep = "\n",
                                   label.x.npc = "center", label.y.npc = "top"),
             xlab = "Body mass (g)",
             facet.by = "Isotope",
             panel.labs = list(Isotope = c("Oxygen 18 (ppm)", "Deuterium (ppm)")),
             scales = "free"
) 

s

#version 3 of graphs
ggplot(data = boxes_Init, aes(x = Avg.Mass))+
  geom_point(aes(y = initValD), color = "red")+
  stat_smooth(aes(y = initValD, x= Avg.Mass),formula = y~x,
              method = "lm",
              color = 'red')+
  geom_point(aes( y = initialValO/3.55), color = "blue")+
  stat_smooth(aes(y = initialValO/3.55, x= Avg.Mass),formula = y~x,
              method = "lm",
              color = 'blue')+
  scale_y_continuous(
    sec.axis = sec_axis(~.x*3.55, name = "Oxygen 18 (ppm)") )+
  labs(x = "Average mass (g)", y = "Deuterium (ppm)") +
  theme_bw()



#######
#######independent graphs

ggscatter(boxes_Init, y = "initValD", 
          x = "predictedD",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "Deuterium vs predicted"
)

ggscatter(boxes_Init, y = "initialValO", 
          x = "predictedO",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "Oxygen-18 vs predicted"
)
