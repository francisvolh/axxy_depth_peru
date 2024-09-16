## merge DLW calculation sheet and HMMclass and  DBAe sheet


#join DLW results and cassification results: DLW_sheet_Boobies_2019.csv

#load DLW sheet 
dlw <- read.csv("E:/05BACKUP July 10 2024/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/DLW_sheet_Boobies_2019v2.csv")  # DLW_sheet_Boobies_2019v2.csv latest version
# C:\Users\francis van oordt\OneDrive - McGill University\Documents\McGill\00Res Prop v2\Chap 1 - DLW axxy


#load classification calculations
merged_summed2 <- read.csv("E:/05BACKUP July 10 2024/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/merged_summed4.csv") #USE merged_summed4.csv! version for latest HMMs and DBAe

#"C:\Users\francis van oordt\OneDrive - McGill University\Documents\McGill\00Res Prop v2\Chap 1 - DLW axxy\axxy_depth_peru\data\processed_acc_third_run\merged_summed3.csv"

calculations <- merge(merged_summed2, dlw, by.x = "dep_id", by.y ="Bird", all = TRUE)

#merge deployments original file  with calculations for male and female variable
raw_dep <- read.csv("E:/05BACKUP July 10 2024/Documents/McGill/Field data/05 GN 2019 Nov/deploymentsAxxys2019.csv", stringsAsFactors = F)

raw_dep <- raw_dep[ , c("dp_ID", "sex")]
#be sure the rows are in the right order
raw_dep$merge.col <- substr(raw_dep$dp_ID, 1, 7)

calculations$merge.col <- substr(calculations$dep_id, 1, 7)


calculationsraw_s <- merge(calculations, raw_dep, by = "merge.col")


calculationsraw_s <-  calculationsraw_s |> 
 dplyr::mutate(
  TotalVeDBA = DBACol+DBAFly+DBAFor+DBARest
  ) 

#############################################################################################
calculations<-calculationsraw_s
calculations$TFlyFor <- calculations$TFly + calculations$TFor
calculations$TColRest <- calculations$TRest + calculations$TCol

calculations$pTFlyFor <- calculations$pTFly + calculations$pTFor
calculations$pTColRest <- calculations$pTRest + calculations$pTCol

calculations$pTFFR <- calculations$pTFly + calculations$pTFor +calculations$pTRest


calculations$DBAFlyFor <- calculations$DBAFly + calculations$DBAFor
calculations$DBAColRest <- calculations$DBARest + calculations$DBACol

calculations$pDBAFlyFor <- calculations$pDBAFly + calculations$pDBAFor
calculations$pDBAColRest <- calculations$pDBARest + calculations$pDBACol

calculations$pDBAFFR <- calculations$pDBAFly + calculations$pDBAFor + calculations$pDBARest

calculations <- calculations |>
 dplyr::mutate(
 pDBA = TotalVeDBA/SampTime#,
 #TotalDEE = DEE.kJ.d*SampTime/24,
#TotalDEEg = TotalDEE/Mass.avg.
)

#because pDBA total and for each activity was the total DBA per deployment divided by sampling time, 
# I had to make it daily multiplying by 24h
vars1 <- c("pDBACol", "pDBAFly", "pDBAFor", "pDBARest","pDBA", "pDBAFlyFor", "pDBAColRest","pDBAFFR")

calculations <- calculations |> 
  dplyr::mutate(
    dplyr::across(all_of(vars1), ~.x*24, .names = paste0("d","{col}"))#total time and ODBA of each activty divided by total Sampling time
)


write.csv(calculations, file ="C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/calculations.csv", row.names = FALSE)
