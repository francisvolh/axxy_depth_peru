
#split guco 22


dep <- readRDS(file.choose())

setwd("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data")

fn <- list.files('/project/6005805/francisv/axxys pebos gucos', full.names = T, 
                 pattern = 'G22GUCO_20191124_A48')


########################################
#split deployments in files of 4
#dep2<-dep
#dep<-dep2[4:5,]
########################################

#all axxy csv files that are available

dat<-NULL

print(paste('Start:', 'at', format(Sys.time(), "%T")))

for (i in 1:length(dep$dep_id)) { #check if this fix helped now looking at an NA at the end of loop and giving an Error
  
  dd <- dep$dep_id[i]
  idx <- grep(dd, fn)
  if(!identical(idx, integer(0))){
    print(paste('Start:', dd, 'at', format(Sys.time(), "%T")))
    
    #dat <- vroom(fn[idx], col_types= "c?nnncnnnnnndnnn", delim = ',') #reads fast but next steps are slower it seems
    
    dat<-read.csv(fn[idx], stringsAsFactors = FALSE, sep = ",")
  }else{}
}
  print(paste('Finished:', 'at', format(Sys.time(), "%T")))
  
  mid1<-(nrow(dat)/2)
  guco22_1 <- dat[1:mid1,]
  
  write.csv(guco22_1, "G22GUCO_20191124_A48_part1.csv")
  rm(guco22_1)
  
  guco22_2 <- dat[((nrow(dat)/2)+1):nrow(dat),]
  write.csv(guco22_2, "G22GUCO_20191124_A48_part2.csv")
  rm(guco22_2)