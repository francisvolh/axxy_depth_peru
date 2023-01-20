#HMM_for_gucos tests

dep <- readRDS("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/dep_dataPeru_seabiRds.RDS")

rownames(dep) <- 1:nrow(dep)
dlw_idx<-grep('GUCO', dep$dep_id)
dep<-dep[dlw_idx,]

rownames(dep) <- 1:nrow(dep)
dlw_idx<-grep('A', dep$dep_id)
dep<-dep[dlw_idx,]

