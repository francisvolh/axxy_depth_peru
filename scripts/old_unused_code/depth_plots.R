#simple depth plots with threshold line

for (i in unique(merged.tdr.gps$dep_id)) {
  A<-merged.tdr.gps[which(merged.tdr.gps$dep_id == i),]
  plot(A$time, A$depth*(-1), type = "l")
  abline(h=-0.5, col = "red")
  title(i)
  readline('next')
}
