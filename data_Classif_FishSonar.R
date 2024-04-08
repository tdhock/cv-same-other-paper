library(data.table)
if(FALSE){
  system("wc -l data_Classif_FishSonar.csv")#2815745 data_Classif_FishSonar.csv
  system("nl data_Classif_FishSonar.csv | grep river")#should be one line
  system("nl data_Classif_FishSonar.csv | grep '[.][0-9]*[.]'")#should be none
  system("grep river data_Classif_FishSonar.csv | head -1 > data_Classif_FishSonar_processed.csv")
  system("cat data_Classif_FishSonar_processed.csv")
  system("grep -v river data_Classif_FishSonar.csv | grep -v '[.][0-9]*[.]' >> data_Classif_FishSonar_processed.csv")
}
FishSonar <- fread("data_Classif_FishSonar.csv")
dim(FishSonar)
FishSonar[1]
FishSonar[is.na(as.numeric(`mean_xmin=-1_xmax=8_ymin=0_ymax=9`))]
names(FishSonar)
sapply(FishSonar, class)
FishSonar[, .(V1)]
FishSonar[, .(pixels=.N), by=.(river, imgID)]
FishSonar[, .(pixels=.N), keyby=.(imgID, label)]

imgID.pattern <- list(
  river=".*?",
  "_",
  id1=".*?",
  "_",
  id2=".*?",
  "_",
  ymd=".*?",
  "_",
  service=".*?",
  "_",
  nc::field("Rec", "", ".*?"),
  "_ss_",
  side=".*?",
  "_",
  last="[^_]+",
  suffix=".*")
(count.dt <- FishSonar[, .(pixels=.N), by=.(imgID,label,label_name)])
count.parsed.dt <- nc::capture_first_df(
  count.dt,
  imgID=imgID.pattern)
## below shows that ymd,Rec are not redundant.
count.parsed.dt[, .(pixels=sum(pixels)), keyby=.(ymd, river)]
count.parsed.dt[, .(ymd, side, last, pixels)]

count.parsed.dt[, .(pixels=sum(pixels)), keyby=.(ymd, label_name)]
count.parsed.dt[, .(pixels=sum(pixels)), keyby=.(river, label, label_name)]

    
FishSonar[, which(river=="river")]

ymd.pattern <- list(
  ".*?",
  "_",
  ".*?",
  "_",
  ".*?",
  "_",
  ymd=".*?",
  "_.*?_",
  ".*?",
  "_ss_",
  side=".*?",
  "_",
  ".*")
nc::capture_first_df(
  FishSonar,
  imgID=ymd.pattern)
FishSonar[, y := ifelse(label_name=="Hard Bottom", "hard", "other")]
name.vec <- c("river", "y", grep("mean", names(FishSonar), value=TRUE))
out.dt <- FishSonar[, name.vec, with=FALSE]
fwrite(out.dt, "data_Classif/FishSonar_river.csv")
out.dt <- fread("data_Classif/FishSonar_river.csv")
out.dt[, table(river, y)]
