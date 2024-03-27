library(data.table)
system("wc -l data_Classif_FishSonar.csv")
system("nl data_Classif_FishSonar.csv | grep river")
system("nl data_Classif_FishSonar.csv | grep '[.][0-9]*[.]'")
system("grep river data_Classif_FishSonar.csv | head -1 > data_Classif_FishSonar_processed.csv")
system("cat data_Classif_FishSonar_processed.csv")
system("grep -v river data_Classif_FishSonar.csv | grep -v '[.][0-9]*[.]' >> data_Classif_FishSonar_processed.csv")
FishSonar <- fread("data_Classif_FishSonar_processed.csv")
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
  "_USM1_",
  nc::field("Rec", "", ".*?"),
  "_ss_",
  side=".*?",
  "_",
  last=".*")
count.dt <- nc::capture_first_df(
  FishSonar[, .(pixels=.N), by=.(imgID,label)],
  imgID=imgID.pattern)
## below shows that id1,id2,ymd,Rec are redundant.
count.dt[, .(pixels=sum(pixels)), by=.(id1, id2, ymd, Rec)]
count.dt[, .(ymd, side, last, pixels)]

count.dt[, .(pixels=sum(pixels)), keyby=.(ymd, label)]

    
FishSonar[, which(river=="river")]

ymd.pattern <- list(
  ".*?",
  "_",
  ".*?",
  "_",
  ".*?",
  "_",
  ymd=".*?",
  "_USM1_",
  ".*?",
  "_ss_",
  side=".*?",
  "_",
  ".*")
nc::capture_first_df(
  FishSonar,
  imgID=ymd.pattern)
FishSonar[, y := ifelse(label==1, 1, 0)]
name.vec <- c("ymd", "y", grep("mean", names(FishSonar), value=TRUE))
out.dt <- FishSonar[, name.vec, with=FALSE]
fwrite(out.dt, "data_Classif/FishSonar_day.csv")
