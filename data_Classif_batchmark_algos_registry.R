library(xtable)
library(ggplot2)
library(data.table)
work.dir <- "/scratch/th798/cv-same-other-paper"
reg.RData <- file.path(work.dir, "data_Classif_batchmark_algos_registry.RData")
(objs=load(reg.RData))
msr.list <- mlr3::msrs(c("classif.auc","classif.ce"))
score.dt <- bmr$score(msr.list)

fwrite(
  score.dt[, .(task_id, learner_id, iteration, classif.auc, classif.ce)],
  "data_Classif_batchmark_algos_registry.csv")

score.dt <- fread("data_Classif_batchmark_algos_registry.csv")

levs <- c(
  "featureless",
  "rpart",
  "cv_glmnet",
  "nearest_neighbors",
  "xgboost")
score.dt[, algorithm := factor(sub(".*[.]", "", learner_id), levs)][]
Data <- function(data.name, Data){
  data.table(data.name, Data)
}
disp.dt <- rbind(
  Data("CanadaFires_all","CanadaFiresA"),
  Data("CanadaFires_downSampled","CanadaFiresD"),
  Data("MNIST_EMNIST", "MNIST_E"),
  Data("MNIST_EMNIST_rot", "MNIST_E_rot"),
  Data("MNIST_FashionMNIST","MNIST_Fashion"))
meta.raw <- data.table::fread("data-meta.csv")[
  grepl("train|test",group.small.name), `:=`(
    test=ifelse(group.small.name=="test", group.small.N, group.large.N),
    train=ifelse(group.small.name=="train", group.small.N, group.large.N)
  )
][
, `test%` := as.integer(100*test/rows)
][
, group.type := fcase(
  grepl("MNIST_", data.name), "MNIST",
  !is.na(test), "train/test",
  default="real")
][]
meta.dt <- disp.dt[
  meta.raw, on="data.name"
][is.na(Data), Data := data.name][]
score.dt[
, percent.error := 100*classif.ce
][
, status := ifelse(.N==10, "complete", "not")
, by=.(algorithm,task_id)][]
time.out <- fread("data_Classif_batchmark_algos_registry_time.csv")
score.join <- meta.dt[
  time.out[score.dt, on=.(task_id,learner_id,iteration)],
  on=.(data.name=task_id)
][
, minutes := seconds/60
][]

ggplot()+
  geom_point(aes(
    percent.error, algorithm, color=status),
    shape=1,
    data=score.join)+
  facet_grid(. ~ Data, scales="free")

ggplot()+
  geom_point(aes(
    ifelse(algorithm=="featureless", Inf, percent.error),
    algorithm, color=status),
    shape=1,
    data=score.join)+
  facet_grid(. ~ Data, scales="free")

ggplot()+
  geom_point(aes(
    minutes, algorithm),
    shape=1,
    data=score.join)+
  facet_grid(. ~ Data, scales="free")+
  scale_x_log10()

score.stats <- dcast(
  score.join[
  , hours := minutes/60
  ][
  , days := hours/24
  ][],
  rows + features + status + algorithm + Data ~ .,
  list(mean, sd),
  value.var=c("days", "hours", "minutes", "seconds", "percent.error")
)
ggplot()+
  geom_segment(aes(
    percent.error_mean+percent.error_sd, algorithm,
    xend=percent.error_mean-percent.error_sd, yend=algorithm,
    color=status),
    data=score.stats[algorithm!="featureless"])+
  geom_point(aes(
    ifelse(algorithm=="featureless", Inf, percent.error_mean),
    algorithm, color=status),
    shape=1,
    data=score.stats)+
  facet_grid(. ~ Data, scales="free")

ggplot()+
  geom_segment(aes(
    minutes_mean+minutes_sd, algorithm,
    xend=minutes_mean-minutes_sd, yend=algorithm,
    color=status),
    data=score.stats)+
  geom_point(aes(
    minutes_mean,
    algorithm, color=status),
    shape=1,
    data=score.stats)+
  facet_grid(. ~ Data, scales="free")+
  scale_x_log10()

some.stats <- score.stats[
  Data %in% c("EMNIST","aztrees3")
][
, data := sprintf(
  "%s N=%d, D=%d",
  Data, rows, features)
][]
dd <- unique(some.stats[,.(data,Data)])
blank.dt <- dd[data.table(
  Data=c("EMNIST","aztrees3"),
  x=c(100,1),
  y="featureless"), on="Data"]
gg <- ggplot()+
  geom_segment(aes(
    percent.error_mean+percent.error_sd, algorithm,
    xend=percent.error_mean-percent.error_sd, yend=algorithm),
    data=some.stats)+
  geom_point(aes(
    percent.error_mean,
    algorithm),
    shape=1,
    data=some.stats)+
  geom_blank(aes(x,y),data=blank.dt)+
  geom_text(aes(
    percent.error_mean+ifelse(algorithm=="featureless",-1,1)*percent.error_sd,
    algorithm,
    hjust=ifelse(algorithm=="featureless", 1, 0),
    label=sprintf(
      "%s%.1f±%.1f%s",
      ifelse(algorithm=="featureless",""," "),
      percent.error_mean,
      percent.error_sd,
      ifelse(algorithm=="featureless"," ",""))),
    size=3,
    data=some.stats)+
  scale_x_continuous(
    "Percent test error (mean±SD over 10 folds in CV)")+
  facet_grid(
    ##. ~ Data + rows + features,
    . ~ data,
    scales="free", labeller=label_both)
png("data_Classif_batchmark_algos_registry_error_mean_sd.png", width=6, height=1.5, units="in", res=200)
print(gg)
dev.off()

some.stats[, `:=`(
  time_mean_SD = NA_character_
)][]
for(unit.name in c("day","hour","minute","second")){
  units <- paste0(unit.name,"s")
  unit.col <- paste0(units,"_mean")
  sd.col <- paste0(units,"_sd")
  sum.vec <- some.stats[[unit.col]]
  some.stats[
    sum.vec>1 & is.na(time_mean_SD),
    time_mean_SD := sprintf("%.1f±%.1f %s", get(unit.col), get(sd.col), units)
  ][]
}
some.stats[
, hjust := 0
][
  (algorithm=="xgboost") |
    (algorithm %in% c("nearest_neighbors","cv_glmnet") & Data=="EMNIST")
, hjust := 1]
blank.dt <- dd[data.table(
  Data="aztrees3",
  x=c(0.1,3),
  y="featureless"), on="Data"]
gg <- ggplot()+
  geom_segment(aes(
    minutes_mean+minutes_sd, algorithm,
    xend=minutes_mean-minutes_sd, yend=algorithm),
    data=some.stats)+
  geom_blank(aes(x,y),data=blank.dt)+
  geom_point(aes(
    minutes_mean,
    algorithm),
    shape=1,
    data=some.stats)+
  geom_text(aes(
    minutes_mean+ifelse(hjust==1,-1,1)*minutes_sd,
    algorithm,
    hjust=hjust,
    label=paste0(
      ifelse(hjust==1,""," "),
      time_mean_SD,
      ifelse(hjust==1," ",""))),
    size=3,
    data=some.stats[!is.na(time_mean_SD)])+
  scale_x_log10(
    "Minutes to train (mean±SD over 10 folds in CV)")+
  facet_grid(
    ##. ~ Data + rows + features,
    . ~ data,
    scales="free", labeller=label_both)
png("data_Classif_batchmark_algos_registry_minutes_mean_sd.png", width=6, height=1.5, units="in", res=200)
print(gg)
dev.off()

system("cd /projects/genomic-ml && unpublish_data cv-same-other-paper && publish_data projects/cv-same-other-paper")
