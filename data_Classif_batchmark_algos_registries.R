library(xtable)
library(ggplot2)
library(data.table)
work.dir <- "/scratch/th798/cv-same-other-paper"
reg.RData.vec <- Sys.glob(file.path(
  work.dir, "data_Classif_batchmark_algos/*.RData"))
score.dt.list <- list()
msr.list <- mlr3::msrs(c("classif.auc","classif.ce"))
for(reg.RData in reg.RData.vec){
  (objs=load(reg.RData))
  score.dt.list[[reg.RData]] <- bmr$score(msr.list)
}
score.dt <- rbindlist(score.dt.list)
names(score.dt)
score.dt[1]

fwrite(score.dt[, .(
  task_id,
  algorithm=sub("classif.", "", learner_id),
  fold=iteration,
  classif.auc,
  classif.ce
)], "data_Classif_batchmark_algos_registries.csv")

## after cache.
score.dt <- fread("data_Classif_batchmark_algos_registries.csv")

levs <- c(
  "featureless",
  "rpart",
  "cv_glmnet",
  "nearest_neighbors",
  "xgboost")
algo.fac <- function(algo){
  factor(algo, levs)
}
score.dt[, algorithm := algo.fac(algorithm)][]
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
time.out <- fread(
  "data_Classif_batchmark_algos_registries_time.csv"
)[
, algorithm := sub("classif.", "", learner_id)
][]
score.join <- meta.dt[
  time.out[score.dt, on=.(task_id,algorithm,iteration=fold)],
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
  scale_y_discrete(drop=FALSE)+
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
  Data %in% c("EMNIST","aztrees3","vowel","waveform")
][
, data := sprintf(
  "%s\nN=%d, D=%d",
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
png("data_Classif_batchmark_algos_registry_error_mean_sd.png", width=8, height=1.5, units="in", res=200)
print(gg)
dev.off()


some.stats <- score.stats[Data %in% c(
  "EMNIST",
  ##"aztrees3",
  "vowel","waveform","NSCH_autism")
][
, data := sprintf(
  "%s\nN=%d, D=%d",
  Data, rows, features)
][]
fralgo <- c(
  xgboost="Boosting",
  rpart="Arbre de décision",
  nearest_neighbors="Plus proches voisins",
  featureless="Sans caractères",
  cv_glmnet="Modèle linéaire")
french <- function(DT)DT[, let(
  données = Data,
  Algorithme = fralgo[algorithm]
)][]
(rank.stats <- french(some.stats)[, .(
  rank = rank(percent.error_mean),
  Algorithme
), by=.(Data, data, données)])
(rank.pval <- french(score.join)[
  rank.stats, on=.(Data, données, Algorithme)
][, {
  best <- percent.error[rank==1]
  .SD[
    i  = rank != 1,
    j  = with(t.test(best, percent.error, alternative="l"), data.table(
      err.best=estimate[1],
      err.other=estimate[2],
      err.diff=diff(estimate),
      p.value)),
    by = .(rank,Algorithme=paste0(Algorithme,"-min"))]
}, by = .(Data,data,données)])
text.size <- 3
rank.pval[, let(
  text.x=err.other,
  vjust=0.5
)][Algorithme==paste0(fralgo[["featureless"]],"-min"), let(
  text.x=err.best,
  vjust=1.5
)][]
diff.color <- "red"
dd <- unique(some.stats[,.(données)])
blank.dt <- dd[data.table(
  données="NSCH_autism",
  x=3.2,
  y="Boosting"), on="données"]
gg <- ggplot()+
  ggtitle("Résultats de prédiction de 5 algorithmes d'apprentissage, sur 4 jeux de données")+
  theme_bw()+
  geom_segment(aes(
    err.best, Algorithme,
    xend=err.other, yend=Algorithme),
    color=diff.color,
    data=rank.pval)+
  geom_text(aes(
    text.x, Algorithme,
    vjust=vjust,
    label=sprintf(
      " Diff=%.1f %s",
      err.diff, ifelse(p.value<0.0001, "p<0.0001", sprintf("p=%.4f", p.value))
    )),
    color=diff.color,
    size=text.size,
    hjust=0,
    data=rank.pval)+
  geom_segment(aes(
    percent.error_mean+percent.error_sd, Algorithme,
    xend=percent.error_mean-percent.error_sd, yend=Algorithme),
    data=some.stats)+
  geom_point(aes(
    percent.error_mean,
    Algorithme),
    shape=1,
    data=some.stats)+
  geom_blank(aes(x,y),data=blank.dt)+
  geom_text(aes(
    percent.error_mean+ifelse(Algorithme==fralgo[["featureless"]],-1,1)*percent.error_sd,
    Algorithme,
    hjust=ifelse(Algorithme==fralgo[["featureless"]], 1, 0),
    label=sprintf(
      "%s%.1f±%.1f%s",
      ifelse(Algorithme==fralgo[["featureless"]],""," "),
      percent.error_mean,
      percent.error_sd,
      ifelse(Algorithme==fralgo[["featureless"]]," ",""))),
    size=text.size,
    data=some.stats)+
  ##scale_y_discrete("Algorithme")+
  scale_x_continuous(
    "Taux d'erreur (%) sur l'ensemble test (moyenne ± écart type sur 10 blocs en validation croisée)")+
  facet_wrap(
    ~données, scales="free", labeller=label_both, nrow=2)
  ## facet_grid(
  ##   ##. ~ Data + rows + features,
  ##   . ~ données,
  ##   scales="free", labeller=label_both)
png("data_Classif_batchmark_algos_registry_error_mean_sd_p.png", width=10, height=5, units="in", res=200)
print(gg)
dev.off()



some.stats[, `:=`(
  time_mean_SD = NA_character_
)][]
abbrev.vec <- c(
  days="day",
  hours="hour",
  mins="minute",
  secs="second")
for(abbrev in names(abbrev.vec)){
  unit.name <- abbrev.vec[[abbrev]]
  units <- paste0(unit.name,"s")
  unit.col <- paste0(units,"_mean")
  sd.col <- paste0(units,"_sd")
  sum.vec <- some.stats[[unit.col]]
  some.stats[
    sum.vec>1 & is.na(time_mean_SD),
    time_mean_SD := sprintf("%.1f±%.1f %s", get(unit.col), get(sd.col), abbrev)
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
  theme(axis.text.x=element_text(angle=30, hjust=1))+
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
png("data_Classif_batchmark_algos_registry_minutes_mean_sd.png", width=8, height=1.7, units="in", res=200)
print(gg)
dev.off()

for(meta.i in 1:nrow(meta.dt)){
  meta.row <- meta.dt[meta.i]
  scores.sub <- score.dt[
    meta.row, on=.(task_id=data.name), nomatch=0L
  ][, `:=`(
    prop.correct = 1-classif.ce,
    AUC=classif.auc
  )][]
  if(nrow(scores.sub)){
    scores.long <- melt(
      scores.sub,
      measure.vars=c("AUC","prop.correct")
    )[is.finite(value)]
    scores.wide <- dcast(
      scores.long,
      algorithm + variable ~ .,
      list(mean, sd, length),
      value.var=c("value"))
    levs <- scores.wide[variable=="AUC"][order(value_mean), paste(algorithm)]
    if(length(levs)==0){
      levs <- scores.wide[variable!="AUC"][order(value_mean), paste(algorithm)]
    }
    scores.wide[, Algorithm := factor(algorithm, levs)]
    scores.show <- scores.wide[
      !(algorithm=="featureless" & variable=="AUC")
    ][
    is.na(value_sd), value_sd := 0
    ][, `:=`(
      value_lo=value_mean-value_sd,
      value_hi=value_mean+value_sd
    )][
    , value_mid := (max(value_hi)+min(value_lo))/2
    , by=variable
    ][
    , hjust := ifelse(value_mean<value_mid, 0, 1)
    ][]
    gg <- ggplot()+
      theme_bw()+
      theme(axis.text.x=element_text(angle=30, hjust=1))+
      meta.row[, ggtitle(sprintf(
        "Data set: %s, N=%d, D=%d, classes=%d, imbalance=%.1f",
        data.name,
        rows,
        features,
        classes,
        label.large.N/label.small.N))]+
      geom_point(aes(
        value_mean, Algorithm),
        shape=1,
        data=scores.show)+
      geom_text(aes(
        ifelse(hjust==0, value_hi, value_lo),
        Algorithm,
        hjust=hjust,
        label=sprintf(
          "%s%.3f±%.3f%s%s",
          ifelse(hjust==0, " ", ""),
          value_mean,
          value_sd,
          ifelse(value_length!=10,paste0("(folds=",value_length,")"),""),
          ifelse(hjust==0, "", " ")
        )),
        data=scores.show)+
      geom_segment(aes(
        value_lo, Algorithm,
        xend=value_hi, yend=Algorithm),
        data=scores.show)+
      scale_x_continuous(
        "mean±SD over 10 folds in CV")+
      scale_y_discrete(
        "Algorithm")+
      facet_grid(. ~ variable, scales="free")
    out.png <- sprintf(
      "data_Classif_figures/%s_error_algos_mean_SD.png",
      meta.row$data.name)
    png(out.png, width=10, height=2, units="in", res=200)
    print(gg)
    dev.off()
  }
}

system("cd /projects/genomic-ml && unpublish_data cv-same-other-paper && publish_data projects/cv-same-other-paper")
