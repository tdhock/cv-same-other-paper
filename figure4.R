library(ggplot2)
library(data.table)
score.atomic <- fread("data_Classif_batchmark_registry.csv")
score.atomic[, `:=`(
  test_subset = test.group,
  train_subsets = train.groups
)][]
Data <- function(data.name, Data){
  data.table(data.name, Data)
}
disp.dt <- rbind(
  Data("CanadaFires_all","CanadaFiresA"),
  Data("CanadaFires_downSampled","CanadaFiresD"),
  Data("MNIST_EMNIST", "IPair_E"),
  Data("MNIST_EMNIST_rot", "IPair_E_rot"),
  Data("MNIST_FashionMNIST","IPair_Fashion"))
meta.raw <- data.table::fread("data-meta.csv")[
  grepl("train|test",group.small.name), `:=`(
    test=ifelse(group.small.name=="test", group.small.N, group.large.N),
    train=ifelse(group.small.name=="train", group.small.N, group.large.N)
  )
][
, `test%` := as.integer(100*test/rows)
][
, subset_type := fcase(
  grepl("MNIST_", data.name), "ImagePair",
  !is.na(test), "train/test",
  default="time/space")
][]
meta.dt <- disp.dt[
  meta.raw, on="data.name"
][is.na(Data), Data := data.name][]
group.meta <- meta.dt[, nc::capture_all_str(
  group.tab,
  test_subset="[^;]+",
  "=",
  subset_rows="[0-9]+", as.integer
), by=data.name]
for(dname in c("NSCH_autism", "FishSonar_river")){
  meta.row <- meta.dt[dname==data.name]
  scores.not <- score.atomic[
    meta.row, on=.(task_id=data.name), nomatch=0L
  ][
    algorithm != "featureless"
  ]
    scores.wide <- dcast(
      scores.not[, let(
        accuracy_prop = 1-classif.ce,
        accuracy_percent = 100*(1-classif.ce),
        AUC = classif.auc
      )],
      data.name + train_subsets + test_subset ~ .,
      list(
        mean, sd, median,
        q75=function(x)quantile(x, 0.75),
        q25=function(x)quantile(x, 0.25)),
      value.var=c("AUC","accuracy_prop"))
  join.wide <- group.meta[scores.wide, on=.(data.name, test_subset)]
  n.subsets <- meta.row$n.groups
  join.long <- melt(
    join.wide,
    id.vars=c("test_subset","subset_rows", "train_subsets"),
    measure.vars=measure(
      metric, value.name,
      pattern="(accuracy_prop|AUC)_(mean|sd)")
  )[, let(
    test = paste0("\n",test_subset),
    rows = paste0("\n",subset_rows)
  )][, let(hjust={
    M <- max(mean+sd)
    m <- min(mean-sd)
    left <- m+(M-m)*0.33
    right <- m+(M-m)*0.67
    fcase(
      mean<left, 0,
      mean>right, 1,
      default=0.5)
  }), by=metric][]
  status = ifelse(
    meta.row$data.name %in% c("NSCH_autism","spam"),
    "similar", "different")
  gg <- ggplot()+
    theme(
      axis.text.x=element_text(angle=30, hjust=1))+
    ggtitle(sprintf(
      "Data set: %s (%d %s subsets)",
      meta.row$data.name, n.subsets, status))+
    geom_point(aes(
      mean, train_subsets),
      shape=1,
      data=join.long)+
    geom_text(aes(
      mean, train_subsets,
      hjust=hjust,
      label=sprintf(
        "%.4f±%.4f", mean,sd)),
      size=3,
      vjust=-0.2,
      data=join.long)+
    geom_segment(aes(
      mean+sd, train_subsets,
      xend=mean-sd, yend=train_subsets),
      data=join.long)+
    facet_grid(test + rows ~ metric, labeller=label_both, scales="free")+
    scale_x_continuous(
      "cv_glmnet performance on CV test subset (mean±SD over 10 folds in CV)")+
    scale_y_discrete(
      "Train subsets",
      limits=c("all","same","other",""))+
    geom_blank(aes(x,y),data=data.frame(x=Inf,y=""))
  out.png <- sprintf(
    "figure4-%s.png",
    meta.row$data.name)
  png(out.png, width=8, height=1+0.75*n.subsets, units="in", res=200)
  print(gg)
  dev.off()
}
