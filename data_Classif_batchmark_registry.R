library(ggplot2)
library(data.table)
work.dir <- "/scratch/th798/cv-same-other-paper"
reg.RData <- file.path(work.dir, "data_Classif_batchmark_registry.RData")
(objs=load(reg.RData))

meta.dt <- data.table::fread("data-meta.csv")[
  grepl("train|test",small_group), `:=`(
    test=ifelse(small_group=="test", small_N, large_N),
    train=ifelse(small_group=="train", small_N, large_N)
  )
][
, `test%` := as.integer(100*test/rows)
][]
group.meta <- meta.dt[, nc::capture_all_str(
  group.tab,
  test.group="[^;]+",
  "=",
  group.rows="[0-9]+", as.integer
), by=data.name]
score.dt <- mlr3resampling::score(bmr, mlr3::msrs(c("classif.auc","classif.ce")))
score.dt[
, percent.error := 100*classif.ce
][
, data.name := task_id
]
(NA.counts <- score.dt[, .(rows=.N, "NA"=sum(is.na(classif.auc))), by=data.name])
score.atomic <- score.dt[,sapply(score.dt, class)!="list", with=FALSE]
if(FALSE){
  fwrite(score.atomic, "data_Classif_batchmark_registry.csv")
  score.atomic <- fread("data_Classif_batchmark_registry.csv")
}
score.join <- meta.dt[score.atomic, on="data.name"]

tt.join <- score.join[
  grepl("train|test",small_group)
][
, predefined.set := test.group
][]
dot.counts <- dcast(
  tt.join,
  data.name + task_id + `test%` + rows + train.groups + predefined.set ~ algorithm,
  list(
    length, median,
    mean, sd,
    q25=function(x)quantile(x,0.25),
    q75=function(x)quantile(x,0.75)),
  value.var="percent.error")
dot.counts[percent.error_length_featureless!= 10, task_id]
ignore.task <- c("14cancer","khan")
(dot.show <- dot.counts[!task_id %in% ignore.task])

score.show <- tt.join[(!task_id %in% ignore.task) & algorithm!="rpart"]
gg <- ggplot()+
  geom_point(aes(
    percent.error, train.groups, color=algorithm),
    shape=1,
    data=score.show)+
  facet_grid(predefined.set ~  rows + data.name +  `test%`, labeller=label_both)+
  scale_x_continuous(
    "Percent prediction error on CV test set in predefined set (one dot for each of 10 folds in CV)")+
  scale_y_discrete(
    "Predefined set(s) used for CV train set")
png("data_Classif_batchmark_registry_glmnet_featureless.png", width=20, height=4, units="in", res=100)
print(gg)
dev.off()

only.glmnet <- score.show[algorithm=="cv_glmnet"]
ggplot()+
  geom_point(aes(
    percent.error, train.groups),
    shape=1,
    data=only.glmnet)+
  facet_grid(predefined.set ~  rows + data.name +  `test%`, labeller=label_both, scales="free")

meta.long <- melt(
  meta.dt,
  measure.vars=c("test","train"),
  variable.name="predefined.set",
  na.rm=TRUE
)[
  !data.name %in% ignore.task
]
gg <- ggplot()+
  theme(panel.spacing=grid::unit(1, "lines"))+
  geom_text(aes(
    Inf, Inf, label=sprintf("%d rows", value)),
    hjust=1,
    vjust=1.1,
    data=meta.long)+
  geom_segment(aes(
    percent.error_q25_cv_glmnet, train.groups,
    xend=percent.error_q75_cv_glmnet, yend=train.groups),
    data=dot.show)+
  geom_point(aes(
    percent.error_median_cv_glmnet, train.groups),
    shape=1,
    data=dot.show)+
  geom_text(aes(
    percent.error_median_cv_glmnet, train.groups,
    label=sprintf("%.1f", percent.error_median_cv_glmnet)),
    vjust=1.5,
    data=dot.show)+
  facet_grid(predefined.set ~ rows + data.name +  `test%`, labeller=label_both, scales="free")+
  scale_x_continuous(
    "Percent prediction error on CV test set in predefined set (median and quartiles over 10 folds)")+
  scale_y_discrete(
    "Predefined set(s) used for glmnet CV train set")
png("data_Classif_batchmark_registry_glmnet_median_quartiles.png", width=20, height=4, units="in", res=100)
print(gg)
dev.off()

gg <- ggplot()+
  theme(panel.spacing=grid::unit(1, "lines"))+
  geom_text(aes(
    Inf, Inf, label=sprintf("%d rows", value)),
    hjust=1,
    vjust=1.1,
    data=meta.long)+
  geom_segment(aes(
    percent.error_mean_cv_glmnet+percent.error_sd_cv_glmnet,
    train.groups,
    xend=percent.error_mean_cv_glmnet-percent.error_sd_cv_glmnet,
    yend=train.groups),
    data=dot.show)+
  geom_point(aes(
    percent.error_mean_cv_glmnet, train.groups),
    shape=1,
    data=dot.show)+
  geom_text(aes(
    percent.error_median_cv_glmnet, train.groups,
    label=sprintf("%.1f", percent.error_median_cv_glmnet)),
    vjust=1.5,
    data=dot.show)+
  facet_grid(predefined.set ~ rows + data.name +  `test%`, labeller=label_both, scales="free")+
  scale_x_continuous(
    "Percent prediction error on CV test set in predefined set (mean +/- SD over 10 folds)")+
  scale_y_discrete(
    "Predefined set(s) used for glmnet CV train set")
png("data_Classif_batchmark_registry_glmnet_mean_sd.png", width=20, height=4, units="in", res=100)
print(gg)
dev.off()

meta.not.tt <- meta.dt#[is.na(test)]
for(meta.i in 1:nrow(meta.not.tt)){
  cat(sprintf("%4d / %4d data sets\n", meta.i, nrow(meta.not.tt)))
  meta.row <- meta.not.tt[meta.i]
  scores.not <- score.dt[meta.row, on=.(task_id=data.name), nomatch=0L]
  if(nrow(scores.not)){
    gg <- ggplot()+
      ggtitle(paste("Data set:", meta.row$data.name))+
      geom_point(aes(
        percent.error, train.groups, color=algorithm),
        shape=1,
        data=scores.not)+
      facet_grid(. ~ test.group, labeller=label_both, scales="free")+
      scale_x_continuous(
        "Percent prediction error on CV test group (one dot for each of 10 folds in CV)")+
      scale_y_discrete(
        "Train groups")
    out.png <- sprintf(
      "data_Classif_figures/%s_error_glmnet_featureless.png",
      meta.row$data.name)
    png(out.png, width=8, height=2, units="in", res=200)
    print(gg)
    dev.off()
    scores.wide <- dcast(
      scores.not[algorithm =="cv_glmnet"],
      data.name + train.groups + test.group ~ .,
      list(mean, sd),
      value.var="percent.error")
    join.wide <- group.meta[scores.wide, on=.(data.name, test.group)]
    gg <- ggplot()+
      theme_bw()+
      theme(axis.text.x=element_text(angle=30, hjust=1))+
      ggtitle(paste("Data set:", meta.row$data.name))+
      geom_point(aes(
        percent.error_mean, train.groups),
        shape=1,
        data=join.wide)+
      geom_segment(aes(
        percent.error_mean-percent.error_sd, train.groups,
        xend=percent.error_mean+percent.error_sd, yend=train.groups),
        data=join.wide)+
      facet_grid(. ~ test.group + group.rows, labeller=label_both, scales="free")+
      scale_x_continuous(
        "Percent error of cv_glmnet on CV test group (mean±SD over 10 folds in CV)")+
      scale_y_discrete(
        "Train groups")
    out.png <- sprintf(
      "data_Classif_figures/%s_error_glmnet_mean_SD.png",
      meta.row$data.name)
    png(out.png, width=8, height=2, units="in", res=200)
    print(gg)
    dev.off()
  }
}

(meta.binary <- meta.dt[classes==2])
for(meta.i in 1:nrow(meta.binary)){
  meta.row <- meta.binary[meta.i]
  scores.not <- score.dt[
    meta.row, on=.(task_id=data.name), nomatch=0L
  ][
    algorithm != "featureless"
  ]
  if(nrow(scores.not)){
    gg <- ggplot()+
      theme_bw()+
      theme(axis.text.x=element_text(angle=30, hjust=1))+
      ggtitle(paste("Data set:", meta.row$data.name))+
      ## geom_vline(
      ##   xintercept=0.5,
      ##   color="grey")+
      geom_point(aes(
        classif.auc, train.groups),
        shape=1,
        data=scores.not)+
      facet_grid(. ~ test.group, labeller=label_both, scales="free")+
      scale_x_continuous(
        "AUC of cv_glmnet on CV test group (one dot for each of 10 folds in CV)")+
      scale_y_discrete(
        "Train groups")
    out.png <- sprintf(
      "data_Classif_batchmark_registry_glmnet_AUC_%s.png",
      meta.row$data.name)
    png(out.png, width=8, height=2, units="in", res=200)
    print(gg)
    dev.off()
    scores.wide <- dcast(
      scores.not,
      data.name + train.groups + test.group ~ .,
      list(mean, sd),
      value.var="classif.auc")
    join.wide <- group.meta[scores.wide, on=.(data.name, test.group)]
    gg <- ggplot()+
      theme_bw()+
      theme(axis.text.x=element_text(angle=30, hjust=1))+
      ggtitle(paste("Data set:", meta.row$data.name))+
      geom_point(aes(
        classif.auc_mean, train.groups),
        shape=1,
        data=join.wide)+
      geom_segment(aes(
        classif.auc_mean-classif.auc_sd, train.groups,
        xend=classif.auc_mean+classif.auc_sd, yend=train.groups),
        data=join.wide)+
      facet_grid(. ~ test.group + group.rows, labeller=label_both, scales="free")+
      scale_x_continuous(
        "AUC of cv_glmnet on CV test group (mean±SD over 10 folds in CV)")+
      scale_y_discrete(
        "Train groups")
    out.png <- sprintf(
      "data_Classif_figures/%s_AUC_glmnet_mean_SD.png",
      meta.row$data.name)
    png(out.png, width=8, height=2, units="in", res=200)
    print(gg)
    dev.off()
  }
}
system("cd /projects/genomic-ml && unpublish_data cv-same-other-paper && publish_data projects/cv-same-other-paper")
