library(ggplot2)
library(data.table)
work.dir <- "/scratch/th798/cv-same-other-paper"
reg.RData <- file.path(work.dir, "data_Classif_batchmark_registry.RData")
(objs=load(reg.RData))

meta.dt <- data.table::fread("data-meta.csv")[
  grepl("train|test",group.small.name), `:=`(
    test=ifelse(group.small.name=="test", group.small.N, group.large.N),
    train=ifelse(group.small.name=="train", group.small.N, group.large.N)
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
  score.atomic.noerr <- fread("data_Classif_batchmark_registry_noerr.csv")
## > setdiff(unique(score.atomic$data.name), unique(score.atomic.old$data.name))
## [1] 
## > setdiff(unique(score.atomic.old$data.name), unique(score.atomic$data.name))
## [1] "QMNIST"   "spam"     "STL10"    "vowel"    "waveform" "zipUSPS"
##EMNIST has 1 error in new, take all from noerr, plus MNIST_* data from new.
  score.both <- rbind(
    score.atomic[data.name%in%c("MNIST_EMNIST","MNIST_EMNIST_rot","MNIST_FashionMNIST")],
    score.atomic.noerr)
  (fold.counts <- dcast(score.both, data.name + train.groups + test.group ~ algorithm, length))
  fold.counts[grepl("MNIST", data.name)]
}
score.join <- meta.dt[score.dt, on="data.name"]

(tab.wide <- dcast(
  score.join[algorithm=="cv_glmnet"],
  data.name + test.group + test.fold ~ train.groups,
  value.var="percent.error"))
(tab.long <- melt(
  tab.wide,
  measure=c("other","all"),
  variable.name="compare_name",
  value.name="compare_error"
)[, {
  test.res <- t.test(compare_error, same, paired=TRUE)
  log.signed.p <- function(signed.p){
    sign(signed.p)*abs(log10(abs(signed.p)))
  }
  with(test.res, data.table(
    estimate,
    p.value,
    log10.p=log10(p.value),
    sign.log.p=log.signed.p(p.value*sign(estimate)),
    N=.N))
}, by=.(data.name,compare_name)])
tab.long[order(compare_name,-estimate)]
tab.long[order(compare_name,-sign.log.p)]

(compare.wide.each <- dcast(
  tab.long,
  data.name + test.group ~ compare_name,
  value.var="estimate"))
ggplot()+
  geom_point(aes(
    other, all, color=data.name),
    data=compare.wide.each)

(compare.wide <- dcast(
  tab.long,
  data.name ~ compare_name,
  value.var=c("estimate","sign.log.p","log10.p")))
(xt <- meta.dt[compare.wide, .(data.name, rows, features, classes, n.groups, "all-same"=estimate_all), on="data.name"][!grepl("MNIST_",data.name)][order(-`all-same`)])
library(xtable)
print(xtable(xt), type="latex")
gg <- ggplot()+
  theme_bw()+
  geom_hline(yintercept=0,color="grey")+
  geom_vline(xintercept=0,color="grey")+
  geom_point(aes(
    estimate_other, estimate_all, color=data.name),
    data=compare.wide)+
  ggrepel::geom_label_repel(aes(
    estimate_other, estimate_all, color=data.name, label=data.name),
    data=compare.wide)+
  coord_equal()+
  theme(legend.position="none")+
  scale_x_continuous(
    "Other-Same, mean percent error, over 10 test folds and all test groups")+
  scale_y_continuous(
    "All-Same, mean percent error,\nover 10 test folds and all test groups")
png("data_Classif_batchmark_registry_scatter_other_all.png", width=7, height=3.5, units="in", res=200)
print(gg)
dev.off()
zoom <- gg+
  scale_x_continuous(
    "Other-Same (mean percent error over 10 test folds and all test groups)",
    limits=c(-1, 1))+
  scale_y_continuous(
    "All-Same (mean percent error over 10 test folds and all test groups)",
    limits=c(-2.5, 0))
png("data_Classif_batchmark_registry_scatter_other_all_similar.png", width=6, height=6, units="in", res=200)
print(zoom)
dev.off()
tlab <- function(x, y, label){
  data.table(x, y, label)
}
text.dt <- rbind(
  tlab(4, 0, "No significant difference"),
  tlab(4, -11, "Highly\nsignificant\ndifference"),
  tlab(-2, -7, "Beneficial\nto combine"),
  tlab(8, -7, "Detrimental\nto combine"))
gg <- ggplot(mapping=aes(
  estimate_all, log10.p_all, color=data.name))+
  ggtitle("Is it beneficial to combine groups?")+
  theme_bw()+
  ##geom_hline(yintercept=0,color="grey")+
  geom_vline(xintercept=0,color="grey")+
  geom_text(aes(
    x, y, label=label, color=NULL),
    vjust=0,
    data=text.dt)+
  geom_point(
    data=compare.wide)+
  ggrepel::geom_text_repel(aes(
    label=data.name),
    size=2.5,
    data=compare.wide)+
  theme(legend.position="none")+
  scale_y_continuous(
    "log10(p-value)",
    breaks=seq(-10,0,by=2))+
  scale_x_continuous(
    "Percent test error difference (same-all)",
    breaks=seq(-10,10,by=2),
    limits=c(-3,NA))
png("data_Classif_batchmark_registry_scatter_all.png", width=5, height=4, units="in", res=200)
print(gg)
dev.off()
text.dt <- rbind(
  ## tlab(15, 0, "No significant difference"),
  ## tlab(15, -40, "Highly\nsignificant\ndifference"),
  tlab(0, -30, "Same\naccuracy"),
  tlab(70, -30, "Worse accuracy"))
ymin <- -2
ymax <- 0
xmin <- -1
xmax <- 1
rect.dt <- data.table(xmin, xmax, ymin, ymax)
gg <- ggplot()+
  ggtitle("Accurate prediction on a new group?")+
  geom_rect(aes(
    xmin=xmin,
    xmax=xmax,
    ymin=ymin,
    ymax=ymax),
    color="grey50",
    fill=NA,
    data=rect.dt)+
  theme_bw()+
  ##geom_hline(yintercept=0,color="grey")+
  geom_vline(xintercept=0,color="grey")+
  geom_text(aes(
    x, y, label=label, color=NULL),
    vjust=0,
    data=text.dt)+
  geom_point(aes(
    estimate_other, log10.p_other, color=data.name),
    data=compare.wide)+
  ggrepel::geom_text_repel(aes(
    estimate_other,
    log10.p_other,
    color=data.name,
    label=data.name),
    size=2.5,
    data=compare.wide)+
  theme(legend.position="none")+
  scale_y_continuous(
    "log10(p-value)\n<- highly significant --- not significant ->",
    limits=c(-40,0),
    breaks=seq(-100,0,by=10))+
  scale_x_continuous(
    "Percent test error difference (same-other)",
    breaks=seq(-10,100,by=10),
    limits=c(-10,NA))
png("data_Classif_batchmark_registry_scatter_other.png", width=7.5, height=4, units="in", res=200)
print(gg)
dev.off()
text.dt <- rbind(
  ## tlab(15, 0, "No significant difference"),
  ## tlab(15, -40, "Highly\nsignificant\ndifference"),
  tlab(0, -30, "Same accuracy"),
  tlab(60, -30, "Worse accuracy"))
gg <- ggplot(mapping=aes(
  estimate_other, log10.p_other, color=data.name))+
  theme_bw()+
  ##geom_hline(yintercept=0,color="grey")+
  geom_vline(xintercept=0,color="grey")+
  ## geom_text(aes(
  ##   x, y, label=label, color=NULL),
  ##   vjust=0,
  ##   data=text.dt)+
  geom_point(
    data=compare.wide)+
  ggrepel::geom_text_repel(aes(
    label=data.name),
    size=3,
    data=compare.wide)+
  theme(legend.position="none")+
  scale_y_continuous(
    "log10(p-value)",
    limits=c(ymin,ymax),
    breaks=seq(-10,10,by=1))+
  scale_x_continuous(
    "Percent test error difference (same-other)",
    breaks=seq(-10,10,by=1),
    limits=c(xmin,xmax))
png("data_Classif_batchmark_registry_scatter_other_zoom.png", width=4, height=3, units="in", res=200)
print(gg)
dev.off()
system("cd /projects/genomic-ml && unpublish_data cv-same-other-paper && publish_data projects/cv-same-other-paper")

tt.join <- score.join[
  grepl("train|test",group.small.name)
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
meta.not.tt <- meta.dt[data.name=="MNIST_EMNIST_rot"]
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
      scores.not,
      algorithm + data.name + train.groups + test.group ~ .,
      list(mean, sd),
      value.var="percent.error")
    join.wide <- group.meta[scores.wide, on=.(data.name, test.group)]
    gg <- ggplot()+
      theme_bw()+
      theme(axis.text.x=element_text(angle=30, hjust=1))+
      ggtitle(paste("Data set:", meta.row$data.name))+
      geom_point(aes(
        percent.error_mean, train.groups, color=algorithm),
        shape=1,
        data=join.wide)+
      geom_segment(aes(
        percent.error_mean-percent.error_sd, train.groups,
        color=algorithm,
        xend=percent.error_mean+percent.error_sd, yend=train.groups),
        data=join.wide)+
      facet_grid(. ~ test.group + group.rows, labeller=label_both, scales="free")+
      scale_x_continuous(
        "Percent error on CV test group (mean±SD over 10 folds in CV)")+
      scale_y_discrete(
        "Train groups")
    out.png <- sprintf(
      "data_Classif_figures/%s_error_glmnet_featureless_mean_SD.png",
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
