library(ggplot2)
library(data.table)

(objs=load("data_Classif_batchmark_registry.RData"))
meta.dt <- data.table::fread("data-meta.csv")
score.dt <- mlr3resampling::score(bmr)
score.dt[
, percent.error := 100*classif.ce
][
, data.name := task_id
]
score.atomic <- score.dt[,sapply(score.dt, class)!="list", with=FALSE]
fwrite(score.atomic, "data_Classif_batchmark_registry.csv")

score.atomic <- fread("data_Classif_batchmark_registry.csv")
score.join <- meta.dt[score.atomic, on="data.name"]

dot.counts <- dcast(
  score.join,
  data.name + task_id + `test%` + rows + train.groups + predefined.set ~ algorithm,
  list(length, median, q25=function(x)quantile(x,0.25), q75=function(x)quantile(x,0.75)),
  value.var="percent.error")
dot.counts[percent.error_length_featureless!= 10, task_id]
ignore.task <- c("14cancer","khan","STL10")
(dot.show <- dot.counts[!task_id %in% ignore.task])

score.show <- score.join[(!task_id %in% ignore.task) & algorithm!="rpart"]
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
  variable.name="predefined.set"
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
    "Percent prediction error on CV test set in predefined set (median and quartiles)")+
  scale_y_discrete(
    "Predefined set(s) used for glmnet CV train set")
png("data_Classif_batchmark_registry_glmnet_median_quartiles.png", width=20, height=4, units="in", res=100)
print(gg)
dev.off()
system("cd /projects/genomic-ml && unpublish_data projects/cv-same-other-paper && publish_data projects/cv-same-other-paper")
