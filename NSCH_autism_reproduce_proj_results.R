library(data.table)
library(ggplot2)
meta_dt <- fread("data_meta/NSCH_autism.csv")
tfac <- function(x)factor(x, c("all","same","other",""))
results_dt <- fread(
  "NSCH_autism_reproduce_proj_results.csv"
)[meta_dt, on="test.subset"][, let(
  Train_subsets=tfac(train.subsets)
)]
full_sample_size <- results_dt[
  n.train.groups==groups & learner_id=="classif.cv_glmnet"]
full_metrics <- nc::capture_melt_single(
  full_sample_size,
  "classif[.]",
  metric=nc::alevels(acc="accuracy_prop", auc="AUC"))
full_stats <- dcast(
  full_metrics,
  Train_subsets + metric + test.subset + rows ~ .,
  list(mean, sd, length)
)[, test := test.subset]

gg <- ggplot()+
  ggtitle("Data set: NSCH_autism (2 similar subsets)")+
  geom_blank(aes(
    x, y),
    data=data.table(x=Inf, y=tfac("")))+
  geom_point(aes(
    value_mean, Train_subsets),
    data=full_stats)+
  geom_text(aes(
    value_mean, Train_subsets, label=sprintf(
      "%.4f±%.4f", value_mean, value_sd)),
    vjust=-0.2,
    data=full_stats)+
  geom_segment(aes(
    value_mean+value_sd, Train_subsets,
    xend=value_mean-value_sd, yend=Train_subsets),
    data=full_stats)+
  scale_y_discrete(
    "Train subsets",
    drop=FALSE)+
  scale_x_continuous(
    "cv_glmnet performance on test subset (mean±SD over 10 folds in CV)")+
  facet_grid(test+rows~metric, scales="free", labeller=label_both)
png(
  "NSCH_autism_reproduce_proj_results_figure4.png",
  width=8, height=2.5, units="in", res=200)
print(gg)
dev.off()

plist <- mlr3resampling::pvalue(full_sample_size[learner_id=="classif.cv_glmnet"])
gg <- plot(plist, text.size=4)
png(
  "NSCH_autism_reproduce_proj_results_figure5.png",
  width=8, height=2.5, units="in", res=200)
print(gg)
dev.off()
