library(data.table)
library(ggplot2)
score_dt <- fread("conv_images_test.csv")

ggplot()+
  geom_point(aes(
    percent_error, algorithm),
    data=score_dt)+
  facet_grid(train.subsets ~ test.subset, labeller=label_both)

(score_stats <- dcast(
  score_dt,
  algorithm + task_id + test.subset + train.subsets ~ .,
  list(mean, sd, length),
  value.var="percent_error"))
ggplot()+
  geom_point(aes(
    percent_error_mean, algorithm),
    shape=1,
    data=score_stats)+
  geom_text(aes(
    percent_error_mean, algorithm, label=percent_error_length),
    hjust=2,
    data=score_stats)+
  geom_segment(aes(
    percent_error_mean+percent_error_sd, algorithm,
    xend=percent_error_mean-percent_error_sd, yend=algorithm),
    data=score_stats)+
  facet_grid(train.subsets ~ task_id + test.subset, labeller=label_both)

disp.levs <- c(
  "torch_conv", "torch_dense_50", "torch_linear","cv_glmnet_min", "featureless")
same_other <- score_stats[
  train.subsets%in%c("same","other") & algorithm %in% disp.levs
][
, Algorithm := factor(algorithm, disp.levs)
][]
gg <- ggplot()+
  geom_point(aes(
    percent_error_mean,
    Algorithm,
    color=train.subsets),
    shape=1,
    data=same_other)+
  geom_text(aes(
    percent_error_mean,
    Algorithm,
    label=sprintf("%.1f±%.1f", percent_error_mean, percent_error_sd),
    vjust=ifelse(train.subsets=="other", -0.5, 1.5),
    color=train.subsets),
    data=same_other)+
  geom_segment(aes(
    percent_error_mean+percent_error_sd,
    Algorithm,
    color=train.subsets,
    xend=percent_error_mean-percent_error_sd, yend=algorithm),
    data=same_other)+
  facet_grid(. ~ test.subset, labeller=label_both)+
  theme_bw()+
  theme(legend.position=c(0.9,0.2))+
  scale_x_continuous(
    "Percent test error (mean±SD over train/test splits in 3-fold CV)",
    limits=c(-10,105),
    breaks=seq(0,100,by=10))
png("conv_images_figures_same_other.png", width=7, height=3, units="in", res=200)
print(gg)
dev.off()

history_dt <- fread("conv_images_history.csv")
pat <- function(...){
  old2new <- c(...)
  if(is.null(names(old2new))){
    names(old2new) <- old2new
  }
  to.rep <- names(old2new)==""
  names(old2new)[to.rep] <- old2new[to.rep]
  list(
    paste(names(old2new), collapse="|"),
    function(x)factor(old2new[x], old2new))
}
melt_history <- function(DT)nc::capture_melt_single(
  DT,
  set=pat(valid="validation", train="subtrain"),
  ".classif.",
  measure=pat(ce="error_prop", auc="AUC", "logloss"))
history_long <- melt_history(history_dt)
history_logloss <- history_long[
  measure=="logloss" & train.subsets!="same" & grepl("linear", algorithm) & test.fold==1]
history_logloss[, .SD[which.min(value)], by=.(test.subset, train.subsets, test.fold, algorithm, set)]
ggplot()+
  geom_line(aes(
    epoch, value, color=set),
    data=history_logloss)+
  facet_wrap(~test.subset+train.subsets+test.fold+algorithm)

history_after_init <- history_logloss[
  history_logloss[epoch==max(epoch)],
  .(test.subset, train.subsets, test.fold, algorithm, set, epoch, value=x.value),
  on=.(test.subset, train.subsets, test.fold, algorithm, value<value)]
ggplot()+
  geom_line(aes(
    epoch, value, color=set),
    data=history_after_init)+
  facet_wrap(~test.subset+train.subsets+test.fold+algorithm, scales="free")

ggplot()+
  geom_line(aes(
    epoch, value, color=set),
    data=history_after_init[set=="validation"])+
  facet_wrap(~test.subset+train.subsets+test.fold+algorithm, scales="free")
