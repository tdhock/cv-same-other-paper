library(data.table)
library(ggplot2)
score_dt <- fread("conv_images_10fold_test.csv")

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
    "Percent test error (mean±SD over train/test splits in 10-fold CV)",
    limits=c(-10,105),
    breaks=seq(0,100,by=10))
png("conv_images_10fold_figures_same_other.png", width=7, height=3, units="in", res=200)
print(gg)
dev.off()


y.limits <- c(
  "all",
  "all-same",
  "same",
  "other-same",
  "other")
torch_conv <- score_stats[
  algorithm == "torch_conv"
][
, Train_subsets := factor(train.subsets, c("all","same","other"))
][]
score_wide <- dcast(
  score_dt,
  test.subset + algorithm + test.fold ~ train.subsets,
  value.var="percent_error")
score_long <- melt(
  score_wide,
  measure.vars=c("other","all"),
  variable.name="train.subsets",
  value.name="percent_error")
torch_conv_pvals <- score_long[algorithm=="torch_conv", {
  paired <- t.test(percent_error, same, paired=TRUE)
  unpaired <- t.test(percent_error, same, paired=FALSE)
  data.table(
    mean_diff=paired$estimate,
    diff_mean=diff(unpaired$estimate),
    p.paired=paired$p.value,
    p.unpaired=unpaired$p.value,
    same_mean=mean(same),
    compare_mean=mean(percent_error),
    N=.N)
}, by=.(test.subset, algorithm, train.subsets)
][
, y := paste0(train.subsets,"-same")
][]
p.color <- "grey50"
gg <- ggplot()+
  geom_point(aes(
    percent_error_mean,
    Train_subsets),
    shape=1,
    data=torch_conv)+
  geom_text(aes(
    percent_error_mean,
    Train_subsets,
    label=sprintf("%.1f±%.1f", percent_error_mean, percent_error_sd)),
    vjust=-0.5,
    data=torch_conv)+
  geom_segment(aes(
    percent_error_mean+percent_error_sd,
    Train_subsets,
    xend=percent_error_mean-percent_error_sd, yend=Train_subsets),
    data=torch_conv)+
  geom_segment(aes(
    compare_mean, y,
    xend=same_mean, yend=y),
    color=p.color,
    data=torch_conv_pvals)+
  geom_text(aes(
    same_mean, y,
    label=paste0(
      ifelse(
        p.paired<0.0001,
        "P<0.0001",
        sprintf("P=%.4f", p.paired))
    )),
    ## label=paste0(
    ##   sprintf("Diff=%.4f ", diff_mean),
    ##   ifelse(
    ##     p.unpaired<0.0001,
    ##     "P<0.0001",
    ##     sprintf("P=%.4f", p.unpaired))
    ## )),
    vjust=-0.5,
    hjust=0,
    color=p.color,
    data=torch_conv_pvals)+
  facet_grid(. ~ test.subset, labeller=label_both)+
  theme_bw()+
  theme(legend.position=c(0.9,0.2))+
  scale_y_discrete(
    "Train subsets",
    limits=y.limits)+
  scale_x_continuous(
    "Percent test error of torch_conv algorithm\nmean±SD over train/test splits in 10-fold CV, P-value in paired T-test",
    breaks=seq(0,10,by=2),
    limits=c(0,10))
png("conv_images_10fold_figure_pval.png", width=7, height=2.5, units="in", res=200)
print(gg)
dev.off()

history_dt <- fread("conv_images_10fold_history.csv")
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
  measure=="logloss" & train.subsets!="same" & test.fold==1]
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
