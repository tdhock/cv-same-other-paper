library(data.table)
library(ggplot2)
score_dt <- fread("conv_images_10fold_test.csv")

(score_stats <- dcast(
  score_dt,
  algorithm + task_id + test.subset + train.subsets ~ .,
  list(mean, sd, length),
  value.var="percent_error"))
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
png("figure8top.png", width=7, height=3, units="in", res=200)
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
png("figure8bottom.png", width=7, height=2.5, units="in", res=200)
print(gg)
dev.off()
