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

score.join <- meta.dt[score.atomic, on="data.name"]
(tab.wide <- dcast(
  score.join[algorithm=="cv_glmnet"],
  subset_type + Data + test_subset + test.fold ~ train_subsets,
  value.var="percent.error"))
(tab.long.raw <- melt(
  tab.wide,
  measure=c("other","all"),
  variable.name="compare_name",
  value.name="compare_error"))
computeP <- function(...){
  by.vec <- c(...)
  tab.long.raw[, {
    test.res <- t.test(compare_error, same, paired=TRUE)
    log.signed.p <- function(signed.p){
      sign(signed.p)*abs(log10(abs(signed.p)))
    }
    with(test.res, data.table(
      estimate,
      p.value,
      compare_mean=mean(compare_error,na.rm=TRUE),
      same_mean=mean(same,na.rm=TRUE),
      log10.p=log10(p.value),
      sign.log.p=log.signed.p(p.value*sign(estimate)),
      N=.N))
  }, by=by.vec]
}
p.each.group <- computeP("subset_type","Data","compare_name","test_subset")
type.colors <- c(
  ImagePair="black",
  "time/space"="white",
  "train/test"="red")


scores.mean.sd <- dcast(
  score.atomic[algorithm!="featureless"][meta.dt, on=.(data.name)],
  Data + subset_type + train_subsets + test_subset ~ .,
  list(
    mean, sd, median, length,
    q75=function(x)quantile(x, 0.75),
    q25=function(x)quantile(x, 0.25)),
  value.var="percent.error")
show.compare <- "all"
show.mean.sd <- scores.mean.sd[train_subsets%in%c("same",show.compare)]
err.mid <- mean(range(show.mean.sd$percent.error_mean))
text.mean.sd <- dcast(
  show.mean.sd,
  Data + test_subset ~ .,
  list(mean, diff, min, max),
  value.var='percent.error_mean'
)[, let(
  compare_name=show.compare,
  hjust = ifelse(percent.error_mean_mean<err.mid, 0, 1),
  ihjust = ifelse(percent.error_mean_mean<err.mid, 1, 0),
  before = ifelse(percent.error_mean_mean<err.mid, " ", ""),
  after = ifelse(percent.error_mean_mean<err.mid, "", " "),
  x = ifelse(percent.error_mean_mean<err.mid, percent.error_mean_max, percent.error_mean_min),
  ix = ifelse(percent.error_mean_mean<err.mid, Inf, -Inf),
  subset_rank=rank(percent.error_mean_diff)
)][p.each.group, on=.(Data, compare_name, test_subset), nomatch=0L]
data.mean.sd <- dcast(
  text.mean.sd,
  Data ~.,
  mean,
  value.var=c("percent.error_mean_diff","p.value")
)[, Data_rank := rank(percent.error_mean_diff)][]
text.join <- data.mean.sd[, .(Data, Data_rank)][
  text.mean.sd, on=.(Data)
][order(-Data_rank, -subset_rank)]
Dt.levs <- text.join[, paste(Data, test_subset)]
add_Dt <- function(DT)DT[
, Data_test_subset := factor(paste(Data, test_subset), Dt.levs)]
gg <- ggplot()+
  geom_text(aes(
    ix, Data_test_subset,
    hjust=ihjust,
    label=sprintf(
      " %s better, diff %.1f%%, %s ",
      ifelse(percent.error_mean_diff>0, "all", "same"),
      -percent.error_mean_diff,
      ifelse(
        p.value<0.001,
        "p<0.001",
        sprintf("p=%.3f",p.value)))),
    size=3,
    data=add_Dt(text.mean.sd))+
  scale_fill_manual(
    "Subset type", values=type.colors)+
  geom_segment(aes(
    percent.error_mean+percent.error_sd, Data_test_subset,
    size=train_subsets,
    xend=percent.error_mean-percent.error_sd, yend=Data_test_subset,
    color=train_subsets),
    data=show.mean.sd)+
  geom_point(aes(
    percent.error_mean, Data_test_subset,
    color=train_subsets),
    size=1.3,
    data=add_Dt(show.mean.sd))+
  geom_point(aes(
    percent.error_mean, Data_test_subset,
    fill=subset_type,
    color=train_subsets),
    shape=21,
    size=1,
    data=add_Dt(show.mean.sd))+
  scale_y_discrete("Data and test subset", position="right")+
  scale_x_continuous("Prediction error on test subset (mean ± SD in 10-fold CV)")+
  theme_bw()+
  scale_size_manual(values=c(
    same=0.5,
    all=1))+
  theme(legend.position=c(0.5,0.75))
png("figure5.png", width=8, height=6, units="in", res=200)
print(gg)
dev.off()



