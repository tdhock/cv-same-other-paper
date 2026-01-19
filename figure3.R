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
tt.join <- score.join[
  grepl("train|test",group.small.name)
][
, predefined.set := test_subset
][]

dot.both <- dcast(
  tt.join,
  Data + task_id + `test%` + rows + train_subsets + predefined.set + algorithm ~ .,
  list(
    length, median,
    mean, sd,
    q25=function(x)quantile(x,0.25),
    q75=function(x)quantile(x,0.75)),
  value.var="percent.error")
dot.counts <- dot.both[algorithm=="cv_glmnet"]
meta.long <- melt(
  meta.dt,
  measure.vars=c("test","train"),
  variable.name="predefined.set",
  na.rm=TRUE)


Show <- function(Data, similarity){
  data.table(Data,similarity)
}
show.names <- rbind(
  Show("STL10","very similar"),
  Show("waveform","slightly similar"),
  Show("KMNIST","slightly different"),
  Show("vowel","very different")
)[, similarity := factor(similarity,similarity)]
show.meta <- meta.long[show.names,on="Data"]
show.both <- dot.both[show.names,on="Data"]
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
show.p <- meta.dt[
  p.each.group[show.names,on="Data"],
  on=.(Data,subset_type)
][
, `:=`(
  train_subsets = paste0(compare_name,"-same"),
  algorithm = "cv_glmnet",
  predefined.set = test_subset
)][]
text.size <- 3.5
gg <- ggplot()+
  theme_bw()+
  theme(legend.position="bottom")+
  scale_color_manual(values=c(
    cv_glmnet="black",
    featureless="red"))+
  geom_text(aes(
    Inf, Inf, label=sprintf("%d rows ", value)),
    hjust=1,
    vjust=1.2,
    size=text.size,
    color="grey50",
    data=show.meta)+
  geom_segment(aes(
    compare_mean, train_subsets,
    color=algorithm,
    xend=same_mean, yend=train_subsets),
    linewidth=1,
    data=show.p)+
  geom_text(aes(
    pmax(compare_mean, same_mean),
    train_subsets,
    label=if(FALSE){
      sprintf(
        "Diff=%.1f\np%s",
        estimate,
        ifelse(
          p.value<0.001,
          "<0.001",
          sprintf("=%.3f",p.value)))
    }else{
      ifelse(
        p.value<0.001,
        "p<0.001",
        sprintf("p=%.3f",p.value))
    }),
    size=text.size,
    hjust=0,
    vjust=1,
    data=show.p)+
  geom_vline(aes(
    xintercept=same_mean),
    color="grey",
    data=show.p)+
  geom_segment(aes(
    percent.error_mean+percent.error_sd,
    train_subsets,
    color=algorithm,
    xend=percent.error_mean-percent.error_sd,
    yend=train_subsets),
    linewidth=1,
    data=show.both)+
  geom_point(aes(
    percent.error_mean, train_subsets, color=algorithm),
    shape=1,
    data=show.both)+
  geom_text(aes(
    percent.error_median, train_subsets,
    color=algorithm,
    hjust=ifelse(algorithm=="featureless",1,0),
    label=sprintf("%.1f", percent.error_median)),
    size=text.size,
    vjust=1.4,
    data=show.both)+
  facet_grid(predefined.set ~ similarity + Data, labeller=label_both, scales="free")+
  scale_x_continuous(
    "Percent prediction error on CV test set in predefined set (mean +/- SD over 10 folds)")+
  scale_y_discrete(
    "Predefined set(s) used for CV train set",
    limits=c(
      "all",
      "all-same",
      "same",
      "other-same",
      "other"))
png("figure3.png", width=7.5, height=4.5, units="in", res=200)
print(gg)
dev.off()
