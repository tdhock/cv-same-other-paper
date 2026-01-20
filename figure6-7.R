library(xtable)
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
compare.wide.groups <- dcast(
  p.each.group,
  subset_type + Data ~ compare_name,
  list(min, max, mean),
  value.var=c("estimate","log10.p"))
tab.long <- computeP("subset_type","Data","compare_name")
(compare.wide <- dcast(
  tab.long,
  subset_type + Data ~ compare_name,
  value.var=c("estimate","log10.p")
)[compare.wide.groups, on=.(subset_type,Data)])

box.size <- "0.5cm"
type.colors <- c(
  ImagePair="black",
  "time/space"="white",
  "train/test"="red")
min.comma.max <- function(m,M){
  sprintf(
    "$\\parbox{%s}{\\rightline{%.1f}},\\parbox{%s}{\\rightline{%.1f}}$",
    box.size,m,
    box.size,M)
}
(wide.xt <- meta.dt[compare.wide, .(
  subset_type, Data,
  #rows, features, classes, n.groups,
  "other-same"=estimate_other,
  log10.p_other,
  "all-same"=estimate_all,
  log10.p_all,
  log10.p_min_all,
  log10.p_max_all,
  estimate_min_all,
  estimate_max_all,
  log10.p_min_other,
  log10.p_max_other,
  estimate_min_other,
  estimate_max_other
), on="Data"])
tikz_dot <- function(subset_type)sprintf(
  "\\tikz\\draw[black,fill=%s] (0,0) circle (.5ex);",
  type.colors[paste(subset_type)])
for(compare_name in c("other","all")){
  s <- function(p)sprintf("%s_%s",p,compare_name)
  compare.xt <- wide.xt[
    order(-estimate_min_COMPARE),
    list(
      Data=paste(
        tikz_dot(subset_type),
        gsub("_","\\\\_",Data)),
      ErrorDiff=min.comma.max(estimate_min_COMPARE,estimate_max_COMPARE),
      `log10(P)`=min.comma.max(log10.p_min_COMPARE,log10.p_max_COMPARE)
    ),
    env=with(CJ(
      var=c("estimate","log10.p"),
      fun=c("min","max")
    )[, arg := paste0(var,"_",fun)],
    structure(lapply(arg, s), names=paste0(arg,"_COMPARE"))
    )
  ]
  print(xtable(compare.xt, digits=1), type="latex", include.rownames=FALSE, sanitize.text.function=identity)
}

compare.wide[log10.p_all < -12, log10.p_all := -Inf][]
compare.wide[log10.p_other < -20, log10.p_other := -Inf][]
scale.fill <- scale_fill_manual(
  "Subset type", values=type.colors)
text.x <- -5
tlab <- function(x, y, label){
  data.table(x, y, label)
}
text.dt <- rbind(
  tlab(-1.8, 8, "p<0.05"),
  tlab(text.x, -4, "Beneficial\nto combine"),
  tlab(text.x, 7, "Detrimental\nto combine"),
  NULL)
set.seed(2)
gg <- ggplot()+
  ggtitle("Is it beneficial to combine subsets?")+
  theme_bw()+
  theme(legend.position=c(0.9,0.9))+
  geom_vline(xintercept=log10(0.05),color="grey")+
  geom_hline(yintercept=0,color="black")+
  geom_text(aes(
    x, y, label=label, color=NULL),
    vjust=0,
    color="grey50",
    data=text.dt)+
  geom_segment(aes(
    y=estimate_min_all, x=log10.p_mean_all,
    yend=estimate_max_all, xend=log10.p_mean_all,
    color=Data),
    data=compare.wide)+
  geom_segment(aes(
    y=estimate_mean_all, x=log10.p_min_all,
    yend=estimate_mean_all, xend=log10.p_max_all,
    color=Data),
    data=compare.wide)+
  geom_point(aes(
    log10.p_mean_all,
    estimate_mean_all,
    color=Data,
    fill=subset_type),
    shape=21,
    data=compare.wide)+
  ggrepel::geom_label_repel(aes(
    log10.p_mean_all,
    estimate_mean_all,
    color=Data,
    label=Data),
    alpha=0.75,
    size=2.8,
    data=compare.wide)+
  scale.fill+
  scale_color_discrete(guide="none")+
  scale_x_continuous(
    "<- highly significant -- log10(p-value) -- not significant ->",
    breaks=seq(-100,0,by=2))+
  scale_y_continuous(
    "Percent test error difference (all-same)",
    breaks=seq(-100,10,by=2))+
  coord_cartesian(
    ylim=c(-4,10),
    xlim=c(-6.5,0))
png("figure6.png", width=5, height=4, units="in", res=200)
print(gg)
dev.off()

text.x <- -12
text.dt <- rbind(
  tlab(-3, 20, "p<0.05"),
  tlab(text.x, -5, "Accurate"),
  tlab(text.x, 20, "Inaccurate"),
  NULL)
set.seed(3)
gg <- ggplot()+
  theme_bw()+
  theme(legend.position=c(0.15, 0.55))+
  ggtitle("Accurate prediction on a new subset?")+
  geom_vline(xintercept=log10(0.05),color="grey")+
  geom_hline(yintercept=0,color="black")+
  geom_segment(aes(
    y=estimate_min_other, x=log10.p_mean_other,
    yend=estimate_max_other, xend=log10.p_mean_other,
    color=Data),
    data=compare.wide)+
  geom_segment(aes(
    y=estimate_mean_other, x=log10.p_min_other,
    yend=estimate_mean_other, xend=log10.p_max_other,
    color=Data),
    data=compare.wide)+
  geom_text(aes(
    x, y, label=label, color=NULL),
    vjust=0,
    color="grey50",
    data=text.dt)+
  geom_point(aes(
    log10.p_mean_other,
    estimate_mean_other,
    color=Data,
    fill=subset_type),
    shape=21,
    data=compare.wide)+
  ggrepel::geom_label_repel(aes(
    log10.p_mean_other,
    estimate_mean_other,
    color=Data,
    label=Data),
    max.overlaps=20,
    alpha=0.75,
    size=3,
    data=compare.wide)+
  scale.fill+
  scale_color_discrete(guide="none")+
  scale_x_continuous(
    "<- highly significant -- log10(p-value) -- not significant ->",
    breaks=seq(-100,0,by=2))+
  scale_y_continuous(
    "Percent test error difference (other-same)",
    breaks=seq(-10,100,by=5))+
  coord_cartesian(
    ylim=c(-5,30),
    xlim=c(-20,2))
png("figure7.png", width=5, height=4, units="in", res=200)
print(gg)
dev.off()
