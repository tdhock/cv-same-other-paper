library(xtable)
library(ggplot2)
library(data.table)
work.dir <- "/scratch/th798/cv-same-other-paper"
reg.RData <- file.path(work.dir, "data_Classif_batchmark_registry.RData")
(objs=load(reg.RData))
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
  score.atomic.noerr <- fread("data_Classif_batchmark_registry_noerr.csv")
## > setdiff(unique(score.atomic$data.name), unique(score.atomic.old$data.name))
## [1] 
## > setdiff(unique(score.atomic.old$data.name), unique(score.atomic$data.name))
## [1] "QMNIST"   "spam"     "STL10"    "vowel"    "waveform" "zipUSPS"
##EMNIST has 1 error in new, take all from noerr, plus MNIST_* data from new.
  score.both <- rbind(
    score.atomic[data.name%in%c("MNIST_EMNIST","MNIST_EMNIST_rot","MNIST_FashionMNIST")],
    score.atomic.noerr)
  (fold.counts <- dcast(score.atomic, data.name + train.groups + test.group ~ algorithm, length))
  fold.counts[cv_glmnet != 10]
  fold.counts[grepl("MNIST", data.name)]
}

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
, group.type := fcase(
  grepl("MNIST_", data.name), "ImagePair",
  !is.na(test), "train/test",
  default="time/space")
][]
meta.dt <- disp.dt[
  meta.raw, on="data.name"
][is.na(Data), Data := data.name][]
meta.dt[order(group.type,data.name), .(group.type, data.name, group.tab)]
table1 <- meta.dt[order(group.type,data.name), .(
  Type=group.type, Data, rows, features,
  classes,
  Im.class=label.large.N/label.small.N,
  groups=n.groups,
  Im.group=group.large.N/group.small.N
)]
xtable1 <- xtable(table1, digits=1)
print(xtable1, type="latex")
group.meta <- meta.dt[, nc::capture_all_str(
  group.tab,
  test.group="[^;]+",
  "=",
  group.rows="[0-9]+", as.integer
), by=data.name]
score.join <- meta.dt[score.atomic, on="data.name"]

## is training on other better than nothing/featureless?
tab.wide.algos <- dcast(
  score.join,
  group.type + Data + test.group + test.fold ~ train.groups + algorithm,
  value.var="percent.error")
tab.wide.algos[, {
  test.res <- t.test(other_cv_glmnet, same_featureless, paired=TRUE)
  with(test.res, data.table(
    estimate,
    p.value,
    same_featureless_mean=mean(same_featureless,na.rm=TRUE),
    other_cv_glmnet_mean=mean(other_cv_glmnet,na.rm=TRUE),
    log10.p=log10(p.value),
    N=.N))
}, by=.(group.type, Data, test.group)
][order(estimate)]



(tab.wide <- dcast(
  score.join[algorithm=="cv_glmnet"],
  group.type + Data + test.group + test.fold ~ train.groups,
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
p.each.group <- computeP("group.type","Data","compare_name","test.group")
compare.wide.groups <- dcast(
  p.each.group,
  group.type + Data ~ compare_name,
  list(min, max, mean),
  value.var=c("estimate","log10.p"))
tab.long <- computeP("group.type","Data","compare_name")
(compare.wide <- dcast(
  tab.long,
  group.type + Data ~ compare_name,
  value.var=c("estimate","log10.p")
)[compare.wide.groups, on=.(group.type,Data)])
(wide.xt <- meta.dt[compare.wide, .(
  group.type, Data,
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
other.xt <- wide.xt[order(-log10.p_other), .(Data,Diff=`other-same`,`log10(p)`=log10.p_other)]
print(xtable(other.xt), type="latex", include.rownames=FALSE)
all.xt <- wide.xt[order(`all-same`), .(Data,Diff=`all-same`,`log10(p)`=log10.p_all)]
print(xtable(all.xt), type="latex", include.rownames=FALSE)
## min/max tables
other.xt <- wide.xt[order(estimate_min_other), .(
  Data=gsub("_","\\\\_",Data),
  ## $\underline D$ & $\overline D$ & $\text{L}\underline P$ & $\text{L}\overline P$
  `$\\underline D$`=estimate_min_other,
  `$\\overline D$`=estimate_max_other,
  `$\\text{L}\\underline P$`=log10.p_min_other,
  `$\\text{L}\\overline P$`=log10.p_max_other)]
print(xtable(other.xt, digits=1), type="latex", include.rownames=FALSE, sanitize.text.function=identity)
all.xt <- wide.xt[order(estimate_min_all), .(
  Data=gsub("_","\\\\_",Data),
  `$\\underline D$`=estimate_min_all,
  `$\\overline D$`=estimate_max_all,
  `$\\text{L}\\underline P$`=log10.p_min_all,
  `$\\text{L}\\overline P$`=log10.p_max_all)]

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
for(compare_name in c("other","all")){
  s <- function(p)sprintf("%s_%s",p,compare_name)
  compare.xt <- wide.xt[
    order(-estimate_min_COMPARE),
    list(
      Data=sprintf(
        "\\tikz\\draw[black,fill=%s] (0,0) circle (.5ex); %s",
        type.colors[paste(group.type)],
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
gg <- ggplot()+
  theme_bw()+
  geom_hline(yintercept=0,color="grey")+
  geom_vline(xintercept=0,color="grey")+
  geom_point(aes(
    estimate_other, estimate_all,
    color=Data,
    fill=group.type),
    shape=21,
    data=compare.wide)+
  scale.fill+
  ggrepel::geom_label_repel(aes(
    estimate_other, estimate_all, color=Data, label=Data),
    data=compare.wide)+
  coord_equal()+
  theme(legend.position="none")+
  scale_x_continuous(
    "Other-Same, mean percent error, over 10 test folds and all test subsets")+
  scale_y_continuous(
    "All-Same, mean percent error,\nover 10 test folds and all test subsets")
png("data_Classif_batchmark_registry_scatter_other_all.png", width=7, height=3.5, units="in", res=200)
print(gg)
dev.off()
zoom <- gg+
  scale_x_continuous(
    "Other-Same (mean percent error over 10 test folds and all test subsets)",
    limits=c(-1, 1))+
  scale_y_continuous(
    "All-Same (mean percent error over 10 test folds and all test subsets)",
    limits=c(-2.5, 0))
png("data_Classif_batchmark_registry_scatter_other_all_similar.png", width=6, height=6, units="in", res=200)
print(zoom)
dev.off()
tlab <- function(x, y, label){
  data.table(x, y, label)
}
text.dt <- rbind(
  tlab(4, 0, "No significant difference"),
  tlab(4, -9, "Highly significant difference"),
  tlab(-2, -7, "Beneficial\nto combine"),
  tlab(8, -7, "Detrimental\nto combine"))
gg <- ggplot()+
  ggtitle("Is it beneficial to combine subsets?")+
  theme_bw()+
  theme(legend.position=c(0.7,0.7))+
  geom_hline(yintercept=log10(0.05),color="grey")+
  geom_vline(xintercept=0,color="grey")+
  geom_text(aes(
    x, y, label=label, color=NULL),
    vjust=0,
    data=text.dt)+
  geom_point(aes(
    estimate_all, log10.p_all,
    color=Data,
    fill=group.type),
    shape=21,
    data=compare.wide)+
  ggrepel::geom_text_repel(aes(
    estimate_all, log10.p_all, color=Data,
    label=Data),
    size=3,
    data=compare.wide)+
  scale.fill+
  scale_color_discrete(guide="none")+
  scale_y_continuous(
    "log10(p-value)\n<- highly significant --- not significant ->",
    breaks=seq(-100,0,by=2))+
  scale_x_continuous(
    "Percent test error difference (all-same)",
    breaks=seq(-100,10,by=2),
    limits=c(-3,NA))
png("data_Classif_batchmark_registry_scatter_all.png", width=6, height=5, units="in", res=200)
print(gg)
dev.off()
text.y <- -6.5
text.dt <- rbind(
  tlab(6, -1, "p<0.05"),
  tlab(-2, text.y, "Beneficial\nto combine"),
  tlab(8, text.y, "Detrimental\nto combine"))
set.seed(2)
gg <- ggplot()+
  ggtitle("Is it beneficial to combine subsets?")+
  theme_bw()+
  theme(legend.position=c(0.9,0.9))+
  geom_hline(yintercept=log10(0.05),color="grey")+
  geom_vline(xintercept=0,color="grey")+
  geom_text(aes(
    x, y, label=label, color=NULL),
    vjust=0,
    color="grey50",
    data=text.dt)+
  geom_segment(aes(
    estimate_min_all, log10.p_mean_all,
    xend=estimate_max_all, yend=log10.p_mean_all,
    color=Data),
    data=compare.wide)+
  geom_segment(aes(
    estimate_mean_all, log10.p_min_all,
    xend=estimate_mean_all, yend=log10.p_max_all,
    color=Data),
    data=compare.wide)+
  geom_point(aes(
    estimate_mean_all, log10.p_mean_all,
    color=Data,
    fill=group.type),
    shape=21,
    data=compare.wide)+
  ggrepel::geom_label_repel(aes(
    estimate_mean_all, log10.p_mean_all, color=Data,
    label=Data),
    alpha=0.75,
    size=2.8,
    data=compare.wide)+
  scale.fill+
  scale_color_discrete(guide="none")+
  scale_y_continuous(
    "log10(p-value)\n<- highly significant --- not significant ->",
    breaks=seq(-100,0,by=2))+
  scale_x_continuous(
    "Percent test error difference (all-same)",
    breaks=seq(-100,10,by=2))+
  coord_cartesian(
    xlim=c(-4,10),
    ylim=c(-7,0))
png("data_Classif_batchmark_registry_scatter_all_segments.png", width=5, height=4, units="in", res=200)
print(gg)
dev.off()

text.x <- -5
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
    fill=group.type),
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
png("data_Classif_batchmark_registry_scatter_all_segments_flip.png", width=5, height=4, units="in", res=200)
print(gg)
dev.off()

all.xt <- wide.xt[order(-estimate_min_all), .(
  Data=gsub("_","\\\\_",Data),
  ErrorDiff=min.comma.max(estimate_min_all,estimate_max_all),
  `log10(P)`=min.comma.max(log10.p_min_all,log10.p_max_all))]
print(xtable(all.xt, digits=1), type="latex", include.rownames=FALSE, sanitize.text.function=identity)
text.y <- -6.5
text.dt <- rbind(
  tlab(6, -1, "p<0.05"),
  tlab(-2, text.y, "Beneficial\nto combine"),
  tlab(8, text.y, "Detrimental\nto combine"))
set.seed(2)
gg <- ggplot()+
  ggtitle("Is it beneficial to combine subsets?")+
  theme_bw()+
  theme(legend.position=c(0.9,0.9))+
  geom_hline(yintercept=log10(0.05),color="grey")+
  geom_vline(xintercept=0,color="grey")+
  geom_text(aes(
    x, y, label=label, color=NULL),
    vjust=0,
    color="grey50",
    data=text.dt)+
  geom_segment(aes(
    estimate_min_all, log10.p_mean_all,
    xend=estimate_max_all, yend=log10.p_mean_all,
    color=Data),
    data=compare.wide)+
  geom_segment(aes(
    estimate_mean_all, log10.p_min_all,
    xend=estimate_mean_all, yend=log10.p_max_all,
    color=Data),
    data=compare.wide)+
  geom_point(aes(
    estimate_mean_all, log10.p_mean_all,
    color=Data,
    fill=group.type),
    shape=21,
    data=compare.wide)+
  ggrepel::geom_label_repel(aes(
    estimate_mean_all, log10.p_mean_all, color=Data,
    label=Data),
    alpha=0.75,
    size=2.8,
    data=compare.wide)+
  scale.fill+
  scale_color_discrete(guide="none")+
  scale_y_continuous(
    "log10(p-value)\n<- highly significant --- not significant ->",
    breaks=seq(-100,0,by=2))+
  scale_x_continuous(
    "Percent test error difference (all-same)",
    breaks=seq(-100,10,by=2))+
  coord_cartesian(
    xlim=c(-4,10),
    ylim=c(-7,0))
png("data_Classif_batchmark_registry_scatter_all_segments.png", width=5, height=4, units="in", res=200)
print(gg)
dev.off()

text.y <- -12
text.dt <- rbind(
  tlab(15, -1, "p<0.05"),
  tlab(-4.5, text.y, "Accurate"),
  tlab(10, text.y, "Inaccurate"))
set.seed(3)
gg <- ggplot()+
  theme_bw()+
  theme(legend.position=c(0.5, 0.2))+
  ggtitle("Accurate prediction on a new subset?")+
  geom_hline(yintercept=log10(0.05),color="grey")+
  geom_vline(xintercept=0,color="grey")+
  geom_segment(aes(
    estimate_min_other, log10.p_mean_other,
    xend=estimate_max_other, yend=log10.p_mean_other,
    color=Data),
    data=compare.wide)+
  geom_segment(aes(
    estimate_mean_other, log10.p_min_other,
    xend=estimate_mean_other, yend=log10.p_max_other,
    color=Data),
    data=compare.wide)+
  geom_text(aes(
    x, y, label=label, color=NULL),
    vjust=0,
    color="grey50",
    data=text.dt)+
  geom_point(aes(
    estimate_mean_other, log10.p_mean_other,
    color=Data,
    fill=group.type),
    shape=21,
    data=compare.wide)+
  ggrepel::geom_label_repel(aes(
    estimate_mean_other, log10.p_mean_other, color=Data,
    label=Data),
    alpha=0.75,
    size=3,
    data=compare.wide)+
  scale.fill+
  scale_color_discrete(guide="none")+
  scale_y_continuous(
    "log10(p-value)\n<- highly significant --- not significant ->",
    breaks=seq(-100,0,by=2))+
  scale_x_continuous(
    "Percent test error difference (other-same)",
    breaks=seq(-10,100,by=10))+
  coord_cartesian(
    xlim=c(-10,40),
    ylim=c(-20,0))
png("data_Classif_batchmark_registry_scatter_other_segments.png", width=5, height=4, units="in", res=200)
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
    fill=group.type),
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
png("data_Classif_batchmark_registry_scatter_other_segments_flip.png", width=5, height=4, units="in", res=200)
print(gg)
dev.off()
other.xt <- wide.xt[order(estimate_min_other), .(
  Data=gsub("_","\\\\_",Data),
  ErrorDiff=min.comma.max(estimate_min_other,estimate_max_other),
  `log10(P)`=min.comma.max(log10.p_min_other,log10.p_max_other))]
print(xtable(other.xt, digits=1), type="latex", include.rownames=FALSE, sanitize.text.function=identity)

ymin <- -1
ymax <- 0
xmin <- -1
xmax <- 1
rect.dt <- data.table(xmin, xmax, ymin, ymax)
gg <- ggplot()+
  theme_bw()+
  theme(legend.position=c(0.5, 0.25))+
  ggtitle("Accurate prediction on a new subset?")+
  geom_rect(aes(
    xmin=xmin,
    xmax=xmax,
    ymin=ymin,
    ymax=ymax),
    color="grey50",
    fill=NA,
    data=rect.dt)+
  geom_hline(yintercept=log10(0.05),color="grey")+
  geom_vline(xintercept=0,color="grey")+
  geom_text(aes(
    x, y, label=label, color=NULL),
    vjust=0,
    data=text.dt)+
  geom_point(aes(
    estimate_other, log10.p_other,
    fill=group.type,
    color=Data),
    shape=21,
    data=compare.wide)+
  scale.fill+
  scale_color_discrete(guide="none")+
  ggrepel::geom_text_repel(aes(
    estimate_other,
    log10.p_other,
    color=Data,
    label=Data),
    size=4,
    data=compare.wide)+
  scale_y_continuous(
    "log10(p-value)\n<- highly significant --- not significant ->",
    limits=c(-20,0),
    breaks=seq(-100,0,by=10))+
  scale_x_continuous(
    "Percent test error difference (other-same)",
    breaks=seq(-10,100,by=10))
##limits=c(-10,NA))
png("data_Classif_batchmark_registry_scatter_other.png", width=6, height=4, units="in", res=200)
print(gg)
dev.off()
text.dt <- rbind(
  ## tlab(15, 0, "No significant difference"),
  ## tlab(15, -40, "Highly\nsignificant\ndifference"),
  tlab(0, -30, "Same accuracy"),
  tlab(60, -30, "Worse accuracy"))
gg <- ggplot()+
  theme_bw()+
  theme(legend.position="none")+
  geom_hline(yintercept=log10(0.05),color="grey")+
  geom_vline(xintercept=0,color="grey")+
  ## geom_text(aes(
  ##   x, y, label=label, color=NULL),
  ##   vjust=0,
  ##   data=text.dt)+
  scale.fill+
  scale_color_discrete(guide="none")+
  geom_point(aes(
    estimate_other, log10.p_other,
    fill=group.type,
    color=Data),
    shape=21,
    data=compare.wide)+
  ggrepel::geom_text_repel(aes(
    estimate_other, log10.p_other, color=Data,
    label=Data),
    size=4,
    data=compare.wide)+
  scale_y_continuous(
    "log10(p-value)",
    limits=c(ymin,ymax),
    breaks=seq(-10,10,by=1))+
  scale_x_continuous(
    "Percent test error difference (other-same)",
    breaks=seq(-10,10,by=1),
    limits=c(xmin,xmax))
png("data_Classif_batchmark_registry_scatter_other_zoom.png", width=3.2, height=3, units="in", res=200)
print(gg)
dev.off()

system("cd /projects/genomic-ml && unpublish_data cv-same-other-paper && publish_data projects/cv-same-other-paper")

tt.join <- score.join[
  grepl("train|test",group.small.name)
][
, predefined.set := test.group
][]
gg <- ggplot()+
  geom_point(aes(
    percent.error, train.groups, color=algorithm),
    shape=1,
    data=tt.join)+
  facet_grid(predefined.set ~ rows + Data +  `test%`, labeller=label_both)+
  scale_x_continuous(
    "Percent prediction error on CV test set in predefined set (one dot for each of 10 folds in CV)")+
  scale_y_discrete(
    "Predefined set(s) used for CV train set")
png("data_Classif_batchmark_registry_glmnet_featureless.png", width=20, height=4, units="in", res=100)
print(gg)
dev.off()

dot.both <- dcast(
  tt.join,
  Data + task_id + `test%` + rows + train.groups + predefined.set + algorithm ~ .,
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
gg <- ggplot()+
  theme(panel.spacing=grid::unit(1, "lines"))+
  geom_text(aes(
    Inf, Inf, label=sprintf("%d rows", value)),
    hjust=1,
    vjust=1.1,
    data=meta.long)+
  geom_segment(aes(
    percent.error_q25, train.groups,
    xend=percent.error_q75, yend=train.groups),
    data=dot.counts)+
  geom_point(aes(
    percent.error_median, train.groups),
    shape=1,
    data=dot.counts)+
  geom_text(aes(
    percent.error_median, train.groups,
    label=sprintf("%.1f", percent.error_median)),
    vjust=1.5,
    data=dot.counts)+
  facet_grid(predefined.set ~ rows + Data +  `test%`, labeller=label_both, scales="free")+
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
    percent.error_mean+percent.error_sd,
    train.groups,
    xend=percent.error_mean-percent.error_sd,
    yend=train.groups),
    data=dot.counts)+
  geom_point(aes(
    percent.error_mean, train.groups),
    shape=1,
    data=dot.counts)+
  geom_text(aes(
    percent.error_median, train.groups,
    label=sprintf("%.1f", percent.error_median)),
    vjust=1.5,
    data=dot.counts)+
  facet_grid(predefined.set ~ rows + Data +  `test%`, labeller=label_both, scales="free")+
  scale_x_continuous(
    "Percent prediction error on CV test set in predefined set (mean +/- SD over 10 folds)")+
  scale_y_discrete(
    "Predefined set(s) used for glmnet CV train set")
png("data_Classif_batchmark_registry_glmnet_mean_sd.png", width=20, height=4, units="in", res=100)
print(gg)
dev.off()

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
show.p <- meta.dt[
  p.each.group[show.names,on="Data"],
  on=.(Data,group.type)
][
, `:=`(
  train.groups = paste0(compare_name,"-same"),
  algorithm = "cv_glmnet",
  predefined.set = test.group
)][]
text.size <- 3.5
gg <- ggplot()+
  theme_bw()+
  ##theme(panel.spacing=grid::unit(1, "lines"))+
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
    compare_mean, train.groups,
    color=algorithm,
    xend=same_mean, yend=train.groups),
    size=1,
    data=show.p)+
  geom_text(aes(
    pmax(compare_mean, same_mean),
    train.groups,
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
    train.groups,
    color=algorithm,
    xend=percent.error_mean-percent.error_sd,
    yend=train.groups),
    size=1,
    data=show.both)+
  geom_point(aes(
    percent.error_mean, train.groups, color=algorithm),
    shape=1,
    data=show.both)+
  geom_text(aes(
    percent.error_median, train.groups,
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
    "Predefined set(s) used for CV train set")
png("data_Classif_batchmark_registry_glmnet_featureless_mean_sd.png", width=7.5, height=4, units="in", res=200)
print(gg)
dev.off()

meta.not.tt <- meta.dt[data.name=="MNIST_EMNIST_rot"]
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
