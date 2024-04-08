library(ggplot2)
library(data.table)
off1 <- 0
xmax <- 10
set.seed(1)
x.vec <- seq(-xmax, xmax, l=100)
fun.dt.list <- list()
for(off2 in seq(0, pi, l=5)){
  difference <- off2-off1
  fun.list <- list()
  off.list <- c(off1,off2)
  for(off.i in seq_along(off.list)){
    Group <- factor(off.i)
    off.val <- off.list[[off.i]]
    true.f <- sin(x.vec+off.val)
    fun.dt.list[[paste(off2, off.i)]] <- data.table(
      off2, difference, off.i, Group, x=x.vec, true.f)
  }
}
(fun.dt <- rbindlist(
  fun.dt.list
)[
, diff.prop := difference/max(difference)
][
, similarity := 1-diff.prop
])
ggplot()+
  geom_line(aes(
    x, true.f,
    size=Group,
    color=Group),
    data=fun.dt)+
  facet_grid(similarity ~ ., labeller=label_both)+
  scale_size_manual(
    values=c("1"=2, "2"=1))

sim.dt <- data.table(difficulty=c(0.01,0.5,10))[, {
  fun.dt[, data.table(
    x,
    y=true.f+rnorm(.N, sd=difficulty),
    Group
  ), by=similarity]
}, by=difficulty]
ggplot()+
  geom_point(aes(
    x, y,
    color=Group),
    shape=1,
    data=sim.dt)+
  facet_grid(
    difficulty ~ similarity,
    labeller=label_both,
    scales="free")

knn.learner <- mlr3learners::LearnerRegrKKNN$new()
featureless.learner <- mlr3::LearnerRegrFeatureless$new()
learner.list <- list(knn.learner, featureless.learner)
same.other.cv <- mlr3resampling::ResamplingSameOtherCV$new()
task.dt <- sim.dt[, .(task=list(mlr3::TaskRegr$new(
  paste0("similarity=",similarity,",difficulty=",difficulty),
  data.table(x, y, Group),
  target="y"
)$set_col_roles("Group",c("group","stratum")))), by=.(similarity,difficulty)]
bgrid <- mlr3::benchmark_grid(
  task.dt$task,
  learner.list,
  same.other.cv)
bres <- mlr3::benchmark(bgrid)
score.dt <- mlr3resampling::score(bres)
score.dt[1]
nc::capture_first_df(score.dt, task_id=list(
  nc::field("similarity","=",".*?",as.numeric),
  ",",
  nc::field("difficulty","=",".*",as.numeric)), existing.error=FALSE)

ggplot()+
  theme_bw()+
  geom_point(aes(
    regr.mse, train.groups),
    shape=1,
    data=score.dt)+
  facet_grid(
    similarity+algorithm ~ difficulty,
    labeller=label_both, scales="free")+
  scale_x_log10()

score.mean <- dcast(
  score.dt,
  task_id + difficulty + similarity ~ algorithm + train.groups,
  mean,
  value.var="regr.mse"
)[, `:=`(
  logratio_same_all=log10(kknn_same/kknn_all),
  logratio_knn_featureless=log10(kknn_same/featureless_same)
)][]
line.color <- "grey"
ggplot()+
  theme_bw()+
  geom_vline(
    color=line.color,
    xintercept=0)+
  geom_hline(
    color=line.color,
    yintercept=0)+
  geom_point(aes(
    logratio_knn_featureless,
    logratio_same_all),
    data=score.mean)+
  ggrepel::geom_text_repel(aes(
    logratio_knn_featureless,
    logratio_same_all,
    label=sub(",","\n",task_id)),
    data=score.mean)
    
