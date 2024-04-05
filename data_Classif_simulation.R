library(ggplot2)
library(data.table)
off1 <- 0
xmax <- 20
set.seed(1)
N <- 500
x.vec <- seq(-xmax, xmax, l=N)
fun.dt.list <- list()
n.off <- 3
n.off <- 5
for(off2 in seq(0, pi, l=n.off)){
  difference <- off2-off1
  fun.list <- list()
  off.list <- c(off1,off2)
  for(off.i in seq_along(off.list)){
    Group <- factor(off.i)
    fun.dt.list[[paste(off2, off.i)]] <- data.table(
      off2, difference, off.i, Group, x=x.vec,
      true.f=sin(x.vec+off.list[[off.i]]),
      noise=rnorm(N))
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

difficulty.vec <- c(0.01,0.5,10)
difficulty.vec <- c(0.01,0.1,1,10)
sim.dt <- data.table(difficulty=difficulty.vec)[, {
  fun.dt[, data.table(
    x,
    y=ifelse(true.f+noise*difficulty<0, 0, 1),
    Group, off.i
  ), by=similarity]
}, by=difficulty]
ggplot()+
  geom_point(aes(
    x, y+ifelse(off.i==2, 0.1, 0),
    color=Group),
    shape=1,
    data=sim.dt)+
  facet_grid(
    difficulty ~ similarity,
    labeller=label_both,
    scales="free")

knn.learner <- mlr3learners::LearnerClassifKKNN$new()
knn.learner$param_set$values$k <- 3
featureless.learner <- mlr3::LearnerClassifFeatureless$new()
learner.list <- list(knn.learner, featureless.learner)
task.dt <- sim.dt[, .(task=list(mlr3::TaskClassif$new(
  paste0("similarity=",similarity,",difficulty=",difficulty),
  data.table(x, y=factor(y), Group),
  target="y"
)$set_col_roles("Group",c("group","stratum")))), by=.(similarity,difficulty)]
same.other.cv <- mlr3resampling::ResamplingSameOtherCV$new()$instantiate(
  task.dt$task[[1]])
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
  nc::field("difficulty","=",".*",as.numeric)
), existing.error=FALSE)

## why aren't the test error numbers identical in different similarity
## data sets? is there some randomness in kknn predictions?
(should.be.same <- score.dt[
  test.fold==1 & train.groups=="same" & difficulty==0.01 &
    algorithm=="featureless" & test.group==2])
dcast(
  score.dt,
  test.group+test.fold+ train.groups+ difficulty+algorithm ~ similarity,
  value.var="classif.ce")
ggplot()+
  theme_bw()+
  geom_point(aes(
    classif.ce, train.groups, color=algorithm),
    shape=1,
    data=score.dt)+
  facet_grid(
    similarity ~ difficulty,
    labeller=label_both, scales="free")

score.mean <- dcast(
  score.dt,
  task_id + difficulty + similarity ~ algorithm + train.groups,
  mean,
  value.var="classif.ce"
)[, `:=`(
  logratio_same_all=log10(kknn_same/kknn_all),
  logratio_same_other=log10(kknn_same/kknn_other),
  logratio_knn_featureless=log10(kknn_same/featureless_same)
)][]
ggplot()+
  geom_line(aes(
    similarity, kknn_other, color=factor(difficulty)),
    data=score.mean)


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
    logratio_same_other),
    data=score.mean)+
  ggrepel::geom_text_repel(aes(
    logratio_knn_featureless,
    logratio_same_other,
    label=sub(",","\n",similarity)),
    data=score.mean)+
  facet_grid(. ~ difficulty, labeller=label_both)
    
