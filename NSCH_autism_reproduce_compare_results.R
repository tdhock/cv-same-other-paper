library(data.table)
library(ggplot2)
proj_dt <- fread("NSCH_autism_reproduce_proj_results.csv")
seq_dt <- fread("NSCH_autism_reproduce_proj_results_sequential.csv")
reg_dt <- fread("NSCH_autism_reproduce_registry_scores.csv")

compare_dt <- rbind(
  proj_dt[, .(computation="mpi", learner_id, train.subsets, test.subset, test.fold, n.train.groups, groups, classif.auc, classif.acc)],
  seq_dt[, .(computation="local", learner_id, train.subsets, test.subset, test.fold, n.train.groups, groups, classif.auc, classif.acc)],
  reg_dt[, .(computation="batchtools", learner_id, train.subsets, test.subset, test.fold, n.train.groups, groups, classif.auc, classif.acc)]
)[learner_id=="classif.cv_glmnet" & groups==n.train.groups]
ucomp <- unique(compare_dt$computation)
yfac <- function(x)factor(x, c(
  ##"",
  unique(compare_dt$computation)))
compare_dt[, let(
  train=train.subsets,
  test=test.subset,
  AUC = classif.auc,
  Computation = yfac(computation)
)][]
compare_stats <- dcast(
  compare_dt,
  Computation+train+test~.,
  list(mean,sd,length),
  value.var="AUC")

compare_wide <- dcast(
  compare_dt,
  train+test+test.fold~Computation,
  value.var=c("AUC","classif.acc"))
compare_long <- melt(
  compare_wide,
  measure.vars=paste0("AUC_", ucomp[-1]),
  value.name = "AUC_compare",
  variable.name="compare")
test_dt <- compare_long[, {
  AUC_ref <- .SD[[paste0("AUC_", ucomp[1])]]
  tlist <- t.test(AUC_compare, AUC_ref, paired=TRUE)
  with(tlist, data.table(p.value, N=.N))
}, by=.(train, test, compare)]


gg <- ggplot()+
  test_dt[, ggtitle(sprintf("No significant differences between Computation methods\n(different random train/test splits, different random subtrain/validation splits)\nP-value range: %.2f–%.2f", min(p.value), max(p.value)))]+
  geom_point(aes(
    AUC_mean, Computation),
    data=compare_stats)+
  geom_text(aes(
    AUC_mean, Computation, label=sprintf(
      "%.4f±%.4f", AUC_mean, AUC_sd)),
    vjust=-0.5,
    size=3,
    data=compare_stats)+
  geom_segment(aes(
    AUC_mean+AUC_sd, Computation,
    xend=AUC_mean-AUC_sd, yend=Computation),
    data=compare_stats)+
  scale_y_discrete(
    "Train subsets",
    drop=FALSE)+
  scale_x_continuous(
    "cv_glmnet performance on test subset (mean±SD over 10 folds in CV)")+
  facet_grid(train~test, scales="free", labeller=label_both)
png(
  "NSCH_autism_reproduce_compare_results.png",
  width=8, height=4, units="in", res=200)
print(gg)
dev.off()



