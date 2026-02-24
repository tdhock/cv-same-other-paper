library(data.table)
library(ggplot2)
proj_dt <- fread("NSCH_autism_reproduce_proj_results.csv")
seq_dt <- fread("NSCH_autism_reproduce_proj_results_sequential.csv")
reg_dt <- fread("NSCH_autism_reproduce_registry_scores.csv")

compare_dt <- rbind(
  proj_dt[, .(fun="compute_all_mpi", learner_id, train.subsets, test.subset, test.fold, n.train.groups, classif.auc)],
  seq_dt[, .(fun="compute_all", learner_id, train.subsets, test.subset, test.fold, n.train.groups, classif.auc)],
  reg_dt[, .(fun="reduceResultsBatchmark", learner_id, train.subsets, test.subset, test.fold, n.train.groups, classif.auc)]
)[learner_id=="classif.cv_glmnet"]
yfac <- function(x)factor(x, c(
  unique(compare_dt$fun),
  ##"",
  NULL))
compare_dt[, let(
  AUC = classif.auc,
  Fun = yfac(fun),
  N_train=n.train.groups
)][]
compare_stats <- dcast(
  compare_dt,
  Fun+train.subsets+test.subset+N_train~.,
  list(mean,sd,length),
  value.var="AUC")

compare_wide <- dcast(compare_dt, train.subsets+N_train+test.subset+test.fold~fun, value.var="AUC")
compare_long <- nc::capture_melt_single(
  compare_wide,
  nc::field("compute", "_", ".+"),
  value.name = "compute_AUC"
)[
, ref_AUC := reduceResultsBatchmark
][]
test_dt <- compare_long[, {
  tlist <- t.test(ref_AUC, compute_AUC, paired=TRUE)
  with(tlist, data.table(p.value, N=.N))
}, by=.(train.subsets, N_train, test.subset, compute)]

gg <- ggplot()+
  test_dt[, ggtitle(sprintf("No significant differences\nbetween results computed using different random seeds\nP-value range: %.2f–%.2f", min(p.value), max(p.value)))]+
  geom_point(aes(
    AUC_mean, Fun),
    data=compare_stats)+
  geom_text(aes(
    AUC_mean, Fun, label=sprintf(
      "%.4f±%.4f", AUC_mean, AUC_sd)),
    vjust=-0.5,
    size=3,
    data=compare_stats)+
  geom_segment(aes(
    AUC_mean+AUC_sd, Fun,
    xend=AUC_mean-AUC_sd, yend=Fun),
    data=compare_stats)+
  scale_y_discrete(
    "Train subsets",
    drop=FALSE)+
  scale_x_continuous(
    "cv_glmnet performance on test subset (mean±SD over 10 folds in CV)")+
  facet_grid(train.subsets+N_train~test.subset, scales="free", labeller=label_both)
png(
  "NSCH_autism_reproduce_compare_results.png",
  width=8, height=9, units="in", res=200)
print(gg)
dev.off()



