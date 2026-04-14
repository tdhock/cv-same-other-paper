library(data.table)
library(ggplot2)

score_dt <- fread("figure-SOAKED-seeds-data.csv")

setkey(score_dt, task_id, test.subset, seed)
comb_dt <- unique(score_dt[, .(task_id, test.subset, seed)])
p_dt_list <- list()
for(comb_i in 1:nrow(comb_dt)){
  comb_row <- comb_dt[comb_i]
  score_sub <- score_dt[comb_row]
  dlist <- mlr3resampling::pvalue_downsample(score_sub)
  p_dt_list[[comb_i]] <- data.table(comb_row, dlist$pvalues)
}

cvals <- c(
  "full (Same/Other different sizes)"="blue",
  "downsample Same/Other to min train size\n100 random seeds"="red")
(p_dt <- rbindlist(p_dt_list)[, let(
  Sample_size = ifelse(sample_size=="full", names(cvals)[1], names(cvals)[2]),
  Simulation = task_id
)])
other_dt <- p_dt[Train_subsets=="other-same"]

thresh_dt <- data.table(prob=0.05)
gg <- ggplot()+
  geom_vline(aes(
    xintercept=prob),
    data=thresh_dt)+
  geom_point(aes(
    p.paired, test.subset, color=Sample_size),
    data=other_dt)+
  facet_grid(task_id ~ .)+
  scale_x_log10()
png("figure-SOAKED-seeds-dots.png", width=8, height=4, units="in", res=200)
print(gg)
dev.off()

tsize <- 3
wside <- other_dt[sample_size!="full" & Simulation=="iid_easy"][p.paired<0.05]
wsum <- dcast(wside, Simulation + Sample_size + test.subset~., list(length, min, max), value.var="p.paired")
gg <- ggplot()+
  geom_vline(aes(
    xintercept=prob),
    data=thresh_dt)+
  geom_text(aes(
    0.02, test.subset, color=Sample_size, label=sprintf(
      "Significant difference\nfor %d seeds, %.3f<P<%.3f", p.paired_length, p.paired_min, p.paired_max)),
    hjust=1,
    size=tsize,
    vjust=1,
    data=wsum)+
  geom_text(aes(
    prob, Inf, label="P>0.05\n(test error difference\nnot significant)"),
    size=tsize,
    hjust=0,
    vjust=1,
    data=data.table(thresh_dt, Simulation="different"))+
  geom_text(aes(
    prob, test.subset, color=Sample_size, label="All downsample differences significant\n(as expected)"),
    size=tsize,
    data=data.table(prob=1e-3, test.subset="large", Sample_size=names(cvals)[2], Simulation="different"))+
  geom_text(aes(
    0.35, Inf, color=Sample_size, label="Most downsample\ndifferences not significant\n(as expected)"),
    size=tsize,
    vjust=1,
    data=data.table(Sample_size=names(cvals)[2], Simulation="iid_easy"))+
  geom_text(aes(
    prob, Inf, label="P<0.05 (significant difference between Same/Other test error) "),
    size=tsize,
    hjust=1,
    vjust=1,
    data=data.table(thresh_dt, Simulation="different"))+
  geom_boxplot(aes(
    p.paired, test.subset, color=Sample_size),
    coef=1,
    shape=21,
    data=other_dt)+
  facet_grid(Simulation ~ ., labeller=label_both)+
  scale_x_log10(
    "P-value for difference, 10-fold CV test error (Same-Other), paired T-test",
    breaks=10^seq(-10, 10))+
  theme(legend.position=c(0.2,0.2))+
  scale_color_manual(values=cvals, breaks=names(cvals))
png("figure-SOAKED-seeds.png", width=8, height=4, units="in", res=200)
print(gg)
dev.off()
