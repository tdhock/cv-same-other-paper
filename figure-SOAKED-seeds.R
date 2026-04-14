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
(p_dt <- rbindlist(p_dt_list))
other_dt <- p_dt[Train_subsets=="other-same"]

thresh_dt <- data.table(prob=0.05)
gg <- ggplot()+
  geom_vline(aes(
    xintercept=prob),
    data=thresh_dt)+
  geom_point(aes(
    p.paired, test.subset, color=sample_size=="full"),
    data=other_dt)+
  facet_grid(task_id ~ .)+
  scale_x_log10()
png("figure-SOAKED-seeds.png", width=8, height=4, units="in", res=200)
print(gg)
dev.off()
