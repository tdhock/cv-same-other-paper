library(data.table)
library(ggplot2)
seq_dt <- fread("NSCH_autism_reproduce_proj_results_sequential.csv")
proj_dt <- fread("NSCH_autism_reproduce_proj_results.csv")
reg_dt <- fread("NSCH_autism_reproduce_registry_jobs.csv")
minutes <- function(seconds)as.numeric(seconds)/60
all_dt <- rbind(
  seq_dt[, .(cpus=1, host="laptop", pkg="mlr3resampling", started=start.time, done=end.time, learner_id, job.id=process)],
  proj_dt[, .(cpus=100, host="rorqual", pkg="mlr3resampling", started=start.time, done=end.time, learner_id, job.id=process)],
  reg_dt[, .(cpus=240, host="rorqual", pkg="mlr3batchmark", started, done, learner_id, job.id)]
)[, let(
  start.minutes=minutes(started-min(started)),
  end.minutes=minutes(done-min(started))
), by=.(pkg,cpus,host)]

both_dt <- all_dt[cpus==1]
gg <- ggplot()+
  facet_grid(pkg+cpus+host~., labeller=label_both, scales="free", space="free")+
  scale_y_continuous(breaks=seq(0, 300, by=50))+
  scale_x_continuous(
    "Minutes from start of computation")+
  geom_segment(aes(
    start.minutes, job.id,
    color=learner_id,
    xend=end.minutes, yend=job.id),
    data=both_dt)+
  geom_point(aes(
    start.minutes, job.id,
    color=learner_id),
    shape=21,
    fill="white",
    data=both_dt)+
  theme(legend.position=c(0.3, 0.8))
png(
  "NSCH_autism_reproduce_timings.png",
  width=8, height=5, units="in", res=200)
print(gg)
dev.off()
