library(data.table)
library(ggplot2)
proj_dt <- fread("NSCH_autism_reproduce_proj_results.csv")
reg_dt <- fread("NSCH_autism_reproduce_registry_jobs.csv")
minutes <- function(seconds)as.numeric(seconds)/60
both_dt <- rbind(
  proj_dt[, .(pkg="mlr3resampling", started=start.time, done=end.time, learner_id, job.id=process)],
  reg_dt[, .(pkg="mlr3batchmark", started, done, learner_id, job.id)]
)[, let(
  start.minutes=minutes(started-min(started)),
  end.minutes=minutes(done-min(started))
), by=pkg]
  
gg <- ggplot()+
  facet_grid(pkg~., labeller=label_both, scales="free", space="free")+
  scale_y_continuous(breaks=seq(0, 300, by=50))+
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
