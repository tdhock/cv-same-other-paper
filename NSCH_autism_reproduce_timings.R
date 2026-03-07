library(data.table)
library(ggplot2)
seq_dt <- fread("NSCH_autism_reproduce_proj_results_sequential.csv")
proj_dt <- fread("NSCH_autism_reproduce_proj_results.csv")
reg_dt <- fread("NSCH_autism_reproduce_registry_jobs.csv")
sec_all_dt <- rbind(
  seq_dt[, .(cpus=1, host="laptop", pkg="mlr3resampling", started=start.time, done=end.time, learner_id, job.id=process, row=.I)],
  proj_dt[, .(cpus=100, host="rorqual", pkg="mlr3resampling", started=start.time, done=end.time, learner_id, job.id=process, row=.I)],
  reg_dt[, .(cpus=240, host="rorqual", pkg="mlr3batchmark", started, done, learner_id, job.id, row=.I)])
unit_all_dt <- nc::capture_melt_single(
  sec_all_dt,
  time="started|done",
  value.name="POSIXct"
)[
, seconds := as.numeric(POSIXct-min(POSIXct)), by=.(pkg,cpus,host)
][
, minutes := seconds/60
][
, hours := minutes/60
]
all_dt <- dcast(
  unit_all_dt,
  cpus+host+pkg+learner_id+job.id+row ~ time,
  value.var=c("seconds","minutes","hours"))

both_dt <- all_dt[cpus>1]
gg <- ggplot()+
  facet_grid(pkg+cpus+host~., labeller=label_both, scales="free", space="free")+
  scale_y_continuous(breaks=seq(0, 300, by=50))+
  scale_x_continuous(
    "Minutes from start of computation")+
  geom_segment(aes(
    minutes_started, job.id,
    color=learner_id,
    xend=minutes_done, yend=job.id),
    data=both_dt)+
  geom_point(aes(
    minutes_started, job.id,
    color=learner_id),
    shape=21,
    fill="white",
    data=both_dt)+
  theme(legend.position=c(0.3, 0.8))
png(
  "NSCH_autism_reproduce_timings_rorqual.png",
  width=8, height=5, units="in", res=200)
print(gg)
dev.off()

gg <- ggplot()+
  facet_grid(pkg+cpus+host~., labeller=label_both, scales="free")+
  scale_y_continuous(breaks=seq(0, 300, by=50))+
  scale_x_continuous(
    "Hours from start of computation")+
  geom_segment(aes(
    hours_started, job.id,
    color=learner_id,
    xend=hours_done, yend=job.id),
    data=all_dt)+
  geom_point(aes(
    hours_started, job.id,
    color=learner_id),
    shape=21,
    fill="white",
    data=all_dt)+
  theme(legend.position=c(0.3, 0.8))
png(
  "NSCH_autism_reproduce_timings.png",
  width=8, height=5, units="in", res=200)
print(gg)
dev.off()
