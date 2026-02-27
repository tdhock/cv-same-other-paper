library(data.table)
library(ggplot2)
times_dt <- fread("data_Classif_batchmark_registry_times.csv")[, let(
  row_i = .I
)][, let(
  row_in_data = seq_len(.N)
), by=task_id]
unit_all_dt <- nc::capture_melt_single(
  times_dt,
  time="started|done",
  value.name="POSIXct"
)[
, seconds := as.numeric(POSIXct-min(POSIXct)), 
][
, minutes := seconds/60
][
, hours := minutes/60
][
, days := hours/24
]
all_dt <- dcast(
  unit_all_dt,
  algorithm+task_id+repl+row_i+row_in_data ~ time,
  value.var=c("seconds","minutes","hours","days")
)[, Data := factor(task_id, unique(task_id))]
all_dt[, .(
  cpu_days=sum(days_done-days_started),
  wall_hours=max(hours_done)
)]

gg <- ggplot()+
  theme_bw()+
  theme(
    legend.position=c(0.8, 0.2),
    panel.spacing=grid::unit(0, "lines"))+
  geom_point(aes(
    hours_started, row_in_data,
    color=algorithm),
    shape=21,
    data=all_dt)+
  scale_color_manual(values=c(
    featureless="blue",
    cv_glmnet="red",
    rpart="black"))+
  geom_segment(aes(
    hours_started, row_in_data,
    color=algorithm,
    xend=hours_done, yend=row_in_data),
    data=all_dt)+
  scale_y_reverse("train/test split in data set")+
  scale_x_continuous("hours from start of computation")+
  facet_grid(Data~.)
png("data_Classif_batchmark_registry_times.png", width=5, height=11, units="in", res=200)
print(gg)
dev.off()
