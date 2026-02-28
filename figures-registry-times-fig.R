library(data.table)
library(ggplot2)
byvec <- c(
  "figures-registry-times/data_Classif_batchmark_algos_registry.csv"=20, 
  "figures-registry-times/data_Classif_batchmark_registry.csv"=100, 
  "figures-registry-times/data_Classif_batchmark_sizes_registry.csv"=200)
for(times.csv in names(byvec)){
  brby=byvec[[times.csv]]
  times_dt <- fread(times.csv)[, let(
    row_i = .I
  )][, let(
    row_in_data = seq_len(.N)
  ), by=task_id][
  , task_id := gsub("Fashion", "Fashion\n", gsub("_","\n",task_id))
  ]
  unit_all_dt <- nc::capture_melt_single(
    times_dt,
    time="started|done",
    value.name="POSIXct"
  )[
  , seconds := as.numeric(POSIXct-min(POSIXct))
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
    value.var=c("seconds","minutes","hours","days"))
  levs <- all_dt[, .(
    first=min(hours_started)
  ), by=task_id][order(first), task_id]
  all_dt[, Data := factor(task_id, levs)]
  all_dt[, .(
    cpu_days=sum(days_done-days_started),
    wall_hours=max(hours_done)
  )]
  gg <- ggplot()+
    theme_bw()+
    ggtitle(times.csv)+
    theme(
      legend.position=c(0.2, 0.1),
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
    scale_y_reverse(
      "train/test split in data set",
      breaks=seq(brby, 10000, by=brby))+
    scale_x_continuous("hours from start of computation")+
    facet_grid(Data~., scales="free", space="free")
  out.png <- sub("csv","png",times.csv)
  png(out.png, width=7, height=15, units="in", res=200)
  print(gg)
  dev.off()
}

