library(ggplot2)
library(data.table)
work.dir <- "/scratch/th798/cv-same-other-paper"
reg.csv <- file.path(work.dir, "data_Classif_batchmark_sizes_registry.csv")
score.dt <- fread(reg.csv)

meta.dt <- data.table::fread("data-meta.csv")[
  grepl("train|test",small_group), `:=`(
    test=ifelse(small_group=="test", small_N, large_N),
    train=ifelse(small_group=="train", small_N, large_N)
  )
][
, `test%` := as.integer(100*test/rows)
][]
score.dt[
, percent.error := 100*classif.ce
][
, data.name := task_id
]
score.join <- meta.dt[score.dt, on="data.name"]

data.list <- split(score.join, score.join$data.name)
for(data.name in names(data.list)){
}

    gg <- ggplot()+
      ggtitle(paste("Data set:", meta.row$data.name))+
      geom_point(aes(
        percent.error, train.groups, color=algorithm),
        shape=1,
        data=scores.not)+
      facet_grid(. ~ test.group, labeller=label_both, scales="free")+
      scale_x_continuous(
        "Percent prediction error on CV test group (one dot for each of 10 folds in CV)")+
      scale_y_discrete(
        "Train groups")
    out.png <- sprintf(
      "data_Classif_batchmark_registry_glmnet_featureless_%s.png",
      meta.row$data.name)
    png(out.png, width=8, height=2, units="in", res=200)
    print(gg)
    dev.off()
  }
}
system("cd /projects/genomic-ml && unpublish_data cv-same-other-paper && publish_data projects/cv-same-other-paper")
