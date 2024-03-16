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
  data.scores <- data.list[[data.name]]
  full.size <- data.scores[atoms==n.train.atoms]
  gg <- ggplot()+
    ggtitle(paste("Data set:", data.name))+
    geom_point(aes(
      percent.error, train.groups),
      shape=1,
      data=full.size)+
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

  p.color <- "red"
  score.stats <- dcast(
    full.size,
    train.groups + test.group + algorithm ~ .,
    list(mean, sd, length),
    value.var="percent.error")
  score.wide.train <- dcast(
    full.size,
    seed + test.fold + test.group ~ train.groups,
    value.var="percent.error")
  score.wide.train.compare <- melt(
    score.wide.train,
    measure.vars=c("all","other"),
    variable.name="train.groups")
  min.max.dt <- dcast(
    full.size,
    test.group ~ .,
    list(min, max),
    value.var="percent.error"
  )[, per  _mid := (percent.error_min+percent.error_max)/2]
  glmnet.same <- score.stats[
    algorithm=="cv_glmnet" & train.groups=="same"
  ][
  , same_mean := regr.mse_mean
  ][]
  vline.dt <- glmnet.same[
  , .(same_mean, `Test group`, Rows)]
  train.p.values <- score.wide.train.compare[, {
    test.res <- t.test(
      value,
      same,
      alternative = "two.sided",
      paired=TRUE)
    with(test.res, data.table(algorithm="cv_glmnet", p.value, estimate))
  }, by=.(train.groups,test.group)
  ][
    score.stats, on=c("train.groups","test.group","algorithm"), nomatch=0L
  ][
    min.max.dt, on=c("test.group"), nomatch=0L
  ][
    glmnet.same[, .(same_mean, test.group)], on=c("test.group")
  ]
  print(train.p.values[order(p.value), .(train.groups, test.group, p.value, estimate)])
  train.list <- list(
    same="same",
    other=c("same","other"),
    all=c("same","other","all"))
  for(suffix in names(train.list)){
    some <- function(DT){
      DT[train.groups %in% train.list[[suffix]] ]
    }
    some.scores <- some(full.size)
    some.stats <- some(score.stats)
    gg <- ggplot()+
      ggtitle(tit)+
      theme(axis.text.x=element_text(angle=30, hjust=1))+
      geom_point(aes(
        regr.mse, algorithm),
        shape=1,
        data=some.scores)+
      geom_blank(aes(
        regr.mse, algorithm),
        data=full.size)+
      facet_grid(
        `Train\ngroups` ~ `Test group`,
        scales="free",
        labeller=label_both)+
      scale_x_log10("Mean squared prediction error (test set)")
    n.test <- length(unique(full.size$test.group))
    (comparison.png <- sub("RData", paste0(suffix, ".png"), comparison.RData))
    png(comparison.png, height=3, width=(n.test+1)*1.5, units="in", res=200)
    print(gg)
    dev.off()
    gg <- ggplot()+
      ggtitle(tit)+
      theme_bw()+
      theme(
        panel.spacing=grid::unit(0, "lines"),
        axis.text.x=element_text(angle=30, hjust=1))+
      geom_vline(aes(
        xintercept=same_mean),
        data=vline.dt,
        color="grey50")+
      geom_text(aes(
        regr.mse_mean, algorithm,
        hjust=ifelse(
          log10(regr.mse_mean)<log10.regr.mse_mid,
          0, 1),
        label=ifelse(
          p.value<0.0001,
          "p<0.0001",
          sprintf("p=%.4f",p.value))),
        color=p.color,
        vjust=-0.8,
        size=3,
        data=some(train.p.values))+
      geom_point(aes(
        regr.mse_mean, algorithm),
        shape=1,
        data=some.stats)+
      geom_segment(aes(
        regr.mse_mean+regr.mse_sd, algorithm,
        xend=regr.mse_mean-regr.mse_sd, yend=algorithm),
        linewidth=1,
        data=some.stats)+
      geom_segment(aes(
        same_mean, algorithm,
        xend=regr.mse_mean, yend=algorithm),
        data=some(train.p.values),
        color=p.color)+
      geom_blank(aes(
        regr.mse, algorithm),
        data=full.size)+
      facet_grid(
        `Train\ngroups` ~ Rows + `Test group`,
        scales="free",
        labeller=label_both)+
      scale_x_log10(
        "Mean squared prediction error on test set\n(mean +/- SD over 10 folds, log scale, paired t-test in red)")
    print(comparison.png <- sub("RData", paste0(suffix, "-stats.png"), comparison.RData))
    png(comparison.png, height=3.5, width=(n.test+1)*1.5, units="in", res=200)
    print(gg)
    dev.off()

    
}
system("cd /projects/genomic-ml && unpublish_data cv-same-other-paper && publish_data projects/cv-same-other-paper")
