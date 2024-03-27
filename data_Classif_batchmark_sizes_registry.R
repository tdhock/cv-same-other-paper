library(ggplot2)
library(data.table)
work.dir <- "."
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
  print(data.name)
  data.scores <- data.list[[data.name]]
  full.size <- data.scores[atoms==n.train.atoms]
  tit <- ggtitle(paste("Data set:", data.name))
  gg <- ggplot()+
    tit+
    geom_point(aes(
      percent.error, train.groups),
      shape=1,
      data=full.size)+
    facet_grid(. ~ test.group, labeller=label_both, scales="free")+
    scale_x_continuous(
      "Percent prediction error on CV test group (one dot for each of 10 folds in CV)")+
    scale_y_discrete(
      "Train groups")
  dir.create("data_Classif_figures", showWarnings = FALSE)
  out.png <- sprintf(
    "data_Classif_figures/%s_error_each_fold.png",
    data.name)
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
  )[, percent.error_mid := (percent.error_min+percent.error_max)/2]
  glmnet.same <- score.stats[
    train.groups=="same"
  ][
  , same_mean := percent.error_mean
  ][]
  vline.dt <- glmnet.same[
  , .(same_mean, test.group)]
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
  gg <- ggplot()+
    tit+
    theme_bw()+
    theme(
      panel.spacing=grid::unit(0, "lines"),
      axis.text.x=element_text(angle=30, hjust=1))+
    geom_vline(aes(
      xintercept=same_mean),
      data=vline.dt,
      color="grey50")+
    geom_text(aes(
      percent.error_mean, train.groups,
      hjust=ifelse(
        percent.error_mean<percent.error_mid,
        0, 1),
      label=ifelse(
        p.value<0.0001,
        "p<0.0001",
        sprintf("p=%.4f",p.value))),
      color=p.color,
      vjust=-0.8,
      size=3,
      data=train.p.values)+
    geom_point(aes(
      percent.error_mean, train.groups),
      shape=1,
      data=score.stats)+
    geom_segment(aes(
      percent.error_mean+percent.error_sd, train.groups,
      xend=percent.error_mean-percent.error_sd, yend=train.groups),
      linewidth=1,
      data=score.stats)+
    geom_segment(aes(
      same_mean, train.groups,
      xend=percent.error_mean, yend=train.groups),
      data=train.p.values,
      color=p.color)+
    facet_grid(
      . ~ test.group,
      scales="free",
      labeller=label_both)+
    scale_x_continuous(
      "Percent prediction error on test set\n(mean +/- SD over 10 folds, paired t-test in red)")
  n.test <- length(unique(score.stats$test.group))
  out.png <- sprintf(
    "data_Classif_figures/%s_error_mean_sd.png",
    data.name)
  png(out.png, height=2, width=(n.test+1)*1.5, units="in", res=200)
  print(gg)
  dev.off()
  ggplot()+
    tit+
    geom_point(aes(
      n.train.atoms, percent.error,
      color=train.groups),
      shape=1,
      data=data.scores)+
    geom_line(aes(
      n.train.atoms, percent.error,
      color=train.groups,
      group=paste(train.groups, seed)),
      data=data.scores)+
    facet_grid(test.fold ~ test.group, labeller=label_both, scales="free")
  ggplot()+
    tit+
    geom_point(aes(
      n.train.atoms, percent.error,
      color=train.groups),
      shape=1,
      data=data.scores)+
    geom_line(aes(
      n.train.atoms, percent.error,
      color=train.groups,
      group=paste(train.groups, seed)),
      data=data.scores)+
    facet_grid(test.group ~ test.fold, labeller=label_both, scales="free")
  more.scores <- setkey(
    data.scores[, .SD[n.train.atoms %in% atoms], by=test.group],
    train.groups, n.train.atoms
  )[
  , train.groups.N := paste(train.groups, as.integer(n.train.atoms))
  ][
  , train.groups.N.fac := factor(train.groups.N, unique(train.groups.N))
  ][
  , train.size := ifelse(n.train.atoms==atoms, "full", "reduced")
  ][]
  gg <- ggplot()+
    tit+
    geom_point(aes(
      percent.error, train.groups.N.fac, color=train.size),
      shape=1,
      data=more.scores)+
    facet_grid(. ~ test.group, labeller=label_both, scales="free")+
    scale_x_continuous(
      "Percent prediction error on CV test group (one dot for each of 10 folds in CV)")+
    scale_y_discrete(
      "Train groups")
  more.stats <- dcast(
    more.scores,
    train.size + n.train.atoms + train.groups + train.groups.N.fac + test.group ~ .,
    list(mean, sd, length),
    value.var="percent.error")
  ggplot()+
    tit+
    theme_bw()+
    theme(
      ##panel.spacing=grid::unit(0, "lines"),
      axis.text.x=element_text(angle=30, hjust=1))+
    geom_point(aes(
      percent.error_mean, n.train.atoms, color=train.size),
      shape=1,
      data=more.stats)+
    geom_segment(aes(
      percent.error_mean+percent.error_sd, n.train.atoms,
      color=train.size,
      xend=percent.error_mean-percent.error_sd, yend=n.train.atoms),
      linewidth=1,
      data=more.stats)+
    facet_grid(
      train.groups ~ test.group,
      scales="free",
      labeller=label_both)+
    scale_x_continuous(
      "Percent prediction error on test set\n(mean +/- SD over 10 folds and 3 random seeds)")
  n.test <- length(unique(score.stats$test.group))
  out.png <- sprintf(
    "data_Classif_figures/%s_error_mean_sd_more_panels.png",
    data.name)
  png(out.png, height=6, width=(n.test+1)*1.5, units="in", res=200)
  print(gg)
  dev.off()
  more.min.max.dt <- dcast(
    more.scores,
    test.group ~ .,
    list(min, max),
    value.var="percent.error"
  )[
  , percent.error_mid := (percent.error_min+percent.error_max)/2
  ][]
  more.join <- more.stats[min.max.dt, on=c("test.group"), nomatch=0L]
  gg <- ggplot()+
    tit+
    theme_bw()+
    theme(
      ##panel.spacing=grid::unit(0, "lines"),
      axis.text.x=element_text(angle=30, hjust=1))+
    geom_point(aes(
      percent.error_mean, train.groups.N.fac, color=train.size),
      shape=1,
      data=more.stats)+
    geom_text(aes(
      percent.error_mean, train.groups.N.fac,
      color=train.size, 
      hjust=ifelse(
        percent.error_mean<percent.error_mid,
        0, 1),
      label=sprintf("%.2f±%.2f", percent.error_mean, percent.error_sd)),
      vjust=-0.5,
      data=more.join)+
    geom_segment(aes(
      percent.error_mean+percent.error_sd, train.groups.N.fac,
      color=train.size, 
      xend=percent.error_mean-percent.error_sd, yend=train.groups.N.fac),
      linewidth=1,
      data=more.stats)+
    facet_wrap(
      "test.group",
      nrow=1,
      scales="free",
      labeller=label_both)+
    scale_y_discrete("train.groups, N")+
    scale_color_manual(values=c(
      full="black",
      reduced="red"))+
    scale_x_continuous(
      "Percent prediction error on test set\n(mean +/- SD over 10 folds and 3 random seeds)")
  out.png <- sprintf(
    "data_Classif_figures/%s_error_mean_sd_more.png",
    data.name)
  max.ticks <- more.stats[, .(ticks=.N), by=test.group][, max(ticks)]
  png(out.png, height=1.5+max.ticks/3, width=(n.test+1)*2.5, units="in", res=200)
  print(gg)
  dev.off()
}
##system("cd /projects/genomic-ml && unpublish_data cv-same-other-paper && publish_data projects/cv-same-other-paper")
system("firefox data_Classif_figures/*more.png")
