library(data.table)
N <- 2400
abs.x <- 3*pi
set.seed(2)
grid.dt <- data.table(
  x=seq(-abs.x,abs.x, l=201),
  y=0)
x.vec <- runif(N, -abs.x, abs.x)
standard.deviation.vec <- c(
  easy=0.1,
  hard=1.7)
reg.data.list <- list()
grid.signal.dt.list <- list()
sim_fun <- sin
for(difficulty in names(standard.deviation.vec)){
  standard.deviation <- standard.deviation.vec[[difficulty]]
  signal.vec <- sim_fun(x.vec)
  y <- signal.vec+rnorm(N,sd=standard.deviation)
  task.dt <- data.table(x=x.vec, y)
  reg.data.list[[difficulty]] <- data.table(difficulty, task.dt)
  grid.signal.dt.list[[difficulty]] <- data.table(
    difficulty,
    algorithm="ideal",
    x=grid.dt$x,
    y=sim_fun(grid.dt$x))
}
(reg.data <- rbindlist(reg.data.list))
(grid.signal.dt <- rbindlist(grid.signal.dt.list))
algo.colors <- c(
  featureless="blue",
  rpart="red",
  ideal="black")
if(require(ggplot2)){
  ggplot()+
    theme_bw()+
    geom_point(aes(
      x, y),
      fill="white",
      color="grey",
      data=reg.data)+
    geom_line(aes(
      x, y, color=algorithm),
      linewidth=2,
      data=grid.signal.dt)+
    scale_color_manual(values=algo.colors)+
    facet_grid(. ~ difficulty, labeller=label_both)
}
set.seed(1)
sim.meta.list <- list(
  different=rbind(
    reg.data[difficulty=="easy"][sample(.N, 400)],
    reg.data[difficulty=="hard"][sample(.N, 200)]
  )[, .(x,y,Subset=ifelse(difficulty=="easy", "large", "small"))],
  iid_easy=reg.data[
    difficulty=="easy"
  ][sample(.N, 120)][
  , Subset := rep(c("large","large","small"), l=.N)
  ][, .(x,y,Subset)])
d_task_list <- list()
gg_list <- list()
for(sim.name in names(sim.meta.list)){
  sim.i.dt <- sim.meta.list[[sim.name]]
  sub_task <- mlr3::TaskRegr$new(
    sim.name, sim.i.dt, target="y")
  sub_task$col_roles$subset <- "Subset"
  sub_task$col_roles$feature <- "x"
  d_task_list[[sim.name]] <- sub_task
  if(require("ggplot2")){
    gg_list[[sim.name]] <- ggplot()+
      ggtitle(paste("Simulation:", sim.name))+
      geom_point(aes(
        x, y),
        color="black",
        fill="white",
        data=sim.i.dt)+
      geom_line(aes(
        x, y, color=algorithm),
        data=grid.signal.dt)+
      scale_color_manual(values=algo.colors)+
      facet_grid(Subset~., labeller=label_both)
  }
}
gg_list

if(require(future))plan("multisession")
if(require(lgr))get_logger("mlr3")$set_threshold("warn")

SOAKED_seeds <- mlr3resampling::ResamplingSameOtherSizesCV$new()
SOAKED_seeds$param_set$values$sizes <- 0
SOAKED_seeds$param_set$values$folds <- 10
SOAKED_seeds$param_set$values$seeds <- 100
SOAKED_seeds$param_set$values$seeds <- 2

(reg.bench.grid <- mlr3::benchmark_grid(
  d_task_list,
  mlr3::lrn("regr.rpart"),
  SOAKED_seeds))
(reg.bench.result <- mlr3::benchmark(reg.bench.grid))

score_dt <- mlr3resampling::score(
  reg.bench.result, mlr3::msr("regr.rmse"))
fwrite(score_dt[, .SD, .SDcols=is.atomic], file="figure-SOAKED-seeds-data.csv")
