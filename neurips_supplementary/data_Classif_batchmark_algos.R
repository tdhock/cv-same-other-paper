library(data.table)
if(FALSE){
  remotes::install_github("tdhock/mlr3resampling@cv-ignore-group")
}
(data.csv.vec <- Sys.glob("data_Classif/*.csv"))
task.list <- list()
for(data.csv in data.csv.vec){
  task_id <- gsub("data_Classif/|.csv","",data.csv)
  task.dt <- fread(
    data.csv,
    colClasses=list(factor="y"),
    check.names=TRUE)#required by mlr3.
  task.obj <- mlr3::TaskClassif$new(
    task_id, task.dt, target="y")
  group.col <- names(task.dt)[1]
  task.obj$col_roles$stratum <- c(group.col, "y")
  task.obj$col_roles$feature <- setdiff(names(task.dt), task.obj$col_roles$stratum)
  task.list[[task_id]] <- task.obj
}
scratch.dir <- "/scratch/th798/cv-same-other-paper"
dir.create(scratch.dir, showWarnings=FALSE)
cv <- mlr3::ResamplingCV$new()
subtrain.valid.cv <- mlr3::ResamplingCV$new()
subtrain.valid.cv$param_set$values$folds <- 10
cv$param_set$values$folds <- 10
knn.learner <- mlr3learners::LearnerClassifKKNN$new()
knn.learner$param_set$values$k <- paradox::to_tune(1, 20)
knn.learner$predict_type <- "prob"
knn.tuned = mlr3tuning::auto_tuner(
  tuner = mlr3tuning::TunerGridSearch$new(),
  learner = knn.learner,
  resampling = subtrain.valid.cv)
knn.tuned$id <- "classif.nearest_neighbors"
xgboost.learner <- mlr3learners::LearnerClassifXgboost$new()
xgboost.learner$param_set$values$eta <- paradox::to_tune(0.001, 1, log=TRUE)
xgboost.learner$param_set$values$nrounds <- paradox::to_tune(1, 100)
grid.search.5 <- mlr3tuning::TunerGridSearch$new()
grid.search.5$param_set$values$resolution <- 5
xgboost.learner$predict_type <- "prob"
xgboost.tuned = mlr3tuning::auto_tuner(
  tuner = grid.search.5,
  learner = xgboost.learner,
  resampling = subtrain.valid.cv)
xgboost.tuned$id <- "classif.xgboost"
if(FALSE){
  ## Error : <TaskClassif:age_sex.6> has the following unsupported feature types: factor
  ## https://mlr3book.mlr-org.com/chapters/chapter9/preprocessing.html#factor-encoding
  xgboost.tuned$train(task.list[[1]])
  xgboost.pipeline$train(task.list[[1]])
}
if(FALSE){#ranger is computationally intensive
  ranger.learner <- mlr3learners::LearnerClassifRanger$new()
  ranger.learner$predict_type <- "prob"
  ranger.tuned = mlr3tuning::auto_tuner(
    tuner = grid.search.5,
    learner = mlr3tuningspaces::lts(ranger.learner),
    resampling = subtrain.valid.cv)
  ranger.tuned$id <- "classif.ranger"
}
glmnet.learner <- mlr3learners::LearnerClassifCVGlmnet$new()
rpart.learner <- mlr3::LearnerClassifRpart$new()
fless.learner <- mlr3::LearnerClassifFeatureless$new()
(learner.list <- list(
  xgboost.tuned, knn.tuned,
  glmnet.learner, rpart.learner, fless.learner))

##                   learner_id megabytes       hours
##                       <char>     <num>       <num>
## 1:         classif.cv_glmnet 13622.254 10.27416667
## 2:       classif.featureless  1224.910  0.01250000
## 3: classif.nearest_neighbors 18201.949 48.00694444
## 4:             classif.rpart  9167.699  0.07527778
## 5:           classif.xgboost 14129.508 48.00750000
limit <- function(algorithm, gigabytes, hours){
  data.table(algorithm, gigabytes, hours)
}
limit.dt <- rbind(
  limit("cv_glmnet", 20, 24),
  limit("featureless", 2, 1),
  limit("nearest_neighbors", 30, 4*24),
  limit("rpart", 16, 1),
  limit("xgboost", 20, 4*24))
to.launch <- seq_along(learner.list)
to.launch <- 5
for(learner.i in to.launch){
  learner.obj <- learner.list[[learner.i]]
  learner.obj$predict_type <- "prob"
  algo <- sub("classif.","",learner.obj$id,fixed=TRUE)
  cat(sprintf("%4d / %4d algo=%s\n",learner.i,length(learner.list),algo))
  limit.row <- limit.dt[algo, on="algorithm"]
  (class.bench.grid <- mlr3::benchmark_grid(
    task.list,
    learner.obj,
    cv))
  reg.dir <- file.path(scratch.dir, "data_Classif_batchmark_algos", algo)
  dir.create(dirname(reg.dir),showWarnings=FALSE)
  unlink(reg.dir, recursive=TRUE)
  reg = batchtools::makeExperimentRegistry(
    file.dir = reg.dir,
    seed = 1,
    packages = "mlr3verse"
  )
  mlr3batchmark::batchmark(
    class.bench.grid, store_models = TRUE, reg=reg)
  (job.table <- batchtools::getJobTable(reg=reg))
  chunks <- data.frame(job.table, chunk=1)
  resources <- with(limit.row, list(
    walltime = hours*60*60,#seconds
    memory = gigabytes*1000,#megabytes per cpu
    ncpus=1,  #>1 for multicore/parallel jobs.
    ntasks=1, #>1 for MPI jobs.
    chunks.as.arrayjobs=TRUE))
  str(resources)
  batchtools::submitJobs(chunks, resources=resources, reg=reg)
}

## code to analyze memory usage.
sdt <- slurm::sacct("-j9764165")
sdt.join <- sdt[
  jobs.after, on=.(task=job.id)
][, `:=`(
  learner_id = sapply(algo.pars, "[[", "learner_id"),
  task_id=sapply(prob.pars, "[[", "task_id")
)][]
dcast(
  sdt.join,
  task_id ~ .,
  function(x)max(x,na.rm=TRUE),
  value.var=c("megabytes","hours"))

## old code to get results
reg.dir <- file.path(scratch.dir, "data_Classif_batchmark_algos_registry")
reg=batchtools::loadRegistry(reg.dir)
print(batchtools::getStatus(reg=reg))
batchtools::findExpired(reg=reg)
jobs.after <- batchtools::getJobTable(reg=reg)
(time.out <- jobs.after[, data.table(
  seconds=as.integer(time.running),
  task_id=sapply(prob.pars, "[[", "task_id"),
  learner_id=sapply(algo.pars, "[[", "learner_id"),
  iteration=repl
)][!is.na(seconds)])
fwrite(time.out,"data_Classif_batchmark_algos_registry_time.csv")
dcast(jobs.after[, data.table(done.str=ifelse(is.na(done), "notDone", "done"),error.str=ifelse(is.na(error),"ok","error"),task_id=sapply(prob.pars, "[[", "task_id"), learner_id=sapply(algo.pars, "[[", "learner_id"))], task_id ~ done.str + error.str, length)
table(jobs.after$error)
ids <- jobs.after[!is.na(done) & is.na(error), job.id]
ignore.learner <- function(L){
  L$learner_state$model <- NULL
  L
}
if(FALSE){#https://github.com/mlr-org/mlr3batchmark/pull/29
  remotes::install_github("tdhock/mlr3batchmark@reduceResultsList.fun")
}
bmr = mlr3batchmark::reduceResultsBatchmark(ids, reg = reg, store_backends = FALSE, fun=ignore.learner)
out.RData <- paste0(reg.dir, ".RData")
save(bmr, file=out.RData)

## new code to get results
reg.glob <- file.path(
  scratch.dir, "data_Classif_batchmark_algos", "*")
reg.dir.vec <- Sys.glob(reg.glob)
log.glob <- file.path(
  reg.glob, "logs", "*")
grep.cmd <- paste("grep 'CANCELLED'", log.glob)
system(grep.cmd)
for(reg.dir in reg.dir.vec){
  out.RData <- paste0(reg.dir, ".RData")
  if(!file.exists(out.RData)){
    print(reg.dir)
    reg <- batchtools::loadRegistry(reg.dir)
    print(batchtools::getStatus(reg=reg))
    jobs.after <- batchtools::getJobTable(reg=reg)
    (time.out <- jobs.after[, data.table(
      job.id,
      seconds=as.integer(time.running),
      task_id=sapply(prob.pars, "[[", "task_id"),
      learner_id=sapply(algo.pars, "[[", "learner_id"),
      iteration=repl
    )])
    exp.dt <- batchtools::findExpired(reg=reg)
    print(time.out[exp.dt,on="job.id"])
    ids <- jobs.after[!is.na(done) & is.na(error), job.id]
    ignore.learner <- function(L){
      L$learner_state$model <- NULL
      L
    }
    bmr = mlr3batchmark::reduceResultsBatchmark(ids, reg = reg, store_backends = FALSE, fun=ignore.learner)
    save(bmr, file=out.RData)
  }
}

time.dt.list <- list()
for(reg.dir in reg.dir.vec){
  print(reg.dir)
  reg <- batchtools::loadRegistry(reg.dir)
  print(batchtools::getStatus(reg=reg))
  jobs.after <- batchtools::getJobTable(reg=reg)
  time.dt.list[[reg.dir]] <- jobs.after[, data.table(
    job.id,
    seconds=as.integer(time.running),
    task_id=sapply(prob.pars, "[[", "task_id"),
    learner_id=sapply(algo.pars, "[[", "learner_id"),
    iteration=repl
  )]
}
time.dt <- rbindlist(time.dt.list)
fwrite(time.dt, "data_Classif_batchmark_algos_registries_time.csv")
