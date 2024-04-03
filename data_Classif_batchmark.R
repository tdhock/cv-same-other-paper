library(data.table)
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
  task.obj$col_roles$group <- group.col
  task.obj$col_roles$stratum <- c(group.col, "y")
  task.obj$col_roles$feature <- setdiff(names(task.dt), task.obj$col_roles$stratum)
  task.list[[task_id]] <- task.obj
}  
scratch.dir <- "/scratch/th798/cv-same-other-paper"
dir.create(scratch.dir, showWarnings=FALSE)

if(do.sizes){
  reg.dir <- file.path(scratch.dir, "data_Classif_batchmark_sizes_registry")
  same_other_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  same_other_cv$param_set$values$folds <- 10
  (class.learner.list <- list(
    mlr3learners::LearnerClassifCVGlmnet$new()))
}else{
  reg.dir <- file.path(scratch.dir, "data_Classif_batchmark_registry")
  same_other_cv <- mlr3resampling::ResamplingSameOtherCV$new()
  same_other_cv$param_set$values$folds <- 10
  (class.learner.list <- list(
    mlr3learners::LearnerClassifCVGlmnet$new(),
    mlr3::LearnerClassifFeatureless$new()))
}
for(learner.i in seq_along(class.learner.list)){
  class.learner.list[[learner.i]]$predict_type <- "prob"
}

(class.bench.grid <- mlr3::benchmark_grid(
  task.list,
  class.learner.list,
  same_other_cv))
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
batchtools::submitJobs(chunks, resources=list(
  walltime = 24*60*60,#seconds
  memory = 64000,#megabytes per cpu
  ncpus=1,  #>1 for multicore/parallel jobs.
  ntasks=1, #>1 for MPI jobs.
  chunks.as.arrayjobs=TRUE), reg=reg)

reg.dir <- "data_Classif_batchmark_registry"
reg=batchtools::loadRegistry(reg.dir)
print(batchtools::getStatus(reg=reg))
jobs.after <- batchtools::getJobTable(reg=reg)
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


result <- readRDS("~/genomic-ml/projects/cv-same-other-paper/data_Classif_batchmark_registry/results/1.rds")

orig.tabs = batchtools::getJobTable(ids, reg = reg)[, c(
  "job.id", "job.name", "repl", "prob.pars", "algo.pars"), with = FALSE]
unnest.tabs = mlr3misc::unnest(orig.tabs, c("prob.pars", "algo.pars"))
tabs = split(unnest.tabs, by = "job.name")
bmr = mlr3::BenchmarkResult$new()

result.list <- list()
for (tab.i in seq_along(tabs)) {
  cat(sprintf("%4d / %4d\n", tab.i, length(tabs)))
  tab <- tabs[[tab.i]]
  result.list[[tab.i]] <- batchtools::reduceResultsList(tab$job.id, reg = reg, fun=ignore.learner)
}
