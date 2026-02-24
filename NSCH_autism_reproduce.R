library(data.table)
options(timeout=9999)
if(!file.exists("data_Classif.zip")){
  download.file("https://zenodo.org/records/18273949/files/SOAK_data_Classif.zip?download=1", "data_Classif.zip")
}

SOAK <- mlr3resampling::ResamplingSameOtherSizesCV$new()
SOAK$param_set$values$folds <- 10
SOAK$param_set$values$sizes <- 0
task.list <- list()
dir.create("data_meta", showWarnings = FALSE)
for(task_id in "NSCH_autism"){
  data.csv <- sprintf("data_Classif/%s.csv", task_id)
  meta.csv <- sprintf("data_meta/%s.csv", task_id)
  if(!file.exists(data.csv)){
    unzip("data_Classif.zip", data.csv)
  }
  task.dt <- fread(
    data.csv,
    colClasses=list(factor="y"),
    check.names=TRUE)#required by mlr3.
  task.obj <- mlr3::TaskClassif$new(
    task_id, task.dt, target="y")
  subset.col <- names(task.dt)[1]
  task.obj$col_roles$subset <- subset.col
  task.obj$col_roles$stratum <- c(subset.col, "y")
  task.obj$col_roles$feature <- setdiff(names(task.dt), task.obj$col_roles$stratum)
  task.list[[task_id]] <- task.obj
  meta_dt <- setnames(
    task.dt[, .(rows=.N), by=subset.col],
    subset.col,
    "test.subset")
  fwrite(meta_dt, meta.csv)
}  
scratch.dir <- "scratch"
dir.create(scratch.dir, showWarnings=FALSE)

(class.learner.list <- list(
  mlr3resampling::LearnerClassifCVGlmnetSave$new(),
  mlr3::LearnerClassifFeatureless$new()))
for(learner.i in seq_along(class.learner.list)){
  class.learner.list[[learner.i]]$predict_type <- "prob"
}

proj.dir <- file.path(scratch.dir, "proj")
unlink(proj.dir, recursive=TRUE)
score_args <- mlr3::msrs(c("classif.auc","classif.acc"))
mlr3resampling::proj_grid(
  proj.dir,
  task.list,
  class.learner.list,
  SOAK,
  score_args=score_args)
mlr3resampling::proj_test(proj.dir)

if(on.cluster){
  120*10/12 # ~100 CPUS for 10 minutes each, 12 hours total
  mlr3resampling::proj_submit(
    proj.dir,
    tasks=100,
    hours=12,
    gigabytes=3)#jid 7290208 on rorqual
  mlr3resampling::proj_results(proj.dir)
  mlr3resampling::proj_todo(proj.dir)
}else if(FALSE){
  reg.dir <- file.path(scratch.dir, "registry")
  unlink(reg.dir, recursive=TRUE)
  (class.bench.grid <- mlr3::benchmark_grid(
    task.list,
    class.learner.list,
    SOAK))
  unlink(reg.dir, recursive=TRUE)
  reg = batchtools::makeExperimentRegistry(
    file.dir = reg.dir,
    seed = 1)
  mlr3batchmark::batchmark(
    class.bench.grid, store_models = TRUE, reg=reg)
  (job.table <- batchtools::getJobTable(reg=reg))
  chunks <- data.frame(job.table, chunk=1)
  batchtools::submitJobs(chunks, resources=list(
    walltime = 1*60*60,#seconds
    memory = 3000,#megabytes per cpu
    ncpus=1,  #>1 for multicore/parallel jobs.
    ntasks=1, #>1 for MPI jobs.
    chunks.as.arrayjobs=TRUE), reg=reg)
  ## rorqual 7290254
  reg <- batchtools::loadRegistry(reg.dir)
  bmr = mlr3batchmark::reduceResultsBatchmark(
    reg = reg, store_backends = FALSE)
  out.RData <- paste0(reg.dir, ".RData")
  save(bmr, file=out.RData)
  score_dt <- mlr3resampling::score(bmr, mlr3::msrs(c("classif.auc", "classif.acc")))
  out.csv <- paste0(reg.dir, "_scores.csv")
  fwrite(score_dt[, .SD, .SDcols=is.atomic], out.csv)
  job.csv <- paste0(reg.dir, "_jobs.csv")
  fwrite(job.table[, .SD, .SDcols=is.atomic], job.csv)
}else{
  ##started on my laptop at 21H30 on 23 Feb 2026.
  mlr3resampling::proj_compute_all(proj.dir, verbose=TRUE)
}
