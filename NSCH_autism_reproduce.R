library(data.table)
options(timeout=9999)
if(!file.exists("data_Classif.zip")){
  download.file("https://zenodo.org/records/18273949/files/SOAK_data_Classif.zip?download=1", "data_Classif.zip")
}

SOAK <- mlr3resampling::ResamplingSameOtherSizesCV$new()
SOAK$param_set$values$folds <- 10
SOAK$param_set$values$sizes <- 0
task.list <- list()
for(task_id in "NSCH_autism"){
  data.csv <- sprintf("data_Classif/%s.csv", task_id)
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
}  
scratch.dir <- "/scratch/th798/cv-same-other-paper"
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
  120*10/12 # 100 CPUS for 10 minutes each, 12 hours total
  mlr3resampling::proj_submit(
    proj.dir,
    tasks=50,
    hours=12,
    gigabytes=3)
  ##started at 21H30
}else{
  mlr3resampling::proj_compute_all(proj.dir, verbose=TRUE)
}
