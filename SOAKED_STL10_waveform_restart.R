library(data.table)
options(timeout=9999)
if(!file.exists("data_Classif.zip")){
  download.file("https://zenodo.org/records/18273949/files/SOAK_data_Classif.zip?download=1", "data_Classif.zip")
}

SOAK <- mlr3resampling::ResamplingSameOtherSizesCV$new()
SOAK$param_set$values$folds <- 10
SOAK$param_set$values$sizes <- 0
task.list <- list()
task_order <- c("waveform","STL10")
for(task_id in task_order){
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
scratch.dir <- "scratch"
dir.create(scratch.dir, showWarnings=FALSE)

(class.learner.list <- list(
  mlr3resampling::LearnerClassifCVGlmnetSave$new(),
  mlr3::LearnerClassifFeatureless$new()))
for(learner.i in seq_along(class.learner.list)){
  class.learner.list[[learner.i]]$predict_type <- "prob"
}

proj.dir <- file.path(scratch.dir, "SOAKED_STL10_waveform_restart")
unlink(proj.dir, recursive=TRUE)
mlr3resampling::proj_grid(
  proj.dir,
  task.list,
  class.learner.list,
  SOAK,
  order_jobs=function(DT)DT[,order(
    factor(task_id,task_order),
    factor(learner_id,paste0("classif.",c("featureless","cv_glmnet"))))],
  score_args=mlr3::msrs(c("classif.acc")))

jid <- mlr3resampling::proj_submit(
  proj.dir,
  tasks=50,
  hours=1,
  gigabytes=3)

slurm::sjob(jid)
mlr3resampling::proj_results(proj.dir)
mlr3resampling::proj_todo(proj.dir)

jid <- mlr3resampling::proj_submit(
  proj.dir,
  tasks=50,
  hours=6,
  gigabytes=30)

slurm::sjob(jid)
mlr3resampling::proj_results(proj.dir)
mlr3resampling::proj_todo(proj.dir)

jid <- mlr3resampling::proj_submit(
  proj.dir,
  tasks=30,
  hours=6,
  gigabytes=30)

file.copy(file.path(proj.dir, "results.csv"), "SOAKED_STL10_waveform_restart.csv")
