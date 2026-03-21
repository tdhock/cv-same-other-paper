torch::torch_tensor(pi)
other.name.vec <- c("EMNIST")
data.name.vec <- c(other.name.vec, "MNIST")
prefix <- "https://rcdata.nau.edu/genomic-ml/cv-same-other-paper/data_Classif/"
data_Classif <- "~/data_Classif"
options(timeout = 600)#seconds
for(data.name in data.name.vec){
  data.csv <- paste0(data.name, ".csv")
  local.csv <- file.path(data_Classif, data.csv)
  if(!file.exists(local.csv)){
    remote.csv <- paste0(prefix, data.csv)
    download.file(remote.csv, local.csv)
  }
}
data.list <- list()
library(data.table)
for(data.name in data.name.vec){
  data.csv <- paste0(data.name, ".csv")
  local.csv <- file.path(data_Classif, data.csv)
  data.list[[data.name]] <- fread(local.csv)
}
n.pixels <- 28
data.list$EMNIST_rot <- data.list$EMNIST[,c(
  1,2,as.integer(matrix(seq(1,n.pixels^2),n.pixels,n.pixels,byrow=TRUE))+2
),with=FALSE]
data.list$EMNIST <- NULL

soak <- mlr3resampling::ResamplingSameOtherSizesCV$new()
soak$param_set$values$folds <- 10
task.list <- list()
other.task.names <- c("EMNIST_rot","FashionMNIST")
other.task.names <- c("EMNIST_rot")
for(other.name in other.task.names){
  ipair.dt.list <- list()
  for(Data in c(other.name,"MNIST")){
    one.dt <- data.list[[Data]][,-1][, y := factor(y)][]
    setnames(one.dt, c("y", paste0("X", names(one.dt)[-1])))
    ipair.dt.list[[Data]] <- data.table(Data, one.dt)
  }
  ipair.dt <- rbindlist(ipair.dt.list, use.names=FALSE)
  ipair.name <- paste0("MNIST_",other.name)
  itask <- mlr3::TaskClassif$new(
    ipair.name, ipair.dt, target="y")
  itask$col_roles$stratum <- "y"
  itask$col_roles$subset <- "Data"
  itask$col_roles$feature <- paste0("X",seq(0,n.pixels^2-1))
  task.list[[ipair.name]] <- itask
}
task.list


measure_list <- mlr3::msrs(c("classif.logloss", "classif.ce"))
n.epochs <- 200
make_torch_learner <- function(id,...){
  po_list <- c(
    list(
      mlr3pipelines::po(
        "select",
        selector = mlr3pipelines::selector_type(c("numeric", "integer"))),
      mlr3torch::PipeOpTorchIngressNumeric$new()),
    list(...),
    list(
      mlr3pipelines::po("nn_head"),
      mlr3pipelines::po(
        "torch_loss",
        mlr3torch::t_loss("cross_entropy")),
      mlr3pipelines::po(
        "torch_optimizer",
        mlr3torch::t_opt("sgd", lr=0.1)),
      mlr3pipelines::po(
        "torch_callbacks",
        mlr3torch::t_clbk("history")),
      mlr3pipelines::po(
        "torch_model_classif",
        batch_size = 100,
        patience=n.epochs,
        measures_valid=measure_list,
        measures_train=measure_list,
        predict_type="prob",
        epochs = paradox::to_tune(upper = n.epochs, internal = TRUE)))
  )
  graph <- Reduce(mlr3pipelines::concat_graphs, po_list)
  glearner <- mlr3::as_learner(graph)
  mlr3::set_validate(glearner, validate = 0.5)
  mlr3tuning::auto_tuner(
    learner = glearner,
    tuner = mlr3tuning::tnr("internal"),
    resampling = mlr3::rsmp("insample"),
    measure = mlr3::msr("internal_valid_score", minimize = TRUE),
    term_evals = 1,
    id=id,
    store_models = TRUE)
}

learner.list <- list(
  mlr3::LearnerClassifFeatureless$new()$configure(id="featureless"),
  make_torch_learner("torch_linear"),
  mlr3learners::LearnerClassifCVGlmnet$new()$configure(
    id="cv_glmnet"),
  make_torch_learner(
    "torch_dense_50",
    mlr3pipelines::po(
      "nn_linear",
      out_features = 50),
    mlr3pipelines::po("nn_relu_1", inplace = TRUE)
  ),
  make_torch_learner(
    "torch_conv",
    mlr3pipelines::po(
      "nn_reshape",
      shape=c(-1,1,n.pixels,n.pixels)),
    mlr3pipelines::po(
      "nn_conv2d_1",
      out_channels = 20,
      kernel_size = 6),
    mlr3pipelines::po("nn_relu_1", inplace = TRUE),
    mlr3pipelines::po(
      "nn_max_pool2d_1",
      kernel_size = 4),
    mlr3pipelines::po("nn_flatten"),
    mlr3pipelines::po(
      "nn_linear",
      out_features = 50),
    mlr3pipelines::po("nn_relu_2", inplace = TRUE)
  )
)
(learner_ord <- sapply(learner.list, "[[", "id"))
for(learner.i in seq_along(learner.list)){
  learner.list[[learner.i]]$predict_type <- "prob"
}

proj.dir <- "scratch/conv_images_restart"
mlr3resampling::proj_grid(
  proj.dir,
  task.list,
  learner.list,
  soak,
  order_jobs=function(DT)DT[, order(
    factor(learner_id, learner_ord))],
  score_args=mlr3::msrs(c("classif.acc")))
  
jid <- mlr3resampling::proj_submit(
  proj.dir,
  tasks=50,
  hours=8,
  gigabytes=1)

slurm::sjob(jid)

jid <- mlr3resampling::proj_submit(
  proj.dir,
  tasks=50,
  hours=8,
  gigabytes=2)

slurm::sjob(jid)
slurm::sacct(paste0("-j",jid))

slurm::sacct("-j7585175")
slurm::sacct_lines("-j7585175", out_file="~/sacct-2GB")

jid <- mlr3resampling::proj_submit(
  proj.dir,
  tasks=50,
  hours=8,
  gigabytes=4)

jid <- mlr3resampling::proj_submit(
  proj.dir,
  tasks=50,
  hours=8,
  gigabytes=8)

slurm::sjob(jid)
pres <- mlr3resampling::proj_results(proj.dir)
fwrite(pres[, .SD, .SDcols=is.atomic], file.path(proj.dir, "results_partial.csv"))
(todo <- mlr3resampling::proj_todo(proj.dir))
gjobs <- fread(file.path(proj.dir, "grid_jobs.csv"))
gjobs[todo]

jid <- mlr3resampling::proj_submit(
  proj.dir,
  tasks=50,
  hours=24,
  gigabytes=8)

jid <- mlr3resampling::proj_submit(
  proj.dir,
  tasks=50,
  hours=24,
  gigabytes=16)

jid <- mlr3resampling::proj_submit(
  proj.dir,
  tasks=50,
  hours=72,
  gigabytes=16)

jid <- mlr3resampling::proj_submit(
  proj.dir,
  tasks=5,
  hours=80,
  gigabytes=16)
