torch::torch_tensor(pi)
other.name.vec <- c("EMNIST", "FashionMNIST")
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
  ),
  make_torch_learner("torch_linear"),
  mlr3::LearnerClassifFeatureless$new()$configure(id="featureless"),
  ## make_torch_learner(
  ##   "torch_conv_small",
  ##   mlr3pipelines::po(
  ##     "nn_reshape",
  ##     shape=c(-1,1,n.pixels,n.pixels)),
  ##   mlr3pipelines::po(
  ##     "nn_conv2d_1",
  ##     out_channels = 40,
  ##     kernel_size = 5),
  ##   mlr3pipelines::po("nn_relu_1", inplace = TRUE),
  ##   mlr3pipelines::po(
  ##     "nn_max_pool2d_1",
  ##     kernel_size = 2),
  ##   mlr3pipelines::po("nn_flatten"),
  ##   mlr3pipelines::po(
  ##     "nn_linear",
  ##     out_features = 25)
  ## ),
  ## make_torch_learner(
  ##   "torch_conv_2",
  ##   mlr3pipelines::po(
  ##     "nn_reshape",
  ##     shape=c(-1,1,n.pixels,n.pixels)),
  ##   mlr3pipelines::po(
  ##     "nn_conv2d_1",
  ##     out_channels = 40,
  ##     kernel_size = 5),
  ##   mlr3pipelines::po("nn_relu_1", inplace = TRUE),
  ##   mlr3pipelines::po(
  ##     "nn_max_pool2d_1",
  ##     kernel_size = 2),
  ##   mlr3pipelines::po(
  ##     "nn_conv2d_2",
  ##     out_channels = 20,
  ##     kernel_size = 3),
  ##   mlr3pipelines::po("nn_relu_2", inplace = TRUE),
  ##   mlr3pipelines::po(
  ##     "nn_max_pool2d_2",
  ##     kernel_size = 2),
  ##   mlr3pipelines::po("nn_flatten"),
  ##   mlr3pipelines::po(
  ##     "nn_linear",
  ##     out_features = 50)
  ## )
  ## make_torch_learner(
  ##   "torch_dense_25",
  ##   mlr3pipelines::po(
  ##     "nn_linear",
  ##     out_features = 25)
  ## ),
  make_torch_learner(
    "torch_dense_50",
    mlr3pipelines::po(
      "nn_linear",
      out_features = 50),
    mlr3pipelines::po("nn_relu_1", inplace = TRUE)
  )
  ## make_torch_learner(
  ##   "torch_dense_50_20",
  ##   mlr3pipelines::po(
  ##     "nn_linear_1",
  ##     out_features = 50),
  ##   mlr3pipelines::po(
  ##     "nn_linear_2",
  ##     out_features = 20)
  ## ),
  ## make_torch_learner(
  ##   "torch_dense_100",
  ##   mlr3pipelines::po(
  ##     "nn_linear",
  ##     out_features = 100)
  ## ),
)
  
for(s_param in c("min")){
  learner.list[[s_param]] <- mlr3learners::LearnerClassifCVGlmnet$new()$configure(
    s=paste0("lambda.",s_param),
    id=paste0("cv_glmnet_",s_param))
}
(bench.grid <- mlr3::benchmark_grid(
  task.list,
  learner.list,
  soak))

reg.dir <- "conv_images_10fold"
cache.RData <- paste0(reg.dir,".RData")
if(file.exists(cache.RData)){
  load(cache.RData)
}else{#code below should be run interactively.
  if(on.cluster){
    unlink(reg.dir, recursive=TRUE)
    reg = batchtools::makeExperimentRegistry(
      file.dir = reg.dir,
      seed = 1,
      packages = c("mlr3learners","mlr3torch","glmnet","mlr3resampling")
    )
    mlr3batchmark::batchmark(
      bench.grid, store_models = TRUE, reg=reg)
    job.table <- batchtools::getJobTable(reg=reg)
    chunks <- data.frame(job.table, chunk=1)
    batchtools::submitJobs(chunks, resources=list(
      walltime = 60*60*24,#seconds
      memory = 8000,#megabytes per cpu
      ncpus=1,  #>1 for multicore/parallel jobs.
      ntasks=1, #>1 for MPI jobs.
      chunks.as.arrayjobs=TRUE), reg=reg)
    batchtools::getStatus(reg=reg)
    reg <- batchtools::loadRegistry(reg.dir)
    jobs.after <- batchtools::getJobTable(reg=reg)
    extra.cols <- c(algo.pars="learner_id", prob.pars="task_id")
    for(list_col_name in names(extra.cols)){
      new_col_name <- extra.cols[[list_col_name]]
      value <- sapply(jobs.after[[list_col_name]], "[[", new_col_name)
      set(jobs.after, j=new_col_name, value=value)
    }
    table(jobs.after$error)
    exp.ids <- batchtools::findExpired(reg=reg)
    ids <- jobs.after[is.na(error), job.id]
    ok.ids <- setdiff(ids,exp.ids$job.id)
    bench.result <- mlr3batchmark::reduceResultsBatchmark(ok.ids, fun=keep_history, reg = reg)
  }else{
    ## In the code below, we declare a multisession future plan to
    ## compute each benchmark iteration in parallel on this computer
    ## (data set, learning algorithm, cross-validation fold). For a
    ## few dozen iterations, using the multisession backend is
    ## probably sufficient (I have 12 CPUs on my work PC).
    if(require(future))plan("multisession")
    bench.result <- mlr3::benchmark(bench.grid, store_models = TRUE)
  }
  save(bench.result, file=cache.RData)
}
score_dt <- mlr3resampling::score(bench.result)[
    , percent_error := 100*classif.ce
][]

keep_history <- function(x){
  learners <- x$learner_state$model$marshaled$tuning_instance$archive$learners
  x$learner_state$model <- if(is.function(learners)){
    L <- learners(1)[[1]]
    x$history <- L$model$torch_model_classif$model$callbacks$history
  }
  x
}
(reg.dirs <- c(
  "conv_images_convnets",
  "conv_images_long",
  "conv_images_nnets"))
library(data.table)
score.list <- list()
for(reg.dir in reg.dirs){
  if(! reg.dir %in% names(score.list)){
    reg <- batchtools::loadRegistry(reg.dir)
    jobs.after <- batchtools::getJobTable(reg=reg)
    extra.cols <- c(algo.pars="learner_id", prob.pars="task_id")
    for(list_col_name in names(extra.cols)){
      new_col_name <- extra.cols[[list_col_name]]
      value <- sapply(jobs.after[[list_col_name]], "[[", new_col_name)
      set(jobs.after, j=new_col_name, value=value)
    }
    print(reg.dir)
    print(batchtools::getStatus(reg=reg))
    print(table(jobs.after$error))
    exp.ids <- batchtools::findExpired(reg=reg)
    ids <- jobs.after[is.na(error) & !is.na(done), job.id]
    ok.ids <- setdiff(ids,exp.ids$job.id)
    bench.result <- mlr3batchmark::reduceResultsBatchmark(ok.ids, fun=keep_history, reg = reg)
    score.list[[reg.dir]] <- mlr3resampling::score(bench.result)[
    , percent_error := 100*classif.ce
    ][]
  }
}
score_dt <- rbindlist(score.list)


test_dt <- score_dt[, .(task_id, test.subset, train.subsets, test.fold, algorithm, percent_error)]
fwrite(test_dt, "conv_images_10fold_test.csv")
history_dt <- score_dt[
, learner[[1]]$model
, by=.(task_id, test.subset, train.subsets, test.fold, algorithm)]
fwrite(history_dt, "conv_images_10fold_history.csv")

ggplot()+
  geom_point(aes(
    percent_error, algorithm),
    shape=1,
    data=score_dt)+
  facet_grid(train.subsets ~ task_id + test.subset, labeller=label_both)

(score_stats <- dcast(
  score_dt,
  algorithm + task_id + test.subset + train.subsets ~ .,
  list(mean, sd),
  value.var="percent_error"))
ggplot()+
  geom_point(aes(
    percent_error_mean, algorithm),
    shape=1,
    data=score_stats)+
  geom_segment(aes(
    percent_error_mean+percent_error_sd, algorithm,
    xend=percent_error_mean-percent_error_sd, yend=algorithm),
    data=score_stats)+
  facet_grid(train.subsets ~ task_id + test.subset, labeller=label_both)

