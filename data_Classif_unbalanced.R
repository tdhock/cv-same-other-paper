library(data.table)
MNIST_dt <- fread("~/projects/cv-same-other-paper/data_Classif/MNIST.csv")
data.table(
  name=names(MNIST_dt),
  first_row=unlist(MNIST_dt[1]),
  last_row=unlist(MNIST_dt[.N]))
```
(subset_dt <- fread("~/projects/cv-same-other-paper/data_Classif_unbalanced/MNIST.csv"))
```
subset_dt[, identical(
  seed1_prop0.1=="balanced",
  seed1_prop0.05=="balanced")]
subset_dt[, identical(
  seed2_prop0.005=="balanced",
  seed2_prop0.001=="balanced")]
```
subset_dt[, all(which(
  seed1_prop0.05=="unbalanced"
) %in% which(
  seed1_prop0.1=="unbalanced"
))]
subset_dt[, all(which(
  seed2_prop0.001=="unbalanced"
) %in% which(
  seed2_prop0.005=="unbalanced"
))]
```
(SOAK <- mlr3resampling::ResamplingSameOtherSizesCV$new())
task.list <- list()
task_dt <- data.table(subset_dt, MNIST_dt)[, odd := factor(y %% 2)]
feature.names <- grep("^[0-9]+$", names(task_dt), value=TRUE)
for(subset.name in c("seed1_prop0.1","seed1_prop0.01")){
  subset_vec <- task_dt[[subset.name]]
  itask <- mlr3::TaskClassif$new(
    subset.name, task_dt[subset_vec != ""], target="odd")
  itask$col_roles$stratum <- "odd"
  itask$col_roles$subset <- subset.name
  itask$col_roles$feature <- feature.names
  task.list[[subset.name]] <- itask
}
task.list
ROC_curve <- function(pred_tensor, label_tensor){
  is_positive = label_tensor == 1
  is_negative = label_tensor != 1
  fn_diff = torch::torch_where(is_positive, -1, 0)
  fp_diff = torch::torch_where(is_positive, 0, 1)
  thresh_tensor = -pred_tensor$flatten()
  sorted_indices = torch::torch_argsort(thresh_tensor)
  fp_denom = torch::torch_sum(is_negative) #or 1 for AUM based on count instead of rate
  fn_denom = torch::torch_sum(is_positive) #or 1 for AUM based on count instead of rate
  sorted_fp_cum = fp_diff[sorted_indices]$cumsum(dim=1)/fp_denom
  sorted_fn_cum = -fn_diff[sorted_indices]$flip(1)$cumsum(dim=1)$flip(1)/fn_denom
  sorted_thresh = thresh_tensor[sorted_indices]
  sorted_is_diff = sorted_thresh$diff() != 0
  sorted_fp_end = torch::torch_cat(c(sorted_is_diff, torch::torch_tensor(TRUE)))
  sorted_fn_end = torch::torch_cat(c(torch::torch_tensor(TRUE), sorted_is_diff))
  uniq_thresh = sorted_thresh[sorted_fp_end]
  uniq_fp_after = sorted_fp_cum[sorted_fp_end]
  uniq_fn_before = sorted_fn_cum[sorted_fn_end]
  FPR = torch::torch_cat(c(torch::torch_tensor(0.0), uniq_fp_after))
  FNR = torch::torch_cat(c(uniq_fn_before, torch::torch_tensor(0.0)))
  list(
    FPR=FPR,
    FNR=FNR,
    TPR=1 - FNR,
    "min(FPR,FNR)"=torch::torch_minimum(FPR, FNR),
    min_constant=torch::torch_cat(c(torch::torch_tensor(-Inf), uniq_thresh)),
    max_constant=torch::torch_cat(c(uniq_thresh, torch::torch_tensor(Inf))))
}
Proposed_AUM <- function(pred_tensor, label_tensor){
  roc = ROC_curve(pred_tensor, label_tensor)
  min_FPR_FNR = roc[["min(FPR,FNR)"]][2:-2]
  constant_diff = roc$min_constant[2:N]$diff()
  torch::torch_sum(min_FPR_FNR * constant_diff)
}
nn_AUM_loss <- torch::nn_module(
  "nn_AUM_loss",
  inherit = torch::nn_mse_loss,
  initialize = function() {
    super$initialize()
  },
  forward = function(input, target) {
    Proposed_AUM(input, target)
  }
)
afun <- nn_AUM_loss()
afun(torch::torch_tensor(c(5,-5)), torch::torch_tensor(c(0,1)))
afun(torch::torch_tensor(c(5,-5)), torch::torch_tensor(c(1,0)))
mlr3torch_loss_list <- list(
  AUM=mlr3torch::TorchLoss$new(
    torch_loss = nn_AUM_loss,
    task_types = "classif"),
  cross_entropy=mlr3torch::t_loss("cross_entropy"))
measure_list <- mlr3::msrs(c("classif.auc", "classif.acc"))
n.epochs <- 200
learner_list <- list(
  mlr3learners::LearnerClassifCVGlmnet$new(),
  mlr3::LearnerClassifFeatureless$new())
for(train_loss in names(mlr3torch_loss_list)){
  mt_loss <- mlr3torch_loss_list[[train_loss]]
  po_list_linear_ce <- list(
    mlr3pipelines::po(
      "select",
      selector = mlr3pipelines::selector_type(c("numeric", "integer"))),
    mlr3torch::PipeOpTorchIngressNumeric$new(),
    mlr3pipelines::po("nn_head"),
    mlr3pipelines::po(
      "torch_loss",
      mt_loss),
    mlr3pipelines::po(
      "torch_optimizer",
      mlr3torch::t_opt("sgd", lr=0.1)),
    mlr3pipelines::po(
      "torch_callbacks",
      mlr3torch::t_clbk("history")),
    mlr3pipelines::po(
      "torch_model_classif",
      batch_size = 500,
      patience=n.epochs,
      measures_valid=measure_list,
      measures_train=measure_list,
      predict_type="prob",
      epochs = paradox::to_tune(upper = n.epochs, internal = TRUE)))
  (graph_linear_ce <- Reduce(mlr3pipelines::concat_graphs, po_list_linear_ce))
  (glearner_linear_ce <- mlr3::as_learner(graph_linear_ce))
  mlr3::set_validate(glearner_linear_ce, validate = 0.5)
  learner_list[[train_loss]] <- mlr3tuning::auto_tuner(
    learner = glearner_linear_ce,
    tuner = mlr3tuning::tnr("internal"),
    resampling = mlr3::rsmp("insample"),
    measure = mlr3::msr("internal_valid_score", minimize = FALSE),
    term_evals = 1,
    id=train_loss,
    store_models = TRUE)
}
(bench.grid <- mlr3::benchmark_grid(
  task.list,
  learner_list,
  SOAK))

cache.RData <- "2025-03-21-mlr3torch-aum.RData"
if(file.exists(cache.RData)){
  load(cache.RData)
}else{#code below should be run interactively.
  if(on.cluster){
    reg.dir <- "2024-10-30-mlr3torch-benchmark"
    unlink(reg.dir, recursive=TRUE)
    reg = batchtools::makeExperimentRegistry(
      file.dir = reg.dir,
      seed = 1,
      packages = "mlr3verse"
    )
    mlr3batchmark::batchmark(
      bench.grid, store_models = TRUE, reg=reg)
    job.table <- batchtools::getJobTable(reg=reg)
    chunks <- data.frame(job.table, chunk=1)
    batchtools::submitJobs(chunks, resources=list(
      walltime = 60*60,#seconds
      memory = 2000,#megabytes per cpu
      ncpus=1,  #>1 for multicore/parallel jobs.
      ntasks=1, #>1 for MPI jobs.
      chunks.as.arrayjobs=TRUE), reg=reg)
    batchtools::getStatus(reg=reg)
    jobs.after <- batchtools::getJobTable(reg=reg)
    table(jobs.after$error)
    ids <- jobs.after[is.na(error), job.id]
    bench.result <- mlr3batchmark::reduceResultsBatchmark(ids, reg = reg)
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
```


