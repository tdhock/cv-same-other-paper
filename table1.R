library(xtable)
library(data.table)
score.atomic <- fread("data_Classif_batchmark_registry.csv")
score.atomic[, `:=`(
  test_subset = test.group,
  train_subsets = train.groups
)][]

Data <- function(data.name, Data){
  data.table(data.name, Data)
}
disp.dt <- rbind(
  Data("CanadaFires_all","CanadaFiresA"),
  Data("CanadaFires_downSampled","CanadaFiresD"),
  Data("MNIST_EMNIST", "IPair_E"),
  Data("MNIST_EMNIST_rot", "IPair_E_rot"),
  Data("MNIST_FashionMNIST","IPair_Fashion"))
meta.raw <- data.table::fread("data-meta.csv")[
  grepl("train|test",group.small.name), `:=`(
    test=ifelse(group.small.name=="test", group.small.N, group.large.N),
    train=ifelse(group.small.name=="train", group.small.N, group.large.N)
  )
][
, `test%` := as.integer(100*test/rows)
][
, subset_type := fcase(
  grepl("MNIST_", data.name), "ImagePair",
  !is.na(test), "train/test",
  default="time/space")
][]
meta.dt <- disp.dt[
  meta.raw, on="data.name"
][is.na(Data), Data := data.name][]
meta.dt[order(subset_type,data.name), .(subset_type, data.name, group.tab)]

table1 <- meta.dt[order(subset_type,data.name), .(
  "Subset type"=subset_type,
  Data,
  Rows=rows,
  Features=features,
  Classes=classes,
  "Class imb."=label.large.N/label.small.N,
  Subsets=n.groups,
  "Subset imb."=group.large.N/group.small.N
)]
xtable1 <- xtable(table1, digits=1)
print(xtable1, type="latex", file="table1.tex")
