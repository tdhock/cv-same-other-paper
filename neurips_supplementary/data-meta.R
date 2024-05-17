library(data.table)
(data.csv.vec <- Sys.glob("data_Classif/*.csv"))
meta.dt.list <- list()
for(data.csv.i in seq_along(data.csv.vec)){
  data.csv <- data.csv.vec[[data.csv.i]]
  cat(sprintf("%4d / %4d %s\n", data.csv.i, length(data.csv.vec), data.csv))
  data.dt <- fread(data.csv)
  memory.bytes <- object.size(data.dt)
  disk.bytes <- file.size(data.csv)
  group.vec <- data.dt[[1]]
  y.vec <- data.dt[["y"]]
  (group.tab <- sort(table(group.vec)))
  n.groups <- length(group.tab)
  (label.tab <- sort(table(y.vec)))
  (both.tab <- table(y.vec, group.vec))
  bytes2kb <- function(x)as.integer(x/1024)
  tab2dt <- function(tab){
    data.table(
      name=names(tab)[1],
      N=tab[1])
  }
  small_large <- function(tab){
    data.table(
      small=tab2dt(tab[1]),
      large=tab2dt(tab[length(tab)]))
  }
  meta.dt.list[[data.csv]] <- print(data.table(
    data.name=gsub(".*/|.csv","",data.csv),
    memory.kb=bytes2kb(memory.bytes),
    rows=nrow(data.dt),
    n.groups,
    group.tab=paste(paste0(names(group.tab),"=",group.tab), collapse=";"),
    group=small_large(group.tab),
    label=small_large(label.tab),
    features=ncol(data.dt)-2L,
    classes=length(label.tab),
    min.rows=min(both.tab)))
}
options(width=150)
(meta.dt <- setkey(
  rbindlist(meta.dt.list),
  memory.kb))
fwrite(meta.dt, "data-meta.csv")
