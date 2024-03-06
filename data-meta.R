library(data.table)
(data.csv.vec <- Sys.glob("data_Classif/*.csv"))
meta.dt.list <- list()
for(data.csv in data.csv.vec){
  data.dt <- fread(data.csv)
  memory.bytes <- object.size(data.dt)
  disk.bytes <- file.size(data.csv)
  group.vec <- data.dt[[1]]
  y.vec <- data.dt[["y"]]
  (group.tab <- sort(table(group.vec)))
  n.groups <- length(group.tab)
  (label.tab <- table(y.vec))
  (both.tab <- table(y.vec, group.vec))
  bytes2kb <- function(x)as.integer(x/1024)
  meta.dt.list[[data.csv]] <- data.table(
    data.name=gsub(".*/|.csv","",data.csv),
    memory.kb=bytes2kb(memory.bytes),
    rows=nrow(data.dt),
    n.groups,
    small_group=names(group.tab)[1],
    small_N=group.tab[1],
    large_group=names(group.tab)[n.groups],
    large_N=group.tab[n.groups],
    features=ncol(data.dt)-2L,
    classes=length(label.tab),
    min.rows=min(both.tab))
}
options(width=150)
(meta.dt <- setkey(
  rbindlist(meta.dt.list),
  memory.kb))
fwrite(meta.dt, "data-meta.csv")
