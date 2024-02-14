library(data.table)
(data.csv.vec <- Sys.glob("data/*.csv"))
meta.dt.list <- list()
for(data.csv in data.csv.vec){
  data.dt <- fread(data.csv)
  memory.bytes <- object.size(data.dt)
  disk.bytes <- file.size(data.csv)
  (group.tab <- table(data.dt[["predefined.set"]]))
  (label.tab <- table(data.dt[["y"]]))
  (both.tab <- data.dt[, table(y, predefined.set)])
  bytes2kb <- function(x)as.integer(x/1024)
  meta.dt.list[[data.csv]] <- data.table(
    data.name=gsub("data/|.csv","",data.csv),
    memory.kb=bytes2kb(memory.bytes),
    rows=nrow(data.dt),
    as.data.table(as.list(group.tab)),
    `test%`=as.integer(100*group.tab[["test"]]/nrow(data.dt)),
    features=ncol(data.dt)-2L,
    classes=length(label.tab),
    min.rows.set.class=min(both.tab))
}  
(meta.dt <- setkey(
  rbindlist(meta.dt.list),
  memory.kb))
fwrite(meta.dt, "data-meta.csv")
