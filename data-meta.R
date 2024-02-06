library(data.table)
(data.csv.vec <- Sys.glob("data/*.csv"))
meta.dt.list <- list()
for(data.csv in data.csv.vec){
  data.dt <- fread(data.csv)
  memory.bytes <- object.size(data.dt)
  disk.bytes <- file.size(data.csv)
  (group.tab <- table(data.dt[,1]))
  bytes2kb <- function(x)as.integer(x/1024)
  meta.dt.list[[data.csv]] <- data.table(
    data.name=gsub("data/|.csv","",data.csv),
    memory.kb=bytes2kb(memory.bytes),
    disk.kb=bytes2kb(disk.bytes),
    as.data.table(as.list(group.tab)),
    rows=nrow(data.dt),
    features=ncol(data.dt)-2L)
}  
(meta.dt <- setkey(
  rbindlist(meta.dt.list)[, `test%`:=test/rows][],
  memory.kb))
fwrite(meta.dt, "data-meta.csv")
