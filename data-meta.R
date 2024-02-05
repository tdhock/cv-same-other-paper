library(data.table)
(data.csv.vec <- Sys.glob("data/*.csv"))
meta.dt.list <- list()
for(data.csv in data.csv.vec){
  data.dt <- fread(data.csv)
  (group.tab <- table(data.dt[,1]))
  meta.dt.list[[data.csv]] <- data.table(
    data.name=gsub("data/|.csv","",data.csv),
    groups=length(group.tab),
    features=ncol(data.dt)-2L,
    rows=nrow(data.dt))
}  
(meta.dt <- rbindlist(meta.dt.list))
fwrite(meta.dt, "data-meta.csv")
