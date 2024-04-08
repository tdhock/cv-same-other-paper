library(data.table)
other.name.vec <- c("EMNIST", "FashionMNIST")
data.name.vec <- c(other.name.vec, "MNIST")
data.list <- list()
for(data.i in seq_along(data.name.vec)){
  data.name <- data.name.vec[[data.i]]
  data.csv <- paste0("data_Classif/", data.name, ".csv")
  data.dt <- fread(file=data.csv)
  data.list[[data.i]] <- data.table(data.name, data.dt[,-1])
}
all.dt <- rbindlist(data.list)
dcast(all.dt, y ~ data.name, length)
for(other.name in other.name.vec){
  mnist.other <- all.dt[data.name %in% c(other.name, "MNIST")]
  out.csv <- sprintf("data_Classif/MNIST_%s.csv", other.name)
  fwrite(mnist.other, out.csv)
}
