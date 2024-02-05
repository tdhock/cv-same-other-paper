library(mlbench)
foo=data(package="mlbench")
for(data.name in foo$results[,"Item"]){
  data(list=data.name, package="mlbench")
  d <- get(data.name)
  str(d)
}
