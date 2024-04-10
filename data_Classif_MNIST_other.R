library(data.table)
library(ggplot2)
other.name.vec <- c("EMNIST", "FashionMNIST")
data.name.vec <- c(other.name.vec, "MNIST")
data.list <- list()
for(data.i in seq_along(data.name.vec)){
  data.name <- data.name.vec[[data.i]]
  data.csv <- paste0("data_Classif/", data.name, ".csv")
  data.dt <- fread(file=data.csv)
  data.list[[data.name]] <- data.table(data.name, data.dt[,-1])
}
n.pixels <- 28
rlong <- function(DT){
  nc::capture_melt_single(
    DT,
    col.int="[0-9]+", as.integer
  )[, `:=`(
    col=col.int %% n.pixels,
    row=col.int %/% n.pixels,
    "data set"=paste0("\n", data.name)
  )][]
}
EMlong <- rlong(
  data.table(data.list$EMNIST)[, image := .I]
)
EMwide <- dcast(
  EMlong[, new.col := col*n.pixels+row],
  image + data.name + y ~ new.col,
  value.var="value")
data.list$EMNIST_rot <- EMwide[,-1][, data.name := "EMNIST_rot"][]
all.dt <- rbindlist(data.list)
dcast(all.dt, y ~ data.name, length)

for(n.images in 1:2){
  some.dt <- all.dt[
  , .SD[1:n.images][, image := .I]
  , by=.(data.name, y)]
  some.long <- rlong(some.dt)[, label := y]
  show.dt <- some.long[image==1 & y==4 & data.name=="MNIST"]
  show.dt <- some.long
  gg <- ggplot()+
    geom_tile(aes(
      col, row, fill=value),
      data=show.dt)+
    coord_equal()+
    facet_grid(
      if(n.images==1){
        `data set` ~ label
      }else{
        `data set` + image ~ label
      }, labeller=label_both)+
    scale_y_reverse()+
    scale_fill_gradient(low="white",high="black")+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))
  out.png <- sprintf("data_Classif_MNIST_other_%d.png", n.images)
  png(out.png, width=11, height=4.2*n.images, units="in", res=300)
  print(gg)
  dev.off()
}

other.out.vec <- setdiff(names(data.list),"MNIST")
for(other.name in other.out.vec){
  mnist.other <- all.dt[data.name %in% c(other.name, "MNIST")]
  some.dt <- mnist.other[
  , .SD[1][, image := .I]
  , by=.(data.name, y)]
  show.dt <- rlong(some.dt)[, label := y]
  gg <- ggplot()+
    geom_tile(aes(
      col, row, fill=value),
      data=show.dt)+
    coord_equal()+
    facet_grid(`data set` ~ label, labeller=label_both)+
    scale_y_reverse()+
    scale_fill_gradient(low="white",high="black")+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))
  out.png <- sprintf("data_Classif_MNIST_other_%s.png", other.name)
  png(out.png, width=11, height=2.5, units="in", res=300)
  print(gg)
  dev.off()
}
system("cd /projects/genomic-ml && unpublish_data cv-same-other-paper && publish_data projects/cv-same-other-paper")

for(other.name in other.out.vec){
  mnist.other <- all.dt[data.name %in% c(other.name, "MNIST")]
  out.csv <- sprintf("data_Classif/MNIST_%s.csv", other.name)
  print(out.csv)
  fwrite(mnist.other, out.csv)
}
