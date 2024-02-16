library(data.table)
canada.fires <- fread(
  "data_Classif_canada_fires/Undersampling_75.csv"
)[, `:=`(
  image=Id_feu,
  label=classe3
)]
canada.fires[, table(Id_image, Id_feu)]
canada.fires[, table(classe2, classe3)]
## here is the data (Undersampling_75) that I think would work the best. Its a undersampling of my original data to have a more equal number of points by class and by image. I kept only one satellite since using images from three created to much variability in the input data.

## I also put my selection of 17 best input features in a .csv file. They give the best results.
fread("data_Classif_canada_fires/select17_vari_undersampling.csv")

##     The column 'Id_feu' identifie each image/fire
##     column 'classe3' is the real class so the output value (its a variation of 'classe2' where I combined some underrepresented classes)
##     For the input features, you can use all the columns that start with 'mean, max, median and min', dnbr, dnbrwoff, rbr, rbrwoff, rdnbr, rdnbrwoff,NDVI, r,g,b, nir,IB, GCC, VARI
names(canada.fires)
keep.cols <- c(
  "image", "label",
  grep("mean|max|median|min", names(canada.fires), value=TRUE),
  "dnbr", "dnbrwoff", "rbr", "rbrwoff", "rdnbr", "rdnbrwoff",
  #"NDVI",
  "r","g","b", "nir","IB")#, "GCC", "VARI")
out.dt <- canada.fires[, keep.cols, with=FALSE]
na.counts <- colSums(is.na(out.dt))
na.counts[na.counts>0]
fwrite(out.dt, "data_Classif/CanadaFires.csv")
