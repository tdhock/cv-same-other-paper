library(data.table)
fread("data_Classif_canada_fires/select17_vari_undersampling.csv")
fires.csv.list <- list(
  all="All_data_SkysatImages_Gaby.csv",
  downSampled="Undersampling_75_classe2.csv",
  missing="Undersampling_75.csv")
for(fires.name in names(fires.csv.list)){
  path.csv <- file.path(
    "data_Classif_canada_fires",
    fires.csv.list[[fires.name]])
  print(fires.name)
  print(path.csv)
  canada.fires <- fread(
    path.csv
  )[
  , image := Id_feu
  ]
  print(names(canada.fires))
  summary(canada.fires)
  y.var <- if("classe3" %in% names(canada.fires)){
    "classe3"
  }else{
    "classe2"
  }
  set(
    canada.fires,
    j="y",
    value=ifelse(
      canada.fires[[y.var]] %in% c("charred","scorched"),
      "burned",
      "no burn"))
  canada.fires[, table(Id_image, Id_feu)]
  print(canada.fires[, table(classe2)])
  ##print(canada.fires[, table(geometry)])
  ##print(canada.fires[, table(Id_image)])
  ##canada.fires[, table(classe2, classe3)]
  table(canada.fires[[5]], canada.fires[[10]])#both id_feu
  ## here is the data (Undersampling_75) that I think would work the best. Its a undersampling of my original data to have a more equal number of points by class and by image. I kept only one satellite since using images from three created to much variability in the input data.
## I also put my selection of 17 best input features in a .csv file. They give the best results.
  ##     The column 'Id_feu' identifie each image/fire
  ##     column 'classe3' is the real class so the output value (its a variation of 'classe2' where I combined some underrepresented classes)
  ##     For the input features, you can use all the columns that start with 'mean, max, median and min', dnbr, dnbrwoff, rbr, rbrwoff, rdnbr, rdnbrwoff,NDVI, r,g,b, nir,IB, GCC, VARI
  keep.cols <- c(
    "image", "y",
    grep("mean|max|median|min", names(canada.fires), value=TRUE),
    "dnbr", "dnbrwoff", "rbr", "rbrwoff", "rdnbr", "rdnbrwoff",
    "NDVI",
    "r","g","b", "nir","IB", "GCC", "VARI")
  out.dt <- canada.fires[, keep.cols, with=FALSE]
  print(out.dt[, table(image,y)])
  na.counts <- colSums(is.na(out.dt))
  ##Share in public? No problem, maybe just get the column 'interprete' out
  canada.fires$interprete
  out.dt$interprete
  if(all(na.counts==0)){
    out.csv <- file.path(
      "data_Classif", sprintf("CanadaFires_%s.csv", fires.name))
    print(out.csv)
    fwrite(out.dt, out.csv)
  }else{
    print("NA found so not outputting CSV")
    print(na.counts[na.counts>0])
  }
}
