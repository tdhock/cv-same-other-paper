library(data.table)
library(ggplot2)
score_dt <- fread("conv_images_10fold_test.csv")

plist <- mlr3resampling::pvalue(score_dt[, n.train.groups :=1], value.var="percent_error")
ent.map <- c(
  same="même",
  "all-same"="tout-même",
  other="autre",
  "other-same"="autre-même",
  all="tout")
for(name in c("stats","pvalues")){
  plist[[name]][, let(
    Algorithme = factor(algorithm, disp.levs),
    "sous-ensemble test"=test.subset
  )]
}
gg <- plot(plist)+
  facet_grid(Algorithme~`sous-ensemble test`, labeller=label_both)+
  scale_y_discrete("Entraînement",drop=F, labels=ent.map)+
  scale_x_continuous("Pourcentage d’erreur de prédiction (moyenne ± écart type, 10 divisions de validation croisée)")
png("figure8fr.png", width=7, height=8.6, units="in", res=200)
print(gg)
dev.off()
