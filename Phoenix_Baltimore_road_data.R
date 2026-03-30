library(data.table)
sheet_dt_list <- list()
for(sheet in 1:2){
  sheet_df <- readxl::read_xlsx("Phoenix_Baltimore_road_data.xlsx", sheet)
  sheet_dt_list[[sheet]] <- data.table(sheet_df)
}
(sheet_dt <- rbindlist(sheet_dt_list))
table(sheet_dt$city_key)

feature_dt <- nc::capture_all_str("
*NTL (Nighttime Lights)
*POP (Population)
*ROAD_DENS (Road Density)
S2_R (Sentinel-2 Red band reflectance)
S2_G (Sentinel-2 Green band reflectance)
S2_B (Sentinel-2 Blue band reflectance)
S2_NIR (Sentinel-2 Near-Infrared band reflectance)
", "\n[*]?", col_name=".*?", " \\(", desc=".*?", "\\)")
na_dt <- sheet_dt[, c(
  "city_key", "AADT", feature_dt[["col_name"]]
), with=FALSE]
ml_dt <- na_dt[!apply(is.na(na_dt), 1, any)]
table(ml_dt$city_key)

fwrite(ml_dt, "Phoenix_Baltimore_road_data.csv")

quantile(ml_dt$AADT)
hist(ml_dt$AADT)
hist(log10(ml_dt$AADT))
ml_dt[, y := log10(AADT)]

SOAK <- mlr3resampling::ResamplingSameOtherSizesCV$new()
SOAK$param_set$values$folds <- 5
SOAK$param_set$values$sizes <- 0
atask <- mlr3::TaskRegr$new(id="AADT", ml_dt, target="y")
atask$col_roles$subset <- "city_key"
atask$col_roles$feature <- feature_dt[["col_name"]]

future::plan("multisession")
learner_list <- list(
  mlr3::LearnerRegrFeatureless$new(),
  mlr3learners::LearnerRegrCVGlmnet$new())
bgrid <- mlr3::benchmark_grid(atask, learner_list, SOAK)
bres <- mlr3::benchmark(bgrid)
test_sizes <- ml_dt[, .(test.subset.rows=.N), by=.(test.subset=city_key)]
score_dt <- mlr3resampling::score(bres, mlr3::msr("regr.rmse"))
plist <- mlr3resampling::pvalue(score_dt)

png("Phoenix_Baltimore_road_data.png", width=7, height=5, units="in", res=200)
plot(plist)
dev.off()

mlr3resampling::pvalue_downsample(score_dt)
