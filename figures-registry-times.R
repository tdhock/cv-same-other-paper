reg.dirs=dirname(Sys.glob("/scratch/th798/cv-same-other-paper/*/registry.rds"))
out.dir="figures-registry-times"
dir.create(out.dir)

for(reg.dir in reg.dirs){
  reg=  batchtools::loadRegistry(reg.dir)
  jdt=batchtools::getJobTable()
  jdt[, let(
    algorithm=gsub("classif.","",sapply(algo.pars,"[[","learner_id")),
    task_id=sapply(prob.pars,"[[","task_id")
  )]
  out_dt <- jdt[, .(started, done, repl, algorithm, task_id)]
  (out.csv <- sprintf("%s/%s.csv", out.dir, basename(reg.dir)))
  data.table::fwrite(out_dt, out.csv)
}

library(data.table)
time_dt <- nc::capture_first_glob(
  "figures-registry-times/*csv",
  "figures-registry-times/data_Classif_",
  reg=".*?",
  "_registry.csv")
time_dt[, isNA := is.na(started)]
print(dcast(time_dt, reg+algorithm+task_id~isNA), nrows=200)
dcast(time_dt, reg+algorithm+task_id~.), nrows=200)
