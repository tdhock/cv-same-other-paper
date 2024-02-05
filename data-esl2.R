library(data.table)
data.list <- list()

prefix <- "https://hastie.su.domains/ElemStatLearn/datasets/"
cache.fread <- function(data.name, f){
  cache.dir <- file.path("data", data.name)
  dir.create(cache.dir, showWarnings=FALSE, recursive=TRUE)
  local.path <- file.path(cache.dir, f)
  if(!file.exists(local.path)){
    u <- paste0(prefix, f)
    download.file(u, local.path)
  }
  fread(local.path)
}

##https://hastie.su.domains/ElemStatLearn/datasets/14cancer.info.txt
for(data.name in c("14cancer", "khan")){
  ##https://hastie.su.domains/ElemStatLearn/datasets/khan.info.txt
  set.list <- list()
  for(predefined.set in c("train","test")){
    dt.args <- list()
    for(data.type in c("y","x")){
      data.dt <- cache.fread(
        data.name,
        sprintf("%s.%s%s", data.name, data.type, predefined.set)
      )
      data.mat <- t(data.dt)
      colnames(data.mat) <- if(data.type=="y"){
        "y"
      }else{
        paste0("x",1:ncol(data.mat))
      }
      dt.args[[data.type]] <- data.mat
    }
    one.set.dt <- do.call(data.table, unname(dt.args))
    set.list[[predefined.set]] <- data.table(predefined.set, one.set.dt)
  }
  data.list[[data.name]] <- rbindlist(set.list)
}

spam.prefix <- paste0(prefix, "spam")
spam.list <- list()
for(data.type in c("data", "traintest")){
  spam.list[[data.type]] <- cache.fread(
    data.name,
    paste0("spam.", data.type)
  )
}
new.names <- names(spam.list$data)
xseq <- seq(1, length(new.names)-1)
new.names[xseq] <- paste0("x",xseq)
new.names[length(new.names)] <- "y"
setnames(spam.list$data, new.names)
data.list$spam <- data.table(
  predefined.set=ifelse(spam.list$traintest$V1==1, "test", "train"),
  spam.list$data)

for(data.name in c("vowel","waveform","zip")){
  suffix <- if(data.name=="zip")".gz" else ""
  set.list <- list()
  for(predefined.set in c("test","train")){
    one.set.dt <- cache.fread(
      data.name,
      paste0(data.name, ".", predefined.set, suffix)
    )
    if("row.names" %in% names(one.set.dt)){
      one.set.dt[, row.names := NULL]
    }
    setnames(one.set.dt, old=names(one.set.dt)[1], new="y")
    set.list[[predefined.set]] <- data.table(
      predefined.set, one.set.dt)
  }
  data.list[[data.name]] <- rbindlist(set.list)
}

for(data.name in names(data.list))fwrite(
  data.list[[data.name]],
  paste0(file.path("data", data.name), ".csv")
)
