library(data.table)
data.dir <- "data_Classif_NSCH_autism"
dir.create(data.dir, showWarnings = FALSE)
nsch.prefix <- "https://www.census.gov/programs-surveys/nsch/data/datasets."
for(year in 2019:2020){
  year.html <- paste0(year, ".html")
  ##https://www.census.gov/programs-surveys/nsch/data/datasets.2020.html
  u <- paste0(nsch.prefix, year.html)
  dest.html <- file.path(data.dir, year.html)
  if(!file.exists(dest.html))download.file(u, dest.html)
  url.dt <- nc::capture_all_str(
    dest.html,
    'href="',
    url=".*?_topical_Stata.zip",
    '"')
  if(nrow(url.dt)==0)stop("no urls on ", dest.html)
  for(url.i in 1:nrow(url.dt)){
    zip.url <- paste0("http:", url.dt[url.i, url])
    year.zip <- file.path(data.dir, basename(zip.url))
    if(!file.exists(year.zip))download.file(zip.url, year.zip)
    unzip(year.zip, exdir=data.dir)
  }
}

(year.do.vec <- Sys.glob(file.path(data.dir, "*.do")))
out.dt.list <- list()
compare.dt.list <- list()
for(year.do in year.do.vec){
  year.dta <- sub("do$", "dta", year.do)
  year.tib <- haven::read_dta(year.dta)
  var.csv <- paste0(year.do, ".var.csv")
  if(file.exists(var.csv)){
    var.dt <- fread(var.csv)
  }else{
    var.dt <- nc::capture_all_str(
      year.do,
      "label var ",
      variable=".*?",
      ' +"',
      desc=".*?",
      '"')
    fwrite(var.dt, var.csv)
  }
  setkey(var.dt, variable)
  define.csv <- paste0(year.do, ".define.csv")
  if(file.exists(define.csv)){
    define.dt <- fread(define.csv)
  }else{
    define.dt <- nc::capture_all_str(
      year.do,
      "label define ",
      variable=".*?",
      "_lab +",
      value=".*?",
      ' +"',
      desc=".*?",
      '"')
    fwrite(define.dt, define.csv)
  }
  ## label var screentime  "How Much Time Spent with TV, Cellphone, Computer"
  ## cap label values screentime screentime_lab
  ## label define screentime_lab  1  "Less than 1 hour"
  ## label define screentime_lab  2  "1 hour", add
  define.dt[variable=="screentime"]
  define.not.missing <- define.dt[
    !grepl("[.]", value)
  ][
  , value.int := as.integer(value)
  ][]
  year.dt <- data.table(
    year.tib
  )[, survey_year := as.integer(sub("_.*", "", sub(".*?_", "", basename(year.do))))]
  ## check parsing.
  ##options(width=150)
  define.not.missing[, {
    print(variable)
    num.vec <- year.tib[[variable]]
    fac.vec <- factor(num.vec, value.int, desc)
    val.tab <- table(fac.vec, num.vec, useNA="always")
    na.i <- which(is.na(rownames(val.tab)))
    na.counts <- val.tab[na.i,]
    if(sum(na.counts!=0)>1){
      ## More than one original numeric value would be converted to NA,
      ## so just print out what would happen, and keep the original
      ## numeric column.
      print(val.tab)
    }else{
      set(year.dt, j=variable, value=fac.vec)
    }
    NULL
  }, by=variable]
  year.dt[, y := k2q35a]
  prop.NA <- colMeans(is.na(year.dt))
  half.names <- setdiff(names(prop.NA)[prop.NA<0.1], c("y","k2q35a","survey_year","year"))
  some.col.names <- c("survey_year", "y", half.names)
  year.some.cols <- year.dt[, some.col.names, with=FALSE]
  keep <- apply(!is.na(year.some.cols), 1, all)
  year.keep <- year.some.cols[keep]
  year.X.dt <- year.keep[, half.names, with=FALSE]
  X.mat.list <- list()
  for(col.i in seq_along(year.X.dt)){
    col.vec <- year.X.dt[[col.i]]
    col.tab <- table(col.vec)
    if(length(col.tab)>1){
      col.name <- names(year.X.dt)[[col.i]]
      new.name <- var.dt[col.name, desc]
      if(is.na(new.name)){
        cat(sprintf("consider excluding column %s\n", col.name))
      }
      if(is.factor(col.vec)){
        col.levs <- levels(col.vec)
        out.levs <- col.levs[-length(col.levs)]
        for(lev in out.levs){
          X.mat.list[[sprintf("%s=%s", new.name, lev)]] <- ifelse(col.vec==lev, 1, 0)
        }
      }else if(is.numeric(col.vec)){
        X.mat.list[[new.name]] <- col.vec
      }
    }
  }
  X.mat <- do.call(cbind, X.mat.list)
  y.vec <- year.keep$y
  if(FALSE){
    y.tab <- table(y.vec)
    class.weight.list <- list(
      balanced=1/y.tab[paste(y.vec)],
      identity=rep(1, length(y.vec)))
    fit.list <- list()
    for(class.weight.name in names(class.weight.list)){
      class.weight.vec <- class.weight.list[[class.weight.name]]
      fit.list[[class.weight.name]] <- cv.glmnet(
        X.mat, y.vec, class.weight.vec, family="binomial")
    }
    for(class.weight.name in names(fit.list)){
      fit <- fit.list[[class.weight.name]]
      weight.mat <- coef(fit)
      print(weight.mat[as.logical(weight.mat!=0),])
    }
  }
  year.out <- year.keep[, data.table(
    survey_year, y, X.mat)]
  out.dt.list[[year.dta]] <- year.out
  get_meta <- function(data.type, dt, question.vec){
    na.dt <- is.na(dt)
    is.autism <- if(data.type=="raw"){
      dt[["k2q35a"]]==1
    }else{
      dt[["y"]]=="Yes"
    }
    data.table(
      data.type,
      nrow=nrow(dt),
      ncol=ncol(dt),
      questions=length(question.vec),
      "%Autism"=100*mean(is.autism, na.rm=TRUE),
      "%rowsNA"=100*mean(apply(na.dt, 1, any)),
      "%colsNA"=100*mean(apply(na.dt, 2, any)))
  }
  print(compare.dt.list[[year.dta]] <- data.table(
    year.out[1, .(year=survey_year)], rbind(
      get_meta("raw", year.tib, names(year.tib)),
      get_meta("processed", year.out, unique(sub("=.*", "", names(year.out))))
    )
  ))
}
(compare.dt <- rbindlist(compare.dt.list))

common.names <- Reduce(intersect, sapply(out.dt.list, names))
out.dt <- rbindlist(lapply(out.dt.list, function(DT)DT[, common.names,with=FALSE]))
setnames(out.dt, gsub("'", "", gsub("[$]", "USD", gsub("[],:;+?()/<> =[-]", "_", common.names, perl=TRUE))))
fwrite(out.dt, "data_Classif/NSCH_autism.csv")
