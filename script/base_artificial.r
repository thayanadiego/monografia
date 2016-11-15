pacotes <- c("clusterGeneration", "cluster", "dplyr", "data.table", "stringr")
sapply(pacotes, function(x) require(x, character.only = TRUE))

#.Platform$OS.type
if (tolower(Sys.info()["sysname"]) == "linux") {
  wd <- "/media/diego/SAMSUNG/monografia"
} else {
  wd <- "E:/monografia"
}

dir_artificial <- "/bases/artificial/"

obj_mult <- c(5, 50)
prop_ruido <- c(0, 0.1, 0.2, 0.5, 0.8)
n_vars <- c(10, 20, 40)
n_cluster <-  5:10
sep_val <-  seq(0.4, 0.8, by = 0.1)

parametros <- CJ(obj_mult = obj_mult,
                 prop_ruido = prop_ruido,
                 vars_total = n_vars,
                 n_cluster = n_cluster,
                 sep_val = sep_val)

parametros[, n_vars := round((1 - prop_ruido)*vars_total, 0)]
parametros[, n_ruido := vars_total - n_vars]
parametros[, n_obj := obj_mult*vars_total]
parametros[, time := numeric(nrow(parametros))]


n_replicate <- 3L
for (i in 1:n_replicate) {
  set(x = parametros, j = paste("n_obj_real_", i, sep = ""),
      value = integer(nrow(parametros)))
}

parametros <- parametros[order(n_cluster)]


n <- nrow(parametros)


if(installed.packages()["data.table", "Version"] == "1.9.7") {
  write_file <- fwrite
} else {
  write_file <- write.csv
}


pb <- txtProgressBar(min = 0, max = n, style = 3)
for (itr in 1:n) {
  t0 <- proc.time()
  dados <- genRandomClust(numClust = parametros[itr, n_cluster],
                          sepVal = parametros[itr, sep_val],
                          numNonNoisy = parametros[itr, n_vars],
                          numNoisy = parametros[itr, n_ruido],
                          numReplicate = n_replicate)
  
  for (i_base in 1:n_replicate) {
    test <- paste("test", i_base, sep = "_")
    
    base <- dados$datList[[test]]
    base <- base %>% as.data.frame()
    col_ruido <- dados$noisyList[[test]]
    
    set(x = parametros, i = itr,
        j = paste("n_obj_real_", i_base, sep = ""),
        value = nrow(base))

    if (parametros[itr, n_ruido] > 0) {
      colnames(base)[col_ruido] <- paste("r", col_ruido, sep = "")
    }
    
    pasta <- paste("x", parametros[itr, n_vars], "_",
                   "r", parametros[itr, n_ruido], "_",
                   "k", parametros[itr, n_cluster], "_",
                   "s", parametros[itr, sep_val], "/",
                   sep = "")
    new_dir <- paste(wd, dir_artificial, pasta, sep = "")
    if (!dir.exists(new_dir)) dir.create(path = new_dir)
    
    export_name <- paste(wd, dir_artificial, pasta,
                         "artificial_n", nrow(base), "_",
                         "x", parametros[itr, n_vars], "_",
                         "r", parametros[itr, n_ruido], "_",
                         "k", parametros[itr, n_cluster], "_",
                         "g", i_base,
                         ".csv", sep = "")
    # x: numero de variaveis nao ruido
    # r: numero de variaveis de ruido
    # k: numero de clusters
    
    write_file(x = base, file = export_name)
  }
  t1 <- proc.time() - t0
  parametros[itr, time := t1["elapsed"]]

  setTxtProgressBar(pb, itr)
}
close(pb)