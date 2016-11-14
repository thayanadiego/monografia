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
n_vars <- c(10, 50, 150)
n_cluster <-  5:15
sep_val <-  0.7

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



n <- nrow(parametros)
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
    
    base <- dados$datList[[test]] %>% as.data.frame()
    col_ruido <- dados$noisyList[[test]]
    
    set(x = parametros, i = itr,
        j = paste("n_obj_real_", i_base, sep = ""),
        value = nrow(base))

    if (parametros[itr, n_ruido] > 0) {
      colnames(base)[col_ruido] <- paste("r", col_ruido, sep = "")
    }
    
    pasta <- paste("x", parametros[itr, n_vars], "_",
                   "r", parametros[itr, n_ruido], "_",
                   "k", parametros[itr, n_cluster], "/",
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
    
    fwrite(x = base, file = export_name)
  }
  t1 <- proc.time() - t0
  parametros[itr, time := t1["elapsed"]]
  setTxtProgressBar(pb, itr)
}
close(pb)



# ---- verificar a versao do data.table para fwrite!!!




resultado <- matrix(data = NA, nrow = length(sep_val), ncol = 3)
for (i in seq_along(sep_val)) {
  dados <- genRandomClust(numClust = n_cluster, sepVal = sep_val[i], numNonNoisy = 15L)
  
  x1 <- dados$datList$test_1
  x2 <- dados$datList$test_2
  x3 <- dados$datList$test_3
  
  d1 <- dist(x1)
  d2 <- dist(x2)
  d3 <- dist(x3)
  
  
  n <- 100L
  sil_1 <- sil_2 <- sil_3 <- numeric(n)
  
  for (j in 1:n) {
    
    sil_1[j] <- silhouette(x = kmeans(x = x1, centers = n_cluster)$cluster,
                           dist = d1)[, "sil_width"] %>% mean()
    
    sil_2[j] <- silhouette(x = kmeans(x = x2, centers = n_cluster)$cluster,
                           dist = d2)[, "sil_width"] %>% mean()
    
    sil_3[j] <- silhouette(x = kmeans(x = x3, centers = n_cluster)$cluster,
                           dist = d3)[, "sil_width"] %>% mean()
  }
  
  resultado[i, ] <-c(sil_1 %>% median(), sil_2 %>% median(), sil_3 %>% median())
  
}



row.names(resultado) <- sep_val

