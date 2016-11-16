pacotes <- c("data.table", "vscc", "cluster")
sapply(pacotes, function(x) require(x, character.only = TRUE))

sil <- function(x, d) {
  library(cluster)
  out <- mean(cluster::silhouette(x = x, dist = d)[, "sil_width"])
  return(out)
}

if (tolower(Sys.info()["sysname"]) == "linux") {
  wd <- "/media/diego/SAMSUNG/monografia"
} else {
  wd <- "E:/monografia"
}

base_ok <- fread(input = paste0(wd, "/bases/var_ok.csv"),
                 verbose = TRUE)

tabela_resumo <- fread(input = paste0(wd, "/tabelas/", "tabela_resumo.csv"),
                       verbose = TRUE)

bases <- tabela_resumo[P > 1, INSTANCIA]
n_base <- length(bases)

# tabela_resumo <- data.table(INSTANCIA = character(n_base),
#                             N = integer(n_base),
#                             P = integer(n_base))

n_grupos <- 10L
n_relacao <- 5L
n_sil <- n_base * (n_grupos - 1) * n_relacao 


sil_export <- CJ(BASE = bases,
                 RELACAO = 1:5,
                 N_GRUPOS = 2:10)

sil_export[, ":="("KMEANS_ALL" = numeric(),
                  "KMEANS_VSCC" = numeric(),
                  "KMEDOIDS_ALL" = numeric(),
                  "KMEDOIDS_VSCC" = numeric())]

t0 <- proc.time()
for (i in 1:n_base) {
  #i <- which(bases == "ecoli")
  file_dir <- paste0(wd, "/bases/archive.ics.uci.edu/",
                     bases[i], "/", bases[i], ".data.txt")
  dados <- fread(input = file_dir, verbose = TRUE)
  setDF(dados)
  
  row_len <- nrow(dados)
  col_len <- ncol(dados)
  
  row_na <- apply(X = dados, MARGIN = 1, FUN = function(x) sum(is.na(x)))
  col_na <- sapply(X = dados, FUN = function(x) sum(is.na(x)))
  
  col_type <- sapply(X = dados, FUN = class)
  
  col_ok <- base_ok[BASE == bases[i] &
                      DISTINCT >= 5 &
                      TIPO %in% c("double", "integer"),
                    NOME]
  
  dados_scale <- dados[, col_ok, drop = FALSE]
  
  # analise exploratoria
  png(filename = paste0(wd, "/graficos/dispersao/", "plot_", bases[i], ".png"),
      width = 1600, height = 900, res = 100)
  plot(dados_scale[, 1:(min(ncol(dados_scale), 20))])
  dev.off()
  
  if (NCOL(dados_scale) > 1) {
    for (j_col in 1:ncol(dados_scale)) {
      dados_scale[, j_col] <- scale(dados_scale[, j_col])
    }
  # 
  #   tabela_resumo[i, ":="(INSTANCIA = bases[i],
  #                         N = nrow(dados_scale),
  #                         P = ncol(dados_scale))]
  # } else {
  #   tabela_resumo[i, ":="(INSTANCIA = bases[i],
  #                         N = NROW(dados_scale),
  #                         P = 1L)]
  }
  
  png(filename = paste0(wd, "/graficos/boxplot/", "boxplot_",
                        bases[i], ".png"),
      width = 1600, height = 900, res = 100)
  boxplot(dados_scale[, 1:(min(ncol(dados_scale), 20))], main = bases[i])
  dev.off()
  
  var_select <- vscc(x = dados_scale)
  
  dist_all <- dist(x = dados_scale)
  
  for (i_rel in 1:n_relacao) {
    col_select <- colnames(var_select$selected[[i_rel]])
    dist_vscc <- dist(x = dados_scale[, col_select])
    
    kmeans_all <- vector(mode = "list", length = n_grupos - 1L)
    kmeans_vscc <- vector(mode = "list", length = n_grupos - 1L)
    kmedoids_all <- vector(mode = "list", length = n_grupos - 1L)
    kmedoids_vscc <- vector(mode = "list", length = n_grupos - 1L)
    
    for (i_grupo in 2:n_grupos) {
      set.seed(seed = 1L)
      kmeans_all[[i_grupo]] <- kmeans(x = dados_scale, centers = i_grupo)
      
      set.seed(seed = 1L)
      kmeans_vscc[[i_grupo]] <- kmeans(x = dados_scale[, col_select],
                                       centers = i_grupo)
      
      kmedoids_all[[i_grupo]] <- pam(x = dados_scale, k = i_grupo)
      
      kmedoids_vscc[[i_grupo]] <- pam(x = dados_scale[, col_select],
                                      k = i_grupo)

      sil_export[BASE == bases[i] & RELACAO == i_rel & N_GRUPOS == i_grupo,
                 ":="("KMEANS_ALL" = sil(x = kmeans_all[[i_grupo]]$cluster,
                                         d = dist_all),
                      "KMEANS_VSCC" = sil(x = kmeans_vscc[[i_grupo]]$cluster,
                                          d = dist_vscc),
                      "KMEDOIDS_ALL" = sil(x = kmedoids_all[[i_grupo]]$clustering,
                                           d = dist_all),
                      "KMEDOIDS_VSCC" = sil(x = kmedoids_vscc[[i_grupo]]$clustering,
                                            d = dist_vscc))]
    }
  }
}
t1 <- proc.time()
delta <- t1 - t0

### ...TESTE...

calc_sil <- function(x, d) {
  library(cluster)
  out <- mean(cluster::silhouette(x = x, dist = d)[, "sil_width"])
  return(out)
}

select_columns <- function(x) {
  
  row_len <- nrow(dados)
  col_len <- ncol(dados)
  
  row_na <- apply(X = dados, MARGIN = 1, FUN = function(x) sum(is.na(x)))
  col_na <- sapply(X = dados, FUN = function(x) sum(is.na(x)))
  
  col_type <- sapply(X = dados, FUN = class)
  
}

apply_vscc <- function(x, nome, n_grupos = 10L, n_relacao = 5L) {
  
  library(vscc)
  library(cluster)
  library(data.table)
  
  x_scale <- copy(x)
  setDF(x_scale)
  
  sil_DF <- CJ(RELACAO = 1:n_relacao,
               N_GRUPOS = 2:n_grupos)
  
  sil_DF[, ":="("KMEANS_ALL" = numeric(),
                "KMEANS_VSCC" = numeric(),
                "KMEDOIDS_ALL" = numeric(),
                "KMEDOIDS_VSCC" = numeric())]
  
  for (j_col in 1:ncol(x_scale)) {
    x_scale[, j_col] <- scale(x_scale[, j_col])
  }
  
  var_select <- vscc(x = x_scale, G = 2:9)
  
  dist_all <- dist(x = x_scale)
  
  for (i_rel in 1:n_relacao) {
    col_select <- colnames(var_select$selected[[i_rel]])
    dist_vscc <- dist(x = dados_scale[, col_select])
    
    kmeans_all <- vector(mode = "list", length = n_grupos)
    kmeans_vscc <- vector(mode = "list", length = n_grupos)
    kmedoids_all <- vector(mode = "list", length = n_grupos)
    kmedoids_vscc <- vector(mode = "list", length = n_grupos)
    
    for (i_grupo in 2:n_grupos) {#trocar a ordem dos for
      set.seed(seed = 1L)
      kmeans_all[[i_grupo]] <- kmeans(x = x_scale, centers = i_grupo)
      
      set.seed(seed = 1L)
      kmeans_vscc[[i_grupo]] <- kmeans(x = x_scale[, col_select],
                                       centers = i_grupo)
      
      kmedoids_all[[i_grupo]] <- pam(x = x_scale, k = i_grupo)
      
      kmedoids_vscc[[i_grupo]] <- pam(x = x_scale[, col_select],
                                      k = i_grupo)
      
      sil_DF[RELACAO == i_rel & N_GRUPOS == i_grupo,
             ":="("KMEANS_ALL" = calc_sil(x = kmeans_all[[i_grupo]]$cluster,
                                          d = dist_all),
                  "KMEANS_VSCC" = calc_sil(x = kmeans_vscc[[i_grupo]]$cluster,
                                           d = dist_vscc),
                  "KMEDOIDS_ALL" = calc_sil(x = kmedoids_all[[i_grupo]]$clustering,
                                            d = dist_all),
                  "KMEDOIDS_VSCC" = calc_sil(x = kmedoids_vscc[[i_grupo]]$clustering,
                                             d = dist_vscc))]
    }
  }
  
  return(cbind(BASE = nome, sil_DF))
  
}


n_grupos = 10L
n_relacao = 5L

sil_DF <- CJ(RELACAO = 1:n_relacao,
             GRUPOS = 2:n_grupos,
             AGRUPAMENTO = c("KMEANS", "KMEDIAN"),
             METODO_SELECAO = c("ALL", "VSCC")
             )

sil_DF[, SILHOUETE := numeric()]




# MAIN

pacotes <- c("data.table", "vscc", "cluster")
sapply(pacotes, function(x) require(x, character.only = TRUE))

# library(fpc)
# library(dplyr)
# library(nycflights13)

if (tolower(Sys.info()["sysname"]) == "linux") {
  wd <- "/media/diego/SAMSUNG/monografia"
} else {
  wd <- "E:/monografia"
}

base_ok <- fread(input = paste0(wd, "/bases/var_ok.csv"),
                 verbose = TRUE)

tabela_resumo <- fread(input = paste0(wd, "/tabelas/", "tabela_resumo.csv"),
                       verbose = TRUE)

bases <- tabela_resumo[P > 1, INSTANCIA]
n_base <- length(bases)


final <- vector(mode = "list", length = n_base)

for (i in 1:n_base) {
  #i <- which(bases == "ecoli")
  file_dir <- paste0(wd, "/bases/archive.ics.uci.edu/",
                     bases[i], "/", bases[i], ".data.txt")
  dados <- fread(input = file_dir, verbose = TRUE)
  setDF(dados)
  
  row_len <- nrow(dados)
  col_len <- ncol(dados)
  
  row_na <- apply(X = dados, MARGIN = 1, FUN = function(x) sum(is.na(x)))
  col_na <- sapply(X = dados, FUN = function(x) sum(is.na(x)))
  
  col_type <- sapply(X = dados, FUN = class)
  
  col_ok <- base_ok[BASE == bases[i] &
                      DISTINCT >= 5 &
                      TIPO %in% c("double", "integer"),
                    NOME]
  
  final[[i]] <- apply_vscc(dados = dados[, col_ok], nome = bases[i])
}

export <- rbindlist(l = final)

### --- LEADER
venda <- fread(input = "/media/diego/SAMSUNG/monografia/bases/leader/venda_scale.csv")
dim(venda)

teste <- apply_vscc(dados = venda[, c(-1), with = FALSE], nome = "venda")
teste[, KMEANS_VAR := KMEANS_VSCC/KMEANS_ALL - 1]
teste[, KMEDOIDS_VAR := KMEDOIDS_VSCC/KMEDOIDS_ALL - 1]

write.csv(x = teste, file = "/media/diego/SAMSUNG/monografia/tabelas/tabela_silhueta_venda_leader.csv",
          row.names = FALSE)


### --- ATLAS
atlas <- fread(input = "/media/diego/SAMSUNG/monografia/bases/atlas/AtlasBrasil_Consulta.csv")

library(readxl)
library(stringr)
library(dplyr)

atlas <- read_excel(path = "/media/diego/SAMSUNG/monografia/bases/atlas/AtlasBrasil_Consulta.xlsx")
setDT(atlas)

estados <- str_match(atlas[-1, Lugar], str_c("\\(", "(.+)", "\\)"))[, 2] %>%
  unique() %>% sort()

# Só tem um objeto
estados <- setdiff(estados, "DF")

i <- which(estados == "SP")

r <- data.table(INSTANCIA = str_c("IDH - ", estados))
r[, N := integer()]
r[, P := integer()]


teste <- vector(mode = "list", length = length(estados))
for (i in seq_along(estados)) {
  dados <- atlas[str_detect(Lugar, str_c("\\(", estados[i], "\\)")),
                 c(-8:-1), with = FALSE]
  
  teste[[i]] <- apply_vscc(dados = dados, nome = str_c("idh_", estados[i]))
  teste[[i]][, KMEANS_VAR := KMEANS_VSCC/KMEANS_ALL - 1]
  teste[[i]][, KMEDOIDS_VAR := KMEDOIDS_VSCC/KMEDOIDS_ALL - 1]
  
  write.csv(x = teste[[i]],
            file = str_c("/media/diego/SAMSUNG/monografia/tabelas/tabela_silhueta_idh_", estados[i], ".csv"),
            row.names = FALSE)
  # r[i, N := nrow(dados)]
  # r[i, P := ncol(dados)]
}

# tabela_resumo <- rbind(tabela_resumo, r)
# write.csv(x = tabela_resumo[!is.na(N)],
#           file = paste0(wd, "/tabelas/", "tabela_resumo.csv"),
#           row.names = FALSE)

teste <- apply_vscc(dados = dados, nome = "idh")
teste[, KMEANS_VAR := KMEANS_VSCC/KMEANS_ALL - 1]
teste[, KMEDOIDS_VAR := KMEDOIDS_VSCC/KMEDOIDS_ALL - 1]


# ------------------------------- executa o vscc

sil <- function(x, d) {
  
  library(cluster)
  
  out <- mean(cluster::silhouette(x = x, dist = d)[, "sil_width"])
  
  return(out)
  
}


/media/diego/SAMSUNG/monografia/bases/archive.ics.uci.edu/abalone

x <- fread("/media/diego/SAMSUNG/monografia/bases/archive.ics.uci.edu/abalone/abalone.data.txt")

vscc_apply <- function(x, cluster_method = "kmeans") {
  
  # cluster_method: kmeans, kmedoids
  
  packages <- c("data.table", "vscc", "cluster")
  sapply(packages, function(x) require(x, character.only = TRUE))
  
  setDF(x)
  
  # atributos
  col_len <- ncol(x)
  col_na <- sapply(X = x, FUN = function(x) sum(is.na(x)))
  col_distinct <- sapply(X = x, FUN = function(x) length(unique(x)))
  col_type <- sapply(X = x, FUN = class)

  col_ok <- col_distinct >= 5 & col_type %in% c("double", "integer", "numeric")
  
  # objetos
  row_len <- nrow(x)
  row_na <- apply(X = x, MARGIN = 1, FUN = function(x) sum(is.na(x)))
  
  # outlier

  x_scale <- x[, col_ok, drop = FALSE]
  
  for (j_col in 1:ncol(x_scale)) {
    x_scale[, j_col] <- scale(x_scale[, j_col])
  }

  var_select <- vscc(x = x_scale, G = 2:9)
  
  
  
  dist_all <- dist(x = x_scale)
  
  
}







