pacotes <- c("data.table", "vscc", "cluster")
sapply(pacotes, function(x) require(x, character.only = TRUE))

sil <- function(x, d) {
  library(cluster)
  out <- mean(cluster::silhouette(x = x, dist = d)[, "sil_width"])
  return(out)
}

# library(fpc)
# library(dplyr)
# library(nycflights13)

if (tolower(Sys.info()["sysname"]) == "linux") {
  wd <- "/media/diego/SAMSUNG/a Monografia/MONOGRAFIA"
} else {
  wd <- "E:/a Monografia/MONOGRAFIA"
}

base_ok <- fread(input = paste0(wd, "/datasets/bases/var_ok.csv"),
                 verbose = TRUE)

tabela_resumo <- fread(input = paste0(wd, "/tabelas/", "tabela_resumo.csv"),
                       verbose = TRUE)

bases <- tabela_resumo[P > 1, INSTANCIA]
n_base <- length(bases)

# tabela_resumo <- data.table(INSTANCIA = character(n_base),
#                             N = integer(n_base),
#                             P = integer(n_base))

n_grupos <- 5L
n_relacao <- 5L
n_sil <- n_base * (n_grupos - 1) * n_relacao * 4

sil_export <- data.table(BASE = character(n_sil),
                         MET_SELECAO = character(n_sil),
                         MET_AGRUP = character(n_sil),
                         N_GRUPOS = integer(n_sil),
                         RELACAO = integer(n_sil),
                         SILHUETA = numeric(n_sil))
itr <- 0L

for (i in 1:n_base) {
  #i <- which(bases == "ecoli")
  file_dir <- paste0(wd, "/datasets/bases/archive.ics.uci.edu/",
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
  
  # if (NCOL(dados_scale) > 1) {
  #   for (j_col in 1:ncol(dados_scale)) {
  #     dados_scale[, j_col] <- scale(dados_scale[, j_col])
  #   }
  # 
  #   tabela_resumo[i, ":="(INSTANCIA = bases[i],
  #                         N = nrow(dados_scale),
  #                         P = ncol(dados_scale))]
  # } else {
  #   tabela_resumo[i, ":="(INSTANCIA = bases[i],
  #                         N = NROW(dados_scale),
  #                         P = 1L)]
  # }
  
  png(filename = paste0(wd, "/graficos/boxplot/", "boxplot_", bases[i], ".png"),
      width = 1600, height = 900, res = 100)
  boxplot(dados_scale[, 1:(min(ncol(dados_scale), 20))], main = bases[i])
  dev.off()
  
  var_select <- vscc(x = dados_scale)
  
  dist_all <- dist(x = dados_scale)
  
  for (i_rel in 1:n_relacao) {
    col_select <- colnames(var_select$selected[[i_rel]])
    dist_vscc <- dist(x = dados_scale[, col_select])
    
    n_grupos <- 10L
    kmeans_all <- vector(mode = "list", length = n_grupos - 1L)
    kmeans_vscc <- vector(mode = "list", length = n_grupos - 1L)
    kmedoids_all <- vector(mode = "list", length = n_grupos - 1L)
    kmedoids_vscc <- vector(mode = "list", length = n_grupos - 1L)
    
    for (i_grupo in 2:n_grupos) {
      kmeans_all[[i_grupo]] <- kmeans(x = dados_scale, centers = i_grupo)
      
      itr <- itr + 1L
      sil_export[itr, BASE := bases[i]]
      sil_export[itr, MET_SELECAO := "all"]
      sil_export[itr, MET_AGRUP := "kmeans"]
      sil_export[itr, N_GRUPOS := i_grupo]
      sil_export[itr, RELACAO := i_rel]
      sil_export[itr, SILHUETA := sil(x = kmeans_all[[i_grupo]]$cluster,
                                      d = dist_all)]
      
      kmeans_vscc[[i_grupo]] <- kmeans(x = dados_scale[, col_select],
                                       centers = i_grupo)
      
      itr <- itr + 1L
      sil_export[itr, BASE := bases[i]]
      sil_export[itr, MET_SELECAO := "vscc"]
      sil_export[itr, MET_AGRUP := "kmeans"]
      sil_export[itr, N_GRUPOS := i_grupo]
      sil_export[itr, RELACAO := i_rel]
      sil_export[itr, SILHUETA := sil(x = kmeans_vscc[[i_grupo]]$cluster,
                                      d = dist_vscc)]
      
      kmedoids_all[[i_grupo]] <- pam(x = dados_scale, k = i_grupo)
      
      itr <- itr + 1L
      sil_export[itr, BASE := bases[i]]
      sil_export[itr, MET_SELECAO := "all"]
      sil_export[itr, MET_AGRUP := "kmedoids"]
      sil_export[itr, N_GRUPOS := i_grupo]
      sil_export[itr, RELACAO := i_rel]
      sil_export[itr, SILHUETA := sil(x = kmedoids_all[[j]]$clustering,
                                      d = dist_all)]
      
      kmedoids_vscc[[i_grupo]] <- pam(x = dados_scale[, col_select],
                                      k = i_grupo)
      
      itr <- itr + 1L
      sil_export[itr, BASE := bases[i]]
      sil_export[itr, MET_SELECAO := "vscc"]
      sil_export[itr, MET_AGRUP := "kmedoids"]
      sil_export[itr, N_GRUPOS := i_grupo]
      sil_export[itr, RELACAO := i_rel]
      sil_export[itr, SILHUETA := sil(x = kmedoids_vscc[[i_grupo]]$clustering,
                                      d = dist_vscc)]
      
    }
  }
}


### ...TESTE...

fwrite(x = tabela_resumo,
       file = paste0(wd, "/tabelas/", "tabela_resumo.csv"))

cor_dados <- function(dados, cor_min = 0.7) {
  cor_mat <- which(abs(cor(dados)) > cor_min, arr.ind = TRUE)
  return(cor_mat[which(cor_mat[, 1] > cor_mat[, 2]), ])
}

cor_dados(dados = dados_scale)


#file_dir <- paste0(wd, "/atlas_brasil/", "base_idh.csv")
#dados <- fread(input = file_dir, verbose = TRUE, dec = ",")


t0 <- proc.time()
var_select <- vscc(x = dados_scale)
t1 <- proc.time()
(t1 - t0) / 60
# usuário   sistema decorrido 
# 7.332333  0.001500  7.337167
# em minutos
#
var_select
summary(var_select)

var_select$selected[[1]] # relacao linear
var_select$selected[[2]] # relacao quadratica
var_select$selected[[3]] # relacao cubica
var_select$selected[[4]]
var_select$selected[[5]]


# --- TESTE --- #
dbscan

