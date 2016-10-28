pacotes <- c("data.table", "vscc", "cluster")
sapply(pacotes, function(x) require(x, character.only = TRUE))

# library(fpc)
# library(dplyr)
# library(nycflights13)


if (tolower(Sys.info()["sysname"]) == "linux") {
  wd <- "/media/diego/SAMSUNG/a Monografia/MONOGRAFIA/datasets/bases"
} else {
  wd <- "E:/a Monografia/MONOGRAFIA/datasets/bases"
}


base_ok <- fread(input = paste0(wd, "/var_ok.csv"),
                 verbose = TRUE)

bases <- base_ok[, sort(unique(BASE))]

i <- which(bases == "flag")
#file_dir <- paste0(wd, "/archive.ics.uci.edu/", bases[i], "/", bases[i], ".data.txt")

file_dir <- paste0(wd, "/atlas_brasil/", "base_idh.csv")

dados <- fread(input = file_dir, verbose = TRUE, dec = ",")
setDF(dados)

row_len <- nrow(dados)
col_len <- ncol(dados)

row_na <- apply(X = dados, MARGIN = 1, FUN = function(x) sum(is.na(x)))
col_na <- sapply(X = dados, FUN = function(x) sum(is.na(x)))

col_type <- sapply(X = dados, FUN = class)

# col_ok <- names(col_type[which(col_type %in% c("numeric", "integer"))])      


col_ok <- base_ok[BASE == bases[i] &
                    DISTINCT >= 5 &
                    TIPO %in% c("double", "integer"),
                  NOME]

dados_scale <- dados[, col_ok]

# paste0(round(length(dados[, "V5"] %>% unique()) / nrow(dados) * 100, 4),
#        "%")


for (i_col in 1:ncol(dados_scale)) {
  dados_scale[, i_col] <- scale(dados_scale[, i_col])
}

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

# essa parte que nao sei como sera a regra de decisao

sil <- function(x, d) {
  library(cluster)
  out <- mean(cluster::silhouette(x = x, dist = d)[, "sil_width"])
  return(out)
}


for (i_rel in 1:5) {
  col_select <- colnames(var_select$selected[[i_rel]])
  
  n_grupos <- 10L
  kmeans_all <- vector(mode = "list", length = n_grupos - 1L)
  kmeans_vscc <- vector(mode = "list", length = n_grupos - 1L)
  kmedoids_all <- vector(mode = "list", length = n_grupos - 1L)
  kmedoids_vscc <- vector(mode = "list", length = n_grupos - 1L)
  
  for (j in 2:n_grupos) {
    kmeans_all[[j]] <- kmeans(x = dados_scale, centers = j)
    kmeans_vscc[[j]] <- kmeans(x = dados_scale[, col_select], centers = j)
    
    kmedoids_all[[j]] <- pam(x = dados_scale, k = j)
    kmedoids_vscc[[j]] <- pam(x = dados_scale[, col_select], k = j)
  }
  
  d1 <- dist(x = dados_scale)
  cat(paste("relacao", i_rel, "all", "\n"))
  for (ii in 2:n_grupos) print(sil(x = kmeans_all[[ii]]$cluster, d = d1))
  
  cat("\n")
  
  d2 <- dist(x = dados_scale[, col_select])
  cat(paste("relacao", i_rel, "vscc", "\n"))
  for (jj in 2:n_grupos) print(sil(x = kmeans_vscc[[jj]]$cluster, d = d2))
  cat("\n\n\n")
}





str(sil)
names(sil)




# --- TESTE --- #


matriz.dist <- dist(variaveis[[l]]$selected[[i]])
for (j in 1:length(k)) {
  grupo <- kmeans(matriz.dist, k[j])$cluster
  result.prelim[l, j] <- mean(?silhouette(grupo, matriz.dist)[,'sil_width'])
  grupo <- pam(matriz.dist, k[j])$clustering
  result.prelim[l, 2 * j] <- mean(silho





matriz.dist <- dist(dados_scale)
matriz.dist

system.time(pam(x = matriz.dist, k = i))
set.seed(1); a <- kmeans(x = dados_scale, centers = i)
set.seed(1); b <- kmeans(x = matriz.dist, centers = i)

names(a)


d <- data.table(A = a$cluster, B = b$cluster %>% as.integer())
str(d)

d[, TESTE := d[, A] == d[, B]]

d[!which(TESTE)]




silhouette()
pam
#pam(x = dados_scale[, col_select], k = 5)
dbscan



library(mclust)

# --- Incompleto
vscc_ <- vscc(x = dados_scale[, col_ok])
# Show preview of selected variables
head(vscc_$topselected)

# Clustering results on full data set
table(dados_scale[, 1], vscc_$initialrun$classification)

# Clustering results on reduced data set
table(dados_scale[, 1], vscc_$bestmodel$classification)
# ---

data("banknote")
head(banknote)
(bankrun <- vscc(banknote[, -1]))
plot(bankrun)

# Show preview of selected variables
head(bankrun$topselected)

# Clustering results on full data set
table(banknote[,1], bankrun$initialrun$classification)

# Clustering results on reduced data set
table(banknote[,1], bankrun$bestmodel$classification)








