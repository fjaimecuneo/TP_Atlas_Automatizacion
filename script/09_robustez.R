# ============================================================
# 09_robustez.R  (Entrega final — chequeos que pidió el docente)
#  (a) Chi-cuadrado con TERCILES (en vez de cortes fijos)
#  (b) CLUSTERING: silueta para elegir k, y estabilidad con/sin outliers
#  (c) Descriptivas por cluster
# Lee de:  input/arg_ocupaciones.csv · input/paises.csv
# Escribe: output/tablas/t14_chi_terciles.csv, t15_silueta.csv,
#          t16_descriptivas_cluster.csv, t17_cluster_sin_outliers.csv
# ============================================================
suppressMessages({library(dplyr); library(readr); library(cluster)})
set.seed(2026)
out <- file.path("output","tablas")
a <- read_csv("input/arg_ocupaciones.csv", show_col_types = FALSE)
p <- read_csv("input/paises.csv", show_col_types = FALSE)

fmap <- c("11"="Dirección","13"="Dirección","15"="Ciencia","17"="Ciencia","19"="Ciencia",
 "21"="Social/salud","23"="Social/salud","25"="Social/salud","27"="Social/salud","29"="Social/salud",
 "31"="Servicios","33"="Servicios","35"="Servicios","37"="Servicios","39"="Servicios",
 "41"="Ventas/admin","43"="Ventas/admin","45"="Manual","47"="Manual","49"="Manual","51"="Manual","53"="Manual")
a <- a %>% mutate(familia = fmap[substr(onet_soc_code,1,2)]) %>% filter(!is.na(familia))

cramer <- function(tab){ ch <- suppressWarnings(chisq.test(tab))
  list(X2=unname(ch$statistic), gl=unname(ch$parameter), p=ch$p.value,
       V=sqrt(unname(ch$statistic)/(sum(tab)*(min(dim(tab))-1)))) }

# ---------- (a) Chi-cuadrado con TERCILES ----------
br <- quantile(a$mean_exposure, c(0,1/3,2/3,1))
a$tercil <- cut(a$mean_exposure, br, include.lowest=TRUE, labels=c("Bajo","Medio","Alto"))
tt <- table(a$familia, a$tercil); rt <- cramer(tt)
cat(sprintf("(a) Chi² TERCILES: X2=%.1f · gl=%d · p=%.2g · V=%.3f\n", rt$X2, rt$gl, rt$p, rt$V))
cat("    (comparar con cortes fijos de la Instancia 2: X2=356.5 · V=0.44)\n")
cat(sprintf("    puntos de corte de los terciles: %.2f y %.2f\n", br[2], br[3]))
write_csv(as.data.frame.matrix(tt) %>% tibble::rownames_to_column("familia"),
          file.path(out,"t14_chi_terciles.csv"))

# ---------- (b) CLUSTERING: silueta + estabilidad ----------
vars <- c("mean_task_exposure","share_exposed","share_high_exposure","sub_only_share","aug_only_share","both_share")
X <- scale(p[vars]); D <- dist(X)
sil <- sapply(2:8, function(k){ km <- kmeans(X, k, nstart=25); mean(silhouette(km$cluster, D)[,3]) })
wss <- sapply(1:8, function(k) kmeans(X, k, nstart=25)$tot.withinss)
cat("\n(b) Silueta promedio por k (2..8):\n"); print(round(setNames(sil,2:8),3))
cat("    WSS (codo) por k (1..8):\n"); print(round(setNames(wss,1:8),1))
write_csv(tibble(k=2:8, silueta=round(sil,3), wss=round(wss[2:8],1)), file.path(out,"t15_silueta.csv"))

# k = 4 (elegido). descriptivas por cluster
km4 <- kmeans(X, 4, nstart=50); p$cl <- km4$cluster
desc <- p %>% group_by(cl) %>%
  summarise(n=n(), exp_media=mean(mean_task_exposure), share_exp=mean(share_exposed),
            sub=mean(sub_only_share), aug=mean(aug_only_share),
            pbi_pc=mean(gdp_per_capita), .groups="drop") %>%
  mutate(across(c(exp_media,share_exp,sub,aug), ~round(.,3)), pbi_pc=round(pbi_pc))
cat("\n(c) Descriptivas por cluster (k=4):\n"); print(as.data.frame(desc))
arg_cl <- p$cl[p$iso3=="ARG"]; cat(sprintf("    Argentina -> cluster %d (n=%d)\n", arg_cl, sum(p$cl==arg_cl)))
write_csv(desc, file.path(out,"t16_descriptivas_cluster.csv"))

# estabilidad: sacar outliers y re-clusterizar.
# Mismo criterio que la Instancia 2 (IQR x1.5), aplicado a las 6 variables:
# un país es outlier si lo es en al menos una variable de cluster.
iqr_out <- function(x){ q <- quantile(x, c(.25,.75)); i <- q[2]-q[1]
  x < q[1]-1.5*i | x > q[2]+1.5*i }
outl <- apply(sapply(p[vars], iqr_out), 1, any)
cat(sprintf("\n    Outliers (IQR x1.5, en alguna variable): %d países -> %s\n", sum(outl),
            paste(p$country_name[outl], collapse=", ")))
Xo <- X[!outl,]; km4o <- kmeans(Xo, 4, nstart=50)
po <- p[!outl,]; po$cl2 <- km4o$cluster
arg_cl2 <- po$cl2[po$iso3=="ARG"]
# ¿los compañeros de cluster de ARG se mantienen?
comp_full <- sort(p$country_name[p$cl==arg_cl & p$iso3!="ARG"])
comp_sin  <- sort(po$country_name[po$cl2==arg_cl2 & po$iso3!="ARG"])
estab <- round(length(intersect(comp_full, comp_sin))/length(union(comp_full, comp_sin)),3)
cat(sprintf("    Estabilidad del cluster de ARG (Jaccard con/sin outliers): %.2f\n", estab))
write_csv(tibble(pais=po$country_name, cluster_sin_outliers=po$cl2), file.path(out,"t17_cluster_sin_outliers.csv"))
cat("\nOK -> t14..t17 en output/tablas/\n")
