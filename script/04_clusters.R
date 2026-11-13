# ============================================================
# 04_clusters.R  (Unidad 3 — analisis de clusters)
# Objetivo: agrupar paises por su perfil de exposicion a la IA
# y ubicar a Argentina dentro de la tipologia resultante.
# Lee de:   input/paises.csv
# Escribe:  output/tablas/ (perfiles, asignacion) + input para figura
# ============================================================
suppressMessages({library(ggplot2); library(ragg)})
set.seed(2026)

input_dir <- "input"
out_dir   <- file.path("output", "tablas")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

paises <- read.csv(file.path(input_dir, "paises.csv"), stringsAsFactors = FALSE)

# --- 1. variables del perfil de exposicion ---
vars <- c("mean_task_exposure","share_exposed","share_high_exposure",
          "sub_only_share","aug_only_share","both_share")
X <- paises[, vars]
X <- X[complete.cases(X), ]
paises_ok <- paises[complete.cases(paises[, vars]), ]

# --- 2. estandarizacion (k-means es sensible a la escala) ---
Xs <- scale(X)

# --- 3. eleccion de k: metodo del codo (within-cluster SS) ---
wss <- sapply(1:8, function(k) kmeans(Xs, centers = k, nstart = 25)$tot.withinss)
cat("== WSS por k (codo) ==\n"); print(round(wss, 1))

K <- 4  # elegido por el codo (caida marcada hasta 4, luego se aplana)
km <- kmeans(Xs, centers = K, nstart = 50)

# --- 4. perfiles de cluster (medias en la escala original) ---
paises_ok$cluster <- km$cluster
perf <- aggregate(paises_ok[, c(vars,"gdp_per_capita")],
                  by = list(cluster = paises_ok$cluster), FUN = mean)
perf$n <- as.integer(table(paises_ok$cluster))
perf[, -1] <- round(perf[, -1], 3)
cat("\n== Perfiles de cluster (medias) ==\n"); print(perf)
write.csv(perf, file.path(out_dir, "t8_perfiles_cluster.csv"), row.names = FALSE)

# --- 5. asignacion por pais + ubicacion de Argentina ---
asig <- paises_ok[, c("iso3","country_name","region","income_level",
                      "mean_task_exposure","substitution_share","cluster")]
asig <- asig[order(asig$cluster, -asig$mean_task_exposure), ]
write.csv(asig, file.path(out_dir, "t9_asignacion_cluster.csv"), row.names = FALSE)

arg_cl <- asig$cluster[asig$iso3 == "ARG"]
cat(sprintf("\nArgentina -> cluster %d\n", arg_cl))
cat("Otros paises del cluster de Argentina:\n")
print(head(asig$country_name[asig$cluster == arg_cl & asig$iso3 != "ARG"], 15))

# --- 6. figura: proyeccion PCA de los clusters (output/graficos) ---
fam <- "DejaVu Sans"
out_g <- file.path("output", "graficos")
if (!dir.exists(out_g)) dir.create(out_g, recursive = TRUE)

# etiquetas semanticas por cluster (segun exposicion y perfil sust/compl)
cs <- aggregate(cbind(mean_task_exposure, aug_only_share) ~ cluster, paises_ok, FUN = mean)
cs <- cs[order(cs$mean_task_exposure), ]           # de menor a mayor exposicion
lab_map <- setNames(rep(NA_character_, K), as.character(cs$cluster))
lab_map[as.character(cs$cluster[1])] <- "Baja exposición"
lab_map[as.character(cs$cluster[2])] <- "Media-baja"
hi <- cs[3:4, ]                                    # dos de alta exposicion
lab_map[as.character(hi$cluster[which.max(hi$aug_only_share)])] <- "Alta · complemento"
lab_map[as.character(hi$cluster[which.min(hi$aug_only_share)])] <- "Alta · sustitución"
niv <- c("Baja exposición","Media-baja","Alta · complemento","Alta · sustitución")
nconteo <- table(lab_map[as.character(paises_ok$cluster)])
etiqueta <- sprintf("%s (n=%d)", niv, nconteo[niv])
names(etiqueta) <- niv

pca <- prcomp(Xs)
ve  <- round(100 * pca$sdev^2 / sum(pca$sdev^2))
pc1 <- pca$x[,1]; pc2 <- pca$x[,2]
if (cor(pc1, paises_ok$mean_task_exposure) < 0) pc1 <- -pc1  # mayor exposición -> derecha
df  <- data.frame(PC1 = pc1, PC2 = pc2,
                  grupo = factor(etiqueta[lab_map[as.character(paises_ok$cluster)]], levels = etiqueta[niv]),
                  iso3 = paises_ok$iso3)
arg <- df[df$iso3 == "ARG", ]
pal <- setNames(c("#7D8C94","#0072B2","#E69F00","#D55E00"), etiqueta[niv])

g <- ggplot(df, aes(PC1, PC2, colour = grupo)) +
  geom_point(size = 2.6, alpha = 0.85, stroke = 0) +
  geom_point(data = arg, shape = 8, size = 4.2, colour = "#11212E", stroke = 1.1) +
  geom_text(data = arg, aes(label = "Argentina"), colour = "#11212E",
            fontface = "bold", hjust = 1.15, vjust = 0.4, size = 4, family = fam) +
  scale_colour_manual(values = pal, name = NULL) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 3.2))) +
  labs(title = "Los países forman un gradiente de exposición a la IA",
       subtitle = "k-means (k=4) sobre 6 variables de perfil · proyección sobre las 2 primeras componentes",
       x = sprintf("Componente 1 — intensidad de exposición  (%d%%)", ve[1]),
       y = sprintf("Componente 2 — sustitución vs. complemento  (%d%%)", ve[2]),
       caption = "Fuente: elaboración propia en base a Global Automation Atlas (2026).") +
  theme_minimal(base_size = 13, base_family = fam) +
  theme(plot.title = element_text(face = "bold", size = 15, colour = "#11212E"),
        plot.subtitle = element_text(colour = "grey35", size = 10.5, margin = margin(b = 8)),
        plot.caption = element_text(colour = "grey45", hjust = 0, size = 8.5),
        legend.position = "top", legend.text = element_text(size = 10.5),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(), plot.margin = margin(10, 16, 8, 22))
ggsave(file.path(out_g, "clusters_pca.png"), g, device = ragg::agg_png,
       width = 20, height = 13, units = "cm", dpi = 200)

cat("\nOK -> output/tablas/ (t8, t9) · output/graficos/clusters_pca.png\n")
