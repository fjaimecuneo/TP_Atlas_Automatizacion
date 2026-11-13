# ============================================================
# 02_descriptivas.R
# Objetivo: reproducir TODAS las estadisticas descriptivas de la
# Instancia 1 (las que aparecen en la presentacion).
# Lee de:   input/
# Escribe:  output/tablas/
# ============================================================

# --- funciones propias ---
source(file.path("utils", "gini.R"))
source(file.path("utils", "resumen_numerico.R"))

# --- rutas ---
input_dir <- "input"
out_dir   <- file.path("output", "tablas")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

paises   <- read.csv(file.path(input_dir, "paises.csv"), stringsAsFactors = FALSE)
arg_ocup <- read.csv(file.path(input_dir, "arg_ocupaciones.csv"), stringsAsFactors = FALSE)

orden_ingreso <- c("Low income", "Lower middle income",
                   "Upper middle income", "High income")
paises$income_level <- factor(paises$income_level, levels = orden_ingreso, ordered = TRUE)

# ------------------------------------------------------------
# T1. Gradiente por nivel de ingreso  (slide 5)
# ------------------------------------------------------------
t1 <- aggregate(cbind(mean_task_exposure, share_exposed,
                      substitution_share, augmentation_share) ~ income_level,
                data = paises, FUN = mean)
t1[ , -1] <- round(t1[ , -1], 3)
t1$n <- as.integer(table(paises$income_level)[t1$income_level])
cat("== T1. Exposicion media por nivel de ingreso ==\n"); print(t1)
write.csv(t1, file.path(out_dir, "t1_gradiente_ingreso.csv"), row.names = FALSE)

# correlaciones (slide 5)
c_exp <- round(cor(paises$mean_task_exposure, paises$log_gdp_per_capita), 3)
c_gap <- round(cor(paises$substitution_minus_augmentation, paises$log_gdp_per_capita), 3)
cat("\ncor(exposicion media, log PBI pc) =", c_exp, "\n")
cat("cor(sustitucion - complemento, log PBI pc) =", c_gap, "\n")

# ------------------------------------------------------------
# T2. Argentina vs America Latina vs Mundo  (slide 6)
# ------------------------------------------------------------
lat <- paises[paises$region == "Latin America & Caribbean", ]
arg <- paises[paises$iso3 == "ARG", ]
metricas <- c("mean_task_exposure", "share_exposed",
              "substitution_share", "augmentation_share")
t2 <- data.frame(
  indicador = metricas,
  ARG   = round(as.numeric(arg[1, metricas]), 3),
  LatAm = round(sapply(lat[metricas], mean), 3),
  Mundo = round(sapply(paises[metricas], mean), 3),
  row.names = NULL)
rank_arg <- sum(paises$mean_task_exposure > arg$mean_task_exposure) + 1
cat("\n== T2. Argentina vs LatAm vs Mundo ==\n"); print(t2)
cat("Ranking de Argentina por exposicion media:", rank_arg, "de", nrow(paises), "\n")
write.csv(t2, file.path(out_dir, "t2_arg_vs_region_mundo.csv"), row.names = FALSE)

# ------------------------------------------------------------
# T3. Distribucion de la exposicion entre ocupaciones de ARG  (slide 7)
# ------------------------------------------------------------
t3 <- resumen_numerico(arg_ocup$mean_exposure)
t3$gini <- round(gini(arg_ocup$mean_exposure), 3)
cat("\n== T3. Exposicion por ocupacion (Argentina) ==\n"); print(t3)
write.csv(t3, file.path(out_dir, "t3_dist_ocupaciones_arg.csv"), row.names = FALSE)

# ------------------------------------------------------------
# T4. Ocupaciones mas y menos expuestas en ARG  (slide 7)
# ------------------------------------------------------------
cols   <- c("occupation_title", "mean_exposure")
top    <- head(arg_ocup[order(-arg_ocup$mean_exposure), cols], 6)
bottom <- head(arg_ocup[order( arg_ocup$mean_exposure), cols], 6)
top$mean_exposure    <- round(top$mean_exposure, 2)
bottom$mean_exposure <- round(bottom$mean_exposure, 2)
cat("\n== T4a. Mas expuestas (ARG) ==\n"); print(top, row.names = FALSE)
cat("\n== T4b. Menos expuestas (ARG) ==\n"); print(bottom, row.names = FALSE)
write.csv(top,    file.path(out_dir, "t4a_top_expuestas_arg.csv"),    row.names = FALSE)
write.csv(bottom, file.path(out_dir, "t4b_menos_expuestas_arg.csv"), row.names = FALSE)

cat("\nOK -> tablas en output/tablas/\n")
