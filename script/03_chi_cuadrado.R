# ============================================================
# 03_chi_cuadrado.R  (Unidad 2 — test de independencia)
# Objetivo: testear si el nivel de exposicion a la IA es
# independiente del grupo ocupacional (Argentina).
# Lee de:   input/arg_ocupaciones.csv
# Escribe:  output/tablas/  (contingencia, resultado, residuos)
# ============================================================

input_dir <- "input"
out_dir   <- file.path("output", "tablas")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

arg <- read.csv(file.path(input_dir, "arg_ocupaciones.csv"), stringsAsFactors = FALSE)

# --- 1. variables categoricas ---
# 1a. Familia ocupacional a partir del grupo mayor SOC (2 primeros digitos)
arg$soc2 <- substr(arg$onet_soc_code, 1, 2)
familia_map <- c(
  "11"="Dirección y negocios","13"="Dirección y negocios",
  "15"="Ciencia y tecnología","17"="Ciencia y tecnología","19"="Ciencia y tecnología",
  "21"="Prof. social/salud/educ.","23"="Prof. social/salud/educ.","25"="Prof. social/salud/educ.",
  "27"="Prof. social/salud/educ.","29"="Prof. social/salud/educ.",
  "31"="Servicios","33"="Servicios","35"="Servicios","37"="Servicios","39"="Servicios",
  "41"="Ventas y administración","43"="Ventas y administración",
  "45"="Manual y producción","47"="Manual y producción","49"="Manual y producción",
  "51"="Manual y producción","53"="Manual y producción")
arg$familia <- familia_map[arg$soc2]
arg <- arg[!is.na(arg$familia), ]

# 1b. Tramo de exposicion (ordinal) sobre la escala 0-3
arg$tramo <- cut(arg$mean_exposure, breaks = c(-Inf, 1, 2, Inf),
                 labels = c("Bajo (<1)","Medio (1–2)","Alto (≥2)"))

# --- 2. tabla de contingencia ---
tab <- table(arg$familia, arg$tramo)
cat("== Tabla de contingencia (familia x tramo) ==\n"); print(tab)
write.csv(as.data.frame.matrix(tab), file.path(out_dir, "t5_contingencia.csv"))

# --- 3. supuestos: frecuencias esperadas ---
esp <- chisq.test(tab)$expected
prop_bajas <- mean(esp < 5)
cat(sprintf("\nCeldas con frecuencia esperada < 5: %.1f%% (min esperada = %.2f)\n",
            100*prop_bajas, min(esp)))

# --- 4. test Chi-cuadrado (con simulacion si hay celdas ralas) ---
usar_sim <- prop_bajas > 0.20
chi <- chisq.test(tab, simulate.p.value = usar_sim, B = 10000)
n <- sum(tab); k <- min(dim(tab))
cramer_v <- sqrt(as.numeric(chi$statistic) / (n * (k - 1)))
cat(sprintf("\n== Resultado Chi-cuadrado ==\nX2 = %.1f | gl = %s | p = %.3g%s\nCramér's V = %.3f (n = %d)\n",
            chi$statistic, ifelse(is.na(chi$parameter),"(simulado)",chi$parameter),
            chi$p.value, ifelse(usar_sim," [p simulado por Monte Carlo]",""), cramer_v, n))

res_df <- data.frame(estadistico="Chi-cuadrado de Pearson",
  X2=round(as.numeric(chi$statistic),2),
  gl=ifelse(is.na(chi$parameter),NA,as.numeric(chi$parameter)),
  p_valor=signif(chi$p.value,3),
  cramer_v=round(cramer_v,3), n=n,
  metodo=ifelse(usar_sim,"Monte Carlo (B=10000)","asintótico"))
write.csv(res_df, file.path(out_dir, "t6_chi_resultado.csv"), row.names = FALSE)

# --- 5. residuos estandarizados (donde se concentra la dependencia) ---
stdres <- round(chi$stdres, 2)
cat("\n== Residuos estandarizados (|>2| = celda relevante) ==\n"); print(stdres)
write.csv(as.data.frame.matrix(stdres), file.path(out_dir, "t7_residuos.csv"))

cat("\nOK -> output/tablas/ (t5, t6, t7)\n")
