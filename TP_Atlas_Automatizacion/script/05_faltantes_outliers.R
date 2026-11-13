# ============================================================
# 05_faltantes_outliers.R  (Instancia 2 — sección 2)
# Objetivo: diagnosticar valores faltantes y outliers de las
# variables que entran en los métodos, y documentar decisiones.
# Lee de:   input/
# Escribe:  output/tablas/ (t10 faltantes, t11 outliers)
# ============================================================
input_dir <- "input"; out_dir <- file.path("output","tablas")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

paises <- read.csv(file.path(input_dir,"paises.csv"), stringsAsFactors = FALSE)
arg    <- read.csv(file.path(input_dir,"arg_ocupaciones.csv"), stringsAsFactors = FALSE)

cvars <- c("mean_task_exposure","share_exposed","share_high_exposure",
           "sub_only_share","aug_only_share","both_share")

# --- 1. FALTANTES ---
falt <- data.frame(
  variable = c(cvars, "manufacturing_value_added_pct_gdp","trade_pct_gdp","internet_users_pct",
               "mean_exposure (ocup. ARG)"),
  usada_en_metodo = c(rep("Clustering",6), "no (descriptor)","no (descriptor)","no (descriptor)","Chi-cuadrado"),
  nulos = c(sapply(cvars, function(v) sum(is.na(paises[[v]]))),
            sum(is.na(paises$manufacturing_value_added_pct_gdp)),
            sum(is.na(paises$trade_pct_gdp)),
            sum(is.na(paises$internet_users_pct)),
            sum(is.na(arg$mean_exposure))))
cat("== Faltantes por variable ==\n"); print(falt, row.names = FALSE)
write.csv(falt, file.path(out_dir,"t10_faltantes.csv"), row.names = FALSE)

# --- 2. OUTLIERS (criterio IQR 1.5, adecuado para variables acotadas) ---
iqr_out <- function(x){ q <- quantile(x, c(.25,.75), na.rm=TRUE); i <- q[2]-q[1]
  lo <- q[1]-1.5*i; hi <- q[2]+1.5*i; list(lo=lo, hi=hi, n=sum(x<lo|x>hi, na.rm=TRUE)) }

out_tab <- do.call(rbind, lapply(cvars, function(v){ r <- iqr_out(paises[[v]])
  data.frame(variable=v, nivel="país (cluster)", lim_inf=round(r$lo,3), lim_sup=round(r$hi,3), outliers=r$n) }))
re <- iqr_out(arg$mean_exposure)
out_tab <- rbind(out_tab, data.frame(variable="mean_exposure", nivel="ocupación ARG (Chi²)",
                 lim_inf=round(re$lo,3), lim_sup=round(re$hi,3), outliers=re$n))
cat("\n== Outliers por variable (IQR 1.5) ==\n"); print(out_tab, row.names = FALSE)
write.csv(out_tab, file.path(out_dir,"t11_outliers.csv"), row.names = FALSE)

# ocupaciones extremas (genuinas, se mantienen)
ext <- arg[arg$mean_exposure < re$lo | arg$mean_exposure > re$hi, c("occupation_title","mean_exposure")]
cat("\nOcupaciones outlier (ARG) — valores genuinos, se mantienen:\n")
print(ext[order(ext$mean_exposure),], row.names = FALSE)

cat("\nOK -> output/tablas/ (t10, t11)\n")
