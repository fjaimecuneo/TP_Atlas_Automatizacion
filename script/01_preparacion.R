# ============================================================
# 01_preparacion.R
# Objetivo: cargar los datos crudos del Global Automation Atlas,
# validarlos y construir las bases de trabajo en input/.
# Lee de:   raw/
# Escribe:  input/
# ============================================================

# --- rutas (raiz del proyecto) ---
raw_dir   <- "raw"
input_dir <- "input"
if (!dir.exists(input_dir)) dir.create(input_dir, recursive = TRUE)

# --- 1. carga ---
paises <- read.csv(file.path(raw_dir, "countries.csv"),
                   stringsAsFactors = FALSE, check.names = FALSE)
ocup   <- read.csv(file.path(raw_dir, "country-occupation-panel.csv"),
                   stringsAsFactors = FALSE, check.names = FALSE)

cat("Base pais (complementaria):", nrow(paises), "filas x", ncol(paises), "columnas\n")
cat("Base pais-ocupacion (principal):", nrow(ocup), "filas x", ncol(ocup), "columnas\n")
cat("Paises en el panel:", length(unique(ocup$iso3)),
    "| ocupaciones por pais (mediana):", median(table(ocup$iso3)), "\n")

# --- 2. validaciones minimas ---
stopifnot("ARG" %in% paises$iso3)                 # Argentina presente
stopifnot(all(ocup$mean_exposure >= 0 & ocup$mean_exposure <= 3))  # escala 0-3

# --- 3. tipado explicito (documenta la naturaleza de cada variable) ---
paises$income_level <- factor(
  paises$income_level,
  levels = c("Low income", "Lower middle income",
             "Upper middle income", "High income"),
  ordered = TRUE)                                 # ORDINAL
paises$region <- factor(paises$region)            # categorica nominal

# --- 4. bases de trabajo ---
# 4a. panel pais-ocupacion (principal), con grupo ocupacional (2 primeros digitos SOC)
ocup$grupo_ocup <- substr(ocup$onet_soc_code, 1, 2)
# 4b. subconjunto Argentina para el analisis focal
arg_ocup <- ocup[ocup$iso3 == "ARG", ]

# --- 5. guardar en input/ ---
write.csv(paises,   file.path(input_dir, "paises.csv"),          row.names = FALSE)
write.csv(ocup,     file.path(input_dir, "pais_ocupacion.csv"),  row.names = FALSE)
write.csv(arg_ocup, file.path(input_dir, "arg_ocupaciones.csv"), row.names = FALSE)

cat("\nOK -> input/paises.csv, input/pais_ocupacion.csv, input/arg_ocupaciones.csv\n")
