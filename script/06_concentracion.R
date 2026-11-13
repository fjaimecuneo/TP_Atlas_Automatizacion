# ============================================================
# 06_concentracion.R  (Instancia 2 — método cuantitativo: índice)
# [DEPRECADO en la entrega final] Medía la concentración de la
# exposición con Gini y HHI/CR2 por familia. Se descartó por
# indicación del docente: el Gini aplicado a un puntaje ordinal de
# exposición (no a una magnitud monetaria o de stock) no es una
# medida de concentración bien definida. Se conserva por trazabilidad;
# NO alimenta el deck final (su lugar lo toma 08 y 09).
# Lee de:   input/arg_ocupaciones.csv   ·   utils/gini.R
# Escribe:  output/tablas/t12_concentracion.csv
# ============================================================
source(file.path("utils","gini.R"))
input_dir <- "input"; out_dir <- file.path("output","tablas")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
arg <- read.csv(file.path(input_dir,"arg_ocupaciones.csv"), stringsAsFactors = FALSE)

# --- Gini de la exposición entre las 923 ocupaciones ---
g <- gini(arg$mean_exposure)

# --- Concentración de la exposición ALTA (>=2) por familia ocupacional ---
soc2 <- substr(arg$onet_soc_code,1,2)
fmap <- c("11"="Dirección y negocios","13"="Dirección y negocios",
 "15"="Ciencia y tecnología","17"="Ciencia y tecnología","19"="Ciencia y tecnología",
 "21"="Prof. social/salud/educ.","23"="Prof. social/salud/educ.","25"="Prof. social/salud/educ.",
 "27"="Prof. social/salud/educ.","29"="Prof. social/salud/educ.",
 "31"="Servicios","33"="Servicios","35"="Servicios","37"="Servicios","39"="Servicios",
 "41"="Ventas y administración","43"="Ventas y administración",
 "45"="Manual y producción","47"="Manual y producción","49"="Manual y producción",
 "51"="Manual y producción","53"="Manual y producción")
arg$familia <- fmap[soc2]
alto <- arg[arg$mean_exposure >= 2, ]
sh <- sort(prop.table(table(alto$familia)), decreasing = TRUE)
hhi <- sum(sh^2)                 # índice de Herfindahl-Hirschman (0-1)
cr2 <- sum(head(sh,2))           # participación de las 2 familias top

cat(sprintf("Gini de la exposición (923 ocupaciones): %.3f\n", g))
cat(sprintf("Ocupaciones de exposición Alta (>=2): %d\n", nrow(alto)))
cat("Participación por familia entre las Alto:\n"); print(round(sh*100,1))
cat(sprintf("HHI (familias | Alto) = %.3f  ·  CR2 = %.1f%%\n", hhi, 100*cr2))

res <- data.frame(indicador=c("Gini exposición (ocupaciones)","HHI familias | exposición Alta","CR2 familias | exposición Alta"),
                  valor=c(round(g,3), round(hhi,3), round(cr2,3)))
write.csv(res, file.path(out_dir,"t12_concentracion.csv"), row.names = FALSE)
cat("\nOK -> output/tablas/t12_concentracion.csv\n")
