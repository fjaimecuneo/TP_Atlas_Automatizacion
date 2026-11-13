# utils/resumen_numerico.R
# Resumen descriptivo de una variable numerica.
# Uso: resumen_numerico(x) -> data.frame de una fila (n, media, sd, min, q25, mediana, q75, max).
resumen_numerico <- function(x) {
  x <- as.numeric(x[!is.na(x)])
  data.frame(
    n       = length(x),
    media   = round(mean(x), 3),
    sd      = round(sd(x), 3),
    min     = round(min(x), 3),
    q25     = round(quantile(x, 0.25), 3),
    mediana = round(median(x), 3),
    q75     = round(quantile(x, 0.75), 3),
    max     = round(max(x), 3),
    row.names = NULL
  )
}
