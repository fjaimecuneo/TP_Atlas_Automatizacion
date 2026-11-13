# utils/gini.R
# Coeficiente de Gini para un vector numerico no negativo.
# Uso: gini(x) -> valor en [0, 1). Mide concentracion/desigualdad.
gini <- function(x) {
  x <- sort(as.numeric(x[!is.na(x)]))
  n <- length(x)
  if (n == 0 || sum(x) == 0) return(0)
  (2 * sum(seq_len(n) * x) / (n * sum(x))) - (n + 1) / n
}
