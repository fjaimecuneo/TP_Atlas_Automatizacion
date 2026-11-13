# ============================================================
# 10_descriptivas_limpieza.R  (Entrega final — faltantes y outliers)
#  Diagnóstico de faltantes · outliers (IQR×1,5) · descriptivas
#  ANTES y DESPUÉS de excluir outliers (decisión: se mantienen).
# Lee de:  input/arg_ocupaciones.csv · input/paises.csv
# Escribe: output/tablas/t18_faltantes.csv, t19_descriptivas_antes_despues.csv
#          output/graficos/faltantes_outliers.jpg
# ============================================================
suppressMessages({library(dplyr); library(readr); library(tidyr)
                  library(ggplot2); library(scales); library(ragg)})
FAM <- "DejaVu Sans"
out_t <- file.path("output","tablas"); out_g <- file.path("output","graficos")
a <- read_csv("input/arg_ocupaciones.csv", show_col_types = FALSE)
p <- read_csv("input/paises.csv", show_col_types = FALSE)

# ---------- (1) FALTANTES ----------
vars_a <- c("mean_exposure","substitution_share","augmentation_share","task_count",
            "sub_only_share","aug_only_share","both_share")
vars_p <- c("mean_task_exposure","share_exposed","share_high_exposure",
            "sub_only_share","aug_only_share","both_share","gdp_per_capita")
falt <- bind_rows(
  tibble(base="Ocupaciones ARG (923)", variable=vars_a,
         faltantes=sapply(vars_a, function(v) sum(is.na(a[[v]])))),
  tibble(base="Países (124)", variable=vars_p,
         faltantes=sapply(vars_p, function(v) sum(is.na(p[[v]]))))
)
cat("== FALTANTES en las variables usadas ==\n"); print(as.data.frame(falt), row.names=FALSE)
write_csv(falt, file.path(out_t,"t18_faltantes.csv"))

# ---------- (2) OUTLIERS (IQR×1,5) ----------
fences <- function(x){ q <- quantile(x, c(.25,.75), na.rm=TRUE); i <- q[2]-q[1]
  c(lo=unname(q[1]-1.5*i), hi=unname(q[2]+1.5*i)) }
fe <- fences(a$mean_exposure)
a$out <- a$mean_exposure < fe["lo"] | a$mean_exposure > fe["hi"]
cat(sprintf("\n== OUTLIERS en exposición de ocupaciones ARG (IQR×1,5) ==\n"))
cat(sprintf("Bigotes: [%.2f ; %.2f] · outliers: %d de %d\n", fe["lo"], fe["hi"], sum(a$out), nrow(a)))
print(a %>% filter(out) %>% transmute(occupation_title, mean_exposure=round(mean_exposure,2)) %>%
        arrange(desc(mean_exposure)) %>% as.data.frame(), row.names=FALSE)

# ---------- (3) DESCRIPTIVAS ANTES / DESPUÉS ----------
desc <- function(x){ c(n=length(x), media=mean(x), sd=sd(x),
  mediana=median(x), min=min(x), max=max(x), p25=quantile(x,.25,names=FALSE),
  p75=quantile(x,.75,names=FALSE)) }
antes   <- desc(a$mean_exposure)
despues <- desc(a$mean_exposure[!a$out])
tab <- as.data.frame(round(rbind(Antes=antes, Despues=despues), 3))
tab$delta_media_pct <- NA; tab["Despues","delta_media_pct"] <- round(100*(despues["media"]-antes["media"])/antes["media"],2)
cat("\n== DESCRIPTIVAS de la exposición (ARG) — antes vs. después de excluir outliers ==\n")
print(tab)
cat(sprintf("La media cambia %.2f%% y la mediana %.3f -> %.3f: la decisión es MANTENERLOS.\n",
            tab["Despues","delta_media_pct"], antes["mediana"], despues["mediana"]))
write_csv(tibble::rownames_to_column(tab, "muestra"), file.path(out_t,"t19_descriptivas_antes_despues.csv"))

# ---------- (4) GRÁFICO: boxplot con outliers marcados ----------
etq <- a %>% filter(out) %>% arrange(desc(mean_exposure)) %>% slice(1)
g <- ggplot(a, aes(x = mean_exposure, y = "")) +
  geom_boxplot(width = 0.5, outlier.shape = NA, fill = "#EDF1F3", colour = "grey45") +
  geom_jitter(aes(colour = out), height = 0.18, width = 0, size = 1.4, alpha = 0.5, stroke = 0) +
  geom_vline(xintercept = fe["hi"], linetype = "dashed", colour = "#D55E00") +
  annotate("text", x = fe["hi"] - 0.05, y = 1.48, label = sprintf("Bigote superior = %.2f", fe["hi"]),
           hjust = 1, vjust = 1, size = 3.5, colour = "#9A3A00", family = FAM) +
  annotate("text", x = fe["hi"] - 0.05, y = 0.58,
           label = sprintf("%d outliers altos\n(valores genuinos: se mantienen)", sum(a$out)),
           hjust = 1, size = 3.5, colour = "grey25", family = FAM) +
  scale_colour_manual(values = c("FALSE"="grey70","TRUE"="#D55E00"), guide = "none") +
  scale_x_continuous(limits = c(0,3), breaks = 0:3) +
  labs(title = "Los outliers de exposición son pocos y genuinos: se mantienen",
       subtitle = "923 ocupaciones de Argentina · criterio de outlier: rango intercuartílico × 1,5",
       x = "Exposición media a la IA por ocupación  (0 – 3)", y = NULL,
       caption = "Fuente: elaboración propia en base a Global Automation Atlas (2026). Excluirlos cambia la media < 2%.") +
  theme_minimal(base_size = 13, base_family = FAM) +
  theme(plot.title = element_text(face="bold", size=15, colour="#11212E"),
        plot.subtitle = element_text(colour="grey35", size=11, margin=margin(b=8)),
        plot.caption = element_text(colour="grey45", hjust=0, size=8.5),
        axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = margin(12,16,8,12))
ggsave(file.path(out_g,"faltantes_outliers.jpg"), g, device = agg_jpeg,
       width = 24, height = 12, units = "cm", dpi = 300)
cat("\nOK -> t18, t19, faltantes_outliers.jpg\n")
