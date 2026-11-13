# ============================================================
# 07_graficos_instancia3.R  (Instancia 3 — visualización)
# Dos gráficos en ggplot2: uno exploratorio y uno comunicativo,
# sobre el hallazgo robusto (dentro de Argentina): la exposición
# a la IA se concentra en la familia "Ventas y administración".
# Lee de:   input/arg_ocupaciones.csv
# Escribe:  output/graficos/grupo_09_exploratorio.jpg
#           output/graficos/grupo_09_comunicativo.jpg
# ============================================================
suppressMessages({library(ggplot2); library(dplyr); library(readr)
                  library(scales); library(ggrepel); library(ragg)})
FAM <- "DejaVu Sans"

out <- file.path("output","graficos"); if (!dir.exists(out)) dir.create(out, recursive = TRUE)
a <- read_csv("input/arg_ocupaciones.csv", show_col_types = FALSE)

fmap <- c("11"="Dirección y negocios","13"="Dirección y negocios",
 "15"="Ciencia y tecnología","17"="Ciencia y tecnología","19"="Ciencia y tecnología",
 "21"="Prof. social/salud/educ.","23"="Prof. social/salud/educ.","25"="Prof. social/salud/educ.",
 "27"="Prof. social/salud/educ.","29"="Prof. social/salud/educ.",
 "31"="Servicios","33"="Servicios","35"="Servicios","37"="Servicios","39"="Servicios",
 "41"="Ventas y administración","43"="Ventas y administración",
 "45"="Manual y producción","47"="Manual y producción","49"="Manual y producción",
 "51"="Manual y producción","53"="Manual y producción")
a <- a %>% mutate(soc2 = substr(onet_soc_code,1,2),
                  familia = fmap[soc2],
                  neto = substitution_share - augmentation_share) %>%
           filter(!is.na(familia))

pal <- c("Ventas y administración"="#D55E00","Dirección y negocios"="#0072B2",
         "Manual y producción"="#E69F00","Ciencia y tecnología"="#009E73",
         "Servicios"="#56B4E9","Prof. social/salud/educ."="#CC79A7")
mx <- median(a$mean_exposure); my <- median(a$neto)

# ---------- GRÁFICO 1 — EXPLORATORIO ----------
g_exp <- ggplot(a, aes(mean_exposure, neto)) +
  annotate("rect", xmin = mx, xmax = Inf, ymin = my, ymax = Inf, fill = "#D55E00", alpha = 0.05) +
  geom_hline(yintercept = my, linetype = "dashed", colour = "grey55") +
  geom_vline(xintercept = mx, linetype = "dashed", colour = "grey55") +
  geom_point(aes(colour = familia, size = task_count), alpha = 0.72, stroke = 0) +
  annotate("text", x = 2.95, y = 1.02, hjust = 1,
           label = "Más expuestas y de sustitución", fontface = "italic",
           size = 3.4, colour = "grey30") +
  scale_colour_manual(values = pal, name = "Familia ocupacional") +
  scale_size_continuous(range = c(1, 5.5), name = "Nº de tareas") +
  scale_x_continuous(limits = c(0,3), breaks = 0:3) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, decimal.mark = ",")) +
  labs(
    title = "Exposición a la IA y perfil sustitución–complemento, por familia ocupacional",
    subtitle = "Cada punto es una ocupación · 923 ocupaciones O*NET · Argentina",
    x = "Exposición media a la automatización por IA  (0 = nula · 3 = extensa)",
    y = "Sustitución menos complemento  ( + sustituye · − complementa )",
    caption = "Fuente: elaboración propia en base a Global Automation Atlas (2026). Exposición potencial; sin ponderar por empleo."
  ) +
  guides(colour = guide_legend(override.aes = list(size = 4, alpha = 1), order = 1)) +
  theme_minimal(base_size = 13, base_family = FAM) +
  theme(plot.title = element_text(face = "bold", size = 14.5),
        plot.subtitle = element_text(colour = "grey35", margin = margin(b = 8)),
        plot.caption = element_text(colour = "grey45", hjust = 0, size = 8.5),
        legend.position = "right", panel.grid.minor = element_blank(),
        plot.margin = margin(10, 14, 8, 12))

ggsave(file.path(out,"grupo_09_exploratorio.jpg"), g_exp, device = agg_jpeg,
       width = 24, height = 15, units = "cm", dpi = 300)

# ---------- GRÁFICO 2 — COMUNICATIVO ----------
ord <- a %>% group_by(familia) %>% summarise(med = median(mean_exposure)) %>% arrange(med)
a <- a %>% mutate(familia = factor(familia, levels = ord$familia),
                  destacar = familia == "Ventas y administración")
meds <- a %>% group_by(familia) %>% summarise(med = median(mean_exposure))
etq <- a %>% filter(occupation_title %in% c("Bookkeeping, Accounting, and Auditing Clerks","Clergy")) %>%
  mutate(lab = ifelse(occupation_title=="Clergy","Clero (0,04)","Contabilidad y auditoría (2,9)"),
         ny = ifelse(occupation_title=="Clergy", 0.45, -0.45))

set.seed(9)
g_com <- ggplot(a, aes(mean_exposure, familia)) +
  geom_jitter(aes(colour = destacar), height = 0.28, width = 0, size = 1.5, alpha = 0.55, stroke = 0) +
  geom_point(data = meds, aes(med, familia), shape = 124, size = 7, colour = "grey20") +
  geom_text(data = meds, aes(med, familia, label = number(med, accuracy = 0.1, decimal.mark = ",")),
            vjust = -1.5, size = 3.6, colour = "grey20", fontface = "bold", family = FAM) +
  geom_text_repel(data = etq, aes(mean_exposure, familia, label = lab),
                  size = 3.3, colour = "grey25", family = FAM, nudge_y = etq$ny,
                  segment.colour = "grey55", min.segment.length = 0, seed = 9, box.padding = 0.5) +
  scale_colour_manual(values = c("TRUE"="#D55E00","FALSE"="grey72"), guide = "none") +
  scale_x_continuous(limits = c(0,3.1), breaks = 0:3) +
  labs(
    title = "Las tareas administrativas y de ventas son, por lejos,\nlas más expuestas a la IA en Argentina",
    subtitle = "923 ocupaciones de Argentina · exposición potencial a la automatización (0 a 3) · barra = mediana",
    x = "Exposición media a la IA por ocupación  (0 – 3)",
    y = NULL,
    caption = "Fuente: elaboración propia en base a Global Automation Atlas (2026) · sin ponderar por empleo."
  ) +
  theme_minimal(base_size = 13, base_family = FAM) +
  theme(plot.title = element_text(face = "bold", size = 15, colour = "#9A3A00", lineheight = 1.1),
        plot.subtitle = element_text(colour = "grey35", margin = margin(t = 2, b = 12), size = 10.5),
        plot.caption = element_text(colour = "grey45", hjust = 0, size = 8.5),
        axis.text.y = element_text(size = 11.5, colour = "grey15"),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = margin(12, 16, 8, 10))

ggsave(file.path(out,"grupo_09_comunicativo.jpg"), g_com, device = agg_jpeg,
       width = 24, height = 14.5, units = "cm", dpi = 300)

cat("OK -> grupo_09_exploratorio.jpg · grupo_09_comunicativo.jpg\n")
