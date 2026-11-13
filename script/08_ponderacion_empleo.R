# ============================================================
# 08_ponderacion_empleo.R  (Entrega final — prioridad del docente)
# Pondera la exposición por el empleo real de Argentina.
# Las 923 ocupaciones O*NET se mapean a los 9 grandes grupos
# ISCO-08, y se ponderan por el empleo de cada grupo (ILOSTAT).
# Lee de:   input/arg_ocupaciones.csv
# Escribe:  output/tablas/t13_ponderacion_empleo.csv
#           output/graficos/empleo_vs_exposicion.jpg
# Limitación: el puente SOC->ISCO a 1 dígito es aproximado.
# ============================================================
suppressMessages({library(dplyr); library(readr); library(ggplot2); library(ggrepel); library(scales); library(ragg)})
FAM <- "DejaVu Sans"
out_t <- file.path("output","tablas"); out_g <- file.path("output","graficos")
a <- read_csv("input/arg_ocupaciones.csv", show_col_types = FALSE)
a$soc2 <- substr(a$onet_soc_code,1,2)

# puente SOC-2díg -> ISCO-08 1 díg (aproximado, documentado)
s2i <- c("11"="1","13"="2","15"="2","17"="2","19"="2","21"="2","23"="2","25"="2","27"="2",
         "29"="3","31"="5","33"="5","35"="5","37"="9","39"="5","41"="5","43"="4",
         "45"="6","47"="7","49"="7","51"="8","53"="8","55"="0")
# base auxiliar: empleo por grupo ISCO-08, Argentina ~2025, en miles (Fuente: ILOSTAT / INDEC)
aux <- read_csv("auxiliar/empleo_isco_arg.csv", show_col_types = FALSE)
aux$isco1 <- as.character(aux$isco1)
lab <- setNames(aux$grupo, aux$isco1)       # etiqueta de cada grupo ISCO-08
emp <- setNames(aux$empleo_miles, aux$isco1) # empleo (miles) de cada grupo

a$isco <- s2i[a$soc2]
g <- a %>% filter(isco != "0") %>% group_by(isco) %>%
  summarise(exposicion = mean(mean_exposure), n_ocup = n(), .groups="drop") %>%
  mutate(grupo = lab[isco], empleo_mil = emp[isco]) %>%
  mutate(peso = empleo_mil / sum(empleo_mil))

sin_pond <- mean(a$mean_exposure)
con_pond <- sum(g$exposicion * g$peso)
cat(sprintf("Exposición media SIN ponderar (por ocupación): %.3f\n", sin_pond))
cat(sprintf("Exposición media PONDERADA por empleo:          %.3f\n", con_pond))
cat(sprintf("Empleo en grupos de exposición alta (>=1,5):    %.1f%%\n", 100*sum(g$peso[g$exposicion>=1.5])))
write_csv(g %>% arrange(desc(exposicion)) %>%
          mutate(across(c(exposicion,peso), ~round(.,3))), file.path(out_t,"t13_ponderacion_empleo.csv"))

# --- gráfico: exposición del grupo (x) vs peso en el empleo (y) ---
g <- g %>% mutate(destacar = ifelse(isco=="4","alto",ifelse(isco=="5","grande","otro")))
col <- c("alto"="#D55E00","grande"="#0072B2","otro"="grey60")
p <- ggplot(g, aes(exposicion, peso*100)) +
  annotate("rect", xmin=1.5, xmax=Inf, ymin=-Inf, ymax=Inf, fill="#D55E00", alpha=0.05) +
  geom_point(aes(size = empleo_mil, colour = destacar), alpha = 0.85) +
  geom_text_repel(aes(label = grupo), family = FAM, size = 3.6, colour = "grey20",
                  box.padding = 0.6, max.overlaps = 20, seed = 9) +
  scale_colour_manual(values = col, guide = "none") +
  scale_size_area(max_size = 22, guide = "none") +
  scale_x_continuous(limits = c(1, 2.5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "El trabajo más expuesto a la IA es de los más chicos en empleo",
    subtitle = "Lo administrativo es lo más expuesto, pero es 1 de cada 9 empleos; la mayoría del empleo está en grupos menos expuestos",
    x = "Exposición media a la IA del grupo ocupacional  (0 – 3)",
    y = "Participación en el empleo de Argentina",
    caption = "Fuente: Global Automation Atlas (2026) y empleo por ocupación ISCO-08 de ILOSTAT/INDEC (~2025). Puente SOC→ISCO aproximado."
  ) +
  theme_minimal(base_size = 13, base_family = FAM) +
  theme(plot.title = element_text(face="bold", size=16, colour="#9A3A00"),
        plot.subtitle = element_text(colour="grey35", size=11, margin=margin(b=8)),
        plot.caption = element_text(colour="grey45", hjust=0, size=8.5),
        panel.grid.minor = element_blank())
ggsave(file.path(out_g,"empleo_vs_exposicion.jpg"), p, device = agg_jpeg,
       width = 24, height = 15, units = "cm", dpi = 300)
cat("OK -> t13_ponderacion_empleo.csv · empleo_vs_exposicion.jpg\n")
