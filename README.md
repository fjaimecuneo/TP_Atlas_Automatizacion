# TP Ciencia de Datos — Atlas de Automatización

**Materia:** Ciencia de Datos para Economía y Negocios — FCE-UBA
**Integrantes:** Francisco Jaime · Tomas Langmosser
**Base:** Global Automation Atlas (https://automationatlas.org), release 2026 · O*NET 29.1

## Objetivo

Estudiar, desde el análisis de datos y **sin pretensión causal**, cómo se distribuye la exposición del trabajo a la automatización por IA entre países y ocupaciones, con foco en la posición de Argentina.

## Hipótesis

La exposición a la IA no se reparte pareja: se concentra en ocupaciones cognitivo-administrativas de rutina y crece con el ingreso del país. Argentina, pese a ser de ingreso medio-alto, estaría más expuesta que el promedio regional y mundial, con un perfil dominado por la sustitución más que por la complementación.

> La hipótesis se **revisó** respecto de la Instancia 1 (es esperable). El hallazgo intra-país —la concentración ocupacional— resiste; la parte "entre países" se matiza, porque resultó ser en buena medida un artefacto del método (ver **Conclusiones**).

## Base de datos

El *Global Automation Atlas* mide, **tarea por tarea** (18.797 tareas O*NET), el nivel de exposición a la automatización en una escala **ordinal 0–3**, distinguiendo si el efecto es de **sustitución** o de **complemento**.

- **Fuente:** https://automationatlas.org · release 2026 · O*NET 29.1
- **Unidad de observación:** el par **país × ocupación** (panel principal) y el **país** (base resumen).
- **Cobertura:** 124 países (99% del PBI mundial); 923 ocupaciones O*NET por país.
- **Período:** corte transversal **2026** (sin serie temporal).
- **Dimensiones de los archivos crudos:** `countries.csv` 124×37 · `country-occupation-panel.csv` 114.452×12.
- **Limitaciones conocidas:** mide exposición **potencial**, no empleo perdido observado; es transversal (no hay dinámica temporal); y la grilla de 923 ocupaciones es **común a todos los países** (las diferencias entre países provienen del ajuste per-país del modelo, no de su estructura ocupacional).

## Estructura del repositorio

```
TP_Atlas_Automatizacion/
├── raw/            # Datos originales del Atlas, sin modificar
│   ├── countries.csv                    # resumen por país (124×37)
│   └── country-occupation-panel.csv     # panel país-ocupación (114.452×12)
├── auxiliar/       # Bases complementarias
│   └── empleo_isco_arg.csv              # empleo por grupo ISCO-08, AR ~2025 (ILOSTAT/INDEC)
├── input/          # Datos procesados (los genera 01_preparacion.R)
├── output/
│   ├── tablas/     # Tablas de resultados (t1…t19) generadas por los scripts
│   └── graficos/   # Gráficos generados con ggsave() (scripts 04, 07, 08, 10)
├── script/
│   ├── 01_preparacion.R        # raw → input: carga, validación, tipado
│   ├── 02_descriptivas.R       # Descriptivas (Instancia 1) → t1…t4
│   ├── 03_chi_cuadrado.R       # Inferencia: Chi² cortes fijos (Instancia 2) → t5…t7
│   ├── 04_clusters.R           # Cuantitativo: k-means + PCA de países (Inst. 2) → t8–t9 + clusters_pca.png
│   ├── 05_faltantes_outliers.R # Diagnóstico de faltantes y outliers (Inst. 2) → t10–t11
│   ├── 06_concentracion.R      # [DEPRECADO] Gini/HHI — ver nota de métodos
│   ├── 07_graficos_instancia3.R# Visualización: exploratorio + comunicativo (Inst. 3)
│   ├── 08_ponderacion_empleo.R # Ponderación por empleo; lee auxiliar/ (final)
│   ├── 09_robustez.R           # Robustez: terciles · silueta · con/sin outliers (final)
│   └── 10_descriptivas_limpieza.R # Faltantes · outliers · descriptivas antes/después (final)
├── utils/
│   ├── gini.R                  # coeficiente de Gini (usado por 02 y 06)
│   └── resumen_numerico.R      # resumen descriptivo de una variable (usado por 02)
├── entrega_final/  # Presentación final (.pptx y .pdf)
└── README.md
```

## Métodos

Tres métodos además de las estadísticas descriptivas, en orden jerárquico:

1. **Descriptivas** (`02`): gradiente de exposición por nivel de ingreso, Argentina vs. región y mundo, y distribución entre las 923 ocupaciones argentinas.
2. **Unidad 2 — Chi-cuadrado de independencia** (`03`, `09`): familia ocupacional × tramo de exposición, sobre las 923 ocupaciones de Argentina. Se rechaza la independencia; la exposición se concentra en "Ventas y administración". En la entrega final se reporta con **terciles** (cortes data-driven) → X²=255,4, gl=10, p<0,001, **V=0,37** (vs. 0,44 con cortes fijos, que venía inflado por el corte). Supuesto verificado: frecuencias esperadas ≥ 5 en el 100% de las celdas.
3. **Unidad 3 — Clustering k-means + PCA** (`04`, `09`): 124 países por perfil de exposición (6 variables estandarizadas), k=4. La silueta favorece k=2 (0,62); se mantiene k=4 por interpretabilidad, explicitándolo. La pertenencia de Argentina a su cluster es estable al excluir outliers (Jaccard=1,00). El PBI no es variable de cluster (solo descriptor).
4. **Ponderación por empleo** (`08`, aporte final): la exposición por grupo ISCO-08 se pondera por el empleo real de Argentina (base auxiliar ILOSTAT/INDEC, vía puente SOC→ISCO de 1 dígito, aproximado y documentado). La media ponderada (1,44) ≈ la simple (1,43), pero lo más expuesto (administrativos, 2,35) es sólo ~11,6% del empleo; sólo ~21% del empleo está en grupos de exposición alta (≥1,5).

**Faltantes y outliers** (`05`, `10`): 0 faltantes en las 14 variables usadas (verificado, no asumido). Outliers por IQR×1,5: 5 de 923 ocupaciones (4 administrativas/contables, 1 el clero). Decisión: mantenerlos (valores genuinos); excluirlos mueve la media sólo −0,34% (1,432 → 1,428), lo que justifica la decisión.

**Nota — `06_concentracion.R` (deprecado):** medía concentración con Gini/HHI. Se descartó por indicación del docente — aplicar Gini a un puntaje ordinal de exposición (no a una magnitud monetaria o de stock) no es una medida de concentración bien definida. Se conserva por trazabilidad; no alimenta el deck final.

## Reproducción

Requiere **R ≥ 4.0** y los paquetes: `dplyr`, `readr`, `tidyr`, `ggplot2`, `ggrepel`, `scales`, `ragg`, `cluster`.

```r
install.packages(c("dplyr","readr","tidyr","ggplot2","ggrepel","scales","ragg","cluster"))
```

Desde la **raíz del proyecto** (las rutas son relativas), correr en orden. Los scripts que generan gráficos usan acentos, así que conviene el locale UTF-8:

```bash
LC_ALL=C.UTF-8 Rscript script/01_preparacion.R          # → input/ (paises · pais_ocupacion · arg_ocupaciones)
LC_ALL=C.UTF-8 Rscript script/02_descriptivas.R         # → t1–t4
LC_ALL=C.UTF-8 Rscript script/03_chi_cuadrado.R         # → t5–t7
LC_ALL=C.UTF-8 Rscript script/04_clusters.R             # → t8–t9 + graficos/clusters_pca.png
LC_ALL=C.UTF-8 Rscript script/05_faltantes_outliers.R   # → t10–t11
LC_ALL=C.UTF-8 Rscript script/06_concentracion.R        # → t12 (deprecado)
LC_ALL=C.UTF-8 Rscript script/07_graficos_instancia3.R  # → graficos/grupo_09_*.jpg
LC_ALL=C.UTF-8 Rscript script/08_ponderacion_empleo.R   # → t13 + graficos/empleo_vs_exposicion.jpg
LC_ALL=C.UTF-8 Rscript script/09_robustez.R             # → t14–t17
LC_ALL=C.UTF-8 Rscript script/10_descriptivas_limpieza.R# → t18–t19 + graficos/faltantes_outliers.jpg
```

Cada script es autónomo: guarda sus resultados en `input/` o `output/`, y el siguiente los lee desde el archivo (no depende de la sesión anterior). Probado de punta a punta en un clon limpio, sin errores.

## Trazabilidad: qué produce cada número del deck final

| Resultado | Script → salida |
|---|---|
| Descriptivas y gradiente por ingreso | `02` → `t1`–`t4` |
| Concentración ocupacional (2 gráficos Inst. 3) | `07` → `grupo_09_exploratorio.jpg`, `grupo_09_comunicativo.jpg` |
| Faltantes / outliers (antes-después) | `10` → `t18`, `t19`, `faltantes_outliers.jpg` |
| Chi² con terciles (V=0,37) | `09` → `t14_chi_terciles.csv` |
| Clustering + PCA de países | `04` → `t8`–`t9`, `clusters_pca.png` |
| Silueta · descriptivas por cluster · con/sin outliers | `09` → `t15`, `t16`, `t17` |
| Ponderación por empleo | `08` → `t13_ponderacion_empleo.csv`, `empleo_vs_exposicion.jpg` |

## Conclusiones principales

- **Dentro de Argentina, la exposición a la IA se concentra** en ocupaciones administrativas y de ventas, con perfil de sustitución (Chi² con terciles: V=0,37, p<0,001). Es robusto al cambio de método, de cortes y a los outliers.
- **Al ponderar por el empleo real, el riesgo es concentrado, no generalizado:** el núcleo más expuesto es chico (~21% del empleo en grupos de exposición alta), y la media ponderada ≈ la simple. La lectura relevante es la requalificación focalizada, no el "reemplazo masivo".
- **El ranking "Argentina entre países ricos" es casi un artefacto del método:** como las 923 ocupaciones son idénticas en todos los países, las diferencias provienen del ajuste per-país del modelo. Se presenta como demostración de método y descriptivo, no como evidencia independiente.
- En suma, la hipótesis se **confirma en su parte intra-país** (concentración ocupacional, perfil de sustitución) y se **matiza en su parte entre-países**.

## Próximos pasos

Serie temporal cuando haya nuevas releases; empleo a nivel ocupación (microdatos EPH/INDEC) en vez de los 9 grandes grupos ISCO; puente SOC→ISCO a 2–3 dígitos; y cruce con salarios e informalidad para pasar de exposición a vulnerabilidad.
