# Dimensiones del instrumento -------------------------------------------------
# Reconstruye a qué dimensión (sección del cuestionario) pertenece cada ítem
# Likert, a partir de los encabezados de sección del formulario exportado.

if (!exists("lk")) source("R/prep.R", encoding = "UTF-8")

nm <- names(raw)
base_nm <- sub("\\.[0-9]+$", "", nm)                     # quita sufijos de make.unique
es_meta <- grepl("^(Puntos|Comentarios): ", base_nm)
txt_meta <- sub("^(Puntos|Comentarios): ", "", base_nm)
# Un encabezado de sección es un "Puntos:/Comentarios:" cuyo texto no es una pregunta
es_seccion <- es_meta & !(trimws(txt_meta) %in% trimws(base_nm[!es_meta]))

opciones_likert <- names(lk)
es_likert <- sapply(raw, function(x) all(na.omit(unique(x)) %in% opciones_likert)) & !es_meta &
  sapply(raw, function(x) sum(!is.na(x)) > 0)

seccion_actual <- NA_character_
dim_tab <- list()
for (j in seq_along(nm)) {
  if (es_seccion[j]) seccion_actual <- trimws(txt_meta[j])
  if (es_likert[j]) dim_tab[[length(dim_tab) + 1]] <- data.frame(col = j, item = trimws(nm[j]), seccion = seccion_actual)
}
dim_tab <- do.call(rbind, dim_tab)

# Nombres cortos de las dimensiones
dim_corta <- function(s) dplyr::case_when(
  grepl("dominio del ingl", s, ignore.case = TRUE) ~ "Dominio del inglés",
  grepl("aprendizaje disciplinar", s, ignore.case = TRUE) ~ "Aprendizaje disciplinar",
  grepl("^Pedagog", s) ~ "Pedagogía y recursos",
  grepl("motivaci", s, ignore.case = TRUE) ~ "Motivación y actitudes",
  grepl("Pensando", s) ~ "Proyección futura",
  TRUE ~ s)
dim_tab$dimension <- dim_corta(dim_tab$seccion)

# Ítems usados en el índice (columnas de la base cruda; ver R/prep.R)
uso <- c(`87` = "Índice (5 ítems)", `93` = "Índice (5 ítems), invertido", `96` = "Índice (5 ítems)",
         `102` = "Índice (5 ítems)", `105` = "Índice (5 ítems)",
         `70` = "Solo versión de 7 ítems, invertido", `133` = "Solo versión de 7 ítems")
dim_tab$uso <- ifelse(as.character(dim_tab$col) %in% names(uso), uso[as.character(dim_tab$col)], "No se usa")
dim_tab$item <- sub("\\.2$", "", sub("2$", " (repetido)", dim_tab$item))
