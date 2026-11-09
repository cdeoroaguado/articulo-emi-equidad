# Exporta la base depurada (N = 348) --------------------------------------------
# Paso 1 (este script): prepara las hojas y las guarda como CSV UTF-8 en una
#   carpeta temporal.
# Paso 2 (R/armar_excel.py): arma el Excel ../Base_EMI_depurada_N348.xlsx.
# Se hace en dos pasos porque en este equipo la política de control de
# aplicaciones de Windows bloquea las librerías compiladas de openxlsx/writexl.
# El Excel queda fuera del repositorio: contiene datos personales y *.xlsx está
# excluido en .gitignore.
#   Rscript R/exportar_depurada.R <carpeta_temporal>
#   python R/armar_excel.py <carpeta_temporal> ../Base_EMI_depurada_N348.xlsx

source("R/prep.R", encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
tmp <- if (length(args)) args[1] else file.path(tempdir(), "export_emi")
dir.create(tmp, showWarnings = FALSE, recursive = TRUE)

# 1. Datos originales de los 348 estudiantes -----------------------------------
#    Mismas columnas del formulario, sin las columnas vacías de "Puntos" y
#    "Comentarios". Filas en el mismo orden que el archivo original.
no_vacias <- !sapply(raw, function(x) all(is.na(x)))
datos <- raw[raw$ID %in% d$id, no_vacias]
names(datos) <- trimws(gsub("[\r\n]+", " ", names(datos)))
stopifnot(nrow(datos) == N, !anyDuplicated(datos$ID))

# 2. Variables recodificadas que usa el análisis -------------------------------
vars <- d |>
  arrange(match(id, datos$ID)) |>
  transmute(
    ID = id, programa = as.character(programa), semestre, genero,
    edad, colegio, colegio_publico = publico, colegio_bilingue = bilingue,
    estrato, estrato_grupo = as.character(estr_g),
    edu_madre = madre, edu_padre = padre, edu_padres_max = edu_max, padres_universitarios = padres_univ,
    nivel_ingles = nivel_ing,
    conceptos, barrera, barrera_invertido = barrera_r, ingles_desemp, buen_desemp, aprendi_igual,
    no_capaz, no_capaz_invertido = no_capaz_r, comodo,
    indice_exito_5items = round(EXITO, 4)
  )
stopifnot(identical(vars$ID, datos$ID))

# 3. Diccionario de variables ---------------------------------------------------
dicc <- data.frame(
  Variable = names(vars),
  Descripción = c(
    "Identificador de la respuesta en el formulario original",
    "Programa académico", "Semestre actual", "Género declarado",
    "Edad en 2026 (vacía si el año de nacimiento está mal digitado)",
    "Tipo de colegio (texto original)", "Colegio público", "El colegio de bachillerato es bilingüe",
    "Estrato socioeconómico", "Estrato agrupado",
    "Nivel máximo de escolaridad de la madre", "Nivel máximo de escolaridad del padre",
    "Nivel educativo más alto entre madre y padre", "Al menos un padre con pregrado o posgrado",
    "Mi nivel de inglés es (autopercibido)",
    "Pude entender los conceptos clave de las asignaturas impartidas en inglés.",
    "El inglés fue una barrera para demostrar mi conocimiento sobre los temas de la clase.",
    "Ítem de barrera invertido (6 − barrera)",
    "Mi nivel de inglés me ayudó a tener un buen desempeño académico en las asignaturas.",
    "Logré un buen desempeño académico en las asignaturas, a pesar de que fueron dictadas en inglés.",
    "En mis cursos del programa en inglés, aprendí igual que en los cursos impartidos en español.",
    "Sentía que no era capaz de aprender bien debido a mis limitaciones en inglés.",
    "Ítem 'no era capaz' invertido (6 − no_capaz); solo en la versión de 7 ítems",
    "Me sentí cómodo/a porque entendí las temáticas de las clases en inglés.",
    "Índice de éxito percibido: promedio de conceptos, barrera_invertido, ingles_desemp, buen_desemp y aprendi_igual"),
  Dimensión = c(rep("Datos sociodemográficos", 14), "Perfil lingüístico",
                rep("Aprendizaje disciplinar", 6), "Dominio del inglés", "Dominio del inglés",
                "Motivación y actitudes", "Índice (Aprendizaje disciplinar)"),
  Codificación = c(
    "Número", "Texto", "Número", "Texto", "Años", "Público / Privado", "1 = Público; 0 = Privado",
    "1 = Sí; 0 = No", "1 a 6", "1–2 / 3–4 / 5–6", "Texto", "Texto",
    "1 Primaria; 2 Bachillerato; 3 Técnico/Tecnólogo; 4 Pregrado; 5 Posgrado", "1 = Sí; 0 = No",
    "1 Malo; 2 Regular; 3 Bueno; 4 Excelente",
    rep("1 Totalmente en desacuerdo; 2 Parcialmente en desacuerdo; 3 Indiferente; 4 Parcialmente de acuerdo; 5 Totalmente de acuerdo", 2),
    "5 = Totalmente en desacuerdo … 1 = Totalmente de acuerdo",
    rep("1 Totalmente en desacuerdo … 5 Totalmente de acuerdo", 4),
    "5 = Totalmente en desacuerdo … 1 = Totalmente de acuerdo",
    "1 Totalmente en desacuerdo … 5 Totalmente de acuerdo",
    "1 a 5 (promedio)"),
  check.names = FALSE)
stopifnot(nrow(dicc) == ncol(vars))

# 4. Registro de depuración -----------------------------------------------------
pasos <- data.frame(
  Paso = 1:5,
  Criterio = c("Respuestas en el archivo original",
               "Se excluyen quienes no dieron consentimiento informado",
               "Se excluyen respuestas repetidas del mismo estudiante (mismo correo); se conserva la primera por fecha",
               "Se excluyen estudiantes de Lenguas Modernas y Cultura",
               "Base depurada"),
  Cambio = c(n_raw, -n_no_consent, -n_dup_quit, -n_lmc, NA),
  `N resultante` = c(n_raw, n_raw - n_no_consent, n_raw - n_no_consent - n_dup_quit, N, N),
  check.names = FALSE)

ids_consent <- d0$id[d0$consent == "No"]
ids_dup     <- setdiff(d1$id, d2$id)
ids_lmc     <- d2$id[d2$programa == "Lenguas Modernas y Cultura"]
excluidos <- data.frame(
  ID = c(ids_consent, ids_dup, ids_lmc),
  Motivo = c(rep("No dio consentimiento informado", length(ids_consent)),
             rep("Respuesta repetida (se conservó la primera del mismo estudiante)", length(ids_dup)),
             rep("Programa Lenguas Modernas y Cultura", length(ids_lmc))))
excluidos <- excluidos[order(excluidos$ID), ]
stopifnot(nrow(excluidos) == n_raw - N, !any(excluidos$ID %in% datos$ID))

# 5. Hoja de notas y escritura de las hojas como CSV ------------------------------
notas <- data.frame(Nota = c(
  "Base depurada de la encuesta 'Uso del inglés como medio de instrucción en instituciones de educación superior'.",
  paste0("N = ", N, " estudiantes (de ", n_raw, " respuestas originales). Generada el ", format(Sys.Date(), "%d/%m/%Y"), "."),
  "Datos_originales: columnas del formulario de los 348 estudiantes, sin las columnas vacías de Puntos y Comentarios.",
  "Variables_analisis: variables recodificadas que se usan en el artículo, incluido el índice de éxito percibido.",
  "Diccionario: descripción, dimensión y codificación de cada variable recodificada.",
  "Depuracion y Excluidos: pasos de depuración e ID de cada respuesta excluida con su motivo.",
  "CONFIDENCIAL: contiene nombres, correos y códigos de estudiante. No compartir fuera del equipo de investigación."))

hojas <- list(`1_Notas` = notas, `2_Datos_originales` = datos, `3_Variables_analisis` = vars,
              `4_Diccionario` = dicc, `5_Depuracion` = pasos, `6_Excluidos` = excluidos)
fechas <- sapply(datos, inherits, "POSIXt")
hojas$`2_Datos_originales`[fechas] <- lapply(datos[fechas], format, "%Y-%m-%d %H:%M:%S")
for (h in names(hojas)) {
  write.csv(hojas[[h]], file.path(tmp, paste0(h, ".csv")), row.names = FALSE, na = "", fileEncoding = "UTF-8")
}
cat("Hojas guardadas en:", normalizePath(tmp), "\n")
