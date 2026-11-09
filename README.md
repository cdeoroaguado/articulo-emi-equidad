# Equidad, acceso y autopercepción del éxito en EMI

Informe preliminar, en formato de libro (bookdown), sobre cómo se relacionan los aspectos demográficos (tipo de colegio, colegio bilingüe, estrato y educación de los padres) con la autopercepción del éxito en cursos dictados en inglés (EMI).

**Leer el libro:** https://cdeoroaguado.github.io/libro-emi-equidad/

## Estructura

- `index.Rmd` y `01-…09-*.Rmd`: capítulos del libro.
- `R/prep.R`: lectura, depuración y análisis de la base.
- `R/afc_ordinal.R`: análisis factorial confirmatorio ordinal (WLSMV) del índice, con ajuste, cargas, AVE y omega categórico.
- `R/alfa_cronbach.R`: confiabilidad del índice (alfa de Cronbach verificado con `psych`, IC de Feldt y bootstrap, omega de McDonald, versiones ordinales y diagnóstico por ítem). Se puede correr solo con `Rscript R/alfa_cronbach.R`.
- `docs/`: libro compilado que publica GitHub Pages.

## Reproducir

1. Coloque el Excel de la encuesta en la carpeta superior a este proyecto. **La base no se incluye en el repositorio** porque contiene nombres, correos y códigos de estudiante.
2. Abra `libro_emi.Rproj` en RStudio.
3. Pestaña *Build* → *Build Book*, o en la consola: `bookdown::render_book("index.Rmd")`.

Paquetes: bookdown, readxl, dplyr, tidyr, ggplot2, knitr, kableExtra, sandwich, lmtest, MASS, boot, car, psych, lavaan, semTools.

Autores: Kathleen, Lurdes, Jair y Carlos de Oro (Universidad del Norte).
