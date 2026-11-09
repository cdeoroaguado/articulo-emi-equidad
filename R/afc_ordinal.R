# Análisis factorial confirmatorio (AFC) ordinal del índice de éxito percibido --
# Modelo de un factor con 5 ítems tratados como ordinales. Estimador WLSMV
# (mínimos cuadrados ponderados con media y varianza ajustadas) sobre
# correlaciones policóricas. Se usa desde la sección 3 del artículo; también
# puede correrse solo: Rscript R/afc_ordinal.R

if (!exists("d")) source("R/prep.R", encoding = "UTF-8")
suppressPackageStartupMessages({ library(lavaan); library(semTools) })

items5 <- c("conceptos", "barrera_r", "ingles_desemp", "buen_desemp", "aprendi_igual")
Y <- as.data.frame(d[items5])

# 1. Frecuencias por categoría ------------------------------------------------
#    Las categorías con muy pocas respuestas dan umbrales inestables.
frec <- sapply(items5, function(i) table(factor(Y[[i]], levels = 1:5)))
min_celda <- min(frec)

# 2. Recodificación para el modelo principal ----------------------------------
#    Se unen las categorías 1 y 2 (desacuerdo total y parcial) cuando alguna
#    tiene menos de 10 respuestas. El AFC con las 5 categorías originales se
#    estima también, como análisis de sensibilidad.
unir_12 <- function(x) ifelse(x <= 2, 2, x) - 1          # 1–4
escasos <- items5[apply(frec[1:2, , drop = FALSE], 2, min) < 10]
Yr <- Y
for (i in escasos) Yr[[i]] <- unir_12(Yr[[i]])

# 3. Modelo -------------------------------------------------------------------
modelo <- 'EXITO =~ conceptos + barrera_r + ingles_desemp + buen_desemp + aprendi_igual'

ajustar <- function(datos) cfa(modelo, data = datos, ordered = items5,
                               estimator = "WLSMV", std.lv = TRUE)
afc     <- ajustar(Yr)   # modelo principal (categorías escasas unidas)
afc_sen <- ajustar(Y)    # sensibilidad: 5 categorías originales

# 4. Índices de ajuste (versiones escaladas/robustas para WLSMV) ---------------
ind <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust", "tli.robust",
         "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "srmr")
ajuste_afc     <- fitMeasures(afc, ind)
ajuste_afc_sen <- fitMeasures(afc_sen, ind)
# Si la versión robusta no está disponible (NA), se usa la escalada
if (anyNA(ajuste_afc)) {
  alt <- fitMeasures(afc, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled",
                            "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr"))
  ajuste_afc[is.na(ajuste_afc)] <- alt[is.na(ajuste_afc)]
}

# 5. Cargas estandarizadas ----------------------------------------------------
cargas_afc <- subset(standardizedSolution(afc), op == "=~",
                     select = c(rhs, est.std, se, pvalue, ci.lower, ci.upper))
cargas_sen <- subset(standardizedSolution(afc_sen), op == "=~", select = c(rhs, est.std))
R2_afc <- inspect(afc, "rsquare")

# 6. Validez convergente y confiabilidad compuesta -----------------------------
#    AVE: varianza media extraída (Fornell y Larcker, 1981).
#    Omega categórico de Green y Yang (2009): confiabilidad del puntaje suma
#    en la escala observada de los ítems ordinales.
lam <- cargas_afc$est.std
ave <- mean(lam^2)
cr  <- sum(lam)^2 / (sum(lam)^2 + sum(1 - lam^2))   # confiabilidad compuesta (escala latente)
omega_cat <- as.numeric(compRelSEM(afc, ord.scale = TRUE))

# 7. Residuos -----------------------------------------------------------------
res <- lavResiduals(afc)$cov
res_max <- max(abs(res[lower.tri(res)]))
mi <- modindices(afc, sort. = TRUE, maximum.number = 3)

# Salida en consola cuando se corre el script solo ------------------------------
if (sys.nframe() == 0) {
  cat("Frecuencias por categoría:\n"); print(frec)
  cat("Ítems con categorías 1-2 unidas:", paste(escasos, collapse = ", "), "\n\n")
  cat("Ajuste (principal):\n"); print(round(ajuste_afc, 3))
  cat("\nAjuste (sensibilidad, 5 categorías):\n"); print(round(ajuste_afc_sen, 3))
  cat("\nCargas estandarizadas:\n"); print(cargas_afc, digits = 3, row.names = FALSE)
  cat(sprintf("\nAVE = %.3f | CR = %.3f | omega categórico = %.3f | residuo máx = %.3f\n",
              ave, cr, omega_cat, res_max))
  cat("\nÍndices de modificación más altos:\n"); print(mi[, c("lhs", "op", "rhs", "mi", "epc")])
  cat("\nCargas (sensibilidad):\n"); print(cargas_sen, digits = 3, row.names = FALSE)
}
