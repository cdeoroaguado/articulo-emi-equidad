# Comparaciones por pares después de Kruskal-Wallis -----------------------------
# Prueba de Dunn (1964) con corrección por empates y ajuste de Holm (1979) para
# comparaciones múltiples. Tamaño del efecto: r = |z| / sqrt(N).
# Se usa desde la sección 5 del artículo; también puede correrse solo:
#   Rscript R/posthoc_dunn.R

if (!exists("lk")) source("R/prep.R", encoding = "UTF-8")

dunn_holm <- function(y, g) {
  ok <- !is.na(y) & !is.na(g)
  y <- y[ok]; g <- droplevels(factor(g[ok]))
  N <- length(y)
  r <- rank(y)                                        # rangos conjuntos (empates promediados)
  ties <- table(y)
  corr_emp <- sum(ties^3 - ties) / (12 * (N - 1))     # corrección por empates
  s2 <- N * (N + 1) / 12 - corr_emp
  lv <- levels(g)
  Rm <- tapply(r, g, mean); n <- table(g)
  pares <- t(combn(lv, 2))
  out <- data.frame(g1 = pares[, 1], g2 = pares[, 2])
  out$dif_rangos <- Rm[out$g2] - Rm[out$g1]           # positivo: g2 tiene rangos más altos
  out$z <- out$dif_rangos / sqrt(s2 * (1 / n[out$g1] + 1 / n[out$g2]))
  out$p <- 2 * pnorm(-abs(out$z))
  out$p_holm <- p.adjust(out$p, method = "holm")
  out$r <- abs(out$z) / sqrt(N)
  out$med1 <- tapply(y, g, median)[out$g1]
  out$med2 <- tapply(y, g, median)[out$g2]
  out$n1 <- as.integer(n[out$g1]); out$n2 <- as.integer(n[out$g2])
  out
}

dunn_estrato  <- dunn_holm(d$EXITO, factor(d$estrato, 1:6))
dunn_edu      <- dunn_holm(d$EXITO, factor(d$edu_max, 1:5,
                           c("Primaria", "Bachillerato", "Técnico/Tecnólogo", "Pregrado", "Posgrado")))
dunn_programa <- dunn_holm(d$EXITO, factor(d$programa,
                           c("Contaduría Pública", "Administración de Empresas", "Negocios Internacionales")))

if (sys.nframe() == 0) {
  for (nm in c("dunn_estrato", "dunn_edu", "dunn_programa")) {
    cat("\n==", nm, "\n"); print(get(nm), digits = 3, row.names = FALSE)
  }
}
