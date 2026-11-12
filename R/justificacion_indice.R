# Evidencia para justificar la construcción del índice de éxito percibido --------
# 1) ¿El promedio simple equivale a la puntuación factorial del AFC ordinal?
# 2) ¿Las conclusiones cambian si el índice se arma con otros ítems?
#   Rscript R/justificacion_indice.R

if (!exists("lk")) source("R/prep.R", encoding = "UTF-8")
if (!exists("afc")) source("R/afc_ordinal.R", encoding = "UTF-8")
suppressPackageStartupMessages({ library(lavaan); library(sandwich); library(lmtest) })

# 1. Promedio simple frente a puntuación factorial (AFC ordinal, método EBM)
fs <- as.numeric(lavPredict(afc, method = "EBM"))
just_r_fs  <- cor(d$EXITO, fs)
just_rs_fs <- cor(d$EXITO, fs, method = "spearman")

# 2. Versiones alternativas del índice
versiones <- list(
  `5 ítems (índice elegido)` = c("conceptos", "barrera_r", "ingles_desemp", "buen_desemp", "aprendi_igual"),
  `4 ítems (sin el ítem de barrera)` = c("conceptos", "ingles_desemp", "buen_desemp", "aprendi_igual"),
  `3 ítems (sin ítems que nombran el inglés)` = c("conceptos", "buen_desemp", "aprendi_igual"),
  `7 ítems (con "no era capaz" y "me sentí cómodo")` = c("conceptos", "barrera_r", "ingles_desemp", "buen_desemp", "aprendi_igual", "no_capaz_r", "comodo"))
alfa_v <- function(m) { m <- as.matrix(m); k <- ncol(m); k / (k - 1) * (1 - sum(apply(m, 2, var)) / var(rowSums(m))) }
dm_j <- dplyr::filter(d, genero != "Otro")
just_versiones <- do.call(rbind, lapply(names(versiones), function(v) {
  it <- versiones[[v]]
  y <- rowMeans(d[it]); yj <- rowMeans(dm_j[it])
  m1 <- lm(yj ~ publico + bilingue + estrato + edu_max, data = dm_j)
  c1 <- coeftest(m1, vcov = vcovHC(m1, type = "HC3"))
  w <- wilcox.test(y[d$bilingue == 1], y[d$bilingue == 0], exact = FALSE)
  data.frame(version = v, items = length(it), alfa = alfa_v(d[it]),
             r_indice = cor(y, d$EXITO), techo = mean(y >= 4),
             b_bil = c1["bilingue", 1], p_bil = c1["bilingue", 4],
             r_rb = unname(2 * w$statistic / (sum(d$bilingue == 1) * sum(d$bilingue == 0)) - 1))
}))
rownames(just_versiones) <- NULL
# Puntuación factorial como variable dependiente (misma regresión)
dm_j$fs <- fs[d$genero != "Otro"]
m_fs <- lm(scale(fs) ~ publico + bilingue + estrato + edu_max, data = dm_j)
m_ix <- lm(scale(EXITO) ~ publico + bilingue + estrato + edu_max, data = dm_j)
just_beta_std <- c(promedio = coef(m_ix)["bilingue"], factorial = coef(m_fs)["bilingue"])

if (sys.nframe() == 0) {
  cat(sprintf("r(promedio, puntuación factorial) = %.3f | Spearman = %.3f\n", just_r_fs, just_rs_fs))
  print(transform(just_versiones, alfa = round(alfa, 3), r_indice = round(r_indice, 3), techo = round(techo, 3),
                  b_bil = round(b_bil, 3), p_bil = signif(p_bil, 3), r_rb = round(r_rb, 3)), row.names = FALSE)
  print(round(just_beta_std, 3))
}
