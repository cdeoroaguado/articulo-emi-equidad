# Interacciones en el modelo explicativo (análisis exploratorio) ----------------
# Parte del modelo M2 (demográficas + controles) y prueba, una por una, las
# interacciones con sentido teórico para el marco de equidad y acceso.
# Errores robustos HC3, prueba de Wald y ajuste de Holm por comparaciones múltiples.
#   Rscript R/interacciones.R

if (!exists("lk")) source("R/prep.R", encoding = "UTF-8")
suppressPackageStartupMessages({ library(sandwich); library(lmtest); library(car) })

int_d <- dplyr::filter(d, genero != "Otro")
int_d$estrato_c <- int_d$estrato - mean(int_d$estrato)       # variables continuas centradas
int_d$edu_c     <- int_d$edu_max - mean(int_d$edu_max)
int_d$ingles_c  <- int_d$nivel_ing - mean(int_d$nivel_ing)
int_d$masc      <- as.integer(int_d$genero == "Masculino")
int_base <- "EXITO ~ publico + bilingue + estrato_c + edu_c + programa + masc + semestre"
int_m0 <- lm(int_base, int_d)
vc <- function(m) vcovHC(m, type = "HC3")

# 1. Cada interacción por separado -----------------------------------------------
int_prueba <- function(nombre, termino, extra = "") {
  m1 <- lm(paste(int_base, extra, "+", termino), int_d)
  ref <- if (extra == "") int_m0 else lm(paste(int_base, extra), int_d)
  co <- grep(":", names(coef(m1)), value = TRUE)
  W <- linearHypothesis(m1, paste(co, "= 0"), vcov. = vc(m1), test = "F")
  ct <- coeftest(m1, vcov = vc(m1))
  data.frame(Interaccion = nombre,
             b = if (length(co) == 1) ct[co, 1] else NA, ee = if (length(co) == 1) ct[co, 2] else NA,
             gl1 = W$Df[2], gl2 = W$Res.Df[2], F = W$F[2], p = W$`Pr(>F)`[2],
             dR2 = summary(m1)$r.squared - summary(ref)$r.squared)
}
int_res <- rbind(
  int_prueba("Colegio bilingüe × estrato", "bilingue:estrato_c"),
  int_prueba("Colegio bilingüe × programa", "bilingue:programa"),
  int_prueba("Colegio bilingüe × colegio público", "bilingue:publico"),
  int_prueba("Colegio bilingüe × educación de los padres", "bilingue:edu_c"),
  int_prueba("Colegio bilingüe × género", "bilingue:masc"),
  int_prueba("Colegio público × estrato", "publico:estrato_c"),
  int_prueba("Estrato × educación de los padres", "estrato_c:edu_c"),
  int_prueba("Nivel de inglés × colegio bilingüe", "ingles_c:bilingue", extra = "+ ingles_c"),
  int_prueba("Nivel de inglés × estrato", "ingles_c:estrato_c", extra = "+ ingles_c"),
  int_prueba("Nivel de inglés × colegio público", "ingles_c:publico", extra = "+ ingles_c"))
int_res$p_holm <- p.adjust(int_res$p, "holm")

# 2. Modelo final con las dos interacciones que se sostienen ------------------------
int_mf <- lm(paste(int_base, "+ bilingue:estrato_c + bilingue:programa"), int_d)
int_mf_ct <- coeftest(int_mf, vcov = vc(int_mf))
int_r2 <- c(M2 = summary(int_m0)$r.squared, M4 = summary(int_mf)$r.squared)
int_W2 <- linearHypothesis(int_mf, paste(grep(":", names(coef(int_mf)), value = TRUE), "= 0"), vcov. = vc(int_mf), test = "F")

# 3. Efectos simples del colegio bilingüe ------------------------------------------
efecto <- function(m, pesos) {
  g <- pesos[names(pesos) %in% names(coef(m))]; V <- vc(m)[names(g), names(g), drop = FALSE]
  est <- sum(coef(m)[names(g)] * g); se <- sqrt(drop(t(g) %*% V %*% g))
  c(est = est, se = se, li = est - 1.96 * se, ls = est + 1.96 * se, p = 2 * pnorm(-abs(est / se)))
}
m_est <- lm(paste(int_base, "+ bilingue:estrato_c"), int_d)
int_ef_estrato <- t(sapply(c(`1–2` = 1.5, `3–4` = 3.5, `5–6` = 5.5), function(e)
  efecto(m_est, c(bilingue = 1, `bilingue:estrato_c` = e - mean(int_d$estrato)))))
m_prog <- lm(paste(int_base, "+ bilingue:programa"), int_d)
int_ef_programa <- t(sapply(levels(int_d$programa), function(p) {
  w <- c(bilingue = 1); if (p != levels(int_d$programa)[1]) w[paste0("bilingue:programa", p)] <- 1
  efecto(m_prog, w)
}))

# Descriptivos por celda (para las figuras y para la cautela sobre celdas pequeñas)
celda <- function(v) int_d |> dplyr::group_by(g = .data[[v]], bilingue) |>
  dplyr::summarise(n = dplyr::n(), m = mean(EXITO), se = sd(EXITO) / sqrt(dplyr::n()), med = median(EXITO), .groups = "drop")
int_celdas_estrato  <- celda("estr_g")
int_celdas_programa <- celda("programa")

# 4. Robustez de la interacción colegio bilingüe × estrato ---------------------------
rob_b <- function(m, term = "bilingue:estrato_c") { ct <- coeftest(m, vcov = vc(m)); c(b = ct[term, 1], p = ct[term, 4]) }
EX3 <- cut(int_d$EXITO, c(0, 3.6, 4.6, 5), labels = c("bajo", "medio", "alto"), ordered_result = TRUE)
po <- MASS::polr(EX3 ~ publico + bilingue * estrato_c + edu_c + programa + masc + semestre, data = int_d, Hess = TRUE)
po_ct <- coef(summary(po))
ck <- cooks.distance(m_est)
m_sin7 <- lm(paste(int_base, "+ bilingue:estrato_c"), subset(int_d, !(estrato <= 2 & bilingue == 1)))
m_cook <- lm(paste(int_base, "+ bilingue:estrato_c"), int_d[ck <= 4 / nrow(int_d), ])
int_d$estr3 <- factor(int_d$estr_g)
m_cat <- lm(paste(int_base, "+ estr3 + bilingue:estr3"), int_d)
W_cat <- linearHypothesis(m_cat, paste(grep("bilingue:estr3", names(coef(m_cat)), value = TRUE), "= 0"), vcov. = vc(m_cat), test = "F")
int_rob <- data.frame(
  Prueba = c("Modelo principal (estrato de 1 a 6)",
             "Junto con la interacción por programa",
             "Regresión logística ordinal (índice en tres niveles)",
             paste0("Sin los ", sum(int_d$estrato <= 2 & int_d$bilingue == 1), " estudiantes bilingües de estratos 1 y 2"),
             paste0("Sin los ", sum(ck > 4 / nrow(int_d)), " casos más influyentes (distancia de Cook > 4/n)"),
             "Estrato agrupado en tres categorías (1–2, 3–4, 5–6)"),
  b = c(rob_b(m_est)["b"], int_mf_ct["bilingue:estrato_c", 1], po_ct["bilingue:estrato_c", 1],
        rob_b(m_sin7)["b"], rob_b(m_cook)["b"], NA),
  p = c(rob_b(m_est)["p"], int_mf_ct["bilingue:estrato_c", 4], 2 * pnorm(-abs(po_ct["bilingue:estrato_c", 3])),
        rob_b(m_sin7)["p"], rob_b(m_cook)["p"], W_cat$`Pr(>F)`[2]))

# Pendiente del nivel de inglés en cada grupo (¿la ruta inglés → éxito cambia?)
m_ing <- lm(paste(int_base, "+ ingles_c + ingles_c:bilingue"), int_d)
nm_ing <- grep("ingles_c:bilingue|bilingue:ingles_c", names(coef(m_ing)), value = TRUE)
int_pend_ingles <- rbind(`No bilingüe` = efecto(m_ing, c(ingles_c = 1)),
                         `Bilingüe` = efecto(m_ing, setNames(c(1, 1), c("ingles_c", nm_ing))))

if (sys.nframe() == 0) {
  options(width = 200)
  print(transform(int_res, b = round(b, 3), ee = round(ee, 3), F = round(F, 2), p = signif(p, 3), p_holm = signif(p_holm, 3), dR2 = round(dR2, 4)), row.names = FALSE)
  print(round(int_r2, 3)); print(int_W2)
  print(round(int_ef_estrato, 3)); print(round(int_ef_programa, 3))
  print(int_rob, digits = 3); print(round(int_pend_ingles, 3))
  print(round(int_mf_ct, 3))
}
