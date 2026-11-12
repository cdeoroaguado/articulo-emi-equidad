# Validación del instrumento completo: AFE (submuestra A) y AFC (submuestra B) --
# 24 ítems Likert (se excluye la repetición de "expresar ideas"), 5 dimensiones
# teóricas, N = 348 dividido al azar en dos mitades estratificadas por programa.
#   Rscript R/validacion_instrumento.R

if (!exists("lk")) source("R/prep.R", encoding = "UTF-8")
if (!exists("dim_tab")) source("R/dimensiones.R", encoding = "UTF-8")
suppressPackageStartupMessages({ library(psych); library(lavaan); library(semTools) })

# 1. Ítems, códigos e inversión -------------------------------------------------
it <- data.frame(
  codigo = c(paste0("DI", 1:7), paste0("AD", 1:6), paste0("PR", 1:5), paste0("MA", 1:3), paste0("PF", 1:3)),
  col    = c(61, 64, 67, 70, 73, 76, 79,  87, 90, 93, 96, 102, 105,  110, 113, 116, 119, 122,  130, 133, 136,  144, 147, 150),
  invertido = c(F, T, F, T, F, F, T,  F, F, T, F, F, F,  F, F, F, F, F,  T, F, T,  F, F, F),
  dimension = rep(c("Dominio del inglés", "Aprendizaje disciplinar", "Pedagogía y recursos",
                    "Motivación y actitudes", "Proyección futura"), c(7, 6, 5, 3, 3)))
it$texto <- trimws(sub("\\.[0-9]+$", "", names(raw)[it$col]))
stopifnot(all(dim_corta(dim_tab$seccion[match(it$col, dim_tab$col)]) == it$dimension))
fac_cod <- c("Dominio del inglés" = "DI", "Aprendizaje disciplinar" = "AD", "Pedagogía y recursos" = "PR",
             "Motivación y actitudes" = "MA", "Proyección futura" = "PF")

r_ <- raw[match(d$id, raw$ID), ]
X <- as.data.frame(sapply(it$col, function(j) unname(lk[r_[[j]]])))
names(X) <- it$codigo
X[it$codigo[it$invertido]] <- 6 - X[it$codigo[it$invertido]]       # ítems negativos invertidos
stopifnot(nrow(X) == N, !anyNA(X))

# 2. Descriptivos por ítem (escala 1–5, ya invertidos) --------------------------
desc_items <- data.frame(codigo = it$codigo,
  media = sapply(X, mean), de = sapply(X, sd), mediana = sapply(X, median),
  asim = sapply(X, skew), curt = sapply(X, function(x) psych::kurtosi(x)),
  techo = sapply(X, function(x) mean(x == 5)), suelo = sapply(X, function(x) mean(x == 1)))

# 3. Unión de categorías escasas -------------------------------------------------
#    Desde la categoría más baja se unen categorías adyacentes hasta que la
#    categoría inferior reúne al menos 10 respuestas en la muestra total, sin
#    dejar nunca menos de 3 categorías. Se aplica igual en ambas submuestras.
#    (Una regla más agresiva dejaba ítems dicotómicos y una matriz policórica
#    casi singular en la submuestra A.)
unir_bajas <- function(x, min_n = 10) {
  k <- 1
  while (sum(x <= k) < min_n && k < 3) k <- k + 1
  y <- pmax(x, k)                      # categorías 1..k quedan en k
  as.integer(factor(y))                # renumera 1..m
}
Xc <- as.data.frame(lapply(X, unir_bajas))
n_cat <- sapply(Xc, function(x) length(unique(x)))

# 4. División de la muestra (estratificada por programa) -------------------------
set.seed(2026)
grupo <- integer(N)
for (p in unique(d$programa)) {
  idx <- which(d$programa == p)
  grupo[idx] <- sample(rep(1:2, length.out = length(idx)))
}
A <- Xc[grupo == 1, ]; B <- Xc[grupo == 2, ]
XA <- X[grupo == 1, ]; XB <- X[grupo == 2, ]
nA <- nrow(A); nB <- nrow(B)
equiv <- data.frame(
  variable = c("Programa: Administración", "Programa: Negocios Int.", "Programa: Contaduría",
               "Colegio bilingüe", "Colegio público", "Estrato (media)", "Índice de éxito (media)"),
  A = c(sapply(c("Administración de Empresas", "Negocios Internacionales", "Contaduría Pública"), function(p) mean(d$programa[grupo == 1] == p)),
        mean(d$bilingue[grupo == 1]), mean(d$publico[grupo == 1]), mean(d$estrato[grupo == 1]), mean(d$EXITO[grupo == 1])),
  B = c(sapply(c("Administración de Empresas", "Negocios Internacionales", "Contaduría Pública"), function(p) mean(d$programa[grupo == 2] == p)),
        mean(d$bilingue[grupo == 2]), mean(d$publico[grupo == 2]), mean(d$estrato[grupo == 2]), mean(d$EXITO[grupo == 2])))
equiv$p <- c(rep(chisq.test(table(d$programa, grupo))$p.value, 3),
             chisq.test(table(d$bilingue, grupo))$p.value, chisq.test(table(d$publico, grupo))$p.value,
             wilcox.test(d$estrato ~ grupo, exact = FALSE)$p.value, wilcox.test(d$EXITO ~ grupo, exact = FALSE)$p.value)

# 5. AFE en la submuestra A -----------------------------------------------------
RA <- suppressWarnings(polychoric(A)$rho)
kmo_A  <- KMO(RA)
bart_A <- cortest.bartlett(RA, n = nA)
det_A  <- det(RA)
set.seed(2026)
pa <- suppressWarnings(suppressMessages(
  fa.parallel(RA, n.obs = nA, fm = "minres", fa = "fa", n.iter = 100, plot = FALSE, quant = .95)))
k_pa <- pa$nfact
efa_fit <- function(R, k) suppressWarnings(suppressMessages(fa(R, nfactors = k, n.obs = nA, fm = "minres", rotate = "oblimin")))
efa_pa <- efa_fit(RA, k_pa)          # solución que sugiere el análisis paralelo
efa_5  <- efa_fit(RA, 5)             # solución teórica de 5 factores (comparación)

# Asignación de ítems: carga principal ≥ 0,40 y diferencia ≥ 0,20 con la segunda
asignar <- function(L) {
  L <- unclass(L); a <- abs(L)
  prim <- apply(a, 1, which.max); c1 <- apply(a, 1, max)
  c2 <- apply(a, 1, function(z) sort(z, decreasing = TRUE)[2])
  data.frame(codigo = rownames(L), factor = prim, carga = L[cbind(seq_len(nrow(L)), prim)],
             segunda = c2, retenido = c1 >= .40 & (c1 - c2) >= .20)
}
asig_pa <- asignar(efa_pa$loadings)
asig_5  <- asignar(efa_5$loadings)

# 6. AFC en la submuestra B -----------------------------------------------------
sintaxis <- function(grupos) paste(sapply(names(grupos), function(f) paste(f, "=~", paste(grupos[[f]], collapse = " + "))), collapse = "\n")
g_teo <- split(it$codigo, factor(fac_cod[it$dimension], unique(fac_cod)))
g_uno <- list(F1 = it$codigo)
g_afe <- with(asig_pa[asig_pa$retenido, ], split(codigo, paste0("F", factor)))
g_afe <- g_afe[sapply(g_afe, length) >= 3]

afc <- function(grupos) {
  vars <- unlist(grupos)
  suppressWarnings(cfa(sintaxis(grupos), data = B[vars], ordered = vars, estimator = "WLSMV", std.lv = TRUE))
}
ind <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust", "tli.robust", "rmsea.robust",
         "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "srmr")
ajuste <- function(fit) {
  v <- fitMeasures(fit, ind)
  if (anyNA(v)) {   # respaldo con versiones escaladas
    alt <- fitMeasures(fit, sub("robust", "scaled", ind)); v[is.na(v)] <- alt[is.na(v)]
  }
  v
}
neg <- it$codigo[it$invertido]
afc_met <- function(grupos) {   # mismo modelo + factor de método ortogonal para los ítems negativos
  vars <- unlist(grupos); ng <- intersect(neg, vars)
  sx <- paste(sintaxis(grupos), "\nMET =~", paste(ng, collapse = " + "),
              "\nMET ~~", paste0("0*", names(grupos), collapse = " + "))
  suppressWarnings(cfa(sx, data = B[vars], ordered = vars, estimator = "WLSMV", std.lv = TRUE))
}
modelos <- list(`M1. Un factor` = g_uno, `M2. Cinco dimensiones teóricas` = g_teo)
fits <- lapply(modelos, afc)
fits[["M3. Cinco dimensiones + factor de método"]] <- afc_met(g_teo)
if (length(g_afe) >= 2) fits[[paste0("M4. Estructura del AFE (", length(g_afe), " factores)")]] <- afc(g_afe)
g_rev <- g_teo; g_rev$PR <- setdiff(g_rev$PR, "PR5")              # modelo revisado: sin "recursos de apoyo"
fits[["M5. Revisado: cinco dimensiones sin PR5 + factor de método"]] <- afc_met(g_rev)
aj <- t(sapply(fits, ajuste))
conv <- sapply(fits, lavInspect, "converged")
# Caso Heywood: alguna carga estandarizada > 1 (varianza residual negativa)
heywood <- sapply(fits, function(f) { ss <- standardizedSolution(f); any(abs(ss$est.std[ss$op == "=~"]) > 1) })

# Sensibilidad: mismos modelos con los ítems como continuos (MLR, escala 1–5 original)
afc_mlr <- function(grupos, met = FALSE) {
  vars <- unlist(grupos); sx <- sintaxis(grupos)
  if (met) sx <- paste(sx, "\nMET =~", paste(intersect(neg, vars), collapse = " + "),
                       "\nMET ~~", paste0("0*", names(grupos), collapse = " + "))
  f <- suppressWarnings(cfa(sx, data = XB[vars], estimator = "MLR", std.lv = TRUE))
  fitMeasures(f, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))
}
aj_mlr <- rbind(afc_mlr(g_uno), afc_mlr(g_teo), afc_mlr(g_teo, TRUE),
                if (length(g_afe) >= 2) afc_mlr(g_afe) else NA, afc_mlr(g_rev, TRUE))
rownames(aj_mlr) <- names(fits)

# Adecuación muestral con alternativas más estables (la policórica en n = 174 es casi singular)
kmo_pearson_A <- KMO(cor(XA))$MSA
kmo_poly_total <- KMO(suppressWarnings(polychoric(Xc)$rho))$MSA
min_eig_A <- min(eigen(RA)$values)

# 7. Validez y confiabilidad del modelo teórico ----------------------------------
validez <- function(fit) {
  ss <- standardizedSolution(fit); l <- ss[ss$op == "=~", ]
  facs <- unique(l$lhs)
  ave <- sapply(facs, function(f) mean(l$est.std[l$lhs == f]^2))
  cr  <- sapply(facs, function(f) { x <- l$est.std[l$lhs == f]; sum(x)^2 / (sum(x)^2 + sum(1 - x^2)) })
  om  <- tryCatch(unlist(compRelSEM(fit, ord.scale = TRUE)), error = function(e) setNames(rep(NA, length(facs)), facs))
  phi <- lavInspect(fit, "cor.lv")
  list(cargas = l, ave = ave, cr = cr, omega = om[facs], phi = phi)
}
val_teo <- validez(fits[["M2. Cinco dimensiones teóricas"]])
val_rev <- validez(fits[["M5. Revisado: cinco dimensiones sin PR5 + factor de método"]])
htmt_teo <- tryCatch(semTools::htmt(sintaxis(g_teo), data = B, ordered = it$codigo),
                     error = function(e) tryCatch(semTools::htmt(sintaxis(g_teo), sample.cov = lavCor(B, ordered = it$codigo)),
                                                  error = function(e2) NULL))
# Alfa ordinal por dimensión (submuestra B)
alfa_dim <- sapply(g_teo, function(v) { R <- suppressWarnings(polychoric(B[v])$rho); k <- length(v); r <- mean(R[lower.tri(R)]); k * r / (1 + (k - 1) * r) })

if (sys.nframe() == 0) {
  options(width = 200)
  cat("Categorías tras unir:\n"); print(n_cat)
  cat(sprintf("\nnA = %d, nB = %d | KMO = %.3f | Bartlett chi2(%d) = %.1f, p = %.3g | det = %.2e | factores PA = %d\n",
              nA, nB, kmo_A$MSA, bart_A$df, bart_A$chisq, bart_A$p.value, det_A, k_pa))
  cat("\nMSA por ítem:\n"); print(round(kmo_A$MSAi, 2))
  cat("\nAutovalores observados vs simulados (primeros 8):\n"); print(round(cbind(obs = pa$fa.values[1:8], sim = pa$fa.sim[1:8]), 3))
  cat("\nAFE solución PA:\n"); print(efa_pa$loadings, cutoff = .30, sort = TRUE); print(round(efa_pa$Phi, 2))
  print(asig_pa)
  cat("\nAFE 5 factores:\n"); print(efa_5$loadings, cutoff = .30, sort = TRUE); print(round(efa_5$Phi, 2))
  cat("\nAjuste AFC (B):\n"); print(round(aj, 3)); print(conv); print(heywood)
  cat("\nValidez modelo teórico:\n"); print(round(rbind(AVE = val_teo$ave, CR = val_teo$cr, omega = val_teo$omega, alfa_ord = alfa_dim), 3))
  print(round(val_teo$phi, 2)); print(htmt_teo)
  print(val_teo$cargas[, c("lhs", "rhs", "est.std", "se", "pvalue")], digits = 3)
  print(round(equiv[, -1], 3))
}
