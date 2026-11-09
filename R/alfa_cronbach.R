# Confiabilidad del índice de éxito percibido en EMI ---------------------------
# Calcula el alfa de Cronbach (fórmula y verificación con psych), sus intervalos
# de confianza, el diagnóstico por ítem, el omega de McDonald y las versiones
# ordinales (correlaciones policóricas). Se usa desde la sección 3 del artículo;
# también puede correrse solo: Rscript R/alfa_cronbach.R

if (!exists("d")) source("R/prep.R", encoding = "UTF-8")

# 1. Ítems del índice ----------------------------------------------------------
#    barrera_r = 6 - barrera (ítem redactado en negativo, se invierte)
items5 <- c("conceptos", "barrera_r", "ingles_desemp", "buen_desemp", "aprendi_igual")
X <- as.matrix(d[items5])
k <- ncol(X)                     # número de ítems
n <- nrow(X)                     # número de estudiantes
stopifnot(!anyNA(X))             # no hay datos faltantes

# 2. Alfa de Cronbach con la fórmula ------------------------------------------
#    alfa = k/(k-1) * (1 - suma de varianzas de los ítems / varianza del total)
alfa_fun <- function(M) { k <- ncol(M); k / (k - 1) * (1 - sum(apply(M, 2, var)) / var(rowSums(M))) }
var_items <- apply(X, 2, var)
alfa <- alfa_fun(X)

# 3. Alfa estandarizado (a partir de la correlación media entre ítems) ---------
R <- cor(X)
r_media <- mean(R[lower.tri(R)])
alfa_std <- (k * r_media) / (1 + (k - 1) * r_media)

# 4. Intervalos de confianza del 95 % ------------------------------------------
#    (a) Feldt (1965), basado en la distribución F
ic_feldt <- c(1 - (1 - alfa) * qf(0.975, n - 1, (n - 1) * (k - 1)),
              1 - (1 - alfa) * qf(0.025, n - 1, (n - 1) * (k - 1)))

# 5. Omega de McDonald (modelo de un factor, máxima verosimilitud) -------------
#    omega = (suma de cargas)^2 / [(suma de cargas)^2 + suma de unicidades]
omega_fun <- function(M, cor_mat = NULL) {
  f1 <- suppressWarnings(suppressMessages(
    if (is.null(cor_mat)) psych::fa(M, nfactors = 1, fm = "ml")
    else psych::fa(cor_mat, nfactors = 1, fm = "ml", n.obs = nrow(M))))
  l <- as.numeric(f1$loadings[, 1])
  list(omega = sum(l)^2 / (sum(l)^2 + sum(1 - l^2)), cargas = l, fa = f1)
}
om <- omega_fun(X)
omega <- om$omega
cargas <- setNames(om$cargas, items5)
ajuste <- om$fa

#    (b) Bootstrap percentil del alfa y del omega (2.000 remuestreos)
set.seed(2026)
B <- 2000
boot_rel <- t(replicate(B, {
  Xb <- X[sample(n, replace = TRUE), ]
  c(alfa = alfa_fun(Xb), omega = omega_fun(Xb)$omega)
}))
ic_boot_alfa  <- quantile(boot_rel[, "alfa"],  c(0.025, 0.975))
ic_boot_omega <- quantile(boot_rel[, "omega"], c(0.025, 0.975))

# 6. Versiones ordinales (correlaciones policóricas) ---------------------------
#    Tratan las respuestas Likert como ordinales y no como continuas.
poli <- suppressWarnings(psych::polychoric(as.data.frame(X))$rho)
kp <- ncol(poli); rp <- mean(poli[lower.tri(poli)])
alfa_ord  <- (kp * rp) / (1 + (kp - 1) * rp)
omega_ord <- omega_fun(X, cor_mat = poli)$omega

# 7. Diagnóstico por ítem -----------------------------------------------------
diag_items <- data.frame(
  item          = items5,
  varianza      = var_items,
  carga         = cargas,
  r_item_total  = sapply(items5, function(i) cor(X[, i], rowSums(X[, setdiff(items5, i)]))),  # corregida
  alfa_sin_item = sapply(items5, function(i) alfa_fun(X[, setdiff(items5, i)]))
)

# 8. Verificación con el paquete psych ----------------------------------------
ps <- suppressWarnings(psych::alpha(as.data.frame(X), check.keys = FALSE))
stopifnot(abs(ps$total$raw_alpha - alfa) < 1e-10,
          abs(ps$total$std.alpha - alfa_std) < 1e-10)

# Salida en consola cuando se corre el script solo ------------------------------
if (sys.nframe() == 0) {
  cat(sprintf("n = %d, k = %d\n", n, k))
  cat(sprintf("Alfa = %.4f (psych %.4f) | IC Feldt [%.3f, %.3f] | IC boot [%.3f, %.3f]\n",
              alfa, ps$total$raw_alpha, ic_feldt[1], ic_feldt[2], ic_boot_alfa[1], ic_boot_alfa[2]))
  cat(sprintf("Alfa estandarizado = %.4f | correlación media = %.4f\n", alfa_std, r_media))
  cat(sprintf("Omega = %.4f | IC boot [%.3f, %.3f]\n", omega, ic_boot_omega[1], ic_boot_omega[2]))
  cat(sprintf("Alfa ordinal = %.4f | Omega ordinal = %.4f\n", alfa_ord, omega_ord))
  cat(sprintf("Ajuste 1 factor: chi2(%d) = %.2f, p = %.4f, RMSEA = %.3f, TLI = %.3f\n",
              ajuste$dof, ajuste$STATISTIC, ajuste$PVAL, ajuste$RMSEA[1], ajuste$TLI))
  print(diag_items, digits = 3, row.names = FALSE)
}
