# Preparación de datos y análisis -------------------------------------------
# Lee la base cruda (que NO se sube al repositorio) desde la carpeta superior
# del proyecto y deja listos los objetos que usan los capítulos.

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2)
  library(knitr); library(kableExtra); library(sandwich); library(lmtest)
  library(MASS); library(boot)
})

# Formato de números en español
f  <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")
fp <- function(p) ifelse(p < .001, "< 0,001", f(p, 3))
pct <- function(x, d = 0) paste0(f(100 * x, d), " %")

# Lectura ---------------------------------------------------------------------
archivo <- list.files("..", pattern = "^Uso del ingl.*\\.xlsx$", full.names = TRUE)[1]
if (is.na(archivo)) stop("No se encontró el Excel de la encuesta en la carpeta superior.")
raw <- read_excel(archivo, .name_repair = "minimal")
names(raw) <- make.unique(names(raw))
col <- function(i) raw[[i]]   # índices de columna (1 = primera)

n_raw  <- nrow(raw)
n_cols <- ncol(raw)
n_vacias <- sum(sapply(raw, function(x) all(is.na(x))))

lk <- c("Totalmente en desacuerdo" = 1, "Parcialmente en desacuerdo" = 2, "Indiferente" = 3,
        "Parcialmente de acuerdo" = 4, "Totalmente de acuerdo" = 5)

d0 <- tibble(
  id        = col(1),
  inicio    = as.POSIXct(col(2)),
  correo    = tolower(trimws(col(4))),
  consent   = col(9),
  programa  = col(15),
  semestre  = as.numeric(col(18)),
  colegio   = col(21),
  bil_txt   = col(24),
  genero    = col(27),
  nacim     = as.numeric(col(30)),
  madre     = col(33),
  padre     = col(36),
  estrato   = as.numeric(col(39)),
  nivel_txt = col(44),
  conceptos     = unname(lk[col(87)]),
  barrera       = unname(lk[col(93)]),
  ingles_desemp = unname(lk[col(96)]),
  buen_desemp   = unname(lk[col(102)]),
  aprendi_igual = unname(lk[col(105)]),
  no_capaz      = unname(lk[col(70)]),
  comodo        = unname(lk[col(133)]),
  recursos      = unname(lk[col(122)]),
  expresar1     = col(73),
  expresar2     = col(99)
)
nombre_expresar <- names(raw)[73]

# Depuración ------------------------------------------------------------------
n_no_consent <- sum(d0$consent == "No")
d1 <- filter(d0, consent == "Sí")

dup_tab   <- d1 |> count(correo) |> filter(n > 1)
n_dup_est <- nrow(dup_tab)
oleadas   <- d1 |> filter(correo %in% dup_tab$correo) |> mutate(mes = format(inicio, "%Y-%m")) |> count(mes)
d2 <- d1 |> arrange(inicio) |> distinct(correo, .keep_all = TRUE)
n_dup_quit <- nrow(d1) - nrow(d2)

n_lmc <- sum(d2$programa == "Lenguas Modernas y Cultura")
d <- filter(d2, programa != "Lenguas Modernas y Cultura")
N <- nrow(d)

n_nacim_mal  <- sum(!(d1$nacim >= 1990 & d1$nacim <= 2010), na.rm = TRUE)
n_pub_bil    <- sum(d$colegio == "Público" & d$bil_txt == "Sí")
n_sem_raro   <- sum(d$semestre < 7)

# Variables -------------------------------------------------------------------
eo <- c("Primaria" = 1, "Bachillerato" = 2, "Técnico/Tecnólogo" = 3, "Pregrado" = 4, "Posgrado" = 5)
d <- d |> mutate(
  barrera_r   = 6 - barrera,
  no_capaz_r  = 6 - no_capaz,
  publico     = as.integer(colegio == "Público"),
  bilingue    = as.integer(bil_txt == "Sí"),
  edu_max     = pmax(eo[madre], eo[padre]),
  padres_univ = as.integer(edu_max >= 4),
  nivel_ing   = unname(c("Malo" = 1, "Regular" = 2, "Bueno" = 3, "Excelente" = 4)[nivel_txt]),
  estr_g      = cut(estrato, c(0, 2, 4, 6), labels = c("1–2", "3–4", "5–6")),
  edad        = ifelse(nacim >= 1990 & nacim <= 2010, 2026 - nacim, NA),
  programa    = factor(programa, levels = c("Administración de Empresas", "Negocios Internacionales", "Contaduría Pública"))
)

items5 <- c("conceptos", "barrera_r", "ingles_desemp", "buen_desemp", "aprendi_igual")
items7 <- c(items5, "no_capaz_r", "comodo")
d$EXITO <- rowMeans(d[items5])

alpha <- function(m) { m <- as.matrix(m); k <- ncol(m); k / (k - 1) * (1 - sum(apply(m, 2, var)) / var(rowSums(m))) }
a5 <- alpha(d[items5]); a7 <- alpha(d[items7])
a_sin <- sapply(items5, function(i) alpha(d[setdiff(items5, i)]))
r_it  <- sapply(items5, function(i) cor(d[[i]], rowMeans(d[setdiff(items5, i)])))
skew  <- function(x) { n <- length(x); m <- mean(x); s <- sd(x); n / ((n - 1) * (n - 2)) * sum(((x - m) / s)^3) }

# Pruebas por grupo -----------------------------------------------------------
mw <- function(v) {
  g <- split(d$EXITO, d[[v]]); t <- wilcox.test(g[[2]], g[[1]], exact = FALSE)
  r <- 2 * t$statistic / (length(g[[1]]) * length(g[[2]])) - 1
  list(m0 = mean(g[[1]]), m1 = mean(g[[2]]), p = t$p.value, r = unname(r),
       a0 = mean(g[[1]] >= 4), a1 = mean(g[[2]] >= 4), n0 = length(g[[1]]), n1 = length(g[[2]]))
}
kw <- function(v) {
  dd <- d[!is.na(d[[v]]), ]; t <- kruskal.test(dd$EXITO, factor(dd[[v]])); k <- length(unique(dd[[v]]))
  list(H = unname(t$statistic), p = t$p.value, eps2 = unname((t$statistic - k + 1) / (nrow(dd) - k)))
}
t_bil <- mw("bilingue"); t_pub <- mw("publico"); t_pad <- mw("padres_univ")
t_pub$r <- -t_pub$r
k_estr <- kw("estrato"); k_edu <- kw("edu_max"); k_prog <- kw("programa"); k_gen <- kw("genero")
estr_tab <- d |> group_by(estr_g) |> summarise(n = n(), m = mean(EXITO), s = sd(EXITO), a = mean(EXITO >= 4))
prog_tab <- d |> group_by(programa) |> summarise(n = n(), m = mean(EXITO))
gen_tab  <- d |> group_by(genero) |> summarise(n = n(), m = mean(EXITO))
edu_rng  <- d |> group_by(edu_max) |> summarise(m = mean(EXITO)) |> pull(m) |> range()

# Modelos ---------------------------------------------------------------------
dm <- filter(d, genero != "Otro")
rob <- function(m) coeftest(m, vcov = vcovHC(m, type = "HC3"))
m1 <- lm(EXITO ~ publico + bilingue + estrato + edu_max, dm)
m2 <- lm(EXITO ~ publico + bilingue + estrato + edu_max + programa + genero + semestre, dm)
m3 <- lm(EXITO ~ publico + bilingue + estrato + edu_max + nivel_ing, dm)
ma <- lm(nivel_ing ~ publico + bilingue + estrato + edu_max, dm)
vifs <- car::vif(m1)
ci_rob <- function(m, v) { ct <- rob(m); ct[v, 1] + c(-1, 1) * qnorm(.975) * ct[v, 2] }

# Ordinal (sensibilidad)
dm$EX3 <- cut(dm$EXITO, c(0, 3.6, 4.6, 5), labels = c("bajo", "medio", "alto"), ordered_result = TRUE)
mo <- polr(EX3 ~ publico + bilingue + estrato + edu_max, dm, Hess = TRUE)
mo_ct <- coef(summary(mo))
mo_p  <- 2 * pnorm(-abs(mo_ct[, "t value"]))
or_bil <- exp(coef(mo)["bilingue"]); or_ci <- exp(coef(mo)["bilingue"] + c(-1, 1) * 1.96 * mo_ct["bilingue", "Std. Error"])

# Mediación bootstrap
set.seed(2026)
ind_fun <- function(data, i) {
  s <- data[i, ]
  a <- coef(lm(nivel_ing ~ bilingue + publico + estrato + edu_max, s))["bilingue"]
  b <- coef(lm(EXITO ~ nivel_ing + bilingue + publico + estrato + edu_max, s))["nivel_ing"]
  a * b
}
bt <- boot(dm, ind_fun, R = 2000)
ind_ci <- quantile(bt$t, c(.025, .975))
