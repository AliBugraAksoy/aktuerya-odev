# Senaryo 2: Negative Binomial - Pareto Bileşik Dağılım

size_negbin <- 20
prob_negbin <- 0.8
alpha_pareto <- 2.5
xm_pareto <- 5000
n_simulasyon <- 10000

rpareto <- function(n, alpha, xm) {
  u <- runif(n)
  return(xm / (u^(1/alpha)))
}

# a) Toplam Hasarın Simülasyonu
toplam_hasarlar_nb_pareto <- numeric(n_simulasyon)

for (i in 1:n_simulasyon) {
  n_hasar_nb <- rnbinom(1, size = size_negbin, prob = prob_negbin)

  hasar_buyuklukleri_pareto <- rpareto(n_hasar_nb, alpha = alpha_pareto, xm = xm_pareto)

  toplam_hasarlar_nb_pareto[i] <- sum(hasar_buyuklukleri_pareto)
}

# b) Histogram ve Yoğunluk Eğrisi
hist(toplam_hasarlar_nb_pareto, freq = FALSE, main = "Toplam Hasar Simülasyonu (NegBin-Pareto)",
     xlab = "Toplam Hasar", ylab = "Yoğunluk")
lines(density(toplam_hasarlar_nb_pareto), col = "blue")

# c) Simülasyonla İstatistiksel Değerlerin Hesaplanması
ortalama_sim_nb_pareto <- mean(toplam_hasarlar_nb_pareto)
varyans_sim_nb_pareto <- var(toplam_hasarlar_nb_pareto)
yuzdelik_90_sim_nb_pareto <- quantile(toplam_hasarlar_nb_pareto, 0.90)
yuzdelik_99_5_sim_nb_pareto <- quantile(toplam_hasarlar_nb_pareto, 0.995)

cat("\nSimülasyon Sonuçları (Negative Binomial - Pareto):\n")
cat("Ortalama Toplam Hasar:", ortalama_sim_nb_pareto, "\n")
cat("Toplam Hasarın Varyansı:", varyans_sim_nb_pareto, "\n")
cat("90. Yüzdelik Dilim:", yuzdelik_90_sim_nb_pareto, "\n")
cat("99.5 Yüzdelik Dilim:", yuzdelik_99_5_sim_nb_pareto, "\n")

# d) Analitik Olarak E[S] ve Var[S]'nin Hesaplanması

# Negative Binomial dağılımı için beklenen değer ve varyans:
# E[N] = size * (1 - prob) / prob
# Var[N] = size * (1 - prob) / (prob^2)

ortalama_N_nb <- size_negbin * (1 - prob_negbin) / prob_negbin
varyans_N_nb <- size_negbin * (1 - prob_negbin) / (prob_negbin^2)

cat("\nNegative Binomial Dağılımı Analitik Değerleri:\n")
cat("E[N]:", ortalama_N_nb, "\n")
cat("Var[N]:", varyans_N_nb, "\n")

# Pareto dağılımı için beklenen değer ve varyans (alpha > 1 ve alpha > 2 için):
# E[X] = alpha * xm / (alpha - 1)  (alpha > 1)
# Var[X] = (xm^2 * alpha) / ((alpha - 1)^2 * (alpha - 2))  (alpha > 2)

if (alpha_pareto > 1) {
  ortalama_X_pareto <- alpha_pareto * xm_pareto / (alpha_pareto - 1)
  cat("Pareto Dağılımı Analitik Değerleri:\n")
  cat("E[X]:", ortalama_X_pareto, "\n")
} else {
  ortalama_X_pareto <- NA
  cat("Pareto dağılımının beklenen değeri tanımlı değil (alpha <= 1).\n")
}

if (alpha_pareto > 2) {
  varyans_X_pareto <- (xm_pareto^2 * alpha_pareto) / ((alpha_pareto - 1)^2 * (alpha_pareto - 2))
  cat("Var[X]:", varyans_X_pareto, "\n")
} else {
  varyans_X_pareto <- NA
  cat("Pareto dağılımının varyansı tanımlı değil (alpha <= 2).\n")
}

# Bileşik dağılım için beklenen değer ve varyans:
# E[S] = E[N] * E[X]
# Var[S] = E[N] * Var[X] + Var[N] * (E[X])^2

if (!is.na(ortalama_X_pareto)) {
  ortalama_analitik_nb_pareto <- ortalama_N_nb * ortalama_X_pareto
  cat("\nBileşik Dağılım Analitik Değerleri:\n")
  cat("E[S]:", ortalama_analitik_nb_pareto, "\n")
} else {
  ortalama_analitik_nb_pareto <- NA
  cat("\nBileşik Dağılım Analitik Değeri (E[S]): Tanımlı değil (Pareto E[X] tanımsız).\n")
}

if (!is.na(ortalama_X_pareto) && !is.na(varyans_X_pareto)) {
  varyans_analitik_nb_pareto <- ortalama_N_nb * varyans_X_pareto + varyans_N_nb * (ortalama_X_pareto)^2
  cat("Var[S]:", varyans_analitik_nb_pareto, "\n")
} else {
  varyans_analitik_nb_pareto <- NA
  cat("Bileşik Dağılım Analitik Değeri (Var[S]): Tanımlı değil (Pareto E[X] veya Var[X] tanımsız).\n")
}

# e) Simülasyon ve Analitik Yaklaşımların Karşılaştırılması
cat("\nSimülasyon ve Analitik Yaklaşımların Karşılaştırılması:\n")
cat("Simülasyon Ortalaması (E[S]):", ortalama_sim_nb_pareto, "\n")
cat("Analitik Ortalama (E[S]):", ortalama_analitik_nb_pareto, "\n")
cat("Simülasyon Varyansı (Var[S]):", varyans_sim_nb_pareto, "\n")
cat("Analitik Varyansı (Var[S]):", varyans_analitik_nb_pareto, "\n")




