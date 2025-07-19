# Senaryo 1: Poisson - Exponential Bileşik Dağılım


lambda_poisson <- 5000
beta_exponential <- 1/10000
n_simulasyon <- 10000

# a) Toplam Hasarın Simülasyonu
toplam_hasarlar <- numeric(n_simulasyon)

for (i in 1:n_simulasyon) {
 
  n_hasar <- rpois(1, lambda = lambda_poisson)

  hasar_buyuklukleri <- rexp(n_hasar, rate = beta_exponential)

  toplam_hasarlar[i] <- sum(hasar_buyuklukleri)
}

# b) Histogram ve Yoğunluk Eğrisi
hist(toplam_hasarlar, freq = FALSE, main = "Toplam Hasar Simülasyonu",
     xlab = "Toplam Hasar", ylab = "Yoğunluk")
lines(density(toplam_hasarlar), col = "red")

# c) Simülasyonla İstatistiksel Değerlerin Hesaplanması
ortalama_sim <- mean(toplam_hasarlar)
varyans_sim <- var(toplam_hasarlar)
yuzdelik_90_sim <- quantile(toplam_hasarlar, 0.90)
yuzdelik_99_5_sim <- quantile(toplam_hasarlar, 0.995)

cat("Simülasyon Sonuçları:\n")
cat("Ortalama Toplam Hasar:", ortalama_sim, "\n")
cat("Toplam Hasarın Varyansı:", varyans_sim, "\n")
cat("90. Yüzdelik Dilim (10 yılda bir):", yuzdelik_90_sim, "\n")
cat("99.5 Yüzdelik Dilim (200 yılda bir):", yuzdelik_99_5_sim, "\n")

# d) Analitik Olarak İstatistiksel Değerlerin Hesaplanması

# Bileşik Poisson dağılımının özellikleri:
# E[S] = E[N] * E[X]
# Var[S] = E[N] * Var[X] + Var[N] * (E[X])^2

ortalama_N <- lambda_poisson
ortalama_X <- 1 / beta_exponential
varyans_N <- lambda_poisson
varyans_X <- 1 / (beta_exponential^2)

ortalama_analitik <- ortalama_N * ortalama_X
varyans_analitik <- ortalama_N * varyans_X + varyans_N * (ortalama_X)^2


standart_sapma_analitik <- sqrt(varyans_analitik)
z_90 <- qnorm(0.90)
z_99_5 <- qnorm(0.995)

yuzdelik_90_analitik_yaklasik <- ortalama_analitik + z_90 * standart_sapma_analitik
yuzdelik_99_5_analitik_yaklasik <- ortalama_analitik + z_99_5 * standart_sapma_analitik

cat("\nAnalitik Sonuçlar (Yaklaşık Yöntemlerle):\n")
cat("Ortalama Toplam Hasar:", ortalama_analitik, "\n")
cat("Toplam Hasarın Varyansı:", varyans_analitik, "\n")
cat("90. Yüzdelik Dilim (10 yılda bir, Yaklaşık):", yuzdelik_90_analitik_yaklasik, "\n")
cat("99.5 Yüzdelik Dilim (200 yılda bir, Yaklaşık):", yuzdelik_99_5_analitik_yaklasik, "\n")

# e) Risk Sermayesinin Hesaplanması ve Karşılaştırılması


risk_sermayesi_simulasyon <- yuzdelik_99_5_sim
risk_sermayesi_analitik <- yuzdelik_99_5_analitik_yaklasik

cat("\nRisk Sermayesi Karşılaştırması:\n")
cat("Simülasyon ile Risk Sermayesi:", risk_sermayesi_simulasyon, "\n")
cat("Analitik Yaklaşım ile Risk Sermayesi (Yaklaşık):", risk_sermayesi_analitik, "\n")

# Karşılaştırma
cat("\nSimülasyon ve Analitik Risk Sermayesi Arasındaki Fark:",
    abs(risk_sermayesi_simulasyon - risk_sermayesi_analitik), "\n")
    