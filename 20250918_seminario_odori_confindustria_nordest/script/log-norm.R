set.seed(123)
n <- 720          # frequency 5 s     #<<
meanlog <- 0.1    # avg               #<<
sdlog <- 0.33     # standard dev      #<<
pct <- 0.99       # fixed percentile  #<<

# generate data log-norm
log_data <- rlnorm(n,
                   meanlog = meanlog,
                   sdlog = sdlog)

# plot histogram
hist(log_data, breaks = 50, 
     probability = TRUE,
     main = NULL,
     xlab = NULL, ylab = NULL,
     col = "skyblue")

# plot density curve
lines(density(log_data),
      col = "red", lwd = 1.5)

# calculate Pct99, quantile
ql99 <- qlnorm(pct,               #<< 
               meanlog = meanlog, #<<
               sdlog = sdlog)     #<<

# plot line Pct99
abline(v = ql99, 
       col = "purple", 
       lty="dashed") 

# plot text Pct99
text(ql99 + 0.2, 0.4, 
     labels = "Pct99", col = "purple")


###########################


# sample mean in original scale
# for pollutant monitored with high frequency
m_hf <- 10^meanlog
m_hf

# dev st in original scale
# for pollutant monitored with high frequency
s_hf <- 10^sdlog
s_hf

# calculate CV adimensional
cv <- s_hf/m_hf
cv 
# serve per stimare devizione standard di inquinanti low frequency
# assunto di base è che cv sia rappresentativo di inquinante low frequency
# s_lf = cv * m_lf

# Pct99 in original scale
# for pollutant monitored with high frequency
q99_hf <- 10^ql99
q99_hf

# raporto tra Pct99 e media
r99 <- q99_hf/m_hf
r99

# utilizzo il rapporto per stimare P99 a partire dalla media nota
# per gli inquinanti low freq
# r99 * m_lf 
