library(ggplot2)
df <- varianceDFNormal

pcorr <-ggplot(df, aes(x = corrSeq)) +
  geom_line(aes(y = sobol, color = "Sobol"), linetype = "solid") +
  geom_line(aes(y = halton, color = "Halton"), linetype = "solid") +
  geom_line(aes(y = pseudo, color = "Pseudo"), linetype = "solid") +
  labs(title = "Variance of Estimate by correlation",
       x = "Correlation",
       y = "Varianceo of estimate") +
  scale_color_manual(values =c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red")) +
  theme_minimal()

ggsave("VarianceByCorrelation.png", plot = pcorr, width = 10, height = 8)