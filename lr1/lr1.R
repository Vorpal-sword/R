library(ggplot2)
library(dplyr)
library(outliers)
library(rstatix)
library(EnvStats)

# Завантаження даних
data(mpg)
df <- mpg

summary(df$hwy)
range(df$hwy)


hist(df$hwy, main="Гістограма для hwy", xlab="hwy", col="lightblue", border="black")

# Використання ggplot2
ggplot(df, aes(x=hwy)) +
  geom_histogram(binwidth=2, fill="blue", alpha=0.5) +
  labs(title="Гістограма розподілу hwy")

boxplot(df$hwy, main="Boxplot для hwy", col="lightgreen")
outliers <- boxplot.stats(df$hwy)$out
outliers

which(df$hwy %in% outliers)

identify_outliers(df, hwy)

mtext(paste("Викиди:", paste(outliers, collapse=", ")))

Q1 <- quantile(df$hwy, 0.25)
Q3 <- quantile(df$hwy, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers_percentiles <- df$hwy[df$hwy < lower_bound | df$hwy > upper_bound]
outliers_percentiles

z_scores <- scale(df$hwy)
outliers_z <- df$hwy[abs(z_scores) > 3]
outliers_z

median_hwy <- median(df$hwy)
mad_hwy <- mad(df$hwy)
outliers_hampel <- df$hwy[abs(df$hwy - median_hwy) > 2.5 * mad_hwy]
outliers_hampel

qqnorm(df$hwy)
qqline(df$hwy, col="red")

grubbs.test(df$hwy)

set.seed(123)
sample_hwy <- sample(df$hwy, 30)
dixon.test(sample_hwy)


rosnerTest(df$hwy, k=5, alpha = 0.1)
