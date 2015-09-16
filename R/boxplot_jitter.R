# example boxplot with jitter
library(dplyr)
library(tidyr)
library(ggplot2)


n <- 1000
set.seed(123)
d1 <-
  data_frame(A = rnorm(n, -3, 3), B = rnorm(n, 5, 1), C = rexp(n, 1)) %>%
  gather(group, value)

# boxplot
ggplot(d1) + aes(x=group, y=value, fill=group) +
  geom_jitter(position = position_jitter(width = .2), alpha=0.2, size=0.8) +
  geom_boxplot(outlier.size=0, alpha=0.1) +
  theme_minimal()

# extra violinplot
ggplot(d1) + aes(x=group, y=value, fill=group) +
  geom_jitter(position = position_jitter(width = .2), alpha=0.2, size=0.8) +
  geom_violin(alpha=0.1) +
  geom_boxplot(outlier.size=0, alpha=0.1) +
  theme_minimal()
