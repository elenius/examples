# funktion för att kontrollera hur den empiriska
# fördelningen är mot en normalfördelning
# använder hexbin istället för enskilda observationer

norm_plot <- function(x) {

  library(ggplot2)
  library(gridExtra)
  library(dplyr)
  library(tidyr)

  n <- length(x)
  i <- seq(1, n, by=1)
  prob.seq <- (i-0.5)/n
  inv.cdf <- qnorm(prob.seq, 0, 1)
  x.normalize <- (x - mean(x))/sd(x)
  df <- data_frame(empirical=sort(x),
                   empirical.normalized=sort(x.normalize),
                   normal=inv.cdf)

  dfl <-
    df %>% dplyr::select(-empirical) %>%
    gather(., cdf, value) %>%
    tbl_df

  a <- ggplot(df) + aes(x=normal, y=empirical.normalized) + stat_binhex() +
    geom_smooth(method="lm") + theme_minimal() +
    ggtitle("QQ-Plot")

  b <- ggplot(dfl) + aes(x=value, colour=cdf, fill=cdf) + stat_ecdf() +
    theme_minimal() + theme(legend.title=element_blank(), legend.position="top") +
    ggtitle("ECDF")

  c <- ggplot(dfl) + aes(x=value, colour=cdf, fill=cdf) + geom_density(alpha=0.3) +
    theme_minimal() + theme(legend.title=element_blank(), legend.position="top") +
    ggtitle("PDF")

  grid.arrange(a, b, c, ncol=2)

  # format(object.size(a), units="Mb")
  # format(object.size(b), units="Mb")
  # format(object.size(c), units="Mb")
}

# Exempel
set.seed(123)
x <- runif(10000,0,100) # sample data

# pdf("test.pdf", width=10, height=10)
norm_plot(x)
# graphics.off()
