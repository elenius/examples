library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(parallel)

# Data från metodbeskrivning ####
dat <-
  "S 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
1 5 3 7 5 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0
2 0 0 1 2 5 5 2 4 1 0 0 0 0 0 0 0 0 0 0 0 0
3 0 0 0 0 1 1 6 1 4 3 3 2 4 3 0 3 0 0 0 0 0
4 0 0 0 0 0 0 0 1 1 2 4 3 4 8 5 0 3 2 4 0 1
5 0 0 0 0 0 0 0 0 0 0 0 0 1 2 0 2 4 2 4 7 1" %>%
  read_delim(., delim = " ") %>%
  gather(age, value, -S) %>%
  mutate(Klassificering = ifelse(S %in% 1:3, "omoget", "moget")) %>%
  group_by(age, Klassificering) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  spread(Klassificering, value) %>%
  mutate(
    age = as.numeric(age),
    age_adjust = age + 0.5, # borde det skattas med den här åldern?
    p = moget / (moget + omoget),
    n = moget + omoget
  ) %>%
  rowwise %>%
  mutate(
    ci.lower = binom.test(moget, n)$conf.int[1],
    ci.upper = binom.test(moget, n)$conf.int[2],
    ci = ci.upper - ci.lower # bredd på ci
  )

# Resultat ####
mod.org <- glm(formula = cbind(moget, omoget) ~ age, family = binomial,
           data = dat)

mod.adjust <- glm(formula = cbind(moget, omoget) ~ age_adjust, family = binomial,
                  data = dat)

# Dataset för prediktion, använder detta för att plotta sannolikhetskurvan
new.dat <- tibble(age = seq(10, 30, .1))
new.dat$y <- predict(mod.org, new.dat, type="response")

# Plottar sannolikhetskurvan, med de faktiska observationerna
# (Stor orsäkerhet på sannolikhetskurvan)
ggplot(new.dat) + aes(x = age, y = y) + geom_line() + theme_bw() +
  scale_x_continuous(breaks = seq(10, 30, 5)) + ylab("Sannolikhet för moget knä") +
  xlab("Ålder") +
  geom_linerange(data = dat, aes(ymin =  ci.lower, ymax = ci.upper,  x = age, y = p), alpha = 0.2) +
  geom_point(data = dat, aes(x = age, y = p, size = 10/ci), alpha = 0.9, color = "black") +
  geom_text(data = dat, aes(x = age, y = p, label = n, size = 8/ci), alpha = 1, color = "white") +
  theme(legend.position = "none", legend.title = element_blank())

# Nedan är för bootstrap ####
# Primitiv funktion för logistisk regression
# över en likformig fördelning
logistic_integrate_uniform <- function(intercept, slope, lower, upper) {

  # Primitiv funktion för logistisk regression
  # http://mathworld.wolfram.com/SigmoidFunction.html
  Fx <- function(intercept, slope, x) {
    slope^(-1) * log(1 + exp(intercept + slope * x))
  }

  lower.prop <- Fx(intercept, slope, lower)
  upper.prop <- Fx(intercept, slope, upper)

  above <- (upper.prop - lower.prop) / (upper - lower)
  below <- 1 - above

  tibble(above = above, below = below, sum = above + below, cum.log = upper.prop - lower.prop)
}

# Funktion för bootstrap ####
boot_dedouit2012 <- function(boot.n = 1000, dat = dat) {

  sim_fun <- function() {
    moget <- rbinom(nrow(dat), dat$n, dat$p)
    omoget <- dat$n - moget
    cbind(moget, omoget, dat$age)
  }

  # generate bootstrapped dataset
  boot.data <- replicate(boot.n, sim_fun(), simplify = FALSE)

  # logistic regression
  boot.coef <-
    lapply(boot.data, function(x) {
      coef <- coef(glm(cbind(x[,1],  x[,2]) ~ x[,3], family = binomial))
      tibble(intercept = coef[1], slope = coef[2])
    }) %>%
    bind_rows
}

# Funktion för bootstrap (parallell) - mycket snabbare!
par_boot_dedouit2012 <- function(boot.n = 1000, dat = dat) {

  sim_fun <- function(x) {
    moget <- rbinom(nrow(dat), dat$n, dat$p)
    omoget <- dat$n - moget
    cbind(moget, omoget, age = dat$age)
  }

  # Skapar kluster, använder antal kärnor - 1
  cl <- makeCluster(detectCores() - 1)

  # Exportera funktioner till klustret
  clusterExport(cl, c("sim_fun", "dat", "tibble", "as_tibble"))

  # generate bootstrapped dataset
  boot.data <- parLapply(cl, 1:boot.n, sim_fun)

  clusterExport(cl, c("boot.data"))

  # logistic regression
  res <- parLapply(cl, boot.data, function(x) {
    coef <- coef(glm(cbind(x[,1], x[,2]) ~ x[,3], family = binomial))
    cbind(intercept = coef[1], slope = coef[2])
  }) %>%
    do.call(rbind, .)
  res <- as_tibble(res)
  stopCluster(cl)
  res
}


# Punktskattningarna
# Vuxna med omoget knä
point.vuxna <- logistic_integrate_uniform(intercept = coef(mod.org)[1],
                                          slope = coef(mod.org)[2], 18, 21)$below
point.vuxna
# nedan med age + 0.5
logistic_integrate_uniform(intercept = coef(mod.adjust)[1],
                           slope = coef(mod.adjust)[2], 18, 21)$below

# Barn med moget knä
point.barn <- logistic_integrate_uniform(intercept = coef(mod.org)[1],
                                         slope = coef(mod.org)[2], 15, 18)$above
point.barn
# nedan med age + 0.5
logistic_integrate_uniform(intercept = coef(mod.adjust)[1],
                           slope = coef(mod.adjust)[2], 15, 18)$above


# Bootstrap
system.time(db <- boot_dedouit2012(boot.n = 1000, dat = dat))

vuxna <- logistic_integrate_uniform(db$intercept, db$slope, 18, 21)$below
barn <- logistic_integrate_uniform(db$intercept, db$slope, 15, 18)$above

# Vuxna med omoget knä 95% bootstrap CI
quantile(vuxna, c(0.025, 0.975))
# Barn med moget knä 95% bootstrap CI
quantile(barn, c(0.025, 0.975))

# Bootstrap (parallell) snabbare!
system.time(db.par <- par_boot_dedouit2012(boot.n = 10000, dat = dat))

# vuxna
vuxna.par <- logistic_integrate_uniform(db.par$intercept, db.par$slope, 18, 21)$below
barn.par <- logistic_integrate_uniform(db.par$intercept, db.par$slope, 15, 18)$above

# Vuxna med omoget knä 95% bootstrap CI
quantile(vuxna.par, c(0.025, 0.975))
# Barn med moget knä 95% bootstrap CI
quantile(barn.par, c(0.025, 0.975))

# plottar
resultat <- tibble(vuxna = vuxna.par, barn = barn.par) %>% gather(variable, value)

ggplot(resultat) + aes(x = value, fill = variable) + geom_histogram(bins = 300) +
  geom_vline(xintercept = point.barn, color = "red") +
  geom_vline(xintercept = point.vuxna, color = "blue") + theme_bw() +
  theme(legend.position = "top", legend.title = element_blank())
