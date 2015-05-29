# example to visualise weighted regression
library(dplyr)
library(ggplot)


set.seed(123)
d1 <- data_frame(x= rnorm(100, 100, 5),
                 y = 3 + 2*x + rnorm(100, 0, 10))

# adding weights on deviating observations
d1 <- 
    d1 %>%
    mutate(weights = ifelse((x < 100 & y> 200) | (x>100 & y < 200), 25, 1),
           max_weight = max(weights)
    )

lm(y~x, data=d1, weights=weights)

ggplot(d1) + aes(x=x, y=y) + 
    scale_size_area() + 
    geom_point(aes(size=sqrt(weights))) + # , alpha=sqrt(weights)/sqrt(max_weight))) + 
    theme_minimal() + 
    theme(legend.position="none") + 
    geom_smooth(method="lm",  aes(weight=weights), colour="darkgreen") + 
    geom_smooth(method="lm")
