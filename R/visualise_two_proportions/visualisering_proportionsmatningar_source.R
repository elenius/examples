

dat <- 
    data_frame(
        rep = 1:nview,
        x_1 = rbinom(nview, n1, p1), 
        x_2 = rbinom(nview, n2, p2),
        n_1 = n1,
        n_2 = n2,
        prop_1 = x_1 / n_1,
        prop_2 = x_2 / n_2,
        pt_1 = p1,
        pt_2 = p2,
        prop_diff = p2 - p1
    ) %>%
    rowwise %>%
    mutate(
        p = prop.test(c(x_2, x_1), c(n_2, n_1))$p.value,
        pdiff = prop_2 - prop_1,
        lowerdiff = prop.test(c(x_2, x_1), c(n_2, n_1))$conf.int[1],
        upperdiff = prop.test(c(x_2, x_1), c(n_2, n_1))$conf.int[2],
        grupp = paste0(formatC(rep, flag = "0", format="d", width = 2), " : p-värde = ", round(p, 3)),
        lower_1 = binom.test(x_1, n_1)$conf.int[1],
        upper_1 = binom.test(x_1, n_1)$conf.int[2],
        lower_2 = binom.test(x_2, n_2)$conf.int[1],
        upper_2 = binom.test(x_2, n_2)$conf.int[2]
    ) %>%
    ungroup %>%
    gather(variable, value, -rep, -p, -grupp, -lowerdiff, -upperdiff, -pdiff, -prop_diff) %>%
    separate(variable, c("var", "measure")) %>%
    spread(var, value) %>%
    mutate(
        measure_num = as.integer(as.numeric(measure)),
        grupp = as.factor(grupp)
    ) %>%
    arrange(rep, measure)

plot_1 <- 
    ggplot(dat) + 
    aes(x = measure, y = 100*prop, fill = measure, label = paste0(round(100*prop, 1), "%"), 
        colour = measure, xmin = measure_num - 0.4, xmax = measure_num + 0.4) + 
    facet_wrap(~grupp) +
    geom_rect(ymin = 0, ymax = 100, fill = "white") + 
    geom_bar(stat="identity", width = 0.8, alpha=0.8) + 
    geom_errorbar(aes(ymin = 100*lower, ymax = 100*upper), width = 0.1,  color = "black") +
    geom_segment(aes(x = measure_num - 0.5, xend = measure_num + 0.5, y = 100*pt, yend = 100*pt), lty = 2, colour="black") + 
    # geom_text(aes(x = measure, y = 100*prop), vjust = 0, hjust = -0.5, colour="black") + 
    geom_text(aes(x = measure, y = 0), vjust = -1, hjust = 0.5, colour="white", size=9) + 
    geom_text(aes(x = measure, y = 100*upper, label = paste0(round(100*upper, 1), "%")), color = "black", vjust = -1) + 
    geom_text(aes(x = measure, y = 100*lower, label = paste0(round(100*lower, 1), "%")), color = "black", vjust = 1.3) +
    scale_y_continuous(lim = c(0, 110), breaks = c(0, 25, 50, 75, 100)) + 
    # geom_text(y = max(c(n1, n2)), x = 1.5, aes(label = paste0("p-värde = ", round(p, 3))), size = 6) + 
    theme_minimal() + xlab("") + ylab("") +
    theme(
        legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_blank()
    ) + 
    scale_color_manual(values = pm.colors) + scale_fill_manual(values = pm.colors)


# tittar på skillnaden
dat2 <- 
    dat %>% 
    select(rep, pdiff, p, lowerdiff, upperdiff, grupp, prop_diff) %>%
    distinct

plot_2 <- 
    ggplot(dat2) + aes(x = as.integer(rep), y = 100*pdiff, ymin = 100*lowerdiff, ymax = 100*upperdiff) + 
    geom_errorbar() + geom_point(size = 5) + 
    scale_x_discrete(breaks = 1:nview, labels = 1:nview) +
    theme_minimal() + xlab("") + ylab("") + ggtitle("skillnad mellan mätning 2 och 1 i %") + 
    theme(
        legend.position = "none",
        # panel.grid = element_blank()# ,
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
        # axis.text.x = element_blank()
    ) +
    geom_segment(aes(x = rep - 0.5, xend = rep + 0.5, y = 0, yend = 0), lty = 2, colour="black") + 
    geom_segment(aes(x = rep - 0.5, xend = rep + 0.5, y = 100*prop_diff, yend = 100*prop_diff), lty = 2, colour=pm.colors[3]) + 
    geom_text(aes(y = 100*lowerdiff, label = paste0(round(100*lowerdiff, 1), "%")), vjust = -2, hjust = -0.5) +
    geom_text(aes(y = 100*upperdiff, label = paste0(round(100*upperdiff, 1), "%")), vjust = -2, hjust = 1.3) +
    geom_text(aes(y = 100*pdiff, label = paste0(round(100*pdiff, 1), "%")), vjust = -2, hjust = 0) +
    coord_flip()
