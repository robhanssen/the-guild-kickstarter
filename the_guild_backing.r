# Kickstarter contribution distribution
# Source: https://www.kickstarter.com/projects/feliciaday/watchtheguild
#
#

library(tidyverse)
library(ggtext)

theme_set(
    theme_light() +
        theme(
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0),
        )
)

data <- read_csv("sources/backing.csv", show_col_types = FALSE, comment = "#") %>%
    arrange(value)


mod <- lm(
    log10(backers) ~ log10(value),
    data = data
)

summary(mod)

coeff <- broom::tidy(mod)
tvalue <- (coeff[2, "estimate"] - -1) / coeff[2, "std.error"]
pvalue <- pt(as.numeric(tvalue), df = 12, lower.tail = FALSE)
rejection <- pvalue < 0.05

r.sq.adj <- broom::glance(mod)[["adj.r.squared"]]

mod_comments <-
    glue::glue(
        "Model R<sup>2</sup><sub>adj</sub>: {round(r.sq.adj, 3)}<br/>",
        "t-value (slope = -1): {round(tvalue, 3)}<br/>",
        "p-value (slope \U2260 -1): {round(pvalue, 3)}"
    )


conflevel <- 0.99

extrap <-
    broom::augment(
        mod,
        newdata = tibble(value = seq(10, 26000, by = 100)),
        interval = "confidence", conf.level = conflevel
    ) %>%
    mutate(
        across(.cols = c(.fitted, .lower, .upper), .fns = ~ 10^.x)
    )

hadj <- c(rep(-0.1, length(data$value) - 1), 1)
vadj <- -0.5 * c(1, 1, 1, -4, 1, -4, 1, 1, 1, 1, 1, 1, 1, -4, 1)


log_g <-
    ggplot(data, aes(x = value, y = backers)) +
    geom_line(
        data = extrap,
        aes(y = .fitted), linetype = "dashed"
    ) +
    geom_ribbon(
        data = extrap,
        aes(ymin = .lower, ymax = .upper, y = .fitted),
        alpha = 0.2, fill = "gray70"
    ) +
    geom_point(
        shape = 21, color = "black", fill = "white",
        size = 3
    ) +
    scale_x_log10(
        labels = scales::label_dollar()
    ) +
    scale_y_log10() +
    labs(
        x = "Pledge value (in $)",
        y = "Number of backers",
        title = glue::glue("Kickstarter contribution distribution for Watch the Guild"),
        caption = glue::glue("Data @ 7/21/2026 EST 09:10\nConfidence ribbon at {conflevel}")
    ) +
    geom_text(
        aes(label = scales::dollar(value)),
        vjust = vadj, hjust = hadj
    ) +
    annotate(
        geom = "richtext", x = 10, y = 10, label = mod_comments,
        hjust = 0, vjust = 0.5
    )

ggsave("graphs/guild_backing_analysis.png", width = 8, height = 6, plot = log_g)
