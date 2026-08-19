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

nlmod <- nls(
    backers ~ a * value^b,
    data = data,
    start = list(a = 3e4, b = -1)
)

summary(mod)
summary(nlmod)


ggplot(data, aes(x = value, y = backers)) +
    geom_point() +
 geom_line(data = broom::augment(nlmod), aes(x = value, y = .fitted), color = "red") + 
 geom_line(data = broom::augment(mod) %>% mutate(across(1:3, \(x) 10^x)) %>% select(1:3) %>% setNames(c("backers", "value", ".fitted")), aes(x = value, y = .fitted), color = "blue") + 
    scale_x_log10() +
    scale_y_log10() +
    labs(
        title = "Backers vs Value",
        x = "Value",
        y = "Backers"
    )