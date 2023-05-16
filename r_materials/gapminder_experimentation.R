library(tidyverse)
library(gapminder)
library(scales)
library(ggdist)
source("r_materials/theme_stigler.R")

print(getwd())

df <- gapminder

##### EXAMPLE BUBBLE CHART, W/ PANELS #####
ggplot(
    df %>%
        filter(year %in% c(1967, 2007))
) +
    geom_point(
        aes(
            x = gdpPercap,
            y = lifeExp,
            size = pop,
            color = continent
        ),
        alpha = 0.8
    ) +
    scale_x_continuous(
        name = "GDP per capita",
        labels = scales::dollar
    ) +
    scale_y_continuous(
        position = "right",
        name = "this is a test y-axis title"
    ) +
    scale_color_stigler(
        palette = "main",
        name = "Continent"
    ) +
    scale_size_binned(
        guide  = "none"
    ) +
    guides(
        color = guide_legend(
            override.aes = list(size = 4)
        )
    ) +
    labs(
        title = "**Life expectancy vs GDP per Capita**",
        subtitle = "Years",
        caption = "Caption text goes here:",
        tag = "Figure X"
    ) +
    theme_stigler() +
    facet_wrap(~year)

##### LINE CHART ####
ggplot(
    df %>%
        filter(
            country %in% c("United Kingdom",
            "United States",
            "Greece")
        ) %>%
        mutate(
            year = ymd(year, truncated = 2L)
        )
) +
    geom_line(
        aes(
            x = year,
            y = lifeExp,
            color = country,
            group = country
        ),
        lineend = "round",
        size = 2
    ) +
    scale_x_date(
        name = "Year",
        breaks = date_breaks("5 years"),
        labels = label_date_short(format = c("%Y"))
    ) +
    scale_y_continuous(
        position = "right"
    ) +
    # scale_color_stigler(
    #     name = "Country"
    # ) +
    labs(
        title = "Life Expectancy over Time",
        subtitle = "Years, selected countries",
        tag = "Figure 2"
    ) +
    theme_stigler()

### To-Dos
# - Decide whether y-axis titles live at the top of the figure or along the axis
# - Plot panel margins
# - Get positioning of the y-axis tick text right
# - Generate color scales functions
    # - Generating explicit color orders as well
    # - Generating palettes that permit a highlight color
