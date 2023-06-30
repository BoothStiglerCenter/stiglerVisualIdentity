library(tidyverse)
library(gapminder)
library(scales)
library(ggdist)
source("r_materials/theme_stigler.R")

print(getwd())

df <- gapminder

##### EXAMPLE BUBBLE CHART, W/ PANELS #####
scatter_facet <- ggplot(
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
        caption = "Note: US dollars are adjusted for inflation.",
        tag = "Figure X"
    ) +
    theme_stigler() +
    facet_wrap(~year)

ggsave(
    filename = "r_materials/example_figures/scatterplot_faceted.jpg",
    plot = scatter_facet,
    width = 12,
    height = 6.75,
    units = "in",
    dpi = 300
)

##### LINE CHART ####
line <- ggplot(
    df %>%
        filter(
            country %in% c("China",
            "United States",
            "Greece",
            "Yemen, Rep.")
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
        size = 1.5
    ) +
    scale_x_date(
        limits = c(
            ymd(1950, truncated = 2L),
            ymd(2010, truncated = 2L)
        ),
        breaks = date_breaks("5 years"),
        labels = label_date_short(format = c("%Y"))
    ) +
    scale_y_continuous(
        position = "right"
    ) +
    scale_color_stigler(
        name = "Country"
    ) +
    labs(
        title = "Life Expectancy over Time",
        subtitle = "Years, selected countries",
        tag = "Figure X"
    ) +
    theme_stigler()

ggsave(
    filename = "r_materials/example_figures/line.jpeg",
    plot = line,
    width = 12,
    height = 6.75,
    units = "in",
    dpi = 300
)


### To-Dos
# - Decide whether y-axis titles live at the top of the figure or along the axis
# - Plot panel margins
# - Get positioning of the y-axis tick text right
# - Generate color scales functions
    # - Generating explicit color orders as well
    # - Generating palettes that permit a highlight color
