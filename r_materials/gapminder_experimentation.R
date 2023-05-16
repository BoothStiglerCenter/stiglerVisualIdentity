library(tidyverse)
library(gapminder)
library(scales)
library(ggdist)
source("r_materials/theme_stigler.R")

print(getwd())

df <- gapminder

create_footer <- function(source_text, notes_text) {
    footer_text <- paste0(source_text, notes_text, sep = "\n")

    footer <- grid::grobTree(
        grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.1, "npc")),
        grid::textGrob(footer_text, x = 0.004, hjust = 0, gp = grid::gpar(fontsize=12, fontfamily="Trade Gothic LT Std"))
    )

    return(footer)
}

ggplot(
    df %>%
        filter(year == 2007)
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
        title = "**Life __expectancy__ vs GDP per Capita**",
        subtitle = "Years",
        caption = "Caption text goes here:",
        tag = "Figure X"
    ) +
    theme_stigler()

### To-Dos
# - Start splitting out functions and themes into separate files.
# - Identify greys
# - Decide whether y-axis titles live at the top of the figure or along the axis
# - Plot panel margins
# - Get positioning of the y-axis tick text right
# - Generate color scales functions
    # - Generating explicit color orders as well
    # - Generating palettes that permit a highlight color
# - Think about x- and y-axis scale functions
