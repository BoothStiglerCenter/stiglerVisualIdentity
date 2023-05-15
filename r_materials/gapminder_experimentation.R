library(tidyverse)
library(gapminder)
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


finalise_plot <- function(
    plot_object,
    figure_number,
    title_text,
    subtitle_text,
    source_text,
    notes_text,
    # save_filepath = file.path(Sys.getenv("TMPDIR", "tmp-nc.png")),
    plot_width = 695,
    plot_height = 450 
) {
    touching_up_plot <- plot_object +
        labs(
            title = title_text,
            subtitle = subtitle_text,
            tag = paste0("Figure ", figure_number)
        )
    footer <- create_footer(source_text, notes_text)
    plot_grid <- ggpubr::ggarrange(
        touching_up_plot,
        footer,
        ncol = 1,
        nrow = 2,
        heights = c(1, 0.045/(plot_height/450))
    )
    plot_grid
}


test_plot <- ggplot(
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
    labs(
        title = "**Life __expectancy__ vs GDP per Capita**",
        subtitle = "Years",
        caption = "Caption text goes here:",
        tag = "Figure X"
    ) +
    scale_y_continuous(
        position = "right",
        name = "this is a test y-axis title"
    ) +
    scale_color_stigler(
        name = element_blank()
    ) +
    scale_size_binned(
        guide = "none"
    ) +
    theme_stigler()
test_plot


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
