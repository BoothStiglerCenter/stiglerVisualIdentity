library(tidyverse)
library(gapminder)
library(showtext)
library(ggpubr)

print(getwd())

df <- gapminder



### Stigler colors and palettes
stigler_colors <- c(
    booth_maroon = "#800000",
    booth_teal = "#115E67",
    black_bean = "#4D0000",
    persian_red = "#BA402C",
    burnt_sienna = "#EA6A51",
    pale_tangerine = "#FFA487",
    midnight_green = "#00323B",
    munsell_blue = "#4C8F98",
    sky_blue = "#7EC0CA",
    celeste = "#BAF5FF"
)

stigler_cols <- function(...) {
    cols <- c(...)
    if (is.null(cols)) {
        return(stigler_colors)
    }
    stigler_colors[cols]
}

stigler_palettes <- list(
    `main` = stigler_cols(
        "booth_maroon",
        "booth_teal",
        "black_bean",
        "persian_red",
        "burnt_sienna",
        "pale_tangerine",
        "midnight_green",
        "munsell_blue",
        "sky_blue",
        "celeste"
    ),
    `reds` = stigler_cols(
        "booth_maroon",
        "persian_red",
        "burnt_sienna",
        "pale_tangerine",
        "black_bean"
    ),
    `blues` = stigler_cols(
        "booth_teal",
        "munsell_blue",
        "sky_blue",
        "celeste",
        "midnight_green"
    )
)

stigler_pal <- function(palette = "main", reverse = FALSE, ...) {
    pal <- stigler_palettes[[palette]]

    if (reverse) pal <- rev(pal)

    colorRampPalette(pal)
}

scale_color_stigler <- function(
    palette = "main",
    reverse = FALSE,
    discrete = TRUE,
    ...
    ) {
        pal <- stigler_pal(palette = palette, reverse = reverse, ...)
        if (discrete) {
            discrete_scale("color", paste0("stigler_", palette), palette = pal, ...)
        } else {
            scale_color_gradientn(colors = pal(256), ...)
        }
}

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




theme_stigler <- function() {
    font_add(
        family = "Trade Gothic LT Std",
        regular = "Trade Gothic LT Std/Trade Gothic LT Std Regular.otf",
        bold = "Trade Gothic LT Std/Trade Gothic LT Std Bold.otf",
        italic = "Trade Gothic LT Std/Trade Gothic LT Std Oblique.otf"
    )
    showtext_auto()

    font <- "Trade Gothic LT Std"
    theme_minimal() %+replace%
        theme(

            ### DEBUGGING
            plot.background = element_rect(
                color = "palevioletred1",
                linewidth = 3
            ),

            ### Setup
            plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
            line = element_line(
                color = "#676E73"
            ),
            text = element_text(
                family = font
            ),

            ### Labelling
            plot.title = element_text(
                face = "bold", # Typeface style
                size = 18, # Size (pt)
                hjust = 0, # hjust = 0 "left-aligned"
                vjust = 0, # vjust = 0 "top-aligned"
                margin = margin(t = 2.5, r = 0, b = 0, l = 0, unit = "pt"),
                debug = TRUE
            ),
            plot.subtitle = element_text(
                size = 16, # Size (pt)
                hjust = 0, # hjust = 0 "left-aligned"
                vjust = 0, # vjust = 0 "top-aligned"
                margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "pt"),
                debug = TRUE
            ),
            plot.caption.position = "panel",
            plot.caption = element_text(
                size = 12,
                hjust = 0,
                vjust = 0,
                margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "pt"),
                debug = TRUE
            ),
            plot.tag.position = c(0, 1),
            plot.tag = element_text(
                size = 14,
                color = "#800000",
                hjust = 0,
                vjust = 0,
                debug = TRUE
            ),

            ### Axes
            axis.title.x = element_text(
                family = font,
                face = "italic",
                size = 12,
                hjust = 0.5,
                vjust = 1,
                margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "pt"),
                debug = TRUE
            ),
            axis.title.y = element_blank(),
            axis.text = element_text(
                family = font
            ),
            axis.text.y.right = element_text(
                hjust = 0,
                vjust = 0
            ),
            axis.line.x = element_line(
                color = "black",
                linewidth = 1
            ),
            axis.ticks.x = element_line(
                color = "black",
            ),
            axis.ticks.length.x = unit(5, unit = "pt"),

            ### Panel features
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),

            ### Legend features
            legend.position = "top",
            legend.justification = "left",
            legend.title = element_text(
                family = font,
                size = 12,
                hjust = 0,
                vjust = 0.5,
                debug = TRUE
            ),
            legend.text = element_text(
                family = font,
                size = 12,
                hjust = 0,
                vjust = 0.5,
                debug = TRUE
            )
        )
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
        # title = "Life expectancy vs GDP per Capita",
        # subtitle = "Years",
        caption = "Caption text goes here:",
        # tag = "Figure X"
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
