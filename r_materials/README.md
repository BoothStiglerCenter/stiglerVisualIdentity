# README.md

## How to use `theme_stigler()` in R
To use the `theme_stigler()` options, only two things need to be modified.
First, in the preamble of your script you should include the line `source(path)` to the `theme_stigler.R` file. You can choose to make this a global variable if you wish to just have one version of this file stored locally on your computer rather than copying it into each projects/folder you're working on. Secondly, you should modify that file to include paths to the appropriate folder that contains each of the font `.otf` files.


## Example

```r
### theme_stigler.R

# Make sure that the appropriate fonts are available
...
theme_stigler <- function() {
    font_add(
        family = "folder_path/Trade Gothic LT Std"
        regular = "folder_path/Trade Gothic LT Std Regular.otf",
        bold = "folder_path/Trade Gothic LT Std Bold.otf",
        italic = "folder_path/Trade Gothic LT Std Oblique.otf"
    )
    showtext_auto()
    showtext_opts(dpi = 300)
    ...
}
...

```


```r
### ggplot_example.R
library(mtcars)
library(tidyverse)

# Makes the theme_stigler() and accompanying functions
# available in this file
theme_stigler_path <- "folder/theme_stigler.R"
source(theme_stigler_path)

df <- mtcars %>%
    mutate(cyl = as_factor(cyl))

p <- ggplot(df) + 
    geom_point(
        aes(
            x = disp,
            y = mpg,
            color = cyl
        ),
        size = 3
    ) +
    # scale_[color/fill]_stigler() make the Stigler VIG palettes available
    scale_color_stigler(
        name = "No. of cylinders"
    ) +
    scale_x_continuous(
        name = "Engine displacement (cc)"
    ) + 
    # Stigler VIG has usually places vertical axis labels on the right
    scale_y_continuous(
        position = "right",
        expand = expansion(add = 1)
    ) +
    # Many of these text elements are actually Markdown objects, allowing for styling
    labs(
        title = "**Fuel efficiency vs engine displacement**",
        subtitle = "Miles per gallon",
        tag = "Figure 1"
    ) +
    # Applies fonts, spacing, line stylings etc.
    theme_stigler()

# We like to output to .jpg with some multiple of 16:9 aspect ratios (which we can then scale up and down as we see fit)
ggsave(
    filename = "plotname.jpg",
    plot = p,
    width = 16,
    height = 9,
    units = "in",
    dpi = 300
)

```