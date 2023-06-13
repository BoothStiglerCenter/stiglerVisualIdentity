# README.md

## How to use `theme_stigler()` in R
To use the `theme_stigler()` options, only one or two things need to be modified.
First, in the preamble of your script you should include the line `source(path)` to the `theme_stigler.R` file. You can choose to make this a global variable if you wish to just have one version of this file stored locally on your computer rather than copying it into each projects/folder you're working on. Secondly (and optionally), you should modify that file to include paths to the appropriate folder that contains each of the font `.otf` files.

Once you have done this you should be able to add `theme_stigler()` to a ggplot object in the same way that you would use, `theme_minimal()`, for exmaple.

## Example

### Setting up access to fonts

There are two ways to setup your project to have access to the Stigler Center's preferred font, Trade Gothic LT Std, available in your plots.
1. The **recommended** method is to simply have a folder named `Trade Gothic LT Std` with all of the appropriate .otf files *somewhere* in your project directory. The theme_stigler() function automatically searches your poject directory for this folder and will accordingly add those fonts to your R environment. For ease of use it might be best to include this folder at the top level of your project directory.
2. The second way is to modify the `theme_stigler()` function in `theme_stigler.R` to search for a folder with the .otf files or to hardcode the paths to those files. Examples with comments are below

```r
### theme_stigler.R

# Making sure that the appropriate fonts are available
...

theme_stigler <- function() {

    # If you want to search other folders/directories for folders with the name 'Trade Gothic LT Std' you can add those folders to this character vector.
    # Note that by default the `finding_font_folders()` function also searches your OS' default fonts folder so no need to specify that one.
    # Note that the existing search is recursive so it's not a good idea to point to a folder close to your machine's root.
    # Eg. font_folder_path <- finding_font_folders(c(".", "folder/folder1"))
    font_folder_path <- finding_font_folders(c("."))
    
    ...

    # Another option is to comment out the above lines (inside the theme_stigler() function) and just hardcode a value for font_folder_path
    # Eg. font_folder_path <- "path_to_fonts/font_folder"

    font_add(
        family = "Trade Gothic LT Std",
        regular = paste(font_folder_path, "Trade Gothic LT Std Regular.otf", sep = "\\"),
        bold = paste(font_folder_path, "Trade Gothic LT Std Bold.otf", sep = "\\"),
        italic = paste(font_folder_path, "Trade Gothic LT Std Oblique.otf", sep = "\\"),
    )
    showtext_auto()
    showtext_opts(dpi = 300)
    ...
}
...

```

### Using `theme_stigler()` on a ggplot object 

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