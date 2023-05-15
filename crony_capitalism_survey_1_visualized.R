#### PREAMBLE ####
library(tidyverse)
library(readxl)
library(ggdist)
library(scales)
library(modelsummary)
source("r_materials/theme_stigler.R")

#### READING DATA IN ####
data_in <- read_excel(
    "crony_capitalism_survey.xlsx"
)

df_full <- data_in

df_stats <- data_in %>%
    select(
        id,
        mckinsey_1,
        mckinsey_2,
        mckinsey_3,
        sensetime_4,
        ibm_5,
        demo_1,
        demo_1_notes,
        demo_2,
        demo_3
    )

#### PLOTTING ####
blue_to_red_palette = colorRampPalette(c(stigler_cols("booth_teal"), stigler_cols("booth_maroon")))

########## Minimum Acceptable Salary Distributions ##########
ggplot(
    df_stats %>%
        select(
            id,
            mckinsey_2,
            mckinsey_3,
            sensetime_4,
            ibm_5
        ) %>%
        pivot_longer(
            cols = c(
                mckinsey_2,
                mckinsey_3,
                sensetime_4,
                ibm_5
            ),
            names_to = "question_name",
            values_to = "value"
        ) %>%
        mutate(
            question_name = as_factor(question_name),
            question_label = case_when(
                question_name == "mckinsey_2" ~ "McKinsey -- McKinley",
                question_name == "mckinsey_3" ~ "McKinsey -- Colleagues",
                question_name == "sensetime_4" ~ "SenseTime -- InSenseTime",
                question_name == "ibm_5" ~ "IBM -- Simplex TRC"

            )
        ),
    aes(group = question_name)
) +
    stat_slab(
        aes(
            x = value,
            y = as.factor(question_name),
            group = question_name,
            color = question_name,
            fill = question_name
        ),
        alpha = 0.7
    ) +
    stat_pointinterval(
        aes(
            x = value,
            y = as.factor(question_name),
            group = question_name,
            color = question_name,
            fill = question_name,
            point_fill = question_name
        ),
        point_color = "black",
        size = 7
    ) +
    stat_pointinterval(
        aes(
            x = value,
            y = as.factor(question_name),
            group = question_name,
            color = question_name,
            fill = question_name,
            point_fill = question_name
        ),
        .width = c(0, 0),
        point_interval = mean_qi,
        point_color = "white",
        size = 7
    ) +
    geom_point(
        aes(
            x = value,
            y = question_name,
            color = question_name,
        ),
        alpha = 0.3,
        size = 5,
        shape = "|",
        position = position_dodge(
            width = 0.9
        )
    ) +
    geom_text(
        aes(
            x = 150000,
            y = question_name,
            color = question_name,
            label = question_label
        ),
        family = "Trade Gothic LT Std",
        vjust = -1
    ) +
    scale_point_color_discrete(
        guide = "none",
    ) +
    scale_x_continuous(
        labels = scales::dollar,
        expand = expansion(mult = c(0, 0.05)),
        name = "Ethical-alternative minimum salary"
    ) +
    scale_y_discrete(
        labels = c(
            "mckinsey_2" = "",
            "mckinsey_3" = "",
            "sensetime_4" = "",
            "ibm_5" = ""
        ),
        position = "right",
        expand = expansion(add = c(0.01, 0))
    ) +
    scale_color_stigler(
        "main",
        guide = "none"
    ) +
    scale_fill_stigler(
        "main",
        guide = "none"
    ) +
    labs(
        title = "**Distribution of ethical-alternative minimum salaries required**",
        subtitle = "2023, USD",
        tag = "Figure 1"
    ) +
    theme_stigler() +
    theme(
        legend.position = "none"
    )

ggsave(
    plot = last_plot(),
    filename = "crony_capitalism_1_figs/alternatives_dist_plot.png",
    width = 9.7,
    height = 5.5,
    units = "in",
    dpi = 300
)

########## Average Acceptable Salary Cuts, by program ##########
ggplot(
    df_full %>%
        group_by(demo_1) %>%
        summarize(
            demo_program_size = n(),
            mckinsey_2_altmean = mean(mckinsey_2, na.rm = TRUE),
            mckinsey_3_altmean = mean(mckinsey_3, na.rm = TRUE),
            sensetime_4_altmean = mean(sensetime_4, na.rm = TRUE),
            ibm_5_altmean = mean(ibm_5, na.rm = TRUE)
        ) %>%
        pivot_longer(
            cols = c(
                mckinsey_2_altmean,
                mckinsey_3_altmean, 
                sensetime_4_altmean, 
                ibm_5_altmean,
            ),
            names_to = "question_name",
            values_to = "altmean"
        ) %>% 
        ungroup() %>%
        mutate(
            altmean_delta_percentage = case_when(
                str_detect(question_name, "mckinsey") ~ (altmean - 120000) / 120000,
                str_detect(question_name, "sensetime") ~ (altmean - 100000) / 100000,
                str_detect(question_name, "ibm") ~ (altmean - 100000) / 100000
            )
        )
) +
    geom_linerange(
        aes(
            xmin = 0,
            xmax = altmean_delta_percentage,
            y = question_name,
            color = demo_1
        ),
        linewidth = 2.4,
        lineend = "round",
        position = position_dodge(width = 0.5)
    ) + 
    geom_segment( 
        aes(
            x = 0,
            xend = 0, 
            y = Inf,
            yend = -Inf
        ),
        linewidth = 0.75,
        color = "grey"
    ) + 
    scale_color_stigler(
        name = "Academic Program",
        breaks = c(
            "a", "b", "c", "d"
        ),
        labels = c("Undergrad", "MBA", "MPP", "Other")
    ) +
    scale_x_continuous(
        name = "Acceptable salary (share of ex ante salary)",
        labels = scales::percent,
        expand = expansion(add = c(0.01, 0.05))
    ) +
    scale_y_discrete(
        position = "right",
        labels = c(
            "mckinsey_2_altmean" = "McKinsey -- McKinley",
            "mckinsey_3_altmean" = "McKinsey -- Colleagues",
            "sensetime_4_altmean" = "SenseTime -- InSenseTime",
            "ibm_5_altmean" = "IBM -- Simplex TRC"
        ),
        breaks = c(
            "mckinsey_2_altmean",
            "mckinsey_3_altmean",
            "sensetime_4_altmean",
            "ibm_5_altmean"
        )
    ) +
    labs(
        title = "**Average maximum-acceptable salary cuts to work for ethical alternatives**",
        subtitle = "Share of ex ante salary*, by academic program",
        caption = "*Ex ante salary for McKinsey questions is assumed to be $120,000 including performance bonus. For others, the ex ante salary is $100,000",
        tag = "Figure 2A"
    ) +
    theme_stigler() +
    theme(
        panel.grid.major.y = element_blank(),
        axis.text.y.right = element_text(
            hjust = 1,
            vjust = -2.2,
            margin = margin(t = 0, r = 10, b = 0, l = -210, unit = "pt"),
            debug = FALSE
        )
    )

ggsave(
    plot = last_plot(),
    filename = "crony_capitalism_1_figs/avg_alt_salary_program.png",
    width = 9.7,
    height = 5.5,
    units = "in",
    dpi = 300
)

########## Median Acceptable Salary Cuts, by program ##########
ggplot(
    df_full %>%
        group_by(demo_1) %>%
        summarize(
            mckinsey_2_altmedian = median(mckinsey_2, na.rm = TRUE),
            mckinsey_3_altmedian = median(mckinsey_3, na.rm = TRUE),
            sensetime_4_altmedian = median(sensetime_4, na.rm = TRUE),
            ibm_5_altmedian = median(ibm_5, na.rm = TRUE)
        ) %>%
        pivot_longer(
            cols = c(
                mckinsey_2_altmedian,
                mckinsey_3_altmedian,
                sensetime_4_altmedian,
                ibm_5_altmedian,
            ),
            names_to = "question_name",
            values_to = "altmedian"
        ) %>% 
        ungroup() %>%
        mutate(
            altmedian_delta_percentage = case_when(
                str_detect(question_name, "mckinsey") ~ (altmedian - 120000) / 120000,
                str_detect(question_name, "sensetime") ~ (altmedian - 100000) / 100000,
                str_detect(question_name, "ibm") ~ (altmedian - 100000) / 100000
            )
        )
) +
    geom_linerange(
        aes(
            xmin = 0,
            xmax = altmedian_delta_percentage,
            y = question_name,
            color = demo_1
        ),
        linewidth = 2.4,
        lineend = "round",
        position = position_dodge(width = 0.5)
    ) + 
    geom_segment( 
        aes(
            x = 0,
            xend = 0, 
            y = Inf,
            yend = -Inf
        ),
        linewidth = 0.75,
        color = "grey"
    ) + 
    scale_color_stigler(
        name = "Academic Program",
        breaks = c(
            "a", "b", "c", "d"
        ),
        labels = c("Undergrad", "MBA", "MPP", "Other")
    ) +
    scale_x_continuous(
        name = "Acceptable salary (share of ex ante salary)",
        labels = scales::percent,
        expand = expansion(add = c(0.01, 0.05))
    ) +
    scale_y_discrete(
        position = "right",
        labels = c(
            "mckinsey_2_altmedian" = "McKinsey -- McKinley",
            "mckinsey_3_altmedian" = "McKinsey -- Colleagues",
            "sensetime_4_altmedian" = "SenseTime -- InSenseTime",
            "ibm_5_altmedian" = "IBM -- Simplex TRC"
        ),
        limits = c(
            "mckinsey_2_altmedian",
            "mckinsey_3_altmedian",
            "sensetime_4_altmedian",
            "ibm_5_altmedian"
        ),
        breaks = c(
            "mckinsey_2_altmedian",
            "mckinsey_3_altmedian",
            "sensetime_4_altmedian",
            "ibm_5_altmedian"
        )
    ) +
    labs(
        title = "**Median maximum-acceptable salary cuts to work for ethical alternatives**",
        subtitle = "Share of ex ante salary*, by academic program",
        caption = "*Ex ante salary for McKinsey questions is assumed to be $120,000 including performance bonus. For others, the ex ante salary is $100,000",
        tag = "Figure 2B"
    ) +
    theme_stigler() +
    theme(
        panel.grid.major.y = element_blank(),
        axis.text.y.right = element_text(
            hjust = 1,
            vjust = -2.2,
            margin = margin(t = 0, r = 10, b = 0, l = -210, unit = "pt"),
            debug = FALSE
        )
    )

ggsave(
    plot = last_plot(),
    filename = "crony_capitalism_1_figs/med_alt_salary_program.png",
    width = 9.7,
    height = 5.5,
    units = "in",
    dpi = 300
)

########## Average Acceptable Salary Cuts, by ideology ##########
ggplot(
    df_full %>%
        group_by(demo_2) %>%
        summarize(
            mckinsey_2_altmean = mean(mckinsey_2, na.rm = TRUE),
            mckinsey_3_altmean = mean(mckinsey_3, na.rm = TRUE),
            sensetime_4_altmean = mean(sensetime_4, na.rm = TRUE),
            ibm_5_altmean = mean(ibm_5, na.rm = TRUE)
        ) %>%
        pivot_longer(
            cols = c(
                mckinsey_2_altmean,
                mckinsey_3_altmean,
                sensetime_4_altmean,
                ibm_5_altmean,
            ),
            names_to = "question_name",
            values_to = "altmean"
        ) %>%
        ungroup() %>%
        mutate(
            altmean_delta_percentage = case_when(
                str_detect(question_name, "mckinsey") ~ (altmean - 120000) / 120000,
                str_detect(question_name, "sensetime") ~ (altmean - 100000) / 100000,
                str_detect(question_name, "ibm") ~ (altmean - 100000) / 100000
            ),
            demo_2 = case_when(
                demo_2 == "1" ~ "Most liberal",
                demo_2 == "2" ~ "Liberal",
                demo_2 == "3" ~ "Centrist",
                demo_2 == "4" ~ "Conservative",
                demo_2 == "5" ~ "Most conservative",
                TRUE ~ "Other"
            ),
            demo_2 = as_factor(demo_2)
        ) %>%
        filter(demo_2 != "Other")
) +
    geom_linerange(
        aes(
            xmin = 0,
            xmax = altmean_delta_percentage,
            y = question_name,
            color = demo_2
        ),
        linewidth = 2.4,
        lineend = "round",
        position = position_dodge(width = 0.5)
    ) + 
    geom_segment( 
        aes(
            x = 0,
            xend = 0,
            y = Inf,
            yend = -Inf
        ),
        linewidth = 0.75,
        color = "grey"
    ) +
    scale_color_manual(
        values = blue_to_red_palette(5),
        name = "Most liberal",
        breaks = c("Most liberal",
            "Liberal",
            "Centrist",
            "Conservative",
            "Most conservative"
        ),
        labels = c("", "", "", "", "Most conservative")
    ) + 
    scale_x_continuous(
        name = "Acceptable salary (share of ex ante salary)",
        labels = scales::percent,
        expand = expansion(add = c(0.01, 0.05))
    ) +
    scale_y_discrete(
        position = "right",
        labels = c(
            "mckinsey_2_altmean" = "McKinsey -- McKinley",
            "mckinsey_3_altmean" = "McKinsey -- Colleagues",
            "sensetime_4_altmean" = "SenseTime -- InSenseTime",
            "ibm_5_altmean" = "IBM -- Simplex TRC"
        ),
        limits = c(
            "mckinsey_2_altmean",
            "mckinsey_3_altmean",
            "sensetime_4_altmean",
            "ibm_5_altmean"
        ),
        breaks = c(
            "mckinsey_2_altmean",
            "mckinsey_3_altmean",
            "sensetime_4_altmean",
            "ibm_5_altmean"
        )
    ) +
    labs(
        title = "**Average maximum-acceptable salary cuts to work for ethical alternatives**",
        subtitle = "Share of ex ante salary*, by ideological leaning",
        caption = "*Ex ante salary for McKinsey questions is assumed to be $120,000 including performance<br> bonus. For others, the ex ante salary is $100,000",
        tag = "Figure 2C"
    ) +
    theme_stigler() +
    theme(
        panel.grid.major.y = element_blank(),
        axis.text.y.right = element_text(
            hjust = 1,
            vjust = -2.5,
            margin = margin(t = 0, r = 10, b = 0, l = -219, unit = "pt"),
            debug = FALSE
        )
    )

ggsave(
    plot = last_plot(),
    filename = "crony_capitalism_1_figs/avg_alt_salary_ideology.png",
    width = 9.7,
    height = 5.5,
    units = "in",
    dpi = 300
)

########## Median Acceptable Salary Cuts, by ideology ##########
ggplot(
    df_full %>%
        group_by(demo_2) %>%
        summarize(
            mckinsey_2_altmedian = median(mckinsey_2, na.rm = TRUE),
            mckinsey_3_altmedian = median(mckinsey_3, na.rm = TRUE),
            sensetime_4_altmedian = median(sensetime_4, na.rm = TRUE),
            ibm_5_altmedian = median(ibm_5, na.rm = TRUE)
        ) %>%
        pivot_longer(
            cols = c(
                mckinsey_2_altmedian,
                mckinsey_3_altmedian,
                sensetime_4_altmedian,
                ibm_5_altmedian,
            ),
            names_to = "question_name",
            values_to = "altmedian"
        ) %>%
        ungroup() %>%
        mutate(
            altmedian_delta_percentage = case_when(
                str_detect(question_name, "mckinsey") ~ (altmedian - 120000) / 120000,
                str_detect(question_name, "sensetime") ~ (altmedian - 100000) / 100000,
                str_detect(question_name, "ibm") ~ (altmedian - 100000) / 100000
            ),
            demo_2 = case_when(
                demo_2 == "1" ~ "Most liberal",
                demo_2 == "2" ~ "Liberal",
                demo_2 == "3" ~ "Centrist",
                demo_2 == "4" ~ "Conservative",
                demo_2 == "5" ~ "Most conservative",
                TRUE ~ "Other"
            ),
            demo_2 = as_factor(demo_2)
        ) %>%
        filter(demo_2 != "Other")
) +
    geom_linerange(
        aes(
            xmin = 0,
            xmax = altmedian_delta_percentage,
            y = question_name,
            color = demo_2
        ),
        linewidth = 2.4,
        lineend = "round",
        position = position_dodge(width = 0.5)
    ) + 
    geom_segment( 
        aes(
            x = 0,
            xend = 0,
            y = Inf,
            yend = -Inf
        ),
        linewidth = 0.75,
        color = "grey"
    ) +
    scale_color_manual(
        values = blue_to_red_palette(5),
        name = "Most liberal",
        breaks = c("Most liberal",
            "Liberal",
            "Centrist",
            "Conservative",
            "Most conservative"
        ),
        labels = c("", "", "", "", "Most conservative")
    ) + 
    scale_x_continuous(
        name = "Acceptable salary (share of ex ante salary)",
        labels = scales::percent,
        expand = expansion(add = c(0.01, 0.05))
    ) +
    scale_y_discrete(
        position = "right",
        labels = c(
            "mckinsey_2_altmedian" = "McKinsey -- McKinley",
            "mckinsey_3_altmedian" = "McKinsey -- Colleagues",
            "sensetime_4_altmedian" = "SenseTime -- InSenseTime",
            "ibm_5_altmedian" = "IBM -- Simplex TRC"
        ),
        limits = c(
            "mckinsey_2_altmedian",
            "mckinsey_3_altmedian",
            "sensetime_4_altmedian",
            "ibm_5_altmedian"
        ),
        breaks = c(
            "mckinsey_2_altmedian",
            "mckinsey_3_altmedian",
            "sensetime_4_altmedian",
            "ibm_5_altmedian"
        )
    ) +
    labs(
        title = "**Median maximum-acceptable salary cuts to work for ethical alternatives**",
        subtitle = "Share of ex ante salary*, by ideological leaning",
        caption = "*Ex ante salary for McKinsey questions is assumed to be $120,000 including performance<br> bonus. For others, the ex ante salary is $100,000",
        tag = "Figure 2D"
    ) +
    theme_stigler() +
    theme(
        panel.grid.major.y = element_blank(),
        axis.text.y.right = element_text(
            hjust = 1,
            vjust = -2.5,
            margin = margin(t = 0, r = 10, b = 0, l = -219, unit = "pt"),
            debug = FALSE
        )
    )

ggsave(
    plot = last_plot(),
    filename = "crony_capitalism_1_figs/med_alt_salary_ideology.png",
    width = 9.7,
    height = 5.5,
    units = "in",
    dpi = 300
)

########## Median Acceptable Salary Cuts, by income quintile ##########
ggplot(
    df_full %>%
        group_by(demo_3) %>%
        summarize(
            mckinsey_2_altmean = mean(mckinsey_2, na.rm = TRUE),
            mckinsey_3_altmean = mean(mckinsey_3, na.rm = TRUE),
            sensetime_4_altmean = mean(sensetime_4, na.rm = TRUE),
            ibm_5_altmean = mean(ibm_5, na.rm = TRUE)
        ) %>%
        pivot_longer(
            cols = c(
                mckinsey_2_altmean,
                mckinsey_3_altmean,
                sensetime_4_altmean,
                ibm_5_altmean,
            ),
            names_to = "question_name",
            values_to = "altmean"
        ) %>%
        ungroup() %>%
        mutate(
            altmean_delta_percentage = case_when(
                str_detect(question_name, "mckinsey") ~ (altmean - 120000) / 120000,
                str_detect(question_name, "sensetime") ~ (altmean - 100000) / 100000,
                str_detect(question_name, "ibm") ~ (altmean - 100000) / 100000
            ),
            demo_3 = case_when(
                demo_3 == "a" ~ "First",
                demo_3 == "b" ~ "Second",
                demo_3 == "c" ~ "Third",
                demo_3 == "d" ~ "Fourth",
                demo_3 == "e" ~ "Fifth",
                TRUE ~ "Other"
            ),
            demo_3 = as_factor(demo_3)
        ) %>%
        filter(demo_3 != "Other")
) +
    geom_linerange(
        aes(
            xmin = 0,
            xmax = altmean_delta_percentage,
            y = question_name,
            color = demo_3
        ),
        linewidth = 2.4,
        lineend = "round",
        position = position_dodge(width = 0.5)
    ) +
    geom_segment(
        aes(
            x = 0,
            xend = 0,
            y = Inf,
            yend = -Inf
        ),
        linewidth = 0.75,
        color = "grey"
    ) +
    scale_color_stigler(
        palette = "reds",
        name = "First quintile",
        breaks = c("First", "Second", "Third", "Fourth", "Fifth"),
        limits = c("First", "Second", "Third", "Fourth", "Fifth"),
        labels = c("", "", "", "", "Fifth quintile")
    ) +
    scale_x_continuous(
        name = "Acceptable salary (share of ex ante salary)",
        labels = scales::percent,
        expand = expansion(add = c(0.01, 0.07))
    ) +
    scale_y_discrete(
        position = "right",
        labels = c(
            "mckinsey_2_altmean" = "McKinsey -- McKinley",
            "mckinsey_3_altmean" = "McKinsey -- Colleagues",
            "sensetime_4_altmean" = "SenseTime -- InSenseTime",
            "ibm_5_altmean" = "IBM -- Simplex TRC"
        ),
        limits = c(
            "mckinsey_2_altmean",
            "mckinsey_3_altmean",
            "sensetime_4_altmean",
            "ibm_5_altmean"
        ),
        breaks = c(
            "mckinsey_2_altmean",
            "mckinsey_3_altmean",
            "sensetime_4_altmean",
            "ibm_5_altmean"
        )
    ) +
    labs(
        title = "**Average maximum-acceptable salary cuts to work for ethical alternatives**",
        subtitle = "Share of ex ante salary*, by income quintile",
        caption = "NOTE: 'Don't know,' 'Prefer not to say,' and 'NA' responses removed. *Ex ante salary for McKinsey questions is assumed to be<br>$120,000 including performance bonus. For others, the ex ante salary is $100,000",
        tag = "Figure 2E"
    ) +
    theme_stigler() +
    theme(
        panel.grid.major.y = element_blank(),
        axis.text.y.right = element_text(
            hjust = 1,
            vjust = -2.5,
            margin = margin(t = 0, r = 10, b = 0, l = -219, unit = "pt"),
            debug = FALSE
        )
    )

ggsave(
    plot = last_plot(),
    filename = "crony_capitalism_1_figs/avg_alt_salary_income.png",
    width = 9.7,
    height = 5.5,
    units = "in",
    dpi = 300
)

########## Median Acceptable Salary Cuts, by income quintile ##########
ggplot(
    df_full %>%
        group_by(demo_3) %>%
        summarize(
            mckinsey_2_altmedian = median(mckinsey_2, na.rm = TRUE),
            mckinsey_3_altmedian = median(mckinsey_3, na.rm = TRUE),
            sensetime_4_altmedian = median(sensetime_4, na.rm = TRUE),
            ibm_5_altmedian = median(ibm_5, na.rm = TRUE)
        ) %>%
        pivot_longer(
            cols = c(
                mckinsey_2_altmedian,
                mckinsey_3_altmedian,
                sensetime_4_altmedian,
                ibm_5_altmedian,
            ),
            names_to = "question_name",
            values_to = "altmedian"
        ) %>%
        ungroup() %>%
        mutate(
            altmedian_delta_percentage = case_when(
                str_detect(question_name, "mckinsey") ~ (altmedian - 120000) / 120000,
                str_detect(question_name, "sensetime") ~ (altmedian - 100000) / 100000,
                str_detect(question_name, "ibm") ~ (altmedian - 100000) / 100000
            ),
            demo_3 = case_when(
                demo_3 == "a" ~ "First",
                demo_3 == "b" ~ "Second",
                demo_3 == "c" ~ "Third",
                demo_3 == "d" ~ "Fourth",
                demo_3 == "e" ~ "Fifth",
                TRUE ~ "Other"
            ),
            demo_3 = as_factor(demo_3)
        ) %>%
        filter(demo_3 != "Other")
) +
    geom_linerange(
        aes(
            xmin = 0,
            xmax = altmedian_delta_percentage,
            y = question_name,
            color = demo_3
        ),
        linewidth = 2.4,
        lineend = "round",
        position = position_dodge(width = 0.5)
    ) +
    geom_segment( 
        aes(
            x = 0,
            xend = 0,
            y = Inf,
            yend = -Inf
        ),
        linewidth = 0.75,
        color = "grey"
    ) +
    scale_color_stigler(
        palette = "reds",
        name = "First quintile",
        breaks = c("First", "Second", "Third", "Fourth", "Fifth"),
        limits = c("First", "Second", "Third", "Fourth", "Fifth"),
        labels = c("", "", "", "", "Fifth quintile")
    ) +
    scale_x_continuous(
        name = "Acceptable salary (share of ex ante salary)",
        labels = scales::percent,
        expand = expansion(add = c(0.01, 0.07))
    ) +
    scale_y_discrete(
        position = "right",
        labels = c(
            "mckinsey_2_altmedian" = "McKinsey -- McKinley",
            "mckinsey_3_altmedian" = "McKinsey -- Colleagues",
            "sensetime_4_altmedian" = "SenseTime -- InSenseTime",
            "ibm_5_altmedian" = "IBM -- Simplex TRC"
        ),
        limits = c(
            "mckinsey_2_altmedian",
            "mckinsey_3_altmedian",
            "sensetime_4_altmedian",
            "ibm_5_altmedian"
        ),
        breaks = c(
            "mckinsey_2_altmedian",
            "mckinsey_3_altmedian",
            "sensetime_4_altmedian",
            "ibm_5_altmedian"
        )
    ) +
    labs(
        title = "**Median maximum-acceptable salary cuts to work for ethical alternatives**",
        subtitle = "Share of ex ante salary*, by income quintile",
        caption = "NOTE: 'Don't know,' 'Prefer not to say,' and 'NA' responses removed. *Ex ante salary for McKinsey questions is assumed to be $120,000 including performance<br> bonus. For others, the ex ante salary is $100,000",
        tag = "Figure 2F"
    ) +
    theme_stigler() +
    theme(
        panel.grid.major.y = element_blank(),
        axis.text.y.right = element_text(
            hjust = 1,
            vjust = -2.5,
            margin = margin(t = 0, r = 10, b = 0, l = -219, unit = "pt"),
            debug = FALSE
        )
    )

ggsave(
    plot = last_plot(),
    filename = "crony_capitalism_1_figs/med_alt_salary_income.png",
    width = 9.7,
    height = 5.5,
    units = "in",
    dpi = 300
)


########## Demographic Questions: Program x Ideology##########
blue_to_red_palette = colorRampPalette(c(stigler_cols("booth_teal"), stigler_cols("booth_maroon")))
ggplot(
    df_full %>%
        select(demo_1, demo_2) %>%
        mutate(
            demo_1 = case_when(
                demo_1 == "a" ~ "Undergrad",
                demo_1 == "b" ~ "MBA",
                demo_1 == "c" ~ "MPP",
                demo_1 == "d" ~ "Other"
            ),
            demo_2 = case_when(
                is.na(demo_2) ~ 3,
                TRUE ~ demo_2
            ),
            demo_2 = as_factor(demo_2)
        ) %>%
        filter(
            !is.na(demo_1)
        )
) +
    geom_bar(
        aes(
            x = demo_1,
            group = demo_2,
            fill = demo_2
        )
    ) +
    scale_x_discrete(
        breaks = c(
            "Undergrad",
            "MBA",
            "MPP",
            "Other"
        ),
        limits = c(
            "Undergrad",
            "MBA",
            "MPP",
            "Other"
        ),
        name = "Academic program"
    ) +
    scale_y_continuous(
        position = "right",
        expand = expansion(mult = 0)
    ) +
    scale_fill_manual(
        name = "Most liberal",
        values = blue_to_red_palette(5),
        breaks = c(1, 2, 3, 4, 5),
        labels = c("", "", "", "", "Most conservative")
    ) +
    labs(
        title = "**Distributions of respondent program and ideological leaning**",
        subtitle = "Counts",
        tag = "Figure 3A"
    ) +
    theme_stigler() +
    theme(
        axis.text.y.right = element_text(
            hjust = 1,
            vjust = -0.6,
            margin = margin(t = 0, r = 10, b = 0, l = -16, unit = "pt"),
            debug = FALSE
        )
    )

ggsave(
    plot = last_plot(),
    filename = "crony_capitalism_1_figs/demographics_program_ideology.png",
    width = 9.7,
    height = 5.5,
    units = "in",
    dpi = 300
)

########## Demographic Questions: Income x ideology ##########

ggplot(
    df_full %>%
        select(demo_2, demo_3) %>%
        mutate(
            demo_3 = case_when(
                demo_3 %in% c("f", "g") ~ "Other**",
                is.na(demo_3) ~ "Other**",
                TRUE ~ demo_3
            ),
            demo_2 = case_when(
                is.na(demo_2) ~ 3,
                TRUE ~ demo_2
            ),
            demo_2 = as.factor(demo_2),
        )
) +
    geom_bar(
        aes(
            x = demo_3,
            group = demo_2,
            fill = demo_2,
        )
    ) +
    scale_x_discrete(
        name = "Income Quintiles",
        limits = c("a", "b", "c", "d", "e", "Other**"),
        breaks = c(
            "a", "b", "c", "d", "e", "Other**"
        ),
        labels = c(
            "a" = "First",
            "b" = "Second",
            "c" = "Third",
            "d" = "Fourth",
            "e" = "Fifth",
            "Other**" = "Other**"
        )
    ) +
    scale_y_continuous(
        position = "right",
        expand = expansion(mult = 0)
    ) +
    scale_fill_manual(
        name = "Most liberal",
        values = blue_to_red_palette(5),
        breaks = c(1,2,3,4,5),
        labels = c("", "", "", "", "Most conservative")
    ) +
    labs(
        title = "Distributions of respondent income and ideological leaning",
        subtitle = "Counts*",
        caption = "*For ideological leaning, NAs are coerced to the central value; **Other includes 'Don't know', 'Prefer not to say', and NAs",
        tag = "Figure 3B"
    ) +
    theme_stigler() +
    theme(
        axis.text.y.right = element_text(
            hjust = 1,
            vjust = -0.6,
            margin = margin(t = 0, r = 10, b = 0, l = -16, unit = "pt"),
            debug = FALSE
        )
    )

ggsave(
    plot = last_plot(),
    filename = "crony_capitalism_1_figs/demographics_income_ideology.png",
    width = 9.7,
    height = 5.5,
    units = "in",
    dpi = 300
)

########## McKinsey-MBS Yes/No: Ideology ##########

ggplot(
    df_full %>%
        select(
            mckinsey_1, demo_2, demo_3
        ) %>%
        filter(!is.na(mckinsey_1)) %>%
        mutate(
            demo_3 = case_when(
                demo_3 %in% c("f", "g") ~ "Other**",
                is.na(demo_3) ~ "Other**",
                TRUE ~ demo_3
            ),
            demo_2 = case_when(
                is.na(demo_2) ~ 3,
                TRUE ~ demo_2
            ),
            demo_2 = as.factor(demo_2),
        )
) +
    geom_bar(
        aes(
            x = demo_2,
            fill = mckinsey_1
        ),
    ) +
    scale_x_discrete(
        name = "Ideological alignment",
        breaks = c(1,2,3,4,5),
        limits = c(1,2,3,4,5),
        labels = c("Most liberal", "Liberal", "Centrist", "Conservative", "Most conservative")
    ) +
    scale_y_continuous(
        position = "right",
        expand = expansion(mult = 0)
    ) +
    scale_fill_stigler(
        name = "Leave the project?",
        breaks = c("a", "b"),
        labels = c("Yes", "No")
    ) +
    labs(
        title = "**Share of respondents who would leave the MBS McKinsey project**",
        subtitle = "Counts, by ideological alignment",
        tag = "Figure 4A"
    ) +
    theme_stigler()

ggsave(
    plot = last_plot(),
    filename = "crony_capitalism_1_figs/mckinsey_binary_ideology.png",
    width = 9.7,
    height = 5.5,
    units = "in",
    dpi = 300
)

########## McKinsey-MBS Yes/No: Income ##########

ggplot(
    df_full %>%
        select(
            mckinsey_1, demo_2, demo_3
        ) %>%
        filter(!is.na(mckinsey_1)) %>%
        mutate(
            demo_3 = case_when(
                demo_3 %in% c("f", "g") ~ "Other**",
                is.na(demo_3) ~ "Other**",
                TRUE ~ demo_3
            ),
            demo_2 = case_when(
                is.na(demo_2) ~ 3,
                TRUE ~ demo_2
            ),
            demo_2 = as.factor(demo_2),
        )
) +
    geom_bar(
        aes(
            x = demo_3,
            fill = mckinsey_1
        ),
    ) +
    scale_x_discrete(
        name = "Income quintile",
        breaks = c("a", "b", "c", "d", "e", "Other**"),
        limits = c("a", "b", "c", "d", "e", "Other**"),
        labels = c("First", "Second", "Third", "Fourth", "Fifth", "Other*"),
    ) + 
    scale_y_continuous(
        position = "right",
        expand = expansion(mult = 0)
    ) +
    scale_fill_stigler(
        name = "Leave the project?",
        breaks = c("a", "b"),
        labels = c("Yes", "No")
    ) +
    labs(
        title = "**Share of respondents who would leave the MBS McKinsey project**",
        subtitle = "Counts, by income quintile",
        tag = "Figure 4B"
    ) +
    theme_stigler() +
    theme(
        axis.text.y.right = element_text(
            hjust = 1,
            vjust = -0.6,
            margin = margin(t = 0, r = 10, b = 0, l = -16, unit = "pt"),
            debug = FALSE
        )
    )

ggsave(
    plot = last_plot(),
    filename = "crony_capitalism_1_figs/mckinsey_binary_income.png",
    width = 9.7,
    height = 5.5,
    units = "in",
    dpi = 300
)


##### TABLES #####

########## Crosstab Salary by Ideology ##########

salary_ideo_df <- df_full %>%
        select(
            mckinsey_2, mckinsey_3, sensetime_4, ibm_5, demo_2
        ) %>%
        rename(
            "McKinsey -- McKinley" = "mckinsey_2",
            "McKinsey -- Colleagues" = "mckinsey_3",
            "SenseTime" = "sensetime_4",
            "IBM" = "ibm_5"
        ) %>%
        group_by(demo_2) %>%
        mutate(
            demo_2 = case_when(
                demo_2 == 1 ~ "Most liberal",
                demo_2 == 2 ~ "Liberal",
                demo_2 == 3 ~ "Centrist",
                demo_2 == 4 ~ "Conservative",
                demo_2 == 5 ~ "Most conservative"
            ),
            demo_2_labelled = paste0(
                demo_2, " (", n(), ")",
                sep = ""
            )
        ) %>%
        mutate(demo_2_labelled = as_factor(demo_2_labelled))

salary_ideo_df$demo_2_labelled <- factor(salary_ideo_df$demo_2_labelled, levels = c(
    "Most liberal (7)",
    "Liberal (15)",
    "Centrist (12)",
    "Conservative (10)",
    "Most conservative (4)"
))

datasummary(
    All(salary_ideo_df) ~ demo_2_labelled * (Mean + Median) + Mean + Median,
    # output = "crony_capitalism_1_figs/salary_ideology_tab.html",
    title = "Table 1: Minimum acceptable salaries by ideological leaning",    
    fmt = 0,
    # output = "crony_capitalism_1_figs/salary_ideology_tab.xlsx",
    output = "gt",
    data = salary_ideo_df
)  %>%
    gtsave(
        filename = "crony_capitalism_1_figs/salary_ideology_tab.png"
    )

########## Crosstab Salary by Program ##########

salary_program_df <- df_full %>%
    select(
            mckinsey_2, mckinsey_3, sensetime_4, ibm_5, demo_1
        ) %>%
        rename(
            "McKinsey -- McKinley" = "mckinsey_2",
            "McKinsey -- Colleagues" = "mckinsey_3",
            "SenseTime" = "sensetime_4",
            "IBM" = "ibm_5"
        ) %>%
        filter(!is.na(demo_1)) %>%
        group_by(demo_1) %>%
        mutate(
            demo_1 = case_when(
                demo_1 == "a" ~ "Undergrad",
                demo_1 == "b" ~ "MBA",
                demo_1 == "c" ~ "MPP",
                demo_1 == "d" ~ "Other"
            ),
            demo_1_labelled = paste0(
                demo_1,
                " (", n(), ")",
                sep = ""
            )
        ) %>%
        mutate(demo_1_labelled = as_factor(demo_1_labelled))

salary_program_df$demo_1_labelled <- factor(salary_program_df$demo_1_labelled, levels = c(
    "Undergrad (16)",
    "MBA (26)",
    "MPP (4)",
    "Other (4)"
))

datasummary(
    All(salary_program_df) ~ demo_1_labelled * (Mean + Median) + Mean + Median,
    fmt = 0,
    title = "Table 2: Minimum acceptable salaries by academic program",
    data = salary_program_df,
    output = "gt"
    # output = "crony_capitalism_1_figs/salary_program_tab.xlsx"
) %>%
    gtsave(
        filename = "crony_capitalism_1_figs/salary_program_tab.png"
    )
