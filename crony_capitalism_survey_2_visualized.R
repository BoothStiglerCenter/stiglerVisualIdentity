#### PREAMBLE ####
library(tidyverse)
library(readxl)
library(ggdist)
library(scales)
library(modelsummary)
library(gt)
source("r_materials/theme_stigler.R")
print(getwd())

#### READING DATA IN ####
data_in <- read_excel(
    "crony_capitalism_survey_2.xlsx"
)

df <- data_in %>%
    select(
        id, farandia_1, farandia_2, farandia_3, farandia_4,
        farandia_5, farandia_6, farandia_7,
        farandia_8, farandia_9, farandia_10,
        demo_1, demo_2, demo_3, demo_4
    ) %>%
    mutate(
        across(
            farandia_5:farandia_7, ~ ifelse(
                .x == "y",
                "Yes", ifelse(
                    .x == "n",
                    "No", NA
                ))
        ),
        across(
            farandia_8:farandia_10, ~ ifelse(
                .x == "y",
                "Yes", ifelse(
                    .x == "n",
                    "No", NA
                ))
        ),
        demo_1 = case_when(
            demo_1 == "a" ~ "Undergrad",
            demo_1 == "b" ~ "MBA",
            demo_1 == "c" ~ "MPP",
            demo_1 == "d" ~ "Other",
            TRUE ~ NA
        ),
        demo_2 = case_when(
            demo_2 == 1 ~ "Most liberal",
            demo_2 == 2 ~ "Liberal",
            demo_2 == 3 ~ "Centrist",
            demo_2 == 4 ~ "Conservative",
            demo_2 == 5 ~ "Most Conservative",
            TRUE ~ NA
        ),
        demo_3 = case_when(
            demo_3 == "a" ~ "First",
            demo_3 == "b" ~ "Second",
            demo_3 == "c" ~ "Third",
            demo_3 == "d" ~ "Fourth",
            demo_3 == "e" ~ "Fifth",
            demo_3 == "f" ~ "Other",
            demo_3 == "g" ~ "Other",
            TRUE ~ NA
        ),
        demo_4 = case_when(
            demo_4 == "a" ~ "Male",
            demo_4 == "b" ~ "Female",
            demo_4 == "c" ~ "Other",
        )
    )


#### PLOTS ####
########## Figure 1: Vote Counts ##########
ggplot(
    df %>% select(
        farandia_5:farandia_10, "id"
    ) %>%
    pivot_longer(
        cols = farandia_5:farandia_10,
        names_to =  "question_name",
        values_to = "answer"
    ) %>%
    filter(
        !is.na(answer)
    ) %>%
    group_by(
        question_name, answer
    ) %>%
    mutate(
        count = n()
    )
) +
    geom_bar(
        aes(
            x = question_name,
            fill = answer
        )
    ) +
    scale_fill_stigler(
        name = "Vote in favor of shareholder resolution"
    ) + 
    scale_x_discrete(
        limits = c(
            "farandia_5", "farandia_6", "farandia_7",
            "farandia_8", "farandia_9", "farandia_10"
        ),
        labels = c(
            "Q5: 20% RoI", "Q6: 10% RoI", "Q7: 0% RoI",
            "Q8: 20% RoI", "Q9: 10% RoI", "Q10: 0% RoI"
        ),
        name = ""
    ) +
    scale_y_continuous(
        position = "right",
        expand = expansion(mult = 0)
    ) +
    labs(
        title =  "Votes in favor of selling* or freeing** ACME slaves",
        subtitle = "Counts",
        caption = "Note: *ex ante* RoI given as 25%. *Questions 5, 6, and 7 pertain to a motion to sell slaves; **Questions 8, 9, and 10 pertain to a motion to free slaves.",
        tag = "Figure 1"
    ) +
    theme_stigler() +
    theme(
        axis.text.y.right = element_text(
            hjust = 0,
            margin = margin(t = -0, r = 0, b = 0, l = 0, unit = "pt"),
        )
    )

ggsave(
    plot = last_plot(),
    filename = "crony_capitalism_2_figs/figure_1_shareholder_votes.png",
    width = 11.5,
    height = 6.57,
    units = "in",
    dpi = 300
)

#### TABLES ####
########## Crosstab Salary by Academic Program ##########
salary_program_df <- df %>%
    select(
        c(farandia_2, farandia_4, demo_1)
    ) %>%
    filter(!is.na(demo_1)) %>%
    rename(
        "ACME" = "farandia_2",
        "ACME -- Supplier" = "farandia_4"
    ) %>%
    group_by(demo_1) %>%
    mutate(
        demo_1_labelled = paste0(
            demo_1, " (" , n(),  ")",
            sep = ""
        ),
        demo_1_labelled = as_factor(demo_1_labelled)
    )

salary_program_df$demo_1_labelled <- factor(
    salary_program_df$demo_1_labelled,
    levels = c(
        "Undergrad (13)",
        "MBA (26)",
        "MPP (3)",
        "Other (5)"
    )
)

datasummary(
    All(salary_program_df) ~ demo_1_labelled * (Mean + Median) + Mean + Median,
    title = "Table 3: Minimum acceptable salaries by academic program",
    fmt = 0,
    output = "gt",
    data = salary_program_df
) %>%
    gtsave(
        filename = "crony_capitalism_2_figs/tab_1_salary_program.png"
    )

########## Crosstab Salary by ideology ##########
salary_ideo_df <- df %>%
    select(
        c(farandia_2, farandia_4, demo_2)
    ) %>%
    filter(!is.na(demo_2)) %>%
     rename(
        "ACME" = "farandia_2",
        "ACME -- Supplier" = "farandia_4"
    ) %>%
    group_by(demo_2) %>%
    mutate(
        demo_2_labelled = paste0(
            demo_2, " (", n(),  ")",
            sep = ""
        ),
        demo_2_labelled = as_factor(demo_2_labelled)
    )

salary_ideo_df$demo_2_labelled <- factor(
    salary_ideo_df$demo_2_labelled,
    levels = c(
        "Most liberal (5)",
        "Liberal (13)",
        "Centrist (17)",
        "Conservative (10)",
        "Most conservative (1)"
        )
)

datasummary(
    All(salary_ideo_df) ~ demo_2_labelled * (Mean + Median) + Mean + Median,
    title = "Table 4: Minimum acceptable salaries by ideological learning",
    fmt = 0,
    output = "gt",
    data = salary_ideo_df
) %>%
    gtsave(
        filename = "crony_capitalism_2_figs/tab_2_salary_ideology.png"
    )
