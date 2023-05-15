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

crony_capitalism_1_in <- read_excel(
    "crony_capitalism_survey_1.xlsx"
)

crony_capitalism_1_df <- crony_capitalism_1_in %>%
    select(
        mckinsey_2, mckinsey_3, sensetime_4, ibm_5
    ) %>%
    pivot_longer(
        cols = c(mckinsey_2, mckinsey_3, sensetime_4, ibm_5),
        names_to = "question_name",
        values_to = "alt_salary"
    ) %>%
    group_by(
        question_name
    ) %>%
    summarize(
        mean_alt_salary = mean(alt_salary, na.rm = TRUE),
        median_alt_salary = median(alt_salary, na.rm = TRUE)
    ) %>%
    view()


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

########## Figure 2: Salary Cuts ##########


alt_salaries_df <- df %>%
    select(farandia_2, farandia_4) %>%
    pivot_longer(
        cols = c(farandia_2, farandia_4),
        names_to = "question_name",
        values_to = "alt_salary"
    ) %>%
    group_by(question_name) %>%
    summarize(
        mean_alt_salary = mean(alt_salary, na.rm = TRUE),
        median_alt_salary = median(alt_salary, na.rm = TRUE)
    ) %>%
    rbind(
        crony_capitalism_1_df
    ) %>%
    pivot_longer(
        cols = c("mean_alt_salary", "median_alt_salary"),
        names_to = "statistic",
        values_to = "alt_salary"
    ) %>%
    mutate(
        salary_cut = case_when(
            str_detect("mckinsey", question_name) ~ 120000 - alt_salary,
            TRUE ~ 100000 - alt_salary
        )
    ) %>%
    view()

alt_salaries_df %>%
    mutate(
        question_name = case_when(
            question_name == "farandia_2" ~ "ACME",
            question_name == "farandia_4" ~ "ACME -- Upstream",
            question_name == "mckinsey_2" ~ "McKinsey -- MBS",
            question_name == "mckinsey_3" ~ "McKinsey -- Colleagues",
            question_name == "sensetime_4" ~ "SenseTime",
            question_name == "ibm_5" ~ "IBM",
        ),
        statistic = case_when(
            statistic == "mean_alt_salary" ~ "Mean",
            statistic == "median_alt_salary" ~ "Median",
        )
    ) %>%
    rename(
        "Alternative_salary" = "alt_salary",
        "Salary_cut" = "salary_cut"
    ) %>%
    write_csv(
        "crony_capitalism_2_figs/aggregated_alternative_salaires.csv"
    ) %>%
    view()


ggplot(mean_alt_salaries_df) +
    geom_col(
        aes(
            x = question_name,
            y = salary_cut,
            fill = statistic,
            group = statistic
        ),
        position = "dodge"
    ) +
    scale_fill_stigler(
        name = "Statistic",
        breaks = c("mean_alt_salary", "median_alt_salary"),
        labels = c("Mean", "Median")
    ) +
    scale_y_reverse(
        labels = scales::dollar,
        position = "right",
        expand = expansion(mult = 0)
    ) +
    scale_x_discrete(
        name = "Question",
        limits = c(
            "mckinsey_2", "mckinsey_3", "sensetime_4",
            "ibm_5", "farandia_2", "farandia_4"
        ),
        labels = c(
            "McKinsey -- MBS", "McKinsey -- Colleagues", "SenseTime", "IBM", "ACME", "ACME -- Upstream"
        ),
        position = "top"
    ) + 
    labs(
        title = "Maximum-acceptable salary cuts",
        subtitle = "US Dollars, 2023",
        tag = "Figure 2",
        notes = "Note : Initial compensation for McKinsey assumed to be $120,000; all others have initial compensation of $100,000"
    ) + 
    theme_stigler()


ggsave(
    plot = last_plot(),
    filename = "crony_capitalism_2_figs/figure_2_mean_med_salary_cuts.png",
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
