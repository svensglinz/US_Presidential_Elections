# load packages
library(tidyverse)
library(ggsci)
library(cowplot)
library(showtext)
library(data.table)
library(dtplyr)

# add fonts for plotting
font_add(
    family = "lmroman",
    regular = "Fonts/lmroman10_regular.ttf",
    bold = "Fonts/lmroman10_bold.ttf",
    italic = "Fonts/lmroman10_italic.ttf",
    bolditalic = "Fonts/lmroman10_bolditalic.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 800)

# read data set
donations <- fread(
    "donations_1988_2020.csv",
    sep = ",", header = TRUE, colClasses = c("character")
)

# group donations by party, year and professional status
donations <- donations |>
    group_by(PARTY, YEAR, OCCUPATION, EMPLOYER) |>
    summarize(DONATION = sum(as.double(TRANSACTION_AMT))) |>
    ungroup() |>
    as.data.table()

# filter for retired / non-employed people & set Flag (T/F)
# filter for donations where no employer/ occupation entry exists
donations <- donations |>
    mutate(
        EMPLOYER = ifelse(
            grepl(pattern = "^\\s*$", EMPLOYER),
            NA_character_,
            EMPLOYER
        ),
        OCCUPATION = ifelse(
            grepl(pattern = "^\\s*$", OCCUPATION),
            NA_character_,
            OCCUPATION
        ),
        CONTAINS_JOB = ifelse(
            !is.na(EMPLOYER) | !is.na(OCCUPATION), TRUE, FALSE
        ),
        IS_RETIRED = (
            grepl("retir", OCCUPATION, ignore.case = TRUE) |
                grepl("retir", EMPLOYER, ignore.case = TRUE)
        ),
        IS_UNEMPLOYED = (
            grepl("unemploy|not emplo", OCCUPATION, ignore.case = TRUE) |
                grepl("unemploy|not emplo", EMPLOYER, ignore.case = TRUE)
        )
    ) |>
    as.data.table()

# remove entries where no occupational information exists
# group by Party & Year and Calculate donations from
# unemployed and employed share
donations <- donations |>
    filter(CONTAINS_JOB) |>
    select(-CONTAINS_JOB) |>
    group_by(PARTY, YEAR, IS_RETIRED, IS_UNEMPLOYED) |>
    summarize(DONATION = sum(DONATION)) |>
    ungroup() |>
    as.data.table()

# calulate Ratio of Employed / Unemployed / Retired
donations <- lazy_dt(donations) |>
    group_by(PARTY, YEAR) |>
    summarize(
        RETIRED_RATIO = sum(DONATION[IS_RETIRED]) / sum(DONATION),
        UNEMPLOYED_RATIO = sum(DONATION[IS_UNEMPLOYED & !IS_RETIRED]) / sum(DONATION),
        TOTAL_RETIREE_RATIO = sum(DONATION[IS_RETIRED | IS_UNEMPLOYED] / sum(DONATION))
    ) |>
    pivot_longer(3:4) |>
    as_tibble()

# assemble plot
out <- donations |>
    ggplot(aes(x = YEAR, y = value, fill = interaction(name, PARTY))) +
    geom_bar(stat = "identity", position = "stack") +
    theme_minimal(base_family = "lmroman") +
    geom_segment(
        data = donations |> filter(PARTY == "DEM"),
        aes(x = 6, y = .35, xend = 9, yend = .22),
        size = .25,
        arrow = arrow(length = unit(.3, "cm"))
    ) +
    geom_segment(
        data = donations |> filter(PARTY == "DEM"),
        aes(x = 5.5, y = .27, xend = 5, yend = .15),
        size = .25, arrow = arrow(length = unit(.3, "cm"))
    ) +
    geom_text(
        data = donations |> filter(PARTY == "DEM"),
        aes(
            family = "lmroman", x = 6, y = .36, label = "Not -/ Unemployed"
        ),
        size = 3,
        check_overlap = TRUE
    ) +
    geom_text(
        data = donations |> filter(PARTY == "DEM"),
        aes(
            family = "lmroman", x = 5.5, y = .285,
            label = "Retired"
        ),
        size = 3,
        check_overlap = TRUE
    ) +
    theme(
        text = element_text(family = "lmroman"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(
            size = 10, face = "italic",
            margin = margin(t = 0, b = .3, l = 0, r = 0, unit = "cm")
        ),
        plot.caption = element_text(
            size = 9, margin = margin(t = 4, l = 0, r = 0, b = 0, unit = "cm")
        ),
        panel.background = element_rect(color = "black", fill = "#F9F6EE"),
        plot.background = element_rect(color = "white", fill = "#F9F6EE"),
        plot.margin = margin(t = .5, l = 1, b = .5, r = 1, unit = "cm"),
        axis.text = element_text(size = 10),
        legend.position = "none",
    ) +
    scale_y_continuous(
        breaks = seq(0, .4, by = .1),
        labels = scales::percent_format(),
        expand = expansion(mult = c(.01, .05))
    ) +
    labs(
        x = NULL, y = NULL,
        fill = NULL, linetype = NULL,
        title = "With Age comes Political Influence?",
        subtitle = paste0(
            "Share of Donations from Unemployed/ Retired People\n",
            "100% = Donations which contain an Employer / Occupation (1988 - 2000  ~ 50%, 2004 - 2020 ~ 90% )"
        ),
        caption = "Own Depiction | Source: Federal Election Commission",
    ) +
    scale_fill_manual(
        values = c(
            "RETIRED_RATIO.DEM" = "#5B8FA8FF",
            "RETIRED_RATIO.REP" = "#B1746FFF",
            "UNEMPLOYED_RATIO.DEM" = "#124860",
            "UNEMPLOYED_RATIO.REP" = "#581b15"
        )
    ) +
    facet_wrap(~PARTY)

text_1 <- paste("The Donation Platform ActBlue which collects donations for the Democratic Party asks 'are you employed' to identify their Donor's Occupation.\n",
    "In turn, the Republican Donation Platoform Winred provides a radio button which says 'I am Retired'. As data shows, around 50% of Winred Donors\n",
    "identified as 'Retired' and around 50% of ActBlue Donors identified as Not Employed in 2020 Donations. Note that donations through Winred and\n",
    "Actbue have only made up a significant part of the two final Candidates donations in 2020. The huge spike in Not Employed Contributions to Democrats \n",
    "between 2016 and 2020 is likely due to ActBlue's framing of the Employment question, where Democratic Retirees who previously identified as 'retired'\n",
    "chose to state 'not employed' instead. Further, the spike in overall contribution from retirees in 2020 could also partly be due to the convenience \n",
    "of opting for the Not Employed / Retired Option when donating through ActBue or Winred",
    sep = ""
)

text_2 <- c("For an interesting alternative interpretation of the results, check out Fox News at:  bit.ly/3HCTYSt :)")

ggdraw(out) + draw_label(text_1,
    x = .06, y = .11, hjust = 0, vjust = 0,
    size = 8, fontfamily = "lmroman"
) +
    draw_label(text_2,
        x = .06, y = .08, hjust = 0, vjust = 0,
        size = 8, fontfamily = "lmroman",
        fontface = "bold"
    )

ggsave("Plots/share_retired.png", plot = last_plot(), dpi = 800, width = 11, height = 7.35)
