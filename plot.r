# import packages
library(tidyverse)
library(lubridate)
library(showtext)
library(ggsci)

# add fonts for plotting
font_add(
    family = "lmroman",
    regular = "Fonts/lmroman10_regular.ttf",
    bold = "Fonts/lmroman10_bold.ttf",
    italic = "Fonts/lmroman10_italic.ttf",
    bolditalic = "Fonts/lmroman10_bolditalic.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 500)

# read, clean and modify baby names for name matching
paths <- list.files("names")
baby_names <- data.frame()

for (i in paths) {
    temp <- read_delim(paste("names/", i, sep = ""),
        col_names = FALSE, delim = ","
    )
    baby_names <- bind_rows(baby_names, temp)
}

colnames(baby_names) <- c("NAME", "GENDER", "COUNT")

baby_names <- baby_names |>
    mutate(NAME = toupper(NAME)) |>
    group_by(NAME, GENDER) |>
    summarize(COUNT = sum(COUNT)) |>
    pivot_wider(names_from = GENDER, values_from = COUNT) |>
    mutate(
        M = ifelse(is.na(M), 0, M),
        F = ifelse(is.na(F), 0, F),
        GENDER = case_when(
            F / (M + F) >= .75 ~ "F",
            M / (M + F) >= .75 ~ "M"
        )
    ) |>
    ungroup()

# function which assigns gender to name & groups by election year & gender
prepare <- function(file, year) {
    file <- file |>
        mutate(
            ELECTION_YEAR = year,
            TRANSACTION_DT = as.Date(TRANSACTION_DT, format = "%m%d%Y")
        )

    # clean names
    names <- file$NAME

    # first name = all characters after first comma
    first_name <- str_split(names, pattern = ",", simplify = TRUE)[, 2]

    # split by non alphanum- characters and select longest string as first name
    first_name <- sapply(
        str_split(first_name, "[^[:alpha:]]"),
        function(x) x[which.max(nchar(x))]
    )

    file <- file |>
        mutate(FIRST_NAME = first_name) |>
        select(-NAME) |>
        left_join(baby_names, by = c("FIRST_NAME" = "NAME")) |>
        mutate(MONTH = lubridate::month(TRANSACTION_DT)) |>
        group_by(CANDIDATE, GENDER, ELECTION_YEAR, MONTH) |>
        summarize(VALUE = sum(TRANSACTION_AMT)) |>
        pivot_wider(names_from = GENDER, values_from = VALUE) |>
        mutate(RATIO_F = F / (M + F))

    return(file)
}

# read in donation data and run prepare function
donations_2008 <- read_csv("Election_2008/donations_2008.csv")
donations_2008 <- prepare(donations_2008, 2008)

donations_2012 <- read_csv("Election_2012/donations_2012.csv")
donations_2012 <- prepare(donations_2012, 2012)

donations_2016 <- read_csv("don_2016.csv")
donations_2016 <- prepare(donations_2016, 2016)

# change this such that they are in the same format!
donations_2020 <- read_csv("Election_2020/donations_2020.csv")
donations_2020 <-
    donations_2020 |>
    mutate(GENDER = ifelse(GENDER == "NA", NA, GENDER))

donations_2020 <- donations_2020 |>
    mutate(MONTH = lubridate::month(TRANSACTION_DT)) |>
    group_by(CANDIDATE, GENDER, ELECTION_YEAR, MONTH) |>
    summarize(VALUE = sum(TRANSACTION_AMT)) |>
    pivot_wider(names_from = GENDER, values_from = VALUE) |>
    mutate(RATIO_F = F / (M + F)) |>
    ungroup()

# bind files from each election year together & format
joined <- bind_rows(
    donations_2008, donations_2012,
    donations_2016, donations_2020
) |>
    mutate(
        ELECTION_YEAR = as.factor(ELECTION_YEAR),
        PARTY = ifelse(CANDIDATE %in% c("TRUMP", "ROMNEY", "MCCAIN"),
            "REP", "DEM"
        ),
        CANDIDATE = str_to_title(CANDIDATE)
    )

# get the position of the Name label in the plot
pos <- joined |>
    filter(MONTH == "11") |>
    select(CANDIDATE, ELECTION_YEAR, RATIO_F) |>
    rename(POS = RATIO_F)

joined <- joined |>
    left_join(pos, by = c("CANDIDATE", "ELECTION_YEAR"))

# adjust y- position of Trump 2016 such that it does not overlap with Romney
joined <- joined |>
    mutate(
        POS = ifelse(ELECTION_YEAR == 2016 & PARTY == "REP", POS - 0.01, POS)
    )
# assemble plot
joined |>
    filter(MONTH != "12") |>
    ggplot(aes(
        x = MONTH, y = RATIO_F,
        linetype = PARTY,
        group = interaction(ELECTION_YEAR, PARTY),
        color = ELECTION_YEAR,
    )) +
    geom_line() +
    geom_point(
        data = joined |> filter(MONTH == "11"),
        aes(x = MONTH, y = RATIO_F)
    ) +
    geom_text(
        hjust = 0,
        show.legend = FALSE,
        data = joined |> filter(MONTH == "11"),
        aes(x = MONTH + .1, y = POS, label = CANDIDATE)
    ) +
    labs(
        x = NULL, y = NULL,
        color = NULL, linetype = NULL,
        title = "Monthly Female Donation Ratio to the nominated Presidential Candidate",
        subtitle = "Donations to Trump in 2016 include Donations to MAKE AMERICA GREAT AGAIN PAC\nto include more donations",
        caption = "Own Depiction | Source: Federal Election Commission",
    ) +
    scale_y_continuous(
        labels = scales::label_percent(),
        breaks = seq(0.2, 0.6, 0.05)
    ) +
    scale_x_continuous(
        breaks = seq(1, 11, 1),
        expand = expansion(mult = c(.02, .1))
    ) +
    theme_minimal(base_family = "lmroman") +
    theme(
        text = element_text(family = "lmroman"),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 9),
        panel.background = element_rect(color = "black", fill = "#F9F6EE"),
        plot.background = element_rect(color = "white", fill = "#F9F6EE"),
        plot.margin = margin(t = .2, l = .2, b = .2, r = .2, unit = "cm"),
        axis.text = element_text(size = 10)
    ) +
    scale_color_uchicago(breaks = c(2008, 2012, 2016, 2020))

# save output
ggsave("out.png", plot = last_plot(), width = 8, height = 8, dpi = 500)
