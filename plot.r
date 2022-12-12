# import packages
library(tidyverse)
library(lubridate)
library(showtext)
library(ggsci)
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
showtext_opts(dpi = 500)


# read in cleaned files
donations_1988 <- fread("Election_1988/donations_1988.csv", header = TRUE, sep = ",", colClasses = c("character"))
donations_1992 <- fread("Election_1992/donations_1992.csv", header = TRUE, sep = ",", colClasses = c("character"))
donations_1996 <- fread("Election_1996/donations_1996.csv", header = TRUE, sep = ",", colClasses = c("character"))
donations_2000 <- fread("Election_2000/donations_2000.csv", header = TRUE, sep = ",", colClasses = c("character"))
donations_2004 <- fread("Election_2004/donations_2004.csv", header = TRUE, sep = ",", colClasses = c("character"))
donations_2008 <- fread("Election_2008/donations_2008.csv", header = TRUE, sep = ",", colClasses = c("character"))
donations_2012 <- fread("Election_2012/donations_2012.csv", header = TRUE, sep = ",", colClasses = c("character"))
donations_2016 <- fread("Election_2016/donations_2016.csv", header = TRUE, sep = ",", colClasses = c("character"))
donations_2020 <- fread("Election_2020/donations_2020.csv", header = TRUE, sep = ",", colClasses = c("character"))

# also calculate Ratio_F somehow !!!
# bind files from each election year together & format data correctly (leading 0!)
joined <- bind_rows(
    donations_1992, donations_1996, donations_2000,
    donations_2004, donations_2008, donations_2012, donations_2016,
    donations_2020
)

rm(
    donations_1992, donations_1996, donations_2000,
    donations_2004, donations_2008, donations_2012, donations_2016,
    donations_2020
)

joined <- joined |>
    mutate(
        YEAR = as.factor(YEAR),
        TRANSACTION_DT = ifelse(nchar(TRANSACTION_DT) != 8,
            paste0(0, TRANSACTION_DT), TRANSACTION_DT
        ),
        TRANSACTION_DT = as.Date(TRANSACTION_DT, format = "%m%d%Y"),
        Cand_Name = str_to_title(CANDIDATE),
        TRANSACTION_AMT = as.double(TRANSACTION_AMT)
    )

# format data for ratio calculation
ratio_df <- joined |>
    mutate(
        MONTH = lubridate::month(TRANSACTION_DT),
        GENDER = ifelse(is.na(GENDER), "NA", GENDER)
    ) |>
    group_by(Cand_Name, GENDER, YEAR, MONTH, Cand_Party_Affiliation) |>
    summarize(SUM = sum(TRANSACTION_AMT)) |>
    pivot_wider(names_from = GENDER, values_from = SUM) |>
    rename(FEMALE = F, MALE = M, UNKNOWN = `NA`) |>
    as.data.frame()

ratio_df <- ratio_df |>
    mutate(RATIO_F = FEMALE / (FEMALE + MALE))

# get the position of the Name label in the plot
pos <- ratio_df |>
    filter(MONTH == "11") |>
    select(Cand_Name, YEAR, RATIO_F) |>
    rename(POS = RATIO_F)

ratio_df <- ratio_df |>
    left_join(pos, by = c("Cand_Name", "YEAR"))

# adjust y- position of Trump 2016 such that it does not overlap with Romney
joined <- joined |>
    mutate(
        POS = ifelse(ELECTION_YEAR == 2016 & Cand_Party_Affiliation == "REP", POS - 0.01, POS)
    )

# assemble plot
ratio_df <- ratio_df |>
    filter(YEAR %in% c(2008, 2012, 2016, 2020))

as_tibble(ratio_df) |>
    filter(MONTH != "12") |>
    ggplot(aes(
        x = MONTH, y = RATIO_F,
        linetype = Cand_Party_Affiliation,
        group = interaction(YEAR, Cand_Party_Affiliation),
        color = YEAR,
    )) +
    geom_line() +
    geom_point(
        data = as_tibble(ratio_df) |> filter(MONTH == "11"),
        aes(x = MONTH, y = RATIO_F)
    ) +
    geom_text(
        hjust = 0,
        show.legend = FALSE,
        data = as_tibble(ratio_df) |> filter(MONTH == "11"),
        aes(x = MONTH + .1, y = POS, label = Cand_Name)
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
    ggsci::scale_color_uchicago(breaks = c(2008, 2012, 2016, 2020))

# save output
ggsave("out.png", plot = last_plot(), width = 8, height = 8, dpi = 500)

# rato election cycl COUNT DONORS!
ratio_df <- lazy_dt(joined) |>
    mutate(
        GENDER = ifelse(is.na(GENDER), "NA", GENDER)
    ) |>
    group_by(CANDIDATE, GENDER, YEAR, ZIP_CODE, FIRST_NAME, LAST_NAME, PARTY) |>
    summarize(SUM = sum(TRANSACTION_AMT))

ratio_df <- ratio_df |>
    as.data.frame() |>
    lazy_dt() |>
    group_by(CANDIDATE, YEAR, GENDER, PARTY) |>
    summarize(COUNT = n()) |>
    pivot_wider(names_from = GENDER, values_from = COUNT) |>
    rename(FEMALE = F, MALE = M, UNDEFINED = `NA`)

ratio_df <- ratio_df |>
    as.data.frame() |>
    mutate(RATIO_F = FEMALE / (MALE + FEMALE))

# rato election cycl
ratio_df <- joined |>
    mutate(
        GENDER = ifelse(is.na(GENDER), "NA", GENDER)
    ) |>
    group_by(CANDIDATE, GENDER, YEAR, PARTY) |>
    summarize(SUM = sum(TRANSACTION_AMT)) |>
    pivot_wider(names_from = GENDER, values_from = SUM) |>
    rename(FEMALE = F, MALE = M, UNKNOWN = `NA`) |>
    as.data.frame()

ratio_df <- ratio_df |>
    mutate(YEAR = fct_relevel(YEAR, as.character(seq(from = 1988, to = 2020, by = 4))))

ratio_df <- ratio_df |>
    mutate(RATIO_F = FEMALE / (FEMALE + MALE)) |>
    as_tibble()


as_tibble(ratio_df) |>
    ggplot(aes(x = YEAR, y = RATIO_F, group = PARTY, fill = PARTY)) +
    geom_bar(stat = "identity", position = "dodge") +
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
    scale_y_continuous(
        breaks = seq(0, .6, by = .1),
        labels = scales::percent_format(),
        expand = expansion(mult = c(.01, .03))
    ) +
    labs(
        x = NULL, y = NULL,
        fill = NULL, linetype = NULL,
        title = "The Female Donor Gap between Democrates and Republicans",
        subtitle = "Relative Share of Female Donors to the Nominated Presidential Candidate",
        caption = "Own Depiction | Source: Federal Election Commission",
    ) +
    ggsci::scale_fill_jama()

ggsave("out_2.png", plot = last_plot(), width = 12, height = 8, dpi = 500)

# plot in terms of number of donors!
