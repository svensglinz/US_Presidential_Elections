# import packages
library(tidyverse)
library(lubridate)
library(showtext)
library(ggsci)
library(data.table)
library(dtplyr)
library(tidytext)

# add fonts for plotting
font_add(
    family = "lmroman",
    regular = "Fonts/lmroman10_regular.ttf",
    bold = "Fonts/lmroman10_bold.ttf",
    italic = "Fonts/lmroman10_italic.ttf",
    bolditalic = "Fonts/lmroman10_bolditalic.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 1200)

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

rm(donations_1988,
    donations_1992, donations_1996, donations_2000,
    donations_2004, donations_2008, donations_2012, donations_2016,
    donations_2020
)



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

#---------------------------------------------------------------------------------


# report above bar charts also donation share of act blue and winred (get from otherID! need to to data sets again!!!)
# say in post some results are more meaningful than others. others show that 
#making sense of the data and conclusively interpreting them is often very difficult!

ggsave("out_5.png", plot = last_plot(), dpi = 1200)

ggsci::pal_uchicago()

library("scales")
 show_col(pal_uchicago("default")(9))
 show_col(pal_uchicago("light")(9))
 show_col(pal_uchicago("dark")(9))
# put share of no job or occupation somewhere as well in the statistic!
joined <- fread("donations_1988_2020.csv", header = TRUE, sep = ",")

joined |>
    group_by(PARTY, YEAR, IS_RETIRED) |>
    summarize(DONATION = sum(TRANSACTION_AMT))

    group_by(PARTY, YEAR) |>
    summarize(SUM = sum(TRANSACTION_AMT))

# on top rather put percent of total donatiosn through act blue and winred!! (before was not like that!!!)
# mention in text that winred is relatively new, act blue only since 2008! (but advantage: all donations are identified!)

# total donations per candidate and gender
total_donations <- joined |>
    group_by(PARTY, YEAR) |>
    summarize(SUM = sum(TRANSACTION_AMT)) |>
    as.data.table()

unemployed_donations <- filtered |>
    group_by(PARTY, YEAR) |>
    summarize(SUM = sum(SUM)) |>
    as.data.table()

unemployed_donations |>
    left_join(total_donations, by = c("YEAR", "PARTY")) |>
    rename(TOTAL_RETIRED = SUM.x, TOTAL = SUM.y) |>
    mutate(RATIO = TOTAL_RETIRED / TOTAL) |>
    as_tibble() |>
    ggplot(aes(x = YEAR, y = RATIO, group = interaction(YEAR, PARTY), fill = PARTY)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal(base_family = "lmroman") +
    theme(
        text = element_text(family = "lmroman"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size = 10, face = "italic"),
        plot.caption = element_text(size = 9),
        panel.background = element_rect(color = "black", fill = "#F9F6EE"),
        plot.background = element_rect(color = "white", fill = "#F9F6EE"),
        plot.margin = margin(t = .2, l = .2, b = .2, r = .2, unit = "cm"),
        axis.text = element_text(size = 10)
    ) +
    scale_y_continuous(
        breaks = seq(0, .4, by = .1),
        labels = scales::percent_format(),
        expand = expansion(mult = c(.01, .2))
    ) +
    labs(
        x = NULL, y = NULL,
        fill = NULL, linetype = NULL,
        title = "Ageing America",
        subtitle = "Share of Donations from Unemployed / Retired People",
        caption = "Own Depiction | Source: Federal Election Commission",
    ) +
    ggsci::scale_fill_jama()

ggsave("out_6.png", plot = last_plot(), dpi = 500)

data_2020 <- joined |>
    filter(YEAR == 2020) |>

test <- data_2020 |>
    group_by(GENDER, OCCUPATION) |>
    summarize(COUNT = n()) |>
    mutate(GENDER = ifelse(is.na(GENDER), "NA", GENDER)) |>
    pivot_wider(names_from = GENDER, values_from = COUNT) |>
    rename(FEMALE = F, MALE = M, UNDEFINED = `NA`) |>
    as.data.table() |>
    mutate(RATIO = MALE / (MALE + FEMALE))

test |>
    filter(MALE + FEMALE > 100000) |>
    arrange(desc(RATIO))

test <- data_2020 |>
    as.data.table() |>
    group_by(GENDER, OCCUPATION, ZIP_CODE, FIRST_NAME, LAST_NAME) |>
    summarize(SUM = sum(TRANSACTION_AMT)) |>
    filter(!is.na(GENDER)) |>
    as.data.table()


# try this out!
test |>
    filter(!is.na(OCCUPATION)) |>
    add_count(OCCUPATION, FIRST_NAME, name = "OCC_COUNT") |>
    filter(OCC_COUNT > 1000) |>
    arrange(desc(OCC_COUNT)) |>
    group_by(OCCUPATION) |>
    mutate(n = n()) |>
    mutate(RATIO = OCC_COUNT / n) |>
    select(FIRST_NAME, OCCUPATION, RATIO) |>
    unique() |>
    arrange(desc(RATIO)) |>
    slice(1:100) |>
    view()

lazy_dt(donations) |>
    group_by(CANDIDATE, GENDER) |>
    summarize(DONATION = sum(DONATION)) |>
    add_count(CANDIDATE, wt = DONATION, name = "TOTAL_DONATION") |>
    mutate(RATIO = DONATION / TOTAL_DONATION) |>
    replace_na(list(GENDER = "UNCLASSIFIED")) |>
    pivot_wider(names_from = GENDER, values_from = RATIO)

baby_names |>
    arrange(desc(MALE))

donations <- fread("Election_2020/donations_2020.csv", sep = ",", header = TRUE)

head(donations)
donations <- donations |>
    select(CANDIDATE, FIRST_NAME, LAST_NAME, ZIP_CODE, OCCUPATION, TRANSACTION_AMT, GENDER)

###############################################
###############################################

library(mapdata)

zip_codes <- read_csv("uszips.csv") |>
    select(county_name, zip)

donations <-
    lazy_dt(donations_2020) |>
    mutate(
        ZIP_CODE = str_extract(string = as.character(ZIP_CODE),  pattern =  "^[:digit:]{5}")
    ) |>
    as.data.table()

coordinates <- map_data("county")

zip <- lazy_dt(donations) |>
    left_join(zip_codes, by = c("ZIP_CODE" = "zip")) |>
    mutate(county_name = tolower(county_name)) |> 
    as.data.table()

grouped <- zip |>
    group_by(PARTY, ZIP_CODE, county_name) |>
    summarize(DONATION = sum(DONATION)) |>
    as.data.table()

coords <- coordinates |>
    left_join(grouped, by = c("subregion" = "county_name"))

sum(coords$DONATION, na.rm = T)

coords |>
    filter(PARTY == "DEM") |>
    ggplot(
        aes(
            x = long,
            y = lat,
            group = interaction(region, subregion),
            fill = log(DONATION, base = 20)
        )
    ) +
    geom_polygon(color = "black") +
    scale_fill_gradient(
        low = "blue",
        high = "red",
        na.value = "grey50",
        guide = "colorbar",
        aesthetics = "fill"
    )


republican <- major_rep |>
        mutate(OCCUPATION = str_to_title(OCCUPATION)) |>
        ggplot(aes(x = SHARE, y = reorder(OCCUPATION, SHARE))) +
        geom_point(aes(size = DONORS_PER_OCCUP), color = "#800000FF") +
        geom_segment(
            aes(
                x = .7, xend = SHARE,
                yend = reorder(OCCUPATION, SHARE)
            ), , color = "#800000FF"
        ) +
        labs(x = NULL,
        y = NULL,
        title = NULL,
        size = NULL,
        color = NULL,
        subtitle = NULL,
        caption = NULL)+
        scale_x_continuous(labels = scales::label_percent(), limits = c(.7, 1)) +
            theme_minimal() +
            theme(
                text = element_text(family = "lmroman"),
                plot.title = element_text(size = 15, hjust = .5),
                plot.background = element_rect(fill = "#F9F6EE", color = "transparent"),
                panel.grid = element_blank(),
                panel.grid.major.x = element_line(
                    linetype = "dashed",
                    color = "grey"
                ),
                legend.position = "bottom",
                legend.justification = "center"
            ) +
            ggsci::scale_color_uchicago() +
            guides(color = "none")

democrat + republican +
    plot_annotation(
        title = "Professions where Donations are heavily skewed towards one Party",
        caption = "Own Depiction | Source: Federal Election Commission",
        subtitle = "Professions selected by those occuring most often and the share per Party is > 75%",
        theme = theme(
            text = element_text(family = "lmroman"),
            legend.position = "bottom",
            plot.title = element_text(size = 15, face = "bold"),
            plot.subtitle = element_text(face = "italic", margin = margin(t = 0, b = .5, l = 0, r = 0, unit = "cm")),
            plot.background = element_rect(fill = "#F9F6EE", color = "transparent")
            legend.
        )
    )

# add discrete groups for sizes!!!

# all should not include predicate unemoployed or retired! (what if they contain multiples!) say that this could be the case!!!

view(attorney)
test_2 <- lazy_dt(test) |>
    add_count(PARTY, preferredLabel, wt = DONATION, name = "DONATION_PER_PARTY_OCCUPATION") |>
    ungroup() |>
    add_count(preferredLabel, wt = DONATION, name = "DONATION_PER_OCCUPATION") |>
    mutate(SHARE_PARTY = DONATION_PER_PARTY_OCCUPATION / DONATION_PER_OCCUPATION) |>
    filter(DONATION_PER_OCCUPATION > 1000000) |>
    select(PARTY, preferredLabel, SHARE_PARTY, DONATION_PER_OCCUPATION) |>
    unique()

top_20_rep <- test_2 |>
    filter(SHARE_PARTY < 1) |>
    filter(PARTY == "REP") |>
    arrange(desc(SHARE_PARTY)) |>
    slice(1:20)|>
    mutate(DIRECTION = "REPUBLICAN")

# republican donations contain over 500 mio NA! Fix!
top_20_dem <- test_2 |>
    filter(SHARE_PARTY < 1) |>
    filter(PARTY == "DEM") |>
    arrange(desc(SHARE_PARTY)) |>
    slice(1:20) |>
    mutate(DIRECTION = "DEMOCRAT")

library(tidytext)

top_20_dem <- as_tibble(top_20_dem)[1:20, ]
top_20_rep <- as_tibble(top_20_rep)[1:20, ]

plot_df <- bind_rows(as.data.frame(top_20_rep), as.data.frame(top_20_dem))

plot_df |>
    as_tibble() |>
    ggplot(aes(x = SHARE_PARTY,
               y = preferredLabel |> reorder_within(SHARE_PARTY, DIRECTION))) +
    geom_point(aes(color = DONATION_PER_OCCUPATION)) +
        facet_wrap(~DIRECTION, scales = "free") +
        scale_y_reordered() +
        theme(legend.position = "bottom")

test_2 |>
    slice(1:100) |>
    view()

reorder(plot_df$SHARE_PARTY, )

joined <- joined |>
    filter(YEAR == 2020) |>
    as.data.table()

joined |>
    group_by(PARTY, FIRST_NAME, LAST_NAME, ZIP_CODE, STATE, OCCUPATION) |>
    summarize(SUM = sum(TRANSACTION_AMT)) |>
    ungroup() |>
    group_by(PARTY) |> 
    summarize(mean = mean(SUM), median = median(SUM), quant = quantile(SUM, .9)) |>
    as.data.table()

joined <- fread("donations_1988_2020.csv", sep = ",", header = TRUE)
#MOST DONATIONS THROUGH ACTBLUE STANDARD FIELD = UNEMPLOYED, THROUGH WINRED STARNDASRD FIELD = RETIRED --> tHIS COULD EXPLAIN THE SURGE!!!
# some error in the trump data ??? why so many missing donors ? 

#this has also caused serios discussion in the media with the reupbincans claiming foreign influence as it is not 
#possible that so many unemployed people actually donate! (they do it with retired!)


# Post:
# In the last week, we did an exercise on Presidential Donation data @RuserGroup and tried to visualize... 
# As I have first worked with FEC transaction data in my Maturaarbeit, I decided to dig a bit deeper into the 
# data, and among other tried to assign a Gender to each Donors and tried to create some compelling visualizations.

# What I learned: It is often difficult to draw simple colculsions from the data. I tried to point this out a bit
# in some of the chart annotations.
# I wish I had known R during my time in Kanti. Excel did not prove to be a good choice in skimming through millions 
# of transaction records...
# Spending hours on creating charts with GGplot can be a highly effective, but also very learnful way to procrastinate.

# results are highly similar to previous analysis! (must be somewhat correct at least!)
# If you are interested, find below the repo with the code.

# also do the analysis for bernie sanders and check number of individual contributors (according to hidden donors paper! see if results are consistent)