library(tidyverse)

library(tinytable)
library(ggpubr)

library(boot)
library(boot.pval)

# Load WE Data ------------------------------------------------------------

healthcare <- read.csv("data/healthcare_chrono_means_wCI.csv") |>
  mutate(year = factor(year)) |>
  select(-X) |>
  mutate(
    year = factor(year),
    type = ifelse(type == "racialization", "race", type),
    type = ifelse(type == "poverty", "inequality", type),
    mean = ifelse(type == "obamacare" & year %in% c(2004, 2005, 2006, 2007), NA, mean),
    ci.low = ifelse(type == "obamacare" & year %in% c(2004, 2005, 2006, 2007), NA, ci.low),
    ci.high = ifelse(type == "obamacare" & year %in% c(2004, 2005, 2006, 2007), NA, ci.high)
  )

immig <- read.csv("data/immigration_chrono_means_wCI.csv") |>
  mutate(year = factor(year)) |>
  select(-X) |>
  mutate(
    source = "NYT",
    year = factor(year),
  )

crime <- read.csv("data/crime_chrono_means_wCI.csv") |>
  mutate(year = factor(year)) |>
  select(-X) |>
  mutate(
    source = "NYT",
    year = factor(year),
  )

# Plot: Placebo -----------------------------------------------------------

plot_hc_placebo <- ggplot(
  healthcare |>
    filter(type == "placebo"),
  aes(x = year, y = mean, group = 1)
) +
  geom_smooth(method = "loess") + 
  geom_point() + 
  ylim(0, .12) + 
  scale_color_grey() +
  scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020)) +
  labs(
    x = "",
    y = "Cosine Similarity",
    title = "Healthcare"
  ) +
  theme_bw() + 
  theme(text = element_text(family = "Times New Roman")) 

plot_hc_placebo

plot_immig_placebo <- ggplot(
  immig |>
    filter(type == "placebo"),
  aes(x = year, y = mean, group = 1)
) +
  geom_smooth(method = "loess") + 
  ylim(0, 0.1) + 
  geom_point() + 
  scale_color_grey() +
  scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020)) +
  labs(
    x = "",
    y = "Cosine Similarity",
    title = "Immigration",
    caption = "Note: Loess smoothing is used to show the trend in the data. The shaded area represents the 95% confidence interval."
  ) +
  theme_bw() + 
  theme(text = element_text(family = "Times New Roman")) 

plot_immig_placebo

plot_crime_placebo <- ggplot(
  crime |>
    filter(type == "placebo"),
  aes(x = year, y = mean, group = 1)
) +
  ylim(0, .1) + 
  geom_smooth(method = "loess") + 
  geom_point() + 
  scale_color_grey() +
  scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020)) +
  labs(
    x = "",
    y = "Cosine Similarity",
    title = "Crime"
  ) +
  theme_bw() + 
  theme(text = element_text(family = "Times New Roman")) 

plot_crime_placebo

plot_placebo_combined <- ggarrange(
  plot_crime_placebo,
  plot_hc_placebo,
  plot_immig_placebo,
  ncol = 1,
  nrow = 3,
  heights = c(.8, .8, 1)
)

ggsave("paper/figures/plot_placebo_combined.png",
  plot_placebo_combined,
  width = 6,
  height = 5, dpi = 300
)


# Plot: External Validity -------------------------------------------------

# HC Obamacare

plot_hc_ocare <- ggplot(
  healthcare |>
    filter(type %in% c("obamacare", "obama", "placebo")),
  aes(x = year, y = mean, group = type, color = type)
) +
  geom_line() +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.high), width = 0.1) +
  geom_vline(xintercept = "2008", linetype = "dashed") +
  geom_vline(xintercept = "2017", linetype = "dashed") +
  annotate("text",
    x = "2008", y = 0.4, label = "First references \nto Obamacare",
    hjust = -.1,
    size = 3
  ) +
  ylim(0, .5) + 
  annotate("text",
    x = "2017", y = 0.4, label = "Repeal of \nindividual mandate",
    hjust = -.1,
    size = 3
  ) +
  ylim(0,.5) + 
  scale_color_grey() +
  annotate("text",
    x = "2010", y = .17, label = "Obamacare",
    hjust = -.1, color = "gray50"
  ) +
  annotate("text",
    x = "2011", y = 0.05, label = "Placebo",
    hjust = -.1, color = "gray"
  ) +
  annotate("text",
           x = "2020", 
           y = .25,
           label = "Obama", 
           color = "black") + 
  scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020)) +
  labs(
    x = "",
    y = "Cosine Similarity",
    title = "Healthcare + Obamacare"
  ) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman"))

plot_hc_ocare

# Immig Wall

plot_immig_wall <- ggplot(
  immig |>
    filter(type %in% c("placebo", "wall")),
  aes(x = year, y = mean, group = type, color = type)
) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.high), width = 0.1) +
  geom_line() +
  scale_color_manual(values = c("grey", "black")) +
  geom_vline(xintercept = "2015", linetype = "dashed") +
  annotate("text",
    x = "2012", y = .4, label = "Start of Trump\n Campaign",
    hjust = -.1,
    size = 3
  ) +
  ylim(-.02, .5) + 
  annotate("text",
    x = "2021", y = .2, label = "Wall",
    hjust = -.1, color = "black"
  ) +
  annotate("text",
    x = "2011", y = .15, label = "Placebo",
    hjust = -.1, color = "gray"
  ) +
  scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020)) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Cosine Similarity",
    title = "Immigration + Wall",
    caption = "Note: Each estimate is the average distance between each policy and the specified target term/placebo\n 95% confidence intervals are included in the graphic, but are sufficiently small so are\n difficult to distinguish from the point estimates."
  ) +
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman"))

plot_immig_wall

# Crime Asian

plot_crime_asian <- ggplot(
  crime |>
    filter(type %in% c("placebo", "hatecrime")),
  aes(x = year, y = mean, group = type, color = type)
) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.high), width = 0.1) +
  geom_line() +
  scale_color_manual(values = c("black", "grey")) +
  geom_vline(xintercept = "2020", linetype = "dashed") +
  annotate("text",
    x = "2018", y = 0.4, label = "Onset of \nCOVID-19\n in US",
    size = 3
  ) +
  ylim(-.1, .5) + 
  annotate("text",
    x = "2012", y = .2, label = "Placebo",
    hjust = -.1, color = "grey"
  ) +
  annotate("text",
    x = "2015", y = .13, label = "Hate Crime + Asian",
    hjust = .1, color = "black"
  ) +
  scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020)) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Cosine Similarity",
    title = "Hate Crime + Asian"
  ) +
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman"))

plot_crime_asian

plot_evalid_combined <- ggarrange(
  plot_crime_asian,
  plot_hc_ocare,
  plot_immig_wall,
  ncol = 1,
  nrow = 3,
  heights = c(.8, .8, 1)
)

ggsave("paper/figures/plot_evalid_combined.png",
  plot_evalid_combined,
  width = 6,
  height = 6.5, dpi = 300
)


# Plot: Race --------------------------------------------------------------

plot_hc_race <- ggplot(
  healthcare |>
    filter(type %in% c("race", "placebo")),
  aes(x = year, y = mean, color = type, group = type)
) +
  scale_color_manual(values = c("grey", "black")) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.high), width = 0.1) +
  geom_rect(aes(xmin = "2008", 
                xmax = "2009", 
                ymin = -Inf, 
                ymax = Inf), 
            alpha = 0.01, 
            color = "NA",
            fill = "dodgerblue") +
  geom_line() +
  annotate("text", 
           x = "2011", 
           y = .3, 
           label = "Tesler (2012)", 
           color = "dodgerblue",
           alpha = .7) + 
  annotate("text",
    x = "2020", y = .25, label = "Race",
    hjust = -.1, color = "black"
  ) +
  ylim(0,.5) + 
  annotate("text",
    x = "2020", y = .05, label = "Placebo",
    hjust = -.1, color = "grey"
  ) +
  scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020)) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Cosine Similarity",
    title = "Healthcare + Race"
  ) +
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman"))

plot_hc_race

plot_immig_race <- ggplot(
  immig |>
    filter(type %in% c("racialization", "placebo")),
  aes(x = year, y = mean, color = type, group = type)
) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.high), width = 0.1) +
  geom_line() +
  annotate("text",
           x = "2020", y = .25, label = "Race",
           hjust = -.1, color = "black"
  ) +
  annotate("text", 
           x = "2013", y = 0.05, label = "Placebo", 
           hjust = -.1, color = "grey") +
  scale_color_manual(values = c("grey", "black", "grey30")) +
  scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020)) +
  theme_bw() +
  ylim(0, .5) + 
  labs(
    x = "Year",
    y = "Cosine Similarity",
    title = "Immigration + Race",
    caption = "Note: Each estimate is the average distance between each policy and the specified target term/placebo\n 95% confidence intervals are included in the graphic, but are sufficiently small so are\n difficult to distinguish from the point estimates."
  ) +
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman"))

plot_immig_race

plot_crime_race <- ggplot(
  crime |>
    filter(type %in% c("racialization", "placebo")),
  aes(x = year, y = mean, color = type, group = type)
) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.high), width = 0.1) +
  geom_line() +
  annotate("text",
           x = "2018", y = .28, label = "Race",
           hjust = -.1, color = "black"
  ) +
  annotate("text", 
           x = "2013", y = 0.05, label = "Placebo", 
           hjust = -.1, color = "grey") +
  scale_color_manual(values = c("grey", "black", "grey30")) +
  scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020)) +
  theme_bw() +
  ylim(0, .5) + 
  labs(
    x = "Year",
    y = "Cosine Similarity",
    title = "Crime + Race"
  ) + 
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman"))

plot_crime_race

plot_race_combined <- ggarrange(
  plot_crime_race,
  plot_hc_race,
  plot_immig_race,
  ncol = 1, 
  nrow = 3, 
  heights = c(.8, .8, 1))

ggsave("paper/figures/plot_race_combined.png",
       plot_race_combined,
       width = 6,
       height = 6.5, dpi = 300
)


# Plot: Ethnicity ---------------------------------------------------------

plot_hc_ethn <- ggplot(
  healthcare |>
    filter(type %in% c("black", "placebo")),
  aes(x = year, y = mean, color = type, group = type)
) +
  scale_color_manual(values = c("black", "grey")) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.high), width = 0.1) +
  geom_rect(aes(xmin = "2008", 
                xmax = "2009", 
                ymin = -Inf, 
                ymax = Inf), 
            alpha = 0.01, 
            color = "NA",
            fill = "dodgerblue") +
  geom_line() +
  annotate("text", 
           x = "2011", 
           y = .3, 
           label = "Tesler (2012)", 
           color = "dodgerblue",
           alpha = .7) + 
  annotate("text",
           x = "2020", y = .2, label = "Black",
           hjust = -.1, color = "black"
  ) +
  ylim(0,.5) + 
  annotate("text",
           x = "2020", y = .05, label = "Placebo",
           hjust = -.1, color = "grey"
  ) +
  scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020)) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Cosine Similarity",
    title = "Healthcare + Black"
  ) +
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman"))

plot_hc_ethn

plot_immig_ethn <- ggplot(
  immig |>
    filter(type %in% c("black", "latinx","asian", "placebo")),
  aes(x = year, y = mean, color = type, group = type)
) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.high), width = 0.1) +
  geom_line() +
  annotate("text",
           x = "2020", y = .25, label = "Latinx",
           hjust = -.1, color = "grey50"
  ) +
  annotate("text",
           x = "2020", y = 0.05, label = "Black",
           color = "black"
  ) +
  annotate("text", 
           x = "2013", y = 0.05, label = "Placebo", 
           hjust = -.1, color = "grey") +
  scale_color_manual(values = c("black", "grey50", "grey")) +
  scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020)) +
  theme_bw() +
  ylim(0, .5) + 
  labs(
    x = "Year",
    y = "Cosine Similarity",
    title = "Immigration + Race",
    caption = "Note: Each estimate is the average distance between each policy and the specified target term/placebo\n 95% confidence intervals are included in the graphic, but are sufficiently small so are\n difficult to distinguish from the point estimates."
  ) +
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman"))

plot_immig_ethn

plot_crime_ethn <- ggplot(
  crime |>
    filter(type %in% c("black", "latinx", "placebo")),
  aes(x = year, y = mean, color = type, group = type)
) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.high), width = 0.1) +
  geom_line() +
  annotate("text",
           x = "2010", y = .3, label = "Latinx",
           hjust = -.1, color = "grey50"
  ) +
  annotate("text",
           x = "2018", y = 0.2, label = "Black",
           color = "black"
  ) +
  annotate("text", 
           x = "2013", y = 0.05, label = "Placebo", 
           hjust = -.1, color = "grey") +
  scale_color_manual(values = c("black", "grey50", "grey")) +
  scale_x_discrete(breaks = c(2004, 2008, 2012, 2016, 2020)) +
  theme_bw() +
  ylim(0, .5) + 
  labs(
    x = "Year",
    y = "Cosine Similarity",
    title = "Crime + Race"
  ) + 
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman"))

plot_crime_ethn

plot_ethn_combined <- ggarrange(
  plot_crime_ethn,
  plot_hc_ethn,
  plot_immig_ethn,
  ncol = 1, 
  nrow = 3, 
  heights = c(.8, .8, 1))

ggsave("paper/figures/plot_ethn_combined.png",
       plot_ethn_combined,
       width = 6,
       height = 6.5, dpi = 300
)


# Plot: Mentions of Race --------------------------------------------------

race_mentions <- read.csv("data/crime_grouped_mentions.csv") |> 
  mutate(Type = "crime") |>
  bind_rows(read.csv("data/immigration_grouped_mentions.csv")) |> 
  mutate(Type = ifelse(is.na(Type), "immigration", Type)) |> 
  bind_rows(read.csv("data/healthcare_grouped_mentions.csv")) |>
  mutate(Type = ifelse(is.na(Type), "healthcare", Type)) |>
  mutate(logged = log(racialization_mentions))

line_annotations <- race_mentions %>%
  group_by(Type) %>%
  summarize(Year = last(Year), racialization_mentions = last(racialization_mentions))

plot_race_mentions <- ggplot(race_mentions, aes(x = Year, y = racialization_mentions, color = Type)) + 
  geom_point() +
  geom_line() + 
  geom_text(data = line_annotations, aes(label = Type), vjust = -0.5, hjust = 0.5, nudge_y = 0.1, check_overlap = TRUE) +  # Add text annotations for each line
  scale_color_grey() + 
  theme_bw() + 
  labs(x = "", 
       y = "Articles that mention race")+ 
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman"))

ggsave("paper/figures/plot_mentions.png",
       plot_race_mentions,
       width = 5,
       height = 3
)


# ANES Load ---------------------------------------------------------------

library(broom)
library(tidyverse)

anes <- read_csv("data/anes_timeseries_cdf_csv_20220916.csv") |>
  filter(VCF0004 %in% c(2004, 2008, 2012, 2016, 2020)) |>
  select(VCF0301, VCF0803, VCF0004, VCF0894, VCF0009z, VCF0806, VCF0206, VCF9039, VCF9040, VCF9041, VCF9042) |>
  rename(
    year = VCF0004,
    party = VCF0301,
    ideology = VCF0803,
    healthcare = VCF0806,
    welfare = VCF0894,
    thermometer = VCF0206,
    conditions = VCF9039,
    special = VCF9040,
    try = VCF9041,
    deserve = VCF9042,
    weight = VCF0009z
  ) |>
  mutate(
    rrb = ifelse(conditions %in% c(8, 9), NA, conditions),
    rra = case_when(
      special == 5 ~ 1,
      special == 4 ~ 2,
      special == 3 ~ 3,
      special == 2 ~ 4,
      special == 1 ~ 5,
      TRUE ~ NA
    ),
    try = case_when(
      try == 5 ~ 1,
      try == 4 ~ 2,
      try == 3 ~ 3,
      try == 2 ~ 4,
      try == 1 ~ 5,
      TRUE ~ NA
    ),
    favors = ifelse(special %in% c(8, 9, 0), NA, special),
    party = ifelse(party %in% c(8, 9, 0), NA, party),
    ideology = ifelse(ideology %in% c(8, 9, 0), NA, ideology),
    thermometer = ifelse(thermometer %in% c(8, 9, 0), NA, thermometer),
    healthcare = ifelse(healthcare %in% c(8, 9, 0), NA, healthcare),
    scaled_hc = healthcare / 7, # As per Tesler, converting to 0 to 1
    welfare = ifelse(welfare >= 8, NA, welfare),
    scaled_wf = welfare / 3, # As per Tesler, converting to 0 to 1
    rr2 = rra / 10 + rrb / 10, # As per Tesler, converting to 0 to 1
    rr4 = rra / 20 + rrb / 20 + try / 20 + special / 20, # As per Tesler, converting to 0 to 1
    scaled_ideo = ideology / 7, # As per Tesler, converting to 0 to 1
    scaled_party = party / 7 # As per Tesler, converting to 0 to 1
  )


# ANES Healthcare Models --------------------------------------------------

tempdf <- tibble()

years <- c(2004, 2008, 2012, 2016, 2020)

for (i in years) {
  test <- lm(scaled_hc ~ rr2 + scaled_party + scaled_ideo,
             data = anes |> filter(year == i),
             weights = weight
  ) |>
    tidy() |>
    filter(term == "rr2") |>
    mutate(year = i)
  
  tempdf <- bind_rows(tempdf, test)
}

anes_hc_df <- tempdf |>
  rename(mean = estimate) |>
  mutate(
    ci.low = mean - 1.96 * std.error,
    ci.high = mean + 1.96 * std.error,
    source = "ANES",
    year = factor(year)
  ) |>
  select(year, source, mean, ci.low, ci.high)

# CES Load ----------------------------------------------------------------

library(dataverse)
## aca - 1:Support 2:Oppose
## rr - 1:Strongly Agree 5:Strongly Disagree
cces2012 <- get_dataframe_by_name(
  filename = "commoncontent2012.tab",
  dataset = "10.7910/DVN/HQEVPK",
  server = "dataverse.harvard.edu"
) |>
  rename(
    case_id = V101,
    rra = CC422a,
    rrb = CC422b,
    weight = V103,
    aca = CC332G
  ) |>
  mutate(aca = ifelse(aca == 1, 1, 0)) |>
  select(case_id, pid7, ideo5, rra, rrb, aca, weight) |>
  mutate(year = 2012)

## aca - 1:Support 2: Oppose
## rr - 1:Strongly Agree 5:Strongly Disagree
cces2014 <- get_dataframe_by_name(
  filename = "CCES14_Common_Content_Validated.tab",
  dataset = "10.7910/DVN/XFXJVY",
  server = "dataverse.harvard.edu"
) |>
  rename(
    case_id = V101,
    rra = CC422a,
    rrb = CC422b,
    healthcare = CC426_2,
    welfare = CC426_1
  ) |>
  mutate(welfare = case_when(
    welfare >= 1 & welfare <= 2 ~ 1,
    welfare == 3 ~ 2,
    welfare >= 4 & welfare <= 5 ~ 3,
    TRUE ~ NA
  )) |>
  select(case_id, pid7, ideo5, rra, rrb, healthcare, welfare, weight) |>
  mutate(year = 2014)

cces2016 <- get_dataframe_by_name(
  filename = "CCES16_Common_OUTPUT_Feb2018_VV.tab",
  dataset = "10.7910/DVN/GDF6Z0",
  server = "dataverse.harvard.edu"
) |>
  rename(
    case_id = V101,
    healthcare = CC16_426_2,
    weight = commonweight,
    welfare = CC16_426_1
  ) |>
  mutate(welfare = case_when(
    welfare >= 1 & welfare <= 2 ~ 1,
    welfare == 3 ~ 2,
    welfare >= 4 & welfare <= 5 ~ 3,
    TRUE ~ NA
  )) |>
  select(case_id, pid7, ideo5, healthcare, welfare, weight) |>
  mutate(year = 2016)

cces2018 <- get_dataframe_by_name(
  filename = "cces18_common_vv.tab",
  dataset = "10.7910/DVN/ZSBZ7K",
  server = "dataverse.harvard.edu"
) |>
  rename(
    case_id = caseid,
    rra = CC18_422e,
    rrb = CC18_422f,
    weight = commonweight,
    healthcare = CC18_426_2,
    welfare = CC18_426_1
  ) |>
  mutate(welfare = case_when(
    welfare >= 1 & welfare <= 2 ~ 1,
    welfare == 3 ~ 2,
    welfare >= 4 & welfare <= 5 ~ 3,
    TRUE ~ NA
  )) |>
  select(case_id, pid7, ideo5, rra, rrb, healthcare, welfare, weight) |>
  mutate(year = 2018)

cces2020 <- get_dataframe_by_name(
  filename = "CES20_Common_OUTPUT_vv.csv",
  dataset = "10.7910/DVN/E9N6PH",
  server = "dataverse.harvard.edu",
  .f = read_csv
) |>
  rename(
    case_id = caseid,
    rra = CC20_441a,
    rrb = CC20_441b,
    weight = commonweight,
    healthcare = CC20_443_2,
    welfare = CC20_443_1
  ) |>
  mutate(
    welfare = case_when(
      welfare >= 1 & welfare <= 2 ~ 1,
      welfare == 3 ~ 2,
      welfare >= 4 & welfare <= 5 ~ 3,
      TRUE ~ NA
    )
  ) |>
  select(case_id, pid7, ideo5, rra, rrb, healthcare, welfare, weight) |>
  mutate(year = 2020)

cces2022 <- get_dataframe_by_name(
  filename = "CCES22_Common_OUTPUT_vv_topost.csv",
  dataset = "10.7910/DVN/PR4L8P",
  server = "dataverse.harvard.edu",
  .f = read_csv
) |>
  rename(
    case_id = caseid,
    rra = CC22_441a,
    rrb = CC22_441b,
    weight = commonweight,
    healthcare = CC22_443_2,
    welfare = CC22_443_1
  ) |>
  mutate(
    welfare = case_when(
      welfare >= 1 & welfare <= 2 ~ 1,
      welfare == 3 ~ 2,
      welfare >= 4 & welfare <= 5 ~ 3,
      TRUE ~ NA
    )
  ) |>
  select(case_id, pid7, ideo5, rra, rrb, healthcare, welfare, weight) |>
  mutate(year = 2022)

cces <- bind_rows(cces2014, cces2016, cces2018, cces2020, cces2022) |>
  mutate(
    rra = case_when(
      rra == 1 ~ 5,
      rra == 2 ~ 4,
      rra == 3 ~ 3,
      rra == 4 ~ 2,
      rra == 5 ~ 1,
      TRUE ~ NA
    ),
    scaledrra = rra / 10, # As per Tesler, converting to 0 to 1
    scaledrrb = rrb / 10, # As per Tesler, converting to 0 to 1
    rr = (scaledrra + scaledrrb),
    scaled_wf = welfare / 5, # As per Tesler, converting to 0 to 1
    scaled_hc = healthcare / 5, # As per Tesler, converting to 0 to 1
    pid = case_when(
      pid7 == 1 ~ 1,
      pid7 == 2 ~ 3,
      pid7 == 3 ~ 7,
      pid7 == 4 ~ 5,
      pid7 == 5 ~ 2,
      pid7 == 6 ~ 6,
      pid7 == 7 ~ 4,
      TRUE ~ NA
    ),
    pid = pid7 / 7, # As per Tesler, converting to 0 to 1
    ideo = ifelse(ideo5 %in% c(6), NA, ideo5),
    ideo = ideo / 5 # As per Tesler, converting to 0 to 1
  ) |>
  filter(
    !is.na(weight)
  )


# CES Healthcare ----------------------------------------------------------

years <- c(2014, 2018, 2020)

cescoefs <- tibble()

for (i in years) {
  model <- lm(scaled_hc ~ rr + pid + ideo,
              data = cces |> filter(year == i),
              weights = weight
  ) |>
    tidy() |>
    filter(term == "rr") |>
    mutate(year = i)
  
  cescoefs <- bind_rows(cescoefs, model)
}

ces_hc <- cescoefs |>
  rename(mean = estimate) |>
  mutate(
    ci.low = mean - 1.96 * std.error,
    ci.high = mean + 1.96 * std.error,
    source = "CES",
    year = factor(year)
  ) |>
  select(year, source, mean, ci.low, ci.high)



# Plot: HC Survey --------------------------------------------------------

hc_df <- ces_hc |>
  bind_rows(anes_hc_df) |>
  mutate(year = as.Date(year, format = "%Y"))

plot_hc_survey <- ggplot(hc_df, aes(x = year, y = mean, color = source, group = source)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.high), width = 0.1) +
  geom_vline(xintercept = as.Date("2007-01-01"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2017-01-01"), linetype = "dashed") +
  scale_color_grey() +
  annotate("text",
    x = as.Date("2013-01-01"), y = .1, label = "CES",
    hjust = -.1, color = "black"
  ) +
  annotate("text",
    x = as.Date("2018-01-01"), y = .09, label = "ANES",
    hjust = -.1, color = "grey"
  ) +
  scale_x_date(
    date_labels = "%Y",
    limits = as.Date(c("2004-01-01", "2022-12-31")),
    breaks = as.Date(c("2004-01-01", "2008-01-01", "2012-01-01", "2016-01-01", "2020-01-01"))
  ) +
  labs(
    x = "Year",
    y = "Coefficient Estimate",
    title = "Policy on RR (Healthcare & Obamacare)"
  ) +
  theme_bw() +
  theme(legend.position = "none")



# Table: Model Evaluation -------------------------------------------------

eval <- read.csv("data/eval_models.csv") |>
  mutate(
    value = gsub(".*statistic=([0-9.-]+).*", "\\1", value),
    value = as.numeric(value),
    Policy = ifelse(grepl("hc", name), "Healthcare",
      ifelse(grepl("immig", name), "Immigration", "Crime")
    ),
    category = str_extract(name, "(?<=_).*"),
    value = value * 100
  ) |>
  select(-name) |>
  pivot_wider(names_from = category, values_from = value) |>
  select(-analogy) |>
  xtable(digits = 1) |>
  print(
    type = "latex",
    include.rownames = FALSE
  )

latex_string <- eval |>
  paste()

# Define the pattern to match everything before "word1" and after "word2"
pattern <- paste0(".*(Healthcare.*?)\\\\\\\\ \\n   \\\\hline.*$")

# Extract the substring between "word1" and "word2"
desired_string <- gsub(pattern, "\\1", latex_string)
desired_string <- gsub(" \\n", "\\\\\\\\", desired_string)

read_lines("paper/tables/eval.tex") |>
  str_replace(
    "& 41.7 \\\\",
    paste0("& 41.7 \\\\", "\\\\ \\\\hline ", desired_string, "\\\\")
  ) |>
  write_lines("paper/tables/eval.tex")


