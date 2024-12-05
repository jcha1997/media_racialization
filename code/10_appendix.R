library(tidyverse)
library(xtable)

library(stringi)


bold <- function(x) {
  paste("{\\textbf{", x, "}}", sep = "")
}

# A. Selective Stemming ---------------------------------------------------

# Load the data
dictionary <- tibble(
  Topic = c("Crime", "Healthcare", "Immigration",
            "Race", "Black", "Latinx", "Asian"),
  Placeholder = c("krime", "healthcare", "immigration",
                  "racialization", "blck", "latinx", "azn"),
  Words = c(
    "crime(s), criminal(s), criminalize(s), criminalizing, criminalization, criminalized, criminalizes, criminality, criminality, criminalize",
    "healthcare, health-care, health insurance",
    "immigrant(s), migrant(s), migration", 
    "racism, racial, racist(s), racialized, racialize(s), racializing",
    "blacks, african american(s), black men, black man, black woman, black women, black people",
    "hispanic(s), hispanic american(s), latino(s), latina(s), latin american(s), central american(s), mexican(s), mexican american(s), cuban(s), cuban american(s), salvordoran(s)",
    "asian(s), asian american(s), pan asian, chinese, chinese american(s), phillipines, filipino(s), filipino american(s), vietnamese, vietnamese american(s), korean(s), korean american(s), japanese, japanese american(s), cambodian(s), cambodian american(s), khmer, laos, laotian(s), laotian american(s), thai, thai american(s)"
  )
)

dictionaryxtab <- xtable(dictionary,
  caption = "Dictionary of Words",
  label = "atab:dictionary",
  table.placement = "H",
  latex.environments = "center",
  rownames = F,
  type = "latex",
  booktabs = T,
  align = c("|l|", "|p{2cm}", "|p{2.5cm}|", "p{.6\\textwidth}|")
)

print(dictionaryxtab,
  caption.placement = "top",
  include.rownames = F,
  hline.after = c(-1, 0, 1, 2, 3, 4, 5, 6, 7),
  sanitize.colnames.function = bold,
  file = "paper/tables/dictionary.tex"
)


# A. Previous Definitions -------------------------------------------------

# Load the data
previous_definitions <- tibble(
  Work = c("Tesler (2012)", "Gilens (1995)", "Gilens (1996)", "Winter (2006)", "Federico (2004)", "Tesler (2013)"),
  Measure = c("Racial Resentment", "Racial Resentment + Index", "Racial Resentment", "Racial Resentment + Index", "Racial Resentment", "Old Fashioned Racism"),
  Definitions = c(
    "how racial attitudes come to influence white opinion about governmental policies (p. 692)",
    "particular beliefs about blacks lead to particular policy preferences on the part of white Americans (p. 1010)",
    "how important... racial considerations [are] in the public's evaluation of race coded issues (p. 593)",
    "the process that associates political issues - jointly in political discourse and in citizens' minds - with considerations of race (p. 401)",
    "certain political issues have become implicitly linked to beliefs about stigmatized racial groups (p. 374)",
    "this phenomenon in which the large effects of racial attitudes on the public's assessments of Barack Obama are transferred to their related political evaluations (p. 111)"
  ),
) |>
  mutate(
    year = str_extract_all(Work, "\\d{4}"),
    year = as.numeric(year)
  ) |>
  arrange(year, Work) |>
  select(-year)

previous_definitionsxtab <- xtable(previous_definitions,
  caption = "Previous Definitions",
  label = "a_tab_definitions",
  table.placement = "H",
  latex.environments = "center",
  rownames = F,
  type = "latex",
  booktabs = T,
  align = c("|l", "|l|", "p{4cm}|", "p{.5\\textwidth}|")
)

print(previous_definitionsxtab,
  caption.placement = "top",
  include.rownames = F,
  sanitize.colnames.function = bold,
  file = "paper/tables/a_definitions.tex"
)


# A. Keyword Search -------------------------------------------------------


# Load the data
keywords <- tibble(
  Policy = c("Crime", "Healthcare", "Immigration"),
  Keywords = c(
    "crime, felon, felony, criminal, criminal justice",
    "healthcare, health-care, health care, health insurance",
    "immigration, immigrant, migrant, migration"
  ),
  Location = c(
    "United States, New York, Washington DC, Los Angeles",
    "United States", 
    "United States"
  ),
  N = c(
    "66,821",
    "48,310", 
    "98,729"
  )
)

keywordsxtab <- xtable(keywords,
                         caption = "Keyword Searches",
                         label = "atab:keywords",
                         table.placement = "H",
                         latex.environments = "center",
                         rownames = F,
                         type = "latex",
                         booktabs = T,
                         align = c("|l|", "|p{2cm}", "|p{2.5cm}|", "p{.2\\textwidth}|", "p{.2\\textwidth}|")
)

print(keywordsxtab,
      caption.placement = "top",
      include.rownames = F,
      hline.after = c(-1, 0, 1, 2, 3),
      sanitize.colnames.function = bold,
      file = "paper/tables/keywords.tex"
)


# A. Nearest Neighbors ----------------------------------------------------

crime_nn <- read.csv('data/crime_word_neighbors.csv') |> 
  mutate(
    Policy = "Crime"
  )

hc_nn <- read.csv('data/hc_word_neighbors.csv') |> 
  mutate(
    Policy = "Healthcare")

immig_nn <- read.csv('data/immig_word_neighbors.csv') |> 
  mutate(
    Policy = "Immigration")

crime_race_nn <- read.csv('data/crime_neighbors_race.csv') |> 
  mutate(
    Policy = "Crime"
  )

hc_race_nn <- read.csv('data/hc_neighbors_race.csv') |> 
  mutate(
    Policy = "Healthcare")

immig_race_nn <- read.csv('data/immig_neighbors_race.csv') |> 
  mutate(
    Policy = "Immigration")

nn <- crime_nn |> 
  bind_rows(hc_nn) |> 
  bind_rows(immig_nn) |> 
  bind_rows(crime_race_nn) |> 
  bind_rows(hc_race_nn) |> 
  bind_rows(immig_race_nn) |> 
  rename(Word = word,
         `Nearest Neighbors` = neighbors) |> 
  relocate(Policy, Word, `Nearest Neighbors`) |> 
  arrange(Word) |> 
  mutate(`Nearest Neighbors` = stri_trans_general(str = `Nearest Neighbors`, 
                            id = "Latin-ASCII"))
                     
nnxtab <- xtable(nn,
                       caption = "Nearest Neighbors for Targets",
                       label = "atab:nn",
                       table.placement = "H",
                       latex.environments = "center",
                       rownames = F,
                       type = "latex",
                       booktabs = T,
                       align = c("|l|", "|p{2cm}|",  "p{.15\\textwidth}|", "p{.7\\textwidth}|")
)

print(nnxtab,
      caption.placement = "top",
      include.rownames = F,
      hline.after = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
      sanitize.colnames.function = bold,
      file = "paper/tables/nn.tex"
)

