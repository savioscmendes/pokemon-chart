library(tidyverse)
# this didn't work
# type_comparisons <- read_csv("https://github.com/robinsones/pokemon-chart/blob/master/chart.csv")
library(datapasta)
# use tribble_paste()
type_comparisons <- tibble::tribble(
     ~Attacking, ~Normal, ~Fire, ~Water, ~Electric, ~Grass, ~Ice, ~Fighting, ~Poison, ~Ground, ~Flying, ~Psychic, ~Bug, ~Rock, ~Ghost, ~Dragon, ~Dark, ~Steel, ~None,
       "Normal",       1,     1,      1,         1,      1,    1,         1,       1,       1,       1,        1,    1,   0.5,      0,       1,     1,    0.5,      1,
         "Fire",       1,   0.5,    0.5,         1,      2,    2,         1,       1,       1,       1,        1,    2,   0.5,      1,     0.5,     1,      2,      1,
        "Water",       1,     2,    0.5,         1,    0.5,    1,         1,       1,       2,       1,        1,    1,     2,      1,     0.5,     1,      1,      1,
     "Electric",       1,     1,      2,       0.5,    0.5,    1,         1,       1,       0,       2,        1,    1,     1,      1,     0.5,     1,      1,      1,
        "Grass",       1,   0.5,      2,         1,    0.5,    1,         1,     0.5,       2,     0.5,        1,  0.5,     2,      1,     0.5,     1,    0.5,      1,
          "Ice",       1,   0.5,    0.5,         1,      2,  0.5,         1,       1,       2,       2,        1,    1,     1,      1,       2,     1,    0.5,      1,
     "Fighting",       2,     1,      1,         1,      1,    2,         1,     0.5,       1,     0.5,      0.5,  0.5,     2,      0,       1,     2,      2,      1,
       "Poison",       1,     1,      1,         1,      2,    1,         1,     0.5,     0.5,       1,        1,    1,   0.5,    0.5,       1,     1,      0,      1,
       "Ground",       1,     2,      1,         2,    0.5,    1,         1,       2,       1,       0,        1,  0.5,     2,      1,       1,     1,      2,      1,
       "Flying",       1,     1,      1,       0.5,      2,    1,         2,       1,       1,       1,        1,    2,   0.5,      1,       1,     1,    0.5,      1,
      "Psychic",       1,     1,      1,         1,      1,    1,         2,       2,       1,       1,      0.5,    1,     1,      1,       1,     0,    0.5,      1,
          "Bug",       1,   0.5,      1,         1,      2,    1,       0.5,     0.5,       1,     0.5,        2,    1,     1,    0.5,       1,     2,    0.5,      1,
         "Rock",       1,     2,      1,         1,      1,    2,       0.5,       1,     0.5,       2,        1,    2,     1,      1,       1,     1,    0.5,      1,
        "Ghost",       0,     1,      1,         1,      1,    1,         1,       1,       1,       1,        2,    1,     1,      2,       1,   0.5,    0.5,      1,
       "Dragon",       1,     1,      1,         1,      1,    1,         1,       1,       1,       1,        1,    1,     1,      1,       2,     1,    0.5,      1,
         "Dark",       1,     1,      1,         1,      1,    1,       0.5,       1,       1,       1,        2,    1,     1,      2,       1,   0.5,    0.5,      1,
        "Steel",       1,   0.5,    0.5,       0.5,      1,    2,         1,       1,       1,       1,        1,    1,     2,      1,       1,     1,    0.5,      1,
      	 "None",       1,     1,      1,         1,      1,    1,         1,       1,	      1,       1,	       1,    1,     1,      1,       1,     1,      1,      1 
)
 

tidied_comparison <- type_comparisons %>%
  gather(Defending, outcome, -Attacking)

Weaknesses <- tidied_comparison %>%
  group_by(Defending) %>%
  summarize(Weaknesses = sum(ifelse(outcome > 1, 1, 0))) %>%
  arrange(desc(Weaknesses)) %>%
  knitr::kable()

Resistances <-tidied_comparison %>%
  group_by(Defending) %>%
  summarize(Resistances = sum(ifelse(outcome < 1, 1, 0))) %>%
  arrange(desc(Resistances)) %>%
  knitr::kable()

Immunities <- tidied_comparison %>%
  group_by(Defending) %>%
  summarize(Immunities = sum(ifelse(outcome == 0, 1, 0))) %>%
  arrange(desc(Immunities)) %>%
  head(6) %>%
  knitr::kable()

m <- as.matrix(type_comparisons[, -1])
rownames(m) <- type_comparisons$Attacking

m[m == 2] <- -1
m[m >= 0 & m < 1 ] <- 3
m[m == 1 ] <- 0
m[m == 3 ] <- 1

super_effective_m <- t(m)

super_effective_nb <- function(indices) {
  sum(colSums(super_effective_m[indices, ]) < 0)
}

all_combinations <- combn(18, 2)

super_effective_results <- apply(all_combinations, 2, super_effective_nb)

dim(all_combinations)

sort(super_effective_results,decreasing = TRUE)

head(sort(super_effective_results,decreasing = TRUE), n=39)
head(sort(super_effective_results,decreasing = FALSE), n=37)

best_combos_wek <- all_combinations[, head(order(super_effective_results,decreasing = TRUE), n=39)]

best_combos <- all_combinations[, head(order(super_effective_results,decreasing = FALSE), n=37)]

Weaknesses
Resistances
Immunities

strongest_teams_wek <-t(matrix(rownames(super_effective_m)[best_combos_wek], nrow = 2))

strongest_teams_wek

wek<-strongest_teams_wek %>%
  as_tibble() %>%
  gather(team, type) %>%
  count(type, sort = TRUE) %>%
  knitr::kable()

wek

strongest_teams <-t(matrix(rownames(super_effective_m)[best_combos], nrow = 2))

strongest_teams

type <- strongest_teams %>%
  as_tibble() %>%
  gather(team, type) %>%
  count(type, sort = TRUE) %>%
  knitr::kable()
  
type