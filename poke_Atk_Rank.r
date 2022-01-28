library(tidyverse)
# this didn't work
# type_comparisons <- read_csv("https://github.com/robinsones/pokemon-chart/blob/master/chart.csv")
library(datapasta)
# use tribble_paste()
type_comparisons <- tibble::tribble(
     ~Attacking, ~Normal, ~Fire, ~Water, ~Electric, ~Grass, ~Ice, ~Fighting, ~Poison, ~Ground, ~Flying, ~Psychic, ~Bug, ~Rock, ~Ghost, ~Dragon, ~Dark, ~Steel, ~Fairy,
       "Normal",       1,     1,      1,         1,      1,    1,         1,       1,       1,       1,        1,    1,   0.5,      0,       1,     1,    0.5,      1, 
         "Fire",       1,   0.5,    0.5,         1,      2,    2,         1,       1,       1,       1,        1,    2,   0.5,      1,     0.5,     1,      2,      1,
        "Water",       1,     2,    0.5,         1,    0.5,    1,         1,       1,       2,       1,        1,    1,     2,      1,     0.5,     1,      1,      1, 
     "Electric",       1,     1,      2,       0.5,    0.5,    1,         1,       1,       0,       2,        1,    1,     1,      1,     0.5,     1,      1,      1,
        "Grass",       1,   0.5,      2,         1,    0.5,    1,         1,     0.5,       2,     0.5,        1,  0.5,     2,      1,     0.5,     1,    0.5,      1, 
          "Ice",       1,   0.5,    0.5,         1,      2,  0.5,         1,       1,       2,       2,        1,    1,     1,      1,       2,     1,    0.5,      1, 
     "Fighting",       2,     1,      1,         1,      1,    2,         1,     0.5,       1,     0.5,      0.5,  0.5,     2,      0,       1,     2,      2,    0.5, 
       "Poison",       1,     1,      1,         1,      2,    1,         1,     0.5,     0.5,       1,        1,    1,   0.5,    0.5,       1,     1,      0,      2, 
       "Ground",       1,     2,      1,         2,    0.5,    1,         1,       2,       1,       0,        1,  0.5,     2,      1,       1,     1,      2,      1, 
       "Flying",       1,     1,      1,       0.5,      2,    1,         2,       1,       1,       1,        1,    2,   0.5,      1,       1,     1,    0.5,      1, 
      "Psychic",       1,     1,      1,         1,      1,    1,         2,       2,       1,       1,      0.5,    1,     1,      1,       1,     0,    0.5,      1,
          "Bug",       1,   0.5,      1,         1,      2,    1,       0.5,     0.5,       1,     0.5,        2,    1,     1,    0.5,       1,     2,    0.5,    0.5, 
         "Rock",       1,     2,      1,         1,      1,    2,       0.5,       1,     0.5,       2,        1,    2,     1,      1,       1,     1,    0.5,      1, 
        "Ghost",       0,     1,      1,         1,      1,    1,         1,       1,       1,       1,        2,    1,     1,      2,       1,   0.5,      1,      1,
       "Dragon",       1,     1,      1,         1,      1,    1,         1,       1,       1,       1,        1,    1,     1,      1,       2,     1,    0.5,      0, 
         "Dark",       1,     1,      1,         1,      1,    1,       0.5,       1,       1,       1,        2,    1,     1,      2,       1,   0.5,      1,    0.5, 
        "Steel",       1,   0.5,    0.5,       0.5,      1,    2,         1,       1,       1,       1,        1,    1,     2,      1,       1,     1,    0.5,      2,
        "Fairy",       1,   0.5,      1,         1,      1,    1,         2,     0.5,       1,       1,        1,    1,     1,      1,       2,     2,    0.5,      1,
)
tidied_comparison <- type_comparisons %>%
  gather(Defending, outcome, -Attacking)

Super_effective <-tidied_comparison %>%
  group_by(Attacking) %>%
  summarize(Super_effective = sum(ifelse(outcome == 2, 1, 0))) %>%
  arrange(desc(Super_effective)) %>%
  knitr::kable()

Neutral<- tidied_comparison %>%
  group_by(Attacking) %>%
  summarize(Neutral = sum(ifelse(outcome == 1, 1, 0))) %>%
  arrange(desc(Neutral)) %>%
  knitr::kable()

Super_Effective_and_Neutral <-tidied_comparison %>%
  group_by(Attacking) %>%
  summarize(Super_Effective_and_Neutral = sum(ifelse(outcome >= 1, 1, 0))) %>%
  arrange(desc(Super_Effective_and_Neutral)) %>%
  knitr::kable()

Not_Very_Effective <-tidied_comparison %>%
  group_by(Attacking) %>%
  summarize(Not_Very_Effective = sum(ifelse(outcome == 0.5, 1, 0))) %>%
  arrange(desc(Not_Very_Effective)) %>%
  knitr::kable()

No_Effect  <-tidied_comparison %>%
  group_by(Attacking) %>%
  summarize(No_Effect  = sum(ifelse(outcome == 0, 1, 0))) %>%
  arrange(desc(No_Effect )) %>%
  knitr::kable()

Not_Very_Effective_and_No_Effect  <-tidied_comparison %>%
  group_by(Attacking) %>%
  summarize(Not_Very_Effective_and_No_Effect  = sum(ifelse(outcome <= 0.5, 1, 0))) %>%
  arrange(desc(Not_Very_Effective_and_No_Effect )) %>%
  knitr::kable()

type <- strongest_teams %>%
  as_tibble() %>%
  gather(team, type) %>%
  count(type, sort = TRUE) %>%
  knitr::kable()

m <- as.matrix(type_comparisons[, -1])
rownames(m) <- type_comparisons$Attacking

super_effective_m <- (m == 2) * 1L

super_effective_nb <- function(indices) {
  sum(colSums(super_effective_m[indices, ]) > 0)
}
all_combinations <- combn(18, 4)

super_effective_results <- apply(all_combinations, 2, super_effective_nb)

dim(all_combinations)

head(sort(super_effective_results,decreasing = TRUE), n=263)

head(order(super_effective_results,decreasing = TRUE), n=51)

best_combos <- all_combinations[, head(order(super_effective_results,decreasing = TRUE), n=51)]
Super_effective
Neutral
Super_Effective_and_Neutral
Not_Very_Effective
Not_Very_Effective_and_No_Effect 

strongest_teams <- t(matrix(rownames(super_effective_m)[best_combos], nrow = 4))

strongest_teams

type

