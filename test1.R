library(dplyr)          # for data manipulation
library(tidyr)          # for data tidying
library(ggplot2)        # for generating the visualizations

head(midwest)
## # A tibble: 6 x 28
##     PID    county state  area poptotal popdensity popwhite popblack
##   <int>     <chr> <chr> <dbl>    <int>      <dbl>    <int>    <int>
## 1   561     ADAMS    IL 0.052    66090  1270.9615    63917     1702
## 2   562 ALEXANDER    IL 0.014    10626   759.0000     7054     3496
## 3   563      BOND    IL 0.022    14991   681.4091    14477      429
## 4   564     BOONE    IL 0.017    30806  1812.1176    29344      127
## 5   565     BROWN    IL 0.018     5836   324.2222     5264      547
## 6   566    BUREAU    IL 0.050    35688   713.7600    35157       50
## # ... with 20 more variables: popamerindian <int>, popasian <int>,
## #   popother <int>, percwhite <dbl>, percblack <dbl>, percamerindan <dbl>,
## #   percasian <dbl>, percother <dbl>, popadults <int>, perchsd <dbl>,
## #   percollege <dbl>, percprof <dbl>, poppovertyknown <int>,
## #   percpovertyknown <dbl>, percbelowpoverty <dbl>,
## #   percchildbelowpovert <dbl>, percadultpoverty <dbl>,
## #   percelderlypoverty <dbl>, inmetro <int>, category <chr>

#ohio <- midwest %>%
  #filter(state == "OH") %>%
  #select(county, percollege) %>%
  #arrange(desc(percollege)) %>%
  #top_n(25) %>%
  #arrange(percollege) %>%
  #mutate(county = factor(county, levels = .$county))

ohio <- midwest %>%
  filter(state == "OH") %>%
  select(county, percollege) %>%
  arrange(percollege) %>%
  mutate(Avg = mean(percollege, na.rm = TRUE),
         Above = ifelse(percollege - Avg > 0, TRUE, FALSE),
         county = factor(county, levels = .$county))

head(ohio)

ggplot(ohio, aes(percollege, county, color = Above)) +
  geom_segment(aes(x = Avg, y = county, xend = percollege, yend = county), color = "grey50") +
  geom_point()
# bar chart
#ggplot(midwest, aes(county, percollege)) +
 # geom_bar(stat = "identity") +
  #coord_flip()

# dot plot
#ggplot(midwest, aes(percollege, county)) +
  #geom_point()

# lollipop chart
#ggplot(midwest, aes(percollege, county)) +
 # geom_segment(aes(x = 0, y = county, xend = percollege, yend = county), color = "grey50") +
  #geom_point()
