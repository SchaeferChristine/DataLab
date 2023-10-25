Analyse Wahlprogramm der Grünen in der Landtagswahl Bayern 2023
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(tokenizers)
library(tidyverse)
library(tidytext)
library(SnowballC)  # Stemming
library(lsa)  # Stopwörter
library(pdftools)
```

    ## Using poppler version 22.02.0

``` r
library(future)
library(furrr)
library(widyr)
```

# Daten laden

``` r
sentiws <- read_csv("https://osf.io/x89wq/?action=download")
```

    ## Rows: 3468 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): neg_pos, word, inflections
    ## dbl (1): value
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
d_wahl <- pdf_text("/Users/chrissi/Documents/Studium/Module/Data Science/Data Science Projects/Regierungsprogramm_Grüne_23.pdf") %>% 
  as_tibble()
```

Wörter zählen: Wie lang ist das Parteiprogramm?

``` r
str_count(d_wahl$value, pattern = "\\w") %>% sum()
```

    ## [1] 238243

``` r
stop1 <- tibble(word = quanteda::stopwords("german"))
```

``` r
d_wahl_token <- 
  d_wahl %>% 
  unnest_tokens(output = word, input = value) %>% 
  anti_join(stop1) %>% 
  count(word, sort = TRUE) 
```

    ## Joining with `by = join_by(word)`

Zahlen entfernen:

``` r
d_wahl_token %>% 
  dplyr::filter(str_detect(word, "[A-Za-z]")) -> gruene_long
```

# Stemming

``` r
gruene_stem <- 
  gruene_long %>% 
  mutate(stem = wordStem(word, language = "german")) %>% 
  count(stem, sort = TRUE)
```

``` r
gruene_stem
```

    ## # A tibble: 5,585 × 2
    ##    stem           n
    ##    <chr>      <int>
    ##  1 alt            9
    ##  2 lang           9
    ##  3 wirtschaft     9
    ##  4 gross          8
    ##  5 hoh            8
    ##  6 klein          8
    ##  7 sich           8
    ##  8 stark          8
    ##  9 beruf          7
    ## 10 eig            7
    ## # ℹ 5,575 more rows

# Sentiment-Analyse

``` r
senti2 <- sentiws %>% 
  unnest_tokens(output = inflections_tidy, input = inflections) 
```

Jetzt sind die inflections aufgeräumter und untereinander.
Praktischerweise wurde value immer übernommen. Jetzt hänge ich
inflections_tidy unten an words ran

``` r
senti3 <- data.frame(word = c(sentiws$word, senti2$inflections_tidy),
                     value = c(sentiws$value, senti2$value),
                     neg_pos = c(sentiws$neg_pos, senti2$neg_pos))
```

Jetzt haben wir alle Spalten ergänzt und somit mehr Wörter für eine
Sentiment-Analyse.

Problem: Manche Wörter doppelt. z.B. abbauen mit zwei unterschiedlichen
Werten -\> Dopplungen entfernen?

``` r
senti4 <- senti3 %>% 
  distinct(word, .keep_all = TRUE)
```

Alle doppelten Werte entfernen, hierbei die Werte aus dem ursprünglichen
df alle behalten \<3

``` r
gruene_senti <- 
  gruene_long %>% 
  inner_join(senti4, by = c("word" = "word"))  #nur die Werte behalten, die im Lexikon drin sind und danach zählen
```

``` r
gruene_senti %>% 
  slice(1:15) %>% 
  ggplot()+
  aes(y = reorder(word, n), x = n)+
  geom_col()+
  geom_col(fill = "purple")
```

![](Textanalyse-Grüne_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
gruene_senti_tab <- 
  gruene_senti %>% 
  group_by(neg_pos) %>% 
  summarise(polarity_mean = mean(value),
            polarity_sum = sum(value),
            polarity_count = n()) %>% 
  mutate(polarity_prop = (polarity_count / sum(polarity_count)) %>% round(2)) 
```

``` r
show(gruene_senti_tab)
```

    ## # A tibble: 2 × 5
    ##   neg_pos polarity_mean polarity_sum polarity_count polarity_prop
    ##   <chr>           <dbl>        <dbl>          <int>         <dbl>
    ## 1 neg           -0.237         -44.8            189          0.22
    ## 2 pos            0.0879         60.3            686          0.78

## Negativste Wörter

``` r
gruene_senti %>% 
  dplyr::filter(neg_pos == "neg") %>% 
  mutate(value_abs = abs(value)) %>% 
  top_n(20, value_abs) %>% 
  pull(word)
```

    ##  [1] "geringem"    "fehlen"      "schlecht"    "gefahren"    "ablehnen"   
    ##  [6] "brechen"     "falschen"    "fehlt"       "gefährliche" "gemein"     
    ## [11] "geringen"    "nachteils"   "schwache"    "schwächere"  "schädliche" 
    ## [16] "ungerecht"   "unnötig"     "unnötigen"   "unsicheren"  "verbieten"

## Positivste Wörter

``` r
gruene_senti %>% 
  dplyr::filter(neg_pos == "pos") %>% 
  mutate(value_abs = abs(value)) %>% 
  top_n(20, value_abs) %>% 
  pull(word)
```

    ##  [1] "besonders"    "wichtige"     "wichtiger"    "wichtig"      "genießen"    
    ##  [6] "kreative"     "wichtigen"    "begeistern"   "kreativen"    "wichtigsten" 
    ## [11] "angenehmes"   "bewährt"      "exzellenter"  "feiern"       "freundliche" 
    ## [16] "freundlichen" "gigantischer" "kreativ"      "loben"        "perfekte"    
    ## [21] "riesige"      "riesigen"     "tolle"        "wichtiges"

# Word embedding

``` r
slide_windows <- function(tbl, window_size) {
  skipgrams <- slider::slide(
    tbl, ~ .x,
    .after = window_size - 1,
    .step = 1,
    .complete = FALSE
  )
  
  safe_mutate <- safely(mutate)
  
  out <- map2(skipgrams, 1:length(skipgrams), ~ safe_mutate(.x, window_id = .y))
  
  out %>% 
    transpose() %>% 
    pluck("result") %>% 
    compact() %>% 
    bind_rows()
  
}
```

## Ähnlichkeit berechnen

``` r
plan(multisession)
```

``` r
nested_words <- gruene_long %>% 
  nest(words = c(word)) %>% 
  mutate(id = 1:length(words))
```

``` r
slide_windows(nested_words, 4) -> skips
```

``` r
tidy_pmi <- 
  skips %>% 
  unnest(words) %>% 
  pairwise_pmi(word, window_id)
```

``` r
head(tidy_pmi)
```

    ## # A tibble: 6 × 3
    ##   item1    item2    pmi
    ##   <chr>    <chr>  <dbl>
    ## 1 innen    bayern 1.11 
    ## 2 menschen bayern 0.708
    ## 3 mehr     bayern 0.420
    ## 4 bayern   innen  1.11 
    ## 5 menschen innen  0.708
    ## 6 mehr     innen  0.420

## Nearest neighbours

Aus smltar:

``` r
tidy_word_vectors <- tidy_pmi %>%
  widely_svd(
    item1, item2, pmi,
    nv = 100, maxit = 1000
  )

tidy_word_vectors
```

    ## # A tibble: 732,800 × 3
    ##    item1        dimension     value
    ##    <chr>            <int>     <dbl>
    ##  1 innen                1  2.13e-19
    ##  2 menschen             1  4.49e-19
    ##  3 mehr                 1 -7.40e-19
    ##  4 bayern               1  1.13e-18
    ##  5 dass                 1 -3.97e-19
    ##  6 unsere               1  3.27e-19
    ##  7 unterstützen         1  8.20e-19
    ##  8 dafür                1  1.83e-18
    ##  9 schaffen             1  8.79e-19
    ## 10 stärken              1 -2.34e-18
    ## # ℹ 732,790 more rows

``` r
nearest_neighbours <- function(df, token) {
  df %>%
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) / 
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
        
        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE
    )(item1, dimension, value) %>%
    select(-item2)
}
```

``` r
tidy_word_vectors %>% 
  nearest_neighbours("bayern")
```

    ## # A tibble: 7,328 × 2
    ##    item1        value
    ##    <chr>        <dbl>
    ##  1 bayern      1     
    ##  2 kinder      0.118 
    ##  3 mehr        0.115 
    ##  4 menschen    0.0875
    ##  5 leben       0.0750
    ##  6 unserer     0.0593
    ##  7 bayerischen 0.0425
    ##  8 schaffen    0.0394
    ##  9 unternehmen 0.0369
    ## 10 setzen      0.0365
    ## # ℹ 7,318 more rows

``` r
tidy_word_vectors %>% 
  nearest_neighbours("verbieten")
```

    ## # A tibble: 7,328 × 2
    ##    item1                 value
    ##    <chr>                 <dbl>
    ##  1 verbieten             1    
    ##  2 eingesetzte           0.539
    ##  3 beteili               0.536
    ##  4 studierendenwerke     0.527
    ##  5 letztes               0.525
    ##  6 harmonisierung        0.518
    ##  7 ersparnis             0.473
    ##  8 tierschutzrechtlichen 0.467
    ##  9 partizipation         0.466
    ## 10 fern                  0.463
    ## # ℹ 7,318 more rows
