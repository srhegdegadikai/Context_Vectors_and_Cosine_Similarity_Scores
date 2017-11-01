Context Vectors and Cosine Similarity Scores
================
Suhas Hegde
10/31/2017

The following program reads in a text corpus and determines the context vectors (window size 1) for the selected words. Then cosine similarity scores are calculated for word-pairs.

#### Context vector -

These vectors contain the words that are behind or ahead of the word in question. The window size determines how many words are taken into account while building the vector. Here a window size of one has been used.

#### Cosine Similarity Score -

This score determines the similarity of the context vectors in question. Higher scores indicate that the context vectors are similar in nature(ie, they share a lot of words together). Thus we can say that the "original words" are used in similar contexts.

#### Reading the raw-text and basic data tidying -

``` r
#load up the required libraries
library(tidyverse)
library(tidytext)
library(stringr)


# read the text corpus
raw_text <- read_delim("sents_BNCbaby.txt", col_names = FALSE, delim = "\n")

# filter out the sentence numbers and create another column
raw_text$X1 %>% str_extract("[0-9]{1,}(?=\t)") -> raw_text$sentence_num

# rename the columns
names(raw_text) <- c("text","sentence_num")

# convert sentence number to numeric type column
as.integer(raw_text$sentence_num) -> raw_text$sentence_num
```

#### Convert the raw text into monograms and filter out required words -

``` r
# convert the raw_text into a table with each row containing only one word and then filter out the required words from the raw_text
raw_text %>% unnest_tokens(word,text) %>%
  filter(word ==  "priceless"  |
           word ==  "valuable" | 
           word ==  "absent" |
           word ==  "specialists" | 
           word ==  "specialist" | 
           word ==  "experts" |
           word ==  "expert" | 
           word == "courage" ) -> df

df
```

    ## # A tibble: 622 x 2
    ##    sentence_num       word
    ##           <int>      <chr>
    ##  1           39    experts
    ##  2           64   valuable
    ##  3          381    experts
    ##  4          440    experts
    ##  5          633 specialist
    ##  6          644    experts
    ##  7         1377   valuable
    ##  8         1467    courage
    ##  9         1633     expert
    ## 10         1778     expert
    ## # ... with 612 more rows

#### Filtering Context Vectors -

``` r
# left_join the raw_text into the newly fromed table "df" with the the required words, this filters out all the unnecessary text and only preserves the ones where we have the required words such as "priceless,courage..." 
# then add a grouping variable and create another variable called line_no
# that means we will have a bunch of line_no for each individual senetence number
df %>% left_join(raw_text)  %>% 
  unnest_tokens(context_vector,text) %>% 
  group_by(sentence_num) %>%
  mutate(line_no = row_number()) ->df

df
```

    ## # A tibble: 17,552 x 4
    ## # Groups:   sentence_num [609]
    ##    sentence_num    word context_vector line_no
    ##           <int>   <chr>          <chr>   <int>
    ##  1           39 experts             39       1
    ##  2           39 experts            but       2
    ##  3           39 experts           even       3
    ##  4           39 experts             at       4
    ##  5           39 experts           this       5
    ##  6           39 experts          stage       6
    ##  7           39 experts          there       7
    ##  8           39 experts            are       8
    ##  9           39 experts           many       9
    ## 10           39 experts        hurdles      10
    ## # ... with 17,542 more rows

``` r
# first, filter out rows where context_vector == word column
# calculate the index(row numbers) for the "word behind" and the "word ahead" that make up the context vector of the given word
df %>%  filter(context_vector ==  word) %>% select(sentence_num,line_no) %>%
  mutate(word_behind = line_no-1, word_ahead = line_no+1) %>%
  select(-line_no) %>%
  gather(word_type, line_no, word_behind:word_ahead) %>%
  left_join(df)  -> df

df
```

    ## # A tibble: 1,276 x 5
    ## # Groups:   sentence_num [?]
    ##    sentence_num   word_type line_no       word  context_vector
    ##           <int>       <chr>   <dbl>      <chr>           <chr>
    ##  1           39 word_behind      17    experts            city
    ##  2           64 word_behind      17   valuable            them
    ##  3          381 word_behind       3    experts            some
    ##  4          440 word_behind      15    experts           which
    ##  5          633 word_behind      12 specialist               a
    ##  6          644 word_behind       4    experts     independent
    ##  7         1377 word_behind      14   valuable psychologically
    ##  8         1467 word_behind      10    courage     originality
    ##  9         1633 word_behind      12     expert            have
    ## 10         1778 word_behind      38     expert              of
    ## # ... with 1,266 more rows

``` r
# replace the word names with appropriate names
# eg - "experts OR expert" should be changed to one group called expert_experts
df$word %>% 
  str_replace_all(c("specialists|specialist" = "specialist_specialists",
                              "experts|expert" = "expert_experts")) -> df$word

# filtering out the context vector for each word
# first group all the records based on the word column, we should have 6 distinct groups
# next drop all the "NA" values (they were introduced in case if we were trying to select rows that do not exist)
# finally, select only distinct values for each group
# that means each word will have a context vector without any repetetions
df %>% ungroup() %>%
  group_by(word) %>%
  select(context_vector) %>%
  drop_na() %>% distinct() -> df

df
```

    ## # A tibble: 648 x 2
    ## # Groups:   word [6]
    ##                      word  context_vector
    ##                     <chr>           <chr>
    ##  1         expert_experts            city
    ##  2               valuable            them
    ##  3         expert_experts            some
    ##  4         expert_experts           which
    ##  5 specialist_specialists               a
    ##  6         expert_experts     independent
    ##  7               valuable psychologically
    ##  8                courage     originality
    ##  9         expert_experts            have
    ## 10         expert_experts              of
    ## # ... with 638 more rows

``` r
# calculate the length of each context vector and save it to a table
df %>% group_by(word) %>%
  summarise(vector_length = n()) -> context_vec

context_vec
```

    ## # A tibble: 6 x 2
    ##                     word vector_length
    ##                    <chr>         <int>
    ## 1                 absent            68
    ## 2                courage            42
    ## 3         expert_experts           180
    ## 4              priceless            19
    ## 5 specialist_specialists           221
    ## 6               valuable           118

#### Cosine Similarity Function -

We would have to define a custom function that takes in user inputs such as word pairs and calculates the cosine similarity scores for them.

``` r
# cosine similarity function
# takes in 4 inputs

# df - table(dataframe) containing context vectors for each word or word-group
# word1 - the first string
# word2 - the second string
# context_vec - table(dataframe) containing the lengths of context vectors for each word or word-group

# The function basically works likes this, based on the given word it filters out the context vector for that word, repeats the same process for the next word
# then using inner_join intersection V1.V2 is found and saved to  "word_intersection"
# x and y determine the length of context vectors for word1 and word2
# finally, the score is calculated and is returned

cos_sim_score <- function(df, word1,word2, context_vec ){
  
  df %>% filter(word == word1) %>%
    ungroup() %>%
    select(context_vector) %>% distinct() -> a
  
  df %>% filter(word == word2) %>%
    ungroup() %>%
    select(context_vector) %>% distinct() -> b
  
  a %>% inner_join(b) %>% summarise(vector_length = n()) %>%
    pull(vector_length) -> word_intersection
  
  context_vec %>% filter(word == word1) %>% ungroup() %>%
    pull(vector_length) -> x
  
  context_vec %>% filter(word == word2) %>% ungroup() %>%
    pull(vector_length) -> y
  
  cos_score <- word_intersection/(sqrt(x)*sqrt(y)) 
    
    return(cos_score)
}
```

#### Applying the "cos\_sim\_score" function and calculating the scores-

``` r
# build two string vectors containing all the words

first_word <- c("priceless","priceless","specialist_specialists","specialist_specialists")
second_word <- c("valuable","absent","expert_experts","courage")


# "map" the cos_sim_score function to the word-pairs and display the results
# this is similar to using two "for-loops", we loop through the words and apply the function("cos_sim_score") to each set of words

map2(first_word, second_word ,cos_sim_score, df=df,  context_vec=context_vec) %>%
  set_names(c("S1_1","S1_2","S2_1","S2_2")) %>% 
  unlist() %>% 
  data_frame(word_pair = attr(.,"names") ,cosine_similarity_score= .)
```

    ## # A tibble: 4 x 2
    ##   word_pair cosine_similarity_score
    ##       <chr>                   <dbl>
    ## 1      S1_1               0.1267166
    ## 2      S1_2               0.1391037
    ## 3      S2_1               0.2155937
    ## 4      S2_2               0.1453139

#### Result -

The results are bit contrary in nature. Cosine similarity score for S1\_1 is less than S1\_2. That is saying the word "priceless" is semantically closer to the word "absent", which is not exactly true. In the other case, there is significant difference between the scores(S2\_1 &gt; S2\_2). So, we can safely assume that the word group "specialist/specialists" is much more semantically closer to "expert/experts" than to "courage".
