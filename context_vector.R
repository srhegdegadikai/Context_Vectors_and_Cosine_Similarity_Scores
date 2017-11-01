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

# convert the raw_text into a table with each row containing only one word
# then filter out the required words from the raw_text
raw_text %>% unnest_tokens(word,text) %>%
  filter(word ==  "priceless"  |
           word ==  "valuable" | 
           word ==  "absent" |
           word ==  "specialists" | 
           word ==  "specialist" | 
           word ==  "experts" |
           word ==  "expert" | 
           word == "courage" ) -> df

# left_join the raw_text into the newly fromed table "df" with the the required words
# this filters out all the unnecessary text and only preserves the ones where 
# we have the required words such as "priceless,courage..." 
# then add a grouping variable and create another variable called line_no
# that means we will have a bunch of line_no for each individual senetence number
df %>% left_join(raw_text)  %>% 
  unnest_tokens(context_vector,text) %>% 
  group_by(sentence_num) %>%
  mutate(line_no = row_number()) ->df

# first, filter out rows where context_vector == word column
# calculate the index(row numbers) for the "word behind" and the "word ahead" that make up the context vector of the given word
df %>%  filter(context_vector ==  word) %>% select(sentence_num,line_no) %>%
  mutate(word_behind = line_no-1, word_ahead = line_no+1) %>%
  select(-line_no) %>%
  gather(word_type, line_no, word_behind:word_ahead) %>%
  left_join(df)  -> df

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

# calculate the length of each context vector and save it to a table
df %>% group_by(word) %>%
  summarise(vector_length = n()) -> context_vec


# calculate cosine similarity scores for the required set of words
first_word <- c("priceless","priceless","specialist_specialists","specialist_specialists")
second_word <- c("valuable","absent","expert_experts","courage")


# "map" the cos_sim_score function to the word-pairs 
map2(first_word, second_word ,cos_sim_score, df=df,  context_vec=context_vec) %>%
  set_names(c("S1_1","S1_2","S2_1","S2_2")) %>% 
  unlist() %>% 
  data_frame(word_pair = attr(.,"names") ,cosine_similarity_score= .)


# cosine similarity function
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
