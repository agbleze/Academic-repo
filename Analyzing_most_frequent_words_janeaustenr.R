##tidying the work of Jane Austen
# load library
library(janeaustenr) ## contains dataset of Jane austren in data frame format
library(dplyr) ## data manipulation
library(stringr)
library(tidyr) # data cleaning
library(tidytext) ## for tokenizing 
library(ggplot2)  # plotting graphs

# load dataset and store in variable
original_books <- austen_books()
#View(original_books)
#### group the book, add row numbers as line number and chapter number
#piping the dataset
original_books%>%
  #group the dataset based on the column name books
  group_by(book)%>%
  # add 2 columns to the data for line number and chapter number
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup()
original_books
View(original_books)

### tokenize the data
# assign the preprocessed data to a variable and pipe
tidy_books <- original_books %>%
  # tokenize the data based on text column and ouptput tokens in word column 
  unnest_tokens(word, text)
tidy_books  
View(tidy_books) 

## removing stop_words
# load the dataset of stop_words
data(stop_words)
tidy_books %>%
  #remove stop_words from the preprocessed dataset
  anti_join(stop_words) %>%
  # count frequency words (token) and arrange in descending order
  count(word, sort = TRUE) %>%
#View(tidy_books)
  # filter or reduce to words with frequecy greater than 600
  filter(n > 600) %>%
  # add column with word arranged in descending order
  mutate(word = reorder(word, n)) %>%
  # plot bar graph and flip the x and y axis
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()
  
  