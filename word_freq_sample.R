
library(tidyverse)
library(lubridate)
library(tm)
library(wordcloud)
library(ggwordcloud)
all_dat = read.csv('/Volumes/Programming/Spring 2022/project/all_dat.csv')

colnames(all_dat) <- c('X', 'Name','Finished','Multiple','Phone_pickups',
                       'Screen_time','Created','Date','Drink',
                       'Meditation','Mentality','Satisfaction',
                       'Reading','Productivity','Rise_time','Run','Social',
                       'Tech','Total','Total_todo','Work_done','Key_words')


cur_month = 6
if (nchar(cur_month - 1) == 1 & cur_month != 1){
  previous_month = cur_month - 1  
  previous_month = paste0("0", previous_month, '/')
} else if (cur_month == 1){
  previous_month = 12
  previous_month = paste0(previous_month, '/')
} else {
  previous_month = cur_month - 1
  previous_month = paste0(previous_month, '/')
}
previous_month
cur_year = year(Sys.Date())


words_dat <- all_dat %>%
  select(Key_words, Date) %>%
  filter(startsWith(Date, previous_month) == TRUE)  %>%
  filter(grepl(paste0('/', cur_year), Date) == TRUE) %>%
  filter(Key_words != 0 & Key_words != '' & Key_words != '[]') %>%
  mutate(Key_words = gsub('[\']','',Key_words),
         Key_words = gsub('[\\[]','',Key_words),
         Key_words = gsub('\\]','',Key_words)) %>%
  separate(Key_words, c('word_1', 'word_2', 'word_3'), sep = ',') %>%
  pivot_longer(cols = starts_with("word"), names_to = 'word_num', values_to = 'words') %>%
  group_by(words) %>%
  summarise(freq = n())

ggplot(words_dat, aes(label = words, size = freq)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_size_area(max_size = 20) +
  theme_minimal()

wordcloud(words = words_dat$words, freq = words_dat$freq)

wordcloud(words = words_dat$words, freq = words_dat$freq, min.freq = .01,           
          max.words=70, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))
