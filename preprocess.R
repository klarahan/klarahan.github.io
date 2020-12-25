
setwd("C:/Users/hanou/Dropbox/0 MYCOMP/0_Book manuscript/Lexington/data")
Sys.setlocale("LC_ALL", locale = "korean")

library(lubridate)
library(dplyr)
library(tidytext)
library(readxl)
library(readr)
library(quanteda)
library(RcppMeCab)
library(RmecabKo)
library(stringr)

# 1a. Keywords: welfare, unification, economic democratization ---------------
# read in data by newspaper and theme
# this part assumes that you have an excel workbook with newspaper-wise sheets
# with 4 columns named: iidx, Date, Title, Body
# we will add 2 more columns each

path <- ("C:/Users/hanou/Dropbox/0 MYCOMP/0_Book manuscript/Lexington/data/all_issues.xls")

chos_wel <- read_xls(path, sheet = "Chosun_º¹Áö", col_types = "text") %>%  
  mutate(Newspaper = "Chosun", Keyword = "welfare") 
chos_uni <- read_xls(path, sheet = "Chosun_ÅëÀÏ", col_types = "text") %>%  
  mutate(Newspaper = "Chosun", Keyword = "unification") 
chos_eco <- read_xls(path, sheet = "Chosun_°æÁ¦¹ÎÁÖÈ­", col_types = "text") %>%  
  mutate(Newspaper = "Chosun", Keyword = "econdem") 

data_chos <- rbind(chos_wel, chos_uni, chos_eco) %>% 
  mutate(iidx = row_number(),
         Date = as.double(Date))

hani_wel <- read_xls(path, sheet = "Hani_º¹Áö", col_types = "text") %>%  
  mutate(Newspaper = "Hankyoreh", Keyword = "welfare") 
hani_uni <- read_xls(path, sheet = "Hani_ÅëÀÏ", col_types = "text") %>%  
  mutate(Newspaper = "Hankyoreh", Keyword = "unification") 
hani_eco <- read_xls(path, sheet = "Hani_°æÁ¦¹ÎÁÖÈ­", col_types = "text") %>%  
  mutate(Newspaper = "Hankyoreh", Keyword = "econdem") 

data_hani <- rbind(hani_wel, hani_uni, hani_eco) %>% 
  mutate(iidx = row_number(),
         Date = as.double(Date))

hank_wel <- read_xls(path, sheet = "Hangook_º¹Áö", col_types = "text") %>%  
  mutate(Newspaper = "Hankook", Keyword = "welfare") 
hank_uni <- read_xls(path, sheet = "Hangook_ÅëÀÏ", col_types = "text") %>%  
  mutate(Newspaper = "Hankook", Keyword = "unification") 
hank_eco <- read_xls(path, sheet = "Hangook_°æÁ¦¹ÎÁÖÈ­", col_types = "text") %>%  
  mutate(Newspaper = "Hankook", Keyword = "econdem") 

data_hank <- rbind(hank_wel, hank_uni, hank_eco) %>% 
  mutate(iidx = row_number(),
         Date = as.double(Date))

rm(chos_wel, hani_wel, hank_wel,
   chos_uni, hani_uni, hank_uni,
   chos_eco, hani_eco, hank_eco)

# 1b. Keyword: democracy -----------------------------------------------------
# assumes that you have text files
# columns and sheets are same as above
chos_dem <- read_csv("chosun_dem.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Chosun", Keyword = "democracy")
hani_dem <- read_csv("hani_dem.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Hankyoreh", Keyword = "democracy")
hank_dem <- read_csv("hankook_dem.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Hankook", Keyword = "democracy")

data_dem <- rbind(chos_dem, hani_dem, hank_dem) %>% 
  mutate(iidx = row_number())
rm(chos_dem, hani_dem, hank_dem)

# 1c. Keyword: left-right ---------------------------------------------------
# assumes that you have text files
# columns and sheets are same as above
chos_prog <- read_tsv("chosun_prog.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Chosun", Keyword = "progressive")
chos_cons <- read_tsv("chosun_cons.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Chosun", Keyword = "conservative")
chos_left <- read_tsv("chosun_left.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Chosun", Keyword = "left")
chos_right <- read_tsv("chosun_right.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Chosun", Keyword = "right")

hani_prog <- read_tsv("hani_prog.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Hankyoreh", Keyword = "progressive")
hani_cons <- read_tsv("hani_cons.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Hankyoreh", Keyword = "conservative")
hani_left <- read_tsv("hani_left.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Hankyoreh", Keyword = "left")
hani_right <- read_tsv("hani_right.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Hankyoreh", Keyword = "right")

hank_prog <- read_tsv("hankook_prog.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Hankook", Keyword = "progressive")
hank_cons <- read_tsv("hankook_cons.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Hankook", Keyword = "conservative")
hank_left <- read_tsv("hankook_left.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Hankook", Keyword = "left")
hank_right <- read_tsv("hankook_right.txt", locale = locale(encoding = "UTF-8")) %>%
  mutate(Newspaper = "Hankook", Keyword = "right")

# 2.  Dates and periods ---------------------------------------------------------------------
# add dates, presidential terms, and presidential parties
data <- bind_rows(data_chos, data_hani, data_hank,
                  data_dem,
                  chos_prog, chos_cons, chos_left, chos_right,
                  hani_prog, hani_cons, hani_left, hani_right,
                  hank_prog, hank_cons, hank_left, hank_right) %>% 
  mutate(Date = ymd(Date)) %>%
  mutate(Government = case_when(Date >= "1990-01-01" & Date <= "1993-02-24" ~ "1990-1993 Roh TW",
                                Date >= "1993-02-25" & Date <= "1998-02-24" ~ "1993-1998 Kim YS",
                                Date >= "1998-02-25" & Date <= "2003-02-24" ~ "1998-2003 Kim DJ",
                                Date >= "2003-02-25" & Date <= "2008-02-24" ~ "2003-2008 Roh MH",
                                Date >= "2008-02-25" & Date <= "2013-02-24" ~ "2008-2013 Lee MB",
                                Date >= "2013-02-25" & Date <= "2014-12-31" ~ "2013-2014 Park GH")) %>%
  mutate(Prezparty = case_when(Date >= "1990-01-01" & Date <= "1993-02-24" ~ "Conservative",
                               Date >= "1993-02-25" & Date <= "1998-02-24" ~ "Conservative",
                               Date >= "1998-02-25" & Date <= "2003-02-24" ~ "Liberal",
                               Date >= "2003-02-25" & Date <= "2008-02-24" ~ "Liberal",
                               Date >= "2008-02-25" & Date <= "2013-02-24" ~ "Conservative",
                               Date >= "2013-02-25" & Date <= "2014-12-31" ~ "Conservative")) 

# clean up work space
rm(data_chos, data_hani, data_hank, data_dem,
   chos_prog, chos_cons, chos_left, chos_right,
   hani_prog, hani_cons, hani_left, hani_right,
   hank_prog, hank_cons, hank_left, hank_right)

# see table overview of counts
data %>% count(Keyword, Newspaper)
data %>% count()

# save count of all articles
data_count_yearly <- data %>% 
  mutate(Date = year(Date)) %>% 
  count(Date, Newspaper, Keyword)
data_count_yearly
saveRDS(data_count_yearly, "C:/Users/hanou/Dropbox/RESEARCH/klarahan.github.io/data/data_count_yearly")

# make table for Microsoft Word: 
# copy paste to doc, click insert>table>convert text to table 
data %>% 
  count(Keyword, Newspaper) %>%
  write.csv(quote = FALSE)

# save 
saveRDS(data, "data.RDS")
data <- readRDS("data.RDS")

data_wel <- data %>% filter(Keyword == "welfare") %>% select(!(Body:Title))
data_uni <- data %>% filter(Keyword == "unification") %>% select(!(Body:Title))
data_econ <- data %>% filter(Keyword == "econdem") %>% select(!(Body:Title))
data_ideo <- data %>% filter(Keyword %in% c("progressive", "conservative", "left", "right")) %>% select(!(Body:Title))
data_dem <- data %>% filter(Keyword == "democracy") %>% select(!(Body:Title))

saveRDS(data_wel, "data_wel.RDS")
saveRDS(data_uni, "data_uni.RDS")
saveRDS(data_econ, "data_econ.RDS")
saveRDS(data_ideo, "data_ideo.RDS")
saveRDS(data_dem, "data_dem.RDS")

data_wel <- readRDS("data_wel.RDS")
data_uni <- readRDS("data_uni.RDS")
data_econ <- readRDS("data_econ.RDS")
data_ideo <- readRDS("data_ideo.RDS")
data_dem <- readRDS("data_dem.RDS")

# 3. Pre-processing ---------------------------------------------------------------------
data <- readRDS("data.RDS")
Sys.setlocale("LC_ALL", locale = "korean")
setwd("C:/Users/hanou/Dropbox/RESEARCH/klarahan.github.io")

library(dplyr, warn.conflicts = FALSE)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

#Plan B: the below method is fast
extract_nouns2 <- function(x) {
  data_econ <- x %>%
    mutate(cleaned = gsub("[^°¡-ÆR]", " ", Body, perl = TRUE)) %>%
    unnest_tokens(word, cleaned, token = "words") %>%
    mutate(word = str_squish(word)) %>% 
    mutate(word = iconv(word, from = "utf-8", "cp949")) %>%
    unnest_tokens(noun, word, token = nouns) %>% 
    filter(nchar(noun) > 1) %>% 
    group_by(Date, Title, Body, Newspaper, Keyword, Government, Prezparty) %>% 
    summarize(text = str_c(noun, collapse = " ")) %>%
    ungroup() 
}

#1. econdem
start_time <- Sys.time()

y <- data %>%
  filter(Keyword == "econdem") %>% 
  split(.$iidx) %>%
  purrr::map_dfr(extract_nouns2) 

end_time <- Sys.time()
end_time - start_time

saveRDS(y, "data/data_econ_nouns")

#2. democracy
start_time <- Sys.time()

y <- data %>%
  filter(Keyword == "democracy") %>% 
  split(.$iidx) %>%
  purrr::map_dfr(extract_nouns2) 

end_time <- Sys.time()
end_time - start_time

saveRDS(y, "data/data_dem_nouns")

#3. ideology

start_time <- Sys.time()

y <- data %>%
  filter(Keyword == "conservative" | Keyword == "right" | Keyword == "progressive" | Keyword == "left") %>% 
  split(.$iidx) %>%
  purrr::map_dfr(extract_nouns2) 

end_time <- Sys.time()
end_time - start_time

saveRDS(y, "data/data_ideo_nouns")

#4. uni

start_time <- Sys.time()

y <- data %>%
  filter(Keyword == "unification") %>% 
  split(.$iidx) %>%
  purrr::map_dfr(extract_nouns2) 

end_time <- Sys.time()
end_time - start_time

saveRDS(y, "data/data_uni_nouns")

#5. welfare

start_time <- Sys.time()

y <- data %>%
  filter(Keyword == "welfare") %>% 
  split(.$iidx) %>%
  purrr::map_dfr(extract_nouns2) 

end_time <- Sys.time()
end_time - start_time

saveRDS(y, "data/data_wel_nouns") 


#make dfm of everything
toks_econ <- readRDS("data/data_econ_nouns") %>% 
  corpus(text_field = "text") %>%  
  tokens()
saveRDS(toks_econ, "data/toks_econ")

toks_dem <- readRDS("data/data_dem_nouns") %>% 
  corpus(text_field = "text") %>%  
  tokens()
saveRDS(toks_dem, "data/toks_dem")

toks_ideo <- readRDS("data/data_ideo_nouns") %>% 
  corpus(text_field = "text") %>%  
  tokens()
saveRDS(toks_ideo, "data/toks_ideo")

toks_uni <- readRDS("data/data_uni_nouns") %>% 
  corpus(text_field = "text") %>%  
  tokens() %>% 
  tokens_sample(size = 59000)
saveRDS(toks_uni, "data/toks_uni")

toks_wel <- readRDS("data/data_wel_nouns") %>% 
  corpus(text_field = "text") %>%  
  tokens()
saveRDS(toks_wel, "data/toks_wel")
  
  
