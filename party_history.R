setwd("C:/Users/hanou/Dropbox/RESEARCH/klarahan.github.io")
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(showtext)
Sys.setlocale(locale = "Korean")
#if Korean text breaks, open with encoding cp949

data <- read_xlsx("data/party history.xlsx", sheet = "OVERVIEW")
data_cleaned <- data %>% 
  mutate(registered = ymd(registered),
         terminated = ymd(terminated)) %>% 
  # mutate(term = case_when(
  #   registered <= ymd(20200529) & registered >= ymd(20160530) ~ 20,
  #   registered <= ymd(20160529) & registered >= ymd(20120530) ~ 19,
  #   registered <= ymd(20120529) & registered >= ymd(20080530) ~ 18,
  #   registered <= ymd(20080529) & registered >= ymd(20040530) ~ 17,
  #   registered <= ymd(20040529) & registered >= ymd(20000530) ~ 16,
  #   registered <= ymd(20000529) & registered >= ymd(19960530) ~ 15,
  #   registered <= ymd(19960529) & registered >= ymd(19920530) ~ 14,
  #   registered <= ymd(19920529) & registered >= ymd(19880530) ~ 13,
  #   registered <= ymd(19880529) & registered >= ymd(19850411) ~ 12
  # )) %>% 
  # mutate(registered = year(registered),
  #        terminated = year(terminated))  %>% 
  select(party_name, registered, terminated, reason, outcome) %>% 
  filter(registered > 1987)
data_cleaned %>% top_n(10)

data_cleaned %>% 
  filter(grepl("등록취소|자진해산", reason)) %>% 
  count(reason)

data_cleaned <- data_cleaned %>% 
  filter(!grepl("등록취소|자진해산", reason)) %>% 
  mutate(newname = gsub(".*\\((.*)\\).*",  "\\1", outcome)) %>% 
  mutate(newname = gsub(".*→",  "", newname)) %>%
  mutate(newname = coalesce(newname, reason)) %>%
  mutate(newname = gsub("과 신설합당|으로 신설합당|에 흡수합당|흡수합당|소멸",  "", newname)) %>% 
  select(!c(reason, outcome))

assembly_seats <- read_xlsx("data/party history.xlsx", sheet = "ASSEMBLY_SEATS",
                            col_names = c("party_name", "perc", "count")) %>%
  tibble::rowid_to_column("ID") %>%
  mutate(legis_term_start = case_when(
    ID < 9 ~ 2012,
    ID < 19 & ID > 10 ~ 2008,
    ID < 29 & ID > 20 ~ 2004,
    ID < 38 & ID > 30 ~ 2000,
    ID < 46 & ID > 39 ~ 1996,
    ID < 56 & ID > 47 ~ 1992,
    ID < 74 & ID > 57 ~ 1988,
    ID < 87 & ID > 75 ~ 1985
  )) %>%
  # mutate(term = case_when(
  #   legis_term_start <= 2015 & legis_term_start >= 2012 ~ 19,
  #   legis_term_start <= 2011 & legis_term_start >= 2008 ~ 18,
  #   legis_term_start <= 2007 & legis_term_start >= 2004 ~ 17,
  #   legis_term_start <= 2003 & legis_term_start >= 2000 ~ 16,
  #   legis_term_start <= 1999 & legis_term_start >= 1996 ~ 15,
  #   legis_term_start <= 1995 & legis_term_start >= 1992 ~ 14,
  #   legis_term_start <= 1991 & legis_term_start >= 1988 ~ 13,
  #   legis_term_start <= 1987 & legis_term_start >= 1985 ~ 12
  # )) %>%
  filter(!perc == "득표율") %>%
  mutate(perc = as.numeric(perc)) 

assembly_seats %>%
  filter(!is.na(legis_term_start)) %>%
  arrange(desc(perc))

assembly_seats %>% filter(term == 14)
data_cleaned %>% filter(party_name == "청년진보당")

joined_list <- assembly_seats %>%
  left_join(data_cleaned) %>% 
  mutate(registered = case_when(
    party_name == "새누리당" ~ ymd(20120213),
    party_name == "한나라당" ~ ymd(19971121),
    party_name == "친박연대" ~ ymd(20080321),
    party_name == "창조한국당" ~ ymd(20071107),
    party_name == "새천년민주당" ~ ymd(20000120),
    party_name == "국민통합21" ~ ymd(20021111),
    party_name == "민주국민당" ~ ymd(20000308),
    party_name == "희망의한국신당" ~ ymd(20000215),
    party_name == "민주자유당" ~ ymd(19900122),
    party_name == "민중당" ~ ymd(19901110),
    party_name == "한겨레민주당" ~ ymd(19880406),
    party_name == "신한민주당" ~ ymd(19850118),
    party_name == "민주한국당" ~ ymd(19810101),
    party_name == "민중의당" ~ ymd(19880311),
    TRUE ~ ymd(registered)
  )) %>% 
  mutate(terminated = case_when(
    party_name == "새누리당" ~ ymd(20170213),
    party_name == "한나라당" ~ ymd(20120213),
    party_name == "친박연대" ~ ymd(20100222),
    party_name == "창조한국당" ~ ymd(20120412),
    party_name == "새천년민주당" ~ ymd(20050506),
    party_name == "국민통합21" ~ ymd(20040913),
    party_name == "민주국민당" ~ ymd(20040418),
    party_name == "희망의한국신당" ~ ymd(20010121),
    party_name == "민주자유당" ~ ymd(19951206),
    party_name == "민중당" ~ ymd(19920301),
    party_name == "한겨레민주당" ~ ymd(19910313),
    party_name == "신한민주당" ~ ymd(19850118),
    party_name == "민주한국당" ~ ymd(19880426),
    party_name == "민중의당" ~ ymd(19880429),
    TRUE ~ ymd(terminated)
  )) 

joined_list <- joined_list %>% 
  group_by()  %>% 
  mutate(party_name = factor(party_name)) %>%
  mutate(terminated = tidyr::replace_na(terminated, ymd(20170904))) %>% 
  mutate(pos = case_when(
    party_name == "새누리당" ~ "conservative",
    party_name == "민주통합당" ~ "liberal",
    party_name == "통합진보당" ~ "progressive",
    party_name == "한나라당" ~ "conservative",
    party_name == "통합민주당" ~ "liberal",
    party_name == "자유선진당" ~ "conservative",
    party_name == "친박연대" ~ "conservative",
    party_name == "민주노동당" ~ "progressive",
    party_name == "열린우리당" ~ "liberal",
    party_name == "한나라당" ~ "conservative",
    party_name == "민주노동당" ~ "progressive",
    party_name == "새천년민주당" ~ "conservative",
    party_name == "자유민주연합" ~ "conservative",
    party_name == "한나라당" ~ "conservative",
    party_name == "새정치국민회의" ~ "conservative",
    party_name == "자유민주연합" ~ "liberal",
    party_name == "민주국민당" ~ "liberal",
    party_name == "신한국당" ~ "conservative",
    party_name == "새정치국민회의" ~ "conservative",
    party_name == "통합민주당" ~ "liberal",
    party_name == "자유민주연합" ~ "conservative",
    party_name == "민주자유당" ~ "conservative",
    party_name == "민주당" ~ "liberal",
    party_name == "통일국민당" ~ "conservative",
    party_name == "신정당" ~ "conservative",
    party_name == "민중당" ~ "progressive",
    party_name == "민주정의당" ~ "conservative",
    party_name == "평화민주당" ~ "liberal",
    party_name == "통일민주당" ~ "conservative",
    party_name == "신민주공화당" ~ "conservative",
    party_name == "한겨레민주당" ~ "liberal",
    party_name == "한겨레민주당" ~ "liberal"
    )) 

joined_list %>% 
  mutate(registered = as.Date(registered, format = "ymd")) %>% 
  mutate(pos = recode(pos, progressive = "prog")) %>% 
  filter(!party_name == "민주정의당") %>% 
  mutate(party_name = case_when(
    party_name == "새누리당" ~ "새누리당\nSaenuri Party",
    party_name == "자유선진당" ~ "자유선진당\nLiberty Forward Party",
    party_name == "친박연대" ~ "친박연대\nChinbak Yeondae",
    party_name == "새천년민주당" ~ "새천년민주당\nMillennium Democratic Party",
    party_name == "한나라당" ~ "한나라당\nHannara Party",
    party_name == "새정치국민회의" ~ "새정치국민회의\nNational Congress for New Politics",
    party_name == "자유민주연합" ~ "자유민주연합\nUnited Liberal Democrats",
    party_name == "통일국민당" ~ "통일국민당\nUnification National Party",
    party_name == "신한국당" ~ "신한국당\nNew Korea Party",
    party_name == "민주자유당" ~ "민주자유당\nDemocratic Liberal Party",
    party_name == "신민주공화당" ~ "신민주공화당\nNew Democratic Republican Party",
    party_name == "통일민주당" ~ "통일민주당\nReunification Democratic Party",
    party_name == "민주정의당" ~ "민주정의당\nDemocratic Justice Party",
    party_name == "민주통합당" ~ "민주통합당\nDemocratic United Party",
    party_name == "평화민주당" ~ "평화민주당\nPeace Democratic Party",
    party_name == "통합민주당" ~ "통합민주당\nUnified Democratic Party",
    party_name == "열린우리당" ~ "열린우리당\nYeollin Uri Party",
    party_name == "민주국민당" ~ "민주국민당\nDemocratic National Party",
    party_name == "민주당" ~ "민주당\nDemocratic Party",
    party_name == "통합진보당" ~ "통합진보당\nUnified Progressive Party",
    party_name == "민주노동당" ~ "민주노동당\nDemocratic Labor Party",
    TRUE ~ as.character(party_name))) %>% 
  filter(!pos == is.na(pos)) %>% 
  filter(perc >= 5) %>% 
  group_by(pos) %>% 
  ggplot(aes(ymin = registered, ymax = terminated, 
             x = reorder(party_name, registered), color = pos, size = 5)) +
  geom_linerange() +
  scale_colour_manual(values = c("black", "darkgrey", "lightgrey")) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        # strip.text.y.right = element_text(angle = 0),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(pos~ ., scales = "free_y",  space = "free_y") +
  # theme(panel.grid = element_blank()) +
  xlab("") +
  ylab("") +
  labs(caption = "parties above 5% votes in legislature.
  raw data source: National Election Commission, The National Assembly of the Republic of Korea.
       visualization: author's own.") +
  scale_y_date(breaks = "1 year", labels = date_format("%Y"), 
             limits = as.Date(c("1987-01-01", "2017-12-31")), expand = c(0,0))

showtext_auto()
x
ggsave("plots/1_parties.pdf", width=6, height=7, dpi = 300, family = "sans")  
ggsave("plots/1_parties.jpg", width=6, height=6, dpi = 300, family = "sans")  
showtext_auto(FALSE)

