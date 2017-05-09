library(tidyverse)
library(stringr)
library(magrittr)

#Read in data
campaign <- read_csv("data/campaign.csv")
sec_campaign <- read_csv("data/sec_campaign.csv")

#Build company name out of Facebook URL
campaign %<>% 
  select(fburl) %>% 
  rename(fb_comp_name = fburl) %>% 
  map(str_replace, ".*(com/)", "") %>% 
  map(str_replace, "/.*", "") %>% 
  map(str_replace, "\\?.*", "") %>% 
  bind_cols(campaign)

#Build a company name out of company's listed url
campaign %<>%
  select(companyurl) %>% 
  rename(url_comp_name = companyurl) %>% 
  map(str_replace, ".*(www.)|.*(://)", "") %>% 
  map(str_replace, "\\..*", "") %>% 
  bind_cols(campaign)

#Add identifiers to track where data came from
campaign %<>% 
  mutate(from_campaign = 1)
sec_campaign %<>% 
  mutate(from_sec_campaign = 1)

#Join campaign and sec_campaign
clean_data <- campaign %>% 
  full_join(sec_campaign, "securl") %>% 
  rename(platform_scrape_date = date.x, sec_campaign_scrape_date = date.y) %>% 
  rename(platform_name = name, platform_url = url)
#clean_data %>% select(cik) %>% summarise(unique = n_distinct(cik), total = nrow(.), not_na = sum(!is.na(cik)))

#Add Reg C annual disclosure data to clean_data
clean_data <- read_csv("data/c_annual_disclosure.csv") %>% 
  select(url, cik:netincomepriorfiscalyear) %>% 
  rename(c_ann_disc_url = url) %>% 
  mutate(from_c_ann_disc = 1) %>% 
  mutate(cik = as.numeric(cik)) %>% 
  full_join(clean_data, "cik")

c_ann_disc <- read_csv("data/c_annual_disclosure.csv") %>% mutate(cik = as.numeric(cik))
c_iss_info <- read_csv("data/c_issuer_info.csv") %>% mutate(cik = as.numeric(cik))
c_off_info <- read_csv("data/c_offering_info.csv") %>% mutate(cik = as.numeric(cik))
c_sig_data <- read_csv("data/c_signature_data.csv") %>% mutate(cik = as.numeric(cik))

df <- c_ann_disc %>% 
  select(url:last_modified, cik:netincomepriorfiscalyear) %>% 
  rename(ann_disc_url = url, ann_disc_crawl_date = date, ann_disc_last_mod = last_modified) %>% 
  full_join(select(c_iss_info, url:last_modified, cik:commissionfilenumber), "cik") %>%
  rename(iss_info_url = url, iss_info_crawl_date = date, iss_info_last_mod = last_modified) %>% 
  full_join(select(c_off_info, url:last_modified, cik:deadlinedate), "cik") %>% 
  rename(off_info_url = url, off_info_crawl_date = date, off_info_last_mod = last_modified) %>% 
  full_join(select(c_sig_data, url:last_modified, cik, issuer, issuertitle), "cik") %>% 
  rename(sig_url = url, sig_crawl_date = date, sig_last_mod = last_modified) %>% 
  filter(cik == 1677405) %>% 
  keep(is.numeric) %>% 
  select(shorttermdebtmostrecentfiscalyear)
  map(sd)
  select(cik) %>% unique %>% nrow

c_ann_disc %>% 
  select(cik) %>% 
  bind_rows(select(c_iss_info, cik)) %>% 
  bind_rows(select(c_off_info, cik)) %>% 
  bind_rows(select(c_sig_data, cik)) %>% 
  unique %>% nrow
  

#Add Reg C issuer info data to clean_data
df <- ("data/c_issuer_info.csv") %>% 
  select(url, cik:commissionfilenumber) %>% 
  rename(c_ann_disc_url = url) %>% 
  mutate(from_c_iss_info = 1) %>% 
  mutate(cik = as.numeric(cik)) %>% 
  full_join(clean_data, "cik")

df %>% select(cik) %>% n_distinct

df <- full_join(c_ann_disc, c_iss_info, "cik")

c_ann_disc %>% semi_join(c_iss_info, "cik") %>% select(cik) %>% n_distinct()




c_off_info <- read_csv("data/c_offering_info.csv")
c_sig_data <- read_csv("data/c_signature_data.csv")












