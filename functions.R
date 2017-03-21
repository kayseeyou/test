rm(list=ls())


library(plyr)
library(dplyr)
library(stringi)
library(stringr)
library(tidyr)
library(tidytext)
library(assertthat)
library(lme4)
library(ggplot2)
library(stargazer)
library(xtable)
library(gender)
library(broom)
library(lubridate)

# Functions----------------------------------------------

#i want to add this to a package!

get_last_name <- function(full.name) {
  name.list <- str_split(full.name, " ")
  name.list.length <-lapply(name.list, length) 
  last.name <-unlist(lapply(1:length(name.list), 
                            function(x) return(name.list[[x]][name.list.length[[x]]])))
  last.name <- tolower(iconv(last.name,"latin1", "ASCII", ""))
  
  return(last.name)
}

get_first_name <- function(full.name) {
  name.list <- str_split(full.name, " ")
  name.list.length <-lapply(name.list, length) 
  first.name <- unlist(lapply(name.list, 
                              function(x) return(x[1])))
  counts <- str_count(first.name,"[A-Za-z]")
  replace <- counts < 2
  first.name[replace] <-  unlist(lapply(name.list[replace], 
                                        function(x) return(x[2])))
  first.name <- tolower(iconv(first.name,"latin1", "ASCII", ""))
  return(first.name)
}

remove_paranthesis <- function(string) {
  string <- str_replace_all(string,regex(" *\\(.*?\\) *"), "")
  return(string)
}
#REDO
get_race <- function(last.name) {
  
  #check out wru package get census api
  #separate these out by race and gender, so that they can be in a mutate on a single variable
  #and make sure you can name something special (paste with string= "manager" eg.)
  #maybe redo this so its more versatile for manager and referree
  #stopifnot(df %has_name% "last.name")
  last.name <- tibble(last.name)
  names(last.name) <- "last.name"
  
  race.df <-  read.csv("~/Desktop/Katherine_training/functions_data_etc/census_names.csv",
                       stringsAsFactors = FALSE)
  race.df <-  race.df %>% 
    mutate(name=tolower(name)) %>% 
    select(name, pctwhite:pcthispanic) %>% 
    rename(last.name=name) %>%
    mutate_at(vars(starts_with("pct")), as.numeric)  %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    gather(race, pct.race, pctwhite:pcthispanic) %>%
    filter(pct.race >=65) %>%
    mutate(race = gsub("pct", "", race)) %>%
    select(-pct.race)
  
  out.df <- last.name %>% 
    left_join(.,race.df) %>%
    mutate(race = stri_trans_totitle(ifelse(is.na(race),"na_race",race)))
  
  return(out.df$race)
}

get_gender <- function(first.name) {
  #separate these out by race and gender, so that they can be in a mutate on a single variable
  #and make sure you can name something special (paste with string= "manager" eg.)
  #maybe redo this so its more versatile for manager and referree
  #stopifnot(df %has_name% "first.name")
  first.name <- tibble(first.name)
  names(first.name) <- "first.name"
  
            
  gen.df <-   distinct(first.name) %>%
              .$first.name %>%
              gender(.,years=c(1960,1998), method="ssa") %>%
              select(-gender,-year_max,-year_min) %>%
              gather(gender, pct.gender, proportion_female:proportion_male) %>%
              filter(pct.gender >=.65) %>%
              mutate(gender = gsub("proportion_", "", gender)) %>%
              select(-pct.gender) %>%
              rename(first.name=name)
            
  out.df <- first.name %>% 
            left_join(.,gen.df) %>% 
            mutate(gender = stri_trans_totitle(ifelse(is.na(gender),"na_gender",gender)))
  
  return(out.df$gender)
}
