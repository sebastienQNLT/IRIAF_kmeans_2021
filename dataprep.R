#https://www.kaggle.com/stefanoleone992/fifa-21-complete-player-dataset/version/1?select=players_21.csv

options(stringsAsFactors = FALSE)
library(stringr)
library(dplyr)

fifa_data <- read.csv2("./players_21.csv",sep=',') %>%
  filter(team_position !="SUB") %>%
  filter(team_position !="RES") %>%
  filter(team_position !="") %>% 
  mutate(right_foot=ifelse(preferred_foot=="Right",1,0)) %>% 
  select(-c(long_name,player_url,dob,player_positions,work_rate,body_type,
            real_face,player_tags,team_jersey_number,loaned_from,joined,contract_valid_until,
            nation_position,nation_jersey_number,
            player_traits,rs,lw,lf, cf,rf,rw,
            lam,cam,ram,
            lm,lcm,cm,                        
            rcm,rm,lwb,                       
            ldm,cdm,rdm,                       
            rwb,lb,lcb,                       
            cb,rcb,rb,ls,st,league_rank,international_reputation,weak_foot,defending_marking,
            league_name,club_name,nationality,
            preferred_foot,skill_moves,
            overall, wage_eur,value_eur,release_clause_eur))

write.csv2(fifa_data,"./fifa_data.csv", row.names = FALSE)
