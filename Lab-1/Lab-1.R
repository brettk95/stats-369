library("tidyverse")
log_tib = read_csv("events_log.csv", col_types = 'cccccicii')
log_tib = log_tib %>% separate(timestamp, c('Day', 'Time'), sep = 8)


# Question 1a
test_df = data.frame(Day = c('Mon', 'Mon', 'Mon', 'Mon','Mon', 'Tue','Tue','Tue','Tue', 'Wed','Wed','Wed','Wed'), 
                     session_id = c('a', 'x' ,'y','b','b','c','c','c','c','d','e','f','f'), 
                     action = c('searchPage','checkin' ,'searchPage','searchPage','visitPage', 'searchPage', 'visitPage','searchPage','visitPage','searchPage', 'checkin', 'visitPage','visitPage'))

test_df %>% group_by(Day, action) %>% summarise(n = n_distinct(session_id))

test_df %>% group_by(Day) %>% summarise(n = n_distinct(session_id))
test_df %>% group_by(Day,action) %>% summarise(n = n_distinct(session_id)) %>% filter(action == 'visitPage') %>% print()

# the number of unique session_id's per day
total_unique_sessions = log_tib %>% group_by(Day) %>% summarise(total_sessions = n_distinct(session_id))
total_unique_sessions = total_unique_sessions[-1,] %>% print()

# the number of session_id's that have 'visitpage' per day
unique_sessions_visits = log_tib %>% group_by(Day,action) %>% summarise(visited = n_distinct(session_id)) %>% filter(action == 'visitPage') %>% print()

answer_1a = inner_join(unique_sessions_visits, total_unique_sessions, by = 'Day')
answer_1a = answer_1a %>% mutate(Clickthrough_rate = visited/total_sessions)
answer_1a

# Question 1b
total_unique_sessions_groups = log_tib %>% group_by(group,Day) %>% summarise(total_sessions = n_distinct(session_id)) %>% print()
total_unique_sessions_groups = total_unique_sessions_groups[-1,] %>% print()
unique_sessions_visits_groups = log_tib %>% group_by(Day,group, action) %>% summarise(visited = n_distinct(session_id)) %>% filter(action == 'visitPage') %>% print()
unique_sessions_visits_groups = unique_sessions_visits_groups[,c('group','Day','action','visited')]

answer_1b = inner_join(unique_sessions_visits_groups, total_unique_sessions_groups, by = c("Day","group"))
answer_1b = answer_1b %>% mutate(Proportion = visited/total_sessions) %>% arrange(Day)
answer_1b

# Question 2

first_page_visit_df = 
  log_tib %>% 
  group_by(session_id) %>% 
  arrange(desc(Time), .by_group = TRUE) %>% 
  filter(action == 'visitPage')%>% 
  group_by(session_id) %>% 
  summarise(first_page_position = first(result_position)) %>% 
  group_by(first_page_position) %>% 
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))
