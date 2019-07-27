library("tidyverse")
log_tib = read_csv("events_log.csv", col_types = 'cccccicii')
log_tib = log_tib %>% separate(timestamp, c('Day', 'Time'), sep = 8)

# Question 1a
 
click_through_df = 
  log_tib %>% 
  group_by(Day, session_id) %>% 
  summarise(visited = sum(action == 'visitPage'), 
            not_visited = sum(action != 'visitPage'), 
            ratio = visited/not_visited) %>% 
  group_by(Day) %>% 
  summarise(visited = sum(visited), 
            not_visited = sum(not_visited), 
            ratio = visited/not_visited)

# Question 1b
click_through_group_df = 
  log_tib %>% 
  group_by(Day, session_id,group) %>% 
  summarise(visited = sum(action == 'visitPage'), 
            not_visited = sum(action != 'visitPage'))%>% 
  group_by(Day, group) %>%
  summarise(visited = sum(visited), 
            not_visited = sum(not_visited), 
            ratio = visited/not_visited)

# Question 2

log_tib %>% 
  group_by(session_id) %>% 
  arrange(desc(Time), .by_group = TRUE) %>% 
  filter(action == 'visitPage') %>% 
  group_by(session_id) %>% 
  summarise(first_page = first(page_id))