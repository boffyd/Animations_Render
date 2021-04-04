#load relevant libraries

library(tidyverse)
library(gganimate)
library(plotly) #includes ggplot and dplyr
library(forcats)
library(janitor)
library(gifski) #used for rendering output for animate (gifs)
library(av) #used for saving outputs
library(gapminder)
library(lubridate) #isoweek

#import dataset
file_name <- 'Covid_19Trends.csv'
indus_df1 = read.table(file_name, sep=",", header = TRUE)

#DATA WRANGLING
#clean names
indus_df1 <- indus_df1 %>%
  clean_names() #good house keeping, all lower case with _ between words

#to order the date, we need to convert from a factor to a date
indus_df1$date_act <- as.Date(indus_df1$date,format = "%d-%b-%y")

#reorder to get the format we want
indus_df1$date_act <- format(indus_df1$date_act,'%d-%b-%Y') #converts to character
indus_df1$date_act <- as.Date(indus_df1$date_act,format = "%d-%b-%Y") #converts back to date

#converting to week number, can help us order it.
indus_df1 <- indus_df1 %>%
  mutate(weekno = isoweek(date_act))


#lets set up a cumulative sum to see how this looks for the animation.  We need to group by industry, and then order by industry and then week no
indus_df1 <- indus_df1 %>%
  group_by(industry) %>% 
  arrange(industry, weekno) %>% 
  mutate(vac_cum = cumsum(vacancies)) %>% 
  ungroup()



#rank is introduced and typically used for real number comparison, however with an integer, it is likely that we will have overlapping or the same, rank, we then create relative ranks based on how it sits in the grouping to simplify the animation.  This is rank_final.

#WE HAVE TWO OPTIONS HERE, they have to be changed in the dataframe rank, and passed to the plot
# PASS -vacancies
# PASS -vac_cum

indus_df1 <- indus_df1 %>%
  group_by(date) %>%
  mutate(rank = rank(-vacancies,ties.method="min")) %>%
  arrange(desc(rank)) %>%
  mutate(rank_final = as.double(rev(seq(12:1))) * 1.0) %>%
  ungroup()

#lets add ladder points to accumulate
indus_df1 <- indus_df1 %>%
  mutate(points = case_when(rank_final == 12 ~ 1,
                            rank_final == 11 ~ 2,
                            rank_final == 10 ~ 3,
                            rank_final == 9 ~ 4,
                            rank_final == 8 ~ 5,
                            rank_final == 7 ~ 6,
                            rank_final == 6 ~ 7,
                            rank_final == 5 ~ 8,
                            rank_final == 4 ~ 9,
                            rank_final == 3 ~ 10,
                            rank_final == 2 ~ 11,
                            rank_final == 1 ~ 12)) %>%
  group_by(weekno) %>%
  arrange(weekno) %>%
  mutate(final_points = cumsum(points))

#Lets add a column to define the fill colours
indus_df1 <- indus_df1 %>%
  mutate(idcheck = case_when(industry == 'Pharmacy retail' ~ 'N',
                             industry == 'Grocery retail' ~ 'N',
                             industry == 'Medical reception / admin' ~ 'N',
                             industry == 'Health Care' ~ 'N',
                             industry == 'Warehousing / Logistics' ~ 'N',
                             industry == 'Delivery / Truck' ~ 'N',
                             industry == 'Disability / Community' ~ 'N',
                             industry == 'General retail' ~ 'Y',
                             industry == 'Aged Care' ~ 'Y',
                             industry == 'Child Care' ~ 'Y',
                             industry == 'Hospitality / Tourism' ~ 'Y',
                             industry == 'Admin' ~ 'Y'))

#run the same piping command as above and change the ranking based on the cumulative points ranking

indus_df1 <- indus_df1 %>%
  group_by(date) %>%
  mutate(rank = rank(-final_points,ties.method="min")) %>%
  arrange(desc(rank)) %>%
  mutate(rank_final = as.double(rev(seq(12:1))) * 1.0) %>%
  ungroup()


##we now have a data set with a ranking (rank_final) that we can use to control the relative position of the animation.
#we will use ggplot to set up the basics and then follow through with gganimate and giski

pp1 <- ggplot(indus_df1, aes(group = industry, fill = idcheck)) +
  
  geom_tile(aes(x = rank_final, y = final_points/2, height = final_points, width = 0.9), alpha = 0.8, colour = NA) +
  
  scale_fill_manual(values = c('Y' = 'gray61','N' = 'navyblue'),
                    guide = FALSE) +
#this geom_text locates the moving axis total.  The largest vacancy number is 9, alt. you can use y = vacancies to put at the end of teh bar chart however this isn't always possible.
  geom_text(aes(x = rank_final, 
                y = max(final_points,4), 
                label = industry), vjust = 0.2, hjust = 1) + #positions text at the y value (remember this is flipped for simplicity)
  
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() + 
  scale_x_reverse() + #put rank 1 at the top
  guides(color = FALSE, fill = FALSE) +
  
  #gganimate specific items {closest_state} for numbers, {frame_time} for dates
  labs(subtitle = 'Australian Industry Vacancies, Week : {closest_state}',
       title='Impact of COVID 19 - Job Vacancies', 
       x = 'Industry', 
       y = 'Relative Vacancies',
       caption = 'Source: APM Job Screening Database') +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # axes post-flip
        axis.text.y  = element_blank(),  # axes post-flip
        axis.text.x  = element_blank()) +
  #use transition_time for dates, and transition_states for numbers
  
  transition_states(weekno, transition_length = 1, #1/4 of the time for transition
                    state_length = 3) + #3/4 of the time for the state length
  enter_grow() +
  exit_shrink() +
  ease_aes('linear')

pp1

#running the below code should limit the looping of the GIF
ppout <- animate(pp1,nframes = 200, fps = 10, renderer = gifski_renderer(loop = FALSE))

anim_save("output5.gif", ppout)


#ggplot for colour test.
indus_dfwk7 <- indus_df1 %>%
  filter(weekno == 12) %>%
  mutate(idcheck = case_when(industry == 'Pharmacy retail' ~ 'N',
                             industry == 'Grocery retail' ~ 'N',
                             industry == 'Medical reception / admin' ~ 'N',
                             industry == 'Health Care' ~ 'N',
                             industry == 'Warehousing / Logistics' ~ 'N',
                             industry == 'Delivery / Truck' ~ 'N',
                             industry == 'Disability / Community' ~ 'N',
                             industry == 'General retail' ~ 'Y',
                             industry == 'Aged Care' ~ 'Y',
                             industry == 'Child Care' ~ 'Y',
                             industry == 'Hospitality / Tourism' ~ 'Y',
                             industry == 'Admin' ~ 'Y'))

#PLOT CHECK

p1 <- ggplot(data = indus_dfwk7)
p1 <- p1 + geom_col(aes(x = reorder(industry,final_points), 
                  y = final_points, fill = idcheck)) + 
  scale_fill_manual(values = c('Y' = 'gray61','N' = 'dodgerblue3'),
                                 guide = FALSE) +
  coord_flip() +
  labs(subtitle = 'Australian Industry Vacancies, By Week',
       title='Impact of COVID 19 - Job Vacancies', 
       x = 'Industry', 
       y = 'Relative Vacancies',
       caption = 'Source: APM Job Screening Database') + 
  theme_minimal() 


library(plotly)

ggplotly(p1)

