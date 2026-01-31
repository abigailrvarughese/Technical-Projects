library(tidyverse)
library(mosaic)
library(scales)
options(scipen = 777)  
glimpse(tate_modern)

## Tate Modern ##

tate_artists = tate_modern %>% 
  group_by(artist) %>% 
  summarise(n_titles = n()) %>% 
  arrange(desc(n_titles))
head(tate_artists)

tate_fem = tate_modern %>% 
  filter(gender == "Female") %>% 
  group_by(acquisition_year) %>% 
  summarize(n_year = n()) %>% 
  arrange(desc(n_year))
head(tate_fem)

ggplot(tate_fem)+
  geom_line(aes(x=acquisition_year, y=n_year))+
  labs(x="Year",
       y= "Number of Works Acquired",
       title="Female-Created Works Acquired by the Tate by Year")

tate_modern = tate_modern %>% 
  mutate(post_1950 = ifelse(creation_year <= 1950,
                           yes="1950 and previous", no="post 1950"))
tate_width = tate_modern %>% 
  filter(width < 400)
glimpse(tate_width)

ggplot(tate_width)+
  geom_histogram(aes(x=width, y=..density.., fill=post_1950), binwidth = 13)+
  facet_wrap(~post_1950)+
  labs(x="Width (cm)",
       y="Percentage of Works",
       title="Widths of Tate Works Based on Creation Relative to 1950")+
  scale_fill_manual(values = c("darkgreen", "lightgreen"))+
  theme(legend.position = 'none')


tate_medium = tate_modern %>% 
  group_by(medium) %>% 
  summarise(max_height = max(height),
            median_height = median(height),
            sd_height = sd(height),
            n_medium = n()) %>% 
  arrange(desc(sd_height))
head(tate_medium)


## Grammar ##

glimpse(grammar)
grammar = grammar %>% 
  mutate(region = fct_infreq(region))
ggplot(grammar)+
  geom_bar(aes(x=region, fill=important))+
  scale_fill_manual(values = c("darkgreen", "lightgreen"))+
  labs(x="Region",
       y="Count",
       title= "The Impoortance of Grammar Across Regions",
       fill="Is Proper Grammar Important?")+
  coord_flip()

grammar_region = grammar %>% 
  group_by(region) %>% 
  summarise(n = n())
head(grammar_region)

grammar = grammar %>%
  mutate(care_comma = factor(care_comma,
                             levels = c("Not at all", "Not much", "Some", "A lot")))

ggplot(grammar, aes(y = care_comma, fill = prefer_comma)) +
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("darkgreen", "lightgreen"))+
  labs(y="How much do you care about the comma?",
       fill="Prefer the comma?",
       x="Count",
       title="Opinions on the Oxford Comma")

diffprop(data_noun ~ prefer_comma, data=grammar)
boot_grammar_data = do(5000)*diffprop(data_noun ~ prefer_comma, data=resample(grammar)) ##bootstrapping##
head(boot_grammar_data)
confint(boot_grammar_data, level=.95)

ggplot(grammar)+
  geom_bar(aes(x=data_noun, fill=prefer_comma))+
  scale_fill_manual(values = c("darkgreen", "lightgreen"))+
  facet_wrap(~prefer_comma)+
  labs(x="Is Data a Plural Noun?",
       y="Count",
       fill="Prefer the Oxford Comma?",
       title="Association between Preference of the Oxford Comma and the Plurality of Data")
  
## Scooby Doo ##
glimpse(scooby)

ggplot(scooby)+
  geom_histogram(aes(x=rating, fill=scrappy))+
  scale_fill_manual(values = c("#128a84", "#bb5c37"))+
  labs(x="Episode IMDB Rating",
       y="Count",
       fill="Scrappy-Doo?",
       title="How does Scrappy-Doo change Scooby-Doo Ratings?")

mean(rating ~ scrappy, data=scooby)
diffmean(rating ~ scrappy, data=scooby)
boot_scooby_scrappy = do(10000)*diffmean(rating ~ scrappy, data=resample(scooby))
head(boot_scooby_scrappy)
confint(boot_scooby_scrappy, level=.95) 

monsters = scooby %>%
  filter(monster_type != 'NULL') %>%
  group_by(monster_type) %>%
  summarize(count = n(),
            SD = sd(engagement),
            mean = mean(engagement)) %>%
  filter(count > 3) %>% 
  arrange(desc(count))
head(monsters)


## gas prices
glimpse(approval)
ggplot(approval, aes(x=gas, y=approval))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Approval Rating from Gallup Poll",
       y="Average Price for Gallon of Gas (in US dollars)",
       title="Relationship between Presidential Approval and Gas Prices")
lm_approval_gas = lm(approval~gas, data=approval)
coef(lm_approval_gas)
boot_lm_approval_gas = do(10000)*lm(approval ~ gas, data=resample(approval))
head(boot_lm_approval_gas)
confint(boot_lm_approval_gas, level = .95)

.05*-0.15593287
.05*-0.20775875
