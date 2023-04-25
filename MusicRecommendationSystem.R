#installing packages
install.packages("magrittr") 
install.packages("dplyr")
install.packages("tidyverse")
install.packages("devtools")
install.packages("recommenderlab")


#loading the packages 
library(tidyverse)
library(recommenderlab)

# Get the Data

polls = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')


polls
rankings


#counting the titles 
polls %>%
  count(title, sort= TRUE)

polls  %>%
  count(year) %>%
  mutate(decade = floor(year/10)*10) %>%
  mutate(decade = as.factor(decade))%>%
  ggplot(aes(x=year,y= n, fill = decade)) + geom_col()

#plotting the popularity as per year 
polls %>%
  count(year)%>%
  mutate(decade = floor(year/10)*10)%>%
  mutate(decade = as.factor(decade))%>%
  ggplot(aes(x=decade, y=n, fill= decade)) + geom_col()

#whose the most famous artist

polls%>%
  count(artist , sort = TRUE)%>%
  ggplot(aes(x=n)) +geom_density()
  
#counting the top artists
rankings%>%
  select(artist, n,n1,n2,n3,n4,n5)%>%
  group_by(artist)%>%
  summarise_all(sum)
  dplyr:: filter(!str_detect(artist,"ft."))%>%   #removing artist that have the word 'ft.' in it
  #slice(1:5)
  
  
  
rankings%>%
  select(artist, n,n1,n2,n3,n4,n5)%>%
  group_by(artist)%>%
  summarise_all(sum)%>%
  arrange(desc(n1))
  #filter(!str_detect(artist,"ft."))   #removing artist that have the word 'ft.' in it
  #slice(1:5)
  
  
  
#15% of the top songs voted by one country 
polls%>%
  count(title,critic_country, name="song_nom")%>%
  add_count(title,name = "number_of_countries")%>%
  filter(number_of_countries ==1 & critic_country !='US')%>%
  nrow() /nrow(polls)
  
polls%>%
  count(title,critic_country, name="song_nom_country")%>%
  add_count(title,name = "number_of_countries")%>%
  filter(number_of_countries !=1)%>%
  select(-number_of_countries)%>%
  pivot_wider(names_from="critic_country", values_from = "song_nom_country", values_fill = list(song_nom_country = 0))
  

rap_matrix <-polls%>%
  select(critic_name,title)%>%
  mutate(n=1)%>%
  arrange(title)%>%
  pivot_wider(names_from = "title",values_from = "n",values_fill = list(n=0))%>%
  select(-critic_name)%>%
  as.matrix()%>%
  as("binaryRatingMatrix")


rap_matrix


training_schema =  recommenderlab:: evaluationScheme(rap_matrix,method = "split", train = .8, given =-1)
training_schema
 
UBCF_Model <- evaluate(training_schema,method = "UBCF", type ="topNList", n= 5)
IBCF_Model <- evaluate(training_schema,method ="IBCF", type = "topNList", n =5)

#comparing the two recommendation models
UBCF_Model %>% avg()
IBCF_Model%>% avg()

#making the models more precise
tune_models<- function(schema, parameters){
  UBCF_Model <- evaluate(training_schema,method = "UBCF", type ="topNList", n= 5, param = list(nn=parameters)) #uses nearest neighbour
  IBCF_Model <- evaluate(training_schema,method ="IBCF", type = "topNList", n =5, param = list(k =parameters)) #uses k means clustering
  
  UBCF_Model%>% avg() %>% as.tibble() %>% mutate(model ='UBCF')%>% 
    rbind()
  IBCF_Model%>% avg() %>% as.tibble() %>% mutate(model ='IBCF')%>% 
    return ()
  
}

#searching for the best nearest neighbor 
tune_grid = tibble(parameters = c(3,10,15,20,25))

tune_models(training_schema, parameters = c(10))

history = tune_grid%>%
  mutate(results = map(parameters,~tune_models(training_schema,param = .x)))%>%
  unnest()

#use 5 nearest neighbors 
history%>%
  ggplot(aes(x=parameters,y =TPR, fill = model,label = parameters)) +geom_col(position = "dodge") + geom_text(aes(x=parameters,y=TPR))


#creating the final model 
UBCF_Final_Model <- Recommender(getData(training_schema,'train'),"UBCF",param = list(nn=5))
UBCF_Final_Model

#making the predictions and calculating prediction accuracy 
predictions = predict(UBCF_Final_Model,getData(training_schema,"known"),type = "topNList")
calcPredictionAccuracy(predictions,getData(training_schema,"unknown"),given = 1)

rec_engine = Recommender(rap_matrix,"UBCF",param=list(nn=5))
rec_engine

polls %>%
  dplyr::filter(str_detect(artist,"Kanye"))%>%
  distinct()
           
#creating my top 5 list 
mySongs = polls%>%
  select(title)%>%
  distinct()%>%
  arrange(title)%>%
  dplyr::filter(title %in% c("All of the lights","Alright","m.A.A.d city","Changes"))%>%
  rbind(polls%>% select(title) %>% distinct())%>%
  count(title)%>%
  mutate(n=n-1)%>%
  pivot_wider(names_from = "title",values_from ="n",values_fill = list(n=0))%>%
  as.matrix()%>%
  as("binaryRatingMatrix")


#testing the model by writing input as mySongs 
predict(rec_engine,mySongs)%>% as("list") %>% as.data.frame()
  

