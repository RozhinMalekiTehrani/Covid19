                                            #Rozhin Maleki Tehrani
                                            # Final project 


library(dplyr)
library(ggplot2)
library(VIM)
library(Rtsne)
library(kohonen)
library(tidyr)








data = read.csv(file.choose("owid-covid-data")) 
colnames(data)
head(data)
View(data)
str(data)




### checking percent of missing data
missing_percent_column = colSums(is.na(data))/nrow(data)
View(round(missing_percent_column, 2))


######
data1 = select(data, -iso_code, -icu_patients, -icu_patients_per_million,
               #      -hosp_patients,
               - hosp_patients_per_million,
               -weekly_icu_admissions,
               - weekly_icu_admissions_per_million,
               -weekly_hosp_admissions,
               -weekly_hosp_admissions_per_million,
               - excess_mortality,
               -new_vaccinations_smoothed_per_million,
               -people_fully_vaccinated_per_hundred,
               -people_vaccinated_per_hundred,
               -total_vaccinations_per_hundred,
               -new_vaccinations_smoothed,
               -new_vaccinations,
               -people_vaccinated,
               -date)

View(data1)
dim(data1)
colnames(data1)



nums <- unlist(lapply(data1, is.numeric))  
data_numeric = data1[,nums]
dim(data_numeric)
View(data_numeric)
data_replace= data_numeric
colnames(data_replace)








# In cleaned data and numeric data, na must be replaced by mean of each columns:

data_cleaned_num = replace_na(data_replace,as.list(colMeans(data_replace,na.rm=T)))

dim(data_cleaned_num)
View(data_cleaned_num)


#data_cleaned with some categorical columns
data_cleaned = mutate( data_cleaned_num, continent = data$continent , location =  data$location)
colnames(data_cleaned)







#It's time to get some information by plotting 


#________________________

p1= ggplot(data_cleaned,
           aes(x = new_deaths , y = stringency_index,
               size = population_density))+
  geom_point(aes(color = data$continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " new_deaths", y = "stringency_index")

p1


#______________________________________________________



p2 = ggplot(data,
            aes(x = people_vaccinated , y = new_cases,
                size = diabetes_prevalence, colour = population_density))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " people_vaccinated", y = "new_cases")

p2






#___________________________________________________




colnames(data)

p3 = ggplot(data,
            aes(x = total_deaths , y = people_fully_vaccinated,
                size = extreme_poverty, colour = location))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " total_deaths", y = "people_fully_vaccinated")

p3

#____________________________________________________________




p4 = ggplot(data,
            aes(x = people_vaccinated , y = handwashing_facilities,
                size = population_density, colour = location))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " people_vaccinated", y = "handwashing_facilities")

p4

#____________________________________________________________




p5 = ggplot(data,
            aes(x = gdp_per_capita , y = new_cases,
                size = human_development_index, colour = population_density))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " gdp_per_capita", y = "new_cases")

p5




#________________________________________________________________


colnames(data)

p6 = ggplot(data,
            aes(x = gdp_per_capita , y = total_deaths,
                size = human_development_index, colour = continent))+
  geom_point()+
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " gdp_per_capita", y = "total_deaths")


p6

#__________________________________________________________-





colnames(data)

p7 = ggplot(data,
            aes(x = gdp_per_capita , y =life_expectancy ,
                size = human_development_index))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " gdp_per_capita", y = "life_expectancy")

p7






#____________________________________________






colnames(data)

p8 <- ggplot(data,
             aes(x = extreme_poverty , y =life_expectancy ,
                 size = human_development_index))+
  geom_point(aes(color = continent, size = gdp_per_capita), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " extreme_poverty", y = "life_expectancy")

p8

#_______________________________________________________________



colnames(data)

p9 = ggplot(data,
             aes(x = people_vaccinated , y =extreme_poverty ,
                 size = human_development_index))+
  geom_point(aes(color = continent, size = gdp_per_capita), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= "people_vaccinated ", y = "extreme_poverty")

p9

#________________________________


ggplot(data,
       aes(x = positive_rate , y =total_cases ,
           size = extreme_poverty))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= "positive_rate ", y = "total_cases")
#______________________________















p10 <- ggplot(data,
              aes(x = people_vaccinated , y =life_expectancy ))+
  geom_point(aes(color = continent, size = gdp_per_capita), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= "people_vaccinated ", y = "life_expectancy")

p10













#####_________________________________-

 ggplot(data,
              aes(x = weekly_icu_admissions	 , y =life_expectancy ))+
  geom_point(aes(color = continent, size = gdp_per_capita), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= "weekly_icu_admissions	 ", y = "life_expectancy")


#________________

 ggplot(data,
              aes(x = weekly_icu_admissions	 , y =life_expectancy ))+
  geom_point(aes(color = continent, size = gdp_per_capita), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= "weekly_icu_admissions	 ", y = "life_expectancy")
#______________________________________________________________________________














# a good one 
#_________________________________
colnames(data)
p11<- ggplot(data_cleaned,
             aes(x = total_deaths , y = cardiovasc_death_rate,
                 size = male_smokers, colour = continent))+
  geom_point( alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " total_deaths", y = "cardiovasc_death_rate")

p11
#______________________________________












#______________________


colnames(data1)


data2 = select(data, total_cases,  new_cases , total_deaths, icu_patients, hosp_patients, total_tests,people_vaccinated, people_fully_vaccinated, 
               population,
               aged_65_older,aged_70_older,extreme_poverty, female_smokers , male_smokers, hospital_beds_per_thousand, handwashing_facilities)

colnames(data2)







#_______ dataset jadid

data3 = select(data, total_deaths, icu_patients ,people_vaccinated, people_fully_vaccinated
               ,extreme_poverty, female_smokers , male_smokers, aged_65_older, total_cases)


dim(data3)


data_remove = na.omit(data3)

dim(data_remove)


















#__________________________________-
#SOM implementation

colnames(data_cleaned_num)


data_s= select(data_cleaned_num,total_cases,handwashing_facilities, hosp_patients, female_smokers, male_smokers,
               extreme_poverty,stringency_index,total_vaccinations,diabetes_prevalence ,total_deaths,hosp_patients)



data_som = as.matrix(scale(data_s))
data_som
data_grid = somgrid( xdim = 8, ydim =8)
set.seed(2021)
data_somModel= som ( data_som, grid = data_grid )


plot(data_somModel , type ="count")


plot(data_somModel , type = "property" ,
     property = getCodes(data_somModel)[,6] , main = colnames(data_s)[6])


plot(data_somModel,type = "code")

#________________________





















colnames(data)
data4=select(data, "total_deaths")

colnames(data)
library(VIM)
data5= select(data, total_deaths)

data_clean <- kNN(data, k=5)

#View(data_clean)

dim(data5)


#____________________________________________________________--

ggplot(data, 
       aes(x= extreme_poverty ,y= people_vaccinated ,colour = continent )) + geom_point()+
  geom_smooth(method = "lm")












#____________________________________________________________
ggplot(data, 
       aes(x= diabetes_prevalence ,y=total_deaths  ,colour = continent  )) + geom_point()
#geom_smooth(method = "lm")





#_____________________________



g = ggplot(data, aes(x = handwashing_facilities , y = total_cases,colour = continent)) +
  geom_point()+
  theme_bw()

plot(g)



#lolipop
ggplot(data ,aes(x=handwashing_facilities , y=total_cases, colour = continent))+
  geom_point(size=3) +
  geom_segment(aes(x=handwashing_facilities, 
                   xend = handwashing_facilities,
                   y=0,
                   yend=total_cases ))






#histogram

colnames(data)
g= ggplot(data, aes(total_deaths,fill= gdp_per_capita ))+
  geom_histogram()

plot(g)


































# Implementing T-sne 
#_______________________________________

newdata = select(data, total_cases , total_deaths, icu_patients, hosp_patients, total_tests ,people_vaccinated, people_fully_vaccinated, 
                                  aged_65_older,gdp_per_capita ,extreme_poverty, female_smokers , male_smokers, hospital_beds_per_thousand, handwashing_facilities)

dim(newdata)


nums = unlist(lapply(newdata, is.numeric))  
data_numeric = newdata[,nums]
dim(data_numeric)
View(data_numeric)
data_replace= data_numeric
colnames(data_replace)
# In cleaned data and numeric data, na must be replaced by mean of each columns:
DATA_cleaned = replace_na(data_numeric,as.list(colMeans(data_replace,na.rm=T)))
dim(DATA_cleaned)
#DATA_cleaned with some categorical columns
DATA = mutate( DATA_cleaned, continent = data$continent , location =  data$location)
colnames(data_numeric)


colnames(DATA)
Labels<-DATA$continent
DATA$ continent<-as.factor(data$continent)
#plotting
colors = rainbow(length(unique(data$continent)))
names(colors) = unique(data$continent)

uni = unique(data_cleaned_num)

tsne = Rtsne(uni, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
exeTimeTsne = system.time(Rtsne(uni, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

# plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=DATA$continent, col=colors[DATA$continent])



#___________________________________________________________________________-













#lle

library(lle)
X = DATA
head(X)
neighbours = find_nn_k(X, k=1)



#IRAN _ case study

####________________________________________________________________________________

#Iran Vs Germany
#_________________________________________________________




colnames(data_cleaned)
Iran_Germany =data_cleaned[data_cleaned$location == c("Iran" ,"Germany") ,]
colnames(Iran_Germany)
View(Iran_Germany)
colnames(Iran_Germany)

dim(Iran_Germany)


ggplot(Iran_Germany,
       aes(x = stringency_index , y = new_cases,
           size = extreme_poverty, colour = location()))+
  geom_point(aes(color = location), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " stringency_index", y = "new_cases", title =  "Iran's data analysis")









colnames(Iran_data)

#_________________________________________________


 ggplot(Iran_Germany,
            aes(x = total_vaccinations , y = new_cases,
                size = diabetes_prevalence, colour = population_density))+
  geom_point(aes(color = location), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " total_vaccinations", y = "new_cases", title= "Iran's data analysis")

q1




#__________________________________________________________-


colnames(data)

q2 = ggplot(Iran_Germany,
            aes(x = people_vaccinated , y = new_cases,
                size = human_development_index, colour = population_density))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " people_vaccinated", y = "new_cases", title= "Iran's data analysis")

q2




#__________________________________________________________________________________________



Q3=ggplot(Iran_Germany,
       aes(x = total_vaccinations , y = handwashing_facilities,
           size = population_density, colour = location))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " people_vaccinated", y = "handwashing_facilities", title =  "Iran's data analyrsis")

Q3



















#_______________________--


ggplot(Iran_Germany,
       aes(x = gdp_per_capita , y =hosp_patients ,
           size = extreme_poverty))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= "positive_rate ", y = "total_cases")
#_____________________________

colnames(data)

p7 = ggplot(Iran_Germany,
            aes(x = gdp_per_capita , y =life_expectancy ,
                size = human_development_index))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " gdp_per_capita", y = "life_expectancy")

p7























#_________________________________________________________



colnames(data_cleaned)
Iran_data =data_cleaned[data_cleaned$location == c("Iran" ,"Germany") ,]
colnames(Iran_data)
View(Iran_data)
colnames(Iran_data)

dim(Iran_data)


ggplot(Iran_data,
       aes(x = stringency_index , y = new_cases,
           size = extreme_poverty, colour = continent))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " stringency_index", y = "new_cases", title =  "Iran's data analysis")









colnames(Iran_data)

#_________________________________________________


q1 = ggplot(data[data_cleaned$location == "Iran" ,],
            aes(x = people_vaccinated , y = new_cases,
                size = diabetes_prevalence, colour = population_density))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " people_vaccinated", y = "new_cases", title= "Iran's data analysis")

q1




#__________________________________________________________-


colnames(data)

q2 = ggplot(data[data_cleaned$location == "Iran" ,],
            aes(x = people_vaccinated , y = new_cases,
                size = human_development_index, colour = population_density))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " people_vaccinated", y = "new_cases", title= "Iran's data analysis")

q2




#__________________________________________________________________________________________



Q3=ggplot(Iran_data,
          aes(x = total_vaccinations , y = handwashing_facilities,
              size = population_density, colour = location))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " people_vaccinated", y = "handwashing_facilities", title =  "Iran's data analyrsis")

Q3



















#_______________________--


ggplot(Iran_data,
       aes(x = gdp_per_capita , y =hosp_patients ,
           size = extreme_poverty))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= "positive_rate ", y = "total_cases")
#_____________________________

colnames(data)

p7 = ggplot(Iran_data,
            aes(x = gdp_per_capita , y =life_expectancy ,
                size = human_development_index))+
  geom_point(aes(color = continent), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " gdp_per_capita", y = "life_expectancy")

p7


#________________________________________


#### Iran vs Afghanistan


Iran_Afgh =data_cleaned[data_cleaned$location == c("Iran" ,"Afghanistan") ,]



colnames(data)


ggplot(Iran_Afgh,
       aes(x = diabetes_prevalence , y = positive_rate,
           size = extreme_poverty, colour = location))+
  geom_point(aes(color = location), alpha = 0.7) +
  scale_color_viridis_d() +
  scale_x_log10() +
  labs(x= " diabetes_prevalence", y = "positive_rate", title =  "Iran's data analysis")


