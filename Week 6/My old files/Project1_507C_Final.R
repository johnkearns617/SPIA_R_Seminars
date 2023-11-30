# Project1_507C_Kearns_GuggenmosEdits.R
# John Kearns (Edits by Greg Guggenmos)
# Date Created: 2022-11-19
# Last Updated: 2022-11-30
# Goal: Complete exercises recommended and extra analysis needed to inform Marlo G. Ball on 
#       immigration attitudes in Switzerland

# set variables
master_dir = "C:/Users/16094/Documents/Year 1 - Princeton/Fall 2022/Quant Analysis/Project/"
do_folder = "C:/Users/16094/Documents/Year 1 - Princeton/Fall 2022/Quant Analysis/Project/Do/"
data_folder = "C:/Users/16094/Documents/Year 1 - Princeton/Fall 2022/Quant Analysis/Project/Data/"
results_folder = "C:/Users/16094/Documents/Year 1 - Princeton/Fall 2022/Quant Analysis/Project/Results/"
charts_folder = "C:/Users/16094/Documents/Year 1 - Princeton/Fall 2022/Quant Analysis/Project/Charts/"

# load packages
# install.packages("tidyverse")
# install.packages("estimatr")
# install.packages("pastecs")
# install.packages("texreg")
# install.packages("reactablefmtr")
# install.packages("plotrix")
#install.packages("logr")
#install.packages("reprex")

library(tidyverse)
library(estimatr)
library(pastecs)
library(texreg)
library(reactablefmtr)
library(plotrix)
library(logr)
library(reprex)

# load data
data = haven::read_dta(paste0(data_folder,"swiss_passports.dta"))

# log using "swiss_passports.smcl", replace

# 1. Construct a table of summary statistics for the variables in the data set. 
# summarize

# a.  percent_novotes is the outcome variable of interest. What is its average value? What is its standard deviation? Applications are approved if the fraction of no votes is less than 50 percent. What fraction of citizenship applications were approved? 

summary(data$percent_novotes)

data$percent_approved = ifelse(data$percent_novotes<50,1,0)
sd(data$percent_novotes)

summary(data$percent_approved)


#The average percentage of no votes is 40.4%. The standard deviation is 14.8%. The fraction of citizenship application that were approved is 68.5%. 

# b. The various land_ variables are the explanatory variables of interest. What fraction of applications were from Turkey and Yugoslavia? From Central and Eastern Europe? etc. How did this fraction vary over the 1970-2003 sample period? 
mean(data$land_ty==1|data$land_ceeu==1)
data = data %>% 
  mutate(nationality=case_when(
    land_ty==1~"Turkey",
    land_ceeu==1~"Yugoslavia",
    land_asia==1~"Asia",
    land_seu==1~"Southern Europe",
    land_neu_poor==1~"NonEuropean Poor",
    land_other==1~"Other"
  ),
  decade=case_when(
    year80==1~"1980",
    year90==1~"1990",
    year00==1~"2000",
    year80==0&year90==0&year00==0~"1970"
  )) 

print(data %>% 
        group_by(nationality) %>% 
        summarize(Share=n()/nrow(data)))

print(data %>% 
        group_by(decade,nationality) %>% 
        summarize(Share=n()/nrow(data[data$decade==decade,])) %>% 
        ungroup() %>% 
        pivot_wider(names_from=nationality,values_from=Share))

# Overall, 49.7% are from Turkey, 6.3% from Yugoslavia, 16.6% from Southern Europe, 7.3% from Asia, 2.1% from non-European poor countries, 17.8% from other places.

# In the 70s, almost no immigrants from Turkey. Most were coming from Southern Europe and "other places". Turkish and Yugoslavian immigration begins to pick up in the 80s. Turkish immigration becomes the majority in the 90s and remained that way in the 2000s.

# There were 1,397 applications in this dataset with location data. 129 in 1970s, 227 in 80s, 503 in 90s, 538 in 2000s. 

# Are there other noteworthy features of the data?

summary(data)

# Most immigrants are male. Lots of the language information is missing.

# 2. a. Construct table showing land_ and columns are number of obs, mean of percent_novotes, sd, standard error, and 95% confidence interval for the mean


print(data %>% 
        group_by(nationality) %>% 
        summarize(num_obs = n(),
                  mean = mean(percent_novotes),
                  st_dev = sd(percent_novotes),
                  st_error = plotrix::std.error(percent_novotes),
                  ci95_low = mean-1.96*st_error,
                  ci95_high = mean+1.96*st_error))
# from this, we can see that Southern Europeans get the best votes while Turkey gets the worst.
# the Non-European poor have the highest variation in their acceptance.
# Southern Europe and Other are much more accepted than Asia, Turkey, YugoSlavia, and NonEuropean

mod1 = lm_robust(percent_novotes~land_ty+land_asia+land_ceeu+land_seu+land_neu_poor,data=data,se_type="stata")
# land_other is excluded so that there is no perfect collinearity

# from this we see how the significance of these differences from the mean varies by source country. 
# Turkey has the largest magnitude, while Asia and Yugoslavia are around the same, relative to the "other" countries.
# Southern Europe get the best outcomes in votes, though the difference be SEU and "other" is not statistically significant.

summary(mod1)

# 3. Add aditional controls to regression
data = data %>% 
  mutate(integration_integrated=ifelse(integration_integrated==-1,NA,integration_integrated),
         integration_assimilated=ifelse(integration_assimilated==-1,NA,integration_assimilated))

mod2 = lm_robust(percent_novotes~land_ty+land_asia+land_ceeu+land_seu+land_neu_poor+skill_hi+skill_middle,data=data,se_type="stata")
# b. Skill_ might differ across land_ variables because the educational systems are different across countries
# Also, Swiss people will understand the skills of countries more different than European countries.
# There is an argument for interaction terms, 
# but at the very least high skilled people are more likely to be accepted because they can bring more to society.

summary(mod2)

# Adding the skill_ variables increases the adjusted R2 by only a little bit
# However, the land_asia and land_ceeu coefficient change by a significant amount
# This would indicate that some of the acceptance these groups get is due to their skill levels

# c. male, kids, married, born_ch, age, and education all may be helpful
# male controls for some immigrant flows that may be older, more male, skewed towards family, have closer ties to Switzerland, etc.
mod3 = lm_robust(percent_novotes~land_ty+land_asia+land_ceeu+land_seu+land_neu_poor+year80+year90+year00+male+kids+married+born_ch+age60+age60plus+educ_high+skill_hi,data=data,se_type="stata")

summary(mod3)

# coefficient on land_ty goes from 18.09 to 16.33
# coefficient on land_asia goes from 8.97 to 6.83
# coefficient on land_ceeu goes from 9.26 to 8.22
# coefficient on land_seu goes from -1.27 to -1.46
# coefficient on land_neu_poor goes from 6.97 to 5.44

# d. 1. How many observations on the language variable
sum(data$lang_NA==0)
# 551 out of 1397

sum(data$integration_NA==0)
# 851 out of 1397

# d. 2. Do the conclusions I reach in part c change if I restrict the samples?
mod4 = lm_robust(percent_novotes~land_ty+land_asia+land_ceeu+land_seu+land_neu_poor+year80+year90+year00+male+kids+married+born_ch+age60+age60plus+educ_high+skill_hi,data=data %>% filter(lang_NA==0),se_type="stata")
mod5 = lm_robust(percent_novotes~land_ty+land_asia+land_ceeu+land_seu+land_neu_poor+year80+year90+year00+male+kids+married+born_ch+age60+age60plus+educ_high+skill_hi,data=data %>% filter(integration_NA==0),se_type="stata")

summary(mod4)
# the magnitudes are lower for those where language is available, but especially for those from Yugoslavia

summary(mod5)
# in this subset, the neu_poor coefficient doubles in size, but the rest are essentially unchanged

# d.3. add language variables as additional controls
data = data %>% 
  mutate_at(vars(lang_perfect:lang_insufficient),funs(ifelse(.==-1,NA,.)))

mod6 = lm_robust(percent_novotes~land_ty+land_asia+land_ceeu+land_seu+land_neu_poor+year80+year90+year00+male+kids+married+born_ch+age60+age60plus+educ_high+skill_hi+lang_good+lang_perfect,data=data %>% filter(integration_NA==0),se_type="stata")
summary(mod6)

# the poor outcomes for NEU Poor immigrants becomes even more clear. The coefficient on the others except Yugoslavia are largely unchanged.
# R2 is now 0.53
# the language variables are significant and are consistent with my priors.
# Immigrants are more accepted if they speak the language better.

# d.4. Add the integration variables
data = data %>% 
  mutate_at(vars(integration_integrated:integration_nodifference),funs(ifelse(.==-1,NA,.)))

View(filter(data, integration_assimilated == 1.5))
mod7 = lm_robust(percent_novotes~land_ty+land_asia+
                   land_ceeu+land_seu+land_neu_poor+year80+
                   year90+year00+male+kids+married+born_ch+
                   age60+age60plus+educ_high+skill_hi+
                   lang_good+lang_perfect+factor(integration_integrated)+
                   factor(integration_assimilated)+integration_adjusted+
                   integration_nodifference,data=data %>% filter(integration_NA==0),se_type="stata")
summary(mod7)
# I think integration assimilated might have a few errors, will be worth cleaning up

# Coefficients for turkey and NEU get larger. 
# Coefficients on the integration variables are almost all significant with negative coefficients
# This fits with my intuition that as integration and assimilation improve, people will be more willing to accept them

# 4. Do the land_ coefficients depend on occupation skills or educational level
mod8 = lm_robust(percent_novotes~land_ty+land_asia+
                   land_ceeu+land_seu+land_neu_poor+
                   educ_high+educ_middle+skill_middle+skill_hi,data=data %>% filter(integration_NA==0),se_type="stata")

summary(mod8)
# it does appear that these variables are important

mod9 = lm_robust(percent_novotes~land_ty*educ_high+land_ty*educ_middle+land_ty*skill_middle+land_ty*skill_hi+
                   land_asia*educ_high+land_asia*educ_middle+land_asia*skill_middle+land_asia*skill_hi+
                   land_ceeu*educ_high+land_ceeu*educ_middle+land_ceeu*skill_middle+land_ceeu*skill_hi+
                   land_seu*educ_high+land_seu*educ_middle+land_seu*skill_middle+land_seu*skill_hi+
                   land_neu_poor*educ_high+land_neu_poor*educ_middle+land_neu_poor*skill_middle+land_neu_poor*skill_hi+
                   educ_high+educ_middle+skill_middle+skill_hi,data=data %>% filter(integration_NA==0),se_type="stata")

summary(mod9)
# there are significant interaction effects for Asians and those with middle education or high skilled occupations
# Same for SEU and high education
# High education and NEU
# Turkey for all the education and skill variables
# the education and skill variables has less interaction effects for Yugoslavia and SEU
# the education and skill variables get positive coefficients now, so all effect is subsumed in the interaction terms

# 5. The Swiss are highly biased against those who are less similar to them.
# This includes Turkish people and those from NEU poor countries. They favor immigrants from Yugoslavia and SEU.


# Look at interaction terms over time
mod10 = lm_robust(percent_novotes~land_ty*decade+
                    land_ceeu*decade+
                    #land_seu*decade+
                    land_asia*decade+
                    land_neu_poor*decade+
                    land_other*decade,data=data,se_type="stata")


time_fe_results = tidy(mod10)
time_fe_fig_data = data.frame()
for(country in c("ty","ceeu","other","asia","poor")){
  
  decades=c(1970,1980,1990,2000)
  coeffs = time_fe_results$estimate[grep(country,time_fe_results$term)]
  
  time_fe_fig_data = bind_rows(time_fe_fig_data,
                               data.frame(decade=decades,
                                          coef=coeffs,
                                          origin=country))
  
}

time_fe_fig_data = time_fe_fig_data %>% 
  mutate(origin=case_when(
    origin=="ty"~"Turkey",
    origin=="ceeu"~"Yugoslavia",
    origin=="other"~"Other",
    origin=="asia"~"Asia",
    origin=="poor"~"NonEuropean Poor"
  )) %>% 
  rowwise() %>% 
  mutate(num_migrants=100*nrow(data[data$nationality==origin&data$decade==decade,])/nrow(data[data$decade==decade,])) %>% 
  ungroup()

ggplot(time_fe_fig_data,aes(x=decade,y=coef,color=origin)) +
  geom_line(size=1) +
  labs(x="Decade",y="Difference in % No Votes\n(Relative to southern European migrants)") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.title.x = element_blank())

ggplot(time_fe_fig_data,aes(x=decade)) +
  facet_wrap(~origin,scales="free_y") +
  geom_line(aes(y=coef,color="Difference in % No Votes\nRelative to southern Europeans"))+
  geom_line(aes(y=num_migrants,color="Share of Migrants in Decade")) +
  labs(y="Percentage") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.title.x = element_blank())

mod11 = lm_robust(percent_novotes~land_ty*decade+
                    land_ceeu*decade+
                    #land_seu*decade+
                    land_asia*decade+
                    land_neu_poor*decade+
                    land_other*decade+
                    age40+age60+age60plus+
                    male+kids+educ_middle+educ_high+
                    married+skill_hi+skill_middle+born_ch,data=data,se_type="stata")

summary(mod11)

time_fe_results = tidy(mod11)
time_fe_fig_data = data.frame()
for(country in c("ty","ceeu","other","asia","poor")){
  
  decades=c(1970,1980,1990,2000)
  coeffs = time_fe_results$estimate[grep(country,time_fe_results$term)]
  
  time_fe_fig_data = bind_rows(time_fe_fig_data,
                               data.frame(decade=decades,
                                          coef=coeffs,
                                          origin=country))
  
}

time_fe_fig_data = time_fe_fig_data %>% 
  rowwise() %>% 
  mutate(coef=coef+time_fe_results$estimate[grep(paste0(origin),time_fe_results$term)][1]+time_fe_results$estimate[grep(paste0("decade",decade),time_fe_results$term)][1]) %>% 
  ungroup() %>% 
  mutate(origin=case_when(
    origin=="ty"~"Turkey",
    origin=="ceeu"~"Yugoslavia",
    origin=="other"~"Other",
    origin=="asia"~"Asia",
    origin=="poor"~"NonEuropean Poor"
  )) %>% 
  rowwise() %>% 
  mutate(num_migrants=100*nrow(data[data$nationality==origin&data$decade==decade,])/nrow(data[data$decade==decade,])) %>% 
  ungroup()

ggplot(time_fe_fig_data,aes(x=decade,y=coef,color=origin)) +
  geom_line(size=1) +
  labs(x="Decade",y="Difference in % No Votes\n(Relative to southern European migrants)") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.title.x = element_blank())

ggplot(time_fe_fig_data,aes(x=decade)) +
  facet_wrap(~origin,scales="free_y") +
  geom_line(aes(y=coef,color="Difference in % No Votes\nRelative to southern Europeans"))+
  geom_line(aes(y=num_migrants,color="Share of Migrants in Decade")) +
  labs(y="Percentage") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.title.x = element_blank())


mod12 = lm_robust(percent_novotes~land_ty*decade+
                    land_ceeu*decade+
                    #land_seu*decade+
                    land_asia*decade+
                    land_neu_poor*decade+
                    land_other*decade+
                    age40+age60+age60plus+
                    male+kids+educ_middle+educ_high+
                    married+skill_hi+skill_middle+born_ch+
                    lang_good+lang_perfect+factor(integration_integrated)+
                    factor(integration_assimilated)+integration_adjusted+
                    integration_nodifference,data=data,se_type="stata")

summary(mod12)

time_fe_results = tidy(mod12)
time_fe_fig_data = data.frame()
for(country in c("ty","ceeu","other","asia","poor")){
  
  decades=c(1970,1980,1990,2000)
  coeffs = time_fe_results$estimate[grep(country,time_fe_results$term)]
  
  time_fe_fig_data = bind_rows(time_fe_fig_data,
                               data.frame(decade=decades,
                                          coef=coeffs,
                                          origin=country))
  
}

time_fe_fig_data = time_fe_fig_data %>% 
  mutate(origin=case_when(
    origin=="ty"~"Turkey",
    origin=="ceeu"~"Yugoslavia",
    origin=="other"~"Other",
    origin=="asia"~"Asia",
    origin=="poor"~"NonEuropean Poor"
  )) %>% 
  rowwise() %>% 
  mutate(num_migrants=100*nrow(data[data$nationality==origin&data$decade==decade,])/nrow(data[data$decade==decade,])) %>% 
  ungroup()

ggplot(time_fe_fig_data,aes(x=decade,y=coef,color=origin)) +
  geom_line(size=1) +
  labs(x="Decade",y="Difference in % No Votes\n(Relative to southern European migrants)") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.title.x = element_blank())

ggplot(time_fe_fig_data,aes(x=decade)) +
  facet_wrap(~origin,scales="free_y") +
  geom_line(aes(y=coef,color="Difference in % No Votes\nRelative to southern Europeans"))+
  geom_line(aes(y=num_migrants,color="Share of Migrants in Decade")) +
  labs(y="Percentage") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.title.x = element_blank())

### Are there any obvious demographic differences
demog_diffs = data %>% 
  group_by(nationality) %>% 
  summarize_at(vars(year80,year90,year00,
                    age40,age60,age60plus,
                    male,kids,educ_middle,educ_high,
                    married,lang_perfect,lang_good,lang_insufficient,lang_NA,
                    integration_NA,integration_integrated,integration_assimilated,integration_adjusted,integration_nodifference,
                    skill_hi,skill_middle,born_ch),mean,na.rm=TRUE) %>% 
  ungroup() %>% 
  pivot_longer(cols=year80:born_ch,names_to = "variable",values_to = "mean") %>% 
  pivot_wider(names_from=nationality,values_from=mean)

# add changes in migration flow share as a control
data = data %>% 
  rowwise() %>% 
  mutate(num_migrants=100*nrow(data[data$nationality==nationality&data$decade==decade,])/nrow(data)) %>% 
  ungroup()

summary(lm_robust(percent_novotes~land_ty*decade+
                    land_ceeu*decade+
                    #land_seu*decade+
                    land_asia*decade+
                    land_neu_poor*decade+
                    land_other*decade+
                    age40+age60+age60plus+
                    male+kids+educ_middle+educ_high+
                    married+skill_hi+skill_middle+born_ch,data=data,se_type="stata"))

# within turkey, what is a good explainer?
summary(lm_robust(percent_novotes~num_migrants+
                    age40+age60+age60plus+
                    male+kids+educ_middle+educ_high+
                    married+skill_hi+skill_middle+born_ch,data=data %>% filter(nationality=="Turkey"),se_type="stata"))



# Logit model
data_logit = data
table(data_logit$percent_approved)
table(data_logit$nationality,data_logit$percent_approved)

data_logit <- filter(data_logit, integration_assimilated!=1.5)

set.seed(1)
options(scipen=999)

sample <- sample(c(TRUE, FALSE), nrow(data_logit), replace=TRUE, prob=c(0.9,0.1))
train <- data_logit[sample, ]
test <- data_logit[!sample, ]  
mod20 <- glm(percent_approved~land_ty+land_asia+
               land_ceeu+land_seu+land_neu_poor+
               educ_high+educ_middle+skill_middle+skill_hi, family="binomial", data=train)

summary(mod20)
exp(coef(mod20))


mod21 <- glm(percent_approved~land_ty+land_asia+land_ceeu+land_seu+land_neu_poor+year80+year90+year00+male+kids+married+born_ch+age60+educ_high+skill_hi+lang_good+lang_perfect, family="binomial", data=train)

summary(mod21)
exp(coef(mod21))


mod22 <- glm(percent_approved~land_ty+land_asia+
               land_ceeu+land_seu+land_neu_poor+year80+
               year90+year00+male+kids+married+born_ch+
               age60+age60plus+educ_high+skill_hi+
               lang_good+lang_perfect+factor(integration_integrated)+
               factor(integration_assimilated)+integration_adjusted+
               integration_nodifference, family="binomial", data=train)

summary(mod22)
exp(coef(mod22))

# Adding ANOVA analysis for Model 7 
mod71 = lm(percent_novotes~land_ty+land_asia+
                   land_ceeu+land_seu+land_neu_poor+year80+
                   year90+year00+male+kids+married+born_ch+
                   age60+age60plus+educ_high+skill_hi+
                   lang_good+lang_perfect+factor(integration_integrated)+
                   factor(integration_assimilated)+integration_adjusted+
                   integration_nodifference,data=data %>% filter(integration_NA==0))
summary(mod71)
anova(mod71)


#### Plan for memo ####
# 1. Brief history of immigration to Switzerland
#    Show graph of applications over time broken down by country
fig1_data = data %>% 
  group_by(decade,nationality) %>% 
  summarize(num=n()) %>% 
  ungroup() %>% 
  rename(origin=nationality)

fig1 = ggplot(fig1_data,aes(x=decade,y=num,fill=origin,group=origin)) +
  geom_area() +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.title.x = element_blank()) +
  labs(x="Decade",y="Number of immigrant applications",caption="Source: Hainmueller and Hangartner")

ggsave(paste0(charts_folder,"figure1.png"),fig1,width=10,height=5,units="in",dpi=320)

# 2. How do immigrants differ?
table1_data = data %>% 
  mutate(percent_novotes=percent_novotes/100) %>% 
  group_by(nationality) %>% 
  summarize_at(vars(percent_novotes,percent_approved,age40,age60,age60plus,
                    male,kids,educ_middle,educ_high,
                    married,lang_perfect,lang_good,lang_insufficient,lang_NA,
                    integration_NA,integration_adjusted,integration_nodifference,
                    skill_hi,skill_middle,born_ch),mean,na.rm=TRUE) %>% 
  ungroup() %>% 
  pivot_longer(cols=percent_novotes:born_ch,names_to = "variable",values_to = "mean") %>% 
  pivot_wider(names_from=nationality,values_from=mean) %>% 
  mutate(variable=case_when(
    variable=="percent_novotes"~"Average %No votes received",
    variable=="percent_approved"~"Percent approved",
    variable=="age40"~"Percent between age 20 and 40",
    variable=="age60"~"Percent between age 40 and 60",
    variable=="age60plus"~"Percent above age 60",
    variable=="male"~"Percent male",
    variable=="kids"~"Percent with children",
    variable=="educ_middle"~"Percent with high-school education",
    variable=="educ_high"~"Percent with college education",
    variable=="married"~"Percent married",
    variable=="lang_perfect"~"Percent language proficiency close to perfect",
    variable=="lang_good"~"Percent language proficiency is good",
    variable=="lang_insufficient"~"Percent language proficiency insufficient",
    variable=="lang_NA"~"Percent language data missing",
    variable=="integration_NA"~"Percent integration data missing",
    variable=="integration_adjusted"~"Percent that have adjusted to Swiss culture",
    variable=="integration_nodifference"~"Percent who are well assimilated",
    variable=="skill_hi"~"Percent in high-skilled occupation",
    variable=="skill_middle"~"Percent in medium-skilled occupation",
    variable=="born_ch"~"Percent born in Switzerland"
  ))

library(purrr)
table1_data1 <-  table1_data %>%
  mutate(row_num=row_number())
table1_data1 = table1_data1 %>% 
  rowwise() %>% 
  mutate(min_val=which.min(table1_data1[table1_data1$row_num==row_num,2:7]),
         max_val=which.max(table1_data1[table1_data1$row_num==row_num,2:7])) %>% 
  ungroup() %>% 
  mutate_all(as.character)
for(i in 1:nrow(table1_data1)){
  
  table1_data1[i,as.numeric(table1_data1$min_val[i])+1] = "#00A7E1"
  table1_data1[i,as.numeric(table1_data1$max_val[i])+1] = "#FFA630"
  table1_data1[i,-c(1,8,9,10,as.numeric(table1_data1$min_val[i])+1,as.numeric(table1_data1$max_val[i])+1)] = "#FFFFFF"
  
  
}


table1 = reactable(table1_data,
                   defaultPageSize = 200,
                   theme=fivethirtyeight(),
                   columns = list(variable=colDef(name="Variable",align = "center",format=colFormat(percent=TRUE,digits=2)),
                                  Asia=colDef(name="Asia",align = "center",format=colFormat(percent=TRUE,digits=2),cell=color_tiles(table1_data1,color_ref="Asia",number_fmt=scales::percent)),
                                  `NonEuropean Poor`=colDef(name="Non-European Developing",align = "center",format=colFormat(percent=TRUE,digits=2),cell=color_tiles(table1_data1,color_ref="NonEuropean Poor",number_fmt=scales::percent)),
                                  Other=colDef(name="Other regions",align = "center",format=colFormat(percent=TRUE,digits=2),cell=color_tiles(table1_data1,color_ref="Other",number_fmt=scales::percent)),
                                  `Southern Europe`=colDef(name="Southern Europe",align = "center",format=colFormat(percent=TRUE,digits=2),cell=color_tiles(table1_data1,color_ref="Southern Europe",number_fmt=scales::percent)),
                                  Turkey=colDef(name="Turkey",align = "center",format=colFormat(percent=TRUE,digits=2),cell=color_tiles(table1_data1,color_ref="Turkey",number_fmt=scales::percent)),
                                  Yugoslavia=colDef(name="Yugoslavia",align = "center",format=colFormat(percent=TRUE,digits=2),cell=color_tiles(table1_data1,color_ref="Yugoslavia",number_fmt=scales::percent))))

data = data %>% mutate(percent_approved=percent_approved*100)
mod1 = lm_robust(percent_approved~land_ty+land_asia+land_ceeu+land_other+land_neu_poor,data=data,se_type="stata")
mod2 = lm_robust(percent_approved~land_ty+land_asia+land_ceeu+land_other+land_neu_poor+
                   year80+year90+year00+age40+age60+age60plus+
                   male+kids+married+born_ch,data=data,se_type="stata")
mod3 = lm_robust(percent_approved~land_ty+land_asia+land_ceeu+land_other+land_neu_poor+
                   educ_middle+educ_high+skill_hi+skill_middle,data=data,se_type="stata")
mod4 = lm_robust(percent_approved~land_ty+land_asia+land_ceeu+land_other+land_neu_poor+
                   lang_perfect+lang_good+lang_insufficient,data=data,se_type="stata")
mod5 = lm_robust(percent_approved~land_ty+land_asia+land_ceeu+land_other+land_neu_poor+
                   factor(integration_integrated)+factor(integration_assimilated)+
                   integration_adjusted+integration_nodifference,data=data,se_type="stata")
mod6 = lm_robust(percent_approved~land_ty+land_asia+land_ceeu+land_other+land_neu_poor+
                   year80+year90+year00+age40+age60+age60plus+
                   male+kids+married+born_ch+
                   educ_middle+educ_high+skill_hi+skill_middle+
                   lang_perfect+lang_good+lang_insufficient+
                   factor(integration_integrated)+factor(integration_assimilated)+
                   integration_adjusted+integration_nodifference,data=data,se_type="stata")
mod7 = lm_robust(percent_approved~factor(nationality)*factor(decade),data=data,se_type="stata")
mod8 = lm_robust(percent_approved~factor(nationality)*factor(decade)+
                   age40+age60+age60plus+
                   male+kids+married+born_ch+
                   educ_middle+educ_high+skill_hi+skill_middle,data=data,se_type="stata")
mod9 = lm_robust(percent_approved~land_ty*factor(decade)+
                   land_ty*age40+land_ty*age60+land_ty*age60plus+
                   land_ty*male+land_ty*kids+land_ty*married+land_ty*born_ch+
                   land_ty*educ_middle+land_ty*educ_high+land_ty*skill_hi+land_ty*skill_middle,data=data,se_type="stata")
htmlreg(list(mod1,mod2,mod3,
             mod4,mod5,mod6,mod7,mod8,mod9),file=paste0(results_folder,"regressions.html"),include.ci=FALSE)


mod12 = lm_robust(percent_approved~land_ty*decade+
                    land_ceeu*decade+
                    #land_seu*decade+
                    land_asia*decade+
                    land_neu_poor*decade+
                    land_other*decade+
                    age40+age60+age60plus+
                    male+kids+educ_middle+educ_high+
                    married+skill_hi+skill_middle+born_ch,data=data,se_type="stata")

summary(mod12)

time_fe_results = tidy(mod12)
time_fe_fig_data = data.frame()
for(country in c("ty","ceeu","other","asia","poor")){
  
  decades=c(1970,1980,1990,2000)
  coeffs = time_fe_results$estimate[grep(country,time_fe_results$term)]
  
  time_fe_fig_data = bind_rows(time_fe_fig_data,
                               data.frame(decade=decades,
                                          coef=coeffs,
                                          origin=country))
  
}

time_fe_fig_data = time_fe_fig_data %>% 
  mutate(origin=case_when(
    origin=="ty"~"Turkey",
    origin=="ceeu"~"Yugoslavia",
    origin=="other"~"Other",
    origin=="asia"~"Asia",
    origin=="poor"~"NonEuropean Poor"
  )) %>% 
  rowwise() %>% 
  mutate(num_migrants=100*nrow(data[data$nationality==origin&data$decade==decade,])/nrow(data[data$decade==decade,])) %>% 
  ungroup()


time_fe_results = tidy(mod9)
coeffs = time_fe_results$estimate[20:30]
bar_data = data.frame(term=gsub("land_ty:","",time_fe_results$term[20:30]),coeff=time_fe_results$estimate[20:30],pval=time_fe_results$p.value[20:30])

fig2 = ggplot(bar_data,aes(x=term,y=coeff,fill=(pval<=0.05)))+
  geom_bar(stat="identity") +
  scale_fill_manual(labels=c(FALSE,TRUE),values=c("red","blue")) +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.title.x = element_blank()) +
  labs(y="Difference in approval probability relative to all other\nimmigrants (percentage points)")

ggsave(paste0(charts_folder,"figure2.png"),fig2,width=10,height=5,units="in",dpi=320)

time_fe_results = tidy(mod9)
bar_data = data.frame(term=gsub("land_ty:","",time_fe_results$term[17:19]),coeff=time_fe_results$estimate[17:19],pval=time_fe_results$p.value[17:19])

fig3 = ggplot(bar_data,aes(x=term,y=coeff,fill=(pval<=0.05)))+
  geom_bar(stat="identity") +
  scale_fill_manual(labels=c(FALSE,TRUE),values=c("red","blue")) +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.title.x = element_blank()) +
  labs(y="Difference in approval probability relative to all other\nimmigrants (percentage points)")

ggsave(paste0(charts_folder,"figure3.png"),fig3,width=10,height=5,units="in",dpi=320)




ggplot(time_fe_fig_data,aes(x=decade,y=coef,color=origin)) +
  geom_line(size=1) +
  labs(x="Decade",y="Difference in % No Votes\n(Relative to southern European migrants)") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.title.x = element_blank())

ggplot(time_fe_fig_data,aes(x=decade)) +
  facet_wrap(~origin,scales="free_y") +
  geom_line(aes(y=coef,color="Difference in % No Votes\nRelative to southern Europeans"))+
  geom_line(aes(y=num_migrants,color="Share of Migrants in Decade")) +
  labs(y="Percentage") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.title.x = element_blank())

# log close