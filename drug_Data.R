library(tidyverse)
mdata <- read_csv("drugData.csv")

# make it a categorical variable
mdata$Group <- as.factor(mdata$Group)

psych::describeBy(x=mdata$Arousal,group=mdata$Group)

# group data by that categorical variable
mdata_grouped <- group_by(mdata, Group)

# gives results for that categorical variable
mdata_grouped %>% summarise(M=mean(Arousal, na.rm=TRUE),SD=sd(Arousal,na.rm=TRUE))

# use this instead of library(car) because it's got beef with tidyverse
# run levene's test
car::leveneTest(mdata$Arousal, group=mdata$Group,center="median")

# it was non sig, so variances were equal hellz yeah


# take the original data and break those groups out

# filter gives me a subset of the rows
exp.group.rows <- mdata %>% filter(Group==0)
control.group.rows <- mdata %>% filter(Group==1)

 
t.test(x=exp.group.rows$Arousal,y=control.group.rows$Arousal,var.equal=TRUE)


# select gives you columns

library(MBESS)

smd(Group.1=exp.group.rows$Arousal,Group.2=control.group.rows$Arousal)

smd(Mean.1=3.2,s.1=.8,Mean.2=2.45,s.2=.91,n.1=10,n.2=10)
ci.smd(smd=0.8753837,n.1=10,n.2=10)

mdata_viagra <- read_csv("Viagra.csv")
mdata_viagra$dose <- as.factor (mdata_viagra$dose)
levels(mdata_viagra$dose) <- list("Placebo" = 1, "Low Dose" = 2, "High Dose" = 3)

car::leveneTest(mdata_viagra$libido, group=mdata_viagra$dose,center="median")
options(contrasts = c("contr.sum","contr.poly"))
one.way.results <- lm(libido~dose,data=mdata_viagra)
library(apaTables)

car::Anova(one.way.results,type=3)

apa.aov.table(one.way.results)
apa.1way.table(iv=dose,dv=libido,data=mdata_viagra)
