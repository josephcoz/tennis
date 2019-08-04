#source('http://grimshawville.byu.edu/Tennis2018.R')

dirstr <- as.character(getwd())
filename <- "/tennis.RData"

path <- paste0(dirstr, filename)

load(path)

#check data read in correctly
str(tennis)

summary(tennis)

#THESE are the comparison cases...
#if factors are not releveled, r defaults to alphabetical order
tennis$Tournament<-factor(tennis$Tournament)
tennis$Tournament<-relevel(tennis$Tournament, "Masters")
tennis$Gender<-factor(tennis$Gender)
tennis$Gender<-relevel(tennis$Gender, "W")
tennis$Surface<-factor(tennis$Surface)
tennis$Surface<-relevel(tennis$Surface, "Hard")
tennis$Round<-factor(tennis$Round)
tennis$Round<-relevel(tennis$Round, "1st Round")
tennis$Best.of<-factor(tennis$Best.of)
tennis$Best.of<-relevel(tennis$Best.of, "5")



#create train and test
set.seed(29)

train_ind <- sample(12914, 10000)

tennis_train <- tennis[train_ind,]
tennis_test <- tennis[-train_ind,]

summary(tennis_train)
summary(tennis_test)

#Response var: DNF

#boxplots for quantitative variables
boxplot(WRank ~ DNF, data = tennis, main ='Winner\'s Rank vs DNF')
boxplot(LRank ~ DNF, data = tennis, main = 'Loser\'s Rank vs DNF')
boxplot(WPts ~ DNF, data = tennis, main = 'Winner Points vs DNF')
boxplot(LPts ~ DNF, data = tennis, main = 'Loser Points vs DNF')
boxplot(AvgW ~ DNF, data = tennis, main = 'Avg Wins vs DNF')
boxplot(AvgL ~ DNF, data = tennis, main = 'Avg Losses vs DNF')

#tables for categorical variables
prop.table(table(tennis$DNF, tennis$Tournament),margin=2)
prop.table(table(tennis$DNF, tennis$Surface),margin=2)
prop.table(table(tennis$DNF, tennis$Round),margin=2)
prop.table(table(tennis$DNF, tennis$Best.of),margin=2)
prop.table(table(tennis$DNF, tennis$Wsets), margin=2)
prop.table(table(tennis$DNF, tennis$Lsets),margin=2)
prop.table(table(tennis$DNF, tennis$Comment),margin=2) #this is like crossing DNF with DNF
prop.table(table(tennis$DNF, tennis$Gender),margin=2)

#Analysis

#Response: DNF
#Explanatory: Gender, Tournament, Surface, Round, Best.of, WRank, LRank

#Model: log( P(DNF) / P(Not DNF) ) =
#           beta0 + beta1 * Men +             #because we releveled factor at the beginning 
#           beta2 *  GrandSlam +
#           beta3 * Clay + beta4 * Grass +    #r orders factors alphabetically by default, I specified Hard for court 

#           beta5 * 2nd Round + beta6 * 3rd Round + beta7 * 4th Round + 
#           beta8 * Quarterfinals + beta9 * Semifinals + beta10 * The Final +
#           beta11 * Three (3) +
#           beta12 * WRank +
#           beta13 * LRank

#fit model
#use proc genmod (or proc logistic..?) in SAS
tennis_out <- glm(DNF ~ Gender + Tournament + Surface + Round + Best.of + WRank + LRank,
                  data = tennis_train, family="binomial") #if you leave out family, it does same analysis of lm

summary(tennis_out)

#Difference b/w Women and Men
#summary interpretation: 
#log-odds: men DNF more often than women holding all else constant

#we usually look at change in odds:
exp(coef(tennis_out)[-1])
#[-1] because we don't care about y-intercept

#95% CI in change in odds
exp(confint(tennis_out)[-1,])
#no statistically significant difference b/w women and men 
# after adjusting for all other factors.

confint(tennis_out)

# compute the pvalue for the LRT test
tennis_red <- glm(DNF ~ Tournament + Surface + Round + Best.of + WRank + LRank,
                  data = tennis_train, family="binomial")

anova(tennis_red, tennis_out, test="Chisq")
#this should get the same value of the z-test (using summary above)

#Tournament Type (Grand Slam, Masters)
#log-odds: players in Grand Slam are less likely to DNF than those in Masters
#   (If it were opposite of above): players in Masters are more likely to DNF than those in Masters
#   Think about if your audience is expecting comparison one way or another

#change in odds (look at above executed)
#holding all else constant, Grand Slam tournaments have a 49% decrease in odds ratio
#   compared to Masters tournaments (95% CI: 33.7%, 77.8%)

#GRAPHIC
#bad: barchart with Masters / GrandSlam proportions (doesn't follow significance)
#good: plot(Masters + GrandSlam, partial logit(DNF))

#graphic of effect
plot(c(0,1), c(0,-0.56), #plot x and y values, will add labels later
     xlim=c(-0.5,1.5), #puts dots more in the center horizontally
     ylim=c(-1.1,0.1), # "  " vertically
     pch=19, #closed circles
     cex=2, #makes dot twice as large--more eye-catching
     xlab="", ylab="Partial log(DNF", 
     axes=FALSE)
#add 95% CI (not exponentiated one)
arrows(1,-1.09,1,-0.25, code=3, angle=90)
axis(2) #draws default y-axis that R would've given
axis(1, c(0,1), c("Masters","Grand Slam")) #draws x-axis with edits
box() #draws a cute box





