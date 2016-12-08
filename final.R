1.
sqf2010 = read.csv("2010_sqf_m35.csv")
testedHispanic = length(sqf2010$race[sqf2010$race=='WHITE-HISPANIC'])
testedAsian = length(sqf2010$race[sqf2010$race=='ASIAN/PACIFIC ISLANDER'])
trueHispanic = testedHispanic*19/18 - testedAsian/18
trueAsian = testedAsian*19/18 - testedHispanic/18
total = testedHispanic + testedAsian

probHispanic = trueHispanic / total
probAsian = trueAsian / total
probActualHispanic = 0.95*trueHispanic / testedHispanic
probActualAsian = 0.95*trueAsian / testedAsian

2.
friskedWhite <-sqf2010$frisked[sqf2010$race =="WHITE"]
friskedBlack <-sqf2010$frisked[sqf2010$race =="BLACK"]
friskedWhiteHispanic <-sqf2010$frisked[sqf2010$race =="WHITE-HISPANIC"]
friskedBlackHispanic <-sqf2010$frisked[sqf2010$race =="BLACK-HISPANIC"]
friskedAsianPI <-sqf2010$frisked[sqf2010$race =="ASIAN/PACIFIC ISLANDER"]
friskedNatAmer <- sqf2010$frisked[sqf2010$race == "AMERICAN INDIAN/ALASKAN NATIVE"]

t.test(x=friskedAsianPI, y = friskedNatAmer, conf.level = .95)





3.
# set the working directory to Math35 within Documents - create Math35 folder
# if it doesn't exist
setwd('~')
if(regexpr('Documents',getwd())==-1) {
  if(!dir.exists('~/Documents')) dir.create('~/Documents')
  setwd('~/Documents')
}
if(!dir.exists('Math35')) dir.create('Math35')
setwd('Math35')

# load the SQF data from the .csv files to data frames sqf2010 and sqf2015
sqf2010 = read.csv("2010_sqf_m35.csv")
sqf2015 = read.csv("2015_sqf_m35.csv")

agesM = subset(sqf2010, sqf2010$age>5&sqf2010$age<98&sqf2010$sex =="M")
agesF = subset(sqf2010, sqf2010$age>5&sqf2010$age<98&sqf2010$sex =="F")
qqnorm(agesM$age)
qqnorm(agesF$age)
# both subsets of the age appear normal
qqplot(agesM$age, agesF$age)
# since the qqplot is a line, the distributions are the same.

agesStop = sqf2010$age[sqf2010$age<98&sqf2010$age>5]
qqplot(agesM$age, agesStop)
# since the qqplot is a line, the distributions are the same.

4.
sample_size = 22563
# the sample size of the 2015 data set is 22563
sample_mean <- mean(sqf2015$frisked)
z_critical <- qnorm(0.975)
pop_stdev <- sd(sqf2015$frisked)
margin_of_error <- z_critical * (pop_stdev / sqrt(sample_size))
confidence_interval <- c(sample_mean - margin_of_error,
                         sample_mean + margin_of_error)
print("Confidence interval:")
confidence_interval

# Confidence Interval for 2015 rates for being frisked(0.6700898 0.6823013)

summary(sqf2015)
sample_size = 639
# the sample size of the 2015 data set is 22563
sample_noId = subset(sqf2015, sqf2015$typeofid =="REFUSED" )
sample_mean <- mean(sample_noId$frisked)
z_critical <- qnorm(0.975)
pop_stdev <- sd(sample_noId$frisked)
margin_of_error <- z_critical * (pop_stdev / sqrt(sample_size))
confidence_interval <- c(sample_mean - margin_of_error,
                         sample_mean + margin_of_error)
print("Confidence interval:")

# Confidence Interval for 2015 rates for being frisked given that 
# one refused to show ID(0.5724871 0.6481702)
m_best = step(lm(formula = perstop ~ arstmade + searched + inside + sumissue + frisked + weap + contrabn + radio + pf, data = sqf2010[sqf2010$perstop<30,]))
plot(m_best$fitted.values, m_best$residuals)
qqnorm(m_best$fitted.values, m_best$residuals)

# m_full = lm(complied ~ pop + hdi + dem + internet + freepress, data = goog_sub)
# m_best = step(m_full)
# plot(m_best$fitted.values, m_best$residuals)
