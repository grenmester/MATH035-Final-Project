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


m_best = step(lm(formula = perstop ~ arstmade + searched + inside + sumissue + frisked + weap + contrabn + radio + pf, data = sqf2010[sqf2010$perstop<30,]))
plot(m_best$fitted.values, m_best$residuals)
qqnorm(m_best$fitted.values, m_best$residuals)

# m_full = lm(complied ~ pop + hdi + dem + internet + freepress, data = goog_sub)
# m_best = step(m_full)
# plot(m_best$fitted.values, m_best$residuals)
