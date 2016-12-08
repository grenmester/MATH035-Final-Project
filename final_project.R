goog = read.csv('http://stat.duke.edu/courses/Spring12/sta101.1/labs/goog.csv')

summary(goog)
plot(goog)

pop_max = which.max(goog$pop)
goog_sub = subset(goog, goog$pop != goog$pop[pop_max])

plot(goog_sub)

request_max = which.max(goog_sub$requests)
goog_sub_2 = subset(goog_sub, goog_sub$requests != goog_sub$requests[request_max])

plot(goog_sub_2)

round(cor(goog_sub[,c(2,3,4,5,7,8)]),2)

m_full = lm(complied ~ pop + hdi + dem + internet + freepress, data = goog_sub)
summary(m_full)
m_best = step(m_full)

plot(m_best$fitted.values, goog_sub$complied)

goog_sub$country
predict(m_best)

predict(m_best, interval = 'confidence')
predict(m_best, interval = 'prediction')

plot(m_best$fitted.values, m_best$residuals)

hist(m_best$residuals)
qqnorm(m_best$residuals)
qqline(m_best$residuals)

summary(m_best)
