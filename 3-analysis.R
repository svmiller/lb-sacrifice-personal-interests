summary(M1 <- glm(sacrificedummy ~ z.age + female + hsed + z.ses + unemployed + z.ideology + z.satisdem + z.preseconc, data=Data))

summary(M2 <- glm(sacrificedummy ~ z.age + female + hsed + z.ses + unemployed + z.ideology + z.satisdem + z.preseconc + factor(year), data=Data))

summary(M3 <- glm(sacrificedummy ~ z.age + female + hsed + z.ses + unemployed + z.ideology + z.satisdem + z.preseconc + factor(year) + factor(country), data=Data))

summary(M4 <- glmer(sacrificedummy ~ z.age + female + hsed + z.ses + unemployed + z.ideology + z.satisdem + z.preseconc + factor(year) + (1 | country), data=Data, family=binomial(link="logit"), na.action=na.omit))
dotplot(ranef(M4, postVar=TRUE))

Data2 <- na.omit(Data)

Z1 <- zelig(sacrificedummy ~ z.age + female + hsed + z.ses + unemployed + z.ideology + z.satisdem + z.preseconc, model = "logit", data = Data2)

Z1.low <- setx(Z1, z.satisdem = min(z.satisdem), z.age = rep(seq(from = min(z.age), to = max(z.age), length.out = 100)))
Z1.high <- setx(Z1, z.satisdem = max(z.satisdem), z.age = rep(seq(from = min(z.age), to = max(z.age), length.out = 100)))


Z1.sim <- sim(Z1, x = Z1.low, x1 = Z1.high)

plot.ci(Z1.sim, xlab = "Age",
        ylab = "E(Sacrfice Personal Interest)",
        main = "Effect of Democratic Satisfaction and \n Age on Sacrificing Personal Interest",
        ci=c(90,95)
)
