
# descriptive statistics voor hoofdstuk 2.	Beschrijving dataset   

summary(df0$indKP)
hist(df0$indKP)  # Visueel niet sprekend omdat aantal mensen met 0 maanden Kobo zo groot is


summary(df0$revenue)
summary(df0$frequency)
summary(df0$recency)

#par(mfrow=c(2,2))
hist(df0$revenue)
hist(df0$frequency)
hist(df0$recency)


# evt df4 gebruiken uit het hoofd script met kobo / a la carte binair?

summary(df4$revenue)
summary(df4$frequency)
summary(df4$recency)

#par(mfrow=c(2,2))
hist(df4$revenue)
hist(df4$frequency)
hist(df4$recency)