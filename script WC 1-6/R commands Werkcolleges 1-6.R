###################
# WC1

install.packages("epibasix") #deze regel code alleen bij de eerste keer
library(epibasix) # dit doe je elke R-sessie waarin je deze library gebruikt
# voorbeeld blz.57/58 van Petrie&Watson
# zet 4 getallen (c(42,259,13,288)) in een matrix met 2 kolommen 
# (ncol=2) en vul regel voor regel (byrow=TRUE):
bse=matrix(c(42,259,13,288),ncol=2,byrow=TRUE)
bse
#Bepalen van de associatiematen en hun betrouwbaarheidsintervallen:
bse.ep=epi2x2(bse,alpha=0.05)
summary(bse.ep)
# NB: deze library hanteert een andere definitie van H0 en H1 dan je gewend 
# bent. 



###################
# WC2
# Tools-Import Dataset - From text File... kies Table10.1.txt 
#
attach(Table10.1)
# de scatterplot, pch staat voor pointcharacter, als je het nummer verandert 
# krijg je andere symbolen
plot(wBAP,PICP,pch=20,col="red")
?cor # helpinformatie
?cor.test # helpinformatie
cor.test(wBAP,PICP)
detach(Table10.1)
####
# Tools-Import Dataset - From text File... kies Table10.2.txt 
#
attach(Table10.2)
# de scatterplot xlab en ylab geven de labels op de assen
plot(chestgth,livewt,xlab="Chest girth (cm)",ylab="Live weight (kg)",pch=20,col="red")
?glm # helpinformatie
fit=glm(livewt~chestgth, family="gaussian")
summary(fit)
# In de kolom Estimate staan de intercept (Intercept) en de slope (chestgth). 
# In de kolom Std.Error staan de standard errors. 
# In de volgende kolom staan de t-toetsen daarna de p-waardes (Pr(>|t|)). 
# Vergelijk met display 10.1 op blz 136 van het boek
#
# om de lijn in de scatterplot toe te voegen:
?abline # helpinformatie
abline(fit,col="blue")
# of
abline(a=-46.041,b=1.043,col="blue")
# Om R^2 (kwadraat van de correlatie) uit te rekenen:
1-(fit$deviance/fit$null.deviance)
# of
(cor(livewt,predict(fit)))^2
# voor predict(fit) zie hieronder
# 95% Betrouwbaarheids intervallen voor de intercept en de slope:
confint(fit)
#############
# Om  de kwadraatsommen te krijgen gebruiken we drop1. 
drop1(fit,test="F")
# Deze functie fit (schat) meerdere modellen door steeds 1 variabele uit het model weg te laten en 
# geeft residu kwadraatsommen van die modellen. In dit geval: 
# 1.) een model met het intercept en livewt. Dit is het volledige model. Er gaat dan dus 
# geen variabele er uit dus drop is dan none. Dit is de eerste regel. De residu kwadraatsom is 452.3.
# 2.) een model waaruit chestgth is weggelaten, dus een model met 1 constante erin. 
# De residukwadraatsom van dit model is SS-totaal. Hier zitten de effecten van chestgth nog in, omdat chestgth weggelaten is uit het model. 
# Dus in de res. kwadraatsom uit de eerste regel zitten geen chestgth effecten meer, in de residu kwadraatsom
#uit de tweede regel nog wel. Het verschil geeft dus de regressie kwadraatsom. 
# Nu is de anova tabel te maken. Zie ook Display 10.1 op blz 136 (P&W).
#
# De residuen en de predicted values (de punten op de lijn) krijg je door
residuals(fit)
predict(fit)
# dus figuur 10.8a en c krijg je met
plot(chestgth,residuals(fit),xlab="Chest girth (cm)",ylab="Residuals (kg)",pch=20,col="red")
plot(predict(fit),residuals(fit),xlab="Predicted values (cm)",ylab="Residuals (kg)",pch=20,col="red")
#
# Om te zien of er afwijkingen zijn van de normaliteit gebruiken we een normal probability plot. 
# Deze plot residuen tegen wat verwacht mag worden als de residuen normaal verdeeld zouden zijn.
# Als de residuen uit een normale verdeling komen zal dus deze plot punten laten zien,
# die op een rechte lijn liggen,
qqnorm(residuals(fit))
abline(0,sd(residuals(fit)))
#
detach(Table10.2)



###################
#WC3
# Tools-Import Dataset - From text File... kies Table8.1.txt 
attach(Table8.1)
# Om duidelijk te maken dat de getallen in de kolom Group geen numerieke waarden voorstellen
# maar labels/groepen zijn, gebruiken we factor(Group). Wat R dan precies doet komt in een later werkcollege.
# Eerst de data plotten
plot(factor(Group),Calc,xlab="Diet supplement",ylab="Calculus index")
# of
boxplot(Calc~Group,xlab="Diet supplement",ylab="Calculus index")
# gemiddelden en standaard afwijkingen zie display 8.1 blz  108
?tapply # helpinformatie
tapply(Calc,Group,mean)  # berekent het gemiddelde van Calc per Group
tapply(Calc,Group,sd)
fit=glm(Calc~factor(Group))
#############
# Om  de kwadraatsommen te krijgen gebruiken we drop1. 
drop1(fit,test="F")
# Dit werkt net als bij de regressie:
# 1.) een model met een constante en Group. Dit is het volledige model. Er gaat dan dus 
# geen variabele er uit dus drop is dan none. Dit is de eerste regel. De residukwadraatsom is 3.1120
# 2.) een model waaruit Group is weggelaten, dus een model met alleen een constante erin. 
# De residukwadraatsom van dit model is SS-totaal. Hier zitten de effecten van Group nog in. Omdat Group niet in het model zit, wordt hier ook geen kwadraatsom voor uitgerekend. 
# Dus in de res. kwadraatsom uit de eerste regel zitten geen Group effecten meer, in de res. kwadraatsom
# uit de tweede regel nog wel. Het verschil geeft dus de kwadraatsom voor group. 
# Nu is de anova table te maken. Zie ook Display 8.1 op blz 108.

# Ook van dit model kan je de residuen en de predicted values (de groepsgemiddelden) krijgen door
residuals(fit)
predict(fit)
# en de grafische relatie tussen predicted values en de residuen met
plot(predict(fit),residuals(fit),xlab="Predicted values (cm)",ylab="Residuals (kg)",pch=20,col="red")
#
# Om te zien of er afwijkingen zijn van de normaliteit gebruiken we weer een normal probability plot. 
# Deze zet de geobserveerde residuen af tegen de verwachte residuen als de residuen normaal verdeeld zouden zijn.
# Als de residuen uit een normale verdeling komen zal dus deze plot punten laten zien
# die op een rechte lijn liggen.
qqnorm(residuals(fit))
abline(0,sd(residuals(fit)))
#
detach(Table8.1)



###################
#WC4
# Tools-Import Dataset - From text File... kies Display11.1
attach(Display11.1)
# de scatterplot xlab en ylab geven de labels op de assen
plot(Bodywt,Htgirth,xlab="Heart girth (cm)",ylab="Body weight (kg)",pch=20,col="red")
fit=glm(Bodywt~Age+Htgirth+Height+Length+Sex+Umbgth,family="gaussian")
summary(fit)
# in de kolom 'estimate' staan de intercept (Intercept) en de slopes; 
# in de kolom 'Std.Error' staan de standard errors. 
# In de volgende kolom staan de t-toetsen daarna de p-waardes (Pr(>|t|)). 
# Vergelijk met display 11.1 op blz 154 van het boek
#
#
# Om R^2 (kwadraat van de correlatie) uit te rekenen:
1-(fit$deviance/fit$null.deviance)
# 95% Betrouwbaarheidsintervallen voor de parameters:
confint(fit)
#############
# Om  de kwadraatsommen te krijgen gebruiken we drop1. 
drop1(fit,test="F")
# 
# De residuen en de predicted values (de punten op de lijn) krijg je door
residuals(fit)
predict(fit)
# dus figuur 11.1 krijg je
plot(Htgirth,residuals(fit),xlab="Chest girth (cm)",ylab="Residuals (kg)",pch=20,col="red")
plot(predict(fit),residuals(fit),xlab="Predicted values (cm)",ylab="Residuals (kg)",pch=20,col="red")
#
# Om te zien of er afwijkingen zijn van de normaliteit gebruiken we een normal probability plot. 
# Deze plot residuen tegen wat verwacht mag worden als de residuen normaal verdeeld zouden zijn.
# Als de residuen uit een normale verdeling komen zal dus deze plot punten laten zien,
# die op een rechte lijn liggen,
qqnorm(residuals(fit))
abline(0,sd(residuals(fit)))
#
detach(Display11.1)
#
# anova met indicator variabelen
#
attach(Table8.1)
fit=glm(Calc~factor(Group))
summary(fit)
# in de regel factor(group)2 staat in de kolom estimate, het verschil in gemiddelde tussen groep 2 en 1
# in de regel factor(group)3 staat in de kolom estimate, het verschil in gemiddelde tussen groep 3 en 1
# nu een regressie met 2 indicatoren. Eerst maken we de groep2 en groep 3 indicatoren:
?"==" # help over ==
gr2=1*(Group==2) # indicator variabele voor groep 2
gr3=1*(Group==3) # indicator variabele voor groep 3
fit.i=glm(Calc~gr2+gr3)
summary(fit.i)
#vergelijk met de anova uitvoer (fit)
detach(Table8.1)


###################
#WC5
# De datafile uit het boek is niet beschikbaar. We gebruiken een voorbeeld over doofheid bij dalmatiers.
# Zie opgave 1 voor een beschrijving van deze data.
# Dit is een zogeheten "comma separated values (csv)" bestand wat wil zeggen dat de getallen met 
# komma's van elkaar gescheiden worden.
# Tools-Import Dataset - From text File... kies dalmatian.csv
attach(dalmatian)
table(deaf,spot)
# om een logistische regressie te doen gebruiken we ook de glm functie maar nu met family=binomial
# dat hoefde bij de normale verdeling niet want die verdeling is default in de glm functie
fit=glm(deaf~factor(spot),family=binomial)
summary(fit)
# dus de deaf log-odds ratio voor spot groep 2 vs spot groep 1 is -.6496 met een 
# standard error van 0.6497, en de log-odds ratio voor spot groep 3 vs spot groep 1 is -.9181 met een 
# standard error van 0.3136
# Betrouwbaarheidsintervallen gebaseerd op de Wald test krijg je door
confint.default(fit)
# De functie confint geeft zg profile likelihood intervallen die in de regel beter zijn (voor de normale verdeling zijn deze het zelfde)
confint(fit)
detach(dalmatian)


###################
#WC6
# Tools-Import Dataset - From text File... kies katten.txt
attach(katten)
# De commandos voor survival analyse zijn niet automatisch bekend in R. 
# Eerst moet de survival library geladen worden 
library(survival)
# Nu moet R verteld worden dat de afhankelijke variabele een combinatie is van de variable tijd
# en de variabele die aan geeft of er een event is geweest of een censurering op dat tijdstip. Dus we maken 
# om in R-termen te spreken een survival object met Surv(tijd,tumor) en gebruiken dat als afhankelijke
# variabele in de survfit functie die dan de Kaplan Meier-tabel uitrekent:
fit=survfit(Surv(tijd,tumor)~1)
summary(fit)
#Om de KM-grafiek te krijgen en de onder- en bovengrens van de betrouwbaarheidsintervallen:
plot(fit)
detach(katten)


