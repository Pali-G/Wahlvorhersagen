library(ggplot2)

allensbach <- read.csv("Allensbach.csv", sep=';')
dimap <- read.csv("dimap.csv", sep=';')
forsa <- read.csv("Forsa.csv", sep=';')
insa <- read.csv("insa.csv", sep=';')
kantar <- read.csv("Kantar.csv", sep=';')

bundw <- read.csv("Bundestagswahl.csv", sep=';')
inst <- read.csv("Institut.csv", sep=';')

INSTITUT <- c('Allensbach', 'Dimap', 'Forsa', 'Insa', 'Kantar')
CDU.CSU <- c(allensbach$CDU.CSU[1], dimap$CDU.CSU[1], forsa$CDU.CSU[1], insa$CDU.CSU[1], kantar$CDU.CSU[1])
SPD <- c(allensbach$SPD[1], dimap$SPD[1], forsa$SPD[1], insa$SPD[1], kantar$SPD[1])
GRUENE <- c(allensbach$GRUENE[1], dimap$GRUENE[1], forsa$GRUENE[1], insa$GRUENE[1], kantar$GRUENE[1])
FDP <- c(allensbach$FDP[1], dimap$FDP[1], forsa$FDP[1], insa$FDP[1], kantar$FDP[1])
LINKE <- c(allensbach$LINKE[1], dimap$LINKE[1], forsa$LINKE[1], insa$LINKE[1], kantar$LINKE[1])
AFD <- c(allensbach$AFD[1], dimap$AFD[1], forsa$AFD[1], insa$AFD[1], kantar$AFD[1])
OTHER <- c(allensbach$OTHER[1], dimap$OTHER[1], forsa$OTHER[1], insa$OTHER[1], kantar$OTHER[1])

last.Survey <- data.frame(INSTITUT, CDU.CSU, SPD, GRUENE, FDP, LINKE, AFD, OTHER)


CDU.CSU.methmean <- mean(c(last.Survey$CDU.CSU[INSTITUT=='Allensbach'], last.Survey$CDU.CSU[INSTITUT=='Insa'], mean(last.Survey$CDU.CSU[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))
SPD.methmean <- mean(c(last.Survey$SPD[INSTITUT=='Allensbach'], last.Survey$SPD[INSTITUT=='Insa'], mean(last.Survey$SPD[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))
GRUENE.methmean <- mean(c(last.Survey$GRUENE[INSTITUT=='Allensbach'], last.Survey$GRUENE[INSTITUT=='Insa'], mean(last.Survey$GRUENE[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))
FDP.methmean <- mean(c(last.Survey$FDP[INSTITUT=='Allensbach'], last.Survey$FDP[INSTITUT=='Insa'], mean(last.Survey$FDP[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))
LINKE.methmean <- mean(c(last.Survey$LINKE[INSTITUT=='Allensbach'], last.Survey$LINKE[INSTITUT=='Insa'], mean(last.Survey$LINKE[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))
AFD.methmean <- mean(c(last.Survey$AFD[INSTITUT=='Allensbach'], last.Survey$AFD[INSTITUT=='Insa'], mean(last.Survey$AFD[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))
OTHER.methmean <- mean(c(last.Survey$OTHER[INSTITUT=='Allensbach'], last.Survey$OTHER[INSTITUT=='Insa'], mean(last.Survey$OTHER[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))

print(sd(CDU.CSU))
print(sd(SPD))
print(sd(GRUENE))
print(sd(FDP))
print(sd(LINKE))
print(sd(AFD))
print(sd(OTHER))

ggplot(data=last.Survey, aes(x=INSTITUT, y=CDU.CSU, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$CDU.CSU[1]) +
  geom_hline(yintercept = mean(CDU.CSU), color='grey') +
  geom_hline(yintercept = CDU.CSU.methmean, color='firebrick')

ggplot(data=last.Survey, aes(x=INSTITUT, y=SPD, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$SPD[1]) +
  geom_hline(yintercept= mean(SPD), color='grey') +
  geom_hline(yintercept = SPD.methmean, color='firebrick')

ggplot(data=last.Survey, aes(x=INSTITUT, y=GRUENE, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$GRUENE[1]) +
  geom_hline(yintercept = mean(GRUENE), color='grey')+
  geom_hline(yintercept = GRUENE.methmean, color='firebrick')

ggplot(data=last.Survey, aes(x=INSTITUT, y=FDP, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$FDP[1]) +
  geom_hline(yintercept = mean(FDP), color='grey')+
  geom_hline(yintercept = FDP.methmean, color='firebrick')

ggplot(data=last.Survey, aes(x=INSTITUT, y=LINKE, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$LINKE[1]) +
  geom_hline(yintercept = mean(LINKE), color='grey')+
  geom_hline(yintercept = LINKE.methmean, color='firebrick')

ggplot(data=last.Survey, aes(x=INSTITUT, y=AFD, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$AFD[1]) +
  geom_hline(yintercept = mean(AFD), color='grey')+
  geom_hline(yintercept = AFD.methmean, color='firebrick')

ggplot(data=last.Survey, aes(x=INSTITUT, y=OTHER, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$OTHER[1]) +
  geom_hline(yintercept = mean(OTHER), color='grey')+
  geom_hline(yintercept = OTHER.methmean, color='firebrick')
