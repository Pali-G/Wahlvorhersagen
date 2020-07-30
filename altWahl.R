library(zoo)

allensbach <- read.csv("Allensbach.csv", sep=';')
dimap <- read.csv("dimap.csv", sep=';')
forsa <- read.csv("Forsa.csv", sep=';')
insa <- read.csv("insa.csv", sep=';')
kantar <- read.csv("Kantar.csv", sep=';')

bundw <- read.csv("Bundestagswahl.csv", sep=';')
inst <- read.csv("Institut.csv", sep=';')

allensbach$CDU.CSU <- na.approx(allensbach$CDU.CSU)
allensbach$SPD <- na.approx(allensbach$SPD)
allensbach$GRUENE <- na.approx(allensbach$GRUENE)
allensbach$FDP <- na.approx(allensbach$FDP)
allensbach$LINKE <- na.approx(allensbach$LINKE)
allensbach$AFD <- na.approx(allensbach$AFD)
allensbach$OTHER <- na.approx(allensbach$OTHER)

allensbach <- allensbach[-c(1),] #Löschen der NA Zeile

dimap$CDU.CSU <- na.approx(dimap$CDU.CSU)
dimap$SPD <- na.approx(dimap$SPD)
dimap$GRUENE <- na.approx(dimap$GRUENE)
dimap$FDP <- na.approx(dimap$FDP)
dimap$LINKE <- na.approx(dimap$LINKE)
dimap$AFD <- na.approx(dimap$AFD)
dimap$OTHER <- na.approx(dimap$OTHER)

dimap <- dimap[-c(1),] #Löschen der NA Zeile

forsa$CDU.CSU <- na.approx(forsa$CDU.CSU)
forsa$SPD <- na.approx(forsa$SPD)
forsa$GRUENE <- na.approx(forsa$GRUENE)
forsa$FDP <- na.approx(forsa$FDP)
forsa$LINKE <- na.approx(forsa$LINKE)
forsa$AFD <- na.approx(forsa$AFD)
forsa$OTHER <- na.approx(forsa$OTHER)

forsa <- forsa[-c(1),] #Löschen der NA Zeile

insa$CDU.CSU <- na.approx(insa$CDU.CSU)
insa$SPD <- na.approx(insa$SPD)
insa$GRUENE <- na.approx(insa$GRUENE)
insa$FDP <- na.approx(insa$FDP)
insa$LINKE <- na.approx(insa$LINKE)
insa$AFD <- na.approx(insa$AFD)
insa$OTHER <- na.approx(insa$OTHER)

insa <- insa[-c(1),] #Löschen der NA Zeile

kantar$CDU.CSU <- na.approx(kantar$CDU.CSU)
kantar$SPD <- na.approx(kantar$SPD)
kantar$GRUENE <- na.approx(kantar$GRUENE)
kantar$FDP <- na.approx(kantar$FDP)
kantar$LINKE <- na.approx(kantar$LINKE)
kantar$AFD <- na.approx(kantar$AFD)
kantar$OTHER <- na.approx(kantar$OTHER)

kantar <- kantar[-c(1),] #Löschen der NA Zeile

INSTITUT <- c('Allensbach', 'Dimap', 'Forsa', 'Insa', 'Kantar')
CDU.CSU <- c(allensbach$CDU.CSU[1], dimap$CDU.CSU[1], forsa$CDU.CSU[1], insa$CDU.CSU[1], kantar$CDU.CSU[1])
SPD <- c(allensbach$SPD[1], dimap$SPD[1], forsa$SPD[1], insa$SPD[1], kantar$SPD[1])
GRUENE <- c(allensbach$GRUENE[1], dimap$GRUENE[1], forsa$GRUENE[1], insa$GRUENE[1], kantar$GRUENE[1])
FDP <- c(allensbach$FDP[1], dimap$FDP[1], forsa$FDP[1], insa$FDP[1], kantar$FDP[1])
LINKE <- c(allensbach$LINKE[1], dimap$LINKE[1], forsa$LINKE[1], insa$LINKE[1], kantar$LINKE[1])
AFD <- c(allensbach$AFD[1], dimap$AFD[1], forsa$AFD[1], insa$AFD[1], kantar$AFD[1])
OTHER <- c(allensbach$OTHER[1], dimap$OTHER[1], forsa$OTHER[1], insa$OTHER[1], kantar$OTHER[1])

last.Survey.Interp <- data.frame(INSTITUT, CDU.CSU, SPD, GRUENE, FDP, LINKE, AFD, OTHER)

CDU.CSU.methmean <- mean(c(last.Survey.Interp$CDU.CSU[INSTITUT=='Allensbach'], last.Survey.Interp$CDU.CSU[INSTITUT=='Insa'], mean(last.Survey.Interp$CDU.CSU[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))
SPD.methmean <- mean(c(last.Survey.Interp$SPD[INSTITUT=='Allensbach'], last.Survey.Interp$SPD[INSTITUT=='Insa'], mean(last.Survey.Interp$SPD[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))
GRUENE.methmean <- mean(c(last.Survey.Interp$GRUENE[INSTITUT=='Allensbach'], last.Survey.Interp$GRUENE[INSTITUT=='Insa'], mean(last.Survey.Interp$GRUENE[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))
FDP.methmean <- mean(c(last.Survey.Interp$FDP[INSTITUT=='Allensbach'], last.Survey.Interp$FDP[INSTITUT=='Insa'], mean(last.Survey.Interp$FDP[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))
LINKE.methmean <- mean(c(last.Survey.Interp$LINKE[INSTITUT=='Allensbach'], last.Survey.Interp$LINKE[INSTITUT=='Insa'], mean(last.Survey.Interp$LINKE[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))
AFD.methmean <- mean(c(last.Survey.Interp$AFD[INSTITUT=='Allensbach'], last.Survey.Interp$AFD[INSTITUT=='Insa'], mean(last.Survey.Interp$AFD[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))
OTHER.methmean <- mean(c(last.Survey.Interp$OTHER[INSTITUT=='Allensbach'], last.Survey.Interp$OTHER[INSTITUT=='Insa'], mean(last.Survey.Interp$OTHER[INSTITUT=='Kantar' | INSTITUT=='Dimap' | INSTITUT=='Forsa'])))

print(sd(CDU.CSU))
print(sd(SPD))
print(sd(GRUENE))
print(sd(FDP))
print(sd(LINKE))
print(sd(AFD))
print(sd(OTHER))

ggplot(data=last.Survey.Interp, aes(x=INSTITUT, y=CDU.CSU, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$CDU.CSU[1]) +
  geom_hline(yintercept = mean(CDU.CSU), color='grey') +
  geom_hline(yintercept = CDU.CSU.methmean, color='firebrick')

ggplot(data=last.Survey.Interp, aes(x=INSTITUT, y=SPD, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$SPD[1]) +
  geom_hline(yintercept= mean(SPD), color='grey') +
  geom_hline(yintercept = SPD.methmean, color='firebrick')

ggplot(data=last.Survey.Interp, aes(x=INSTITUT, y=GRUENE, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$GRUENE[1]) +
  geom_hline(yintercept = mean(GRUENE), color='grey')+
  geom_hline(yintercept = GRUENE.methmean, color='firebrick')

ggplot(data=last.Survey.Interp, aes(x=INSTITUT, y=FDP, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$FDP[1]) +
  geom_hline(yintercept = mean(FDP), color='grey')+
  geom_hline(yintercept = FDP.methmean, color='firebrick')

ggplot(data=last.Survey.Interp, aes(x=INSTITUT, y=LINKE, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$LINKE[1]) +
  geom_hline(yintercept = mean(LINKE), color='grey')+
  geom_hline(yintercept = LINKE.methmean, color='firebrick')

ggplot(data=last.Survey.Interp, aes(x=INSTITUT, y=AFD, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$AFD[1]) +
  geom_hline(yintercept = mean(AFD), color='grey')+
  geom_hline(yintercept = AFD.methmean, color='firebrick')

ggplot(data=last.Survey.Interp, aes(x=INSTITUT, y=OTHER, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=bundw$OTHER[1]) +
  geom_hline(yintercept = mean(OTHER), color='grey')+
  geom_hline(yintercept = OTHER.methmean, color='firebrick')
