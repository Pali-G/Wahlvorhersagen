library(zoo)
library(ggplot2)
library(miscTools)

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

CDU.CSU.Error <- abs(CDU.CSU - bundw$CDU.CSU[1])
SPD.Error <- abs(SPD - bundw$SPD[1])
GRUENE.Error <- abs(GRUENE - bundw$GRUENE[1])
FDP.Error <- abs(FDP - bundw$FDP[1])
LINKE.Error <- abs(LINKE - bundw$LINKE[1])
AFD.Error <- abs(AFD - bundw$AFD[1])
OTHER.Error <- abs(OTHER - bundw$OTHER[1])

last.Survey.Error <- data.frame(INSTITUT, CDU.CSU.Error, SPD.Error, GRUENE.Error, FDP.Error, LINKE.Error, AFD.Error, OTHER.Error)
CDU.CSU.relError <- CDU.CSU.Error / bundw$CDU.CSU[1]*100
SPD.relError <- SPD.Error / bundw$SPD[1]*100
GRUENE.relError <- GRUENE.Error / bundw$GRUENE[1]*100
FDP.relError <- FDP.Error / bundw$FDP[1]*100
LINKE.relError <- FDP.Error / bundw$FDP[1]*100
AFD.relError <- AFD.Error / bundw$AFD[1]*100
OTHER.relError <- OTHER.Error / bundw$OTHER[1]*100

last.Survey.relError <- data.frame(INSTITUT, CDU.CSU.relError, SPD.relError, GRUENE.relError, FDP.relError, LINKE.relError, AFD.relError, OTHER.relError)
last.Survey.Error.Inst.Mittel <- data.frame(INSTITUT=last.Survey.Error[,1], Mittelwert=rowMeans(last.Survey.Error[,-1]))
last.Survey.relError.Inst.Mittel <- data.frame(INSTITUT=last.Survey.relError[,1], Mittelwert=rowMeans(last.Survey.relError[,-1]))


#relativer Fehler Mittel
ggplot(data=last.Survey.relError.Inst.Mittel, aes(x=INSTITUT, y=Mittelwert, fill=INSTITUT)) +
  geom_bar(stat="identity")+
  geom_hline(yintercept = mean(last.Survey.relError.Inst.Mittel$Mittelwert))

#absoluter Fehler Mittel
ggplot(data=last.Survey.Error.Inst.Mittel, aes(x=INSTITUT, y=Mittelwert, fill=INSTITUT)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = mean(last.Survey.Error.Inst.Mittel$Mittelwert))

last.Survey.Error.Inst.Median <- data.frame(INSTITUT=last.Survey.Error[,1], Median=rowMedians(last.Survey.Error[,-1]))
last.Survey.relError.Inst.Median <- data.frame(INSTITUT=last.Survey.relError[,1], Median=rowMedians(last.Survey.relError[,-1]))

#relativer Fehler Median
ggplot(data=last.Survey.relError.Inst.Median, aes(x=INSTITUT, y=Median, fill=INSTITUT)) +
  geom_bar(stat="identity")+
  geom_hline(yintercept = mean(last.Survey.relError.Inst.Median$Median))

#absoluter Fehler Median
ggplot(data=last.Survey.Error.Inst.Median, aes(x=INSTITUT, y=Median, fill=INSTITUT)) +
  geom_bar(stat="identity")+
  geom_hline(yintercept = mean(last.Survey.Error.Inst.Median$Median))
