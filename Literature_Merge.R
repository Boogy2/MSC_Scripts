## @Author: Marius Koch
## @Date: 26.11.2021
## @Version: 1.0
## @Description: R-Skript zur Deduplizierung und Vorprozessierung von Literatursuchergebnissen aus WOS, Dimensions und Google Scholar.
##               Verarbeitet werden vorformatierte Zotero Exporte und ein Exporte aus Dimensions. 
library(readr)
library(dplyr)
library(stringi)
## Leitfrage 1 Subfrage 1 - Literatur Prozessierung #####################################################################################################################
## WOS war hierbei 0 
DimensionsSet_Leit1_Sub1 <- read_csv("Documents/Uni/Master Sc/Masterarbeit/MA/Fragestellung1/Dimensions_Results/Leitfrage1Subfrage1/Dimensions-Publication-2021-08-17_18-00-27.csv", skip=1)
ScholarSet_Leit1_Sub1 <- read_csv("Documents/Uni/Master Sc/Masterarbeit/MA/Fragestellung1/Scholar_Results/Leitfrage1Subfrage1/Scholar ALL.csv")
ScholarSet_Leit1_Sub1 <- ScholarSet_Leit1_Sub1[!is.na(ScholarSet_Leit1_Sub1$DOI),]
DimensionsSet_Leit1_Sub1 <- DimensionsSet_Leit1_Sub1[!is.na(DimensionsSet_Leit1_Sub1$DOI),]

L1S1DimensionsDOIS <- (DimensionsSet_Leit1_Sub1[c("Title", "DOI")])$DOI
L1S1ScholarDOIS <- (ScholarSet_Leit1_Sub1[c("Extra")])$Extra

# Anzahl Treffer Dimensions, GoogleScholar
L1S1AnzahlTrefferDimensions <- length(L1S1DimensionsDOIS)
L1S1AnzahlTrefferScholarDOIS <- length(L1S1ScholarDOIS)

# AnzahlBled eConference detektieren + entfernen
AnzahlBledeConference <- length(grep(".*Bled eConference.*", DimensionsSet_Leit1_Sub1$Title))
DimensionsSet_Leit1_Sub1 <- DimensionsSet_Leit1_Sub1[which(grepl(".*Bled eConference.*", DimensionsSet_Leit1_Sub1$Title) == FALSE),]

# Reduktion auf benötigte Infos + hinzufügen von Quelle der Literatur
# DOI = case insensitiv --> alle dois werden mit kleinen Buchstaben versehen für den Merge
L1S1colnames_ergFrame <- c("Titel", "DOI", "Source")
L1S1DimensionsModFrame <- DimensionsSet_Leit1_Sub1[c("Title", "DOI")]
L1S1DimensionsModFrame$Source <- rep("Dimensions", each=length(L1S1DimensionsModFrame$DOI))
colnames(L1S1DimensionsModFrame) <- L1S1colnames_ergFrame
L1S1DimensionsModFrame$DOI <- tolower(L1S1DimensionsModFrame$DOI)

L1S1ScholarModFrame <- ScholarSet_Leit1_Sub1[c("Title", "Extra")]
L1S1ScholarModFrame$Source <- rep("Scholar", each=length(L1S1ScholarModFrame$Extra))
colnames(L1S1ScholarModFrame) <- L1S1colnames_ergFrame
L1S1ScholarModFrame$DOI <- tolower(L1S1ScholarModFrame$DOI)

#Vereinigung in gesamt frame + Deduplizierung
L1S1ergFrame <- merge(L1S1DimensionsModFrame, L1S1ScholarModFrame, by = 'DOI', all = TRUE)

L1S1AnzahlDeduplizierterTreffer <- length(L1S1ergFrame$DOI)
L1S1AnzahlDuplikate <- ((L1S1AnzahlTrefferDimensions + L1S1AnzahlTrefferScholarDOIS) - AnzahlBledeConference ) - L1S1AnzahlDeduplizierterTreffer

ergFrame <- data.frame("Leitfrage" = 1,
                       "Subfragestellung" = 1,
                       "Anzahl Treffer Ges" = (L1S1AnzahlTrefferDimensions + L1S1AnzahlTrefferScholarDOIS), 
                       "Anzahl gefiltered durch Skript Regex" = AnzahlBledeConference,
                       "Anzahl Treffer Dimensions" = L1S1AnzahlTrefferDimensions, 
                       "Anzahl Treffer WebOfScience" = 0, 
                       "Anzahl Treffer Google Scholar" = L1S1AnzahlTrefferScholarDOIS,
                       "Anzahl Duplikate" = L1S1AnzahlDuplikate, 
                       "Anzahl Deduplizierter Treffer" = L1S1AnzahlDeduplizierterTreffer)


## Leitfrage 1 Subfrage 2 - Literatur Prozessierung #####################################################################################################################
## WOS war hierbei 0 
DimensionsSet_Leit1_Sub2 <- read_csv("Documents/Uni/Master Sc/Masterarbeit/MA/Fragestellung1/Dimensions_Results/Leitfrage1Subfrage2/Dimensions-Publication-2021-11-26_08-24-22.csv", skip=1)
ScholarSet_Leit1_Sub2 <- read_csv("Documents/Uni/Master Sc/Masterarbeit/MA/Fragestellung1/Scholar_Results/Leitfrage1Subfrage2/google scholar all.csv")
ScholarSet_Leit1_Sub2 <- ScholarSet_Leit1_Sub2[!is.na(ScholarSet_Leit1_Sub2$DOI),]
DimensionsSet_Leit1_Sub2 <- DimensionsSet_Leit1_Sub2[!is.na(DimensionsSet_Leit1_Sub2$DOI),]


L1S2DimensionsDOIS <- (DimensionsSet_Leit1_Sub2[c("Title", "DOI")])$DOI
L1S2ScholarDOIS <- (ScholarSet_Leit1_Sub2[c("Extra")])$Extra

# Anzahl Treffer Dimensions, GoogleScholar
L1S2AnzahlTrefferDimensions <- length(L1S2DimensionsDOIS)
L1S2AnzahlTrefferScholarDOIS <- length(L1S2ScholarDOIS)

# Reduktion auf benötigte Infos + hinzufügen von Quelle der Literatur
# DOI = case insensitiv --> alle dois werden mit kleinen Buchstaben versehen für den Merge
L1S2colnames_ergFrame <- c("Titel", "DOI", "Source")
L1S2DimensionsModFrame <- DimensionsSet_Leit1_Sub2[c("Title", "DOI")]
L1S2DimensionsModFrame$Source <- rep("Dimensions", each=length(L1S2DimensionsModFrame$DOI))
colnames(L1S2DimensionsModFrame) <- L1S2colnames_ergFrame
L1S2DimensionsModFrame$DOI <- tolower(L1S2DimensionsModFrame$DOI)

L1S2ScholarModFrame <- ScholarSet_Leit1_Sub2[c("Title", "Extra")]
L1S2ScholarModFrame$Source <- rep("Scholar", each=length(L1S2ScholarModFrame$Extra))
colnames(L1S2ScholarModFrame) <- L1S2colnames_ergFrame
L1S2ScholarModFrame$DOI <- tolower(L1S2ScholarModFrame$DOI)

#Vereinigung in gesamt frame + Deduplizierung
L1S2ergFrame <- merge(L1S2DimensionsModFrame, L1S2ScholarModFrame, by = 'DOI', all = TRUE)

L1S2AnzahlDeduplizierterTreffer <- length(L1S2ergFrame$DOI)
L1S2AnzahlDuplikate <- ((L1S2AnzahlTrefferDimensions + L1S2AnzahlTrefferScholarDOIS) ) - L1S2AnzahlDeduplizierterTreffer
L1S2_ergebnis <- c(1,2,(L1S2AnzahlTrefferDimensions + L1S2AnzahlTrefferScholarDOIS), 0, L1S2AnzahlTrefferDimensions, 0, L1S2AnzahlTrefferScholarDOIS, L1S2AnzahlDuplikate, L1S2AnzahlDeduplizierterTreffer)
ergFrame <- rbind(ergFrame, L1S2_ergebnis)

## Leitfrage 2 Subfrage 1 - Literatur Prozessierung #####################################################################################################################
DimensionsSet_Leit2_Sub1 <- read_csv("/Users/marius/Documents/Uni/Master\ Sc/Masterarbeit/MA/Fragestellung2/Literaturrecherche/Leitfrage2_Sub1DimensionsAll.csv", skip=1)
ScholarSet_Leit2_Sub1 <- read_csv("/Users/marius/Documents/Uni/Master\ Sc/Masterarbeit/MA/Fragestellung2/Literaturrecherche/Leitfrage2_Sub1ScholarAll.csv")
ScholarSet_Leit2_Sub1 <- ScholarSet_Leit2_Sub1[!is.na(ScholarSet_Leit2_Sub1$DOI),]
DimensionsSet_Leit2_Sub1 <- DimensionsSet_Leit2_Sub1[!is.na(DimensionsSet_Leit2_Sub1$DOI),]
WebOfScienceSet_Leit2_Sub1 <- read_csv("/Users/marius/Documents/Uni/Master\ Sc/Masterarbeit/MA/Fragestellung2/Literaturrecherche/Leitfrage2_Sub1WebOfScienceAll.csv")

L2S1DimensionsDOIS <- (DimensionsSet_Leit2_Sub1[c("Title", "DOI")])$DOI
L2S1WOSDOIS <- (WebOfScienceSet_Leit2_Sub1[c("Extra")])$Extra
L2S1ScholarDOIS <- (ScholarSet_Leit2_Sub1[c("Extra")])$Extra

# Anzahl Treffer Dimensions, WOS, GoogleScholar
L2S1AnzahlTrefferDimensions <- length(L2S1DimensionsDOIS)
L2S1AnzahlTrefferWOS <- length(L2S1WOSDOIS)
L2S1AnzahlTrefferScholarDOIS <- length(L2S1ScholarDOIS)

# Reduktion auf benötigte Infos + hinzufügen von Quelle der Literatur
# DOI = case insensitiv --> alle dois werden mit kleinen Buchstaben versehen für den Merge
L2S1colnames_ergFrame <- c("Titel", "DOI", "Source")
L2S1DimensionsModFrame <- DimensionsSet_Leit2_Sub1[c("Title", "DOI")]
L2S1DimensionsModFrame$Source <- rep("Dimensions", each=length(L2S1DimensionsModFrame$DOI))
colnames(L2S1DimensionsModFrame) <- L2S1colnames_ergFrame
L2S1DimensionsModFrame$DOI <- tolower(L2S1DimensionsModFrame$DOI)

L2S1WOSModFrame <- WebOfScienceSet_Leit2_Sub1[c("Title", "Extra")]
L2S1WOSModFrame$Source <- rep("WebOfScience", each=length(L2S1WOSModFrame$Extra))
colnames(L2S1WOSModFrame) <- L2S1colnames_ergFrame
L2S1WOSModFrame$DOI <- tolower(L2S1WOSModFrame$DOI)

L2S1ScholarModFrame <- ScholarSet_Leit2_Sub1[c("Title", "Extra")]
L2S1ScholarModFrame$Source <- rep("Scholar", each=length(L2S1ScholarModFrame$Extra))
colnames(L2S1ScholarModFrame) <- L2S1colnames_ergFrame
L2S1ScholarModFrame$DOI <- tolower(L2S1ScholarModFrame$DOI)

#Vereinigung in gesamt frame
L2S1ergFrame <- merge(L2S1DimensionsModFrame, L2S1WOSModFrame, by = 'DOI', all = TRUE) %>%
  merge(L2S1ScholarModFrame, by = 'DOI', all = TRUE)

L2S1AnzahlDeduplizierterTreffer <- length(L2S1ergFrame$DOI)
L2S1AnzahlDuplikate <- (L2S1AnzahlTrefferDimensions + L2S1AnzahlTrefferWOS + L2S1AnzahlTrefferScholarDOIS) - L2S1AnzahlDeduplizierterTreffer

L1S2_ergebnis <- c(2,1,(L2S1AnzahlTrefferDimensions + L2S1AnzahlTrefferWOS + L2S1AnzahlTrefferScholarDOIS),
                   0, L2S1AnzahlTrefferDimensions, L2S1AnzahlTrefferWOS, L2S1AnzahlTrefferScholarDOIS, 
                   L2S1AnzahlDuplikate, L2S1AnzahlDeduplizierterTreffer)
ergFrame <- rbind(ergFrame,L1S2_ergebnis)






