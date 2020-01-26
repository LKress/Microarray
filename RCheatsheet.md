# R Cheatsheet

## Funktionen auf dem System

getwd() --> print working directory

setwd() --> working directory ändern

dir() --> ordner/dateien im aktuellen system

savehistory() --> speichert die history im aktuellen verzeichnis, falls nicht anders angegeben

ls() --> vergebene Variablen

## Hilfe Funktionen

help()

example()

## Array Funktionen

### Array Initialisierung

x = 1:10 --> array mit Werten 1 bis 10

assign("x", 1:10) --> array mit Werten 1 bis 10

x <- c(2,4,6,2,1,5,7,9,4,2) --> array mit fest definierten Werten

### Dimensionsfunktionen

dim(x)

length(x)

mode(x) --> datentyp von x

### Zugriffsfunktionen

x[n]

x[4:7]

x[c(4,8,3,5)]

x[x>4]

which(x == 7)

### Werte löschen

x[-3]

### Funktionen über mehrere Arrays

x+y

cbind(x,y)

rbind(x,y)


### Sortierung

sort(x)

is.unsorted(x)

order(x)

x[order(x)]

### Funktionen auf einem Array

t(x)

colnames(a) = c("eins", "zwei", "drei", "vier", "fuenf", "sechs")

rownames(a) = c("one", "two", "three", "four", "five")

sapply(1:10, log)

### Dataframe

#### Initialisierung

b = as.data.frame(a)

d = data.frame (Name=c("Schmidt","Mueller","Meier"), Alter=c(17,25,34)) 

x = read.delim("lc7b047rex.DAT") --> von datei einlesen

#### Zugriff

b$vier

b["four",]

b[4,3]



