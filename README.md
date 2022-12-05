# KHvalitetskontroll

Funksjoner for Kvalitetskontroll av KH/NH kuber.

# Installasjon

For å installere prosjektet, kjør:
```r
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_install(KHvalitetskontroll)
```
RStudio restarter når prosessen er ferdig, og prosjekt for KHvalitetskontroll åpnes automatisk. 

Prosjektet lagres her: 
C:/Users/dittBrukerNavn/helseprofil/KHvalitetskontroll.

# Oppstart

- Når prosjektet åpnes, vil du få spørsmål om du vil oppdatere til siste versjon. Velger du ja, vil siste versjon av alle filer lastes fra github. Velger du nei, beholder du filene slik de var, slik at du kan ta kopi av endringer du ønsker å beholde. 
- Alle filer som skal brukes ligger i mappen `USER`.

Ved oppstart kjøres følgende tre skript:
- updateproject.R: Oppdaterer filene dersom ønsket
- setup.R: laster inn pakker, og henter alle interne funksjoner fra github
- welcome.R: Genererer en velkomstbeskjed, og sjekker til slutt at alle pakkeversjoner er korrekte

# Oppstartsproblemer

- Får du beskjed om at en pakke ikke er installert, eller at prosjektet er ute av sync, vil det vanligvis være tilstrekkelig å kjøre `renv::restore()` i konsollen. Følg instruksene som dukker opp, og restart deretter prosjektet (Ctrl + Shift + F10)
- Virker ikke dette, prøv å lukke prosjektet å installer det på nytt (`kh_install()`..., du trenger ikke slette prosjektet først)

# Instruksjoner:
## Kvalitetskontroll del 1 (manuell kontroll)

- Bruk filen `/USER/Kvalitetskontroll_del1.Rmd`
- All input foregår i de to øverste kodechunkene. Her står det også instrukser om hvordan de ulike parametrene skal fylles inn. 
- Kvalitetskontrollen kan gjennomføres stegvis nedover i dokumentet ved å trykke på "play" i hver kodechunk (evt ved å plassere pekeren inni kodechunken og trykke Ctrl + Shift + Enter). Det står instrukser i hver kodechunk for hvordan input til funksjonen eventuelt kan endres ved behov. 
- Kommentarer kan legges til i rapporten utenfor kodechunkene dersom det er ønskelig å ha disse med i rapporten. Bruk gjerne punktlister (* eller -).
- Når alle stegene er gjennomført, kan rapporten lagres ved å kjøre `SaveReport()` i nederste kodechunk. Rapporten får da riktig filnavn, og lagres i mappesystem for validering.
    - **.Rmd-dokumentet må lagres først!!**
    
## Kvalitetskontroll del 2

- Bruk filen `/USER/Kvalitetskontroll_del2.Rmd`
- All input foregår i de to øverste kodechunkene. Her står det også instrukser om hvordan de ulike parametrene skal fylles inn. 
- `FormatData()` lager flaggede versjoner av de to kubene som skal sammenlignes, og en kombinert fil for sammenligning. I INPUT kan det defineres hvilke fildumper som skal lagres i mappesystem for validering. 