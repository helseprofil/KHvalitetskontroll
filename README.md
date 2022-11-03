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

- Når prosjektet åpnes, vil det automatisk lete etter eventuelle oppdateringer i GitHub og laste ned disse. 
- Dersom du får feilmelding ved oppstart, skyldes det som oftest at det er gjort endringer i R-profilen. For å oppdatere må du da kjøre installasjonsrutinen på nytt. 
- Alle filer som skal brukes ligger i mappen `USER`.

# Instruksjoner:
## Kvalitetskontroll del 1 (manuell kontroll)

- Bruk filen `/USER/Kvalitetskontroll_del1.Rmd`
- All input foregår i de to øverste kodechunkene. Her står det også instrukser om hvordan de ulike parametrene skal fylles inn. 
- Kvalitetskontrollen kan gjennomføres stegvis nedover i dokumentet ved å trykke på "play" i hver kodechunk (evt ved å plassere pekeren inni kodechunken og trykke Ctrl + Shift + Enter)
    - Output printes direkte under hver kodechunk
- Når alle stegene er gjennomført, kan rapporten lagres ved å kjøre `SaveReport()` i nederste kodechunk. Rapporten får da riktig filnavn, og lagres i mappesystem for validering.
    - .Rmd-dokumentet må lagres først
    
## Kvalitetskontroll del 2

- Bruk filen `/USER/Kvalitetskontroll_del2.Rmd`
- All input foregår i de to øverste kodechunkene. Her står det også instrukser om hvordan de ulike parametrene skal fylles inn. 
- `FormatData()` lager flaggede versjoner av de to kubene som skal sammenlignes, og en kombinert fil for sammenligning. I INPUT kan det defineres hvilke fildumper som skal lagres i mappesystem for validering. 



### Oversikt
- Funksjoner som starter med `Compare*` sammenligner elementer i den nye kuben med den gamle
- Funksjoner som starter med `Check*` sjekker elementer i den nye kuben
- Funksjoner som starter med `Plot*` genererer figurer