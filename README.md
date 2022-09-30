# KHvalitetskontroll

Funksjoner for Kvalitetskontroll av KH/NH kuber.

# Installasjon

For å installere funksjonene, kjør:
```r
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_install(KHvalitetskontroll)
```
RStudio restarter når prosessen er ferdig, og prosjekt for KHvalitetskontroll åpnes automatisk. 

Prosjektet lagres her: 
C:/Users/dittBrukerNavn/helseprofil/KHvalitetskontroll.

# Oppstart

- Når prosjektet åpnes, vil det automatisk lete etter eventuelle oppdateringer i GitHub og laste ned disse. 
- Alle filer som skal brukes ligger i mappen `USER`.
- Kvalitetskontroll del 1 (manuell sjekk) foregår i `Kvalitetskontroll_del1.Rmd`

# Instruksjoner:
## Kvalitetskontroll del 1 (manuell kontroll)

- Bruk filen `/USER/Kvalitetskontroll_del1.Rmd`
- All input foregår i de to øverste kodechunkene. Her står det også instrukser om hvordan de ulike parametrene skal fylles inn. 
- Kvalitetskontrollen kan gjennomføres stegvis nedover i dokumentet ved å trykke på "play" i hver kodechunk (evt ved å plassere pekeren inni kodechunken og trykke Ctrl + Shift + Enter)
    - Output printes direkte under hver kodechunk
- Når alle stegene er gjennomført, kan du generere en HTML-rapport ved å trykke på `Knit` (garnnøstet) i menyen øverst. Denne lagres som Kvalitetskontroll_del1.html, og kan åpnes i nettleser. 
- For å lagre denne til senere, gi filen nytt navn og flytt/kopier den til ønsket mappe, ellers vil den bli overskrevet neste gang du lager en rapport. 



# UNDER ARBEID...


## Laste inn filer

Hvordan laste filer...

## Input

Hva må defineres og hvordan


## Oversikt
- Funksjoner som starter med `Compare*` sammenligner elementer i den nye kuben med den gamle
- Funksjoner som starter med `Check*` sjekker elementer i den nye kuben

UNDER ARBEID...

For å kontrollere en KUBE brukes filen `Kvalitetskontroll.Rmd`. Denne henter inn de nødvendige funksjonene som gjennomfører de ulike stegene av kvalitetskontrollen. Disse kjøres i rekefølge, og output kan inspiseres stegvis. Dersom ett steg er ok, går du videre til neste. 

Til slutt kan hele kvalitetskontrollen printes som en rapport, og lagres som dokumentasjon, ved å trykke på `Knit` øverst i RStudio.

**INPUT** skjer hovedsakelig i første kodechunk. Her defineres følgende: