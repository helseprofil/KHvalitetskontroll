# KHvalitetskontroll

Funksjoner for Kvalitetskontroll av KH/NH kuber.

# Installasjon

For å installere funksjonene, kjør:
```r
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_install(KHvalitetskontroll)
```
RStudio restarter når prosessen er ferdig. Prosjekt for KHvalitetskontroll åpnes automatisk. Herfra skal du kjøre komando:

```r
renv::restore()
```

Prosjektet lagres her: 
C:/Users/dittBrukerNavn/helseprofil/KHvalitetskontroll.

# Bruk

Hele prosessen foregår i filen `Kvalitetskontroll.Rmd`. I øverste seksjon defineres alt som skal defineres, som sti til ny og gammel (for referanse) kube. 

Etter at all input er definert, gjennomføres kvalitetskontrollen stegvis nedover i filen. Output skal vises under funksjonen. 

Når alle stegene er gjennomført, kan du generere en HTML-rapport ved å trykke på `Knit` (garnnøstet) i menyen øverst. Denne lagres som Kvalitetskontroll.html, og kan åpnes i nettleser. For å lagre denne til senere, gi filen nytt navn og flytt den til ønsket mappe, ellers vil den bli overskrevet neste gang du lager en rapport. 



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