# KHvalitetskontroll

Funksjoner for Kvalitetskontroll av KH/NH kuber.

# Installasjon

KOMMER...

# Bruk

Hele prosessen foregår i filen `Kvalitetskontroll.Rmd`. I øverste seksjon defineres alt som skal defineres, som sti til ny og gammel (for referanse) kube. 

UNDER ARBEID...

For å kontrollere en KUBE brukes filen `Kvalitetskontroll.Rmd`. Denne henter inn de nødvendige funksjonene som gjennomfører de ulike stegene av kvalitetskontrollen. Disse kjøres i rekefølge, og output kan inspiseres stegvis. Dersom ett steg er ok, går du videre til neste. 

Til slutt kan hele kvalitetskontrollen printes som en rapport, og lagres som dokumentasjon, ved å trykke på `Knit` øverst i RStudio.

**INPUT** skjer hovedsakelig i første kodechunk. Her defineres følgende: