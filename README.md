# KHvalitetskontroll

Funksjoner for Kvalitetskontroll av KH/NH kuber.

# Installasjon

For å bruke

# Bruk

For å kontrollere en KUBE brukes filen `Kvalitetskontroll.Rmd`. Denne henter inn de nødvendige funksjonene som gjennomfører de ulike stegene av kvalitetskontrollen. Disse kjøres i rekefølge, og output kan inspiseres stegvis. Dersom ett steg er ok, går du videre til neste. 

Plott lagres i egne mapper på forskningsserver. 

Til slutt kan hele kvalitetskontrollen printes som en rapport, og lagres som dokumentasjon, ved å trykke på `Knit` øverst i RStudio.

**INPUT** skjer hovedsakelig i første kodechunk. Her defineres følgende:

- Sti til ny KUBE som skal kontrolleres
- Sti til gammel KUBE som brukes som sammenligningsgrunnlag for deler av kvalitetskontrollen
- Prikkegrense
