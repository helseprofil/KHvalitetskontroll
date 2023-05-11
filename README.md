<div id="toc">
  <style>
  #toc {
  position: fixed;
  top: 50px;
  right: 10px;
  background-color: #f5f5f5;
  border: 1px solid #ddd;
  padding: 10px;
  border-radius: 5px;
  max-height: 80vh;
  overflow-y: auto;
  }
  
  #toc ul {
    list-style: none;
    margin: 0;
    padding: 0;
  }
  
  #toc li {
    margin: 5px 0;
  }
  
  #toc a {
    text-decoration: none;
    color: #333;
  }
  
  #toc a:hover {
    text-decoration: underline;
  }
</style>
  <h2>Innhold</h2>
  <ul>
    <li><a href="#installasjon">Installasjon</a></li>
    <li><a href="#oppstart">Oppstart</a></li>
    <li><a href="#feilsøking">Feilsøking ved oppdaterings- og oppstartsproblemer</a></li>
    <li><a href="#rprofil">Kan ikke oppdatere Rprofilen</a></li>
  </ul>
</div>

# KHvalitetskontroll

Funksjoner for Kvalitetskontroll av KH/NH kuber.

# R versjon
- Prosjektet er per nå kompatibelt med R versjon 4.1. I versjon 4.2 og nyere er det noen endringer i encoding, som gjør at vi får problemer med norske spesialbokstaver. 

<h1 id="installasjon">Installasjon</h1>

For å installere prosjektet, kjør:
```r
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_install(KHvalitetskontroll)
```
RStudio restarter når prosessen er ferdig, og prosjekt for KHvalitetskontroll åpnes automatisk. 

Prosjektet lagres her: 
C:/Users/dittBrukerNavn/helseprofil/KHvalitetskontroll.

<h1 id="oppstart">Oppstart</h1>

- Når prosjektet åpnes, vil du få spørsmål om du vil oppdatere til siste versjon. Velger du ja, vil siste versjon av alle filer lastes fra GitHub. Velger du nei, beholder du filene slik de var, slik at du kan ta kopi av endringer du ønsker å beholde.
- Funksjonene lastes alltid direkte fra GitHub, men du bør alltid ha siste versjon av brukerfilene.
- Alle filer som skal brukes ligger i mappen `USER`.

<h1 id="feilsøking">Feilsøking ved oppdaterings- og oppstartsproblemer</h1>

<h2 id="rprofil">Kan ikke oppdatere Rprofilen</h2>
Dersom du har en tidligere versjon installert kan det oppstå problemer ved oppdatering ettersom .Rprofilen er endret i nyere tid. Dette er et script som kjøres ved oppstart, og denne kan ikke oppdateres på samme måte som andre filer. Dette kan løses på en av flere måter:

1. I Terminal-vinduet, skriv: `git pull`, forsøk så å restarte (Ctrl + Shift + F10)
2. Forsøk å installere prosjektet på nytt som beskrevet over
3. Slett prosjektet og installer på nytt

## Merge conflicts
Dersom du får feilmelding om merge conflict f.eks. (lib2git::checkout_tree: 1 conflict prevents checkout), så skyldes det sannsynligvis at du har lagret en endring i en lokal fil som ikke er kompatibel med endringene som er gjort i hovedfilene. I Git-vinduet vil du kunne se hvilken fil dette gjelder. Dette kan løses på en av flere måter.

1. I terminal-vinduet, skriv: `git pull`, forsøk så å restarte (Ctrl + Shift + F10)
2. I terminal-vinduet, skriv: `git stash`, deretter `git pull`, forsøk så å restarte (Ctrl + Shift + F10)
3. Dersom filen er 'renv/settings.json' er løsningen å slette denne filen og restarte prosjektet. 
4. If all else fails, slett prosjektet og installer på nytt. 

## renv/activate.R
Får du feilmeldingen: "In file(filename, "r", encoding = encoding) :   cannot open file 'renv/activate.R", løses dette ved å kjøre `renv::activate()` i konsollen.

## Problemer med pakker eller pakkeversjoner
Får du advarsel om at prosjektet ikke er synkronisert med lockfilen, løses dette vanligvis ved å bruke `renv::restore()` i konsollen og følge innstruksene som dukker opp. 

# Brukerveiledning:
## Kvalitetskontroll del 1 (manuell kontroll)

Bruk filen `/USER/Kvalitetskontroll_del1.Rmd`

- Kvalitetskontrollen kan gjennomføres stegvis nedover i dokumentet ved å trykke på "play" i hver kodechunk (evt ved å plassere pekeren inni kodechunken og trykke Ctrl + Shift + Enter). Det står instrukser i hver kodechunk for hvordan input til funksjonen eventuelt kan endres ved behov. 
- Kommentarer kan legges til i rapporten utenfor kodechunkene dersom det er ønskelig å ha disse med i rapporten. Bruk gjerne punktlister (* eller -). Det er også lagt opp til en kommentarliste øverst i filen. 

### Innlesing av filer
Ny og gammel fil leses inn med funksjonen `ReadFiles()` og følgende argumenter.
- dfnew/dfold: Angi navn på ny og gammel fil som fullt filnavn med datotag. 
- foldernew/folderold: Hvilke mapper skal de ulike filene leses fra. Aksepterer "QC", "DATERT", eller 4-sifret årstall for NESSTAR-mappene. 
- modusnew/modusold: KH eller NH. 

Nyttig info: 
- Skal du bare lese inn en fil (finnes ikke gammel), la de tre siste argumentene være NULL. 
- Du vil varsles om ny fil inneholder TELLER, NEVNER, sumTELLER eller sumNEVNER, for å kunne ta stilling til om dette skal være med ut i publikasjon.
- Funksjonen vil automatisk endre navn på kolonnene "antall", "Crude", "Adjusted", "sumteller", "sumnevner", "smr", "FLx" og "utdanningsnivå", du får beskjed om dette i output. og hvilke kolonner som finnes i hver av filene. 
    - Du kan manuelt gi nye kolonnenavn i neste kodechunk, med funksjonen `RenameColumns()`

Noen flere inputparametre er nødvendig:
- `PROFILEYEAR`: styrer hvilken mappe rapporten lagres i
- `PRIKKval` og `PRIKKlimit`: hvilken variabel er filen prikket på og hva er grensen? Dette brukes for å sjekke om det er noen verdier under grensen i filen.  
- `GROUPdims`: Brukes for å gruppere ouput på ønskede strata. Dette gjelder funksjonene som sammenligner prikking i ny/gammel fil, og for sammenligning på tvers av geonivåer i den nye filen.
- `CompareGEO` kan overstyre om funksjonene som sammenligner GEO-nivåer skal kjøres. Denne er som standard satt til `TRUE`, men kan settes til `FALSE` om du ikke vil kjøre disse funksjonene. 

### Sjekk av kolonner og dimensjoner

`CompareCols()` sjekker om det er noen nye (ikke med i gammel fil) eller utgåtte (i gammel men ikke i ny fil) kolonner. Sjekker også om det er med uprikkede QC-kolonner (dersom du sjekker på QC-kube)
`CompareDims()` sjekker hvor mange nivåer det er i hver dimensjon i ny fil, og om det er kommet inn noen nye nivåer/tatt bort noen nivåer sammenlignet med gammel fil. 

### Sjekk av prikking
`CheckPrikk()` sjekker om det er noen verdier under prikkegrensen. Bruker input-verdiene PRIKKval og PRIKKlimit. 
`ComparePrikk()` oppsummerer antall rader med ulike SPVFLAGG i den nye og gamle filen, og gir absolutt og relativ forskjell. Grupperes på GROUPdims, angitt i input. 
`ComparePrikkTS()` sjekker hvor mange tidsserier som inneholder 0 (komplette), 1, 2, ...., max (serieprikket) antall prikker i tidsserien. Gir absolutt antall og som andel av totalt antall tidsserier. 

### Sammenligning av geonivåer
`CompareXXXYYY()` summerer opp antall observasjoner på ulike geonivåer, for å sjekke om Bydel summerer opp til Kommune, kommune til fylke og fylke til land, og om Oslo kommune og fylke er identiske. Output grupperes på dimensjonene angitt i GROUPdims i input. 
- Funksjonene bruker sumTELLER_uprikk > sumTELLER > TELLER. Dersom ingen av disse er med i filen vil det ikke bli noe output. 
- Funksjonene printer ut hvilken variabel som er sammenlignet, og en liste av alle GEO-koder som er inkludert i sammenligningen. 

### Sjekk av ukjent bydel
`UnspecifiedBydel()` summerer opp antallet (teller og nevner) observasjoner for alle bydeler i en kommune og sammenligner dette mot kommunetallet. Forskjellen tilsvarer 'ukjent bydel'. Er denne stor, kan det være grunn til å ikke vise bydelstall. 
- Bruker sumTELLER_uprikk > sumTELLER > TELLER_uprikk > TELLER for å sammenligne teller
- Bruker sumNEVNER_uprikk > sumNEVNER > NEVNER_uprikk > NEVNER for å sammenligne nevner
- Argumentet `minteller_kommune` kan brukes for å filtrere bort de minste strataene, ved å sette et minimumsantall for kommunetelleren.
- Argumentet `maxrows` kan brukes for å begrense størrelsen på output. Som standard er denne satt til `TRUE`, og begrenser output til maks 8000 rader (størst diff) fordelt på antallet tilgjengelige kommuner og antall variabler. Kan settes til `FALSE` for å printe alt, men tabellen vil da kunne bli stor og gjøre rapporten treg.

### Tidsserier på landsnivå
`PlotTimeSeries()` genererer, for hver dimensjon, en tidsserie på landsnivå med en linje per kategori i dimensjonen. Dette er nyttig for å avdekke systematisk feilkoding (linjene bytter plass). 
- Alle dimensjoner utover den som plottes aggregeres til total dersom total ikke allerede eksistere (eks. KJONN == 0). For TELLER-kolonner aggregeres disse til totalsum, mens andre kolonner aggregeres til gjennomsnitt.
- Plottene lagres i et objekt som heter TS. For å vise plottene i RStudio, kan man derfor skrive 'TS' i konsollen. 
- For å tilpasse plottestørrelsen til antallet paneler er denne sjekken delt i to funksjoner. `PrintTimeseries()` printer plottene og genererer overskrifter i rapporten.

### Lagre rapport
`SaveReport()` lagrer en HTML-rapport i riktig mappe angitt av `PROFILEYEAR` i input. For å gjøre dette må man først:
1. Laste inn filene, slik at man har dfnew i environment. Denne trengs for å lage riktig mappe.
2. Lagre .Rmd-filen, da det er denne som kjøres fra disk, og alle inputparametre som brukes er de som er i den lagrede filen. 

Det er viktig at alle de enkelte stegene i rapporten kjører uten problem, ellers vil rapporten ikke kunne genereres. Dersom en kodechunk ikke fungerer, og det ikke er noen løsning for å få den til å fungere, kan man som reserveløsning legge til eval = F som chunk setting, og denne vil da hoppes over. 
- Dette vil da se slik ut: ```{r eval = F}```
- Se eksempel på chunken med `SaveReport()`, som ikke skal evalueres under rapportgenereringen for å unngå at rapporten bare går i loop. 

- Dersom filnavnene blir for lange for Windows, kan argumentet `shortname` settes til `TRUE`. Da blir kubenavnet tatt ut av filnavnet, som da bare blir datotag. 
- Man kan også sette et eget navn på filen ved hjelp av argumentet `savename`, for eksempel savename = "testnavn". 
    
## Kvalitetskontroll del 2

- Bruk filen `/USER/Kvalitetskontroll_del2.Rmd`
- All input foregår i de to øverste kodechunkene. Her står det også instrukser om hvordan de ulike parametrene skal fylles inn. 
- `FormatData()` lager flaggede versjoner av de to kubene som skal sammenlignes, og en kombinert fil for sammenligning. I INPUT kan det defineres hvilke fildumper som skal lagres i mappesystem for validering. 