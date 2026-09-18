# Tata Steel-meldingen bij de Omgevingsdienst

Deze repository bevat de data en R-code achter het onderzoek van Follow the Money naar meldingen van Tata Steel bij de Omgevingsdienst Noordzeekanaalgebied (OD NZKG).

In het onderzoek analyseren we onder meer **op welke momenten van de dag Tata Steel afwijkingen van de normale bedrijfsvoering meldt** en of het verschil tussen meldingen overdag en 's nachts kan worden verklaard door onderhoudswerkzaamheden.

[Lees hier het bijbehorende artikel](https://www.ftm.nl/artikelen/overdag-meldt-tata-braaf-ontsnappende-gifwolken-maar-in-de-nacht-blijft-het-opvallend-stil).

## Data

### Meldingen Tata Steel

Voor dit onderzoek gebruikten we een dataset van de Omgevingsdienst Noordzeekanaalgebied, die is vrijgegeven na een Woo-verzoek van actiegroep Frisse Wind.

De dataset bevat **16.237 meldingen van Tata Steel uit de periode 2014 tot en met april 2025**. Het gaat om afwijkingen van de normale bedrijfsvoering die kunnen leiden tot hinder of schade in de omgeving of potentieel effect kunnen hebben op het milieu.

De Omgevingsdienst verstrekte de gegevens als PDF. We hebben deze PDF met `pdftotext -layout` omgezet naar platte tekst en vervolgens met R gereconstrueerd tot een gestructureerde dataset.

De jaren 2014 en 2025 zijn niet volledig. Voor onze hoofdanalyse gebruiken we daarom alleen de volledige kalenderjaren **2015 tot en met 2024**. Daarmee vallen 784 meldingen buiten de analyse en resteren **15.453 meldingen**.

### Zonsopkomst en zonsondergang

Van iedere melding is tot op de minuut bekend wanneer zij is gedaan. Deze tijdstippen hebben we gekoppeld aan openbare gegevens van het [Koninklijk Nederlands Meteorologisch Instituut (KNMI)](https://cdn.knmi.nl/system/ckeditor/attachment_files/data/000/000/208/original/tijden_van_zonopkomst_en_-ondergang_2023.pdf) over de tijdstippen van zonsopkomst en zonsondergang.

De voor de analyse gebruikte daglichtdata staan in:

```text
data/input/knmi_zonsopgang_ondergang_2015_2025.csv
```

## Methodologie en workflow

Eerst zijn de door de Omgevingsdienst vrijgegeven PDF's samengevoegd tot één bestand en met `pdftotext -layout` via de terminal omgezet in een tekstbestand.

[Deze PDF is hier te downloaden.](https://drive.google.com/file/d/1ug8d1PL0E2H7zYsSCDCr6HcqBpJh0x7l/view?usp=sharing)

Vervolgens worden de scripts in numerieke volgorde uitgevoerd.

### 1. Meldingen extraheren

[`01_meldingen_extraheren.R`](scripts/01_meldingen_extraheren.R) zet de met `pdftotext -layout` geëxtraheerde tekst om naar één rij per melding.

De oorspronkelijke PDF bevat een tabel waarvan toelichtingen over meerdere regels kunnen doorlopen. Het script:

* herkent het begin van iedere melding aan een datum en tijd;
* voegt vervolgregels van dezelfde melding samen;
* gebruikt de vaste tabelopmaak van `pdftotext -layout` om de kolommen te herkennen;
* herstelt regels waarin kolomscheidingen ontbreken of juist te vaak voorkomen;
* bevat expliciete correcties voor enkele uitzonderlijk opgemaakte regels;
* splitst onder meer postcode, soort voorval, stof en hoeveelheid;
* controleert dat alle 16.237 meldingen uiteindelijk uit de verwachte veertien velden bestaan.

Wanneer na de herstelregels nog een onbekende tabelstructuur overblijft, stopt het script in plaats van de melding automatisch verder te verwerken.

Tot slot wordt een willekeurige steekproef van honderd meldingen klaargezet voor visuele controle.

De uitvoer is:

```text
data/output/od_meldingen_uit_pdf.csv
```

### 2. Daglicht en onderhoud classificeren

[`02_daglicht_en_onderhoud_classificeren.R`](scripts/02_daglicht_en_onderhoud_classificeren.R) koppelt iedere melding op datum aan de KNMI-data en maakt de variabelen die nodig zijn voor de tijds- en onderhoudsanalyse.

Een melding geldt als gedaan tijdens **daglicht** wanneer het tijdstip:

* op of na zonsopkomst ligt; en
* vóór zonsondergang ligt.

Alle overige meldingen worden als **donker** geclassificeerd.

Het script controleert of iedere melding uit de gebruikte jaren aan een tijdstip van zonsopkomst en zonsondergang kon worden gekoppeld. Wanneer dit voor één of meer meldingen niet lukt, stopt het script.

Voor deze analyse gebruiken we alleen de volledige jaren **2015–2024**.

Het script maakt daarnaast indicatoren voor mogelijke onderhoudsmeldingen en schrijft afzonderlijke datasets weg voor:

* alle meldingen uit 2015–2024;
* alle meldingen uit 2020–2024;
* meldingen uit 2020–2024 na toepassing van de kernfilter;
* meldingen uit 2020–2024 na toepassing van de uitgebreidere onderhoudsfilter.

### 3. Corrigeren voor het aantal uren licht en donker

Over een kalenderjaar is er niet precies evenveel daglicht als duisternis. Daardoor zou je zelfs bij een volledig gelijkmatige verdeling van meldingen niet exact evenveel meldingen tijdens licht als tijdens donker verwachten.

We corrigeren de waargenomen licht-donkerverhouding daarom voor het totale aantal beschikbare uren daglicht en donker:

```text
gecorrigeerde licht-donkerratio =
(meldingen tijdens licht / meldingen tijdens donker)
/
(uren daglicht / uren donker)
```

Een gecorrigeerde ratio van `1` betekent dat er per beschikbaar uur evenveel meldingen tijdens daglicht als tijdens donker zijn gedaan. Een ratio boven `1` betekent dat er relatief meer meldingen tijdens daglicht zijn gedaan.

Omdat niet alle analyses dezelfde periode bestrijken, berekenen we deze correctiefactor afzonderlijk voor:

* **2015–2024** voor de analyse van alle volledige jaren;
* **2020–2024** voor de analyses waarbij de toelichting bij meldingen wordt gebruikt.

### 4. Onderhoud als mogelijke verklaring

Tata Steel stelt dat het verschil tussen dag en nacht mede kan worden verklaard doordat het bedrijf overdag meer onderhoud uitvoert.

Voor meldingen uit **2015–2019** bevat de dataset echter nog niet of nauwelijks inhoudelijke toelichting. Een tekstuele analyse van mogelijk onderhoud is voor deze periode daardoor niet betrouwbaar. We beperken dit deel van de analyse daarom tot **2020–2024**. Daarmee vallen 7.352 meldingen uit 2015–2019 buiten deze aanvullende analyse.

Om mogelijke onderhoudsmeldingen te vinden hebben we een taalmodel gebruikt om woorden en formuleringen te inventariseren die in de toelichtingen op onderhoud, reparaties of werkzaamheden kunnen wijzen.

Het taalmodel is alleen gebruikt voor het inventariseren van mogelijke zoektermen. De uiteindelijke selectie van meldingen gebeurt met vaste, in het R-script opgenomen reguliere expressies. Individuele meldingen worden dus niet door een taalmodel geclassificeerd.

#### Kernfilter

Voor de primaire analyse gebruiken we een lijst van **24 regexpatronen** die relatief rechtstreeks wijzen op onderhoud, reparaties of technische werkzaamheden.

Deze patronen zoeken onder meer naar varianten van:

```text
onderhoud
reparatie
werkzaamheden
vervangen
herstel
verhelpen
aanpassen
revisie
monteur
montage
demonteren
laswerk
slijpwerk
storingsdienst
technische dienst
ombouwen
vernieuwen
```

Een melding wordt buiten de primaire analyse gehouden wanneer de toelichting minimaal één van de kernpatronen bevat.

We hebben de gevonden treffers bewust niet handmatig gecorrigeerd voor vals-positieven. Een melding waarin bijvoorbeeld alleen staat dat een reparatie nodig is, kan daardoor eveneens worden uitgesloten. Daarmee kiezen we voor een conservatieve test van Tata Steels verklaring.

#### Uitgebreide onderhoudsfilter

Als gevoeligheidsanalyse gebruiken we daarnaast een ruimere filter.

Daarbij worden **eerst alle meldingen verwijderd die op de kernfilter matchen**. Uit de overgebleven dataset verwijderen we vervolgens ook meldingen die matchen op **35 aanvullende regexpatronen**.

Deze aanvullende patronen zoeken onder meer naar termen rond:

```text
testen
inspecteren
reinigen
afstellen
proefdraaien
stilstand
onderhoudsstops
uit bedrijf nemen
opstarten
afdichten
plaatsen
tijdelijke situaties
bypasses
steigers
firma's
aannemers
```

Deze uitgebreidere filter is bewust ruimer en daardoor ook gevoeliger voor vals-positieven. De kernfilter gebruiken we daarom voor de primaire uitkomsten van het onderzoek; de uitgebreidere filter dient als gevoeligheidsanalyse.

### 5. Meldingen analyseren

[`03_meldingen_analyseren.R`](scripts/03_meldingen_analyseren.R) berekent de uiteindelijke resultaten.

Het script maakt onder meer:

* een samenvatting van meldingen tijdens licht en donker;
* de ruwe en voor daglengte gecorrigeerde licht-donkerratio;
* een vergelijking van de ongefilterde, kerngefilterde en uitgebreid gefilterde datasets;
* aantallen meldingen per inrichting;
* aantallen en licht-donkerratio's per seizoen;
* aantallen meldingen per jaar en maand;
* meldingen per uur van de dag;
* meldingen ten opzichte van zonsopkomst;
* gemiddelde tijdstippen van zonsopkomst en zonsondergang per maand.

De afgeleide tabellen worden opgeslagen in:

```text
data/analyse/
```

## Belangrijkste uitkomsten

Na uitsluiting van de onvolledige jaren 2014 en 2025 bevat de hoofdanalyse **15.453 meldingen**.

Na beperking tot de jaren 2020–2024 en toepassing van de kernfilter voor mogelijke onderhoudsmeldingen resteren **6.034 meldingen**:

| Tijdstip         | Aantal meldingen |
| ---------------- | ---------------: |
| Tijdens daglicht |            4.328 |
| In het donker    |            1.706 |
| **Totaal**       |        **6.034** |

Na correctie voor het verschil in het aantal beschikbare uren daglicht en donker bedraagt de licht-donkerratio ongeveer **2,4**.

Voor alle meldingen uit de volledige jaren 2015–2024 bedraagt de gecorrigeerde licht-donkerratio ongeveer **2,5**.

Het verwijderen van mogelijke onderhoudsmeldingen verandert de verhouding tussen dag en nacht in deze analyse dus nauwelijks.

De uitgebreidere onderhoudsfilter maakt het verschil niet kleiner. Deze gevoeligheidsanalyse levert een gecorrigeerde licht-donkerratio van ongeveer **2,8** op. Omdat deze filter veel ruimere zoekpatronen bevat en daardoor gevoeliger is voor vals-positieven, gebruiken we de uitkomst van de kernfilter als primaire analyse.

### Controleanalyse coronajaren

Omdat Tata Steel aangaf dat het bedrijf tijdens de coronajaren 2020 en 2021 minder onderhoud uitvoerde, hebben we voor deze twee jaren ook gekeken naar het verschil tussen meldingen tijdens licht en donker zonder toepassing van een onderhoudsfilter.

De gecorrigeerde licht-donkerratio in deze twee jaren was 2,4. Dat is slechts beperkt lager dan de ratio over de volledige periode 2015–2024 (2,5). Ook in jaren waarin volgens Tata minder onderhoud plaatsvond, bleef het verschil tussen meldingen tijdens licht en donker dus grotendeels bestaan.

## Beperkingen

De analyse kent enkele belangrijke beperkingen.

De gegevens uit **2014 en 2025** bestrijken geen volledige kalenderjaren. Deze jaren zijn daarom uitgesloten van de hoofdanalyse.

Voor de onderhoudsanalyse gebruiken we alleen **2020–2024**, omdat oudere meldingen doorgaans onvoldoende toelichting bevatten om mogelijke onderhoudswerkzaamheden met een tekstfilter te herkennen.

De onderhoudsfilters zijn gebaseerd op zoekpatronen en vormen geen perfecte classificatie van onderhoud. Zowel vals-positieven als vals-negatieven zijn mogelijk. We hebben de resultaten van de kernfilter bewust niet handmatig gecorrigeerd voor vals-positieven.

De uitgebreidere filter bevat nog ruimere indicatoren voor mogelijke werkzaamheden. De kans op vals-positieven is daar daarom groter. Deze analyse gebruiken we alleen als gevoeligheidsanalyse.

Tijdens de analyse merkten we dat een bijwerking van een bestaande melding soms als een nieuwe melding wordt geregistreerd. Daardoor kunnen in de dataset meldingen voorkomen die inhoudelijk met elkaar samenhangen.

We hebben geprobeerd mogelijke dubbelingen te identificeren door meldingsnummers uit de toelichtingen te extraheren en hiermee verbanden tussen meldingen te leggen. Deze aanpak bleek echter niet betrouwbaar genoeg: dezelfde meldingsnummers bleken soms ook voor verschillende meldingen gebruikt te worden.

Het is daarom op basis van de beschikbare data niet mogelijk om dubbelingen betrouwbaar te verwijderen. We kunnen ook niet vaststellen in hoeverre deze mogelijke dubbelingen de resultaten beïnvloeden.

Ten slotte gebruiken we zonsopkomst en zonsondergang als grens tussen licht en donker. Schemering, bewolking en kunstmatige verlichting worden niet afzonderlijk meegenomen.

## Reproduceren

De analyse is geschreven in R.

De gebruikte packages zijn onder meer:

```text
tidyverse
dplyr
readr
readxl
stringr
lubridate
hms
clipr
```

Voor de extractie van de oorspronkelijke PDF is daarnaast [`pdftotext`](https://poppler.freedesktop.org/) uit Poppler nodig.

De PDF kan bijvoorbeeld als volgt naar platte tekst worden geconverteerd:

```bash
pdftotext -layout \
  "data/raw/Meldingen OD alles.pdf" \
  "data/raw/Meldingen OD alles.txt"
```

Vervolgens kunnen de drie R-scripts in deze volgorde worden uitgevoerd:

```text
01_meldingen_extraheren.R
02_daglicht_en_onderhoud_classificeren.R
03_meldingen_analyseren.R
```

## Repositorystructuur

```text
.
├── README.md
├── data
│   ├── raw
│   │   ├── link_naar_pdf.txt
│   │   └── Meldingen OD alles.txt
│   ├── input
│   │   └── knmi_zonsopgang_ondergang_2015_2025.csv
│   ├── output
│   │   ├── od_meldingen_uit_pdf.csv
│   │   └── tijdsanalyse
│   │       ├── meldingen_complete_jaren_alles.csv
│   │       ├── meldingen_2020_2024_alles.csv
│   │       ├── meldingen_2020_2024_zonder_onderhoud_kern.csv
│   │       └── meldingen_2020_2024_zonder_onderhoud_extra.csv
│   └── analyse
│       └── [afgeleide analysetabellen]
└── scripts
    ├── 01_meldingen_extraheren.R
    ├── 02_daglicht_en_onderhoud_classificeren.R
    └── 03_meldingen_analyseren.R
```


## Contact

Vragen over de data of methode kunnen worden gemeld via een GitHub issue of worden gestuurd aan [sjors.hofstede@ftm.nl](mailto:sjors.hofstede@ftm.nl).
