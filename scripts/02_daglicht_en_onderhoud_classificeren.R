# =============================================================================
# DOEL VAN DIT SCRIPT
#
# Dit script neemt de gestructureerde Tata Steel-meldingen uit script 01 en:
# 1. maakt tijdsvariabelen voor iedere melding;
# 2. koppelt iedere melding aan KNMI-data over zonsopkomst en zonsondergang;
# 3. classificeert meldingen als licht of donker;
# 4. markeert mogelijke onderhoudsmeldingen met een kern- en extra woordenlijst;
# 5. maakt de vier datasets die in script 03 worden geanalyseerd.
#
# Input:
# - data/output/od_meldingen_uit_pdf.csv
# - data/input/knmi_zonsopgang_ondergang_2015_2025.csv
#
# Output:
# - vier CSV-bestanden in data/output/tijdsanalyse/
# =============================================================================

library(dplyr)
library(readxl)
library(readr)
library(stringr)
library(lubridate)

# ========================================
# 0. INLEZEN EN PADEN
# ========================================
# Hier leggen we eerst vast waar de tussenbestanden worden opgeslagen en
# lezen we de meldingen en de voorbereide KNMI-daglichtdata in.
#


output_dir <- file.path(
  "data",
  "output",
  "tijdsanalyse"
)

alle_meldingen_path <- file.path(
  output_dir,
  "meldingen_complete_jaren_alles.csv"
)

meldingen_toelichting_path <- file.path(
  output_dir,
  "meldingen_2020_2024_alles.csv"
)

exclusief_onderhoud_kern_path <- file.path(
  output_dir,
  "meldingen_2020_2024_zonder_onderhoud_kern.csv"
)

exclusief_onderhoud_extra_path <- file.path(
  output_dir,
  "meldingen_2020_2024_zonder_onderhoud_extra.csv"
)

# De meldingen komen rechtstreeks uit script 01. De KNMI-data bevat per
# kalenderdatum onder meer zonsopkomst, zonsondergang en het aantal uren daglicht.
meldingen_df <- read_csv("data/output/od_meldingen_uit_pdf.csv")
daglicht_df <- read_csv2("data/input/knmi_zonsopgang_ondergang_2015_2025.csv")

#Volledige jaren in dataset
complete_jaren <- 2015:2024

#Volledige jaren met toelichitng
jaren_toelichting <- 2020:2024

# =============================================================================
# 1. FUNCTIES
# =============================================================================
# Twee hulpfuncties worden later gebruikt om tijdstippen rekenbaar te maken en
# namen van Tata Steel-inrichtingen consistenter weer te geven.
#

# Zet een tijd als "13:45" of "13:45:00" om naar het aantal minuten na 00:00.
# Hierdoor kunnen meldingstijd, zonsopkomst en zonsondergang numeriek met elkaar
# worden vergeleken. Ongeldige tijden worden NA.
tijd_naar_minuten <- function(x) {
  x <- str_squish(as.character(x))
  
  parts <- str_split_fixed(x, ":", 3)
  
  uren <- suppressWarnings(as.integer(parts[, 1]))
  minuten <- suppressWarnings(as.integer(parts[, 2]))
  
  result <- uren * 60L + minuten
  
  invalid <- (
    is.na(uren) |
      is.na(minuten) |
      uren < 0L |
      uren > 23L |
      minuten < 0L |
      minuten > 59L
  )
  
  result[invalid] <- NA_integer_
  result
}

# Schoont de naam van de installatie op.
# Bedrijfsvoorvoegsels, een eventueel voorliggend streepje, tekst tussen haakjes
# aan het begin en de afsluitende rechtsvorm worden verwijderd.
clean_inrichting_naam <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    str_remove("^Tata Steel IJmuiden B\\.V\\.\\s*") %>%
    str_remove("^-\\s*") %>%
    str_remove("^\\([^)]+\\)\\s*") %>%
    str_remove("\\s+B\\.V\\.$") %>%
    str_squish()
}

# =============================================================================
# 3. MELDINGEN VOORBEREIDEN
# =============================================================================
# Van iedere melding maken we de variabelen die nodig zijn voor de tijdsanalyse:
# datum/tijd, jaar, maand, seizoen, minuut van de dag en uur van de dag.
# Daarnaast worden enkele bronkolommen hernoemd en de inrichtingsnaam opgeschoond.
#

meldingen <- meldingen_df %>%
  transmute(
    # Technische rij-ID, bruikbaar bij latere grafieken.
    incident_id = row_number(),
    
    # Combineer datum en tijd tot één tijdstempel in de Nederlandse tijdzone.
    # 'datum' is bij het inlezen al een Date-object en hoeft dus niet opnieuw
    # met dmy() te worden geparsed.
    datum_tijd = ymd_hms(
      paste(
        datum,
        tijd
      ),
      tz = "Europe/Amsterdam",
      quiet = TRUE
    ),
    
    datum = datum,
    
    tijd,
    
    jaar = year(datum),
    maand = month(datum),
    maand_name = month(
      datum,
      label = TRUE,
      abbr = TRUE
    ),
    
    # Deel iedere melding in volgens de meteorologische seizoenen.
    seizoen_meteorologisch = case_when(maand %in% c(3:5) ~ "Voorjaar",
                                    maand %in% c(6:8) ~ "Zomer",
                                    maand %in% c(9:11) ~ "Herfst",
                                    maand %in% c(12,1,2) ~ "Winter"),
    
    # Zet het meldingstijdstip om naar minuten sinds middernacht. Dit is de
    # maat waarmee verderop licht/donker wordt bepaald.
    minuut_van_dag = tijd_naar_minuten(
      tijd
    ),
    
    uur = minuut_van_dag %/% 60L,
    minuut_van_uur = minuut_van_dag %% 60L,
    
    inrichting = inrichting,
    inrichting_schoon = clean_inrichting_naam(
      inrichting
    ),
    
    stof = vrijgekomen_stof,
    
    hoeveelheid_stof = hoeveelheid,
    
    toelichting
  )



# =============================================================================
# 4. DAGLICHT AAN INCIDENTEN KOPPELEN
# =============================================================================
# We houden alleen de volledige kalenderjaren 2015-2024 over en koppelen elke
# melding op datum aan de KNMI-waarden voor die specifieke dag.
#

meldingen_daglicht <- meldingen %>%
  #We filteren incomplete jaren weg voor betrouwbare seizoensanalyse
  filter(jaar %in% complete_jaren) %>% 
  left_join(
    daglicht_df %>%
      select(
        datum,
        opkomst,
        ondergang,
        opkomst_minutes,
        ondergang_minutes,
        daglicht_uren
      ),
    by = "datum"
  )

# Check: is join goed gegaan
# Deze controle is bewust hard: als ook maar één melding geen zonsopkomst of
# zonsondergang heeft gekregen, stopt het script zodat de analyse niet ongemerkt
# met ontbrekende daglichtinformatie doorgaat.
missing_daglicht_count <- meldingen_daglicht %>% filter(
  is.na(opkomst_minutes) |
    is.na(ondergang_minutes)
)

if (nrow(missing_daglicht_count) > 0) {
  stop(
    paste0(
      nrow(missing_daglicht_count),
      " incidenten konden niet aan daglichtdata worden gekoppeld."
    ),
    call. = FALSE
  )
}

# =============================================================================
# 5. LICHT/DONKER EN AFSTAND TOT ZONSOPKOMST/-ONDERGANG
# =============================================================================
# Per melding bepalen we of het tijdstip binnen de daglichtperiode valt. Ook
# berekenen we hoeveel uur een melding vóór of na zonsopkomst en zonsondergang
# plaatsvond. De afgeronde afstanden worden later gebruikt voor aggregaties.
#

meldingen_daglicht_analyse <- meldingen_daglicht %>%
  mutate(
    # Een incident geldt als licht wanneer het tijdstip:
    # - op of na zonsopkomst valt;
    # - vóór zonsondergang valt.
    is_licht = (
      minuut_van_dag >= opkomst_minutes &
        minuut_van_dag < ondergang_minutes
    ),
    
    # Donker is hier exact het complement van de hierboven gedefinieerde
    # daglichtperiode.
    is_donker = !is_licht,
    
    uren_van_zonsopkomst = (
      minuut_van_dag - opkomst_minutes
    ) / 60,
    
    uren_van_zonsondergang = (
      minuut_van_dag - ondergang_minutes
    ) / 60,
    
    uren_van_zonsopkomst_rond = round(uren_van_zonsopkomst),
    
    uren_van_zonsondergang_rond = round(uren_van_zonsondergang)
    
  )


# ---------------------------------------------------------------------------
# 6 INDICATOREN OP BASIS VAN ONDERHOUD
# ---------------------------------------------------------------------------
# De toelichting bij een melding wordt op vaste regexpatronen doorzocht.
# De kernlijst bevat directe aanwijzingen voor onderhoud/reparatie. De extra
# lijst bevat bredere termen die eveneens op werkzaamheden kunnen wijzen.
#
# In de regex staat \b voor een woordgrens en \w* voor nul of meer
# woordtekens. Daardoor matcht bijvoorbeeld "\brepar\w*" meerdere vormen
# die met "repar" beginnen. De matching is niet hoofdlettergevoelig.
#

# Regex patroon voor basis-onderhoud
# Dit is de primaire, relatief directe onderhoudsfilter.
onderhoud_patronen_kern <- c(
  "\\bonderhoud\\w*",
  "\\brepar\\w*",
  "\\b\\w*werkzaam\\w*",
  "\\bvervang\\w*",
  "\\bherstel\\w*",
  "\\bverhelp\\w*",
  "\\bverholpen\\w*",
  "\\baanpass\\w*",
  "\\brevisi\\w*",
  "\\bmonteur\\w*",
  "\\bmontage\\w*",
  "\\bmonteer\\w*",
  "\\bgemonteerd\\w*",
  "\\bdemont\\w*",
  "\\bklus\\w*",
  "\\blaswerk\\w*",
  "\\bslijpwerk\\w*",
  "\\bdichtlassen\\w*",
  "\\bstoringsdienst\\w*",
  "\\btechnische dienst\\b",
  "\\btechnisch beheer\\b",
  "\\bombouw\\w*",
  "\\bomgebouwd\\w*",
  "\\bvernieuw\\w*"
)

onderhoud_regex_kern <- regex(
  str_c(onderhoud_patronen_kern, collapse = "|"),
  ignore_case = TRUE
)

#Regex voor extra onderhoud
# Dit is de aanvullende, ruimere set patronen voor de gevoeligheidsanalyse.
onderhoud_patronen_extra <- c(
  "\\btest\\w*",
  "\\binspect\\w*",
  "\\breinig\\w*",
  "\\bschoonmaak\\w*",
  "\\bschoonmaken\\w*",
  "\\bschoongemaakt\\w*",
  "\\bafstel\\w*",
  "\\bafgesteld\\w*",
  "\\binregel\\w*",
  "\\bingeregeld\\w*",
  "\\bproefdraai\\w*",
  
  "\\bstilstand\\w*",
  "\\bonderhoudsstop\\w*",
  "\\brevisiestop\\w*",
  "\\bstopweek\\w*",
  "\\bstopdag\\w*",
  
  "\\buit\\s*bedrijf\\w*",
  "\\buitbedrijf\\w*",
  "\\bin bedrijf nemen\\b",
  "\\bin bedrijf genomen\\b",
  "\\bopstart na\\b",
  "\\bna opstart\\b",
  
  "\\bafdicht\\w*",
  "\\bdichtgezet\\w*",
  "\\bdichtmaken\\w*",
  "\\bdichtgemaakt\\w*",
  
  "\\bgeplaatst\\w*",
  "\\bplaatsen\\b",
  "\\bplaatsing\\b",
  
  "\\btijdelijk\\w*",
  "\\bbypass\\w*",
  "\\bby-pass\\w*",
  "\\bsteiger\\w*",
  "\\bfirma\\b",
  "\\baannemer\\w*"
)

onderhoud_regex_extra <- regex(
  str_c(onderhoud_patronen_extra, collapse = "|"),
  ignore_case = TRUE
)

# Voor iedere melding bewaren we twee TRUE/FALSE-indicatoren. Er wordt hier
# nog niets verwijderd; het daadwerkelijke filteren gebeurt in het volgende blok.
meldingen_daglicht_onderhoud_analyse <- meldingen_daglicht_analyse %>%
  mutate(
    
    onderhoud_kandidaat_kern = str_detect(
      coalesce(toelichting, ""),
      onderhoud_regex_kern
    ),
    
    onderhoud_kandidaat_extra = str_detect(
      coalesce(toelichting, ""),
      onderhoud_regex_extra
    )
  )

# =============================================================================
# 6. DRIE ANALYSEBESTANDEN MAKEN
# =============================================================================
# Vanuit hetzelfde verrijkte bestand worden vier analysestanden gemaakt:
# - alle meldingen uit de volledige jaren 2015-2024;
# - alle meldingen uit 2020-2024, de jaren met bruikbare toelichting;
# - 2020-2024 na uitsluiting van kernmatches voor onderhoud;
# - dezelfde kerngefilterde set, met daarbovenop uitsluiting van de extra matches.
#

meldingen_complete_jaren_alles <- meldingen_daglicht_onderhoud_analyse

#We filteren hier alles onder 2020 ook weg, omdat daar geen toelichting was
meldingen_toelichting <- meldingen_daglicht_onderhoud_analyse %>% 
  filter(jaar %in% jaren_toelichting)

# Primaire onderhoudsanalyse: verwijder alle meldingen die op minimaal één
# kernpatroon matchen.
meldingen_exclusief_onderhoud_kern <- meldingen_toelichting %>%
  filter(
    !onderhoud_kandidaat_kern 
  )

# Gevoeligheidsanalyse: start met de al op kernwoorden gefilterde dataset en
# verwijder vervolgens ook meldingen die op een patroon uit de extra lijst matchen.
# De extra filter is dus cumulatief ten opzichte van de kernfilter.
meldingen_exclusief_onderhoud_extra <- meldingen_exclusief_onderhoud_kern %>% 
  filter(!onderhoud_kandidaat_extra)


# =============================================================================
# 7. WEGSCHRIJVEN
# =============================================================================
# Schrijf de vier analysebestanden weg. Script 03 leest precies deze bestanden
# weer in voor de uiteindelijke tellingen en tijdsanalyses.
#

dir.create(output_dir)

write_csv2(
  meldingen_complete_jaren_alles,
  alle_meldingen_path,
  na = ""
)

write_csv2(
  meldingen_toelichting,
  meldingen_toelichting_path,
  na = ""
)

write_csv2(
  meldingen_exclusief_onderhoud_kern,
  exclusief_onderhoud_kern_path,
  na = ""
)

write_csv2(
  meldingen_exclusief_onderhoud_extra,
  exclusief_onderhoud_extra_path,
  na = ""
)

