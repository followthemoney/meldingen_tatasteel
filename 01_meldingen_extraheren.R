# ==========================================
# PACKAGES
# ==========================================

library(tidyverse)
library(lubridate)
library(hms)


# ==========================================
# 1. TXT-BESTAND INLEZEN
# ==========================================
#
# De oorspronkelijke PDF is vooraf met `pdftotext -layout` omgezet naar
# platte tekst. De optie `-layout` probeert de visuele kolomindeling van
# de PDF te behouden door met spaties te werken. Dat is cruciaal voor de
# kolomherkenning verderop in dit script.
#
# Form-feedtekens (\f), die pagina-einden uit de PDF markeren, worden
# verwijderd zodat ze de regelherkenning niet verstoren.

regels <- readr::read_lines("data/raw/Meldingen OD alles.txt") %>%
  stringr::str_replace_all("\f", "")


# ==========================================
# 2. HERKEN WANNEER EEN NIEUWE MELDING BEGINT
#
# Iedere nieuwe melding begint met:
# dd-mm-jjjj hh:mm
# ==========================================

meldingen <- tibble(
  regel = regels
) %>%
  mutate(
    nieuwe_melding = str_detect(
      regel,
      "^\\d{2}-\\d{2}-\\d{4}\\s+\\d{2}:\\d{2}"
    ),
    
    melding_id = cumsum(nieuwe_melding)
  ) %>%
  filter(melding_id > 0)


# Controle aantal meldingen.
# Iedere regel die met een datum en tijd begint, is hierboven als begin van
# een nieuwe melding aangemerkt. Het aantal unieke melding_id's is daarom
# een eerste controle op het totaal aantal gevonden meldingen.
n_distinct(meldingen$melding_id)


# ==========================================
# 3. ALLE FYSIEKE REGELS VAN ÉÉN MELDING
# SAMENVOEGEN
#
# eerste_regel = regel waarop de tabelvelden
#                beginnen
#
# vervolgtekst = eventuele regels daaronder
# ==========================================

meldingen_samengevoegd <- meldingen %>%
  group_by(melding_id) %>%
  summarise(
    eerste_regel = first(regel),
    
    vervolgtekst = paste(
      regel[-1],
      collapse = " "
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    vervolgtekst = str_squish(vervolgtekst)
  )


# Controle.
# Na het samenvoegen moet iedere melding precies één rij hebben.
nrow(meldingen_samengevoegd)


# ==========================================
# 4. EERSTE REGEL SPLITSEN OP KOLOMSCHEIDING
#
# In pdftotext -layout worden kolommen
# doorgaans gescheiden door minimaal 3 spaties.
# ==========================================

meldingen_samengevoegd <- meldingen_samengevoegd %>%
  mutate(
    delen = str_split(
      eerste_regel,
      "\\s{3,}"
    ),
    
    n_delen = lengths(delen)
  )


# Controle structuur.
# De bron heeft veertien verwachte velden. Afwijkingen van veertien delen
# wijzen meestal op een kolomscheiding die in de PDF-extractie ontbreekt of
# juist ten onrechte in vrije tekst is ontstaan. Die gevallen worden later
# expliciet hersteld.
table(meldingen_samengevoegd$n_delen)


# Verwachte verdeling ongeveer:
#
#    12    13    14    15    16    17
#     2   199 15959    67     8     2


# ==========================================
# 5. BEKENDE HOEVEELHEDEN VERZAMELEN
#
# De regels met exact 14 onderdelen gebruiken
# we als woordenboek voor het herstellen van
# regels waar stof + hoeveelheid tegen elkaar
# geplakt zijn.
# ==========================================

hoeveelheden_bekend <- meldingen_samengevoegd %>%
  filter(n_delen == 14) %>%
  transmute(
    hoeveelheid = map_chr(
      delen,
      9
    )
  ) %>%
  pull(hoeveelheid) %>%
  unique()


# Langste eerst proberen.
#
# Deze volgorde is belangrijk omdat sommige hoeveelheden substrings van
# andere hoeveelheden kunnen zijn. Door de langste waarde eerst te testen,
# verkleinen we de kans dat een kortere, maar onjuiste hoeveelheid eerder
# wordt herkend.
#
# Daarmee voorkomen we bijvoorbeeld dat
# "1 kg" eerder matcht dan een langere waarde
# die ook met "1 kg" eindigt.

hoeveelheden_bekend <- hoeveelheden_bekend[
  order(
    nchar(hoeveelheden_bekend),
    decreasing = TRUE
  )
]


# ==========================================
# 6. FUNCTIE:
# STOF + HOEVEELHEID UIT ELKAAR HALEN
# ==========================================

split_stof_hoeveelheid <- function(x) {

  # Deze functie wordt alleen gebruikt voor afwijkende regels waarin het veld
  # 'vrijgekomen stof' en het veld 'hoeveelheid' door de PDF-extractie aan
  # elkaar zijn geplakt. De functie probeert eerst een hoeveelheid te herkennen
  # op basis van waarden die al voorkomen in correct gesplitste regels.
  
  # ----------------------------------------
  # Eerst matchen op hoeveelheden die
  # voorkomen in de nette regels
  # ----------------------------------------
  
  for (h in hoeveelheden_bekend) {
    
    if (
      is.na(h) ||
      h == ""
    ) {
      next
    }
    
    
    # Hele veld bestaat alleen uit hoeveelheid
    
    if (x == h) {
      
      return(
        c(
          "",
          h
        )
      )
    }
    
    
    # Hoeveelheid staat aan einde van veld
    
    if (
      str_ends(
        x,
        fixed(
          paste0(
            " ",
            h
          )
        )
      )
    ) {
      
      hoeveelheid <- h
      
      stof <- str_sub(
        x,
        1,
        nchar(x) -
          nchar(h) -
          1
      ) %>%
        str_trim()
      
      
      return(
        c(
          stof,
          hoeveelheid
        )
      )
    }
  }
  
  
  # ----------------------------------------
  # Fallback voor bijzondere hoeveelheden
  #
  # Als geen bekende hoeveelheid wordt gevonden, zoeken we met een bredere
  # reguliere expressie naar een getal gevolgd door een gangbare eenheid.
  # Alles vóór die match wordt als stof gezien; vanaf de match als hoeveelheid.
  # ----------------------------------------
  
  patroon <- regex(
    paste0(
      "\\d+(?:[.,]\\d+)?",
      "(?:-\\d+(?:[.,]\\d+)?)?",
      "\\s*",
      "(?:kg|m3|m³|L|liter|liters|",
      "mg/Nm3|mg/l|microgram|µg|",
      "kilo|ton|gram|ppm).*"
    ),
    ignore_case = TRUE
  )
  
  
  locatie <- str_locate(
    x,
    patroon
  )
  
  
  if (!is.na(locatie[1])) {
    
    stof <- str_sub(
      x,
      1,
      locatie[1] - 1
    ) %>%
      str_trim()
    
    
    hoeveelheid <- str_sub(
      x,
      locatie[1]
    ) %>%
      str_trim()
    
    
    return(
      c(
        stof,
        hoeveelheid
      )
    )
  }
  
  
  return(
    c(
      NA_character_,
      NA_character_
    )
  )
}


# ==========================================
# 7. FUNCTIE:
# AFWIJKENDE REGELS HERSTELLEN
# ==========================================

herstel_delen <- function(x) {

  # Iedere melding hoort uiteindelijk uit exact veertien velden te bestaan.
  # Op basis van het aantal gevonden delen bepaalt deze functie welk type
  # herstel nodig is. Alleen bekende en verklaarde afwijkingen worden
  # gecorrigeerd; onbekende structuren leiden verderop bewust tot een fout.
  
  n <- length(x)
  
  datum <- x[1]
  tijd  <- x[2]
  
  
  # ========================================
  # NORMALE REGEL
  #
  # Bij veertien delen is de tabelstructuur zoals verwacht en hoeft niets
  # hersteld te worden.
  # ========================================
  
  if (n == 14) {
    
    return(x)
    
  }
  
  
  # ========================================
  # TE VEEL DELEN
  #
  # Extra kolomscheidingen ontstaan hier vrijwel steeds doordat in de vrije
  # toelichting meerdere opeenvolgende spaties voorkomen. Omdat de eerste
  # dertien velden vaste tabelkolommen zijn, voegen we alles daarna weer samen
  # tot één toelichtingsveld.
  #
  # Bij 15, 16 of 17 delen zitten de extra
  # scheidingen vrijwel altijd in de
  # toelichting.
  #
  # Eerste 13 delen blijven daarom staan;
  # alles vanaf deel 14 wordt samengevoegd.
  # ========================================
  
  if (n > 14) {
    
    return(
      c(
        x[1:13],
        
        paste(
          x[14:n],
          collapse = " "
        )
      )
    )
  }
  
  
  # ========================================
  # 13 DELEN
  #
  # Er ontbreekt één scheiding. De herstelstrategie hangt af van waar de
  # samenvoeging is ontstaan: soms betreft het stof + hoeveelheid, soms
  # hoeveelheid + zaakresultaat, en enkele regels hebben een unieke bronfout.
  #
  # Er ontbreekt één kolomscheiding.
  # ========================================
  
  if (n == 13) {
    
    
    # --------------------------------------
    # Uitzondering:
    # 13-11-2023 14:45
    #
    # Deze melding wijkt zo specifiek af dat generiek herstel niet betrouwbaar
    # genoeg is. De ontbrekende grens tussen soort voorval en stof wordt daarom
    # expliciet gereconstrueerd op basis van de oorspronkelijke bronregel.
    #
    # Soort voorval en stof lopen in de
    # bron tegen elkaar aan.
    # --------------------------------------
    
    if (
      datum == "13-11-2023" &
      tijd == "14:45"
    ) {
      
      return(
        c(
          x[1:6],
          
          paste0(
            "1951JZ ",
            "Lekkage/emissie ",
            "(gevaarlijke)stof naar lucht; Brand"
          ),
          
          "Kooksgas",
          
          x[8:13]
        )
      )
    }
    
    
    # --------------------------------------
    # Uitzondering:
    # 09-10-2023 11:35
    #
    # Ook hier is een handmatige reconstructie nodig omdat de vrije tekst in
    # twee opeenvolgende velden de vaste kolomscheiding doorbreekt.
    # --------------------------------------
    
    if (
      datum == "09-10-2023" &
      tijd == "11:35"
    ) {
      
      return(
        c(
          x[1:7],
          
          "Hydrauliek olie (maar in damp-vorm)",
          
          paste0(
            "tussen de LEL en HEL ",
            "(dus in het explosiegebied)"
          ),
          
          x[9:13]
        )
      )
    }
    
    
    # --------------------------------------
    # Hoeveelheid + zaakresultaat tegen
    # elkaar geplakt
    #
    # Bijvoorbeeld:
    #
    # 0 mg/Nm3 (was 12 Melding afgehandeld...
    # --------------------------------------
    
    if (
      str_detect(
        x[9],
        "Melding afgehandeld"
      ) &
      !str_starts(
        x[9],
        "Melding"
      )
    ) {
      
      positie <- str_locate(
        x[9],
        "Melding afgehandeld"
      )[1]
      
      
      hoeveelheid <- str_sub(
        x[9],
        1,
        positie - 1
      ) %>%
        str_trim()
      
      
      zaakresultaat <- str_sub(
        x[9],
        positie
      ) %>%
        str_trim()
      
      
      return(
        c(
          x[1:8],
          hoeveelheid,
          zaakresultaat,
          x[10:13]
        )
      )
    }
    
    
    # --------------------------------------
    # Stof + hoeveelheid tegen elkaar geplakt
    #
    # Wanneer x[9] al met "Melding" begint, is het zaakresultaat kennelijk
    # één positie naar links geschoven. Dat betekent dat stof en hoeveelheid
    # waarschijnlijk samen in x[8] staan. Die worden gesplitst met de functie
    # `split_stof_hoeveelheid()`.
    #
    # Als x[9] al begint met "Melding",
    # ontbreekt waarschijnlijk de scheiding
    # tussen stof en hoeveelheid.
    # --------------------------------------
    
    if (
      str_starts(
        x[9],
        "Melding"
      )
    ) {
      
      gesplitst <- split_stof_hoeveelheid(
        x[8]
      )
      
      
      if (
        any(
          is.na(
            gesplitst
          )
        )
      ) {
        
        # Bewust stoppen als herstel niet eenduidig lukt. Zo wordt een
        # problematische melding niet stilzwijgend verkeerd ingedeeld.
        stop(
          paste(
            "Kon stof/hoeveelheid niet splitsen:",
            datum,
            tijd,
            x[8]
          )
        )
      }
      
      
      return(
        c(
          x[1:7],
          gesplitst[1],
          gesplitst[2],
          x[9:13]
        )
      )
    }
    
    
    # --------------------------------------
    # Hoeveelheid en zaakresultaat "-"
    #
    # In deze variant staat het minteken dat het zaakresultaat vormt vast aan
    # het hoeveelheidveld. We halen het minteken los en voegen het als apart
    # tiende veld terug.
    # zitten aan elkaar.
    # --------------------------------------
    
    if (
      str_ends(
        x[9],
        " -"
      )
    ) {
      
      hoeveelheid <- str_remove(
        x[9],
        "\\s+-\\s*$"
      ) %>%
        str_trim()
      
      
      return(
        c(
          x[1:8],
          hoeveelheid,
          "-",
          x[10:13]
        )
      )
    }
  }
  
  
  # ========================================
  # 12 DELEN
  #
  # Bij deze twee meldingen ontbreken twee kolomscheidingen. Omdat het om
  # afzonderlijke, inhoudelijk afwijkende bronregels gaat, worden ze expliciet
  # gereconstrueerd in plaats van met een algemene heuristiek.
  #
  # Twee specifieke zeer afwijkende regels.
  # ========================================
  
  if (n == 12) {
    
    
    # --------------------------------------
    # 09-10-2023 11:02
    # --------------------------------------
    
    if (
      datum == "09-10-2023" &
      tijd == "11:02"
    ) {
      
      return(
        c(
          x[1:7],
          
          "Kooksovengas (ongereiningd)",
          
          paste0(
            "2 m3 ",
            "(geschat door AZ, 0 kan niet)"
          ),
          
          "Melding afgehandeld - geen controle",
          
          x[9:12]
        )
      )
    }
    
    
    # --------------------------------------
    # 07-03-2022 16:28
    # --------------------------------------
    
    if (
      datum == "07-03-2022" &
      tijd == "16:28"
    ) {
      
      return(
        c(
          x[1:7],
          
          paste0(
            "Vulgas; 10.03.2022 aanvulling: ",
            "Geur-Kooks"
          ),
          
          paste0(
            "0 m3 ",
            "(AZ: kan niet, altijd wat ontsnapt ",
            "alvorens proces gestopt wordt)"
          ),
          
          "Melding afgehandeld - geen controle",
          
          x[9:12]
        )
      )
    }
  }
  
  
  # ========================================
  # ALS ER TOCH NOG EEN ONBEKENDE
  # STRUCTUUR OVERBLIJFT:
  #
  # script bewust stoppen
  # ========================================
  
  # Dit is een harde kwaliteitscontrole: iedere afwijking moet hierboven
  # verklaard zijn. Een onbekend patroon stopt het script zodat het eerst
  # handmatig kan worden onderzocht.
  stop(
    paste(
      "Onverwachte regel:",
      datum,
      tijd,
      "-",
      n,
      "delen"
    )
  )
}


# ==========================================
# 8. ALLE REGELS HERSTELLEN
# ==========================================
#
# Pas de herstel-functie toe op iedere melding. Naast de herstelde inhoud
# bewaren we tijdelijk ook het aantal velden, zodat in de volgende stap hard
# kan worden gecontroleerd of werkelijk alle meldingen veertien velden hebben.
# ==========================================

meldingen_hersteld <- meldingen_samengevoegd %>%
  mutate(
    
    delen_hersteld = map(
      delen,
      herstel_delen
    ),
    
    n_delen_hersteld = lengths(
      delen_hersteld
    )
  )


# ==========================================
# 9. CONTROLE:
# ALLES MOET NU 14 DELEN HEBBEN
# ==========================================

table(
  meldingen_hersteld$n_delen_hersteld
)


# Verwacht:
#
#    14
# 16237


# Extra harde controle.
# `stopifnot()` beëindigt het script direct als ook maar één melding na herstel
# niet uit exact veertien delen bestaat.

stopifnot(
  all(
    meldingen_hersteld$n_delen_hersteld == 14
  )
)


# ==========================================
# 10. VAN DE 14 DELEN ECHTE KOLOMMEN MAKEN
# ==========================================
#
# Nu de structuur voor iedere melding identiek is, worden de posities in de
# lijst `delen_hersteld` omgezet naar benoemde kolommen. De oorspronkelijke
# vervolgregels worden nog apart bewaard zodat de volledige toelichting in
# stap 12 kan worden opgebouwd.
# ==========================================

meldingen_df <- meldingen_hersteld %>%
  transmute(
    
    melding_id,
    
    datum = map_chr(
      delen_hersteld,
      1
    ),
    
    tijd = map_chr(
      delen_hersteld,
      2
    ),
    
    inrichting = map_chr(
      delen_hersteld,
      3
    ),
    
    plaats = map_chr(
      delen_hersteld,
      4
    ),
    
    straat = map_chr(
      delen_hersteld,
      5
    ),
    
    huisnr = map_chr(
      delen_hersteld,
      6
    ),
    
    pc_soort_voorval = map_chr(
      delen_hersteld,
      7
    ),
    
    vrijgekomen_stof = map_chr(
      delen_hersteld,
      8
    ),
    
    hoeveelheid = map_chr(
      delen_hersteld,
      9
    ),
    
    zaakresultaat = map_chr(
      delen_hersteld,
      10
    ),
    
    maatregelen_gevolgen = map_chr(
      delen_hersteld,
      11
    ),
    
    maatregelen_preventie = map_chr(
      delen_hersteld,
      12
    ),
    
    vervolgacties_od = map_chr(
      delen_hersteld,
      13
    ),
    
    toelichting_begin = map_chr(
      delen_hersteld,
      14
    ),
    
    vervolgtekst
  )


# ==========================================
# 11. POSTCODE EN SOORT VOORVAL SPLITSEN
#
# In pdftotext staan deze tegen elkaar:
#
# 1951JZ Lekkage/emissie ...
# ==========================================

# De postcode heeft een voorspelbaar Nederlands formaat van vier cijfers
# plus twee letters. Alles na de postcode wordt als omschrijving van het
# soort voorval beschouwd.
meldingen_df <- meldingen_df %>%
  extract(
    
    pc_soort_voorval,
    
    into = c(
      "pc",
      "soort_voorval"
    ),
    
    regex = paste0(
      "^",
      "(\\d{4}\\s?[A-Z]{2})",
      "\\s+",
      "(.*)",
      "$"
    ),
    
    remove = TRUE
  )


# ==========================================
# 12. TOELICHTING SAMENVOEGEN
# ==========================================
#
# In de PDF kan de toelichting over meerdere fysieke regels lopen. Het begin
# van de toelichting staat in het veertiende tabelveld; de rest is eerder als
# `vervolgtekst` verzameld. Hier voegen we beide delen weer samen en normaliseren
# we overbodige witruimte.
# ==========================================

meldingen_df <- meldingen_df %>%
  mutate(
    
    toelichting = str_squish(
      paste(
        toelichting_begin,
        vervolgtekst
      )
    )
    
  ) %>%
  select(
    -toelichting_begin,
    -vervolgtekst
  )


# ==========================================
# 13. DATUM EN TIJD OMZETTEN
# ==========================================
#
# Tot nu toe zijn datum en tijd tekst. We zetten de datum om naar een Date en
# de tijd naar een hms-object, zodat volgende scripts er betrouwbaar mee kunnen
# rekenen en sorteren.
# ==========================================

meldingen_final_df <- meldingen_df %>%
  mutate(
    
    datum = lubridate::dmy(
      datum
    ),
    
    tijd = hms::parse_hms(
      paste0(
        tijd,
        ":00"
      )
    )
  )


# ==========================================
# 14. KOLOMVOLGORDE
# ==========================================
#
# Zet de uiteindelijke dataset in een vaste, logisch leesbare volgorde.
# ==========================================

meldingen_final_df <- meldingen_final_df %>%
  select(
    melding_id,
    datum,
    tijd,
    inrichting,
    plaats,
    straat,
    huisnr,
    pc,
    soort_voorval,
    vrijgekomen_stof,
    hoeveelheid,
    zaakresultaat,
    maatregelen_gevolgen,
    maatregelen_preventie,
    vervolgacties_od,
    toelichting
  )

# ==========================================
# 15. EINDCONTROLES
# ==========================================
#
# Controleer het uiteindelijke aantal rijen en tel voor enkele belangrijke
# velden hoeveel waarden ontbreken. Dit verandert de data niet, maar maakt
# eventuele onverwachte uitval direct zichtbaar.
# ==========================================

nrow(
  meldingen_final_df
)

meldingen_final_df %>%
  summarise(
    
    aantal = n(),
    
    ontbrekende_datum = sum(
      is.na(datum)
    ),
    
    ontbrekende_tijd = sum(
      is.na(tijd)
    ),
    
    ontbrekende_pc = sum(
      is.na(pc)
    ),
    
    ontbrekende_stof = sum(
      is.na(vrijgekomen_stof)
    ),
    
    ontbrekende_hoeveelheid = sum(
      is.na(hoeveelheid)
    )
  )

# ==========================================
# 16. SNEL VISUEEL CONTROLEREN
# ==========================================
#
# Naast de structurele controles hierboven bekijken we de dataset ook visueel.
# De willekeurige steekproef van honderd rijen is bedoeld om te controleren of
# kolommen en toelichtingen ook inhoudelijk plausibel zijn gereconstrueerd.
# ==========================================

View(
  meldingen_final_df
)


# Willekeurige steekproef

meldingen_final_df %>%
  slice_sample(
    n = 100
  ) %>%
  View()


# ==========================================
# 17. EVENTUEEL OPSLAAN ALS CSV
# ==========================================
#
# Schrijf de volledig opgeschoonde dataset weg. Dit bestand vormt de input voor
# `02_daglicht_en_onderhoud_classificeren.R`.
# ==========================================

readr::write_csv(
  meldingen_final_df,
  "data/output/od_meldingen_uit_pdf.csv"
)
