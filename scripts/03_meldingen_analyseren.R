# =============================================================================
# DOEL VAN DIT SCRIPT
#
# Dit script analyseert de vier datasets uit script 02. Het:
# 1. bereidt de KNMI-daglichtdata voor op analyses per periode en seizoen;
# 2. berekent ruwe en voor daglengte gecorrigeerde licht-donkerratio's;
# 3. vergelijkt de ongefilterde data met de twee onderhoudsfilters;
# 4. maakt aggregaties per inrichting, seizoen, maand, jaar en uur;
# 5. schrijft de belangrijkste afgeleide tabellen weg naar data/analyse/.
#
# Input:
# - vier bestanden uit data/output/tijdsanalyse/
# - data/input/knmi_zonsopgang_ondergang_2015_2025.csv
# =============================================================================

library(tidyverse)
library(hms)

# =============================================================================
# 0. DATA INLADEN
# =============================================================================
# Lees de KNMI-daglichtdata en de vier analysestanden uit script 02 in.
# De bestanden met onderhoudsfilter bevatten alleen 2020-2024; het bestand
# "meldingen_complete_jaren_alles.csv" bevat alle volledige jaren 2015-2024.
#

data_input_dir <- file.path(
  "data",
  "output",
  "tijdsanalyse"
)

daglicht_df <- read_csv2("data/input/knmi_zonsopgang_ondergang_2015_2025.csv")

# Alle meldingen uit de volledige jaren, zonder onderhoudsfilter.
meldingen_alles <- read_csv2(
  file.path(
    data_input_dir,
    "meldingen_complete_jaren_alles.csv"
  )
)

# Alle meldingen uit 2020-2024, dus de periode met bruikbare toelichting.
meldingen_toelichting <- read_csv2(
  file.path(
    data_input_dir,
    "meldingen_2020_2024_alles.csv"
  )
)

# Dezelfde periode na verwijdering van meldingen die op de kernfilter matchen.
meldingen_onderhoud_kern <- read_csv2(
  file.path(
    data_input_dir,
    "meldingen_2020_2024_zonder_onderhoud_kern.csv"
  )
)

# Cumulatief verder gefilterd met de aanvullende onderhoudspatronen.
meldingen_onderhoud_extra <- read_csv2(
  file.path(
    data_input_dir,
    "meldingen_2020_2024_zonder_onderhoud_extra.csv"
  )
)

# =============================================================================
# 1. INSTELLINGEN
# =============================================================================
# De hoofdanalyse gebruikt de complete jaren 2015-2024. Analyses waarvoor
# toelichting nodig is, gebruiken alleen 2020-2024. We kijken ook los nog naar de coronajaren 2020-2021.
#

complete_jaren <- 2015:2024

jaren_toelichting <- 2020:2024

jaren_corona <- 2020:2021

# =============================================================================
# 2. DAGLICHTDATA VOORBEREIDEN VOOR ANALYSE
# =============================================================================
# Beperk de KNMI-data tot dezelfde volledige jaren en voeg meteorologische
# seizoenen toe. Deze daglichtdata levert straks de correctiefactoren waarmee
# rekening wordt gehouden met het ongelijke aantal uren licht en donker.
#

daglicht_df <- daglicht_df %>%
  filter(jaar %in% complete_jaren) %>%
  mutate(
    seizoen_meteorologisch = case_when(
      maand %in% 3:5 ~ "Voorjaar",
      maand %in% 6:8 ~ "Zomer",
      maand %in% 9:11 ~ "Herfst",
      maand %in% c(12, 1, 2) ~ "Winter"
    )
  )


# -----------------------------------------------------------------------------
# Daglicht/donker per seizoen
# -----------------------------------------------------------------------------
# Per seizoen berekenen we de gemiddelde daglengte en de bijbehorende verhouding
# tussen uren daglicht en uren donker. Dit gebeurt apart voor 2015-2024 en
# 2020-2024, omdat die perioden in verschillende analyses worden gebruikt.
#

# Voor alle volledige jaren: 2015-2024
daglicht_perseizoen_totaal <- daglicht_df %>%
  filter(jaar %in% complete_jaren) %>%
  group_by(seizoen_meteorologisch) %>%
  summarise(
    uren_daglicht = mean(daglicht_uren),
    uren_donker = 24 - uren_daglicht,
    ratio_licht_donker_seizoen = uren_daglicht / uren_donker,
    .groups = "drop"
  )


# Voor de jaren met bruikbare toelichting: 2020-2024
daglicht_perseizoen_jaren_toelichting <- daglicht_df %>%
  filter(jaar %in% jaren_toelichting) %>%
  group_by(seizoen_meteorologisch) %>%
  summarise(
    uren_daglicht = mean(daglicht_uren),
    uren_donker = 24 - uren_daglicht,
    ratio_licht_donker_seizoen = uren_daglicht / uren_donker,
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# Gemiddelde zonsopkomst/-ondergang per maand
# Deze waarden worden later naast de aantallen per uur gezet, zodat zichtbaar is
# hoe het dagritme van meldingen zich verhoudt tot de gemiddelde daglichtgrenzen.
# Alleen voor 2020-2024, omdat deze tabel wordt gebruikt voor de analyse
# na de onderhoudsfilter
# -----------------------------------------------------------------------------

zonsopkomst_ondergang_permaand_toelichting <- daglicht_df %>%
  filter(jaar %in% jaren_toelichting) %>%
  group_by(maand, maand_naam) %>%
  summarise(
    opkomst_gemiddeld = mean(opkomst_minutes),
    ondergang_gemiddeld = mean(ondergang_minutes),
    .groups = "drop"
  ) %>%
  mutate(
    opkomst_gemiddeld = round(opkomst_gemiddeld),
    ondergang_gemiddeld = round(ondergang_gemiddeld),
    
    opkomst_gemiddeld = sprintf(
      "%02d:%02d",
      opkomst_gemiddeld %/% 60,
      opkomst_gemiddeld %% 60
    ),
    
    ondergang_gemiddeld = sprintf(
      "%02d:%02d",
      ondergang_gemiddeld %/% 60,
      ondergang_gemiddeld %% 60
    )
  )


# -----------------------------------------------------------------------------
# Correctiefactor licht/donker
#
# Voor de hoofdvergelijking tellen we alle beschikbare uren daglicht en donker
# binnen de relevante periode bij elkaar op. De ratio uren licht / uren donker
# is de blootstellingscorrectie voor het feit dat beide tijdvakken niet exact
# even lang duren.
#
# We berekenen deze apart voor:
# - 2015-2024: analyse van alle complete jaren
# - 2020-2024: analyse waarvoor toelichtingen beschikbaar zijn
#
# De factor is:
# totaal aantal beschikbare uren daglicht /
# totaal aantal beschikbare uren donker
# -----------------------------------------------------------------------------

ratio_licht_donker_uren_alles <- daglicht_df %>%
  filter(jaar %in% complete_jaren) %>%
  summarise(
    uren_licht = sum(daglicht_uren, na.rm = TRUE),
    uren_donker = sum(24 - daglicht_uren, na.rm = TRUE),
    ratio = uren_licht / uren_donker
  ) %>%
  pull(ratio)


ratio_licht_donker_uren_toelichting <- daglicht_df %>%
  filter(jaar %in% jaren_toelichting) %>%
  summarise(
    uren_licht = sum(daglicht_uren, na.rm = TRUE),
    uren_donker = sum(24 - daglicht_uren, na.rm = TRUE),
    ratio = uren_licht / uren_donker
  ) %>%
  pull(ratio)

ratio_licht_donker_uren_corona <- daglicht_df %>%
  filter(jaar %in% jaren_corona) %>%
  summarise(
    uren_licht = sum(daglicht_uren, na.rm = TRUE),
    uren_donker = sum(24 - daglicht_uren, na.rm = TRUE),
    ratio = uren_licht / uren_donker
  ) %>%
  pull(ratio)

# Eventueel controleren
ratio_licht_donker_uren_alles
ratio_licht_donker_uren_toelichting
ratio_licht_donker_uren_corona

# =============================================================================
# 3. KORTE SAMENVATTING
# =============================================================================
# Maak één overzichtstabel met vier scenario's:
# - alle meldingen 2015-2024;
# - alle meldingen 2020-2024;
# - 2020-2024 na de kernfilter;
# - 2020-2024 na de cumulatieve extra filter.
#
# Voor ieder scenario tellen we licht en donker, berekenen we de ruwe verhouding
# licht/donker en delen we die vervolgens door de relevante urenratio. Daardoor
# geeft de gecorrigeerde ratio de verhouding weer per beschikbaar licht/donkeruur.
#

samenvating_sets <- bind_rows(
  
  # ---------------------------------------------------------------------------
  # Alle complete jaren: 2015-2024
  # ---------------------------------------------------------------------------
  
  # Voor deze set gebruiken we de daglichtcorrectie van 2015-2024.
  meldingen_alles %>%
    summarise(
      dataset = "alles 2015-2024",
      
      meldingen = n(),
      
      licht_aantal = sum(is_licht, na.rm = TRUE),
      donker_aantal = sum(is_donker, na.rm = TRUE),
      
      licht_donker_ratio =
        licht_aantal / donker_aantal,
      
      licht_donker_ratio_gecorrigeerd =
        licht_donker_ratio /
        ratio_licht_donker_uren_alles
    ),
  
  
  # ---------------------------------------------------------------------------
  # Alle meldingen met bruikbare toelichting: 2020-2024
  # ---------------------------------------------------------------------------
  
  # Vanaf hier gaat het om 2020-2024 en gebruiken we dus de correctiefactor
  # die specifiek voor die periode is berekend.
  meldingen_toelichting %>%
    summarise(
      dataset = "alles 2020-2024",
      
      meldingen = n(),
      
      licht_aantal = sum(is_licht, na.rm = TRUE),
      donker_aantal = sum(is_donker, na.rm = TRUE),
      
      licht_donker_ratio =
        licht_aantal / donker_aantal,
      
      licht_donker_ratio_gecorrigeerd =
        licht_donker_ratio /
        ratio_licht_donker_uren_toelichting
    ),
  
  # ---------------------------------------------------------------------------
  # Alle meldingen uit coronajaren
  # ---------------------------------------------------------------------------
  
  # Vanaf hier gaat het om 2020-2021 en gebruiken we dus de correctiefactor
  # die specifiek voor die periode is berekend.
  
  meldingen_alles %>% 
    filter(jaar %in% jaren_corona) %>% 
    summarise(
      dataset = "alles 2020-2021",
      
      meldingen = n(),
      
      licht_aantal = sum(is_licht, na.rm = TRUE),
      donker_aantal = sum(is_donker, na.rm = TRUE),
      
      licht_donker_ratio =
        licht_aantal / donker_aantal,
      
      licht_donker_ratio_gecorrigeerd =
        licht_donker_ratio /
        ratio_licht_donker_uren_corona
      ),
  

  
  
  # ---------------------------------------------------------------------------
  # 2020-2024 na kernfilter onderhoud
  # ---------------------------------------------------------------------------
  
  meldingen_onderhoud_kern %>%
    summarise(
      dataset = "zonder onderhoud kern 2020-2024",
      
      meldingen = n(),
      
      licht_aantal = sum(is_licht, na.rm = TRUE),
      donker_aantal = sum(is_donker, na.rm = TRUE),
      
      licht_donker_ratio =
        licht_aantal / donker_aantal,
      
      licht_donker_ratio_gecorrigeerd =
        licht_donker_ratio /
        ratio_licht_donker_uren_toelichting
    ),
  
  
  # ---------------------------------------------------------------------------
  # 2020-2024 na extra onderhoudsfilter
  # ---------------------------------------------------------------------------
  
  meldingen_onderhoud_extra %>%
    summarise(
      dataset = "zonder onderhoud extra 2020-2024",
      
      meldingen = n(),
      
      licht_aantal = sum(is_licht, na.rm = TRUE),
      donker_aantal = sum(is_donker, na.rm = TRUE),
      
      licht_donker_ratio =
        licht_aantal / donker_aantal,
      
      licht_donker_ratio_gecorrigeerd =
        licht_donker_ratio /
        ratio_licht_donker_uren_toelichting
    )
  
) %>%
  
  # Pas hier afronden, dus ná alle berekeningen
  mutate(
    licht_donker_ratio = round(
      licht_donker_ratio,
      2
    ),
    
    licht_donker_ratio_gecorrigeerd = round(
      licht_donker_ratio_gecorrigeerd,
      2
    )
  )


# Bekijken
# Dit object bevat de kerncijfers waarmee de verschillende filters direct met
# elkaar kunnen worden vergeleken.
samenvating_sets


# Eventueel naar clipboard
samenvating_sets %>%
  clipr::write_clip()

# Uit de samenvatting blijkt dat de extra filter op onderhoud de verhouding tussen dag en nacht juist nog schever maakt. Dit gaan tegen verwachtingen in. 
# Omdat we hierin een conservatieve houding in willen nemen, passen we de filter toe die focust op kernwoorden. Deze is simpeler, directer en daarom ook minder foutgevoelig.
# We gaan daarom in de analyse vooral verder met het frame meldingen_onderhoud_kern


# =============================================================================
# 2. ANALYSE OP INRICHTING
# =============================================================================
# Tel het aantal resterende meldingen per opgeschoonde Tata Steel-inrichting.
# Hiervoor wordt de kerngefilterde dataset gebruikt.
#

# We nemen hier de hele periode, omdat we vooral willen laten zien welke fabrieken het meest wordt gemeld (ongeacht dag en nacht)
# Technisch gebruikt dit object meldingen_onderhoud_kern en daarmee de periode
# 2020-2024 zoals die in script 02 is aangemaakt.
meldingen_perinrichting <- meldingen_onderhoud_kern %>% 
  group_by(inrichting_schoon) %>% 
  summarise(totaal= n()) %>% 
  arrange(desc(totaal)) %>% 
  clipr::write_clip()

# =============================================================================
# 3. ANALYSE OP TIJD
# =============================================================================
# Hieronder volgen verschillende tijdsdoorsneden. Sommige gebruiken alle
# complete jaren om langetermijnpatronen te tonen; analyses na onderhoudsfilter
# gebruiken de kerngefilterde dataset uit 2020-2024.


# We willen weten of het effect dag/licht het hele jaar door is. Dus corrigeren we de factor licht/donker met daglicht uren. 
# Eerst doen we dit voor alle meldingen uit 2015-2024 en daarna voor de
# kerngefilterde meldingen uit 2020-2024. De seizoensspecifieke urenratio maakt
# seizoenen met zeer verschillende daglengtes onderling beter vergelijkbaar.
meldingen_perseizoen_alles <- meldingen_alles %>% 
  group_by(seizoen_meteorologisch) %>% 
  summarise(medlingen_totaal = n(),
            donker = sum(is_donker),
            licht = sum(is_licht)) %>% 
  mutate(factor = (licht/donker) %>% round(.,1)) %>% 
  left_join(daglicht_perseizoen_totaal %>% 
              select(seizoen_meteorologisch,
                     uren_donker,
                     uren_daglicht,
                     ratio_licht_donker_seizoen)) %>% 
  mutate(factor_gecorrigeerd = factor/ratio_licht_donker_seizoen)

meldingen_perseizoen_gefilterd <- meldingen_onderhoud_kern %>% 
  group_by(seizoen_meteorologisch) %>% 
  summarise(medlingen_totaal = n(),
            donker = sum(is_donker),
            licht = sum(is_licht)) %>% 
  mutate(factor = (licht/donker) %>% round(.,1)) %>% 
  left_join(daglicht_perseizoen_jaren_toelichting %>% 
              select(seizoen_meteorologisch,
                     uren_donker,
                     uren_daglicht,
                     ratio_licht_donker_seizoen)) %>% 
  mutate(factor_gecorrigeerd = factor/ratio_licht_donker_seizoen)

# Hier pakken we weer alle meldingen omdat we de periode voor 2020 willen laten zien. 
# De variabele jaar_maand maakt van jaar en maand één echte maanddatum, waarna
# het totale aantal meldingen per kalendermaand wordt geteld.
meldingen_permaand <- meldingen_alles %>% 
  mutate(jaar_maand = paste0(jaar,"-", maand) %>% 
           ym()) %>% 
  group_by(jaar_maand) %>% 
  summarise(meldingen = n(),
            .groups = "drop") %>% 
  arrange(jaar_maand) %>% 
  clipr::write_clip()

# Overzichtje van meldingen per jaar, voor eigen beeld op langere termijn-ontwikkeling
# Hier wordt geen daglengtecorrectie toegepast; dit is een descriptief overzicht
# van aantallen licht, donker en totaal per jaar plus de ruwe licht/donkerratio.
meldingen_perjaar_alles <- meldingen_alles %>% 
  group_by(jaar) %>% 
  summarise(donker = sum(is_donker),
            licht = sum(is_licht),
            totaal = n()) %>% 
  mutate(factor = (licht/donker) %>% round(.,1)) 

# Hoe verhoudt het aantal meldingen zich tot zonsondergang? Hier corrigeren we weer voor onderhoud
# De gebruikte variabele is uren_van_zonsopkomst_rond: meldingen worden dus
# feitelijk gegroepeerd naar het afgeronde aantal uren ten opzichte van zonsopkomst.
meldingen_peruur_vanzonsopkomst <- meldingen_onderhoud_kern %>% 
  group_by(uren_van_zonsopkomst_rond) %>% 
  summarise(meldingen = n()) %>% 
  clipr::write_clip()

# Op welke uren worden de meldingen gedaan?
# Voor iedere maand tellen we meldingen per uur van de dag. pivot_wider maakt
# vervolgens één kolom per uur. Daarna voegen we de gemiddelde maandelijkse
# zonsopkomst en zonsondergang toe als referentie.
meldingen_peruur_permaand <- meldingen_onderhoud_kern %>% 
  group_by(maand, uur) %>% 
  arrange(maand, uur) %>% 
  summarise(meldingen = n(),
            .groups = "drop") %>% 
  pivot_wider(id_cols = maand,
              values_from = meldingen,
              names_from = uur) %>% 
  left_join(zonsopkomst_ondergang_permaand_toelichting) %>% 
  select(maand, maand_naam, opkomst_gemiddeld, ondergang_gemiddeld, everything()) 


# =============================================================================
# 4. FRAMES WEGSCHRIJVEN
# =============================================================================
# Schrijf de tabellen die gebruikt kunnen worden voor verdere controle, grafieken
# en publicatie weg. De datum in de bestandsnaam maakt verschillende runs
# herkenbaar zonder oudere versies automatisch te overschrijven.
#

output_dir <- file.path(
  "data",
  "analyse"
)

write_csv2(samenvating_sets,
           file.path(
             output_dir,
             paste0(
               "samenvating_sets_versie_",
               Sys.Date(),
               ".csv"
             )
           ))

write_csv2(meldingen_perinrichting,
           file.path(
             output_dir,
             paste0(
               "meldingen_perinrichting_filteronderhoud_versie_",
               Sys.Date(),
               ".csv"
             )
           ))

write_csv2(meldingen_peruur_permaand,
           file.path(
             output_dir,
             paste0(
               "meldingen_peruur_permaand_filteronderhoud_versie_",
               Sys.Date(),
               ".csv"
             )
           ))

write_csv2(meldingen_permaand,
           file.path(
             output_dir,
             paste0(
               "meldingen_permaand_allejaren_versie_",
               Sys.Date(),
               ".csv"
             )
           ))

write_csv2(meldingen_perseizoen_gefilterd,
           file.path(
             output_dir,
             paste0(
               "meldingen_perseizoen_filteronderhoud_versie_",
               Sys.Date(),
               ".csv"
             )
           ))

write_csv2(meldingen_peruur_vanzonsopkomst,
           file.path(
             output_dir,
             paste0(
               "meldingen_peruur_vanzonsopkomst_filteronderhoud_versie_",
               Sys.Date(),
               ".csv"
             )
           ))
