# Crosswalk from CNO70 and ISCO88_AC.

Almost all the occupational information in Colombian surveys is coded using national occupational classification CNO70. CNO70 does not allow comparisons between countries, and its design does not allow to build a correspondence to skill levels. To use the information, people reclassify occupational information to ISCO. Table crosswalks are unofficial files that circulate among social researchers but are not publicly available.

The crosswalks tables are not available from the Colombian national statistical office (DANE). Instead, DANE publishes a description of the ISCO-88 classification (adapted to Colombia) in a PDF document available [in the DANE website](https://www.dane.gov.co/files/sen/nomenclatura/ciuo/CIUO_88A_C_2006.pdf) which contains the equivalence (also available [in the raw data folder]( 01_raw/CIUO-88AC.pdf)).

This code in this repo:

- Extracts the information from such a document and store it in a table format.
- Constructs a 2-digit crosswalk from CNO70 to ISCO88_AC. The final table is ready to use in the Colombian surveys. 

