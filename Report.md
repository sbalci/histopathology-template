---
title: "Histopathology Research Template"
description: |
  Codes Used in Histopathology Research  
  Data Report for Histopathology Research  
  Example Using Random Generated Fakedata
author: 
  - name: Serdar Balci, MD, Pathologist
    url: https://sbalci.github.io/histopathology-template/
    affiliation: serdarbalci.com
    affiliation_url: https://www.serdarbalci.com/
date: "2020-02-18"
mail: drserdarbalci@gmail.com
linkedin: "serdar-balci-md-pathologist"
twitter: "serdarbalci"
github: "sbalci"
home: "https://www.serdarbalci.com/"
header-includes:
- \usepackage{pdflscape}
- \newcommand{\blandscape}{\begin{landscape}}
- \newcommand{\elandscape}{\end{landscape}}
- \usepackage{xcolor}
- \usepackage{afterpage}
- \renewcommand{\linethickness}{0.05em}
- \usepackage{booktabs}
- \usepackage{sectsty} \allsectionsfont{\nohang\centering \emph}
output:
  html_document: 
    toc: yes
    toc_float: yes
    number_sections: yes
    fig_caption: yes
    keep_md: yes
    highlight: kate
    theme: readable
    code_folding: "hide"
    includes:
      after_body: _footer.html
  rmarkdown::html_vignette: 
    css: 
    - !expr system.file("rmarkdown/templates/html_vignette/resources/vignette.css", package = "rmarkdown")
  prettydoc::html_pretty:
    theme: leonids
    highlight: vignette
    toc: true
    number_sections: yes
    css: css/style.css
    includes:
      after_body: _footer.html
  distill::distill_article:
    toc: true
  redoc::redoc:
    highlight_outputs: TRUE
    margins: 1 
    line_numbers: FALSE 
  pdf_document: 
    fig_caption: yes
    highlight: kate
    number_sections: yes
    toc: yes
    latex_engine: lualatex
    toc_depth: 5
    keep_tex: yes
vignette: >
  %\VignetteIndexEntry{Histopathology Research Template}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
bibliography: bib/template.bib
---




<style type="text/css">

h1{
  text-align: center;
}
h2{
  text-align: center;
}
h3{
  text-align: center;
}
h4{
  text-align: center;
}
h4.date{
  text-align: center;
}

</style>

<!-- Open all links in new tab-->  
<base target="_blank"/>  






---

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3635430.svg)](https://doi.org/10.5281/zenodo.3635430)

https://doi.org/10.5281/zenodo.3635430

https://osf.io/3tjfk/

[Histopathology Research Template 🔬](https://sbalci.github.io/histopathology-template/)


---



# Introduction

- **State the marker of interest, the study objectives, and hypotheses [@Knijn2015]**.^[From Table 1: Proposed items for reporting histopathology studies. Recommendations for reporting histopathology studies: a proposal Virchows Arch (2015) 466:611–615 DOI 10.1007/s00428-015-1762-3 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4460276/]

 


# Materials & Methods

**Describe Materials and Methods as highlighted in [@Knijn2015]**.^[From Table 1: Proposed items for reporting histopathology studies. Recommendations for reporting histopathology studies: a proposal Virchows Arch (2015) 466:611–615 DOI 10.1007/s00428-015-1762-3 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4460276/]

- Describe patient characteristics, and inclusion and exclusion criteria

- Describe treatment details

- Describe the type of material used

- Specify how expression of the biomarker was assessed

- Describe the number of independent (blinded) scorers and how they scored

- State the method of case selection, study design, origin of the cases, and time frame

- Describe the end of the follow-up period and median follow-up time

- Define all clinical endpoints examined

- Specify all applied statistical methods

- Describe how interactions with other clinical/pathological factors were analyzed

---

## Header Codes




**Codes for general settings.**^[See [`childRmd/_01header.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_01header.Rmd) file for other general settings]


**Setup global chunk settings**^[Change `echo = FALSE` to hide codes after knitting.]



```r
knitr::opts_chunk$set(
    eval = TRUE,
    echo = TRUE,
    fig.path = here::here("figs/"),
    message = FALSE,
    warning = FALSE,
    error = FALSE,
    cache = TRUE,
    comment = NA,
    tidy = TRUE,
    fig.width = 6,
    fig.height = 4
)
```







**Load Library**

see [`R/loadLibrary.R`](https://github.com/sbalci/histopathology-template/blob/master/R/loadLibrary.R) for the libraries loaded.


```r
source(file = here::here("R", "loadLibrary.R"))
```




---


## Generate Fake Data



**Codes for generating fake data**.^[See [`childRmd/_02fakeData.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_02fakeData.Rmd) file for other codes]

**Generate Fake Data**

This code generates a fake histopathological data.
Some sources for fake data generation
here^[Synthea The validity of synthetic clinical data: a validation study of a leading synthetic data generator (Synthea) using clinical quality measures. BMC Med Inform Decis Mak 19, 44 (2019) doi:10.1186/s12911-019-0793-0]
, here^[https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/s12911-019-0793-0]
, here^[[Synthetic Patient Generation](https://synthetichealth.github.io/synthea/)]
, here^[[Basic Setup and Running](https://github.com/synthetichealth/synthea/wiki/Basic-Setup-and-Running)]
, here^[[intelligent patient data generator (iPDG)](http://www.mli.gmu.edu/index.php/research/ipdg/)]
, here^[https://medium.com/free-code-camp/how-our-test-data-generator-makes-fake-data-look-real-ace01c5bde4a]
, here^[https://forums.librehealth.io/t/demo-data-generation/203]
, here^[https://mihin.org/services/patient-generator/]
, and here^[lung, cancer, breast datası ile birleştir]
.


**Use [this code](https://github.com/sbalci/histopathology-template/blob/master/R/gc_fake_data.R) to generate fake clinicopathologic data**


```r
source(file = here::here("R", "gc_fake_data.R"))
```




```r
wakefield::table_heat(x = fakedata, palette = "Set1", flip = TRUE, print = TRUE)
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/plot fake data-1.png)<!-- -->




---


## Import Data




**Codes for importing data.**^[See [`childRmd/_03importData.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_03importData.Rmd) file for other codes]


**Read the data**


```r
library(readxl)
mydata <- readxl::read_excel(here::here("data", "mydata.xlsx"))
# View(mydata) # Use to view data after importing
```


Add code for 
import multiple data
purrr
reduce








---

## Study Population



### Report General Features

**Codes for reporting general features.**^[See [`childRmd/_04briefSummary.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_04briefSummary.Rmd) file for other codes]


**Dataframe Report**


```r
# Dataframe report
mydata %>% dplyr::select(-contains("Date")) %>% report::report(.)
```

```
The data contains 250 observations of the following variables:
  - ID: 250 entries: 001, n = 1; 002, n = 1; 003, n = 1 and 247 others (0 missing)
  - Name: 249 entries: Aaleyah, n = 1; Abrea, n = 1; Afonso, n = 1 and 246 others (1 missing)
  - Sex: 2 entries: Male, n = 127; Female, n = 122 (1 missing)
  - Age: Mean = 48.59, SD = 14.12, Median = , MAD = 17.79, range: [25, 73], Skewness = 0.07, Kurtosis = -1.17, 1 missing
  - Race: 7 entries: White, n = 151; Hispanic, n = 49; Black, n = 30 and 4 others (1 missing)
  - PreinvasiveComponent: 2 entries: Absent, n = 205; Present, n = 44 (1 missing)
  - LVI: 2 entries: Absent, n = 148; Present, n = 102 (0 missing)
  - PNI: 2 entries: Absent, n = 181; Present, n = 68 (1 missing)
  - Death: 2 levels: FALSE (n = 80, 32.00%); TRUE (n = 169, 67.60%) and missing (n = 1, 0.40%)
  - Group: 2 entries: Treatment, n = 130; Control, n = 119 (1 missing)
  - Grade: 3 entries: 3, n = 97; 2, n = 84; 1, n = 68 (1 missing)
  - TStage: 4 entries: 4, n = 113; 3, n = 65; 2, n = 49 and 1 other (0 missing)
  - AntiX_intensity: Mean = 2.39, SD = 0.66, Median = , MAD = 1.48, range: [1, 3], Skewness = -0.62, Kurtosis = -0.66, 1 missing
  - AntiY_intensity: Mean = 1.91, SD = 0.75, Median = , MAD = 1.48, range: [1, 3], Skewness = 0.15, Kurtosis = -1.20, 1 missing
  - LymphNodeMetastasis: 2 entries: Absent, n = 147; Present, n = 102 (1 missing)
  - Valid: 2 levels: FALSE (n = 134, 53.60%); TRUE (n = 115, 46.00%) and missing (n = 1, 0.40%)
  - Smoker: 2 levels: FALSE (n = 132, 52.80%); TRUE (n = 117, 46.80%) and missing (n = 1, 0.40%)
  - Grade_Level: 3 entries: high, n = 91; moderate, n = 83; low, n = 75 (1 missing)
  - DeathTime: 2 entries: Within1Year, n = 149; MoreThan1Year, n = 101 (0 missing)
```



```r
mydata %>% explore::describe_tbl()
```

```
250 observations with 21 variables
17 variables containing missings (NA)
0 variables with no variance
```





---

## Ethics and IRB


### Always Respect Patient Privacy


<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

**Always Respect Patient Privacy**  
- Health Information Privacy^[https://www.hhs.gov/hipaa/index.html]  
- Kişisel Verilerin Korunması^[[Kişisel verilerin kaydedilmesi ve kişisel verileri hukuka aykırı olarak verme veya ele geçirme Türk Ceza Kanunu'nun 135. ve 136. maddesi kapsamında bizim hukuk sistemimizde suç olarak tanımlanmıştır. Kişisel verilerin kaydedilmesi suçunun cezası 1 ila 3 yıl hapis cezasıdır. Suçun nitelikli hali ise, kamu görevlisi tarafından görevin verdiği yetkinin kötüye kullanılarak veya belirli bir meslek veya sanatın sağladığı kolaylıktan yararlanılarak işlenmesidir ki bu durumda suçun cezası 1.5 ile 4.5 yıl hapis cezası olacaktır.](https://barandogan.av.tr/blog/ceza-hukuku/kisisel-verilerin-ele-gecirilmesi-yayilmasi-baskasina-verilmesi-sucu.html)]  

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

**Always Respect Patient Privacy**  
- Health Information Privacy^[https://www.hhs.gov/hipaa/index.html]  
- Kişisel Verilerin Korunması^[[Kişisel verilerin kaydedilmesi ve kişisel verileri hukuka aykırı olarak verme veya ele geçirme Türk Ceza Kanunu'nun 135. ve 136. maddesi kapsamında bizim hukuk sistemimizde suç olarak tanımlanmıştır. Kişisel verilerin kaydedilmesi suçunun cezası 1 ila 3 yıl hapis cezasıdır. Suçun nitelikli hali ise, kamu görevlisi tarafından görevin verdiği yetkinin kötüye kullanılarak veya belirli bir meslek veya sanatın sağladığı kolaylıktan yararlanılarak işlenmesidir ki bu durumda suçun cezası 1.5 ile 4.5 yıl hapis cezası olacaktır.](https://barandogan.av.tr/blog/ceza-hukuku/kisisel-verilerin-ele-gecirilmesi-yayilmasi-baskasina-verilmesi-sucu.html)]  

}
}



---


## Define Variable Types



**Codes for defining variable types**.^[See [`childRmd/_06variableTypes.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_06variableTypes.Rmd) file for other codes]




**print column names as vector**



```r
dput(names(mydata))
```

```
c("ID", "Name", "Sex", "Age", "Race", "PreinvasiveComponent", 
"LVI", "PNI", "LastFollowUpDate", "Death", "Group", "Grade", 
"TStage", "AntiX_intensity", "AntiY_intensity", "LymphNodeMetastasis", 
"Valid", "Smoker", "Grade_Level", "SurgeryDate", "DeathTime")
```

### Find Key Columns

#### Find ID and key columns to exclude from analysis

```
vctrs::vec_assert()

dplyr::all_equal()

arsenal::compare()

visdat::vis_compare()

```

See the code as function in [`R/find_key.R`](https://github.com/sbalci/histopathology-template/blob/master/R/find_key.R). 



```r
keycolumns <- mydata %>% sapply(., FUN = dataMaid::isKey) %>% tibble::as_tibble() %>% 
    dplyr::select(which(.[1, ] == TRUE)) %>% names()
keycolumns
```

```
[1] "ID"   "Name"
```


### Variable Types

**Get variable types**


```r
mydata %>% dplyr::select(-keycolumns) %>% inspectdf::inspect_types()
```

```
# A tibble: 4 x 4
  type             cnt  pcnt col_name  
  <chr>          <int> <dbl> <list>    
1 character         11  57.9 <chr [11]>
2 logical            3  15.8 <chr [3]> 
3 numeric            3  15.8 <chr [3]> 
4 POSIXct POSIXt     2  10.5 <chr [2]> 
```




```r
mydata %>% dplyr::select(-keycolumns, -contains("Date")) %>% describer::describe() %>% 
    knitr::kable(format = "markdown")
```



|.column_name         |.column_class |.column_type | .count_elements| .mean_value|  .sd_value|.q0_value     | .q25_value| .q50_value| .q75_value|.q100_value |
|:--------------------|:-------------|:------------|---------------:|-----------:|----------:|:-------------|----------:|----------:|----------:|:-----------|
|Sex                  |character     |character    |             250|          NA|         NA|Female        |         NA|         NA|         NA|Male        |
|Age                  |numeric       |double       |             250|   48.586345| 14.1156504|25            |         36|         48|         61|73          |
|Race                 |character     |character    |             250|          NA|         NA|Asian         |         NA|         NA|         NA|White       |
|PreinvasiveComponent |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|LVI                  |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|PNI                  |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|Death                |logical       |logical      |             250|          NA|         NA|FALSE         |         NA|         NA|         NA|TRUE        |
|Group                |character     |character    |             250|          NA|         NA|Control       |         NA|         NA|         NA|Treatment   |
|Grade                |character     |character    |             250|          NA|         NA|1             |         NA|         NA|         NA|3           |
|TStage               |character     |character    |             250|          NA|         NA|1             |         NA|         NA|         NA|4           |
|AntiX_intensity      |numeric       |double       |             250|    2.385542|  0.6629235|1             |          2|          2|          3|3           |
|AntiY_intensity      |numeric       |double       |             250|    1.907631|  0.7483194|1             |          1|          2|          2|3           |
|LymphNodeMetastasis  |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|Valid                |logical       |logical      |             250|          NA|         NA|FALSE         |         NA|         NA|         NA|TRUE        |
|Smoker               |logical       |logical      |             250|          NA|         NA|FALSE         |         NA|         NA|         NA|TRUE        |
|Grade_Level          |character     |character    |             250|          NA|         NA|high          |         NA|         NA|         NA|moderate    |
|DeathTime            |character     |character    |             250|          NA|         NA|MoreThan1Year |         NA|         NA|         NA|Within1Year |


**Plot variable types**


```r
mydata %>% dplyr::select(-keycolumns) %>% inspectdf::inspect_types() %>% inspectdf::show_plot()
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/variable type plot inspectdf-1.png)<!-- -->




```r
# https://github.com/ropensci/visdat
# http://visdat.njtierney.com/articles/using_visdat.html
# https://cran.r-project.org/web/packages/visdat/index.html
# http://visdat.njtierney.com/

# visdat::vis_guess(mydata)

visdat::vis_dat(mydata)
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/variable type plot visdat-1.png)<!-- -->



```r
mydata %>% explore::explore_tbl()
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/variable type plot explore-1.png)<!-- -->



### Define Variable Types


#### Find `character` variables


```r
characterVariables <- mydata %>% dplyr::select(-keycolumns) %>% inspectdf::inspect_types() %>% 
    dplyr::filter(type == "character") %>% dplyr::select(col_name) %>% dplyr::pull() %>% 
    unlist()

characterVariables
```

```
 [1] "Sex"                  "Race"                 "PreinvasiveComponent"
 [4] "LVI"                  "PNI"                  "Group"               
 [7] "Grade"                "TStage"               "LymphNodeMetastasis" 
[10] "Grade_Level"          "DeathTime"           
```


#### Find `categorical` variables


```r
categoricalVariables <- mydata %>% dplyr::select(-keycolumns, -contains("Date")) %>% 
    describer::describe() %>% janitor::clean_names() %>% dplyr::filter(column_type == 
    "factor") %>% dplyr::select(column_name) %>% dplyr::pull()

categoricalVariables
```

```
character(0)
```



#### Find `continious` variables



```r
continiousVariables <- mydata %>% dplyr::select(-keycolumns, -contains("Date")) %>% 
    describer::describe() %>% janitor::clean_names() %>% dplyr::filter(column_type == 
    "numeric" | column_type == "double") %>% dplyr::select(column_name) %>% dplyr::pull()

continiousVariables
```

```
[1] "Age"             "AntiX_intensity" "AntiY_intensity"
```


#### Find `numeric` variables




```r
numericVariables <- mydata %>% dplyr::select(-keycolumns) %>% inspectdf::inspect_types() %>% 
    dplyr::filter(type == "numeric") %>% dplyr::select(col_name) %>% dplyr::pull() %>% 
    unlist()

numericVariables
```

```
[1] "Age"             "AntiX_intensity" "AntiY_intensity"
```


#### Find `integer` variables



```r
integerVariables <- mydata %>% dplyr::select(-keycolumns) %>% inspectdf::inspect_types() %>% 
    dplyr::filter(type == "integer") %>% dplyr::select(col_name) %>% dplyr::pull() %>% 
    unlist()

integerVariables
```

```
NULL
```


#### Find `list` variables


```r
listVariables <- mydata %>% dplyr::select(-keycolumns) %>% inspectdf::inspect_types() %>% 
    dplyr::filter(type == "list") %>% dplyr::select(col_name) %>% dplyr::pull() %>% 
    unlist()
listVariables
```

```
NULL
```


#### Find `date` variables


```r
is_date <- function(x) inherits(x, c("POSIXct", "POSIXt"))

dateVariables <- names(which(sapply(mydata, FUN = is_date) == TRUE))
dateVariables
```

```
[1] "LastFollowUpDate" "SurgeryDate"     
```





---


## Overview the Data



**Codes for overviewing the data.**^[See [`childRmd/_07overView.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_07overView.Rmd) file for other codes]


### View Data



```r
View(mydata)
```



```r
reactable::reactable(data = mydata, sortable = TRUE, resizable = TRUE, filterable = TRUE, 
    searchable = TRUE, pagination = TRUE, paginationType = "numbers", showPageSizeOptions = TRUE, 
    highlight = TRUE, striped = TRUE, outlined = TRUE, compact = TRUE, wrap = FALSE, 
    showSortIcon = TRUE, showSortable = TRUE)
```

<!--html_preserve--><div id="htmlwidget-522754888ffc14382241" class="reactable html-widget" style="width:auto;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-522754888ffc14382241">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"ID":["001","002","003","004","005","006","007","008","009","010","011","012","013","014","015","016","017","018","019","020","021","022","023","024","025","026","027","028","029","030","031","032","033","034","035","036","037","038","039","040","041","042","043","044","045","046","047","048","049","050","051","052","053","054","055","056","057","058","059","060","061","062","063","064","065","066","067","068","069","070","071","072","073","074","075","076","077","078","079","080","081","082","083","084","085","086","087","088","089","090","091","092","093","094","095","096","097","098","099","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250"],"Name":["Chanceller","Seph","Makynsie","Devn","Takiara","Tilda","Alayjha","Keymonie","Kitsia","Rashidat","Kentaro","Marquet","Lydya","Laressa","Enajah","Koey","Marylan","Laurenmarie","Cleary","Zavia","Shedrick","Julayne","Treveion","Emylee","Jais","Alvie","Tramond","Shakisha","Teneka","Nataya","Nastasia","Jaydenalexander","Karlon","Asucena","Judge","Jenesia","Thaissa","Laine","Kenzly","Teiara","Princy","Manijah","Mahkyla","Keshia","Enisa","Jonnatan","Afonso","Danis","Geidi","Brecon","Kawanda","Anastacia","Voncile","Malala","Kirkland","Lorali","Abrea","Zerek","Amella","Sakina","Debroha","Daelani","Zaleth","Melasia","Shearon","Elick","Keason","Kaylanie","Celida","Jahkeem","Danayjah","Dartisha","Alisa","Sonje","Madaleno","Michae",null,"Schyler","Karliyah","Jenaliz","Ayati","Trisa","Laguan","Chekesha","Raavi","Caitriona","Keonta","Shivonni","Srushti","Mcclellan","Marilin","Saline","Nicle","Desaraye","Raygine","Margurete","Shelva","Kahtai","Kemarius","Aubrii","Reyonna","Jameyah","Narsis","Milanie","Edolia","Sahr","Dolena","Aaleyah","Demerius","Tashai","Dorethia","Jemilla","Kaymin","Tavis","Hasset","Althera","Laramy","Bodee","Harpyr","Janyria","Bernasia","Chadron","Aurick","Katheran","Marrell","Deangelio","Deloros","Minhchau","Olicia","Vikram","Oluwatobiloba","Isaias","Vickye","Saamya","Essynce","Walden","Kata","Shantanae","Javiair","Jaynalis","Roziya","Jamaul","Donyetta","Asaad","Maliyani","Genese","Kaena","Niaya","Donivee","Claristine","Montesha","Sonita","Milady","Ellease","Nazhir","Jaqui","Blodwyn","Daisy","Lazarria","Champaine","Desuan","Landon","Kyrea","Dannielynn","Apurva","Wyoma","Jaxi","Braderick","Jackalyn","Jadyel","Timiko","Jennifr","Ahir","Breonia","Dicie","Bronis","Pemberley","Amiyra","Mekea","Jediael","Lynneann","Julenny","Grenda","Austreberto","Marisal","Fennell","Zyanah","Rb","Jahvel","Kaeliana","Sheng","Brishaun","Arkita","Jazzabelle","Katyna","Dashona","Callan","Annavictoria","Filicity","Jacarla","Saga","Kiuna","Chaunice","Hatsumi","Jodanna","Liria","Antavia","Aketzali","Jamyrah","Wandalee","Alinson","Xavius","Lethea","Rediet","Gabrel","Deontray","Jasahn","Daveon","Arceli","Yaxeni","Danyal","Makada","Jaciana","Wladimir","Liah","Rodrika","Terianna","Tselane","Morjorie","Wilhemina","Nil","Latajia","Neeharika","Jniah","Chianti","Jannete","Damar","Harver","Sakeya","Selin","Lakira","Olle","Ashera","Laileen","Tully","Emmabella","Nayvadius","Dayami","Erdman","Dristin"],"Sex":["Female","Female","Female","Female","Male","Male","Female","Male","Male","Male","Female","Male","Male","Male","Female","Female","Female","Female","Male","Male","Male","Female","Female","Male","Female","Female","Female","Male","Male","Male","Male","Male","Male","Male","Male","Male","Female","Male","Female","Male","Female","Male","Female","Female","Female","Female","Female","Male","Male","Female","Female","Male","Female","Male","Female","Male","Male","Female","Female","Female","Female","Male","Male","Male","Male","Male","Female","Female","Female","Male","Male","Male","Male","Female","Male","Female","Female","Female","Female","Female","Male","Male","Male","Female","Male","Female","Female","Male","Female","Male","Female","Female","Male","Male","Female","Male","Male","Male","Female","Male","Female","Male","Female","Female","Male","Female","Female","Female","Female","Female","Female","Female","Female","Female","Female","Male","Male","Male","Male","Female","Female","Male","Male","Female","Male","Male","Male","Male","Male","Male","Female","Male","Male","Male","Female","Female",null,"Male","Male","Male","Male","Female","Female","Male","Female","Male","Male","Female","Female","Male","Male","Female","Male","Female","Male","Male","Male","Male","Female","Female","Female","Female","Male","Male","Female","Male","Female","Female","Female","Female","Female","Female","Female","Male","Male","Male","Male","Female","Male","Female","Female","Male","Male","Male","Male","Female","Male","Male","Male","Female","Female","Female","Female","Female","Female","Female","Male","Female","Male","Female","Female","Male","Male","Female","Female","Male","Female","Male","Male","Male","Female","Female","Female","Male","Male","Male","Female","Male","Male","Male","Female","Male","Female","Male","Female","Female","Female","Male","Male","Male","Female","Male","Male","Female","Male","Female","Female","Female","Female","Male","Male","Male","Male","Male","Female","Male","Female","Female","Male","Female"],"Age":[54,39,32,58,38,50,36,70,43,45,65,27,44,64,44,48,58,71,46,64,48,35,26,34,64,64,72,45,34,62,43,58,55,49,61,48,68,50,34,62,50,51,56,48,31,33,44,44,35,73,27,42,33,40,48,51,31,61,48,73,41,36,50,72,39,68,44,52,32,37,49,41,33,42,53,61,66,54,56,39,42,31,50,28,51,37,"NA",52,40,25,29,48,51,69,26,29,70,73,44,54,53,40,45,29,33,29,66,47,45,72,70,26,26,42,28,30,51,31,29,30,58,41,35,58,57,46,36,48,70,59,73,57,48,39,32,36,36,35,25,42,28,45,45,72,38,60,63,33,30,64,29,55,64,67,40,32,66,65,44,36,54,64,40,61,53,73,68,43,50,55,72,36,40,56,38,62,33,28,62,58,38,61,31,57,45,53,68,47,49,44,47,51,54,70,66,44,71,32,62,29,49,33,68,61,25,70,28,43,25,65,59,53,30,63,55,72,32,58,41,72,34,68,70,66,68,70,31,57,25,49,25,37,48,50,57,27,66,50,35,70,63,55,64,32,35,57,58,57,66,71],"Race":["White","White","White","Hispanic","White","White","Hispanic","White","Hispanic","Black","Hispanic","White","White","Hispanic","Black","Bi-Racial","Asian","White","Asian","Black","White","Hispanic","White","Hispanic","White","Black","Black","Hispanic","White","Hispanic","Asian","White","Hispanic","Hispanic","Hispanic","White","White","White","Hispanic","White","White","Black","Black","Hispanic","White","White","White","White","White","White","Hispanic","White","White","White","Hispanic","Hispanic","White","Black","Hispanic","White","White","Hispanic","Native","White","Hispanic","Other","White","White","White","White","Hispanic","Hispanic","White","Hispanic","Other","White","White","White","White","Hispanic","Hispanic","Bi-Racial","White","White","White","Bi-Racial","White","White","White","White","White","White","White","White","White","White","Asian","Black","White","White","Black","White","Asian","Hispanic","White","Black","White","Black","White","Black","Hispanic","Asian","Black","Black","White","White","White","Hispanic","White","White","White","White","White","Bi-Racial","White","White","Hispanic","Hispanic","Bi-Racial","White","White","Hispanic","Black","White","White","White","White","Hispanic","Hispanic","Asian","Black","White","Black","Hispanic","White","White","Black","Hispanic","White","White","White","White","White","White","Hispanic","White","White","Black","White","White","White","Hispanic","White","White","White","White","Hispanic","White","White","Hispanic","Black","White","White","White","White","White","Hispanic",null,"White","White","White","Hispanic","White","White","Hispanic","White","White","White","White","White","White","White","Asian","White","White","White","Black","White","Black","White","Black","Hispanic","Black","White","White","Black","White","Black","White","Hispanic","White","Hispanic","White","Black","White","Asian","White","Hispanic","White","White","White","Black","White","White","White","White","White","White","White","Asian","Hispanic","White","White","White","White","Hispanic","Black","White","White","White","White","Asian","White","White","White","Hispanic","White","White","White","White"],"PreinvasiveComponent":["Present","Absent","Absent","Present","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent",null,"Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent"],"LVI":["Present","Absent","Present","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Present","Absent","Present","Present","Present","Present","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Present","Present","Absent","Present","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Present","Present","Absent","Present","Absent","Present","Absent","Absent","Present","Present","Present","Present","Present","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Present","Absent","Present","Absent","Absent","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Present","Absent","Present","Absent","Present","Present","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Present","Present","Absent","Present","Present","Present","Present","Present","Present","Present","Absent","Absent","Present","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present"],"PNI":["Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Present","Absent","Present","Absent","Present","Absent","Absent","Present","Absent","Absent",null,"Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Present","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Present","Absent","Present","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present"],"LastFollowUpDate":["2019-05-18T00:00:00","2019-05-18T00:00:00","2019-08-18T00:00:00","2020-01-18T00:00:00","2019-07-18T00:00:00","2019-09-18T00:00:00","2019-05-18T00:00:00","2019-12-18T00:00:00","2019-06-18T00:00:00","2019-03-18T00:00:00","2019-09-18T00:00:00","2019-11-18T00:00:00","2019-09-18T00:00:00","2019-10-18T00:00:00","2019-12-18T00:00:00","2019-06-18T00:00:00","2019-05-18T00:00:00","2019-08-18T00:00:00","2019-12-18T00:00:00","2019-09-18T00:00:00","2019-11-18T00:00:00","2019-11-18T00:00:00","2019-06-18T00:00:00","2020-01-18T00:00:00","2019-10-18T00:00:00","2019-10-18T00:00:00","2020-01-18T00:00:00","2019-06-18T00:00:00","2019-12-18T00:00:00","2020-02-18T00:00:00","2019-12-18T00:00:00","2019-07-18T00:00:00","2019-08-18T00:00:00","2019-10-18T00:00:00","2019-05-18T00:00:00","2019-08-18T00:00:00","2020-02-18T00:00:00","2019-08-18T00:00:00","2019-03-18T00:00:00","2019-06-18T00:00:00","2019-11-18T00:00:00","2019-07-18T00:00:00","2019-09-18T00:00:00","2019-03-18T00:00:00","2019-07-18T00:00:00","2020-01-18T00:00:00","2019-05-18T00:00:00","2020-02-18T00:00:00","2019-03-18T00:00:00","2019-09-18T00:00:00","2019-06-18T00:00:00","2019-07-18T00:00:00","2019-03-18T00:00:00","2019-05-18T00:00:00","2019-05-18T00:00:00","2019-07-18T00:00:00","2019-03-18T00:00:00","2019-08-18T00:00:00","2019-05-18T00:00:00","2019-07-18T00:00:00","2020-02-18T00:00:00","2019-08-18T00:00:00","2019-11-18T00:00:00","2019-03-18T00:00:00","2019-04-18T00:00:00","2019-03-18T00:00:00","2019-03-18T00:00:00","2019-05-18T00:00:00","2020-01-18T00:00:00","2019-10-18T00:00:00","2020-02-18T00:00:00","2019-07-18T00:00:00","2019-06-18T00:00:00","2019-05-18T00:00:00","2019-11-18T00:00:00","2019-09-18T00:00:00","2019-05-18T00:00:00","2019-07-18T00:00:00","2019-11-18T00:00:00","2020-02-18T00:00:00","2019-10-18T00:00:00","2019-07-18T00:00:00","2020-02-18T00:00:00","2019-08-18T00:00:00","2020-01-18T00:00:00","2019-03-18T00:00:00","2020-01-18T00:00:00","2019-11-18T00:00:00","2019-04-18T00:00:00","2019-08-18T00:00:00","2020-01-18T00:00:00","2019-10-18T00:00:00","2019-08-18T00:00:00","2019-03-18T00:00:00","2019-05-18T00:00:00","2019-12-18T00:00:00","2019-05-18T00:00:00","2019-10-18T00:00:00","2019-03-18T00:00:00","2019-07-18T00:00:00","2019-04-18T00:00:00","2019-05-18T00:00:00","2019-03-18T00:00:00","2019-11-18T00:00:00","2019-12-18T00:00:00","2020-02-18T00:00:00","2019-04-18T00:00:00","2019-03-18T00:00:00","2020-01-18T00:00:00","2019-08-18T00:00:00","2019-12-18T00:00:00","2019-10-18T00:00:00","2019-12-18T00:00:00","2020-01-18T00:00:00","2019-11-18T00:00:00","2019-08-18T00:00:00","2019-04-18T00:00:00","2019-09-18T00:00:00","2019-10-18T00:00:00","2019-11-18T00:00:00","2019-09-18T00:00:00","2019-04-18T00:00:00","2019-09-18T00:00:00","2019-04-18T00:00:00","2019-04-18T00:00:00","2019-03-18T00:00:00","2019-11-18T00:00:00","2019-05-18T00:00:00","2019-05-18T00:00:00","2019-09-18T00:00:00","2019-05-18T00:00:00","2019-05-18T00:00:00","2019-07-18T00:00:00","2019-05-18T00:00:00","2019-11-18T00:00:00","2019-09-18T00:00:00","2019-11-18T00:00:00","2019-05-18T00:00:00","2019-05-18T00:00:00","2019-07-18T00:00:00","2020-01-18T00:00:00","2019-03-18T00:00:00","2019-11-18T00:00:00","2019-12-18T00:00:00","2020-01-18T00:00:00","2019-04-18T00:00:00","2019-09-18T00:00:00","2019-09-18T00:00:00","2020-02-18T00:00:00","2019-11-18T00:00:00","2020-01-18T00:00:00","2019-05-18T00:00:00","2019-08-18T00:00:00","2019-11-18T00:00:00","2020-02-18T00:00:00","2019-06-18T00:00:00","2019-05-18T00:00:00","2019-06-18T00:00:00","2019-11-18T00:00:00","2019-09-18T00:00:00","2020-01-18T00:00:00","2019-12-18T00:00:00","2020-02-18T00:00:00","2019-05-18T00:00:00","2019-05-18T00:00:00","2019-08-18T00:00:00","2019-12-18T00:00:00","2019-06-18T00:00:00","2019-03-18T00:00:00","2019-10-18T00:00:00","2019-04-18T00:00:00","2019-06-18T00:00:00","2019-11-18T00:00:00","2019-07-18T00:00:00","2019-05-18T00:00:00","2019-09-18T00:00:00","2019-09-18T00:00:00","2019-05-18T00:00:00","2019-11-18T00:00:00","2019-12-18T00:00:00","2019-11-18T00:00:00","2019-06-18T00:00:00","2019-09-18T00:00:00","2019-03-18T00:00:00","2020-01-18T00:00:00","2019-05-18T00:00:00","2020-01-18T00:00:00","2020-01-18T00:00:00","2019-10-18T00:00:00","2019-12-18T00:00:00","2019-03-18T00:00:00","2020-01-18T00:00:00","2019-04-18T00:00:00","2019-09-18T00:00:00","2019-10-18T00:00:00","2019-12-18T00:00:00","2019-08-18T00:00:00","2019-12-18T00:00:00","2019-11-18T00:00:00","2019-06-18T00:00:00","2019-04-18T00:00:00","2019-06-18T00:00:00","2020-01-18T00:00:00","2020-02-18T00:00:00","2020-01-18T00:00:00",null,"2019-11-18T00:00:00","2019-09-18T00:00:00","2019-10-18T00:00:00","2019-08-18T00:00:00","2019-08-18T00:00:00","2019-08-18T00:00:00","2019-06-18T00:00:00","2019-05-18T00:00:00","2020-02-18T00:00:00","2020-02-18T00:00:00","2019-05-18T00:00:00","2020-01-18T00:00:00","2019-12-18T00:00:00","2019-10-18T00:00:00","2019-08-18T00:00:00","2019-06-18T00:00:00","2020-01-18T00:00:00","2019-10-18T00:00:00","2020-01-18T00:00:00","2019-05-18T00:00:00","2019-04-18T00:00:00","2019-08-18T00:00:00","2019-12-18T00:00:00","2019-08-18T00:00:00","2019-04-18T00:00:00","2019-09-18T00:00:00","2019-11-18T00:00:00","2019-05-18T00:00:00","2019-08-18T00:00:00","2019-03-18T00:00:00","2019-06-18T00:00:00","2020-01-18T00:00:00","2019-03-18T00:00:00","2019-09-18T00:00:00","2020-01-18T00:00:00","2020-02-18T00:00:00","2020-01-18T00:00:00","2019-03-18T00:00:00","2019-06-18T00:00:00","2019-12-18T00:00:00","2019-04-18T00:00:00","2019-12-18T00:00:00","2019-06-18T00:00:00","2019-12-18T00:00:00"],"Death":[true,true,false,true,true,false,false,true,true,false,false,true,true,true,true,true,true,true,false,false,true,true,true,false,true,true,true,false,true,false,false,false,true,true,false,false,true,false,true,true,false,true,true,true,false,false,false,true,true,true,false,true,true,true,false,false,false,false,true,true,false,true,true,false,false,true,true,true,true,true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,false,false,true,true,true,true,true,true,true,false,true,true,false,true,true,true,false,true,false,false,true,true,false,true,true,true,true,true,true,false,true,false,true,true,true,false,false,true,true,false,false,true,true,true,true,true,true,true,true,true,true,false,true,false,true,false,true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,false,false,true,true,false,false,true,false,true,true,true,false,true,true,false,true,false,false,true,true,true,true,true,true,false,true,false,true,true,true,true,true,true,true,false,true,true,true,false,true,false,true,true,false,true,true,false,true,false,false,true,true,true,false,true,true,true,true,true,false,true,true,false,true,true,false,true,true,true,true,false,true,false,false,false,true,true,true,false,true,false,false,false,false,true,true,true,true,false,false,null,false,true],"Group":["Treatment","Treatment","Control","Treatment","Control","Control","Treatment","Treatment","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Control","Control","Treatment","Control","Treatment","Treatment","Treatment","Treatment","Control","Control","Treatment","Control","Control","Treatment","Control","Treatment","Control","Control","Treatment","Control","Treatment","Control","Control","Control","Treatment","Control","Treatment","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Control","Treatment","Control","Treatment","Control","Treatment","Control","Treatment","Treatment","Treatment","Control","Control","Treatment","Control","Treatment","Control","Treatment","Treatment","Control","Control","Treatment","Control","Treatment","Control","Treatment","Control","Treatment","Control","Treatment","Treatment","Control","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Control","Control","Treatment","Control","Control","Control","Treatment","Control","Control","Treatment","Treatment","Control","Control","Treatment","Control","Control","Control","Control","Treatment","Control","Treatment","Control","Treatment","Treatment","Control","Treatment","Control","Treatment","Control","Treatment","Control","Control","Control","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Control","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Treatment","Treatment","Control","Treatment","Control","Treatment","Control","Control","Treatment","Treatment","Treatment","Control","Treatment","Control","Treatment","Control","Treatment","Control","Treatment","Control","Control","Treatment","Control","Control","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Treatment","Control","Control","Control","Treatment","Control","Control","Control","Control","Treatment","Control","Treatment","Treatment","Control","Control","Control","Control","Control","Treatment","Control","Control","Treatment","Control","Control","Control","Treatment","Control","Control","Treatment","Treatment","Treatment","Control","Treatment","Treatment","Treatment","Control","Treatment","Control","Control","Control","Treatment","Treatment","Control","Treatment","Treatment","Control","Control","Control","Treatment","Control",null,"Treatment","Treatment","Control","Control","Treatment","Treatment","Control","Control","Control","Control","Treatment","Control","Treatment","Treatment","Treatment","Control","Treatment","Treatment","Treatment","Control"],"Grade":["1","3","3","3","2","1","2","2","1","3","1","1","2","3","3","2","1","2","3","3","3","1","3","2","2","3","1","2","3","2","3","1","3","2",null,"2","3","3","2","3","1","3","2","2","2","2","1","1","1","1","3","2","3","3","1","2","2","3","1","3","1","2","1","2","2","3","1","2","2","2","2","3","1","3","3","2","2","1","3","3","3","1","3","2","3","1","3","1","3","3","3","3","2","3","1","1","2","1","1","2","1","1","2","2","3","3","2","3","3","2","3","3","3","3","3","2","1","1","3","1","3","2","2","1","1","3","2","1","3","2","3","3","1","3","3","3","1","3","2","1","1","1","3","2","3","1","2","2","2","2","2","2","2","3","2","1","1","2","2","2","3","1","1","2","2","3","2","1","2","3","1","2","2","1","3","3","3","3","3","1","2","2","2","1","3","2","3","1","3","3","1","1","2","1","3","2","3","2","1","1","2","1","1","2","3","2","3","3","1","2","3","2","1","3","3","2","3","3","2","1","3","1","3","1","2","3","1","2","2","3","3","3","2","3","2","3","2","1","3","3","2","3","2","3","1","1","2","3","3","3"],"TStage":["4","4","2","1","1","4","4","4","4","1","1","3","2","4","2","4","4","4","4","4","4","4","2","3","1","2","2","4","4","2","4","2","2","3","3","4","4","4","4","4","4","3","4","2","3","3","3","4","4","2","3","4","3","4","4","3","4","1","4","4","4","2","4","2","1","4","4","4","3","2","3","4","4","1","4","4","2","2","2","3","4","4","4","4","4","4","3","4","4","3","4","4","4","3","2","4","4","4","4","2","4","2","1","4","4","4","2","1","3","1","4","2","4","4","2","1","3","3","2","2","4","4","4","3","4","2","3","3","2","4","4","4","4","4","1","4","2","4","4","3","4","4","2","3","2","2","2","1","3","2","3","2","3","4","3","4","4","4","3","2","4","3","3","4","4","1","4","3","3","4","4","4","4","3","4","2","1","3","4","3","1","3","3","3","4","3","3","4","3","4","2","4","3","4","3","3","3","3","2","4","4","4","2","2","1","2","1","4","2","4","2","4","1","3","4","1","3","3","2","3","3","3","3","3","4","2","2","4","1","4","2","3","3","3","3","2","3","4","4","4","2","1","4","3","3","4","3","4","4","3"],"AntiX_intensity":[2,3,3,3,2,2,1,3,2,2,3,2,3,1,3,3,3,3,2,3,3,2,2,3,2,3,1,2,3,2,3,3,2,2,3,3,2,3,2,3,2,3,3,3,2,2,1,2,2,3,2,3,3,2,3,3,2,2,2,1,2,2,3,2,3,2,3,2,3,3,3,2,1,3,3,2,3,3,1,3,2,3,2,2,2,3,3,3,3,1,2,2,2,2,2,2,2,2,3,1,3,2,3,3,2,2,1,3,3,3,3,2,2,2,3,2,2,3,2,3,1,3,1,2,2,3,2,2,2,3,3,2,3,2,1,2,2,2,3,3,2,3,3,2,2,2,1,3,3,3,3,3,2,1,3,3,2,2,3,3,2,2,3,3,2,2,2,2,3,3,2,3,1,2,2,3,3,3,2,3,3,3,3,3,3,2,3,3,2,3,1,2,3,2,2,3,1,3,2,3,3,3,3,3,2,2,2,2,3,2,3,1,3,3,1,2,"NA",3,3,2,3,1,2,3,3,3,3,3,2,3,3,2,1,3,3,1,2,2,3,2,3,3,1,2,3,2,3,1,3,2],"AntiY_intensity":[1,1,1,1,3,3,2,3,2,2,2,1,1,2,2,2,2,2,1,2,1,1,2,1,3,2,3,1,1,3,2,2,2,2,1,1,3,3,1,3,1,1,1,1,2,2,1,1,2,3,2,2,1,1,2,3,1,1,2,3,1,1,3,2,2,2,2,3,2,2,1,2,2,2,1,1,1,3,2,2,1,2,2,1,1,2,2,2,1,2,3,3,2,2,1,1,1,2,2,2,3,2,1,2,2,1,1,1,1,3,2,1,2,1,3,3,3,2,3,2,2,2,3,3,2,2,1,2,2,1,2,1,1,1,1,2,1,2,3,1,2,3,2,1,1,2,2,2,3,3,2,1,2,2,3,2,3,1,1,2,1,2,2,1,2,2,2,2,1,2,1,2,1,1,2,1,1,3,1,3,3,1,3,1,2,2,3,3,2,3,2,3,1,1,2,2,3,2,3,1,1,1,2,1,3,3,2,3,1,2,2,3,1,3,2,3,2,3,2,3,3,1,2,3,2,2,3,3,1,2,1,1,3,3,2,2,2,3,2,1,2,3,3,2,2,2,2,2,"NA",3],"LymphNodeMetastasis":["Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Present","Absent","Present","Present","Present","Present","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent",null,"Absent","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Present","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Present","Present","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Present","Present","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Present","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Present","Absent","Present","Present","Absent","Present","Present","Present","Present","Absent","Absent","Present","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present"],"Valid":[false,true,false,false,true,false,false,false,false,false,true,true,true,false,false,false,false,false,false,false,false,false,false,true,true,false,false,false,false,true,false,false,false,true,true,false,true,true,false,false,true,false,false,true,true,false,true,true,false,false,true,false,true,true,true,false,false,false,false,false,false,false,true,false,false,false,false,false,true,false,true,true,false,true,true,true,true,true,true,false,true,true,true,false,false,true,true,true,false,false,false,false,true,false,false,true,false,true,true,false,true,false,false,true,false,false,false,true,false,true,false,false,true,false,false,false,false,false,false,true,false,true,true,false,true,true,false,true,true,false,false,true,true,true,false,true,false,false,true,true,false,true,false,false,true,false,true,true,false,true,true,true,true,false,true,true,true,true,false,true,false,true,true,false,null,false,true,false,false,true,false,false,false,true,false,true,true,false,true,false,true,true,true,false,true,true,false,false,true,false,true,true,true,true,false,false,true,false,true,false,true,true,false,false,true,false,false,false,false,false,false,false,true,true,true,true,true,true,false,true,false,false,false,true,false,true,false,true,true,false,false,true,false,true,true,false,false,true,true,false,true,false,false,true,false,true,false,false,true,false],"Smoker":[false,false,true,true,false,true,false,true,false,false,false,false,false,true,true,false,false,true,true,false,false,false,false,false,true,true,true,true,false,false,true,true,false,true,false,true,false,false,false,true,false,false,true,false,true,true,true,true,false,false,true,true,false,true,false,false,true,true,false,false,false,false,false,true,false,false,false,false,false,false,true,false,false,true,false,false,true,true,false,false,true,false,false,true,true,true,true,false,true,false,false,false,false,true,false,false,false,true,true,false,false,false,true,true,false,true,true,false,false,true,true,true,true,true,true,false,false,false,true,false,true,true,false,true,true,true,true,false,false,true,false,true,true,true,true,true,false,true,true,true,true,false,false,true,false,false,false,true,false,false,false,true,false,false,false,false,false,true,true,true,false,true,true,false,true,false,true,false,false,true,true,false,false,true,true,true,true,false,false,true,false,false,false,true,false,true,false,false,false,true,false,true,true,false,true,true,true,true,true,false,false,true,true,false,false,false,false,false,false,false,false,true,true,true,true,true,true,true,true,false,true,false,true,true,true,false,true,false,true,true,true,null,true,false,false,false,false,true,false,false,false,false,false,false,true,false,false,true,false,false],"Grade_Level":["low","low","moderate","high","high","moderate","high","high","high","low","high","moderate","moderate","moderate","high","high","high","moderate","low","high","high","moderate","high","low","high","low","high","low","low","low","moderate","moderate","low","moderate","high","moderate","moderate","moderate","moderate","high","low","low","high","moderate","low","moderate","moderate","high","moderate","high","high","moderate","moderate","low","high","low","high","low","moderate","moderate","moderate","moderate","low","high","low","moderate","low","high","high","low","moderate","moderate","low","high","moderate","low","high","high","moderate","moderate","high","high","high","low","low","high","low","high","high","high","moderate","high","low","low","moderate","low","low","low","moderate","moderate","low","moderate","high","low","low","moderate","moderate","high","low","low","low","low","low","low","low","low","low","low","moderate","moderate","moderate",null,"high","high","high","low","moderate","moderate","moderate","low","low","high","high","high","high","low","high","high","high","low","high","moderate","moderate","high","high","low","high","high","high","low","moderate","high","moderate","high","high","moderate","moderate","low","low","high","moderate","high","low","low","low","low","high","high","low","moderate","low","moderate","low","high","high","low","high","high","high","moderate","high","moderate","high","high","low","low","moderate","moderate","high","high","moderate","moderate","moderate","high","moderate","low","moderate","moderate","moderate","high","high","high","moderate","moderate","high","moderate","low","moderate","moderate","high","low","low","moderate","high","moderate","moderate","low","moderate","low","high","high","high","moderate","high","moderate","moderate","low","moderate","low","high","low","moderate","moderate","high","high","low","moderate","moderate","low","high","moderate","low","high","high","high","moderate","low","moderate","high","high"],"SurgeryDate":["2018-07-03T00:00:00","2018-08-14T00:00:00","2019-01-16T00:00:00","2019-05-18T00:00:00","2019-01-25T00:00:00","2019-01-11T00:00:00","2018-08-25T00:00:00","2019-03-10T00:00:00","2018-09-17T00:00:00","2018-05-02T00:00:00","2019-06-07T00:00:00","2019-02-28T00:00:00","2019-05-24T00:00:00","2019-04-29T00:00:00","2019-04-14T00:00:00","2018-09-22T00:00:00","2018-09-16T00:00:00","2018-11-22T00:00:00","2019-04-28T00:00:00","2019-04-13T00:00:00","2019-01-04T00:00:00","2019-07-17T00:00:00","2018-11-13T00:00:00","2019-08-11T00:00:00","2019-06-11T00:00:00","2019-07-08T00:00:00","2019-09-17T00:00:00","2019-02-18T00:00:00","2019-01-18T00:00:00","2019-03-28T00:00:00","2019-05-10T00:00:00","2018-08-05T00:00:00","2019-02-05T00:00:00","2019-01-27T00:00:00",null,"2019-01-22T00:00:00","2019-08-20T00:00:00","2018-10-04T00:00:00","2018-06-25T00:00:00","2018-10-13T00:00:00","2018-12-03T00:00:00","2018-12-18T00:00:00","2019-03-05T00:00:00","2018-05-29T00:00:00","2019-01-31T00:00:00","2019-03-22T00:00:00","2018-09-11T00:00:00","2019-05-16T00:00:00","2018-12-08T00:00:00","2018-09-29T00:00:00","2018-11-10T00:00:00","2018-08-23T00:00:00","2018-07-31T00:00:00","2018-06-04T00:00:00","2019-01-18T00:00:00","2018-11-15T00:00:00","2018-05-06T00:00:00","2019-02-13T00:00:00","2018-07-31T00:00:00","2019-01-01T00:00:00","2019-02-25T00:00:00","2019-04-16T00:00:00","2019-04-30T00:00:00","2018-09-23T00:00:00","2018-07-08T00:00:00","2018-10-05T00:00:00","2018-08-14T00:00:00","2018-07-30T00:00:00","2019-09-22T00:00:00","2019-05-21T00:00:00","2019-11-09T00:00:00","2018-10-19T00:00:00","2018-12-25T00:00:00","2018-08-08T00:00:00","2019-06-12T00:00:00","2019-01-09T00:00:00","2018-07-08T00:00:00","2018-08-29T00:00:00","2019-04-16T00:00:00","2019-08-05T00:00:00","2019-01-14T00:00:00","2018-10-12T00:00:00","2019-10-07T00:00:00","2018-12-09T00:00:00","2019-10-01T00:00:00","2018-11-25T00:00:00","2019-05-03T00:00:00","2018-12-28T00:00:00","2018-09-20T00:00:00","2018-12-13T00:00:00","2019-09-18T00:00:00","2019-06-18T00:00:00","2018-12-01T00:00:00","2018-09-28T00:00:00","2018-10-06T00:00:00","2019-07-04T00:00:00","2018-10-02T00:00:00","2019-05-21T00:00:00","2018-09-10T00:00:00","2019-01-08T00:00:00","2019-01-16T00:00:00","2018-10-08T00:00:00","2018-08-04T00:00:00","2019-02-06T00:00:00","2019-07-11T00:00:00","2019-07-16T00:00:00","2018-09-30T00:00:00","2018-08-24T00:00:00","2019-06-27T00:00:00","2019-05-02T00:00:00","2019-06-15T00:00:00","2019-02-20T00:00:00","2019-01-26T00:00:00","2019-02-25T00:00:00","2019-06-08T00:00:00","2019-04-29T00:00:00","2018-11-27T00:00:00","2019-03-01T00:00:00","2019-01-30T00:00:00","2019-05-01T00:00:00","2018-10-18T00:00:00","2019-01-17T00:00:00","2018-11-20T00:00:00","2018-12-25T00:00:00","2018-09-13T00:00:00","2018-05-25T00:00:00","2019-07-04T00:00:00","2018-07-17T00:00:00","2018-10-11T00:00:00","2019-01-30T00:00:00","2018-06-01T00:00:00","2018-11-09T00:00:00","2019-02-17T00:00:00","2019-01-29T00:00:00","2018-11-29T00:00:00","2019-05-29T00:00:00","2019-02-09T00:00:00","2018-10-16T00:00:00","2018-10-07T00:00:00","2019-02-07T00:00:00","2019-09-18T00:00:00","2018-06-10T00:00:00","2019-05-30T00:00:00","2019-03-05T00:00:00","2019-02-23T00:00:00","2018-05-27T00:00:00","2019-02-17T00:00:00","2018-11-01T00:00:00","2019-10-28T00:00:00","2019-01-02T00:00:00","2017-10-28T00:00:00","2017-03-31T00:00:00","2017-05-31T00:00:00","2017-07-25T00:00:00","2017-05-26T00:00:00","2017-03-31T00:00:00","2016-07-12T00:00:00","2017-10-17T00:00:00","2017-12-20T00:00:00","2017-02-16T00:00:00","2018-05-26T00:00:00","2018-02-27T00:00:00","2017-11-11T00:00:00","2017-06-20T00:00:00","2017-04-21T00:00:00","2018-01-07T00:00:00","2018-11-23T00:00:00","2017-03-05T00:00:00","2016-09-20T00:00:00","2018-09-20T00:00:00","2017-07-22T00:00:00","2016-08-06T00:00:00","2017-05-22T00:00:00","2016-08-02T00:00:00","2017-09-01T00:00:00","2017-12-02T00:00:00","2018-06-17T00:00:00","2017-03-13T00:00:00","2017-06-26T00:00:00","2018-07-08T00:00:00","2018-05-31T00:00:00","2017-12-23T00:00:00","2018-09-03T00:00:00","2018-02-16T00:00:00","2017-02-22T00:00:00","2017-01-18T00:00:00","2017-02-15T00:00:00","2018-02-09T00:00:00","2018-09-09T00:00:00","2017-06-19T00:00:00","2017-08-06T00:00:00","2018-08-01T00:00:00","2017-03-31T00:00:00","2017-04-08T00:00:00","2018-08-15T00:00:00","2017-06-01T00:00:00","2017-08-19T00:00:00","2017-07-11T00:00:00","2018-06-14T00:00:00","2017-04-25T00:00:00","2017-12-07T00:00:00","2016-07-18T00:00:00","2017-05-07T00:00:00","2017-06-13T00:00:00","2017-12-25T00:00:00","2017-07-02T00:00:00","2017-08-15T00:00:00","2017-11-10T00:00:00","2017-08-20T00:00:00","2017-01-04T00:00:00","2018-08-11T00:00:00","2016-12-15T00:00:00","2017-11-13T00:00:00","2018-01-28T00:00:00","2017-11-16T00:00:00","2017-05-31T00:00:00","2018-03-08T00:00:00","2017-04-13T00:00:00","2018-10-31T00:00:00","2018-09-20T00:00:00","2017-02-18T00:00:00","2016-10-16T00:00:00","2017-05-05T00:00:00","2018-04-12T00:00:00","2018-03-01T00:00:00","2015-05-11T00:00:00","2014-11-02T00:00:00","2015-01-08T00:00:00","2016-10-01T00:00:00","2015-05-16T00:00:00","2014-09-17T00:00:00","2015-01-07T00:00:00","2015-10-15T00:00:00","2014-09-04T00:00:00","2015-10-03T00:00:00","2014-12-01T00:00:00","2015-02-25T00:00:00","2015-02-27T00:00:00","2015-06-23T00:00:00","2016-02-05T00:00:00","2016-11-25T00:00:00","2015-06-22T00:00:00","2016-02-01T00:00:00","2015-05-14T00:00:00","2015-06-10T00:00:00","2016-06-21T00:00:00","2014-11-09T00:00:00","2016-08-03T00:00:00","2016-03-07T00:00:00","2016-03-06T00:00:00"],"DeathTime":["Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","MoreThan1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year"]},"columns":[{"accessor":"ID","name":"ID","type":"character"},{"accessor":"Name","name":"Name","type":"character"},{"accessor":"Sex","name":"Sex","type":"character"},{"accessor":"Age","name":"Age","type":"numeric"},{"accessor":"Race","name":"Race","type":"character"},{"accessor":"PreinvasiveComponent","name":"PreinvasiveComponent","type":"character"},{"accessor":"LVI","name":"LVI","type":"character"},{"accessor":"PNI","name":"PNI","type":"character"},{"accessor":"LastFollowUpDate","name":"LastFollowUpDate","type":"Date"},{"accessor":"Death","name":"Death","type":"logical"},{"accessor":"Group","name":"Group","type":"character"},{"accessor":"Grade","name":"Grade","type":"character"},{"accessor":"TStage","name":"TStage","type":"character"},{"accessor":"AntiX_intensity","name":"AntiX_intensity","type":"numeric"},{"accessor":"AntiY_intensity","name":"AntiY_intensity","type":"numeric"},{"accessor":"LymphNodeMetastasis","name":"LymphNodeMetastasis","type":"character"},{"accessor":"Valid","name":"Valid","type":"logical"},{"accessor":"Smoker","name":"Smoker","type":"logical"},{"accessor":"Grade_Level","name":"Grade_Level","type":"character"},{"accessor":"SurgeryDate","name":"SurgeryDate","type":"Date"},{"accessor":"DeathTime","name":"DeathTime","type":"character"}],"resizable":true,"filterable":true,"searchable":true,"defaultPageSize":10,"showPageSizeOptions":true,"pageSizeOptions":[10,25,50,100],"paginationType":"numbers","showPageInfo":true,"minRows":1,"highlight":true,"outlined":true,"striped":true,"compact":true,"nowrap":true,"showSortable":true,"dataKey":"f76f1245cb6e8a771df131cb45fc8801"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->








### Overview / Exploratory Data Analysis (EDA)


**Summary of Data via summarytools 📦**



```r
summarytools::view(summarytools::dfSummary(mydata %>% dplyr::select(-keycolumns)))
```



```r
if (!dir.exists(here::here("out"))) {
    dir.create(here::here("out"))
}

summarytools::view(x = summarytools::dfSummary(mydata %>% dplyr::select(-keycolumns)), 
    file = here::here("out", "mydata_summary.html"))
```


**Summary via dataMaid 📦**



```r
if (!dir.exists(here::here("out"))) {
    dir.create(here::here("out"))
}

dataMaid::makeDataReport(data = mydata, file = here::here("out", "dataMaid_mydata.Rmd"), 
    replace = TRUE, openResult = FALSE, render = FALSE, quiet = TRUE)
```


**Summary via explore 📦**


```r
if (!dir.exists(here::here("out"))) {
    dir.create(here::here("out"))
}

mydata %>% dplyr::select(-dateVariables) %>% explore::report(output_file = "mydata_report.html", 
    output_dir = here::here("out"))
```




**Glimpse of Data**



```r
dplyr::glimpse(mydata %>% dplyr::select(-keycolumns, -dateVariables))
```

```
Observations: 250
Variables: 17
$ Sex                  <chr> "Female", "Female", "Female", "Female", "Male", …
$ Age                  <dbl> 54, 39, 32, 58, 38, 50, 36, 70, 43, 45, 65, 27, …
$ Race                 <chr> "White", "White", "White", "Hispanic", "White", …
$ PreinvasiveComponent <chr> "Present", "Absent", "Absent", "Present", "Prese…
$ LVI                  <chr> "Present", "Absent", "Present", "Absent", "Absen…
$ PNI                  <chr> "Absent", "Absent", "Absent", "Present", "Absent…
$ Death                <lgl> TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRU…
$ Group                <chr> "Treatment", "Treatment", "Control", "Treatment"…
$ Grade                <chr> "1", "3", "3", "3", "2", "1", "2", "2", "1", "3"…
$ TStage               <chr> "4", "4", "2", "1", "1", "4", "4", "4", "4", "1"…
$ AntiX_intensity      <dbl> 2, 3, 3, 3, 2, 2, 1, 3, 2, 2, 3, 2, 3, 1, 3, 3, …
$ AntiY_intensity      <dbl> 1, 1, 1, 1, 3, 3, 2, 3, 2, 2, 2, 1, 1, 2, 2, 2, …
$ LymphNodeMetastasis  <chr> "Present", "Absent", "Absent", "Absent", "Presen…
$ Valid                <lgl> FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, F…
$ Smoker               <lgl> FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TR…
$ Grade_Level          <chr> "low", "low", "moderate", "high", "high", "moder…
$ DeathTime            <chr> "Within1Year", "Within1Year", "Within1Year", "Wi…
```




```r
mydata %>% explore::describe()
```

```
# A tibble: 21 x 8
   variable             type     na na_pct unique   min  mean   max
   <chr>                <chr> <int>  <dbl>  <int> <dbl> <dbl> <dbl>
 1 ID                   chr       0    0      250    NA NA       NA
 2 Name                 chr       1    0.4    250    NA NA       NA
 3 Sex                  chr       1    0.4      3    NA NA       NA
 4 Age                  dbl       1    0.4     50    25 48.6     73
 5 Race                 chr       1    0.4      8    NA NA       NA
 6 PreinvasiveComponent chr       1    0.4      3    NA NA       NA
 7 LVI                  chr       0    0        2    NA NA       NA
 8 PNI                  chr       1    0.4      3    NA NA       NA
 9 LastFollowUpDate     dat       1    0.4     13    NA NA       NA
10 Death                lgl       1    0.4      3     0  0.68     1
# … with 11 more rows
```




**Explore**


```r
explore::explore(mydata)
```






### Control Data

**Control Data if matching expectations**


```r
visdat::vis_expect(data = mydata, expectation = ~.x == -1, show_perc = TRUE)

visdat::vis_expect(mydata, ~.x >= 25)
```


**See missing values**


```r
visdat::vis_miss(airquality, cluster = TRUE)
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/missing values visdat-1.png)<!-- -->



```r
visdat::vis_miss(airquality, sort_miss = TRUE)
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/missing values visdat 2-1.png)<!-- -->





```r
xray::anomalies(mydata)
```

```
$variables
               Variable   q qNA  pNA qZero pZero qBlank pBlank qInf pInf
1                 Valid 250   1 0.4%   134 53.6%      0      -    0    -
2                Smoker 250   1 0.4%   132 52.8%      0      -    0    -
3                 Death 250   1 0.4%    80   32%      0      -    0    -
4                   Sex 250   1 0.4%     0     -      0      -    0    -
5  PreinvasiveComponent 250   1 0.4%     0     -      0      -    0    -
6                   PNI 250   1 0.4%     0     -      0      -    0    -
7                 Group 250   1 0.4%     0     -      0      -    0    -
8   LymphNodeMetastasis 250   1 0.4%     0     -      0      -    0    -
9                 Grade 250   1 0.4%     0     -      0      -    0    -
10      AntiX_intensity 250   1 0.4%     0     -      0      -    0    -
11      AntiY_intensity 250   1 0.4%     0     -      0      -    0    -
12          Grade_Level 250   1 0.4%     0     -      0      -    0    -
13                 Race 250   1 0.4%     0     -      0      -    0    -
14     LastFollowUpDate 250   1 0.4%     0     -      0      -    0    -
15                  Age 250   1 0.4%     0     -      0      -    0    -
16          SurgeryDate 250   1 0.4%     0     -      0      -    0    -
17                 Name 250   1 0.4%     0     -      0      -    0    -
18                  LVI 250   0    -     0     -      0      -    0    -
19            DeathTime 250   0    -     0     -      0      -    0    -
20               TStage 250   0    -     0     -      0      -    0    -
21                   ID 250   0    -     0     -      0      -    0    -
   qDistinct      type anomalous_percent
1          3   Logical               54%
2          3   Logical             53.2%
3          3   Logical             32.4%
4          3 Character              0.4%
5          3 Character              0.4%
6          3 Character              0.4%
7          3 Character              0.4%
8          3 Character              0.4%
9          4 Character              0.4%
10         4   Numeric              0.4%
11         4   Numeric              0.4%
12         4 Character              0.4%
13         8 Character              0.4%
14        13 Timestamp              0.4%
15        50   Numeric              0.4%
16       229 Timestamp              0.4%
17       250 Character              0.4%
18         2 Character                 -
19         2 Character                 -
20         4 Character                 -
21       250 Character                 -

$problem_variables
 [1] Variable          q                 qNA               pNA              
 [5] qZero             pZero             qBlank            pBlank           
 [9] qInf              pInf              qDistinct         type             
[13] anomalous_percent problems         
<0 rows> (or 0-length row.names)
```





```r
xray::distributions(mydata)
```

```
================================================================================
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/xray 2-1.png)<!-- -->![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/xray 2-2.png)<!-- -->![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/xray 2-3.png)<!-- -->![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/xray 2-4.png)<!-- -->

```
[1] "Ignoring variable LastFollowUpDate: Unsupported type for visualization."
[1] "Ignoring variable SurgeryDate: Unsupported type for visualization."
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/xray 2-5.png)<!-- -->![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/xray 2-6.png)<!-- -->

```
         Variable p_1 p_10 p_25 p_50 p_75 p_90 p_99
1 AntiX_intensity   1  1.8    2    2    3    3    3
2 AntiY_intensity   1    1    1    2    2    3    3
3             Age  25 29.8   36   48   61 68.2   73
```








### Explore Data 


**Summary of Data via DataExplorer 📦**



```r
DataExplorer::plot_str(mydata)
```



```r
DataExplorer::plot_str(mydata, type = "r")
```



```r
DataExplorer::introduce(mydata)
```

```
# A tibble: 1 x 9
   rows columns discrete_columns continuous_colu… all_missing_col…
  <int>   <int>            <int>            <int>            <int>
1   250      21               18                3                0
# … with 4 more variables: total_missing_values <int>, complete_rows <int>,
#   total_observations <int>, memory_usage <dbl>
```



```r
DataExplorer::plot_intro(mydata)
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/DataExplorer 4-1.png)<!-- -->



```r
DataExplorer::plot_missing(mydata)
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/DataExplorer 5-1.png)<!-- -->

**Drop columns**


```r
mydata2 <- DataExplorer::drop_columns(mydata, "TStage")
```



```r
DataExplorer::plot_bar(mydata)
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/DataExplorer 7-1.png)<!-- -->![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/DataExplorer 7-2.png)<!-- -->




```r
DataExplorer::plot_bar(mydata, with = "Death")
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/DataExplorer 8-1.png)<!-- -->![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/DataExplorer 8-2.png)<!-- -->



```r
DataExplorer::plot_histogram(mydata)
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/DataExplorer 9-1.png)<!-- -->




---



<!-- extracodes are below -->

<!-- ### dataMaid -->




















































































---

# Statistical Analysis

**Learn these tests as highlighted in [@Schmidt2017].**^[Statistical Literacy Among Academic Pathologists: A Survey Study to Gauge Knowledge of Frequently Used Statistical Tests Among Trainees and Faculty. Archives of Pathology & Laboratory Medicine: February 2017, Vol. 141, No. 2, pp. 279-287. https://doi.org/10.5858/arpa.2016-0200-OA]


---

# Results

**Write results as described in [@Knijn2015]**^[From Table 1: Proposed items for reporting histopathology studies. Recommendations for reporting histopathology studies: a proposal Virchows Arch (2015) 466:611–615 DOI 10.1007/s00428-015-1762-3 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4460276/]



- Describe the number of patients included in the analysis and reason for dropout

- Report patient/disease characteristics (including the biomarker of interest) with the number of missing values

- Describe the interaction of the biomarker of interest with established prognostic variables

- Include at least 90 % of initial cases included in univariate and multivariate analyses

- Report the estimated effect (relative risk/odds ratio, confidence interval, and p value) in univariate analysis

- Report the estimated effect (hazard rate/odds ratio, confidence interval, and p value) in multivariate analysis

- Report the estimated effects (hazard ratio/odds ratio, confidence interval, and p value) of other prognostic factors included in multivariate analysis



---


<!-- ## Data Dictionary -->

<!-- ```{r data dictionary, child = here::here('childRmd', '_08dataDictionary.Rmd')} -->
<!-- ``` -->

<!-- --- -->


<!-- ## Clean and Recode Data -->

<!-- ```{r clean and recode, child = here::here('childRmd', '_09cleanRecode.Rmd')} -->
<!-- ``` -->

<!-- --- -->

<!-- ## Impute Missing Data -->

<!-- ```{r impute, child = here::here('childRmd', '_10impute.Rmd')} -->
<!-- ``` -->

<!-- --- -->

<!-- \pagebreak -->


## Descriptive Statistics



**Codes for Descriptive Statistics**.^[See [`childRmd/_11descriptives.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_11descriptives.Rmd) file for other codes]


### Table One   

**Report Data properties via report 📦**


```r
mydata %>% dplyr::select(-dplyr::contains("Date")) %>% report::report()
```

```
The data contains 250 observations of the following variables:
  - ID: 250 entries: 001, n = 1; 002, n = 1; 003, n = 1 and 247 others (0 missing)
  - Name: 249 entries: Aaleyah, n = 1; Abrea, n = 1; Afonso, n = 1 and 246 others (1 missing)
  - Sex: 2 entries: Male, n = 127; Female, n = 122 (1 missing)
  - Age: Mean = 48.59, SD = 14.12, Median = , MAD = 17.79, range: [25, 73], Skewness = 0.07, Kurtosis = -1.17, 1 missing
  - Race: 7 entries: White, n = 151; Hispanic, n = 49; Black, n = 30 and 4 others (1 missing)
  - PreinvasiveComponent: 2 entries: Absent, n = 205; Present, n = 44 (1 missing)
  - LVI: 2 entries: Absent, n = 148; Present, n = 102 (0 missing)
  - PNI: 2 entries: Absent, n = 181; Present, n = 68 (1 missing)
  - Death: 2 levels: FALSE (n = 80, 32.00%); TRUE (n = 169, 67.60%) and missing (n = 1, 0.40%)
  - Group: 2 entries: Treatment, n = 130; Control, n = 119 (1 missing)
  - Grade: 3 entries: 3, n = 97; 2, n = 84; 1, n = 68 (1 missing)
  - TStage: 4 entries: 4, n = 113; 3, n = 65; 2, n = 49 and 1 other (0 missing)
  - AntiX_intensity: Mean = 2.39, SD = 0.66, Median = , MAD = 1.48, range: [1, 3], Skewness = -0.62, Kurtosis = -0.66, 1 missing
  - AntiY_intensity: Mean = 1.91, SD = 0.75, Median = , MAD = 1.48, range: [1, 3], Skewness = 0.15, Kurtosis = -1.20, 1 missing
  - LymphNodeMetastasis: 2 entries: Absent, n = 147; Present, n = 102 (1 missing)
  - Valid: 2 levels: FALSE (n = 134, 53.60%); TRUE (n = 115, 46.00%) and missing (n = 1, 0.40%)
  - Smoker: 2 levels: FALSE (n = 132, 52.80%); TRUE (n = 117, 46.80%) and missing (n = 1, 0.40%)
  - Grade_Level: 3 entries: high, n = 91; moderate, n = 83; low, n = 75 (1 missing)
  - DeathTime: 2 entries: Within1Year, n = 149; MoreThan1Year, n = 101 (0 missing)
```


**Table 1 via arsenal 📦**


```r
# cat(names(mydata), sep = " + \n")
library(arsenal)
tab1 <- arsenal::tableby(
  ~ Sex +
    Age +
    Race +
    PreinvasiveComponent +
    LVI +
    PNI +
    Death +
    Group +
    Grade +
    TStage +
    # `Anti-X-intensity` +
    # `Anti-Y-intensity` +
    LymphNodeMetastasis +
    Valid +
    Smoker +
    Grade_Level
  ,
  data = mydata 
)
summary(tab1)
```



|                            | Overall (N=250) |
|:---------------------------|:---------------:|
|**Sex**                     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Female    |   122 (49.0%)   |
|&nbsp;&nbsp;&nbsp;Male      |   127 (51.0%)   |
|**Age**                     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Mean (SD) | 48.586 (14.116) |
|&nbsp;&nbsp;&nbsp;Range     | 25.000 - 73.000 |
|**Race**                    |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Asian     |    11 (4.4%)    |
|&nbsp;&nbsp;&nbsp;Bi-Racial |    5 (2.0%)     |
|&nbsp;&nbsp;&nbsp;Black     |   30 (12.0%)    |
|&nbsp;&nbsp;&nbsp;Hispanic  |   49 (19.7%)    |
|&nbsp;&nbsp;&nbsp;Native    |    1 (0.4%)     |
|&nbsp;&nbsp;&nbsp;Other     |    2 (0.8%)     |
|&nbsp;&nbsp;&nbsp;White     |   151 (60.6%)   |
|**PreinvasiveComponent**    |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   205 (82.3%)   |
|&nbsp;&nbsp;&nbsp;Present   |   44 (17.7%)    |
|**LVI**                     |                 |
|&nbsp;&nbsp;&nbsp;Absent    |   148 (59.2%)   |
|&nbsp;&nbsp;&nbsp;Present   |   102 (40.8%)   |
|**PNI**                     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   181 (72.7%)   |
|&nbsp;&nbsp;&nbsp;Present   |   68 (27.3%)    |
|**Death**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;FALSE     |   80 (32.1%)    |
|&nbsp;&nbsp;&nbsp;TRUE      |   169 (67.9%)   |
|**Group**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Control   |   119 (47.8%)   |
|&nbsp;&nbsp;&nbsp;Treatment |   130 (52.2%)   |
|**Grade**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;1         |   68 (27.3%)    |
|&nbsp;&nbsp;&nbsp;2         |   84 (33.7%)    |
|&nbsp;&nbsp;&nbsp;3         |   97 (39.0%)    |
|**TStage**                  |                 |
|&nbsp;&nbsp;&nbsp;1         |    23 (9.2%)    |
|&nbsp;&nbsp;&nbsp;2         |   49 (19.6%)    |
|&nbsp;&nbsp;&nbsp;3         |   65 (26.0%)    |
|&nbsp;&nbsp;&nbsp;4         |   113 (45.2%)   |
|**LymphNodeMetastasis**     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   147 (59.0%)   |
|&nbsp;&nbsp;&nbsp;Present   |   102 (41.0%)   |
|**Valid**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;FALSE     |   134 (53.8%)   |
|&nbsp;&nbsp;&nbsp;TRUE      |   115 (46.2%)   |
|**Smoker**                  |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;FALSE     |   132 (53.0%)   |
|&nbsp;&nbsp;&nbsp;TRUE      |   117 (47.0%)   |
|**Grade_Level**             |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;high      |   91 (36.5%)    |
|&nbsp;&nbsp;&nbsp;low       |   75 (30.1%)    |
|&nbsp;&nbsp;&nbsp;moderate  |   83 (33.3%)    |



**Table 1 via tableone 📦**

<!-- https://cran.r-project.org/web/packages/tableone/index.html -->
<!-- https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html -->
<!-- library(survival) -->
<!-- data(pbc) -->



```r
library(tableone)
mydata %>% dplyr::select(-keycolumns, -dateVariables) %>% tableone::CreateTableOne(data = .)
```

```
                                    
                                     Overall      
  n                                    250        
  Sex = Male (%)                       127 (51.0) 
  Age (mean (SD))                    48.59 (14.12)
  Race (%)                                        
     Asian                              11 ( 4.4) 
     Bi-Racial                           5 ( 2.0) 
     Black                              30 (12.0) 
     Hispanic                           49 (19.7) 
     Native                              1 ( 0.4) 
     Other                               2 ( 0.8) 
     White                             151 (60.6) 
  PreinvasiveComponent = Present (%)    44 (17.7) 
  LVI = Present (%)                    102 (40.8) 
  PNI = Present (%)                     68 (27.3) 
  Death = TRUE (%)                     169 (67.9) 
  Group = Treatment (%)                130 (52.2) 
  Grade (%)                                       
     1                                  68 (27.3) 
     2                                  84 (33.7) 
     3                                  97 (39.0) 
  TStage (%)                                      
     1                                  23 ( 9.2) 
     2                                  49 (19.6) 
     3                                  65 (26.0) 
     4                                 113 (45.2) 
  AntiX_intensity (mean (SD))         2.39 (0.66) 
  AntiY_intensity (mean (SD))         1.91 (0.75) 
  LymphNodeMetastasis = Present (%)    102 (41.0) 
  Valid = TRUE (%)                     115 (46.2) 
  Smoker = TRUE (%)                    117 (47.0) 
  Grade_Level (%)                                 
     high                               91 (36.5) 
     low                                75 (30.1) 
     moderate                           83 (33.3) 
  DeathTime = Within1Year (%)          149 (59.6) 
```



















<!-- **Table 1 via atable 📦** -->

<!-- https://cran.r-project.org/web/packages/atable/vignettes/atable_usage.pdf -->
<!-- https://journal.r-project.org/archive/2019/RJ-2019-001/index.html -->



**Descriptive Statistics of Continuous Variables**


```r
mydata %>% dplyr::select(continiousVariables, numericVariables, integerVariables) %>% 
    summarytools::descr(., style = "rmarkdown")
```




```r
print(summarytools::descr(mydata), method = "render", table.classes = "st-small")
```
 



```r
mydata %>% summarytools::descr(., stats = "common", transpose = TRUE, headings = FALSE)
```




```r
mydata %>% summarytools::descr(stats = "common") %>% summarytools::tb()
```



```r
mydata$Sex %>% summarytools::freq(cumul = FALSE, report.nas = FALSE) %>% summarytools::tb()
```





```r
mydata %>% explore::describe() %>% dplyr::filter(unique < 5)
```

```
# A tibble: 15 x 8
   variable             type     na na_pct unique   min  mean   max
   <chr>                <chr> <int>  <dbl>  <int> <dbl> <dbl> <dbl>
 1 Sex                  chr       1    0.4      3    NA NA       NA
 2 PreinvasiveComponent chr       1    0.4      3    NA NA       NA
 3 LVI                  chr       0    0        2    NA NA       NA
 4 PNI                  chr       1    0.4      3    NA NA       NA
 5 Death                lgl       1    0.4      3     0  0.68     1
 6 Group                chr       1    0.4      3    NA NA       NA
 7 Grade                chr       1    0.4      4    NA NA       NA
 8 TStage               chr       0    0        4    NA NA       NA
 9 AntiX_intensity      dbl       1    0.4      4     1  2.39     3
10 AntiY_intensity      dbl       1    0.4      4     1  1.91     3
11 LymphNodeMetastasis  chr       1    0.4      3    NA NA       NA
12 Valid                lgl       1    0.4      3     0  0.46     1
13 Smoker               lgl       1    0.4      3     0  0.47     1
14 Grade_Level          chr       1    0.4      4    NA NA       NA
15 DeathTime            chr       0    0        2    NA NA       NA
```



```r
mydata %>% explore::describe() %>% dplyr::filter(na > 0)
```

```
# A tibble: 17 x 8
   variable             type     na na_pct unique   min  mean   max
   <chr>                <chr> <int>  <dbl>  <int> <dbl> <dbl> <dbl>
 1 Name                 chr       1    0.4    250    NA NA       NA
 2 Sex                  chr       1    0.4      3    NA NA       NA
 3 Age                  dbl       1    0.4     50    25 48.6     73
 4 Race                 chr       1    0.4      8    NA NA       NA
 5 PreinvasiveComponent chr       1    0.4      3    NA NA       NA
 6 PNI                  chr       1    0.4      3    NA NA       NA
 7 LastFollowUpDate     dat       1    0.4     13    NA NA       NA
 8 Death                lgl       1    0.4      3     0  0.68     1
 9 Group                chr       1    0.4      3    NA NA       NA
10 Grade                chr       1    0.4      4    NA NA       NA
11 AntiX_intensity      dbl       1    0.4      4     1  2.39     3
12 AntiY_intensity      dbl       1    0.4      4     1  1.91     3
13 LymphNodeMetastasis  chr       1    0.4      3    NA NA       NA
14 Valid                lgl       1    0.4      3     0  0.46     1
15 Smoker               lgl       1    0.4      3     0  0.47     1
16 Grade_Level          chr       1    0.4      4    NA NA       NA
17 SurgeryDate          dat       1    0.4    229    NA NA       NA
```






```r
mydata %>% explore::describe()
```

```
# A tibble: 21 x 8
   variable             type     na na_pct unique   min  mean   max
   <chr>                <chr> <int>  <dbl>  <int> <dbl> <dbl> <dbl>
 1 ID                   chr       0    0      250    NA NA       NA
 2 Name                 chr       1    0.4    250    NA NA       NA
 3 Sex                  chr       1    0.4      3    NA NA       NA
 4 Age                  dbl       1    0.4     50    25 48.6     73
 5 Race                 chr       1    0.4      8    NA NA       NA
 6 PreinvasiveComponent chr       1    0.4      3    NA NA       NA
 7 LVI                  chr       0    0        2    NA NA       NA
 8 PNI                  chr       1    0.4      3    NA NA       NA
 9 LastFollowUpDate     dat       1    0.4     13    NA NA       NA
10 Death                lgl       1    0.4      3     0  0.68     1
# … with 11 more rows
```



### Categorical Variables

**Use `R/gc_desc_cat.R` to generate `gc_desc_cat.Rmd` containing descriptive statistics for categorical variables**



```r
source(here::here("R", "gc_desc_cat.R"))
```



#### Descriptive Statistics Sex  


```r
mydata %>% janitor::tabyl(Sex) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Sex         n  percent   valid_percent 
-------  ----  --------  --------------
Female    122  48.8%     49.0%         
Male      127  50.8%     51.0%         
NA          1  0.4%      -             

\pagebreak

#### Descriptive Statistics Race  


```r
mydata %>% janitor::tabyl(Race) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Race           n  percent   valid_percent 
----------  ----  --------  --------------
Asian         11  4.4%      4.4%          
Bi-Racial      5  2.0%      2.0%          
Black         30  12.0%     12.0%         
Hispanic      49  19.6%     19.7%         
Native         1  0.4%      0.4%          
Other          2  0.8%      0.8%          
White        151  60.4%     60.6%         
NA             1  0.4%      -             

\pagebreak

#### Descriptive Statistics PreinvasiveComponent  


```r
mydata %>% janitor::tabyl(PreinvasiveComponent) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



PreinvasiveComponent      n  percent   valid_percent 
---------------------  ----  --------  --------------
Absent                  205  82.0%     82.3%         
Present                  44  17.6%     17.7%         
NA                        1  0.4%      -             

\pagebreak

#### Descriptive Statistics LVI  


```r
mydata %>% janitor::tabyl(LVI) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



LVI          n  percent 
--------  ----  --------
Absent     148  59.2%   
Present    102  40.8%   

\pagebreak

#### Descriptive Statistics PNI  


```r
mydata %>% janitor::tabyl(PNI) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



PNI          n  percent   valid_percent 
--------  ----  --------  --------------
Absent     181  72.4%     72.7%         
Present     68  27.2%     27.3%         
NA           1  0.4%      -             

\pagebreak

#### Descriptive Statistics Group  


```r
mydata %>% janitor::tabyl(Group) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Group          n  percent   valid_percent 
----------  ----  --------  --------------
Control      119  47.6%     47.8%         
Treatment    130  52.0%     52.2%         
NA             1  0.4%      -             

\pagebreak

#### Descriptive Statistics Grade  


```r
mydata %>% janitor::tabyl(Grade) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Grade     n  percent   valid_percent 
------  ---  --------  --------------
1        68  27.2%     27.3%         
2        84  33.6%     33.7%         
3        97  38.8%     39.0%         
NA        1  0.4%      -             

\pagebreak

#### Descriptive Statistics TStage  


```r
mydata %>% janitor::tabyl(TStage) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



TStage      n  percent 
-------  ----  --------
1          23  9.2%    
2          49  19.6%   
3          65  26.0%   
4         113  45.2%   

\pagebreak

#### Descriptive Statistics LymphNodeMetastasis  


```r
mydata %>% janitor::tabyl(LymphNodeMetastasis) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



LymphNodeMetastasis      n  percent   valid_percent 
--------------------  ----  --------  --------------
Absent                 147  58.8%     59.0%         
Present                102  40.8%     41.0%         
NA                       1  0.4%      -             

\pagebreak

#### Descriptive Statistics Grade_Level  


```r
mydata %>% janitor::tabyl(Grade_Level) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Grade_Level     n  percent   valid_percent 
------------  ---  --------  --------------
high           91  36.4%     36.5%         
low            75  30.0%     30.1%         
moderate       83  33.2%     33.3%         
NA              1  0.4%      -             

\pagebreak

#### Descriptive Statistics DeathTime  


```r
mydata %>% janitor::tabyl(DeathTime) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



DeathTime          n  percent 
--------------  ----  --------
MoreThan1Year    101  40.4%   
Within1Year      149  59.6%   

\pagebreak








```r
race_stats <- summarytools::freq(mydata$Race)
print(race_stats, report.nas = FALSE, totals = FALSE, display.type = FALSE, Variable.label = "Race Group")
```





```r
mydata %>% explore::describe(PreinvasiveComponent)
```

```
variable = PreinvasiveComponent
type     = character
na       = 1 of 250 (0.4%)
unique   = 3
 Absent  = 205 (82%)
 Present = 44 (17.6%)
 NA      = 1 (0.4%)
```




```r
## Frequency or custom tables for categorical variables
SmartEDA::ExpCTable(mydata, Target = NULL, margin = 1, clim = 10, nlim = 5, round = 2, 
    bin = NULL, per = T)
```

```
               Variable         Valid Frequency Percent CumPercent
1                   Sex        Female       122    48.8       48.8
2                   Sex          Male       127    50.8       99.6
3                   Sex            NA         1     0.4      100.0
4                   Sex         TOTAL       250      NA         NA
5                  Race         Asian        11     4.4        4.4
6                  Race     Bi-Racial         5     2.0        6.4
7                  Race         Black        30    12.0       18.4
8                  Race      Hispanic        49    19.6       38.0
9                  Race            NA         1     0.4       38.4
10                 Race        Native         1     0.4       38.8
11                 Race         Other         2     0.8       39.6
12                 Race         White       151    60.4      100.0
13                 Race         TOTAL       250      NA         NA
14 PreinvasiveComponent        Absent       205    82.0       82.0
15 PreinvasiveComponent            NA         1     0.4       82.4
16 PreinvasiveComponent       Present        44    17.6      100.0
17 PreinvasiveComponent         TOTAL       250      NA         NA
18                  LVI        Absent       148    59.2       59.2
19                  LVI       Present       102    40.8      100.0
20                  LVI         TOTAL       250      NA         NA
21                  PNI        Absent       181    72.4       72.4
22                  PNI            NA         1     0.4       72.8
23                  PNI       Present        68    27.2      100.0
24                  PNI         TOTAL       250      NA         NA
25                Group       Control       119    47.6       47.6
26                Group            NA         1     0.4       48.0
27                Group     Treatment       130    52.0      100.0
28                Group         TOTAL       250      NA         NA
29                Grade             1        68    27.2       27.2
30                Grade             2        84    33.6       60.8
31                Grade             3        97    38.8       99.6
32                Grade            NA         1     0.4      100.0
33                Grade         TOTAL       250      NA         NA
34               TStage             1        23     9.2        9.2
35               TStage             2        49    19.6       28.8
36               TStage             3        65    26.0       54.8
37               TStage             4       113    45.2      100.0
38               TStage         TOTAL       250      NA         NA
39  LymphNodeMetastasis        Absent       147    58.8       58.8
40  LymphNodeMetastasis            NA         1     0.4       59.2
41  LymphNodeMetastasis       Present       102    40.8      100.0
42  LymphNodeMetastasis         TOTAL       250      NA         NA
43          Grade_Level          high        91    36.4       36.4
44          Grade_Level           low        75    30.0       66.4
45          Grade_Level      moderate        83    33.2       99.6
46          Grade_Level            NA         1     0.4      100.0
47          Grade_Level         TOTAL       250      NA         NA
48            DeathTime MoreThan1Year       101    40.4       40.4
49            DeathTime   Within1Year       149    59.6      100.0
50            DeathTime         TOTAL       250      NA         NA
51      AntiX_intensity             1        25    10.0       10.0
52      AntiX_intensity             2       103    41.2       51.2
53      AntiX_intensity             3       121    48.4       99.6
54      AntiX_intensity            NA         1     0.4      100.0
55      AntiX_intensity         TOTAL       250      NA         NA
56      AntiY_intensity             1        82    32.8       32.8
57      AntiY_intensity             2       108    43.2       76.0
58      AntiY_intensity             3        59    23.6       99.6
59      AntiY_intensity            NA         1     0.4      100.0
60      AntiY_intensity         TOTAL       250      NA         NA
```




```r
inspectdf::inspect_cat(mydata)
```

```
# A tibble: 16 x 5
   col_name               cnt common      common_pcnt levels            
   <chr>                <int> <chr>             <dbl> <named list>      
 1 Death                    3 TRUE               67.6 <tibble [3 × 3]>  
 2 DeathTime                2 Within1Year        59.6 <tibble [2 × 3]>  
 3 Grade                    4 3                  38.8 <tibble [4 × 3]>  
 4 Grade_Level              4 high               36.4 <tibble [4 × 3]>  
 5 Group                    3 Treatment          52   <tibble [3 × 3]>  
 6 ID                     250 001                 0.4 <tibble [250 × 3]>
 7 LVI                      2 Absent             59.2 <tibble [2 × 3]>  
 8 LymphNodeMetastasis      3 Absent             58.8 <tibble [3 × 3]>  
 9 Name                   250 Aaleyah             0.4 <tibble [250 × 3]>
10 PNI                      3 Absent             72.4 <tibble [3 × 3]>  
11 PreinvasiveComponent     3 Absent             82   <tibble [3 × 3]>  
12 Race                     8 White              60.4 <tibble [8 × 3]>  
13 Sex                      3 Male               50.8 <tibble [3 × 3]>  
14 Smoker                   3 FALSE              52.8 <tibble [3 × 3]>  
15 TStage                   4 4                  45.2 <tibble [4 × 3]>  
16 Valid                    3 FALSE              53.6 <tibble [3 × 3]>  
```

```r
inspectdf::inspect_cat(mydata)$levels$Group
```

```
# A tibble: 3 x 3
  value      prop   cnt
  <chr>     <dbl> <int>
1 Treatment 0.52    130
2 Control   0.476   119
3 <NA>      0.004     1
```



#### Split-Group Stats Categorical



```r
library(summarytools)

grouped_freqs <- stby(data = mydata$Smoker, INDICES = mydata$Sex, FUN = freq, cumul = FALSE, 
    report.nas = FALSE)

grouped_freqs %>% tb(order = 2)
```



#### Grouped Categorical



```r
summarytools::stby(list(x = mydata$LVI, y = mydata$LymphNodeMetastasis), mydata$PNI, 
    summarytools::ctable)
```




```r
with(mydata, summarytools::stby(list(x = LVI, y = LymphNodeMetastasis), PNI, summarytools::ctable))
```








```r
mydata %>% dplyr::select(characterVariables) %>% dplyr::select(PreinvasiveComponent, 
    PNI, LVI) %>% reactable::reactable(data = ., groupBy = c("PreinvasiveComponent", 
    "PNI"), columns = list(LVI = reactable::colDef(aggregate = "count")))
```




\pagebreak


---


### Continious Variables



```r
questionr:::icut()
```




```r
source(here::here("R", "gc_desc_cont.R"))
```




**Descriptive Statistics Age** 


```r
mydata %>% jmv::descriptives(data = ., vars = "Age", hist = TRUE, dens = TRUE, box = TRUE, 
    violin = TRUE, dot = TRUE, mode = TRUE, sd = TRUE, variance = TRUE, skew = TRUE, 
    kurt = TRUE, quart = TRUE)
```

```

 DESCRIPTIVES

 Descriptives                      
 ───────────────────────────────── 
                          Age      
 ───────────────────────────────── 
   N                         249   
   Missing                     1   
   Mean                     48.6   
   Median                   48.0   
   Mode                     48.0   
   Standard deviation       14.1   
   Variance                  199   
   Minimum                  25.0   
   Maximum                  73.0   
   Skewness               0.0724   
   Std. error skewness     0.154   
   Kurtosis                -1.17   
   Std. error kurtosis     0.307   
   25th percentile          36.0   
   50th percentile          48.0   
   75th percentile          61.0   
 ───────────────────────────────── 
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/Descriptive Statistics Age-1.png)<!-- -->![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/Descriptive Statistics Age-2.png)<!-- -->

\pagebreak

**Descriptive Statistics AntiX_intensity** 


```r
mydata %>% jmv::descriptives(data = ., vars = "AntiX_intensity", hist = TRUE, dens = TRUE, 
    box = TRUE, violin = TRUE, dot = TRUE, mode = TRUE, sd = TRUE, variance = TRUE, 
    skew = TRUE, kurt = TRUE, quart = TRUE)
```

```

 DESCRIPTIVES

 Descriptives                               
 ────────────────────────────────────────── 
                          AntiX_intensity   
 ────────────────────────────────────────── 
   N                                  249   
   Missing                              1   
   Mean                              2.39   
   Median                            2.00   
   Mode                              3.00   
   Standard deviation               0.663   
   Variance                         0.439   
   Minimum                           1.00   
   Maximum                           3.00   
   Skewness                        -0.618   
   Std. error skewness              0.154   
   Kurtosis                        -0.648   
   Std. error kurtosis              0.307   
   25th percentile                   2.00   
   50th percentile                   2.00   
   75th percentile                   3.00   
 ────────────────────────────────────────── 
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/Descriptive Statistics AntiX_intensity-1.png)<!-- -->![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/Descriptive Statistics AntiX_intensity-2.png)<!-- -->

\pagebreak

**Descriptive Statistics AntiY_intensity** 


```r
mydata %>% jmv::descriptives(data = ., vars = "AntiY_intensity", hist = TRUE, dens = TRUE, 
    box = TRUE, violin = TRUE, dot = TRUE, mode = TRUE, sd = TRUE, variance = TRUE, 
    skew = TRUE, kurt = TRUE, quart = TRUE)
```

```

 DESCRIPTIVES

 Descriptives                               
 ────────────────────────────────────────── 
                          AntiY_intensity   
 ────────────────────────────────────────── 
   N                                  249   
   Missing                              1   
   Mean                              1.91   
   Median                            2.00   
   Mode                              2.00   
   Standard deviation               0.748   
   Variance                         0.560   
   Minimum                           1.00   
   Maximum                           3.00   
   Skewness                         0.152   
   Std. error skewness              0.154   
   Kurtosis                         -1.20   
   Std. error kurtosis              0.307   
   25th percentile                   1.00   
   50th percentile                   2.00   
   75th percentile                   2.00   
 ────────────────────────────────────────── 
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/Descriptive Statistics AntiY_intensity-1.png)<!-- -->![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/Descriptive Statistics AntiY_intensity-2.png)<!-- -->

\pagebreak





```r
tab <- tableone::CreateTableOne(data = mydata)
# ?print.ContTable
tab$ContTable
```

```
                             
                              Overall      
  n                           250          
  Age (mean (SD))             48.59 (14.12)
  AntiX_intensity (mean (SD))  2.39 (0.66) 
  AntiY_intensity (mean (SD))  1.91 (0.75) 
```

```r
print(tab$ContTable, nonnormal = c("Anti-X-intensity"))
```

```
                             
                              Overall      
  n                           250          
  Age (mean (SD))             48.59 (14.12)
  AntiX_intensity (mean (SD))  2.39 (0.66) 
  AntiY_intensity (mean (SD))  1.91 (0.75) 
```




```r
mydata %>% explore::describe(Age)
```

```
variable = Age
type     = double
na       = 1 of 250 (0.4%)
unique   = 50
min|max  = 25 | 73
q05|q95  = 27.4 | 71
q25|q75  = 36 | 61
median   = 48
mean     = 48.58635
```



```r
mydata %>% dplyr::select(continiousVariables) %>% SmartEDA::ExpNumStat(data = ., 
    by = "A", gp = NULL, Qnt = seq(0, 1, 0.1), MesofShape = 2, Outlier = TRUE, round = 2)
```




```r
inspectdf::inspect_num(mydata, breaks = 10)
```

```
# A tibble: 3 x 10
  col_name        min    q1 median  mean    q3   max     sd pcnt_na hist        
  <chr>         <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl> <named list>
1 Age              25    36     48 48.6     61    73 14.1       0.4 <tibble [12…
2 AntiX_intens…     1     2      2  2.39     3     3  0.663     0.4 <tibble [12…
3 AntiY_intens…     1     1      2  1.91     2     3  0.748     0.4 <tibble [12…
```



```r
inspectdf::inspect_num(mydata)$hist$Age
```

```
# A tibble: 27 x 2
   value        prop
   <chr>       <dbl>
 1 [-Inf, 24) 0     
 2 [24, 26)   0.0241
 3 [26, 28)   0.0281
 4 [28, 30)   0.0482
 5 [30, 32)   0.0402
 6 [32, 34)   0.0562
 7 [34, 36)   0.0402
 8 [36, 38)   0.0402
 9 [38, 40)   0.0321
10 [40, 42)   0.0402
# … with 17 more rows
```



```r
inspectdf::inspect_num(mydata, breaks = 10) %>% inspectdf::show_plot()
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/inspectdf 5-1.png)<!-- -->





#### Split-Group Stats Continious



```r
grouped_descr <- summarytools::stby(data = mydata, INDICES = mydata$Sex, FUN = summarytools::descr, 
    stats = "common")
# grouped_descr %>% summarytools::tb(order = 2)
grouped_descr %>% summarytools::tb()
```














#### Grouped Continious



```r
summarytools::stby(data = mydata, INDICES = mydata$PreinvasiveComponent, FUN = summarytools::descr, 
    stats = c("mean", "sd", "min", "med", "max"), transpose = TRUE)
```




```r
with(mydata, summarytools::stby(Age, PreinvasiveComponent, summarytools::descr), 
    stats = c("mean", "sd", "min", "med", "max"), transpose = TRUE)
```




```r
mydata %>% group_by(PreinvasiveComponent) %>% summarytools::descr(stats = "fivenum")
```




```r
## Summary statistics by – category
SmartEDA::ExpNumStat(mydata, by = "GA", gp = "PreinvasiveComponent", Qnt = seq(0, 
    1, 0.1), MesofShape = 2, Outlier = TRUE, round = 2)
```

```
  Vname                        Group  TN nNeg nZero nPos NegInf PosInf NA_Value
1   Age     PreinvasiveComponent:All 250    0     0  249      0      0        1
2   Age PreinvasiveComponent:Present  44    0     0   44      0      0        0
3   Age  PreinvasiveComponent:Absent 205    0     0  204      0      0        1
4   Age      PreinvasiveComponent:NA   0    0     0    0      0      0        0
  Per_of_Missing   sum min  max  mean median    SD   CV   IQR Skewness Kurtosis
1           0.40 12098  25   73 48.59   48.0 14.12 0.29 25.00     0.07    -1.17
2           0.00  2144  25   73 48.73   48.5 15.11 0.31 27.75    -0.05    -1.26
3           0.49  9910  25   73 48.58   48.0 13.96 0.29 25.00     0.10    -1.16
4            NaN     0 Inf -Inf   NaN     NA    NA   NA    NA      NaN      NaN
  0%  10%  20%  30% 40%  50%  60%  70% 80%  90% 100% LB.25% UB.75% nOutliers
1 25 29.8 34.0 39.0  44 48.0 53.0 58.0  64 68.2   73  -1.50  98.50         0
2 25 29.0 30.6 40.0  45 48.5 54.0 58.3  64 69.4   73  -5.38 105.62         0
3 25 31.0 34.0 38.9  44 48.0 52.8 58.0  63 68.0   73  -1.50  98.50         0
4 NA   NA   NA   NA  NA   NA   NA   NA  NA   NA   NA     NA     NA         0
```



\pagebreak












---

\newpage
\blandscape


## Cross Tables


**Codes for cross tables**.^[See [`childRmd/_12crossTables.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_12crossTables.Rmd) file for other codes]





```r
library(finalfit)
```



```r
dependent <- c("dependent1", "dependent2")

explanatory <- c("explanatory1", "explanatory2")

dependent <- "PreinvasiveComponent"

explanatory <- c("Sex", "Age", "Grade", "TStage")
```



Change `column = TRUE` argument to get row or column percentages.



```r
source(here::here("R", "gc_table_cross.R"))
```




**Cross Table PreinvasiveComponent** 


```r
mydata %>%
    summary_factorlist(dependent = 'PreinvasiveComponent', 
                       explanatory = explanatory,
                       # column = TRUE,
                       total_col = TRUE,
                       p = TRUE,
                       add_dependent_label = TRUE,
                       na_include=FALSE
                       # catTest = catTestfisher
                       ) -> table

knitr::kable(table, row.names = FALSE, align = c('l', 'l', 'r', 'r', 'r'))
```



Dependent: PreinvasiveComponent                     Absent       Present         Total  p     
--------------------------------  ----------  ------------  ------------  ------------  ------
Sex                               Female        101 (49.5)     20 (45.5)    121 (48.8)  0.625 
                                  Male          103 (50.5)     24 (54.5)    127 (51.2)        
Age                               Mean (SD)    48.6 (14.0)   48.7 (15.1)   48.6 (14.1)  0.997 
Grade                             1              53 (25.9)     15 (34.9)     68 (27.4)  0.453 
                                  2              71 (34.6)     12 (27.9)     83 (33.5)        
                                  3              81 (39.5)     16 (37.2)     97 (39.1)        
TStage                            1               18 (8.8)      5 (11.4)      23 (9.2)  0.934 
                                  2              41 (20.0)      8 (18.2)     49 (19.7)        
                                  3              52 (25.4)     12 (27.3)     64 (25.7)        
                                  4              94 (45.9)     19 (43.2)    113 (45.4)        

\pagebreak


\pagebreak


\pagebreak


\pagebreak


\pagebreak











<!-- https://cran.r-project.org/web/packages/arsenal/vignettes/tableby.html -->





<!-- https://cran.r-project.org/web/packages/arsenal/vignettes/write2.html -->


















```r
library(DT)
datatable(mtcars, rownames = FALSE, filter="top", options = list(pageLength = 5, scrollX=T) )
```







\elandscape


<!-- ```{r plots, child = here::here('childRmd', '_13plots.Rmd')} -->
<!-- ``` -->




<!-- ```{r pairedTests, child = here::here('childRmd', '_14pairedTests.Rmd')} -->
<!-- ``` -->



<!-- ```{r hypothesisTests, child = here::here('childRmd', '_15hypothesisTests.Rmd')} -->
<!-- ``` -->

<!-- \newpage -->
<!-- \blandscape -->



<!-- ```{r ROC, child = here::here('childRmd', '_16ROC.Rmd')} -->
<!-- ``` -->


<!-- ```{r Decision Tree, child = here::here('childRmd', '_17decisionTree.Rmd')} -->
<!-- ``` -->



## Survival Analysis



**Codes for Survival Analysis**^[See [`childRmd/_18survival.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_18survival.Rmd) file for other codes, and [`childRmd/_19shinySurvival.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_19shinySurvival.Rmd) for `shiny` application]


- Survival analysis with strata, clusters, frailties and competing risks in in Finalfit

https://www.datasurg.net/2019/09/12/survival-analysis-with-strata-clusters-frailties-and-competing-risks-in-in-finalfit/

- Intracranial WHO grade I meningioma: a competing risk analysis of progression and disease-specific survival

https://link.springer.com/article/10.1007/s00701-019-04096-9


**Calculate survival time**


```r
mydata$int <- lubridate::interval(lubridate::ymd(mydata$SurgeryDate), lubridate::ymd(mydata$LastFollowUpDate))
mydata$OverallTime <- lubridate::time_length(mydata$int, "month")
mydata$OverallTime <- round(mydata$OverallTime, digits = 1)
```




**recode death status outcome as numbers for survival analysis**

<!-- alive 0, death 1, punch cards -->

<!-- reference to addin -->



```r
## Recoding mydata$Death into mydata$Outcome
mydata$Outcome <- forcats::fct_recode(as.character(mydata$Death), `1` = "TRUE", `0` = "FALSE")
mydata$Outcome <- as.numeric(as.character(mydata$Outcome))
```

**it is always a good practice to double-check after recoding**^[[JAMA retraction after miscoding – new Finalfit function to check recoding](https://www.datasurg.net/2019/10/15/jama-retraction-after-miscoding-new-finalfit-function-to-check-recoding/)]

<!-- https://www.datasurg.net/2019/10/15/jama-retraction-after-miscoding-new-finalfit-function-to-check-recoding/ -->




```r
table(mydata$Death, mydata$Outcome)
```

```
       
          0   1
  FALSE  80   0
  TRUE    0 169
```



### Kaplan-Meier

<!-- ref to coursera -->



```r
library(survival)
# data(lung) km <- with(lung, Surv(time, status))
km <- with(mydata, Surv(OverallTime, Outcome))
head(km, 80)
```

```
 [1] 10.5   9.1   7.1+  8.0   5.8   8.2+  8.8+  9.3   9.0  10.5+  3.4+  8.7 
[13]  3.8   5.6   8.1   8.9   8.1   8.9   7.7+  5.2+ 10.5   4.0   7.2   5.2+
[25]  4.2   3.3   4.0   4.0+ 11.0  10.7+  7.3+ 11.4+  6.4   8.7    NA+  6.9+
[37]  5.9  10.5+  8.8   8.2  11.5+  7.0   6.4   9.6   5.6+  9.9+  8.2+  9.1 
[49]  3.3  11.6   7.3+ 10.8   7.6  11.5   4.0+  8.1+ 10.4+  6.2+  9.6   6.5 
[61] 11.8+  4.1   6.6   5.8+  9.3+  5.4   7.1   9.6   3.9   4.9   3.3   9.0 
[73]  5.8   9.3   5.2   8.3  10.3  10.6   7.1+  6.4 
```

```r
plot(km)
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/km-1.png)<!-- -->



**Kaplan-Meier Plot Log-Rank Test**


```r
# Drawing Survival Curves Using ggplot2
# https://rpkgs.datanovia.com/survminer/reference/ggsurvplot.html
dependentKM <- "Surv(OverallTime, Outcome)"
explanatoryKM <- "LVI"

mydata %>%
  finalfit::surv_plot(.data = .,
                      dependent = dependentKM,
                      explanatory = explanatoryKM,
                      xlab='Time (months)',
                      pval=TRUE,
                      legend = 'none',
                      break.time.by = 12,
                      xlim = c(0,60)
                      # legend.labs = c('a','b')
                      )
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/Kaplan-Meier Plot Log-Rank Test-1.png)<!-- -->




```r
# Drawing Survival Curves Using ggplot2
# https://rpkgs.datanovia.com/survminer/reference/ggsurvplot.html

mydata %>%
  finalfit::surv_plot(.data = .,
                      dependent = "Surv(OverallTime, Outcome)",
                      explanatory = "LVI",
                      xlab='Time (months)',
                      pval=TRUE,
                      legend = 'none',
                      break.time.by = 12,
                      xlim = c(0,60)
                      # legend.labs = c('a','b')
                      )
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/Kaplan-Meier Plot Log-Rank Test 2-1.png)<!-- -->




### Univariate Cox-Regression


```r
library(finalfit)
library(survival)
explanatoryUni <- "LVI"
dependentUni <- "Surv(OverallTime, Outcome)"

tUni <- mydata %>% finalfit::finalfit(dependentUni, explanatoryUni)

knitr::kable(tUni, row.names = FALSE, align = c("l", "l", "r", "r", "r", "r"))
```



Dependent: Surv(OverallTime, Outcome)                      all            HR (univariable)          HR (multivariable)
--------------------------------------  --------  ------------  --------------------------  --------------------------
LVI                                     Absent     148 (100.0)                           -                           -
                                        Present    102 (100.0)   1.45 (1.06-1.98, p=0.021)   1.45 (1.06-1.98, p=0.021)


```r
tUni_df <- tibble::as_tibble(tUni, .name_repair = "minimal") %>% janitor::clean_names()

tUni_df_descr <- paste0("When ", tUni_df$dependent_surv_overall_time_outcome[1], 
    " is ", tUni_df$x[2], ", there is ", tUni_df$hr_univariable[2], " times risk than ", 
    "when ", tUni_df$dependent_surv_overall_time_outcome[1], " is ", tUni_df$x[1], 
    ".")
```

<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

When LVI is Present, there is 1.45 (1.06-1.98, p=0.021) times risk than when LVI is Absent.

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

When LVI is Present, there is 1.45 (1.06-1.98, p=0.021) times risk than when LVI is Absent.

}
}



### Kaplan-Meier Median Survival


```r
km_fit <- survfit(Surv(OverallTime, Outcome) ~ LVI, data = mydata)
km_fit
```

```
Call: survfit(formula = Surv(OverallTime, Outcome) ~ LVI, data = mydata)

   3 observations deleted due to missingness 
              n events median 0.95LCL 0.95UCL
LVI=Absent  146    100   19.7    14.1    27.1
LVI=Present 101     69   10.5     9.3    12.8
```

```r
plot(km_fit)
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/Median Survivals-1.png)<!-- -->

```r
# summary(km_fit)
```



```r
km_fit_median_df <- summary(km_fit)
km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>% janitor::clean_names() %>% 
    tibble::rownames_to_column()
```

























```r
km_fit_median_definition <- km_fit_median_df %>% dplyr::mutate(description = glue::glue("When {rowname}, median survival is {median} [{x0_95lcl} - {x0_95ucl}, 95% CI] months.")) %>% 
    dplyr::select(description) %>% dplyr::pull()
```


<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

When LVI=Absent, median survival is 19.7 [14.1 - 27.1, 95% CI] months., When LVI=Present, median survival is 10.5 [9.3 - 12.8, 95% CI] months.

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

When LVI=Absent, median survival is 19.7 [14.1 - 27.1, 95% CI] months., When LVI=Present, median survival is 10.5 [9.3 - 12.8, 95% CI] months.

}
}




### 1-3-5-yr survival
      

```r
summary(km_fit, times = c(12, 36, 60))
```

```
Call: survfit(formula = Surv(OverallTime, Outcome) ~ LVI, data = mydata)

3 observations deleted due to missingness 
                LVI=Absent 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
   12     71      52    0.612  0.0426        0.534        0.701
   36     19      38    0.235  0.0424        0.165        0.334

                LVI=Present 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
   12     27      52    0.391  0.0544       0.2978        0.514
   36      5      15    0.120  0.0436       0.0593        0.245
```


```r
km_fit_summary <- summary(km_fit, times = c(12, 36, 60))

km_fit_df <- as.data.frame(km_fit_summary[c("strata", "time", "n.risk", "n.event", 
    "surv", "std.err", "lower", "upper")])
```




```r
km_fit_definition <- km_fit_df %>% dplyr::mutate(description = glue::glue("When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI].")) %>% 
    dplyr::select(description) %>% dplyr::pull()
```

<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

When LVI=Absent, 12 month survival is 61% [53%-70.1%, 95% CI]., When LVI=Absent, 36 month survival is 23% [16%-33.4%, 95% CI]., When LVI=Present, 12 month survival is 39% [30%-51.4%, 95% CI]., When LVI=Present, 36 month survival is 12% [6%-24.5%, 95% CI].

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

When LVI=Absent, 12 month survival is 61% [53%-70.1%, 95% CI]., When LVI=Absent, 36 month survival is 23% [16%-33.4%, 95% CI]., When LVI=Present, 12 month survival is 39% [30%-51.4%, 95% CI]., When LVI=Present, 36 month survival is 12% [6%-24.5%, 95% CI].

}
}














```r
source(here::here("R", "gc_survival.R"))
```



### Survival Analysis LVI  

**Kaplan-Meier Plot Log-Rank Test**


```r
library(survival)
library(survminer)
library(finalfit)

mydata %>%
  finalfit::surv_plot('Surv(OverallTime, Outcome)', 'LVI', 
  xlab='Time (months)', pval=TRUE, legend = 'none',
    break.time.by = 12, xlim = c(0,60)

# legend.labs = c('a','b')

)
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/Kaplan-Meier LVI-1.png)<!-- -->

**Univariate Cox-Regression**


```r
explanatoryUni <- "LVI"
dependentUni <- "Surv(OverallTime, Outcome)"
tUni <- mydata %>% finalfit(dependentUni, explanatoryUni)

knitr::kable(tUni, row.names = FALSE, align = c("l", "l", "r", "r", "r", "r"))
```



Dependent: Surv(OverallTime, Outcome)                      all            HR (univariable)          HR (multivariable)
--------------------------------------  --------  ------------  --------------------------  --------------------------
LVI                                     Absent     148 (100.0)                           -                           -
                                        Present    102 (100.0)   1.45 (1.06-1.98, p=0.021)   1.45 (1.06-1.98, p=0.021)

**Univariate Cox-Regression Summary**


```r
tUni_df <- tibble::as_tibble(tUni, .name_repair = "minimal") %>% janitor::clean_names(dat = ., 
    case = "snake")


n_level <- dim(tUni_df)[1]

tUni_df_descr <- function(n) {
    paste0("When ", tUni_df$dependent_surv_overall_time_outcome[1], " is ", tUni_df$x[n + 
        1], ", there is ", tUni_df$hr_univariable[n + 1], " times risk than ", "when ", 
        tUni_df$dependent_surv_overall_time_outcome[1], " is ", tUni_df$x[1], ".")
    
}



results5 <- purrr::map(.x = c(2:n_level - 1), .f = tUni_df_descr)

print(unlist(results5))
```

```
[1] "When LVI is Present, there is 1.45 (1.06-1.98, p=0.021) times risk than when LVI is Absent."
```

\pagebreak

**Median Survival**


```r
km_fit <- survfit(Surv(OverallTime, Outcome) ~ LVI, data = mydata)
km_fit
```

```
Call: survfit(formula = Surv(OverallTime, Outcome) ~ LVI, data = mydata)

   3 observations deleted due to missingness 
              n events median 0.95LCL 0.95UCL
LVI=Absent  146    100   19.7    14.1    27.1
LVI=Present 101     69   10.5     9.3    12.8
```

```r
km_fit_median_df <- summary(km_fit)
km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>% janitor::clean_names(dat = ., 
    case = "snake") %>% tibble::rownames_to_column(.data = ., var = "LVI")



km_fit_median_definition <- km_fit_median_df %>% dplyr::mutate(description = glue::glue("When, LVI, {LVI}, median survival is {median} [{x0_95lcl} - {x0_95ucl}, 95% CI] months.")) %>% 
    dplyr::mutate(description = gsub(pattern = "thefactor=", replacement = " is ", 
        x = description)) %>% dplyr::select(description) %>% dplyr::pull()

km_fit_median_definition
```

```
When, LVI, LVI=Absent, median survival is 19.7 [14.1 - 27.1, 95% CI] months.
When, LVI, LVI=Present, median survival is 10.5 [9.3 - 12.8, 95% CI] months.
```

**1-3-5-yr survival**


```r
summary(km_fit, times = c(12, 36, 60))
```

```
Call: survfit(formula = Surv(OverallTime, Outcome) ~ LVI, data = mydata)

3 observations deleted due to missingness 
                LVI=Absent 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
   12     71      52    0.612  0.0426        0.534        0.701
   36     19      38    0.235  0.0424        0.165        0.334

                LVI=Present 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
   12     27      52    0.391  0.0544       0.2978        0.514
   36      5      15    0.120  0.0436       0.0593        0.245
```

```r
km_fit_summary <- summary(km_fit, times = c(12, 36, 60))

km_fit_df <- as.data.frame(km_fit_summary[c("strata", "time", "n.risk", "n.event", 
    "surv", "std.err", "lower", "upper")])

km_fit_df
```

```
       strata time n.risk n.event      surv    std.err      lower     upper
1  LVI=Absent   12     71      52 0.6115055 0.04257648 0.53350053 0.7009157
2  LVI=Absent   36     19      38 0.2347686 0.04235846 0.16483932 0.3343638
3 LVI=Present   12     27      52 0.3911397 0.05442593 0.29777603 0.5137763
4 LVI=Present   36      5      15 0.1204428 0.04355881 0.05928425 0.2446936
```

```r
km_fit_definition <- km_fit_df %>% dplyr::mutate(description = glue::glue("When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI].")) %>% 
    dplyr::select(description) %>% dplyr::pull()

km_fit_definition
```

```
When LVI=Absent, 12 month survival is 61% [53%-70.1%, 95% CI].
When LVI=Absent, 36 month survival is 23% [16%-33.4%, 95% CI].
When LVI=Present, 12 month survival is 39% [30%-51.4%, 95% CI].
When LVI=Present, 36 month survival is 12% [6%-24.5%, 95% CI].
```

\pagebreak


```r
summary(km_fit)$table
```

```
            records n.max n.start events   *rmean *se(rmean) median 0.95LCL
LVI=Absent      146   146     146    100 24.32960   1.653307   19.7    14.1
LVI=Present     101   101     101     69 18.16069   1.840202   10.5     9.3
            0.95UCL
LVI=Absent     27.1
LVI=Present    12.8
```

```r
km_fit_median_df <- summary(km_fit)
results1html <- as.data.frame(km_fit_median_df$table) %>% janitor::clean_names(dat = ., 
    case = "snake") %>% tibble::rownames_to_column(.data = ., var = "LVI")

results1html[, 1] <- gsub(pattern = "thefactor=", replacement = "", x = results1html[, 
    1])

knitr::kable(results1html, row.names = FALSE, align = c("l", rep("r", 9)), format = "html", 
    digits = 1)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> LVI </th>
   <th style="text-align:right;"> records </th>
   <th style="text-align:right;"> n_max </th>
   <th style="text-align:right;"> n_start </th>
   <th style="text-align:right;"> events </th>
   <th style="text-align:right;"> rmean </th>
   <th style="text-align:right;"> se_rmean </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> x0_95lcl </th>
   <th style="text-align:right;"> x0_95ucl </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> LVI=Absent </td>
   <td style="text-align:right;"> 146 </td>
   <td style="text-align:right;"> 146 </td>
   <td style="text-align:right;"> 146 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 24.3 </td>
   <td style="text-align:right;"> 1.7 </td>
   <td style="text-align:right;"> 19.7 </td>
   <td style="text-align:right;"> 14.1 </td>
   <td style="text-align:right;"> 27.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LVI=Present </td>
   <td style="text-align:right;"> 101 </td>
   <td style="text-align:right;"> 101 </td>
   <td style="text-align:right;"> 101 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 18.2 </td>
   <td style="text-align:right;"> 1.8 </td>
   <td style="text-align:right;"> 10.5 </td>
   <td style="text-align:right;"> 9.3 </td>
   <td style="text-align:right;"> 12.8 </td>
  </tr>
</tbody>
</table>

\pagebreak

**Pairwise Comparisons**



\pagebreak












### Pairwise comparison


```r
dependentKM <- "Surv(OverallTime, Outcome)"
explanatoryKM <- "TStage"

mydata %>%
  finalfit::surv_plot(.data = .,
                      dependent = dependentKM,
                      explanatory = explanatoryKM,
                      xlab='Time (months)',
                      pval=TRUE,
                      legend = 'none',
                      break.time.by = 12,
                      xlim = c(0,60)
                      # legend.labs = c('a','b')
                      )
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/Kaplan-Meier Plot Log-Rank Test TStage-1.png)<!-- -->








```r
km_fit
```

```
Call: survfit(formula = Surv(OverallTime, Outcome) ~ LVI, data = mydata)

   3 observations deleted due to missingness 
              n events median 0.95LCL 0.95UCL
LVI=Absent  146    100   19.7    14.1    27.1
LVI=Present 101     69   10.5     9.3    12.8
```

```r
print(km_fit, 
      scale=1,
      digits = max(options()$digits - 4,3),
      print.rmean=getOption("survfit.print.rmean"),
      rmean = getOption('survfit.rmean'),
      print.median=getOption("survfit.print.median"),
      median = getOption('survfit.median')
      
      )
```

```
Call: survfit(formula = Surv(OverallTime, Outcome) ~ LVI, data = mydata)

   3 observations deleted due to missingness 
              n events median 0.95LCL 0.95UCL
LVI=Absent  146    100   19.7    14.1    27.1
LVI=Present 101     69   10.5     9.3    12.8
```




### Multivariate Analysis Survival






<!-- # parsnip -->









---

<!-- # Interactive Survival Analysis -->

<!-- ```{r shiny survival, child = here::here('childRmd', '_19shinySurvival.Rmd')} -->
<!-- ``` -->


<!-- --- -->

<!-- \elandscape -->


<!-- # Correlation -->


<!-- ```{r correlation, child = here::here('childRmd', '_20correlation.Rmd')} -->
<!-- ``` -->


<!-- # Models -->


<!-- ```{r models, child = here::here('childRmd', '_21models.Rmd')} -->
<!-- ``` -->


<!-- --- -->

<!-- \pagebreak -->



<!-- ```{r comments, child = here::here('childRmd', '_22comments.Rmd')} -->
<!-- ``` -->

---

\pagebreak


# Discussion

- Interpret the results in context of the working hypothesis elaborated in the introduction and other relevant studies; include a discussion of limitations of the study.

- Discuss potential clinical applications and implications for future research

\pagebreak


# Footer


**Codes for explaining the software and the packages that are used in the analysis**^[See [`childRmd/_23footer.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_23footer.Rmd) file for other codes]


### Save Final Data  {.appendix}  


```r
projectName <- list.files(path = here::here(), pattern = "Rproj")
projectName <- gsub(pattern = ".Rproj", replacement = "", x = projectName)

analysisDate <- as.character(Sys.Date())

imageName <- paste0(projectName, analysisDate, ".RData")

save.image(file = here::here("data", imageName))

rdsName <- paste0(projectName, analysisDate, ".rds")

readr::write_rds(x = mydata, path = here::here("data", rdsName))

saveRDS(object = mydata, file = here::here("data", rdsName))

excelName <- paste0(projectName, analysisDate, ".xlsx")

rio::export(x = mydata, file = here::here("data", excelName), format = "xlsx")

# writexl::write_xlsx(mydata, here::here('data', excelName))

print(glue::glue("saved data after analysis to ", rownames(file.info(here::here("data", 
    excelName))), " : ", as.character(file.info(here::here("data", excelName))$ctime)))
```

```
saved data after analysis to /Users/serdarbalciold/histopathRprojects/histopathology-template/data/histopathology-template2020-02-18.xlsx : 2020-02-18 18:47:50
```



```r
mydata %>% downloadthis::download_this(output_name = excelName, output_extension = ".csv", 
    button_label = "Download data as csv", button_type = "default")
```

<!--html_preserve--><a href="data:text/csv;base64,SUQsTmFtZSxTZXgsQWdlLFJhY2UsUHJlaW52YXNpdmVDb21wb25lbnQsTFZJLFBOSSxMYXN0Rm9sbG93VXBEYXRlLERlYXRoLEdyb3VwLEdyYWRlLFRTdGFnZSxBbnRpWF9pbnRlbnNpdHksQW50aVlfaW50ZW5zaXR5LEx5bXBoTm9kZU1ldGFzdGFzaXMsVmFsaWQsU21va2VyLEdyYWRlX0xldmVsLFN1cmdlcnlEYXRlLERlYXRoVGltZSxpbnQsT3ZlcmFsbFRpbWUsT3V0Y29tZQowMDEsQ2hhbmNlbGxlcixGZW1hbGUsNTQsV2hpdGUsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA1LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMiwxLFByZXNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTgtMDctMDNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDctMDMgVVRDLS0yMDE5LTA1LTE4IFVUQywxMC41LDEKMDAyLFNlcGgsRmVtYWxlLDM5LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDUtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwzLDEsQWJzZW50LFRSVUUsRkFMU0UsbG93LDIwMTgtMDgtMTRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDgtMTQgVVRDLS0yMDE5LTA1LTE4IFVUQyw5LjEsMQowMDMsTWFreW5zaWUsRmVtYWxlLDMyLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA4LTE4VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsMiwzLDEsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOS0wMS0xNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0xNiBVVEMtLTIwMTktMDgtMTggVVRDLDcuMSwwCjAwNCxEZXZuLEZlbWFsZSw1OCxIaXNwYW5pYyxQcmVzZW50LEFic2VudCxQcmVzZW50LDIwMjAtMDEtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMSwzLDEsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE5LTA1LTE4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA1LTE4IFVUQy0tMjAyMC0wMS0xOCBVVEMsOCwxCjAwNSxUYWtpYXJhLE1hbGUsMzgsV2hpdGUsUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDctMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDEsMiwzLFByZXNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTktMDEtMjVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMjUgVVRDLS0yMDE5LTA3LTE4IFVUQyw1LjgsMQowMDYsVGlsZGEsTWFsZSw1MCxXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOS0xOFQwMDowMDowMFosRkFMU0UsQ29udHJvbCwxLDQsMiwzLEFic2VudCxGQUxTRSxUUlVFLG1vZGVyYXRlLDIwMTktMDEtMTFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMTEgVVRDLS0yMDE5LTA5LTE4IFVUQyw4LjIsMAowMDcsQWxheWpoYSxGZW1hbGUsMzYsSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDUtMThUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwyLDQsMSwyLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTgtMDgtMjVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDgtMjUgVVRDLS0yMDE5LTA1LTE4IFVUQyw4LjgsMAowMDgsS2V5bW9uaWUsTWFsZSw3MCxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMi0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDMsMyxQcmVzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE5LTAzLTEwVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTEwIFVUQy0tMjAxOS0xMi0xOCBVVEMsOS4zLDEKMDA5LEtpdHNpYSxNYWxlLDQzLEhpc3BhbmljLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA2LTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDIsMixQcmVzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxOC0wOS0xN1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wOS0xNyBVVEMtLTIwMTktMDYtMTggVVRDLDksMQowMTAsUmFzaGlkYXQsTWFsZSw0NSxCbGFjayxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDMtMThUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwzLDEsMiwyLEFic2VudCxGQUxTRSxGQUxTRSxsb3csMjAxOC0wNS0wMlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNS0wMiBVVEMtLTIwMTktMDMtMTggVVRDLDEwLjUsMAowMTEsS2VudGFybyxGZW1hbGUsNjUsSGlzcGFuaWMsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDktMThUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwxLDEsMywyLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxOS0wNi0wN1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNi0wNyBVVEMtLTIwMTktMDktMTggVVRDLDMuNCwwCjAxMixNYXJxdWV0LE1hbGUsMjcsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMTEtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsMywyLDEsUHJlc2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTktMDItMjhUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDItMjggVVRDLS0yMDE5LTExLTE4IFVUQyw4LjcsMQowMTMsTHlkeWEsTWFsZSw0NCxXaGl0ZSxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDktMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsMiwzLDEsUHJlc2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTktMDUtMjRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDUtMjQgVVRDLS0yMDE5LTA5LTE4IFVUQywzLjgsMQowMTQsTGFyZXNzYSxNYWxlLDY0LEhpc3BhbmljLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTEwLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMSwyLEFic2VudCxGQUxTRSxUUlVFLG1vZGVyYXRlLDIwMTktMDQtMjlUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDQtMjkgVVRDLS0yMDE5LTEwLTE4IFVUQyw1LjYsMQowMTUsRW5hamFoLEZlbWFsZSw0NCxCbGFjayxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEyLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywyLDMsMixBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTktMDQtMTRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDQtMTQgVVRDLS0yMDE5LTEyLTE4IFVUQyw4LjEsMQowMTYsS29leSxGZW1hbGUsNDgsQmktUmFjaWFsLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wNi0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDMsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE4LTA5LTIyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA5LTIyIFVUQy0tMjAxOS0wNi0xOCBVVEMsOC45LDEKMDE3LE1hcnlsYW4sRmVtYWxlLDU4LEFzaWFuLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDUtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsNCwzLDIsQWJzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxOC0wOS0xNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wOS0xNiBVVEMtLTIwMTktMDUtMTggVVRDLDguMSwxCjAxOCxMYXVyZW5tYXJpZSxGZW1hbGUsNzEsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDgtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsNCwzLDIsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOC0xMS0yMlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMS0yMiBVVEMtLTIwMTktMDgtMTggVVRDLDguOSwxCjAxOSxDbGVhcnksTWFsZSw0NixBc2lhbixBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0xMi0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsNCwyLDEsUHJlc2VudCxGQUxTRSxUUlVFLGxvdywyMDE5LTA0LTI4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA0LTI4IFVUQy0tMjAxOS0xMi0xOCBVVEMsNy43LDAKMDIwLFphdmlhLE1hbGUsNjQsQmxhY2ssUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA5LTE4VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMyw0LDMsMixQcmVzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxOS0wNC0xM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNC0xMyBVVEMtLTIwMTktMDktMTggVVRDLDUuMiwwCjAyMSxTaGVkcmljayxNYWxlLDQ4LFdoaXRlLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMS0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwzLDEsUHJlc2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTktMDEtMDRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMDQgVVRDLS0yMDE5LTExLTE4IFVUQywxMC41LDEKMDIyLEp1bGF5bmUsRmVtYWxlLDM1LEhpc3BhbmljLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTExLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMiwxLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE5LTA3LTE3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA3LTE3IFVUQy0tMjAxOS0xMS0xOCBVVEMsNCwxCjAyMyxUcmV2ZWlvbixGZW1hbGUsMjYsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDYtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDIsMiwyLFByZXNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE4LTExLTEzVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTExLTEzIFVUQy0tMjAxOS0wNi0xOCBVVEMsNy4yLDEKMDI0LEVteWxlZSxNYWxlLDM0LEhpc3BhbmljLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDEtMThUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMiwzLDMsMSxQcmVzZW50LFRSVUUsRkFMU0UsbG93LDIwMTktMDgtMTFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDgtMTEgVVRDLS0yMDIwLTAxLTE4IFVUQyw1LjIsMAowMjUsSmFpcyxGZW1hbGUsNjQsV2hpdGUsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTEwLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDEsMiwzLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOS0wNi0xMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNi0xMSBVVEMtLTIwMTktMTAtMTggVVRDLDQuMiwxCjAyNixBbHZpZSxGZW1hbGUsNjQsQmxhY2ssUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTAtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDIsMywyLFByZXNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOS0wNy0wOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNy0wOCBVVEMtLTIwMTktMTAtMTggVVRDLDMuMywxCjAyNyxUcmFtb25kLEZlbWFsZSw3MixCbGFjayxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAxLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDIsMSwzLEFic2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOS0wOS0xN1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wOS0xNyBVVEMtLTIwMjAtMDEtMTggVVRDLDQsMQowMjgsU2hha2lzaGEsTWFsZSw0NSxIaXNwYW5pYyxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA2LTE4VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMiw0LDIsMSxBYnNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOS0wMi0xOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0xOCBVVEMtLTIwMTktMDYtMTggVVRDLDQsMAowMjksVGVuZWthLE1hbGUsMzQsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTItMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwzLDEsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE5LTAxLTE4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTE4IFVUQy0tMjAxOS0xMi0xOCBVVEMsMTEsMQowMzAsTmF0YXlhLE1hbGUsNjIsSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMjAtMDItMThUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwyLDIsMiwzLEFic2VudCxUUlVFLEZBTFNFLGxvdywyMDE5LTAzLTI4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTI4IFVUQy0tMjAyMC0wMi0xOCBVVEMsMTAuNywwCjAzMSxOYXN0YXNpYSxNYWxlLDQzLEFzaWFuLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTItMThUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMyw0LDMsMixQcmVzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOS0wNS0xMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNS0xMCBVVEMtLTIwMTktMTItMTggVVRDLDcuMywwCjAzMixKYXlkZW5hbGV4YW5kZXIsTWFsZSw1OCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA3LTE4VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDEsMiwzLDIsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOC0wOC0wNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wOC0wNSBVVEMtLTIwMTktMDctMTggVVRDLDExLjQsMAowMzMsS2FybG9uLE1hbGUsNTUsSGlzcGFuaWMsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOC0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywyLDIsMixQcmVzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE5LTAyLTA1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAyLTA1IFVUQy0tMjAxOS0wOC0xOCBVVEMsNi40LDEKMDM0LEFzdWNlbmEsTWFsZSw0OSxIaXNwYW5pYyxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMC0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMywyLDIsQWJzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE5LTAxLTI3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTI3IFVUQy0tMjAxOS0xMC0xOCBVVEMsOC43LDEKMDM1LEp1ZGdlLE1hbGUsNjEsSGlzcGFuaWMsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA1LTE4VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLE5BLDMsMywxLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsTkEsTW9yZVRoYW4xWWVhcixOQS0tTkEsTkEsMAowMzYsSmVuZXNpYSxNYWxlLDQ4LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDgtMThUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwyLDQsMywxLEFic2VudCxGQUxTRSxUUlVFLG1vZGVyYXRlLDIwMTktMDEtMjJUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMjIgVVRDLS0yMDE5LTA4LTE4IFVUQyw2LjksMAowMzcsVGhhaXNzYSxGZW1hbGUsNjgsV2hpdGUsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDIwLTAyLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDIsMyxQcmVzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxOS0wOC0yMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wOC0yMCBVVEMtLTIwMjAtMDItMTggVVRDLDUuOSwxCjAzOCxMYWluZSxNYWxlLDUwLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA4LTE4VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMyw0LDMsMyxQcmVzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxOC0xMC0wNFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMC0wNCBVVEMtLTIwMTktMDgtMTggVVRDLDEwLjUsMAowMzksS2Vuemx5LEZlbWFsZSwzNCxIaXNwYW5pYyxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTAzLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDIsMSxBYnNlbnQsRkFMU0UsRkFMU0UsbW9kZXJhdGUsMjAxOC0wNi0yNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNi0yNSBVVEMtLTIwMTktMDMtMTggVVRDLDguOCwxCjA0MCxUZWlhcmEsTWFsZSw2MixXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNi0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwzLDMsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE4LTEwLTEzVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEwLTEzIFVUQy0tMjAxOS0wNi0xOCBVVEMsOC4yLDEKMDQxLFByaW5jeSxGZW1hbGUsNTAsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMS0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsNCwyLDEsQWJzZW50LFRSVUUsRkFMU0UsbG93LDIwMTgtMTItMDNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTItMDMgVVRDLS0yMDE5LTExLTE4IFVUQywxMS41LDAKMDQyLE1hbmlqYWgsTWFsZSw1MSxCbGFjayxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA3LTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywzLDMsMSxBYnNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTgtMTItMThUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTItMTggVVRDLS0yMDE5LTA3LTE4IFVUQyw3LDEKMDQzLE1haGt5bGEsRmVtYWxlLDU2LEJsYWNrLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA5LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMywxLEFic2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOS0wMy0wNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMy0wNSBVVEMtLTIwMTktMDktMTggVVRDLDYuNCwxCjA0NCxLZXNoaWEsRmVtYWxlLDQ4LEhpc3BhbmljLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTAzLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwyLDMsMSxBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE4LTA1LTI5VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA1LTI5IFVUQy0tMjAxOS0wMy0xOCBVVEMsOS42LDEKMDQ1LEVuaXNhLEZlbWFsZSwzMSxXaGl0ZSxQcmVzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDctMThUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMiwzLDIsMixQcmVzZW50LFRSVUUsVFJVRSxsb3csMjAxOS0wMS0zMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0zMSBVVEMtLTIwMTktMDctMTggVVRDLDUuNiwwCjA0NixKb25uYXRhbixGZW1hbGUsMzMsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMjAtMDEtMThUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMiwzLDIsMixBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE5LTAzLTIyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTIyIFVUQy0tMjAyMC0wMS0xOCBVVEMsOS45LDAKMDQ3LEFmb25zbyxGZW1hbGUsNDQsV2hpdGUsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA1LTE4VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSwzLDEsMSxBYnNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTgtMDktMTFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDktMTEgVVRDLS0yMDE5LTA1LTE4IFVUQyw4LjIsMAowNDgsRGFuaXMsTWFsZSw0NCxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMi0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDEsNCwyLDEsTkEsVFJVRSxUUlVFLGhpZ2gsMjAxOS0wNS0xNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNS0xNiBVVEMtLTIwMjAtMDItMTggVVRDLDkuMSwxCjA0OSxHZWlkaSxNYWxlLDM1LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTAzLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMiwyLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE4LTEyLTA4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEyLTA4IFVUQy0tMjAxOS0wMy0xOCBVVEMsMy4zLDEKMDUwLEJyZWNvbixGZW1hbGUsNzMsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDktMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDIsMywzLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTgtMDktMjlUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDktMjkgVVRDLS0yMDE5LTA5LTE4IFVUQywxMS42LDEKMDUxLEthd2FuZGEsRmVtYWxlLDI3LEhpc3BhbmljLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wNi0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsMywyLDIsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTgtMTEtMTBUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMTAgVVRDLS0yMDE5LTA2LTE4IFVUQyw3LjMsMAowNTIsQW5hc3RhY2lhLE1hbGUsNDIsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDctMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsNCwzLDIsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOC0wOC0yM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wOC0yMyBVVEMtLTIwMTktMDctMTggVVRDLDEwLjgsMQowNTMsVm9uY2lsZSxGZW1hbGUsMzMsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDMtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMywzLDEsUHJlc2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMDctMzFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDctMzEgVVRDLS0yMDE5LTAzLTE4IFVUQyw3LjYsMQowNTQsTWFsYWxhLE1hbGUsNDAsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNS0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDIsMSxQcmVzZW50LFRSVUUsVFJVRSxsb3csMjAxOC0wNi0wNFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNi0wNCBVVEMtLTIwMTktMDUtMTggVVRDLDExLjUsMQowNTUsS2lya2xhbmQsRmVtYWxlLDQ4LEhpc3BhbmljLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA1LTE4VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSw0LDMsMixQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE5LTAxLTE4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTE4IFVUQy0tMjAxOS0wNS0xOCBVVEMsNCwwCjA1NixMb3JhbGksTWFsZSw1MSxIaXNwYW5pYyxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA3LTE4VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDIsMywzLDMsUHJlc2VudCxGQUxTRSxGQUxTRSxsb3csMjAxOC0xMS0xNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMS0xNSBVVEMtLTIwMTktMDctMTggVVRDLDguMSwwCjA1NyxBYnJlYSxNYWxlLDMxLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDMtMThUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMiw0LDIsMSxBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTgtMDUtMDZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDUtMDYgVVRDLS0yMDE5LTAzLTE4IFVUQywxMC40LDAKMDU4LFplcmVrLEZlbWFsZSw2MSxCbGFjayxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA4LTE4VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMywxLDIsMSxBYnNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOS0wMi0xM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0xMyBVVEMtLTIwMTktMDgtMTggVVRDLDYuMiwwCjA1OSxBbWVsbGEsRmVtYWxlLDQ4LEhpc3BhbmljLFByZXNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDUtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDQsMiwyLFByZXNlbnQsRkFMU0UsRkFMU0UsbW9kZXJhdGUsMjAxOC0wNy0zMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNy0zMSBVVEMtLTIwMTktMDUtMTggVVRDLDkuNiwxCjA2MCxTYWtpbmEsRmVtYWxlLDczLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDctMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwxLDMsQWJzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTktMDEtMDFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMDEgVVRDLS0yMDE5LTA3LTE4IFVUQyw2LjUsMQowNjEsRGVicm9oYSxGZW1hbGUsNDEsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMi0xOFQwMDowMDowMFosRkFMU0UsQ29udHJvbCwxLDQsMiwxLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE5LTAyLTI1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAyLTI1IFVUQy0tMjAyMC0wMi0xOCBVVEMsMTEuOCwwCjA2MixEYWVsYW5pLE1hbGUsMzYsSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA4LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDIsMiwxLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE5LTA0LTE2VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA0LTE2IFVUQy0tMjAxOS0wOC0xOCBVVEMsNC4xLDEKMDYzLFphbGV0aCxNYWxlLDUwLE5hdGl2ZSxQcmVzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTEtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDQsMywzLFByZXNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wNC0zMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNC0zMCBVVEMtLTIwMTktMTEtMTggVVRDLDYuNiwxCjA2NCxNZWxhc2lhLE1hbGUsNzIsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDMtMThUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwyLDIsMiwyLEFic2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOC0wOS0yM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wOS0yMyBVVEMtLTIwMTktMDMtMTggVVRDLDUuOCwwCjA2NSxTaGVhcm9uLE1hbGUsMzksSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDQtMThUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwyLDEsMywyLFByZXNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTgtMDctMDhUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDctMDggVVRDLS0yMDE5LTA0LTE4IFVUQyw5LjMsMAowNjYsRWxpY2ssTWFsZSw2OCxPdGhlcixBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTAzLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMiwyLFByZXNlbnQsRkFMU0UsRkFMU0UsbW9kZXJhdGUsMjAxOC0xMC0wNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMC0wNSBVVEMtLTIwMTktMDMtMTggVVRDLDUuNCwxCjA2NyxLZWFzb24sRmVtYWxlLDQ0LFdoaXRlLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wMy0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDEsNCwzLDIsUHJlc2VudCxGQUxTRSxGQUxTRSxsb3csMjAxOC0wOC0xNFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wOC0xNCBVVEMtLTIwMTktMDMtMTggVVRDLDcuMSwxCjA2OCxLYXlsYW5pZSxGZW1hbGUsNTIsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNS0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDIsNCwyLDMsQWJzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxOC0wNy0zMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNy0zMCBVVEMtLTIwMTktMDUtMTggVVRDLDkuNiwxCjA2OSxDZWxpZGEsRmVtYWxlLDMyLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDIwLTAxLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDMsMywyLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxOS0wOS0yMlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wOS0yMiBVVEMtLTIwMjAtMDEtMTggVVRDLDMuOSwxCjA3MCxKYWhrZWVtLE1hbGUsMzcsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMC0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMiwzLDIsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE5LTA1LTIxVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA1LTIxIFVUQy0tMjAxOS0xMC0xOCBVVEMsNC45LDEKMDcxLERhbmF5amFoLE1hbGUsNDksSGlzcGFuaWMsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMi0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwzLDMsMSxQcmVzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE5LTExLTA5VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTExLTA5IFVUQy0tMjAyMC0wMi0xOCBVVEMsMy4zLDEKMDcyLERhcnRpc2hhLE1hbGUsNDEsSGlzcGFuaWMsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA3LTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDIsMixBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE4LTEwLTE5VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEwLTE5IFVUQy0tMjAxOS0wNy0xOCBVVEMsOSwxCjA3MyxBbGlzYSxNYWxlLDMzLFdoaXRlLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wNi0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSw0LDEsMixBYnNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTgtMTItMjVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTItMjUgVVRDLS0yMDE5LTA2LTE4IFVUQyw1LjgsMQowNzQsU29uamUsRmVtYWxlLDQyLEhpc3BhbmljLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA1LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDEsMywyLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOC0wOC0wOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wOC0wOCBVVEMtLTIwMTktMDUtMTggVVRDLDkuMywxCjA3NSxNYWRhbGVubyxNYWxlLDUzLE90aGVyLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTExLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDMsMSxQcmVzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxOS0wNi0xMlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNi0xMiBVVEMtLTIwMTktMTEtMTggVVRDLDUuMiwxCjA3NixNaWNoYWUsRmVtYWxlLDYxLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA5LTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDIsMSxBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wMS0wOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0wOSBVVEMtLTIwMTktMDktMTggVVRDLDguMywxCjA3NyxOQSxGZW1hbGUsNjYsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNS0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwyLDMsMSxBYnNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOC0wNy0wOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNy0wOCBVVEMtLTIwMTktMDUtMTggVVRDLDEwLjMsMQowNzgsU2NoeWxlcixGZW1hbGUsNTQsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA3LTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSwyLDMsMyxBYnNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOC0wOC0yOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wOC0yOSBVVEMtLTIwMTktMDctMTggVVRDLDEwLjYsMQowNzksS2FybGl5YWgsRmVtYWxlLDU2LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTExLTE4VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMywyLDEsMixBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE5LTA0LTE2VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA0LTE2IFVUQy0tMjAxOS0xMS0xOCBVVEMsNy4xLDAKMDgwLEplbmFsaXosRmVtYWxlLDM5LEhpc3BhbmljLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDItMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMywyLFByZXNlbnQsRkFMU0UsRkFMU0UsbW9kZXJhdGUsMjAxOS0wOC0wNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wOC0wNSBVVEMtLTIwMjAtMDItMTggVVRDLDYuNCwxCjA4MSxBeWF0aSxNYWxlLDQyLEhpc3BhbmljLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTEwLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMiwxLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOS0wMS0xNFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0xNCBVVEMtLTIwMTktMTAtMTggVVRDLDkuMSwxCjA4MixUcmlzYSxNYWxlLDMxLEJpLVJhY2lhbCxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA3LTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDMsMixBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTgtMTAtMTJUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMTIgVVRDLS0yMDE5LTA3LTE4IFVUQyw5LjIsMQowODMsTGFndWFuLE1hbGUsNTAsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMi0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDIsMixBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTktMTAtMDdUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMTAtMDcgVVRDLS0yMDIwLTAyLTE4IFVUQyw0LjQsMQowODQsQ2hla2VzaGEsRmVtYWxlLDI4LFdoaXRlLFByZXNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA4LTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDIsMSxQcmVzZW50LEZBTFNFLFRSVUUsbG93LDIwMTgtMTItMDlUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTItMDkgVVRDLS0yMDE5LTA4LTE4IFVUQyw4LjMsMQowODUsUmFhdmksTWFsZSw1MSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAxLTE4VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMyw0LDIsMSxBYnNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOS0xMC0wMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0xMC0wMSBVVEMtLTIwMjAtMDEtMTggVVRDLDMuNSwwCjA4NixDYWl0cmlvbmEsRmVtYWxlLDM3LEJpLVJhY2lhbCxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wMy0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsNCwzLDIsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTgtMTEtMjVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMjUgVVRDLS0yMDE5LTAzLTE4IFVUQywzLjgsMAowODcsS2VvbnRhLEZlbWFsZSxOQSxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMS0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMywzLDIsUHJlc2VudCxUUlVFLFRSVUUsbG93LDIwMTktMDUtMDNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDUtMDMgVVRDLS0yMDIwLTAxLTE4IFVUQyw4LjUsMQowODgsU2hpdm9ubmksTWFsZSw1MixXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMS0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDEsNCwzLDIsQWJzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE4LTEyLTI4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEyLTI4IFVUQy0tMjAxOS0xMS0xOCBVVEMsMTAuNywxCjA4OSxTcnVzaHRpLEZlbWFsZSw0MCxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNC0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDMsMSxBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTgtMDktMjBUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDktMjAgVVRDLS0yMDE5LTA0LTE4IFVUQyw2LjksMQowOTAsTWNjbGVsbGFuLE1hbGUsMjUsV2hpdGUsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA4LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDMsMSwyLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTgtMTItMTNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTItMTMgVVRDLS0yMDE5LTA4LTE4IFVUQyw4LjIsMQowOTEsTWFyaWxpbixGZW1hbGUsMjksV2hpdGUsUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDEtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwyLDMsQWJzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTktMDktMThUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDktMTggVVRDLS0yMDIwLTAxLTE4IFVUQyw0LDEKMDkyLFNhbGluZSxGZW1hbGUsNDgsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMC0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDIsMyxBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE5LTA2LTE4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA2LTE4IFVUQy0tMjAxOS0xMC0xOCBVVEMsNCwxCjA5MyxOaWNsZSxNYWxlLDUxLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA4LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMiwyLFByZXNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOC0xMi0wMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMi0wMSBVVEMtLTIwMTktMDgtMTggVVRDLDguNSwxCjA5NCxEZXNhcmF5ZSxNYWxlLDY5LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDMtMThUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwzLDMsMiwyLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE4LTA5LTI4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA5LTI4IFVUQy0tMjAxOS0wMy0xOCBVVEMsNS42LDAKMDk1LFJheWdpbmUsRmVtYWxlLDI2LFdoaXRlLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNS0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSwyLDIsMSxQcmVzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMTAtMDZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMDYgVVRDLS0yMDE5LTA1LTE4IFVUQyw3LjQsMQowOTYsTWFyZ3VyZXRlLE1hbGUsMjksV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMi0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDEsNCwyLDEsQWJzZW50LFRSVUUsRkFMU0UsbG93LDIwMTktMDctMDRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDctMDQgVVRDLS0yMDE5LTEyLTE4IFVUQyw1LjUsMQowOTcsU2hlbHZhLE1hbGUsNzAsQXNpYW4sUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDUtMThUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwyLDQsMiwxLEFic2VudCxGQUxTRSxGQUxTRSxsb3csMjAxOC0xMC0wMlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMC0wMiBVVEMtLTIwMTktMDUtMTggVVRDLDcuNSwwCjA5OCxLYWh0YWksTWFsZSw3MyxCbGFjayxQcmVzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTAtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDQsMiwyLEFic2VudCxUUlVFLFRSVUUsbG93LDIwMTktMDUtMjFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDUtMjEgVVRDLS0yMDE5LTEwLTE4IFVUQyw0LjksMQowOTksS2VtYXJpdXMsRmVtYWxlLDQ0LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDMtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDQsMywyLFByZXNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTgtMDktMTBUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDktMTAgVVRDLS0yMDE5LTAzLTE4IFVUQyw2LjMsMQoxMDAsQXVicmlpLE1hbGUsNTQsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNy0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwyLDEsMixQcmVzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTktMDEtMDhUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMDggVVRDLS0yMDE5LTA3LTE4IFVUQyw2LjMsMQoxMDEsUmV5b25uYSxGZW1hbGUsNTMsQmxhY2ssQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA0LTE4VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDEsNCwzLDMsUHJlc2VudCxUUlVFLEZBTFNFLGxvdywyMDE5LTAxLTE2VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTE2IFVUQy0tMjAxOS0wNC0xOCBVVEMsMy4xLDAKMTAyLEphbWV5YWgsTWFsZSw0MCxXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNS0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDEsMiwyLDIsQWJzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMTAtMDhUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMDggVVRDLS0yMDE5LTA1LTE4IFVUQyw3LjMsMQoxMDMsTmFyc2lzLEZlbWFsZSw0NSxBc2lhbixBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wMy0xOFQwMDowMDowMFosRkFMU0UsQ29udHJvbCwyLDEsMywxLFByZXNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTgtMDgtMDRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDgtMDQgVVRDLS0yMDE5LTAzLTE4IFVUQyw3LjUsMAoxMDQsTWlsYW5pZSxGZW1hbGUsMjksSGlzcGFuaWMsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMS0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDIsNCwzLDIsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxOS0wMi0wNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0wNiBVVEMtLTIwMTktMTEtMTggVVRDLDkuNCwwCjEwNSxFZG9saWEsTWFsZSwzMyxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0xMi0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwyLDIsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE5LTA3LTExVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA3LTExIFVUQy0tMjAxOS0xMi0xOCBVVEMsNS4yLDEKMTA2LFNhaHIsRmVtYWxlLDI5LEJsYWNrLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMi0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwyLDEsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOS0wNy0xNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNy0xNiBVVEMtLTIwMjAtMDItMTggVVRDLDcuMSwxCjEwNyxEb2xlbmEsRmVtYWxlLDY2LFdoaXRlLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wNC0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDIsMiwxLDEsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOC0wOS0zMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wOS0zMCBVVEMtLTIwMTktMDQtMTggVVRDLDYuNiwwCjEwOCxBYWxleWFoLEZlbWFsZSw0NyxCbGFjayxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wMy0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywxLDMsMSxBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTgtMDgtMjRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDgtMjQgVVRDLS0yMDE5LTAzLTE4IFVUQyw2LjgsMQoxMDksRGVtZXJpdXMsRmVtYWxlLDQ1LFdoaXRlLFByZXNlbnQsQWJzZW50LFByZXNlbnQsMjAyMC0wMS0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMywzLDEsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE5LTA2LTI3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA2LTI3IFVUQy0tMjAyMC0wMS0xOCBVVEMsNi43LDEKMTEwLFRhc2hhaSxGZW1hbGUsNzIsQmxhY2ssQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDgtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDEsMywzLEFic2VudCxUUlVFLFRSVUUsbG93LDIwMTktMDUtMDJUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDUtMDIgVVRDLS0yMDE5LTA4LTE4IFVUQywzLjUsMQoxMTEsRG9yZXRoaWEsRmVtYWxlLDcwLEhpc3BhbmljLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTEyLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMywyLFByZXNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOS0wNi0xNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNi0xNSBVVEMtLTIwMTktMTItMTggVVRDLDYuMSwxCjExMixKZW1pbGxhLEZlbWFsZSwyNixBc2lhbixBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMC0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMiwyLDEsQWJzZW50LEZBTFNFLFRSVUUsbG93LDIwMTktMDItMjBUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDItMjAgVVRDLS0yMDE5LTEwLTE4IFVUQyw3LjksMQoxMTMsS2F5bWluLEZlbWFsZSwyNixCbGFjayxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMi0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwyLDIsUHJlc2VudCxUUlVFLFRSVUUsbG93LDIwMTktMDEtMjZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMjYgVVRDLS0yMDE5LTEyLTE4IFVUQywxMC43LDEKMTE0LFRhdmlzLEZlbWFsZSw0MixCbGFjayxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMjAtMDEtMThUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMyw0LDIsMSxQcmVzZW50LEZBTFNFLFRSVUUsbG93LDIwMTktMDItMjVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDItMjUgVVRDLS0yMDIwLTAxLTE4IFVUQywxMC44LDAKMTE1LEhhc3NldCxGZW1hbGUsMjgsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMS0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMiwzLDMsQWJzZW50LEZBTFNFLFRSVUUsbG93LDIwMTktMDYtMDhUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDYtMDggVVRDLS0yMDE5LTExLTE4IFVUQyw1LjMsMQoxMTYsQWx0aGVyYSxNYWxlLDMwLFdoaXRlLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wOC0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDIsMSwyLDMsUHJlc2VudCxGQUxTRSxGQUxTRSxsb3csMjAxOS0wNC0yOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNC0yOSBVVEMtLTIwMTktMDgtMTggVVRDLDMuNiwwCjExNyxMYXJhbXksTWFsZSw1MSxXaGl0ZSxBYnNlbnQsQWJzZW50LE5BLDIwMTktMDQtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDMsMiwzLEFic2VudCxGQUxTRSxGQUxTRSxsb3csMjAxOC0xMS0yN1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMS0yNyBVVEMtLTIwMTktMDQtMTggVVRDLDQuNywxCjExOCxCb2RlZSxNYWxlLDMxLEhpc3BhbmljLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA5LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDMsMywyLEFic2VudCxGQUxTRSxGQUxTRSxsb3csMjAxOS0wMy0wMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMy0wMSBVVEMtLTIwMTktMDktMTggVVRDLDYuNiwxCjExOSxIYXJweXIsTWFsZSwyOSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEwLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywyLDIsMyxQcmVzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOS0wMS0zMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0zMCBVVEMtLTIwMTktMTAtMTggVVRDLDguNiwxCjEyMCxKYW55cmlhLEZlbWFsZSwzMCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTExLTE4VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSwyLDMsMixBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE5LTA1LTAxVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA1LTAxIFVUQy0tMjAxOS0xMS0xOCBVVEMsNi42LDAKMTIxLEJlcm5hc2lhLEZlbWFsZSw1OCxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wOS0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsNCwxLDIsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOC0xMC0xOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMC0xOCBVVEMtLTIwMTktMDktMTggVVRDLDExLDAKMTIyLENoYWRyb24sTWFsZSw0MSxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNC0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDIsNCwzLDIsUHJlc2VudCxUUlVFLFRSVUUsTkEsMjAxOS0wMS0xN1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0xNyBVVEMtLTIwMTktMDQtMTggVVRDLDMsMQoxMjMsQXVyaWNrLE1hbGUsMzUsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOS0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDEsMyxQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE4LTExLTIwVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTExLTIwIFVUQy0tMjAxOS0wOS0xOCBVVEMsOS45LDEKMTI0LEthdGhlcmFuLEZlbWFsZSw1OCxCaS1SYWNpYWwsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDQtMThUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMSwzLDIsMyxQcmVzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE4LTEyLTI1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEyLTI1IFVUQy0tMjAxOS0wNC0xOCBVVEMsMy44LDAKMTI1LE1hcnJlbGwsTWFsZSw1NyxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA0LTE4VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSw0LDIsMixBYnNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOC0wOS0xM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wOS0xMyBVVEMtLTIwMTktMDQtMTggVVRDLDcuMiwwCjEyNixEZWFuZ2VsaW8sTWFsZSw0NixXaGl0ZSxQcmVzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDMtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDIsMywyLFByZXNlbnQsVFJVRSxUUlVFLGxvdywyMDE4LTA1LTI1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA1LTI1IFVUQy0tMjAxOS0wMy0xOCBVVEMsOS44LDEKMTI3LERlbG9yb3MsTWFsZSwzNixIaXNwYW5pYyxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTExLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDMsMiwxLFByZXNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE5LTA3LTA0VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA3LTA0IFVUQy0tMjAxOS0xMS0xOCBVVEMsNC41LDEKMTI4LE1pbmhjaGF1LE1hbGUsNDgsSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDUtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDMsMiwyLEFic2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMDctMTdUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDctMTcgVVRDLS0yMDE5LTA1LTE4IFVUQywxMCwxCjEyOSxPbGljaWEsTWFsZSw3MCxCaS1SYWNpYWwsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDUtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDIsMiwyLFByZXNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE4LTEwLTExVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEwLTExIFVUQy0tMjAxOS0wNS0xOCBVVEMsNy4yLDEKMTMwLFZpa3JhbSxNYWxlLDU5LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDktMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDQsMywxLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE5LTAxLTMwVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTMwIFVUQy0tMjAxOS0wOS0xOCBVVEMsNy42LDEKMTMxLE9sdXdhdG9iaWxvYmEsRmVtYWxlLDczLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDUtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMywyLEFic2VudCxGQUxTRSxGQUxTRSxsb3csMjAxOC0wNi0wMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNi0wMSBVVEMtLTIwMTktMDUtMTggVVRDLDExLjUsMQoxMzIsSXNhaWFzLE1hbGUsNTcsSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDUtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwyLDEsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTgtMTEtMDlUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMDkgVVRDLS0yMDE5LTA1LTE4IFVUQyw2LjMsMQoxMzMsVmlja3llLE1hbGUsNDgsQmxhY2ssQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDctMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsNCwzLDEsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTktMDItMTdUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDItMTcgVVRDLS0yMDE5LTA3LTE4IFVUQyw1LDEKMTM0LFNhYW15YSxNYWxlLDM5LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDUtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwyLDEsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE5LTAxLTI5VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTI5IFVUQy0tMjAxOS0wNS0xOCBVVEMsMy42LDEKMTM1LEVzc3luY2UsRmVtYWxlLDMyLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTExLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDEsMSwxLEFic2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOC0xMS0yOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMS0yOSBVVEMtLTIwMTktMTEtMTggVVRDLDExLjYsMQoxMzYsV2FsZGVuLEZlbWFsZSwzNixXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wOS0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsNCwyLDIsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxOS0wNS0yOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNS0yOSBVVEMtLTIwMTktMDktMTggVVRDLDMuNiwwCjEzNyxLYXRhLE5BLDM2LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTExLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSwyLDIsMSxQcmVzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxOS0wMi0wOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0wOSBVVEMtLTIwMTktMTEtMTggVVRDLDkuMywxCjEzOCxTaGFudGFuYWUsTWFsZSwzNSxIaXNwYW5pYyxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNS0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsNCwyLDIsUHJlc2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOC0xMC0xNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMC0xNiBVVEMtLTIwMTktMDUtMTggVVRDLDcuMSwwCjEzOSxKYXZpYWlyLE1hbGUsMjUsSGlzcGFuaWMsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA1LTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDMsMyxQcmVzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTgtMTAtMDdUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMDcgVVRDLS0yMDE5LTA1LTE4IFVUQyw3LjQsMQoxNDAsSmF5bmFsaXMsTWFsZSw0MixBc2lhbixQcmVzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDctMThUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMSwzLDMsMSxBYnNlbnQsVFJVRSxUUlVFLGxvdywyMDE5LTAyLTA3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAyLTA3IFVUQy0tMjAxOS0wNy0xOCBVVEMsNS40LDAKMTQxLFJveml5YSxNYWxlLDI4LEJsYWNrLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAyMC0wMS0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSw0LDIsMixBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTktMDktMThUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDktMTggVVRDLS0yMDIwLTAxLTE4IFVUQyw0LDEKMTQyLEphbWF1bCxGZW1hbGUsNDUsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wMy0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSw0LDMsMyxQcmVzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxOC0wNi0xMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNi0xMCBVVEMtLTIwMTktMDMtMTggVVRDLDkuMywxCjE0MyxEb255ZXR0YSxGZW1hbGUsNDUsQmxhY2ssUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTEtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMiwzLDIsQWJzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTktMDUtMzBUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDUtMzAgVVRDLS0yMDE5LTExLTE4IFVUQyw1LjYsMQoxNDQsQXNhYWQsTWFsZSw3MixIaXNwYW5pYyxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMi0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwzLDIsMSxBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTktMDMtMDVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDMtMDUgVVRDLS0yMDE5LTEyLTE4IFVUQyw5LjQsMQoxNDUsTWFsaXlhbmksRmVtYWxlLDM4LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDIwLTAxLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDIsMiwxLFByZXNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTktMDItMjNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDItMjMgVVRDLS0yMDIwLTAxLTE4IFVUQywxMC44LDEKMTQ2LEdlbmVzZSxNYWxlLDYwLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA0LTE4VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDEsMiwyLDIsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE4LTA1LTI3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA1LTI3IFVUQy0tMjAxOS0wNC0xOCBVVEMsMTAuNywwCjE0NyxLYWVuYSxNYWxlLDYzLEJsYWNrLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA5LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDIsMSwyLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxOS0wMi0xN1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0xNyBVVEMtLTIwMTktMDktMTggVVRDLDcsMQoxNDgsTmlheWEsRmVtYWxlLDMzLEhpc3BhbmljLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA5LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDEsMywyLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOC0xMS0wMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMS0wMSBVVEMtLTIwMTktMDktMTggVVRDLDEwLjYsMQoxNDksRG9uaXZlZSxGZW1hbGUsMzAsV2hpdGUsUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDItMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsMywzLDMsUHJlc2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTktMTAtMjhUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMTAtMjggVVRDLS0yMDIwLTAyLTE4IFVUQywzLjcsMQoxNTAsQ2xhcmlzdGluZSxNYWxlLDY0LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTEtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDIsMywzLFByZXNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wMS0wMlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0wMiBVVEMtLTIwMTktMTEtMTggVVRDLDEwLjUsMQoxNTEsTW9udGVzaGEsTWFsZSwyOSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAxLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDMsMywyLEFic2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTctMTAtMjhUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0xMC0yOCBVVEMtLTIwMjAtMDEtMTggVVRDLDI2LjcsMQoxNTIsU29uaXRhLEZlbWFsZSw1NSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA1LTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwyLDMsMSxBYnNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxNy0wMy0zMVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTAzLTMxIFVUQy0tMjAxOS0wNS0xOCBVVEMsMjUuNiwxCjE1MyxNaWxhZHksTWFsZSw2NCxXaGl0ZSxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDgtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsMywyLDIsQWJzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxNy0wNS0zMVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA1LTMxIFVUQy0tMjAxOS0wOC0xOCBVVEMsMjYuNiwxCjE1NCxFbGxlYXNlLEZlbWFsZSw2NyxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTExLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDEsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE3LTA3LTI1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDctMjUgVVRDLS0yMDE5LTExLTE4IFVUQywyNy44LDEKMTU1LE5hemhpcixNYWxlLDQwLEhpc3BhbmljLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDItMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDMsMywzLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNy0wNS0yNlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA1LTI2IFVUQy0tMjAyMC0wMi0xOCBVVEMsMzIuNywxCjE1NixKYXF1aSxNYWxlLDMyLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA2LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMywyLFByZXNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE3LTAzLTMxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDMtMzEgVVRDLS0yMDE5LTA2LTE4IFVUQywyNi42LDEKMTU3LEJsb2R3eW4sTWFsZSw2NixXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA1LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMiwzLFByZXNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE2LTA3LTEyVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMDctMTIgVVRDLS0yMDE5LTA1LTE4IFVUQywzNC4yLDEKMTU4LERhaXN5LE1hbGUsNjUsQmxhY2ssQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA2LTE4VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMiw0LDIsMSxBYnNlbnQsVFJVRSxUUlVFLGxvdywyMDE3LTEwLTE3VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTAtMTcgVVRDLS0yMDE5LTA2LTE4IFVUQywyMCwwCjE1OSxMYXphcnJpYSxGZW1hbGUsNDQsV2hpdGUsTkEsQWJzZW50LEFic2VudCwyMDE5LTExLTE4VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDIsMywzLDEsUHJlc2VudCxGQUxTRSxUUlVFLGxvdywyMDE3LTEyLTIwVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTItMjAgVVRDLS0yMDE5LTExLTE4IFVUQywyMi45LDAKMTYwLENoYW1wYWluZSxGZW1hbGUsMzYsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOS0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwyLDMsMixBYnNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxNy0wMi0xNlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTAyLTE2IFVUQy0tMjAxOS0wOS0xOCBVVEMsMzEuMSwxCjE2MSxEZXN1YW4sRmVtYWxlLDU0LFdoaXRlLFByZXNlbnQsQWJzZW50LFByZXNlbnQsMjAyMC0wMS0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwyLDEsQWJzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMDUtMjZUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wNS0yNiBVVEMtLTIwMjAtMDEtMTggVVRDLDE5LjcsMQoxNjIsTGFuZG9uLEZlbWFsZSw2NCxIaXNwYW5pYyxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0xMi0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsMywyLDIsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTgtMDItMjdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wMi0yNyBVVEMtLTIwMTktMTItMTggVVRDLDIxLjcsMAoxNjMsS3lyZWEsTWFsZSw0MCxXaGl0ZSxQcmVzZW50LEFic2VudCxQcmVzZW50LDIwMjAtMDItMThUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMSwzLDMsMixBYnNlbnQsVFJVRSxUUlVFLGxvdywyMDE3LTExLTExVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTEtMTEgVVRDLS0yMDIwLTAyLTE4IFVUQywyNy4yLDAKMTY0LERhbm5pZWx5bm4sTWFsZSw2MSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA1LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMywxLEFic2VudCxGQUxTRSxGQUxTRSxsb3csMjAxNy0wNi0yMFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA2LTIwIFVUQy0tMjAxOS0wNS0xOCBVVEMsMjIuOSwxCjE2NSxBcHVydmEsRmVtYWxlLDUzLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDUtMThUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMiw0LDIsMixBYnNlbnQsTkEsVFJVRSxsb3csMjAxNy0wNC0yMVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA0LTIxIFVUQy0tMjAxOS0wNS0xOCBVVEMsMjQuOSwwCjE2NixXeW9tYSxNYWxlLDczLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA4LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDEsMiwyLFByZXNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTgtMDEtMDdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wMS0wNyBVVEMtLTIwMTktMDgtMTggVVRDLDE5LjQsMQoxNjcsSmF4aSxGZW1hbGUsNjgsSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTEyLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDIsMixBYnNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOC0xMS0yM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTExLTIzIFVUQy0tMjAxOS0xMi0xOCBVVEMsMTIuOCwxCjE2OCxCcmFkZXJpY2ssRmVtYWxlLDQzLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDYtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDMsMiwyLFByZXNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE3LTAzLTA1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDMtMDUgVVRDLS0yMDE5LTA2LTE4IFVUQywyNy40LDEKMTY5LEphY2thbHluLEZlbWFsZSw1MCxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wMy0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDIsMywzLDEsUHJlc2VudCxGQUxTRSxGQUxTRSxsb3csMjAxNi0wOS0yMFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE2LTA5LTIwIFVUQy0tMjAxOS0wMy0xOCBVVEMsMjkuOSwwCjE3MCxKYWR5ZWwsRmVtYWxlLDU1LEhpc3BhbmljLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTEwLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDMsMixQcmVzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE4LTA5LTIwVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDktMjAgVVRDLS0yMDE5LTEwLTE4IFVUQywxMi45LDEKMTcxLFRpbWlrbyxGZW1hbGUsNzIsQmxhY2ssQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDQtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDQsMiwxLFByZXNlbnQsRkFMU0UsVFJVRSxsb3csMjAxNy0wNy0yMlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA3LTIyIFVUQy0tMjAxOS0wNC0xOCBVVEMsMjAuOSwxCjE3MixKZW5uaWZyLEZlbWFsZSwzNixXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNi0xOFQwMDowMDowMFosRkFMU0UsQ29udHJvbCwyLDQsMywyLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE2LTA4LTA2VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMDgtMDYgVVRDLS0yMDE5LTA2LTE4IFVUQywzNC40LDAKMTczLEFoaXIsRmVtYWxlLDQwLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTEtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsNCwxLDEsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE3LTA1LTIyVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDUtMjIgVVRDLS0yMDE5LTExLTE4IFVUQywyOS45LDEKMTc0LEJyZW9uaWEsTWFsZSw1NixXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNy0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsMywyLDEsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTYtMDgtMDJUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wOC0wMiBVVEMtLTIwMTktMDctMTggVVRDLDM1LjUsMAoxNzUsRGljaWUsTWFsZSwzOCxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNS0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsNCwyLDIsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE3LTA5LTAxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDktMDEgVVRDLS0yMDE5LTA1LTE4IFVUQywyMC41LDAKMTc2LEJyb25pcyxNYWxlLDYyLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDktMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMiwzLDEsUHJlc2VudCxUUlVFLFRSVUUsbG93LDIwMTctMTItMDJUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0xMi0wMiBVVEMtLTIwMTktMDktMTggVVRDLDIxLjUsMQoxNzcsUGVtYmVybGV5LE1hbGUsMzMsSGlzcGFuaWMsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDktMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMSwzLDEsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE4LTA2LTE3VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDYtMTcgVVRDLS0yMDE5LTA5LTE4IFVUQywxNSwxCjE3OCxBbWl5cmEsRmVtYWxlLDI4LE5BLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDUtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMywzLFByZXNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE3LTAzLTEzVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDMtMTMgVVRDLS0yMDE5LTA1LTE4IFVUQywyNi4yLDEKMTc5LE1la2VhLE1hbGUsNjIsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTEtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwyLDEsUHJlc2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNy0wNi0yNlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA2LTI2IFVUQy0tMjAxOS0xMS0xOCBVVEMsMjguNywxCjE4MCxKZWRpYWVsLEZlbWFsZSw1OCxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0xMi0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSwzLDMsMyxBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE4LTA3LTA4VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDctMDggVVRDLS0yMDE5LTEyLTE4IFVUQywxNy4zLDEKMTgxLEx5bm5lYW5uLEZlbWFsZSwzOCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTExLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwxLDMsMyxQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE4LTA1LTMxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDUtMzEgVVRDLS0yMDE5LTExLTE4IFVUQywxNy42LDEKMTgyLEp1bGVubnksTWFsZSw2MSxIaXNwYW5pYyxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNi0xOFQwMDowMDowMFosRkFMU0UsQ29udHJvbCwyLDMsMywxLFByZXNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE3LTEyLTIzVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTItMjMgVVRDLS0yMDE5LTA2LTE4IFVUQywxNy44LDAKMTgzLEdyZW5kYSxNYWxlLDMxLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDktMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDMsMywzLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxOC0wOS0wM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTA5LTAzIFVUQy0tMjAxOS0wOS0xOCBVVEMsMTIuNSwxCjE4NCxBdXN0cmViZXJ0byxNYWxlLDU3LFdoaXRlLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wMy0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsMywzLDEsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE4LTAyLTE2VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDItMTYgVVRDLS0yMDE5LTAzLTE4IFVUQywxMy4xLDAKMTg1LE1hcmlzYWwsTWFsZSw0NSxIaXNwYW5pYyxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAyMC0wMS0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwzLDIsQWJzZW50LFRSVUUsRkFMU0UsbG93LDIwMTctMDItMjJUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wMi0yMiBVVEMtLTIwMjAtMDEtMTggVVRDLDM0LjksMQoxODYsRmVubmVsbCxGZW1hbGUsNTMsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNS0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMywyLDIsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxNy0wMS0xOFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTAxLTE4IFVUQy0tMjAxOS0wNS0xOCBVVEMsMjgsMQoxODcsWnlhbmFoLE1hbGUsNjgsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMjAtMDEtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMywzLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE3LTAyLTE1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDItMTUgVVRDLS0yMDIwLTAxLTE4IFVUQywzNS4xLDEKMTg4LFJiLE1hbGUsNDcsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMS0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDEsNCwzLDMsQWJzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMDItMDlUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wMi0wOSBVVEMtLTIwMjAtMDEtMTggVVRDLDIzLjMsMQoxODksSmFodmVsLE1hbGUsNDksV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMC0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywzLDIsMixQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE4LTA5LTA5VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDktMDkgVVRDLS0yMDE5LTEwLTE4IFVUQywxMy4zLDEKMTkwLEthZWxpYW5hLEZlbWFsZSw0NCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEyLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDMsMyxBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTctMDYtMTlUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNi0xOSBVVEMtLTIwMTktMTItMTggVVRDLDMwLDEKMTkxLFNoZW5nLEZlbWFsZSw0NyxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wMy0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSwyLDEsMixBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE3LTA4LTA2VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDgtMDYgVVRDLS0yMDE5LTAzLTE4IFVUQywxOS40LDEKMTkyLEJyaXNoYXVuLEZlbWFsZSw1MSxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMS0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsNCwyLDMsQWJzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE4LTA4LTAxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDgtMDEgVVRDLS0yMDIwLTAxLTE4IFVUQywxNy41LDAKMTkzLEFya2l0YSxGZW1hbGUsNTQsQXNpYW4sQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDQtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDMsMywxLEFic2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxNy0wMy0zMVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTAzLTMxIFVUQy0tMjAxOS0wNC0xOCBVVEMsMjQuNiwxCjE5NCxKYXp6YWJlbGxlLEZlbWFsZSw3MCxXaGl0ZSxQcmVzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA5LTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDIsMSxQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE3LTA0LTA4VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDQtMDggVVRDLS0yMDE5LTA5LTE4IFVUQywyOS4zLDEKMTk1LEthdHluYSxGZW1hbGUsNjYsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMC0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMywyLDIsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOC0wOC0xNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTA4LTE1IFVUQy0tMjAxOS0xMC0xOCBVVEMsMTQuMSwxCjE5NixEYXNob25hLEZlbWFsZSw0NCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEyLTE4VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDIsMywzLDIsUHJlc2VudCxGQUxTRSxUUlVFLGxvdywyMDE3LTA2LTAxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDYtMDEgVVRDLS0yMDE5LTEyLTE4IFVUQywzMC41LDAKMTk3LENhbGxhbixNYWxlLDcxLEJsYWNrLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wOC0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMywxLDMsUHJlc2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxNy0wOC0xOVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA4LTE5IFVUQy0tMjAxOS0wOC0xOCBVVEMsMjQsMQoxOTgsQW5uYXZpY3RvcmlhLEZlbWFsZSwzMixXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEyLTE4VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMiwzLDMsMixQcmVzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxNy0wNy0xMVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA3LTExIFVUQy0tMjAxOS0xMi0xOCBVVEMsMjkuMiwwCjE5OSxGaWxpY2l0eSxNYWxlLDYyLEJsYWNrLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTEtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDIsMiwzLFByZXNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTgtMDYtMTRUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wNi0xNCBVVEMtLTIwMTktMTEtMTggVVRDLDE3LjEsMQoyMDAsSmFjYXJsYSxGZW1hbGUsMjksV2hpdGUsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA2LTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDMsMSxBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE3LTA0LTI1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDQtMjUgVVRDLS0yMDE5LTA2LTE4IFVUQywyNS44LDEKMjAxLFNhZ2EsRmVtYWxlLDQ5LEJsYWNrLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDQtMThUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwyLDQsMywxLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNy0xMi0wN1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTEyLTA3IFVUQy0tMjAxOS0wNC0xOCBVVEMsMTYuNCwwCjIwMixLaXVuYSxNYWxlLDMzLEhpc3BhbmljLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA2LTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDMsMSxQcmVzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTYtMDctMThUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wNy0xOCBVVEMtLTIwMTktMDYtMTggVVRDLDM1LDEKMjAzLENoYXVuaWNlLE1hbGUsNjgsQmxhY2ssUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDEtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDIsMywyLEFic2VudCxGQUxTRSxUUlVFLG1vZGVyYXRlLDIwMTctMDUtMDdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNS0wNyBVVEMtLTIwMjAtMDEtMTggVVRDLDMyLjQsMQoyMDQsSGF0c3VtaSxGZW1hbGUsNjEsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDIwLTAyLTE4VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDIsMiwzLDEsQWJzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTctMDYtMTNUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNi0xMyBVVEMtLTIwMjAtMDItMTggVVRDLDMyLjIsMAoyMDUsSm9kYW5uYSxGZW1hbGUsMjUsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMjAtMDEtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMSwyLDMsUHJlc2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNy0xMi0yNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTEyLTI1IFVUQy0tMjAyMC0wMS0xOCBVVEMsMjQuOCwxCjIwNixMaXJpYSxNYWxlLDcwLEJsYWNrLFByZXNlbnQsQWJzZW50LEFic2VudCxOQSxGQUxTRSxDb250cm9sLDIsMiwyLDMsQWJzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTctMDctMDJUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNy0wMiBVVEMtLU5BLE5BLDAKMjA3LEFudGF2aWEsRmVtYWxlLDI4LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTExLTE4VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsMSwyLDIsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE3LTA4LTE1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDgtMTUgVVRDLS0yMDE5LTExLTE4IFVUQywyNy4xLDAKMjA4LEFrZXR6YWxpLE1hbGUsNDMsQmxhY2ssQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOS0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDIsMyxQcmVzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTctMTEtMTBUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0xMS0xMCBVVEMtLTIwMTktMDktMTggVVRDLDIyLjMsMQoyMDksSmFteXJhaCxNYWxlLDI1LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTAtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsMiwzLDEsUHJlc2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE3LTA4LTIwVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDgtMjAgVVRDLS0yMDE5LTEwLTE4IFVUQywyNS45LDEKMjEwLFdhbmRhbGVlLE1hbGUsNjUsSGlzcGFuaWMsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOC0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDIsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE3LTAxLTA0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDEtMDQgVVRDLS0yMDE5LTA4LTE4IFVUQywzMS41LDEKMjExLEFsaW5zb24sRmVtYWxlLDU5LFdoaXRlLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wOC0xOFQwMDowMDowMFosRkFMU0UsQ29udHJvbCwzLDIsMywyLFByZXNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTgtMDgtMTFUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wOC0xMSBVVEMtLTIwMTktMDgtMTggVVRDLDEyLjIsMAoyMTIsWGF2aXVzLEZlbWFsZSw1MyxIaXNwYW5pYyxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wOC0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDEsMyxBYnNlbnQsRkFMU0UsVFJVRSxsb3csMjAxNi0xMi0xNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE2LTEyLTE1IFVUQy0tMjAxOS0wOC0xOCBVVEMsMzIuMSwxCjIxMyxMZXRoZWEsRmVtYWxlLDMwLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDYtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsMSwzLDEsUHJlc2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxNy0xMS0xM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTExLTEzIFVUQy0tMjAxOS0wNi0xOCBVVEMsMTkuMiwxCjIxNCxSZWRpZXQsTWFsZSw2MyxCbGFjayxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA1LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDMsMywzLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOC0wMS0yOFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTAxLTI4IFVUQy0tMjAxOS0wNS0xOCBVVEMsMTUuNywxCjIxNSxHYWJyZWwsTWFsZSw1NSxXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMi0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwxLDIsQWJzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE3LTExLTE2VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTEtMTYgVVRDLS0yMDIwLTAyLTE4IFVUQywyNy4xLDEKMjE2LERlb250cmF5LE1hbGUsNzIsQXNpYW4sQWJzZW50LEFic2VudCxQcmVzZW50LDIwMjAtMDItMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsMSwyLDMsUHJlc2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxNy0wNS0zMVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA1LTMxIFVUQy0tMjAyMC0wMi0xOCBVVEMsMzIuNiwxCjIxNyxKYXNhaG4sRmVtYWxlLDMyLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA1LTE4VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsMyxOQSwyLFByZXNlbnQsVFJVRSxUUlVFLGxvdywyMDE4LTAzLTA4VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDMtMDggVVRDLS0yMDE5LTA1LTE4IFVUQywxNC4zLDAKMjE4LERhdmVvbixNYWxlLDU4LEhpc3BhbmljLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDEtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMywzLFByZXNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTctMDQtMTNUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNC0xMyBVVEMtLTIwMjAtMDEtMTggVVRDLDMzLjIsMQoyMTksQXJjZWxpLE1hbGUsNDEsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTEyLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwyLDMsMixQcmVzZW50LEZBTFNFLFRSVUUsbG93LDIwMTgtMTAtMzFUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0xMC0zMSBVVEMtLTIwMTktMTItMTggVVRDLDEzLjYsMQoyMjAsWWF4ZW5pLE1hbGUsNzIsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMC0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsMywyLDMsQWJzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE4LTA5LTIwVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDktMjAgVVRDLS0yMDE5LTEwLTE4IFVUQywxMi45LDAKMjIxLERhbnlhbCxGZW1hbGUsMzQsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA4LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDMsMywzLEFic2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxNy0wMi0xOFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTAyLTE4IFVUQy0tMjAxOS0wOC0xOCBVVEMsMzAsMQoyMjIsTWFrYWRhLE1hbGUsNjgsQmxhY2ssQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDYtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDMsMSwxLFByZXNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE2LTEwLTE2VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMTAtMTYgVVRDLS0yMDE5LTA2LTE4IFVUQywzMi4xLDEKMjIzLEphY2lhbmEsRmVtYWxlLDcwLFdoaXRlLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMS0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsMywyLDIsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxNy0wNS0wNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA1LTA1IFVUQy0tMjAyMC0wMS0xOCBVVEMsMzIuNCwwCjIyNCxXbGFkaW1pcixNYWxlLDY2LFdoaXRlLFByZXNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEwLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDMsMywzLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOC0wNC0xMlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTA0LTEyIFVUQy0tMjAxOS0xMC0xOCBVVEMsMTguMiwxCjIyNSxMaWFoLEZlbWFsZSw2OCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAxLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDMsMixBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE4LTAzLTAxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDMtMDEgVVRDLS0yMDIwLTAxLTE4IFVUQywyMi41LDEKMjI2LFJvZHJpa2EsRmVtYWxlLDcwLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDUtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDIsMywyLFByZXNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE1LTA1LTExVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTUtMDUtMTEgVVRDLS0yMDE5LTA1LTE4IFVUQyw0OC4yLDEKMjI3LFRlcmlhbm5hLEZlbWFsZSwzMSxXaGl0ZSxQcmVzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDQtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDIsMywzLFByZXNlbnQsRkFMU0UsVFJVRSxsb3csMjAxNC0xMS0wMlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE0LTExLTAyIFVUQy0tMjAxOS0wNC0xOCBVVEMsNTMuNSwxCjIyOCxUc2VsYW5lLE1hbGUsNTcsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOC0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDIsNCwzLDMsQWJzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxNS0wMS0wOFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTAxLTA4IFVUQy0tMjAxOS0wOC0xOCBVVEMsNTUuMywwCjIyOSxNb3Jqb3JpZSxNYWxlLDI1LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTEyLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwxLDIsMSxBYnNlbnQsVFJVRSxUUlVFLGxvdywyMDE2LTEwLTAxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMTAtMDEgVVRDLS0yMDE5LTEyLTE4IFVUQywzOC41LDEKMjMwLFdpbGhlbWluYSxNYWxlLDQ5LEFzaWFuLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDgtMThUMDA6MDA6MDBaLEZBTFNFLE5BLDMsNCwzLDIsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE1LTA1LTE2VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTUtMDUtMTYgVVRDLS0yMDE5LTA4LTE4IFVUQyw1MS4xLDAKMjMxLE5pbCxGZW1hbGUsMjUsSGlzcGFuaWMsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDQtMThUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwzLDIsMywxLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE0LTA5LTE3VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTQtMDktMTcgVVRDLS0yMDE5LTA0LTE4IFVUQyw1NSwwCjIzMixMYXRhamlhLE1hbGUsMzcsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOS0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsMywyLDEsUHJlc2VudCxUUlVFLE5BLG1vZGVyYXRlLDIwMTUtMDEtMDdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNS0wMS0wNyBVVEMtLTIwMTktMDktMTggVVRDLDU2LjQsMAoyMzMsTmVlaGFyaWthLE1hbGUsNDgsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMS0xOFQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMywxLDMsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxNS0xMC0xNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTEwLTE1IFVUQy0tMjAxOS0xMS0xOCBVVEMsNDkuMSwxCjIzNCxKbmlhaCxGZW1hbGUsNTAsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA1LTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywzLDMsMyxQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE0LTA5LTA0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTQtMDktMDQgVVRDLS0yMDE5LTA1LTE4IFVUQyw1Ni41LDEKMjM1LENoaWFudGksTWFsZSw1NyxXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOC0xOFQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwzLDMsMixQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE1LTEwLTAzVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTUtMTAtMDMgVVRDLS0yMDE5LTA4LTE4IFVUQyw0Ni41LDEKMjM2LEphbm5ldGUsRmVtYWxlLDI3LEhpc3BhbmljLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTAzLTE4VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMywyLDEsMixBYnNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTQtMTItMDFUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNC0xMi0wMSBVVEMtLTIwMTktMDMtMTggVVRDLDUxLjUsMAoyMzcsRGFtYXIsRmVtYWxlLDY2LEJsYWNrLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDYtMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDMsMiwyLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE1LTAyLTI1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTUtMDItMjUgVVRDLS0yMDE5LTA2LTE4IFVUQyw1MS44LDEKMjM4LEhhcnZlcixGZW1hbGUsNTAsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMS0xOFQwMDowMDowMFosRkFMU0UsQ29udHJvbCwxLDQsMiwzLFByZXNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTUtMDItMjdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNS0wMi0yNyBVVEMtLTIwMjAtMDEtMTggVVRDLDU4LjcsMAoyMzksU2FrZXlhLEZlbWFsZSwzNSxXaGl0ZSxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDMtMThUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMyw0LDMsMixBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxNS0wNi0yM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTA2LTIzIFVUQy0tMjAxOS0wMy0xOCBVVEMsNDQuOCwwCjI0MCxTZWxpbixNYWxlLDcwLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA5LTE4VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsNCwyLDEsUHJlc2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTYtMDItMDVUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wMi0wNSBVVEMtLTIwMTktMDktMTggVVRDLDQzLjQsMAoyNDEsTGFraXJhLE1hbGUsNjMsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMS0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDIsMiwzLDIsQWJzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxNi0xMS0yNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE2LTExLTI1IFVUQy0tMjAyMC0wMS0xOCBVVEMsMzcuOCwwCjI0MixPbGxlLE1hbGUsNTUsQXNpYW4sUHJlc2VudCxBYnNlbnQsUHJlc2VudCwyMDIwLTAyLTE4VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywxLDMsMyxBYnNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTUtMDYtMjJUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNS0wNi0yMiBVVEMtLTIwMjAtMDItMTggVVRDLDU1LjksMQoyNDMsQXNoZXJhLE1hbGUsNjQsV2hpdGUsUHJlc2VudCxBYnNlbnQsUHJlc2VudCwyMDIwLTAxLTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMSwzLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTYtMDItMDFUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wMi0wMSBVVEMtLTIwMjAtMDEtMTggVVRDLDQ3LjUsMQoyNDQsTGFpbGVlbixNYWxlLDMyLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDMtMThUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMywyLDIsQWJzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE1LTA1LTE0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTUtMDUtMTQgVVRDLS0yMDE5LTAzLTE4IFVUQyw0Ni4xLDEKMjQ1LFR1bGx5LEZlbWFsZSwzNSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA2LTE4VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDMsMywyLFByZXNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTUtMDYtMTBUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNS0wNi0xMCBVVEMtLTIwMTktMDYtMTggVVRDLDQ4LjMsMQoyNDYsRW1tYWJlbGxhLE1hbGUsNTcsSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTItMThUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMSw0LDIsMixBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE2LTA2LTIxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMDYtMjEgVVRDLS0yMDE5LTEyLTE4IFVUQyw0MS45LDAKMjQ3LE5heXZhZGl1cyxGZW1hbGUsNTgsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNC0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDIsMywzLDIsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE0LTExLTA5VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTQtMTEtMDkgVVRDLS0yMDE5LTA0LTE4IFVUQyw1My4zLDAKMjQ4LERheWFtaSxGZW1hbGUsNTcsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMi0xOFQwMDowMDowMFosTkEsVHJlYXRtZW50LDMsNCwxLDIsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxNi0wOC0wM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE2LTA4LTAzIFVUQy0tMjAxOS0xMi0xOCBVVEMsNDAuNSxOQQoyNDksRXJkbWFuLE1hbGUsNjYsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNi0xOFQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsNCwzLE5BLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNi0wMy0wN1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE2LTAzLTA3IFVUQy0tMjAxOS0wNi0xOCBVVEMsMzkuNCwwCjI1MCxEcmlzdGluLEZlbWFsZSw3MSxXaGl0ZSxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMTItMThUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMiwzLFByZXNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE2LTAzLTA2VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMDMtMDYgVVRDLS0yMDE5LTEyLTE4IFVUQyw0NS40LDEK" download="histopathology-template2020-02-18.xlsx.csv">
<button class="btn btn-default"><i class="fa fa-save"></i> Download data as csv</button>
</a><!--/html_preserve-->

```r
mydata %>% downloadthis::download_this(output_name = excelName, output_extension = ".xlsx", 
    button_label = "Download data as xlsx", button_type = "primary")
```

<!--html_preserve--><a href="data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,UEsDBBQAAAAIAAAAIQA4nYbYPgEAAAcEAAATAAAAW0NvbnRlbnRfVHlwZXNdLnhtbK2Ty27DIBBF9/0KxLYyJF1UVRUniz6WbRbpB1AYxygYEDNJk78vtpNIrdI8lG6MzNy55w6C0WTdOLaChDb4kg/FgDPwOhjr5yX/mL0WD5whKW+UCx5KvgHkk/HNaLaJgCw3eyx5TRQfpURdQ6NQhAg+V6qQGkX5N81lVHqh5iDvBoN7qYMn8FRQ68HHo2eo1NIRe1nn7T5IAoecPfXCllVyFaOzWlGuy5U3vyjFliByZ6fB2ka8zQIuDxLayt+Abd97PplkDbCpSvSmmqySJuhpChFl1ovjLgdihqqyGrLHssktAtpABkwRsyUksrDPfJStQ4LL4bszarvPJK6dRNo4wKtHxZhAGawBqHGiNz1BpnyfoP8Or+Z3NieAXyEtPkNY/Pew7SoaZf0Z/E6Msluun/pnkL3/Lofs3vH4G1BLAwQUAAAACAAAACEA8p9J2ukAAABLAgAACwAAAF9yZWxzLy5yZWxzrZLBTsMwDEDvfEXk+5puSAihpbsgpN0mND7AJG4btY2jxIPu74mQQAyNaQeOceznZ8vrzTyN6o1S9hwMLKsaFAXLzofOwMv+aXEPKgsGhyMHMnCkDJvmZv1MI0qpyb2PWRVIyAZ6kfigdbY9TZgrjhTKT8tpQinP1OmIdsCO9Kqu73T6yYDmhKm2zkDauiWo/THSNWxuW2/pke1hoiBnWvzKKGRMHYmBedTvnIZX5qEqUNDnXVbXu/w9p55I0KGgtpxoEVOpTuLLWr91HNtdCefPjEtCt/+5HJqFgiN3WQlj/DLSJzfQfABQSwMEFAAAAAgAAAAhAOVEG6PVAAAALAIAABoAAAB4bC9fcmVscy93b3JrYm9vay54bWwucmVsc62Rz4oCMQyH7z5FyX0nMwoiYsfLsuBV9AFKJ/MHZ9rSZHedt7couiuIePAUfgn58kFW6+PQqx+K3HmnochyUOSsrzrXaNjvvj4WoFiMq0zvHWkYiWFdTlZb6o2kHW67wCpBHGtoRcISkW1Lg+HMB3JpUvs4GEkxNhiMPZiGcJrnc4z/GVDeMdWm0hA3VQFqNwZ6he3rurP06e33QE4enMBfHw/cEkmCmtiQaLi1GM+lyBIV8LHM9J0yknbpT+QcL82nDrN3OrCMfXroTeKSr+fx7snlCVBLAwQUAAAACAAAACEAlSQ5VhlsAACuzQMAGAAAAHhsL3dvcmtzaGVldHMvc2hlZXQxLnhtbJ29XZfbOJKtff/+ilp1Py4RBL9mdfdZrrTLktOZFkVnd83cnMWy1ZVq50cdZdrV2b/+lSUwJSJibzowF+dUC9gIO8htgODDwF/+z79vb374ut4+bO7v/vpj9mL24w/ru4/3nzZ3v//1x6sPv/xX/eMPD4/93af+5v5u/dcfn9YPP/6fv/1/f/nzfvv54Xq9fvxhN8Ddw19/vH58/OO/f/rp4eP1+rZ/eHH/x/pu1/LP++1t/7j7n9vff3r4Y7vuP+1Ftzc/udms/Om239z9eBjhv7ffM8b9P/+5+bh+df/xy+367vEwyHZ90z/u/vgP15s/Hn78218+bXZt3/4+P2zX//zrjy+z//7VFdmPP/3tL/vYf9+s/3w4+e8fHvvfuvXN+uPj+tMuAz/+8O2v9tv9/edvjYvdT7Nv0p+E9pf9H2u5/eHT+p/9l5vH1f2f8/Xm9+vH3SDFN8nH+5uH/f/7w+1ml9vmxx9u+3/v//8/N58er//6o5u9qLJZk1fFtxw/3ezS63784eOXh8f7238cumRhoMMQbhbG+PYfxkF+Ovxx9n/0V/1j/7e/bO///GG7/xs/7P/fg+bw1/r2wy7wt/aXzx12v27ubjZ36+5xu2vd7IZ7/Nvi1V9+etwN/+1//fQxaH7mmsv+dq2ozriqW/9bEb3iope/a5Fec9Gq/6ipfuGq5Xa9ufvaP2y+rs/ub//Y2eXuURnlDR/l3d8Ximg+EfpSEy0mIvUPu5v45ub+z6s/dneE9jd+y0d4te4frxXZOZe92d5/+UORvZuS9Z+0P+QFl33oHnv1JricuHPuHje//t/N3eO3f0wen5QB3k8P8D90gOXEBXq6/eP68v7T+mL9uLtWuzvrQRmk5YP8vb/ZfFJkqwmz3d5/Xm8VXfcdF+n/vlt/Xd8o4g8TQb9sf19vn8DNePUdN+OHjfovy9+5dKMa9R9c9H43a/Y3NyDirxPiL48f72PhT7t/kZ//WXbP//46MMZslmn/9KLuZ9f93cf1zY16Vc+Q6pfdTHyj/QVfffsDfv1b4f/y09fTf1nRQP+43qhX9Rck2P1z+gD+BbVL5kjy8jegWLj9BTz8LX1ezvLxX/TtYcTf9u3ZuO0cRfuwWwE93uoB3yGRdpUvUGev/UN3+Eu48R/yvdP+6Et7btvTTMzGw61IW4dC7WYk7d+O8QVxdXTnXcE7b/N4vbnL/mfda3f+30NyqqLMyln0R/xHyNHsRTFu+DVO3si7+bN3c+hdp3kXde/Wf2jz7Bnqj12b7//ceRO5Fg0EXYsE0E9vzIq5WbHIJzybE8+iaNSzSJRrnkWdVc+GCxV5Nlc9a05USxKxyollUSRg2fH1yF0ZWRbedtyyhyy4vNn9KyAse2hsXkR/rV/j1I0c658d66FjtWv6M+p+0X9+2i371MccpMGu9YebIfoX/DUaCLoWCbBrkYJMteYgCz+2bRP9Y/vW4zvyHEU7u7973N5ri9B3SKKaFnXW/v2+9KppvWpac5pakoaVx4buUKTb3aPFVl9rfxhfEl9HY17BW487N6Si9nUjJ9tDYyWc66O/8ci5xbNzC+hc7R/Yn1H3V+uvd5prUX/s2uKwQq4j16KB5puHP/q7zUfNuEhDFslIgudbe5BFMbpNdpc1cm5BJlwUjk64SKR6F3XWVtSXherdQvWuObdtQbxLktShSNeb37UV4YeCr4Cu0HgTvj2kwWVZuZtwY98eGqNL/2ucuZFry2fXltC1heZa1P1D/3nTb3vNuEhyAWxbHm6E2LZoGDjZIgHxLJJgz5oVi3J8h5TRY9TbklgWRSOTLZJo8+cF6qwatlSfakvNxkv7tWhJHlYldnOHQiHHjq+Hb6LxruB9xx17yENWzPJczrSHxuJF7NmSebZ69mwFPVtqnkXdP2xuPqmORQLk2Orw14n+lq/RMNCxSEAciyTYsWbFohrdIVX8FPW2IstjFI04FknUfSjUWX2mrVTHVqpjzWlqSRpWFZliUSS2PB5fEl/FD7bw1uOmDQn6thElp9lDY/0ia0b/F1m4Yovl+tnCNbRwpVkYdX950z/961o1MZLg9XJ9uBGiXL5GA7H1MvzzwgddpCAPuuYgi3pif6omTkbR6HIZidTZF3VWvVxrXnpfaw5fmhPVkkSsSFuHIqHJd3xB8ryKfIzGm/BxSINraueFj+vg43p2+n/xQ2/NfNw8+7iBPq41H6Pu5+un2/s7fbsKadBs3Bwe4+PZGA0DZ2MkwB5GCuJhc5BFM37kzaJb5m1D1s8oGvUwEqkeRp1VDzfqI6/669Ke27YhJiZZ6lAoZOLxFSny+MUQvPO4iQ95cN4XmRMmPjQ2L3Jq4oatp7PZs4t3/4ls3Kh4Dep/vnl82KjTMZQgG+//eN8SGvkYDsTmYyjCZoYS4mZ7mMU3yemcHN89b8OYuqFhQLK+hhp1gQ17q44OFy1eYus/LxNS3I7SEbuaNXYwGvJ1dGl266XI2HDECWcP6cjLSnk4Dq3xwjr8jMx8wssh6GOWzVQzYwzt4XrzqdcuwxkUQTtnhzQWsZ3RQD/f9B8/q16GwBP2MmHmoJftmkWWjacC72IzZ2SFDSPS6Rmq1C1p2Fvd4goXTdhZ/XlpvzAty8eKNXYwGHgVHF0a51xsZjTglJlDMqrSK3vToVUhOEILWGFn7uhnCGBlKoAF+5/vLkK/vVftbOevskMqS2FoNBSdn8181Bu7ZA4lzNJuYvcrI4jROYzILW1CsUy9L8Nlixfd4WdhaXOS24yAaSuWrA4GgxN0xMi5+G0THHHK04ch67pp5NJ7yOALHzvaUUcfqawMYlmZimXB/hf99v99WesTNNYAPweqpYr9bGazoIKY2U5nQQkz8xgIqur44TljhBaMyM2MVLqZLUTX5XDNYjPrmFZCxlqWkFXGUC0Yje1rRxeocPGzNL4bJxwdMuUqlyuWzsOeWEn3tkVexwY/QlwZpLgyleKC/d89fXrSH6cx+AXsfaBYfMxLw4Gwve0UF5Sw9bdds8j81GRNOKVzGJH720JoXZh6X2Y60JXpRFdCxlqWkFVGiK8ORqP+jlC7WRP7OxHsGlKye8atJZM55FG8cBaZHDv6CHdlkO7KVLoL9n/X7y7Gg+5ppIGePjAuMULxGg5El+BmqOiNXTKHEubqMVRUFcLVDPOCEbmrTaAX7K1vkqlQ1/vws1iC22GvjNFeLFkdDEZNPb4+hfgyAo46ZeqQKF+XM8XUgW98UcampuhXdmS/Mgh/ZSr8Bfu/vuv/1atfSEAJebAuwUyNhsI7ZWY6641dMrdLFlk58RIrYxQYDMg2vZFG97OFGrvMVObrfaYCYkt7utqM8F4rlqoOBoOP1OMrU5Rigk7kwYZsZH63BJdeLsMCPH6DlVEkLDsyYRmEwjIVCoP9z+/X2reyZ1BAnHyAYWIq+TUc6ufNf636j5teu4V/gSriZjtRNk/QLLJq6iUWAaDOYUQ+QyOVvu42gWKZyoS9z1SAbGm/MG3GYDHW2MFg0NFV9BariB2dCIsN2cidV75czAZcTDxEV9TRR0Qsg4xYpjJisP9Fv3266dWPKqCGmPpAxojPKuBQLx82avBfsAIb2iyZ2yWLbIoTy2pm5yRSDKr0bTITKxYumbCzTovZE9ZmjBdjjR0MBu0cIWOFmKATmbEhG7sHaM3OAzUmJugYxBvb+UiKZRAVy1RUDPZ/13/Zru9u+62Oi0EdsXRAxrLY0mZmDCqIpROoMXuYRdZMfOSYMXAMBuSeNqFjsLfuaR0eCz8LT5sT1mYMH2PJ6mAw+hA9vj5efKuMb8YJXzd0mm7QNE3ZMXdkxxxkxzKVHYP9z252fwN16Q0laGPMBXYsRrnhQHCOxgpoaLtkDiVkze1mE8/QjsBQ5zAiLxGCVOpTNOytFwnR0TEXY08HQydkrGUJWTmC2XUwGioVMot2xOIX03DAqWIhIUkzl1VKtZBDa/UiuhV+DQ3gzbQ7smMOsmNOZcdg///tv+ocKFRALx84GbHJDQeC22FQwUr9JJBjUEOK/WQTL64cA8dgQG5mEzgGe+tmztTZ2engWEKSW5aQFWvsYDS05I4uTlHWsZsT0bGQjmz3TK4g3qG1eOFiN1NyzLmjmyE55vTSXah/d73+tN2orjqDImhop++KwYFw6a6U2l0pxbsSqne5CdLE0fpdsIga3uGGGt3OtgJeTrez0+fmlBpetIgXreJlpcaiS+PFFje+ESfsfBjSVb4qFDuHZCm1vOI0jv185MYc5Macyo3B/m+/3PRPd+rjM9SQSnyhVlRMgsKh2GtoKCILbjvXNLeHWbgpeMwxeAwG5HO0CR6DvXVT6/CY0+Exe8JaR/CwFWvsYDD2BB1dn7IUi+5EdmzIyKzKlM81QmsMg4o0jk19ZMUcZMWcyorB/rsb6et6c6/uc0MRcfUBjYlXoq/hUHiatgNjUMIsbS/85fzEeyvHeDEYkM3SpuJfsLf6Hnq4YrGh1Z+XCSluHQHCVqyxg9HgLB1VAIu3N67wfTjh5qEGWOUVUswNRcDEopuSYu5IijlIijmVFIP9X98+3az1OdoKirlQXEo8Q6eAYlBE7GwHxeyShZuqBuYIF3UOAzI3I426wQ17q2y30wuCOb0iGBybuZmQYCuWqw5GQ/thUVmwWjxBJxJiQzbyfaEDYeaBEBNmLugT9JEQc5AQcyohBvu/7dXi6WdQQCbmEmyImSuEQQV7frZr5lBDvFxOMJ+OEWIwIF9sm2qFwd7qZ1dOrxbm9HJhCUluWUJWrLGD0eDcHNVwi78ZusK34oSdA+2VZb5QVtoBjJR2ppCYO0JiDkJiToXEYP+XN1/1t89QQfxcAT9DSgxucCcUEMN/RTw120uIuWrKzgwQgwHZ1Iw0+kLbApNdOh0PczoelnBVWsf4MJarDkZDU3OE7sUHKlzBAae8HACwOlOKZA8ZfJHHVqZ0mDvSYQ7SYU6lw2D/D9v+9v5OO6LkDGqImQ84TBWXyoZDYTPb6TC7ZG6XLFw9tcxmdBi5DmxqNtFhsLfuZr2UmFOhsaU9Ya1jdBhLVgeDwZm5jt4jxrAnHHHKzSFJs1LDt0Or2AOjbJg7smEOsmFOZcNg/+66/7x50AsCQhF8aj4gMKJkCRyIPjXbyTC7ZG6XLFwztQdGQKhzGJDb2QSGwd76trZKgL13Mc8U7GwHw1g+Vo6BYTAYmpujymKZmJsTmbAhGbNcq6c93Pexmxv20JwfibAcEmFOJcJg/w/ru/Vn1ctQgrycz/QdMDgQPnnGToRBCXlmtodZ5FNEWM5KieGrQA+gMRFhsLfq5OGaRU7OdSLMnrA2Z0AYa+xgMHQQzfjSeAGE4ftw4iiaAITVdaEAYUOqxFk0tJZYfuTBcsiD5SoPBvtf9o+9XskASqCVAxAWL7HhQGxahiLi5gQkzB5mkY+po7pqYjczJAwG5G5GKnVeNvW+zPVaYrlKii3tCWtH+RCHSjEiDAZDbo6AsCKuJQYHnHJzyFHtCu1kqedaYjHfmVMiLHdHP0MiLFeJMNj/8nAgqO5oKxGWD5mMHQ25K4RrYwW2s72OmF2yyN3U3MzKiMGAZAMMavSZ2cSDhQsmZmanboDBsckGGEvHKifsXAejMXYkujxF/HXMFRx1ytGHIbMmq5USBqG1EttgoQEZ+uSAR4iE5foJjxAJ658+re92Bv13f/dJP6MVaqGvDwyM+FQSDoQX3QnnPSYc+Jhw4mMMHcVPzznhnM5hQOZrExIGe+tzNDj1USXFlvZ0tSwbq5zwcx0MRm0dHQApIBJ8J07YOmSkaVRbB/YrEyUCQwvy9ZEKyyEVlqtUGOx/3m9vdCYMSqCdDwhMEe+HwYHowtsOhdklc7tkkU+dBpkzJgwG5OtuExUGe+ue1qkw/eclHJvN1YwKY40djIYW3uNrU8QF76/ggFN+DnRXWWmnMIfWUtqZQmH5EQrLIRSWq1AY7P/y4cvH3TSt+tlKheUHCsaLw5hTqDAoIu+e8d8SG9qOheVT5cNyVj4MBmRTtAkLg71VLCxXq4S9139e2tPVsmysWGMHg9Epenx1fCOepRPRsCEjuwdp5azX0DpV8TOnpcTyIyiWQ1AsV0Ex2P/tl0+/q2QJVEB7B04srmoAB6L2TkDFEjRzqCH+jmgkeeo6KZ91DgNif19Aje5YvTxYHkNLwbH28mA5I7/YX76DwcD75St8E95v1x+ue2jHX8NfF618j4xWDhmtXGW0YP+367s12qGynvCYg0pecCD8JGuv4mWXzO2SRV5NrXvZOY8wIF/3mmp4wd76HpUOaeUxYBRsZ6/hxfKxyhmjBYPRiTI68zH+/OwK34sTE2XISNVUylfIobUUBUJyetJjfiS1ckhq5SqpBfvv/n3ZgNK5UINJrfwAp8Qfc7+GQ2FL289ufJOgmUMNMXU99RKJsVowIFv7Io3+KGuq45WrBbve5zqplZDilqVjlROOq4PRqKejQmuN8HQirhVSkhWV88r+VChhJz1Nga38CGzlENjKVWAL9n/Xb/TvkKECTtEHPEWcwgwHwn62o1pQwuxsZ7XyqSJeOWO1YEA+RyOVbmgTq5XrRbz0n5cJSW5zwmOtWLY6GI0aenyB8koYOpHYCikBtQXygecStQVyCm35I7TlIbSVq9AW7H++vvvPjVrGC0rwJO0BtgWHYs+wUIR9bZfM7ZKFj8pFiWPjPAO3YEAyS0ONuvCGvVVTe72Ql9exLXu6Ws+wLdbYwWDM09HVceKwdTjqhKeHROnHNIfWqXOaRVrHBj+iXB6iXF5FuWD/D+tNv1VX4VCC5mwPUC44EJyzoYJ4O4HjsodZ+GyCr/aEWzqHAZm5TYW9YG/d3HphL/3npT1drSeg1oqlqoPB0McS0ZXJa2HsRIwrZAPUug+t9YuMbj2HbsjY7mhsyHR5lemC/Zfbzd1Hfea2Hw/pnb4ch0Nha9uZLrtkbpcs/FSNL8+YLhiQrsahSqU/YG/d206fuJ0+cduPhvSE21qxZHUwGHhRHF0an8frcHwbTlj7MGQ+29cbENYOqcrEOjy0oHX4EejyEOjyKtAF+1/slsHgUBqiAWYOIFf8KgkOBL9qhApiZjvIZZcs/BTI5Vl1LxiQTdOWox4vTL0vvQ5yeb22lz1drWcgF2vsYDBk5fGF8QK2hgNOWTkko86zUu6RhdaYtBYZHPv4CHB5CHB5FeCC/S/6689PN/p6217VyweGK67qBYfCTrYDXHbJHEpIZWw/dQqkZwQXjMjnZdMpkLC3Pi/rp0B6/RRIe5JbzwAulqwOBoNr7ojgcnHxTTjilJtDNqqZU4hMjwgukcSxn48El4cEl1cJLtj/fP1wrb+VhhJi54BwxS+x4FB0f8xe2AtK2DO0HeHyxdQGGUO4YEA2OZsQLlPvS69X9vJ6ZS97ulqWjdWoUUzOKQhXdHVcDBRewVGnPB0QriLPlE3v0NqI8x9FIseePkJbHkJbXoW2YP/Xdxv9vTRUEEsHXEistc3lvaCCvJeGGjJF28MsfDm12mbMFozIDG0q7gV766ttvbiX/vMyIV+tZ4gXa+xgNLTcLiMeU2yKJdb2CtnIfKGa+dAqD3P1lBvzR27MQ27Mq9wY7P/2/u6uf9TPi4Mi4ufAIcUfN8KhsJ/t6BiUsPnZzo75MZskSwJ5xo7BgMzOJnIM9tbtrNbxeq//vLSnq2XZWHlGjsFgdH6uotWTsHQiOTZkpJw1yufKobURlImn5Jg/kmMekmNeJcdg/5f/vL97uFcNbQfHfCh2JN5Jm8ExqGATdAI4hvOCHT11BKQnMNQ5DMifoU1FvmBv3dN6kS/956U9Ya1n5Bhr7GAw6un4GMi4NBC+Gyc8PRwDWWjF7v3zMZD83VVNHX7kyDzkyLzKkcH+r3YPtWqBXaiAe90NsLeZI4MKMl8ncGT2MAvfTGChnh0GCQOy+RppdG+bKDKvV/zyasWvlv3VVqyxg38quNkV4Xric0V8T02YNPyN/SxrlLV0EyZewYrQMx2LIwxWQBjMqzAY7P9mvfm00WwJFciWRSDB4o+P4UDQllCBbWmXzKGEPBUXUxxYwTgwGJFOulClGhP2Vo1Z6CSY/vPSnuS2YCQYa+xgMDbpRtfHx9fnCt+M3M8hI3WpvVQe7nxRJKSg6FdxRL8KiH4VKvoF+/+8XX/UiwlACV5HFwfApYofjOFQ2NF2+AtKyERrD7Mops51LBj8BQOSiRZqdD+bSngVOvyl/7y0p6stGPzFGjsYDM3N0ZXJ44rNV/g2nPByyMasqJUa9qE1y8RGV0F5r8Id3Qx5r0LlvWD/8/7P/u6Tum8NNcTOh1TGZwG8hkOxV1FQRBydcLBjgmZRuAmes2DUF4zIJ2lTLS9T78vhusWmVn9e2i9MWzDqizV2MBg0dYR9iYK5cMQpUx+GzJrZTCmYG1plGa/QAJ6DiyP1VUDqq1CpL9j/5d23wnwf9RfMUAUX3eFsu5jPhgPhKdrOfdklcyhhfp4CvwoGfsGI3M+WqlwXsLe+6NbRr0Kv4WVPclsw9Islq4PB6KI7quEVPwFe4ZtxwtMhI7V32kQ9HPr4oo5NTRGw4oiAFRABK1QEDPb/+/3dx43q0DOoIRN1wInEutt8sCNUEFMnHOxoD7Mo/NSTNEPAYEBualMRL1Pvy0JHwAodAUtIcssSsioIINbBaNTV4wuUZ2L5nciBDSlp6m+fXghXD8c7itU35cCKIwdWQA6sUDkw2H835/Y61skkuqMDBRZ/awEHwo62I2B2ydwuWRTFxPuoghFgMCA3tKUw1wXsrc/SehmvQmfA4NjM0AwCY40djAaQkejauLhA5BW+Dye8HEC52axUvqMaciU/thBZHJv5CIAVEAArVAAM9j/fbD/f9PopUlBEpuhw0l2MdcKh6LO0Gel6AyVslrZX7iqmKncVjAKDAbmpLcc2XsDeuqn1Sl+FzoElJLktGAfGstXBaPBZOgLBxJEVcMQpVwcQTD98Zrj1Y0tTDKw4YmAFxMAKFQOD/d/db/sb/d2VtXpYcQBexOdTcCBqZzsFZpfM7ZJFUU09SDMIDAZke90mCAz21lfcevkw/eclHJt5mVFgrLGD0dAEHZUOy2JAGw44ZeUAgGWZauVwAqR4D11QAKw4AmAFBMAKFQCD/V/+tpsTVC8jBfRyKDYlvGymv8gfFhrZfsCjXbIo6qmHZ8Z+wYDMyJYDGy9gb31S1ouGFTr5ZU9Xy7KxKhj5BYPBOXl8ZVz8OvEK34UTRg5Jqg4fNsdGHg5/FJ9PFRT0Ko6gVwFBr0IFvWD//11v19r3iGdQQZbZBx5GVMiFQ8GPIaGCeNl+uqNdsiimKoYVrGIYDMiX2KaKYbC3eo56obNehX66oz1hLcvHqmB4GAyGZuXodMeZmJUT6bAhGeWsmilmDje9OEW9oKXCyiMdVkI6rFDpMNj/5e36Rt8CgxJs5nKmPzPDodgiG4oImZ2gmSdoFuVs4qm5ZJQYjEimZ6hRn5lhb3V6LnVGTP95mZCvtmSQGGvsYDS2sx1dHrmzDUedsPWQkiLzyhwdWuXXkCWlxMojJVZCSqxUKTHYv+s/b/QjKqCE2BpQYnAouN6GCjxH2yVzu2RRZhMPziWDxGBAOkdDlTpHw966pWOSKVhax8TsCWtLhomxxg4Go44eXx9fxqtufC9OODokqspmyv52aC3F9rbI79jR7uhoSIqVKikG+79a/7a9109Vhxpi6ZDJeNkNh8KWtmNidsncLlmUbuIDi5JBYjAgm6NNhcFgb93QTp+jnbrotqerZdlYscYOBqOGjk57dPF3GfhOnDD0Ych81uRKKf0hX5lASkILWnofObEScmKlyonB/q/69c1u8as62kqJlYE4imsKwYHoytsOikEJW3gnkGJlPvEsXTJSDEbk87SJFDP1vixVJOx9qRcJs1+YtmSkGGvsYDBq6/H1KSph60RSbMjIrNK+iQytXmxzi0SOXX0ExUoIipUqKAb7/+/OoY9qyT8ogaYOhcJipgQOdNk/br7qs7QdR3qToJlDDZmno1JhooRnyTgxGJDN00ijz9OmQmGlTonpPy8TUtyydKxKRonBaGB3LLoyRR1DJXDAKTMPhcK8dsZ6ORQKE4/RFBArj4BYCQGxUgXEYP+L3QwNzq/CGuTmAxETf+TyGg6El9x2QgxKmJftiFg5VSSsJJWvzmFAPj2byoSZel+WOiKm/7y0J6xl+ViVjBCDwdCLq+ja5PIROhERC9n4VlKoVDbFDq2FXHAXdMF9JMRKSIiVKiEG+3fXu7+B/u0k1EA3B84oPrgVDkQX3HY8DEqYoe14WFlG6zmx0c3wMBiQG9pUJgz2Vt9dlToeVup4WEKSW5aQFWvsYDQ0PY+vjavjl1dwwClDh2z4Iq+UtfahtXmR00MxSkqLlUdarIS0WKnSYrD/65uN+mb4DCqgucOBe+I9Fhro/eP1WkvlL1BBnG0nxeySRRkXpBIzNamBdQ4DcmNb6K8L2Ftfd+sFw/Sfl3BsZmzGirHGDkajT9JV9HF0Hbs7ERgLKcl8ViqHMofWQmAmZXxo59jPR2CshMBYqQJjsP/5un8As7W9YlgJKobBofDq286MQQmbrO2aRTlFjZXsqEkYkT1Km+qFwd66pdUzJd+XKky2TMhXWzJsjDV2MBqaq6NaYYIaw/fhhJsDF1aXpfZG+tBayX0xespkeYTGSgiNlSo0Bvuf90/fdrvVLyihiPg5HDUpnqbNJcKggvjZzo3ZJYsyLl0l1t6sQhgMyNyMNPrK21QhrNSpsVI/Z9KerrZk1Bhr7GAw+CgdHTGZiV3uRGxsSFLhXK1sjA1FxcTGGC0qVh2xsQpiY6WKjcH+Z+ubjV62BEqwl6tQXSn2MhwKehkqsJehhMzN9jCLaowkyfK8FSPGYEC63oYq1c6wt/ptxnDNIjtXOjNmT1jL8rGqGDIGgyE7R9emyqMRr/CNyO0cspHNskaptz2kUBTnrSguVh1xsQriYpWKi8H+b/vrz+v1repn64GSVWCM4iJEcCDsZjstZpfM7ZJFFZUUi4/yeVsxWgwGJDMz1OhWNpUUq/SSYuFnYWU7K1YxVow1djAYWGZHF6acxctsfBNOODnwXu7bI7N08qHVSydTTKxyRydDTKxSMTHY/1V/1z+BM+egCFo5JDLe5IYDsU1uKCJutoNidsmimgLFKlIg6xwG5FMzUul+NlUTC1dN+Nlpf/4lHJs8N7OErFhjB6OxrbDoAlViKwyOOuXqw5B15ZQXV0MaRUUxkcixqY+kWAVJsUolxWD/V/32cfOgw59QBE0dWJkY/YQDUVPbGa43CZo51BBbTxUVqxgqBgOySdp0miTsrT4+Vzoopv+8tKerZdlYVQwUg8Gop6OSYk38iQYcdcrTISN5MVMOoQutYp6mmFh1xMQqiIlVKiYG+7+8AcdVQQX0M6glBgfC6217LTEoYV5OOE+yGqNIsuJnxSAxGJHP0SZMDPbW7axWDXsffhZ2tp8nWREQbMUaOxgMrbnHl8YX8WtofCNOODnkqPjGcUsnBzhScCUitWMvHymxClJilUqJwf7d/d2/1H1tqCB7YaGOmNgLSzlNEoqIoRNAMXuYRTVVS6xitcRgQO5nUy0x2FvlSir9PMlKB8USktyyhKxYYwejwf2wiBRzYj8skRQb0uF9oXybUQ2HSXKwpKKVxaojN1ZBbqxSuTHY/6L/tHPrnXp4FREBg4dT9sR0jQaCbAlUEHcnUGP2MIuqnEC6K1JD6xwGZEtvy2GRF7C3PlfrzFj4WTxPJzBjLB2rijFjMBpde0c135x4nk4Ex4aU5JUGjg13vqh6IDI5dvQRFasgKlapqBjsf7H5eN3rU7b9dMkq0GLigdp8uiRUEEcnnC5pD7OoxjSSPESjYrgYDMgcbSosBnvrjtZhsSpGnIKj7adLsmysKsaKwWBo9R3VFat87OVETGzIUVZVyjca1VBXbGKuptBYdYTGKgiNVRo0dgb7E5sekJcYe38Nh8I2tRNgdsncLllUUydGVoz/ggH5qtpUN8zU+7LSCbBKrxtmT1jL8rFijR0MBhfV9QStje/DCaMOdcMOL6dioz7XDRPb2BQBq44IWAURsEpFwGD/7uP10426lj2DGuLnQIDFRCccCvvZToBBCZt27ZpF1UxtYjMGDEZk867plEjYW/ezCnu9139e2i9Ly7KxYo0dDAb9HDFg8fuUK3wfTvg5MGBVo1UkCa07P8cQWEUhsPoIgdUQAqtUCAz2P++3N5sn/V0zFGFD1wfsJYYpXsOhoKGhAhsaSoih7WEW9WziybgmZNM5DEhnaKhSn41hb9XRdYwqHRxd6xiYPWFtzTAwlqwOBmOPxtH1kfUL8M3IXT0kSue0Q6vktEMD+IqqPqJgNUTBahUFg/3fru/6m81/VE/bS4fVgS2KCRI4FNvOhiJiazsPZpcs6myCIKkZDwYDklkaanRPW3pf1joPVus8GByb7HfVDAhjjR2MRk0dUWF1/K4Zjjpl6qF42Mwp75rroXhY/ClVTamw2h1NDamwWqXCYP+XT/2jWmgIKtD2dR2yGL+fggNRQ9uRMChh87SdCavdBOFZMyYMBuTztOmESdhb3fEKly3e8Qo/x0/SCUluWUJWrLGD0dDSO7o4Pv7n9gqOOOXnkKW8yZU9r9Aqz3QXWRz7+QiE1RAIq1UgDPb/sAX0CFRAP4fCYfHmNRzo581/rb4dbqlNS79AFTG0/YhJu2RRT8FgNYPBYEA2QyON+hwNe+tu1s+XrHUYzJ6ulmVjVTMYDAaDZo5AMFFkCI44ZeaQDZ/NlLMlQ+vOzM3o/2JrUzCsPoJhNQTDahUMg/3f9b9/6dWvnqEEehvUD4MD4QdqOxlml8ztkkXtp1bejAuDAfk8bTpkEvbWna0CYO/1n5f2hLUsH6uacWEwGHR2VNjNC2cngmEhG1lWVMpr5tDq5bKbgmH1EQyrIRhWq2AY7H92vf68Btw2FJGH6YDPxGVJ4FDYz3YA6Q3UEEPbwbA6AsNEgc+agWH4SpCJ2lQ/DPbW7azXD6v1IyYTrko7Sod4lGZYGIwG3jRHV8b7+DA6fBtO2DkkSX/THFqn3jSLlI7NfaTCakiF1SoVBvuv+v6r/kxtRcLqAMaINTjEcKCv7UiYXTK3SxZ1OfH5c83qiMGAfJ42MWGwt25stWDY+1pnwuwJa1k+VjXhxToYDPk6gvXivdorfBtO+DoUCvOZNksHqE5U1q9p3bD6CIPVEAarVRgM9j/rN4/bzb1+XAZUkWk6nE8YfwANh+KP1HYmDErYHpmdCaunSojV7LBJGJB7Gqn0p2oTFVbrx03Wegkxe8LamlFhrLGDweDaO8LCnDB1IhYWstFUZS5fTg8ZFN9k1PS0yfrIgdWQA6vV4mGw//l6t+LTHW1Gx15DCZ6I7YgYlDDX2hmxup6aiRkjBgOyBTbS6POwpfdlrRNitV4jLCHFLUvHijV2MBqaiKPqbXWMZePbcMKzIRvOf/vMWZi2DgtsMRNTQKw+AmI1BMRqFRCD/bvrzdf7O/1IDCiCq2pQIQwOhM1s58OghJnZXiKsbqZwEoaHwYDMzCY8DPbWJ2AdD6vVymFLe7palo3VqFFsflnxsOjK+FK8c07Ew4Zs1N/eOEszP+NhVexmioc1RzysgXhYreJhsH+3/fJwrb92hhq8qm7C2ZLxZjYcCtoZKrCd7ZI5lBDas4npo5jfbliRMBiRrqihSp2dYW/V0I1eJKxRobGlPcltQwCwFUtWB4MhQ0fXJi/j7S98I3JDD9moM6/MzqG1FKWFRBLHfj6SYQ0kwxqVDIP9Lz5+vPl2WKz6cgqrkJ8DTlTEfjaXCYMKspmdoJlDDZ6fm2xiN7thYBgMyA1tQsNMvS8b/VjJRkfD7AlrG0aGscYOBoOGjo6U9FVs6EQqbMhG5p0CcIfWeuLFs8j02N7uaG/IiDUqIwb7X/Tbze431dz2AyabQ17jfYfXcChsbzuM9AZqyHxth8QaN/Eo3TBIDAbk7jZBYrC3Pl07zcbvw8/x5xn2hLWjfAh3szMmYTDGfUbXR3y0eoVvxgmHu7DKLrzCfYbW+PXzIAKOPlJiDaTEGpUSg/27/tsvqqGRhBg6lA2LXz7DobCh7ZCYXTK3SxZNPgF9NgwSgwG5n001w2Bv3c96zbBGpceW9oS1DSHBVqyxg8HgbB0BfHEBqCt8H054OadeznUvUyysOWJhDcTCGhULg/0vNx9VX55BBVx4ByosftsMB8JGtlNhUMLW3XYsrPFT626GhcGA3MlIpXIksLfuZB0L039eJiS5ZQlZNYwLg9HANnd0bXweY2H4RpxwcsiGvs0dWuU2d0OxsOaIhTUQC2tULAz2f7V+6Lf9k25n67GSzQF8KcVC2wyFQQWxs50Js0sWzdSpkg07VRIG5G42FQsz9b5sdCpM/3lpT1jL8rFqGBQGgyEzR6XCxPeS+DacMHPguXxVefm9ZDMcKhl/BN3QQyWbIwbWQAysUTEw2H/VP/2OVtlIQ1bZgSuKv4GGQ2E32+tQvUnQzKGG+DmqPyXKlDSsPBgMyP1sOSbyAvZWP4JudBis0WGwhCS3DaPBWGMHo9EH5zJydUyP4NtxwtUhJU1eKKe4h9ZKkNsik2NXH5mwBjJhjcqEwf4X/fb3L9u1mp4zogKursBmmLlCGFSQOdp+nqRdsmjGwFGdxW+iG1YgDAYkb6KhRne0CQVr9AJhjV4gzJ6ulmVj1RBuroPB0Aw9vjBl/DXQFb4JJ7w8nCPptbOqmuEcSbHcpiXBmiMK1kAUrFFRMNi/u17ffFVRMCiBRg7n6YmX0JDDetio78t+gQo2PduLiNkli2aqiFhDDkY8hwH57GwqIgZ7615WWbD3jV5EzJ6wluVjxRo7GAx5OTpFshKPzomE2JCMpmwa5ZXVcIqk8HJNV9tHQqyBhFijEmKw/3l//djrSImVD2sOQEzMWLyGA/1806vnS/8CFczLCYAY1BAzN1Nb2gwQgwHZtGwCxGBv3cr6GZL6z0t7ulqWjRVr7GAwZOX4cE/xbiqRDwvJACdVhVZ5UlVD8bBsduTDvv03MrMKiGHB+e45eLv58qD5Gavw0/Phj6mc8owHgyttLMFL7QTNPEGz2Gvojtgwqu5rHJMYG4tUZ+PuqrWHSxezYsPvsbnx8OQxmmZlRVs7HJA9ScdXKo/rvl+RW5MbfchMVuZa4c+huRSFP5+FyOvZidcRA5PNVHgMC15++W27UWdurEFT9+GPqNT+xEMRn9uriiVo5gmaxV5Dy5YMoyKfJxFkWKYux23dL4eLF0Nkw+/S6QkVxsZ5iRfltLXDAbnTI54sXlhekZtzyukhY7uHd6V44NCsOZ1SZNnMnTgdsTDZTOXIsGC1frq/07+mxCI2qR9SKk7MwIPBlTqWMLMnlBxLES32Isp/D8Pqz944KJ3W4YXXzW5CyoaLJ6d19fdlSt7acVbktM7AMhwQLNzji+Tr+JUXHnLS54dBq8YrO2rPKRP1x4YW8ByezfITm8OyVDMVLsOCt/3tGhT/xSI8owfQJt5Zw0ORGT3hUEosYlO6nTHba+j22jAqmtITapGRq657HHUHE7rOmYHflwlpa8dJkfM5Y81wPD6fx4XJpM8TkbMhMVmT1QpzNjRXynxOubNs5k+MDk8vnKnkGRZc9tuHDXhGRxo2nYf6TPGHH3gwuIlOJGQ6txcmwxo6m/vJh3TCVZ3joNTpJgqN3CW60z2YzdXjLJcpeWtpVlbjVvmQbi1SFl+lPIs34vCYkzYPWWkK7XzpoVluqw8tcD4vTmwOjymcqVAaFlxsbvq7jfrCG4uYzwPOFL/0xoOxUsFYxaxux9MSNIu9hn6GPYyKnJ6EqGEZ8LqpdNlw/aTXdUwtIXHtOC1y4c5INRwPLtwjjHAWf5KNh5x0+nCw5b64v3T6cLJlDLYMLdDp5YnTIcE0U4k1LHj96f5mA57PrbXLDn9E5fhpPBRZuNvLlyVo5lhDp/NygnAZhkUL94RjLbFIxVBxd2BxHVwDvy8Tct2OkyJnc8au4XjQ4hFZWMTfdJK7csriw9mWtW5xdLjlsxBZvDqxOESaZiq+hgVdf62es4UVbCYH+BoejGzA2WuTvUkRzbGITeQRxSbKCQ+jIo8ncGxYBDxuItmeL57wuM6yJaStHSdFepxVNsPx+MN5RLWJ8ip43Emjh8TUlb4LV4VVu9yFo2RbNqtPjI44nd0gutGR4NX9zRrttdvPyDz8IZVDMvFgZDa310DDGup0u2ixF03stTPODQedWLObSDdb98vh8slXazrslnCJWpqW1bhVmh3F42aPsTf5Zi2Re3tOzL7ysGL2YAfxncnQAhfuzYnZIRE1U+E3LHi5sy3ccrefoHn4U+5SGlcqxYORid1eJA1rqNvtFNxeM7EVxzg4HHPC7EgGZnYLOnc5XD35gB6jXIPZ7TwcTctq3CpX79aaafFlynO5F5dIxT0npaqaWtuLa4LR48qlMptjo2cnYFwGAamZDsZBwav17RqCcVBFnJ4FME5susPB8LwOJWwJD0XkMT0h0GIvokVZhmGB1WFQtoSHIt3opu6Xw8UTRs/0OmoJuW7HSRGzOmvtcDz0mB5do1IcJUBuywmfDzmpqswpE3o2QHFxeUSZzMjnJ1BcBqG4TIfioOBD/3Ct4+xYw1x+wH2quOQpHgzP51BC5nOoYfO5PdBir6H1HYZRkckTjtzEIn3pju8S3eT6qZvg92VC2lqalBVt7XA8aPLxNSrilyJXeMhJkx9y0uR5Jr9Wec6YfK+WcRwucyceh1RUpuNwUPDqfrve/XX0Z3SoYi4/pFR8hIYHoy/WoIoZ3V5fDWvobO6mNt0zVmMNB+ULdygD87mNiQsXUFrdaVt1y5TMteO8yAmdHciJA0Kvjy9TGR8neYWHnPT6YdCsnFUzZeEemku5HTcIkdlPoLgM4lGZDsVBwdv17ebmBnjdXnTt8KdU6kHgwTAsgyXE6HaUbp4QaLHX0A/VhlGR0VOwOCgCNrdhcRnA4rIY4hpsnoDFZRSLYynrcDzo8vE1KiQqA4ecdHnI1az6VhdCunwg4uLP1mQyI5efEHEZZJ0ynYiDgvP+6VavlIo1zOQemRwNRpbtCUQc1FCT26uy7TUTszmry4ZjUpObjuvE3cFcrldmA78vU3Ld0qysaGuHA0KXR/XZGjmXpwJxQ1LqrNSAuNCsHF7w3IRsfkLEZZCIy3QiDgo+9F8B9wolzOUH1Cfeen6NByMuT4DhoIa63C5a7EUTW3AUh4NBqc1NBdtwd2BzvWTb8LucyxOO8qRZWY1bpc2tNFx0kQonbZ5Kww3Jql2hHOj5nLOZ3GzPOA6XneBwGcThMh2Hg4J5//Cw1q7IGdYwnwd4Ki6UjAcjW+0JQJxdM0/QLPYaDr1mlIeDManLbTwc7A5W7CV4MFd/XyakrR0nRZqcneyJ40GTRzyci6smk5tyyuQBa/OzWjkJcGgu5DcsGefhshMeLoM8XKbzcFDw8ubxer0Fz+XWcm6HP+PufpA7cOaCblhC36alAHE4OcTk1eRGOzvkE8ec2H9DMrDVbikFdzlcPjmZqweALlOy3dK8rGhrhwNCn0dHsdbS56k4XEhKUzTq9ttw4qcAZDJ65GeWndBwGaThMp2Gg4J3/ba/fdJdbq31dvgjKoXS8VDE5QksnF2z2Gs41Zax4z1xTDopw+unu9V0wudwGaRb1aM/lwlpa8dJkWZlNdxwPGjW8TXyymZ5Ks4WcpI5V2rsamj2yvM1Peszy05wtgxSSpmOs0HBz/ef1vr3ZlCCzRpgKGlWNBR/KZZAs0ENnZQTaLYsKuoWbxy+HUZFJk+j2fCF121ugd8uny+gsLle3C0hce04LdLmlGaD8aDNI+YwPunuCg85afMBZsv1fbQBZpOTMofZ3AnM5iDMlukwGxTM++0fT/rXKFADfe5COTDxLQocCk/KUEJMbtfMEzSLvYa/D3MUY4Mx2UQORfrTNeyuP107tZLb++F3MZHD4dmy21GOjeWswwEpnR5dKR8/Il2RW3PC50PGXFFoi+/QXEufOw6zuROYzUFMyekwGxS87e+etoBzgSKykeYy8JQNByNWT6DZ7Jp5gmax1/CNNEfKlZ3jmHw6xxdeN7utyJsDOJsDRd7siWvHaRGvxVjSOhyPez1m2sQbcHxvTnk9o3O6G2q8Sa9n9EHbuROvQ6jN6VAbFPy83t71D8jsCVCbO+S0ELvmcDBi9gSiza6ZYw17OeYmDqV8OwyL3J5GtEEZmNptRFu4euK7s/C7dLv97FCaltW4Vc7sKceHxpcql2/C8c055faQmLrRDv1+zqeA2kIDNPsJ1OYg1OZ0qA0Kzq77T9t7nXeBIryCD5XexJM6HIo4PQFps2vmWEOdnk/txTmKtOHrwZbwFkbtAncHPlePDn0//C59bs9bS7Oyoq0f4pz7WnwXju+yKcsGBq0utRqsemZ+HX6HK/ETPs1BPs3pfBoUvPyy3ajkyBnWYL+Gml/i0zE4FPFrAp1m18wTNIu9ZmJipnAajDkxMdsKtsHuwLBqYbb3zxdVGDYFT2N5WY1b5TrcXLAtuk5evtPGN+aUxQcQc9Zo4EpobiSFKtIcufwET3MQT3M6ngYF5/3+nTaYlxMINVegNTgk1Db/teo/bnptLvoFy5jbE0q2QQ2dnYvJ2ZlCajAonZ3hxdfNbjpWdLh+coNNreS2TMlbS7OyGrfKZTgKiM0+vkq+iE8ixGNOmj0UZau9dnrCkDPJqDnOqLkTRs1BRs3pjBoUXPTb7fpGu6nOmAgZPcA5ou4DHIpM6QmIml0zT9As9poJjxMa6xzHnJjSTWeN4u5gSgdF2/TflwmJa8dpkTM6hdRgPGzy6LjR+F3HFbkvp0weYDNQzMkNJ46Kqm2hBbr8hFJzkD5yOqUGBa/W/d3v65vNve5zM6fmDgyOFx+WwKGIz1M4tQTRHIuY0yMAShZ4cbRwG4xJZ3Nb4TbYHeygqzTa++F36fMUSo1lZUVbOxwQvRGPLpKLq5FckftyyuchKUVZahXVQ3PzYl/l6fn/5PYaL+HmTqA1B6Enp0NrUPBqfXO/vdc/NIEi7PmARUnPo6EoCANVbHq3n1OaoFnsNRMvzijsBmNOTO+2Em6wO1jEA9zNgRJucHhqe8q7sax1OCDfS49OlRXHEONxJ70fMpPVeal5f4DeRDkIkdLI7SfQm4Psk9OhNyi42Nxdf7zuv+h2N3NvLpRwk8/sSdwbVDG7p3Bv9kCLvYYfneIo9wZj0kneRr3B7sDt+omm4PdlQtpampTVuFVuz6F43OzjC+WaWpg9FX0bElPmhVbfKTRnM+F1Tr7lJ+RbDsk3p5NvUPD+ZvMRvCOHGuj0/MD0yLIvcCi+OwdlxOpQw6xuD7TYa7jVc4q/wZjM6lCkr+dhd309nwP8Tf99mZLrlmZlNW4VXocBqdejK5XLT0/guFNeD5nJ6sZlyhbdYAj58J5z/C0/wd9ySEHlOv4GBX/ffN72t7rZzQec5uGAU4G5wqHwkzuUMKMnsG92zWKv4S/dclrIDcakPrcVcoPd9f25HJBvuXrs6TIhbe04KWL9zlLW4XjosT26Rgrgim/KKYeHnDRNoVVlDc2VhN5EMiOHuxOHQ+gt16E3KHh/8+XP/vH+t83N/W9gVk8g3/JDYuO96dd4MGL1BPLNrpknaBZ7zcSUTku5wZjU6jbsDXYHVnfA6g5M6QnYW06xN9ba4XjQ6uNr5AqBz+CbcsrqIVezXD0MLTRnmXxMH5qQ10+Ytxwyb7nOvEHB4qHf9PqeHNTg2fwAAcn3bXAo+pAOVczlKYXc7IEWe82EzSn1BmPyTTkoA0a3cW85KOWWg1Ju9sS1NC0r2trhePCdW3SZfCZKMMMxJ50eklL6TKvlFpqV08pFNiOjn7ByOWTlcp2Vg4K/bz5+ftK/QoUabHQPduPgULjGE5Qwl6dUcrMHWuw1o/N3vHA5heVgzAmXW44rvcDdgcvB6aY5ON3UnriWpmVFWzscD7s8OoJWcXkqKzckJZ95dT4/NMvJnJNy+Qkpl0NYKtdJOSjo+v72CazY4XGo0OOBDJKP5pBEwuv1BELOrpknaBZ7zcRETmCvcxxzwuK2Om6wO7A4QORyUMcNDk834eippqy1wwGxxyNErpEbcKmIXMhK47NK238bEDn5dB5nMzL5CSKXQ1Yq1xE5KHj98PB09xHM5Al13PJQ9EvUa4SDEZ8nQHJQQ+fyBEounyzkltNCbjDmhNFtpdzwjaIbPa4yNhhd/X2ZkLh2nBb5aE4pORgP+3x8mXx84u4VuTGnfB5u9VlRF5rRy+HZXDqdF3PLTzC5HGJyuY7JQcE/+ptPax17hxpm9ABaCWIG/wGw0c3s2husoUZPgOTyanKznRZzgzEnjG7D5GB3MKODYm7678uExLXjtMgJnVJyMB7cg4tK7mVyPk+l5EJOmrLSQJkc1nLLeS23/ASLyyEWl+tYHBSc94/akv3VIZzqV3tdNihhfk05o9QeaLHXTEzMlG+DMemOua2YG+wOXoIDui0HdFtCrttxVuS8TKu5wYB4Xo5K7s3k67FUsm3I1rcj0jTDHpqbFzmnWkVyI/uecG45pJ1ynXODgu66v3vs73qwIDeDbnmoDyY+Q4VD8T30BNANaqjxE0C3fBJ0ywm1dY5jTkzUtuNKYXcwUQPUTf99mZLtluZlNW6VM7X5vNLoOuW1+DoNjjlp/cCr1b7WPk8Lzcop5KEFzdX+BHTzEHTLddANCt72Xzf9Rq/xBkXQ6j4QUcLqcChqdahiX64kiOZYRLzuJ0k3T0k3GJNN8lCkEzCwu+50D84r1X9fpuS6pVlZ0dYOB4RO9zHlJvB1OOaU00NWssaVlVJI3Q+Um4+d7jnl5k8oNw8pN69TblDwtn+662/AsShQha1+4HjkoShwKHy+GZRQn6ccWYr/cMTn2dQbM0+rvMGY1Oe2Gm+wuw6ve0C6eUC62dPWjpMibU5JNxgPPXpH16iI/y2+wkNOujzkJG8yrZRjaC4Ul/Pybt6duBySbl4n3aBgdf+fDXhhBjXY44d8ygNR4FD4pTiUkGU71FCLp9R289FppfLgI08RNxiUr9vxddddboPchssnXK7+vky4RO04LWLZzpLW4Xh4Mp8owXeFx5y0+WHQbFZ47TvU0CxN7vhUfoK4eYi4eR1xg4K3/W3/RS8pATVkG92Hum5y1W6v6wYlzOYJdd3smsVewz819xRwgzEnTA6vu25yG+DmQWE3/fclHp4u2SnhNmoV+3IwIP04JbpWrhQVHPHNOeX0gAR6V2noemie3JvznHnzJ8ybh/CT15k3KHh1f/e0flS318+winnfI+/buTcoocv4hCJxds1ir+Fb8p5ybzDmhPltZ5jC7vqmvAfcmwdnmNoT147TImd4WiMOxuPejxDFTOzLw3EnvR8gNl+r1SZCcyFfpHlOv/kT+s1D+s3r9BsUvHzo+0+61c3wmz+QPZV8XkdD8a25BP4NauhyPgGA89ExpvK0Yk8BOBhzwutIBvbmbDXiPADgPADg7Ilrx2mRXqf8G4yHV/PxQaZiEx6OOenzkCxfq2cchuZGeWjnAJw/AeA85Jq8DsBBwc60u4f2u41u9QQCzgcsSD642wk4KGFOTyHg7IEWe83Eczsl4GDMCafbCDjYHczqoE6cBwRcQrZbmpfVuFVO62YELrpORSatnorADdmqvdMQuCFpypnFIp+R108QOA8ROK8jcFDwZn23uyi6081l4vwB7ylFZQk4FPF5AgAHNdTnCQCcj6rEyXqQngJwMCbdg7ccTnqBuwOXA/xN/32ZkLaWJmVFWzscD+7Bx0XiKuHxVPxtyEmdqUeshOadx8XxiJ4DcP4EgPOQn/I6AAcF5/36Djyim6vC+QMCVIqPzOFQ5AE9gZmza+ZYQ7fg6ynG1VNmDgadmMptNeFM3S+Hqydgdq/SdMuEbLc0LatxqzS5GZqLLpPyYRocc9LlIVm1n6mL9kOztDiH5PwJJOchJOd1SA4KLjc9es2GJGy9HhA5afIkRA6qmM/NmjnWUJ9PnoHqaS04GHTC50gGfG4pHnf5fAGFzwEil5C5luZlRVs7HBAbfXydvKRj4ZiTRg9Zqarca9P5UApO2YbjxeCKE0augIyc1xk5KHh1f7f5Cs47hiLi9iLQVGLVDgfDq3YoYXvuUES8btcs9prTp/Oqia1eUEIOxuRWhzLd6rC7vg9XAEZO/32ZcoXacV7Ewp21djggtHp0napSvFbHd+aE1UNWmqrUpvQhZ3LdXnBErjhB5AqIShU6IgcFZzf9dvPwuPtV97oZkivC2Y9eON1eDA5KyKRu18wTNIu9hr9cK2gxOHxFyOM5FAGb245BLQAip/++xMNTm1NGriBYYYcDoufz6CL5Ujyf47tyyuXhbNhq/7WLtHk2TOiiqkTBy8EV7sTnEJYqdEgOCi52d9X64Vpfv2MVdPkhpfJkczgUcXkCJmfXzBM0i72Gb7YXFJKDMSemcyQDPrcUjrscLp70udNX7vbEtTQtq3GrtHnSAajRpZrFn1xeMXNs1x+ue+b2w7DVLCu0r1GHzJXKrO64209ouQJSU4VOy0FBt1u+A2IGatjqPZSEE8QMHIy4PYGWs2vmCZrFXsM/bykoLQdj0jnddgiqqfvlcOmk10ExOHvaWpqUFW3tcDy8ch9dJFfHX15d4TG/w+eh3lvlskZbvAeOTqFlREIjn5+wcQVk4wqdjYOCi81N/+lJ97m5Hlzh0crdfnYqlDCXp9SDSxAt9qJTn8dFgt8OwyKfp4FxUAacbuHoLofLJ16u6b8vEy5RS9OyGrfKWT0JjBtfKlfXclaHN/q028Md39SNVjNqyFypuJ2zccUJG1dANq7Q2TgoeH1zs+7Bi3QoYtN6Ef6xE4a3F4eDEmb4hOJwds1ir5l4VKdsHIxJp3VbaTjYXUfgC5WAez/8Ls2eQMYVlIxjrR2Oh6f10UVyjRf0OxzzO4weSNB81mi770PWKgnMiERHRj+B4woIxxU6HAcFl/1/rsEH6lCDp/UD8uPl1jsair5ogypm9IQjVO2axV4zsflO0TgYkxrdQrpd4O5gVi/B+l39fZmQtpYmZTVulUY3c3Hji7Sb0aXR4T0+bfRDWurSOe379CFrTnlO52RccULGFRCSKnQyDgre9v/vi47AQgn2eaiIJWh3OBSZzRPAOKihy/cEMK6Iao7l4uP0gh6fCmNOrN5taBzsDiZ0cIBqAdC4hGy3NC+rcas0OgrIl+/V5MM6vNGnzV7x5XuFl+/86NTiBJErICJX6IgcFPx8c//pzye9FCQUYbsHSE4UloNDEbsnQHJ2zTxBs9hrJvbkKCMHY0643VZZDnYHbgeV5cLvclZPqSzH8rIat0q3J52bOr5WrogJzityn0+7/ZCbutmXfZVuD6nz8oS1gtNyxQktV0AGqtBpOSh41W8ewM6cuZxccUCASrn/jobCQCyUMK+n1JJLEC32oom5nRaTg0En3G4j5WB34HZQTK6Iya7B7QnnphYUlGOtHY4HX6uPr9JMmdXhPT7t80NWCleqhdyHZIpDUwteS6484eRKyMkVOicHBe/6//TbLTg2FarInlx5gIC82ISHg6Fp/Q2WkCnarlnsNXx/rST01jmOyR67oUi3LOyuP3aXgHgrY0YrWBYOzyZolpXVuFV4FgZEno0u0kyWf4RDTns2pGW3OJtpj92h2bkXTeza0ARdewK9lRB6K3XoDQrOrvvbP/oNYN6gjNk2IFJiNQ4Hw6txKCEztF0zT9As9hpOspeUeoMx+fwMZcDsNu6tBNxb+F08e9sT19K0rGhrh+PBPbbxZXJVLY5GhGN+h9kPaamzotROOh6ymclyryUn30p3YnZIvpU6+QYFr9YPX9QijGdYw5x+SGohJ2g7+QYljGSHImb1lAJx5ST7VlL2DQal07rtDFTYXV+Jh4snVuLhdzmtJ5BvJWHbVrS1w/HoY3d0oZwX78jhuN/h9nC/u1nmFPJtyFwjd9RFUiO3n5BvJSTfSp18g4J3/d2ne+D2BPKtDDyQdHvSYahQxab2BPgNaqjfx/SbUkKmJFXPznHQibndVisOdgcLeXAYqv77MiHb7Tgtcm6n/BuMh+f28WXab0xFbk/n30JairLJtYfvIWuZ4vacL+RP+LcS8m+lzr9BwfnT7tbSzW7G38pQFk68J4dDkYk9pSwcFDGjp+BvpZ96TV4SkuscB6UTu+00VNgd2BxUhSsB/GbPdTtOirQ5PQ0VxoOP6+NrNCtEDXd8h0+7/JCVaje7avVjhqRVcis9NEGXn3BvJeTeSp17g4JX/d3dZn3zdAfmdXNhuDKAb5mwuh18gxI2pSeAb3bNYq/h785KCr7BmBMzuq0oHOwO1vAFsDooCmdPXDtOi1zDU/QNxoNWj8i3mVy+p5Nvw60+c06r/hia1Z05Tr6VJ+RbCcm3UiffoODlH1+2X8GMnlAUrjxAPYUoMgEHI0ZPAN/smnmCZrHXTBidUFznOCad0W3gG+wObA4qwum/LxPSthonRc7ZaERo5IhsU57D08m28PcuCz/Tjlcb0uIVI5d8zj4h20rIOJU62QYF/3i6vwU+NpNt5YHWiQugvSbRsYsTyDaoYa+/7YEWew3/LqWkZBuMOTFf2848xTeJbmRQ9E3/fZmS7XacFzlhU7INBoQ+H1+mbCa+Ksf3+LTPD2kpdrO1Vj4iNGdKHdeSQ23lCdRWQrqp1KE2KHjb/1sHWKGCTdeBaRM1XOFgfLctAWuDGur1lOJvZT253UbBNhiUztm20m+wO5izAdam/75MuEAtTcqKtnY4Ht5sG18k76TV04m2kJZ89w+9RrSF5szJr1JKTrSVJ0RbCYm2UifaoODnbf9pvd2oXNkZljG/NyGvwu+wzBae2BO4NrtmnqBZ7DWcaitp/TcYk1rdUs7tAncHG26AadN/X+Lh6axOD0hlrR0OiL3ejF+aN5KQSafaQl4ql337AE16fSgPp0zrvP5bdcK1VZBrK3WuDQre9h8/9zcAVocqYvVqFpY1sdXhYNjqUEKsbtfMsYZN69WYs1JOXKooDAeD8jU8lOkTO+yuu70COFwFcLiEzLU0Lyva2uGAaA0/vkyujOuQXuEhp80e0lLVhfoWLTS7Rj6rVxyHq05wuAricJWOw0HB2/7T01o/Ww1qmNXD8ZCCVoeD0VU8VDG3J+BwUEPdHvFwheDhKsrDwaBsZoci/XkddtcX8RWg4SpAwyXkraVZWdHWDgekkEx0pXL5bQoc9zsMH3KWN0WhPLSH5kzZZa84Ele5E8NDJK7SkTgo+LC53Xy+1w2fgMRVLqxthOHtZ6ZCCXN7ypmp9kCLvYYfyVBRIg7GpG63HZgKuwO3O30dH36XM7s91+04K3JmpyemwoBwZnfj12nxiUBXeMjvMPph2MLXpbaMH7I5U4weJzQy+gkNV0EiqtJpOCh4u7672/xTryQBRczpoZqYwNzhYGQVn8DC2TVzrKHzej71xF5RFg4GpU63VYKD3YHTQSW4CpBw9ly3NCkr2trheHxaH1eDK2rp9nQaLqSmmeWVVjZmyKiXD+0Vp+GqExqugmBUpdNwUPASFY2BCmZ1hMPBwYjVE6rB2TXzBM1ir+Efr1W0FhyMOfG8bqsFB7sDr8flyQavq78vExLXjtMivU5rwcF4cFKPysBVYh8eDvkdNg84XF06dVL3+HGdl4GrTnC4CuJwlY7DQcHP2/X9HfjkFIrgy/XqgPnED66v8VDE5wksnF0zxxo6pUcwXMz0vx2GRVN6GgyHr7tudNsJqRU4IbUCMJw92+04LfJZnZ6QCuPBXfjxZdpN6F44PZ2GC2lp8jxzmtMDWljIou2hCU7oJzRcBWm4SqfhoODV5uNG/0YVSrDP0fmocCji8wQUzq6ZYw31+SQLV1EWDgad8LntfFTYHUzogIbTf18mZLulaVmNW6XPzWXgxpfJNfFHCFfkHp/2+cB9NlmubcCXw2O69DmH5aoTWK6CHFSlw3JQ8PN2N6E/6EY303JVqIold+PstByUMKObNfMEzWKv4Z+iVxSWgzEnfG6D5WB3/VP0CpSBq2K2a/B5CizH8rKirR0OCFfu48s0q0S5R3yLT/s8wHJl0ajzeUAMM8XnHJarTmC5CsJylQ7LQcFyffvbenuz1utCQRm2+gAQCasn8XJQxdyeUAYO54dN65NnpVYUl4NBJ+yOZMDulrJxl88XUNhdPUN1mZK5luZlRVs7HBDP6+Pr5GKY84rc59N+D/lqiirT3rOFtEm3c16uOuHlKohNVTovBwUvbzdPW/CYngDLVQERivL5C/kDENsmkG92zWKvmViMU/INxqT76BaU7cLW/XK4DtKz6u9LPDz1LCXfWGuHA2LPjsm3WgLtcMzv8GwoXlhX+iZ6uK1L+bFpxcm3+oR8qyH5VunkGxRcrD+DT8qxBFm2noGlOBwKL8WhhLgcatiLcXugxV7DN9FreuopjMknZyjTjQ6768/c4eqJZ+4aQG8J2W5pXlbjVmF0GBAafXydXJOJulD4Jp80eshLtS/PKo0+pLOWtSNERiOjn1BvNaTeap16g4K360+bHmBvUETm5zpgb2KDDQ5GzJ7AvNk1c6xhK/E6m/pwpabMGww6YXZ45XWzWyC5y+HqiVld/32ZkO12nBYxqbOkdTgefTseXSoniz7Ccb/D74fU+KJUz0oLzVn1Ihd+59Bb7U78DumnWofeoODd093dugc1JKCKGf6QVrmjDgcjhk/A3uyaeYJmsddMTO6UeoMx2RoeivT34/g20d3ugNvV35d4eDq10yNQa8LEdTggntqjInCF2GeDY36H1cOwZZ7NlDV8aN5ZXZy0MDQhq59gbzXE3mode4OCt19u1nd3+i4bFOFVfKgBJ6rFwKHoLhtUMavbGbZ5QqDFXsO5t5pybzAm9bqNe4PdwcwOuLcanICakOt2nBXpdQq+wYB8ah9fqVkjl/Lp4FvIjS9rVyvFJobUKeel1Rx8q0/AtxqCb7UOvkHBm+367hN4aDfXgatDKTFpdzv4BiXM6wngm12z2Gv4hnpNwTcYk1rdhr3B7sDqoAqc/vsyIW0tTcpq3CqdjuLhWX18kXJZUgbf4NMuD2lxda69PQvNmZNvz2rOvdUn3FsN+ada596g4OWXh8ft+rf19lH/dAUKsdUD+yYOQIVDEasnsG9QQ6f1FPitLqa+Sq0p/AaDTjyz2+A32B3YHVSCqwH8Zr9ELU3Latwqn9nN8Ft0mTIvNuLxTT5t95Auvy/kLO0espbLuu01h9/qE/ithvBbrcNvUHDRbzcPPdifM+NvdTgFVXyUCofii/gEAs6umWMNNXt0DKos3F7TY1BhUDqz2/g32B3sxYNjUGvAv9lz3dKkrMatcma31oobXyNXye9W8D0+7fRDVpqsqDWgfUimUiuu5qeg1if4Ww3xt1rH36Dgl93D+voGOB2L8MZcoIIEFgMHI9N6AgFn18wTNIu9hr9crykBB2NSn1uAtgvcHUzpoFic/vsyIW0tTcqKtnY4HvT5+ADUKr4pr/CQ3+HzoTDivqaM9HlImnxI5/BbfQK/1RBoqnX4DQr+96m/67VlzxnW4OkcVYqDQxGLJ2BvUENX7gnHn9b15FxOsTcYk3rcBr2Zul8O107O5eDwU3va2nFS5LKdnn0K4/H9uDqa0OX+ezr3FlLTuN1/aftxIXOFsnTn5Ft9Qr7VkHyrdfINCla/6TY3H3xahwJx8gHdXiAOSpjNEzA5u2ax10y4nGJyMCZ1ua1AHOwOVuwAk9N/XyakrR0nRbqcUnIwHnd5Ez2gi2MZ8E0+7fJAymWZSreGZpcrL9Q5KdeckHINJOVqnZSDgrf99VfAz0ANdHoTzj1tYqfDobDToYQ43a6ZJ2gWew0vGNVQUg7G5BtxUKbP6Kbul8PFE6t2/fclHp69YmN5WY1bhdlhQLgTF12nvBCv1/A9Pmn0kJfcNzPt/JXQnClGbzgp15yQcg3kpRqdlIOC8359s9kt3nWrJ6ByTSCHvDC7HZWDEmb2BFTOrlnsNZyUaygpB2OyaR2KgNVt1eEawMnpvy8T0taOkyKmdZayDsfDTh+flbqzpHB6OiMX0lLVVVMqC/cha+Iw84YTco078TlEnxqdkIOC7np997tu8gQ8rnFg7Q4HIyZPwOPsmjnWsO32xk29W2soHweDTkzptrpwsLv+IWq4eqKCTPhdTukJJ6WytKzGrXJGTzopdXypXCN34+C43+H1w7DFrK4bbVYPGVVObhDJjtx+Ask1EJJrdEgOCn7ebh6u+y86DwtVzPDh7EgBzsDBiOETIDmoYXty9kCLvYY/rTcUkoMxJwxvOygVdgcTOzgotVHxuWVC4tpxWqTh6UGpMB43/PhS5Zk0fDol1wyUnFPrRjXPlJzgZxpOyTUnlFwDKblGp+Sg4OX28+YRLOITCsQ1BwZIHoSO/wAPG/Ug9l+IhNjdfvTpPCHQYq/hVV8bCsrBmHQVbwPlYHfwwA5AuQbUh7OnraVJWdHWDsfjZo+KxDm5kk+H5UJqyp3bK2VzLjQ7LxF4kdTI7CewXAN5qUaH5aDgbf+f//S/rW9U855hHTP8gQKqREVIOBiZ3+0Q25sU0TxFtNiLOBvb0INTYVBqeRssB7uD+R1UimsALJeQt5ZmZTVulSt6My03vkqulmc4wDG/w+/hbq8ytaT7kM5G2aPjB6c2J7RcA2m5RqfloOC8f3xCO3QJB6c24bRJURUSDka8nsDK2TXzBM1ir5nYjqeoHIxJjW5D5UzdL4dLJ40OUDl72tpxUuQOHS0VB+PxuX18oXJ5EBsc9zu8HoC4alaq+/EBivPy9XrDebnmhJdrIC/X6LwcFLzqH67vkdkTeLnmgAMp2/F2Xg5KmNkTeDm7ZrHXTGzHk4NCz3FManYbLwe7A7ODenEN4OXg8HRWp4erNhSYgwERMDe+SK6Ovza+wkN+h9EPaalneaF98TJkU6kLGZrgE/sJMtdAZK7RkTkoOOtvbtQn5jOswW/YDyRQXH/vNR4Kn9ICJczlKchcgmixF/FjlBvKzOHrwXxuY+ZM3S+Hiye34wEzl5C3lmZlRVs7HJDP6mNoronXm1fkPp82e2BEXVOrs3o4lFbuxnNkrjlB5hqIUDU6MgcFL+/u+q+bj4/3W1DZHSrZxB5YK1F/Cg5GJvYEfM6umSdoFnvNxMROWLBzHHNiQx7JwNRuqzPXAICuASeswuGp5SlB1xDosMMBueXHteYaZSGfTtCF3FRlrZ6oPKSukbXmQhOY33eLhWfTf/tvZHqVoMOCXzY3m4+bR7VQBVahGf7wh1TKzeGh4AyPJdjuCZp5gmax19CCNMOo+vyOY5L5nVx1zey4u/q6fbh08UP78Hs8v+PhidlpVla0tcMBmdnjK+XKeEee3OVTZh9y44tsNpNszdCcVeKp/bkJmT07MTtif/Z7gprZ8SmrH/vtjTq5YxGe3A9/ym//mAm3myE6LCHb8SmiORYxv48xOlmUZhgV+T0Bo8Mi4HcTRjdcvHhyH36Pd+MT0taOkxLP7bS1w/HQZnx0kVwdn/R9hcf8Dqsf0lJWlXag8rMVClGPRiY0sro7sTokpGYqSIcFXf878Lmdozv8ERUyHg9GZnU7R5egmSdoFnsNfc0+jKov4nFMuognV133uel41eHiSZ874HM7RjdOi5zWGUaH42Gfjy/TrIof2fGY3+Hz58cCr07pIWulQOiGJrh+z098DsuNzVSEDgvON1/0bXgswYv3wFnFH63joViBCqxiTrefroo1ZINuL5qY0AkLdo6D0gndhM/h7sDoepW54Xdp9IQqczQrK9ra4YDY6dHRqmVcPJbc5dNOD0er7uZW5Un9OZ3xPrzMZ+Rzf+JziFDNVHIOC86+cbKbjypKg1XY6gElij9ex0ORGd1Owb3BImZ0Ozm311BQdhgV+TyBnMMi4HMLaHc5XDvpc6/uyiWkrR0nRS7cGTmH4/Hn9Iici7eQrshdPm31Q2rqImuU4rHPGXXKpE7JOTcrTsyOSJ/dILrZkWDePz58ud3oXrdjc4c/pVI+Fg9GntPtReawhj6mJ1Bze9Gp3eMj/d4Ow6IVfAI1Ry68bndL98vh4km769RcwgVqaVJWtLXD8bjdI3CukTN7Mjg3pKb2ZaW8Y3/OqBN78EMTXMOXJ3ZHsI+bqeAcFry9/9TfoVW8nZw7/Cm/7UUIu5vJOSxhdrdXjJsnBFrsNROTO0PncMyJ53UTPIe7q4Xhn6+e8Ltaf26Zku2W5mU1bpWGtx60Gl+nWeOF2ZPJuSEvZZF55R37czq9sjFHyTk3q07MDhGqmUrOYcG7DXi9jiV4FR+Kb8U4PB6KrOLtWNYbLGKreLPm7aBBk3YCFEeuqG5i0yGqw5WRJlZhuWVCUlqalBVt7XA8PmmPwbgmiytGknt4wse/DqmBU2994kZE5LiZirdhwcu7x/4r8iMSsak3ED3yudpcFI78ocnUaz8LFWvoQruefAFOKpyd46DUsybADXcHE28NPKv+vkzIdUuTsqKtHY4HONboGrlGeaRORtuGrFSHSs5y2g1J015919zozYnRId40U+E2LHj5ef34n/4GPFNbq8Id/pDfbnzhczQUmXntWFuCZp6gWew19Cu0YVS0wk7C2rAMGN1UF264eNLoel04PDxdYTOsjbZ2OCCfncfXaha/rr0i9/m03Q+5KWrfOO21WEipE9+iDU1olZ2dYG0ZJJZmOtYGBW/726etXuoVi6Dbsxl4oIZD4VkdSojb7Zp5gmax19BP0YZRgdthTO52KNO3y/Fdoro9U/m198PvYv8MDs/cPsqLcDtr7XBA6vbxtdpN7jHESu7zSbeH3JR1Vip1ZI5+ENXbZVYjt59wbRnk2jKda4OCf/R3n3be1V+OQRW2e0B9pN3RUPQ9OFQxx9vLwyVoFnsN/U5lGBU5PukgVXLpdcfbyLZw/cT8rv++TEhcO06LNDwl22A8uIE2vkyuysXUju/yabMf0lK7oqmVl2OhOc/EF2lDEzS7OzE7ZJwynWyDgpe7Hx7u1U/SsIg8smeHrBYCboODkck9AW6DGrZbniBa7EUTZqd4GwzKntmhSF/K4/tEt7oDk7sDVk84RZVmZUVbOxwQPbRHFyl3cUUJPOR3WD2ky2Uq3BaaM+XFWGhCD+3ZCdyWQbgt0+E2KPi1/7r58qA73V4d7vCHVA5lwYPxeT2Bb4MaanZ7gbi9ZsLrlG+DMScmdtM5qrg7mNhVku398Lt4cLcnrh2nRZqdAm4wHjT7mG+rMjmvp/NtISv1t49etHk9JM3JHTqR58jsJ4RbBtmlTCfcoODd+vF6re/EQw0ze2B6xLsxOBiZ1u214RI08wTNYq/hIGtGATcYc8LoNsTN1P3y+eIJo+vF4fDwdFpn1eFoa4cD8mf28bWaiYrueNzvsHsohjhztdfm9pA6+eGpzGpk9xPGLYPsUqYzblCwWn/arLULc4Y1+Ik9AG5yZkdD4e14KGFmN2vmCZrFXkOPWxtGRWZPOkMVy8Aa3nSG6nDxpNnV35d4eGp2VhaOtnY4IH5ejw5Rjd8HX5F7fNroh7z4bKYduTY0Z8WLShidloVz2QndlkG6LdPpNih40/+21c9owRps9AOyU8itOTvaBiWMeIEi5vQEtC0rp0DWjKJtMCZ9VreBbbA7WL3HuNXgc70qXELaWpqUFW3tcDw+p0dsW3xrXpG7fNrqh9RUWdbk2j58yJzykl0kO7L6CduWQbQp09k2KHi1/nZz9XoxCajCZg94mygmAYeC5Z2JhMzqCXQb1NCNuWrS7OwUVRx0Ylq3IXCwO1jDAwQuAwhcQuZampcVbe1wQO73uDic+PgU3+jTfg/F4YqqUSq+PqfOiQrPQxP0+wk9l0F6LtPpOSh42z/012AnPgGey0IlMel3OzwHJczvCfAc1FC/15PLeArPwaB0crfBc6bu74drJyfxhCpw47+9dDWrAocDwj248cXIlBfp6ZTccEtXZaV9eBaaMy+xmYxTctkJJZdBSi7TKTkoeNV/XaNXa2ZGLhuQIWFnNBTfbk/A5OyaeYJmsdfwD1EyisnBmNTQNkjO1P1yuHryqRxAcnB4anSSlRVt7XBAPn2Pa7/VuXwyT4fkQm7qar/hLt0eUpcrW3AcknMnkJyD+FOmQ3JQ8HL7cQ2IWKiBbnehFJb4xhQOhSdv/CfGVoca9mYtQbTYi2ipx2FYYHYYlJkdX3XV7Kbul8PFE2Z3+umpKXlrx1kRb9ZYzjocEM3q0UXyM/HJGb7FJ30+pKso61xZprvnw1PFMt1xPM6d4HEOMlJOx+Og4H/6f6/vgM/NcJzLwEM5HIr4PIGMs2vmCZrFXsNZWEcgr3Mckz+TQ5n+Xg121yd1B8g4px+dmpC4dpwWMaezpHU4Htxpjy5THr8CuiK3+LTPQ1ryRjuAZWjOnMRgQxNavTt34nNIPDmdjIOCV/3dU6/vtEMNeRx3h6TGL5Rf48GI0xPAOKihM3oKGOcmwTjHjk/FQSe8bkPjTN0vny+f8Lr6+zLhErXjtMgpnSStw/Gw18dHp1a1nNPT0biQlqquMu1JfchafEzy0ABn9BMwzkE4yulgHBRc9J/7TzorQzTI5wfaR9aCgkPhl+dQwnyeUPMNaqjNJ2u+OcrEwaB04W6r+Qa7A5MDIk7/fZmSt3acFely0trhgNjlYyaujMHsK3KLT7t8YOKc+u7cYSZOZDTy+QkT5yAT53QmDgre9h83PagMA0VsSj+QPrJgBByMTOkpZd8SRHMsYsv3ybpvjpQ4O8cxJ6Z0yyGoF7bul8Plk8t3UPnNnriWpmU1bpVTehIVN75Uri5F2WY47ncYfqj8tvvnXlvC48pvoQku4U+oOAepOKdTcVDwj5v+0+Z2o/1dzrAKT+2Bi4vPUCR/AOz3hPNSsYjN7QlgnCsmn9YpGAdjTtjddF4q7g7sDsA4/fdlyhVqaV5WtLXDAfHkPr5O+3LHkdfTwbiQF1/V2hmKQ3NWy+13x8E4dwLGOQjGOR2Mg4J3G/CBOlSwmb1Ei3g7GAclbBGfwMXZNYu9ZmJep1wcjEnX8PCa6za3cXFOLez2fvhdzuoJXNwoKXJWp1wcjMdn9TJ6rS5n9XQuLqSmaLxXZ/WQOSc/WXWci3MnXJyDeJTTuTgoWN1/2m4+g2W8/bTUw59SXcabT0vFEmb2BDDOrlnsNZyTcZSLgzGp2S3Hn16Qu0Q3u35a6vC7NHsKFceyshq3ygf2JCpufKVcFldNv8LjfofbA9vmyjpX9+HD4cHaxM6xOHeCxTmIxTkdi4OCD+vtBhZ0hSrm90ARyXfrdjAOSugyPoGMSwi02Iv4CSyOnZuKg1LHW+rEXZAbRXe8ej7qe/D7MiVv7Tgrcn6nxBwMCN+tjy5S1nhp9nRizg1I3KwsGs3sh/YiV+Z2emqqcyfInIPInNOROSj48LC+6e/00jNQhB/YAzNXCaebj0vFEjazJwBzds1ir5l448aOSyWXgz+vm45Lxd3BQh4gc/rvy4TEteO0yKmdlpWD8fjUPibmZpl8ZE8n5kJqMl/4LNPcHvxQSEDW8eNS8xNkLoc0lNOROSjY/Y3+db/d6HbHKmT3HBWWg0Nhu0MJsbtdM8caNq/nk8xcTpk5GJTN6/iyq26H3fXvW3JwXmoO6srZc93SpKxoa4fjoWl9fI1c6cXrdWaLCaMPWZllua8Uo4f2vJbTukhoZPQTZi6HzFyuM3NQ8I/NzfX6dgMW8VCGnX4AguQRinAo/C0blhCnJ2Bzds1ir+ETe04IsHc4pv70DbvrM3QOzjzNQWU4ewZa+vdbjVulZ8382zjf3+o6CdOm828hLTvL7p+7pWkP7UUmX5fnHIDL3YlpIQCX6wAcFFxudPoNCsgzd+7Q1IwGo1+vQBXzbMKxp1BDZ+fJc09zWhgOBuWrcSgDXreVhgsXUHrdgfk5gX9jaVmNW6XXrZXhxlcpU45MgkN+h9VDVrwvc+1T89Aef93+69AAjX7Cv+WQf8t1/g0K3vWP/b/AsQ1QhOfmQP2Ih244FFmFJwBwds08QbPYa3gx95ywXOc45oTNLUjbha375XDx5DIcEHBweLa9lhMusMND0ufq8dXYPVdnws7pnNvwt/e7Jy3tcNPQXijHGIcmaOgT0C2HoFuug25QcLleX/fwHRmUYUsf4J347fBrPBSxdEL1N7tmnqBZ7DX8GJacVn+DMelzteW80gvcHRharfH2fvhd7KLZ09aOkyLnbQq5wXjc7WPIzTViFw3f5NNuDylzTVbWmtuDGxplnc5rv+UnlFsOKbdcp9yg4O0dQl+ghK3UD/BOId6Gw8GI1xOKv0ENY1oTRIu9iL8PzynlBoNSt9uKv5m6Xw4XT67SAeOWkLeWZmU1bhWb5jAgfiQvxut0WSEGjvkdVi+GiT33tVLmcTBDqeyjccgtP4Hccgg85TrkBgVn15v+7lH/+BSK8LQeuB+5UrdTblDCXoRDEZvXEzC3vJzcRqOYG4w5sVS3gW6wO/A6AN1yALolXKGW5mU1bpVeN59sOr5OztVy+y2dcgt5yZxzhbqIP7R7zescc8tPMLccAky5jrlBwdv+7m6tuu0Mi9jEHpgo6XY0GN+CSyDdoIbO7QmoWz5G3QrvhOHpGagw5oThbbAbvld0w8f01WB4ALvZE9fStKxoa4fjwS24aky+VPIVWTrmNmRrd/ErDXML7YVyEEvOz07NTzC3HNJLuY65QcGr/lb9m5xhCTP7Ad6Rn6rAwfBXqFDCnJ6AuNk1i72Gf4SaU8INxqRreAuydoG7g3kdHJyq/75MSFs7Toq0OT04FcbjT+zj01NnEmCH436H1+vB641a2TW077wuTi3POeWWn1BuOcSdcp1yg4J5v/26BmZHGmb2Bj2y2zE3KGFmT8Dc7JrFXsO/Vskp5gZjUrMjkY6zwu7gFTo4PDUHkBscni7iKeXGWjsckLs9otzkWclw3O9we6DcCp+rxSUGO9SyZHvOKTd/Qrl5iDvlOuUGBV3/ef2kb8VDDXG7H8plxW6Hg2G3QwlxO9SwRXyCaLEX8VW8J0XQznFQ5nco0tfwsLvudw9Kw3lQGs5+gdpxUoTdWco6HA+t4cfXyGWVqAAJh5x2eshKthvVayenhnbv5bwemqDTTzA3DzE3r2NuUNCtd7/oRjcjbj5UhhOTOhyK2DwBcbNr5lhDXZ5NvUf3tDQcDEpdbgPjYHfgclAYzscnfA4ut+etpVlZ0dYOB4Rbc+OrtHOG+EoF3+LTPg95yXyWe2VGD+0+l+/XPSfjvDvxOSSevE7GQcG7/vNmC2Z0CA1Box9yKk9bgkMRoydwcXbNPEGz2Gv46t1TLA7G5Hty+KrrTrdhceHiyfncgfk8AYvzBHxb0aR1OB5dvo8vlSslG4dv82mzH4ZtGldpz+pDRitlTnfc6ydwnIdwnNfhOCh4f6P69gwrsNMDJiRX7hA/g9Q6lLD3bTgOsXpKbTg/ZuOUI1g8rQ0Hg9I53UbGwe76BypePRX1Pfh9mZDrdpwUOaXT0nAwHly559HKXZzUgO/waZMPxFw1K5zyYv3ZCrLYq+el4fwJMechOuV1Yg4KXj5cr9GMbsbl/IECKkWpVzgUmdFT6sLhvybxuT3QYi+amNMpMAeDTszpNmQOdgerd4DMeYDM2bPdjtMinU5aOxwPL959tHgXcCy+yaetPuByxcxpx6CHdl/JF20i05HVT3A5D3E5r+NyUPCu39ys1+Ax3VwTzgeyShRwh0MRryfgcnbNPEGz2Gsm9uIoLAdjTjjdhsuZul8OF08+p6u/LxMS19K0rMat0ulmWm58mVwmD2TBt/i00wPytht2ppWEC+2+lGCs57ScP6HlPKSmvE7LQcGHLzc3+umJUML23QNvJVfvdloOSpjTE2A5u2ax1/BX6p7CcvhycKcjmf6eDXYHTgewnAewHBye7sjRsnAsax0OiK0+huWyUu7IpcNyfoDlymY3smL1AMvVspKE57CcP4HlPASgvA7LQcHr29v+t/XNDVjCmw9L9YEOErAcHIrCclDF/J4Cy9kDLfYaXk3CU1gOxqQP65bjTy9wd7CEB6el6r8vE9LWjpMiJ3aKysF4fFtuXBZOrMGuyI0+7fjAy2Wzb35XHB/KwmXKEzvn5fwJL+chOuV1Xg4KLvunr/2nzZcH3fEJzJwPlJD4yg0ORib4BGbOrpknaBZ7Df8+3dPzUmHMiQneRs3B7mCCB3XhPKDm7IlraVpWtLXD8eD2XFQWrpBmTwfm/HNZuFmhvnB7Lgsnp3d+kqo/AeY8BOa8DsxBwav+qb/VP3uBGuZ0VBcODkacngDM2TXzBM1ir2Ez+zkedcLLtsNSYXcwe8fHdw5eVmG5ZUJq2kGDvEyZOBiPz95jJq6o5Xo9nYkbUjYrvffaJtyh3c9ONuHGxi1O2LcCsm9eZ9+g4PX20636BusMa+CivDgwPRJqh0Nh20IJsa1dM0/QLPYa/gReUO4NxuSmhjLd1LC7bupCJd+WCQlqx399sfRmyelwPPigPb4czufiC1R8K08atxgqueXFTKvkNqStkehLwRG34gRxKyDiVuiIGxS82m4eHgHkBkVkKi4C5iaKMcPBiKcTMDeoYU/ZCaLFXsQfswtSzewcB2WP2VAEPG07ALUAnFsBDkBNyFs7zoqYqVlrhwNis0ecmyy9Dsf8DrMPnFvjZ9osHdp9oZg9RgcPZv/p4Xq9fnzVP/Z/+8sf/e/ri377++bu4Yeb9T93f8bZi+rHH7a7v+rw34/3f+z/q/jxh9/uHx/vb4f/db3ud0uXb/8r//GHf97fPw7/46ddjD/vt5/3cf72/wNQSwMEFAAAAAgAAAAhAIMYaiVIAQAAJgIAAA8AAAB4bC93b3JrYm9vay54bWyNUctOwzAQvPMV1t5pHmojWjWpxEtUQoBEac8m3jRWHTuyHdL+PetUKXDjtDPj3dHOerk6Nop9oXXS6BySSQwMdWmE1PscPjaP1zfAnOdacGU05nBCB6viatkbe/g05sBoXrscau/bRRS5ssaGu4lpUdNLZWzDPVG7j1xrkQtXI/pGRWkcZ1HDpYazw8L+x8NUlSzx3pRdg9qfTSwq7ml7V8vWQbGspMLtORDjbfvCG1r7qIAp7vyDkB5FDlOipsc/gu3a206qQGbxDKLiEvLNMoEV75Tf0GqjO50rnaZpFjpD11Zi736GAmXHndTC9DmkU7rsaWTJDFg/4J0UviYhi+cX7QnlvvY5zLMsDubRL/fhfmNlegj3HnBC/xTqmvYnbBeSgF2LZHAYx0quSkoTytCYTmfJHFjVKXVH2qt+NnwwCENjkuIbUEsDBBQAAAAIAAAAIQA/2O8hsQUAAFMbAAATAAAAeGwvdGhlbWUvdGhlbWUxLnhtbO1ZTY/TRhi+8ytGvoPjxA7ZFVm0ySbQwsJqN1BxnNgTe8jYY81MdsmtgmOlSlVp1Uul3nqo2iKB1Av9NdtStVTiL/T1R5LxZrJkYasWQQ6JZ/y83x9+x7ly9UHM0CERkvKkbTmXahYiic8DmoRt686gf7FlIalwEmDGE9K2pkRaV7cuXMGbKiIxQUCeyE3ctiKl0k3blj5sY3mJpySBeyMuYqxgKUI7EPgI2MbMrtdqTTvGNLFQgmPgens0oj5Bg4yltTVj3mPwlSiZbfhMHPi5RJ0ixwZjJ/uRU9llAh1i1rZATsCPBuSBshDDUsGNtlXLP5a9dcWeEzG1glaj6+efkq4kCMb1nE6Ewzmh03c3Lu/M+dcL/su4Xq/X7TlzfjkA+z5Y6ixh3X7L6cx4aqDicpl3t+bV3Cpe499Ywm90Oh1vo4JvLPDuEr5Va7rb9QreXeC9Zf07291us4L3FvjmEr5/eaPpVvE5KGI0GS+hs3jOIzOHjDi7boS3AN6aJcACZWvZVdAnalWuxfg+F30A5MHFiiZITVMywj7gujgeCoozAXiTYO1OseXLpa1MFpK+oKlqWx+nGCpiAXn1/MdXz5+iV8+fHD98dvzwl+NHj44f/mwgvI6TUCd8+f0Xf3/7Kfrr6XcvH39lxksd//tPn/3265dmoNKBL75+8sezJy+++fzPHx4b4NsCD3X4gMZEolvkCO3zGGwzCCBDcTaKQYRphQJHgDQAeyqqAG9NMTPhOqTqvLsCGoAJeG1yv6LrQSQmihqAN6K4AtzlnHW4MJpzI5OlmzNJQrNwMdFx+xgfmmR3T4S2N0khk6mJZTciFTX3GEQbhyQhCmX3+JgQA9k9Sit+3aW+4JKPFLpHUQdTo0sGdKjMRNdpDHGZmhSEUFd8s3sXdTgzsd8hh1UkFARmJpaEVdx4DU8Ujo0a45jpyJtYRSYlD6bCrzhcKoh0SBhHvYBIaaK5LaYVdW9g6ETGsO+yaVxFCkXHJuRNzLmO3OHjboTj1KgzTSId+5EcQ4pitMeVUQlerZBsDXHAycpw36VEna2s79AwMidIdmciyq5d6b8xTU5rxoxCN/7QjGfwbXg0mUriZAtehXsHG+8OniR7BHL9Q9/90Hffx767qpbX7baLBmvrc3HOL145JI8oYwdqyshNmbdmCUoHfdjMFznRfCZPI7gsxVVwocD5NRJcfUJVdBDhFMQ4uYRQlqxDiVIu4SRgreSdHycpGJ/vebMzIKCx2uVBsd3Qz4ZzNvkqlLqgRsZgXWGNy28nzCmAa0pzPLM071RptuZNqAaEs4O/06wXoiFjMCNB5veCwSws5x4iGeGAlDFyjIY4jTXd1nq91zRpG423k7ZOkHRx7gpx3jlEqbYUJXu5HFlSXaEj0Mqrexbycdq2RjBJwWWcAj+ZNSDMwqRt+ao05bXFfNJgc1o6tZUGV0SkQqodLKOCKr81e3WSLPSve27mh/MxwNCN1tOi0XL+Qy3sk6EloxHx1YqdxbK8xyeKiIMoOEJDNhH7GPR2i+wKqIRnRn22EFChbpl41covq+DkK5qyOjBLI1z2pJYW+wKeX891yFeaevYK3d/QlMY5muK9v6ZkmQtjayPID1QwBgiMshxtW1yoiEMXSiPq9wUMDrks0AtBWWQqIZa9b850JYeLvlXwKJpcGKl9GiJBodOpSBCyp0o7X8PMqevP1xmjss/M1ZVp8Tskh4QNsuptZvZbKJp1k9IROe5k0GxTdQ3D/v948nFXTD6njwcLQe5ZZhFXa/rao2Dj7VQ446O2bra47q39qE3h8IGyL2jcVPhsMd8O+D5EH80nSgSJeLFVlt98cwg6tzTjMlb/7hi1CEFrRbzPc/jUnN1Y4ezTxb25sz2Dr73TXW0vl6itHWTy1dIfT3x4H2TvwEFpwpQs3iY9gKNmd/aXAfCxF6RbF/4BUEsDBBQAAAAIAAAAIQDLCA6V/QEAAAIFAAANAAAAeGwvc3R5bGVzLnhtbL1UTYvbMBC991cI3bOK0za0wfZSAmYL7VLYLPQqW7It0IeR5GDvr+/IchxnadnSQ3OwZp5m3sxknp3eD0qiM7dOGJ3h5G6LEdeVYUI3GX4+FZtPGDlPNaPSaJ7hkTt8n79LnR8lf2o59wgYtMtw6313IMRVLVfU3ZmOa7ipjVXUg2sb4jrLKXMhSUmy2273RFGhcZ7qXhXKO1SZXntoY4FQPL4yAPcfMIp0R8NCK/DbKLVhDD08HJQ6OIeeT0dM8pTMhHlaG33l3eEI5Kl7QWcqgTQJ4ZWRxiIPjfNQHBBNFY8RRypFaUUAa6qEHCO8C8A06xynhDZ2qh0rxGdJ/ket6QjDCilvhwUgTzvqPbe6AAfN9mnsoLyGlUaaKe6N6MbSMdl9XCVMB9QtjWUgofX6IpSnktceEqxo2nB605Fw6b1RYDBBG6OpDJSXjNkA2opL+RR09rO+4R7qlSy2QRR6MaGh2Yw00Qn8a7bIvaJ9/0+0aKgX/j9lJ29nI9p1cixMnC96X6RotOKXkenFRa2x4gVCgw4qAHgUwlC/amF+Xf56hFj2sVclt8X0ls3yJPNftdrHzTYWFAUhZ/gxJMsVcdkL6YX+zSaAkw3XJUy3npbwYbmpAhyM17SX/rRcZvhqf+dM9OrzEvVDnI2fo672tyDBZD91cP165b8AUEsDBBQAAAAIAAAAIQCyRZ1jJQEAAFACAAARAAAAZG9jUHJvcHMvY29yZS54bWydks1uwjAQhO99isj3xHEoLbKSILUVpyJVKlVRb5a9gNX4R7bbkLevEyCAxKnH9cx+O7tyOd+rJvkF56XRFSJZjhLQ3AiptxX6WC3SGUp8YFqwxmioUAcezeu7klvKjYM3Zyy4IMEnEaQ95bZCuxAsxdjzHSjms+jQUdwYp1iIpdtiy/g32wIu8vwBKwhMsMBwD0ztSERHpOAj0v64ZgAIjqEBBTp4TDKCz94ATvmbDYNy4VQydBZuWk/i6N57ORrbts3ayWCN+QleL1/fh1VTqftTcUB1KTjlDlgwri7xZREP1zAflvHEGwniqYv6jbfjIoc+EEkMQA9xT8rn5PlltUB1kRd5mhcpma3IlN4/0in56kde9Z+B6jjk38QT4JD7+hPUf1BLAwQUAAAACAAAACEAXrqn03cBAAAQAwAAEAAAAGRvY1Byb3BzL2FwcC54bWydksFO6zAQRfd8ReQ9dVIh9FQ5RqiAWPBEpRZYG2fSWDi25Rmilq/HSdWQAiuyujNzdX0ytrjatTbrIKLxrmTFLGcZOO0r47Yle9rcnf9jGZJylbLeQcn2gOxKnolV9AEiGcAsJTgsWUMUFpyjbqBVOEtjlya1j62iVMYt93VtNNx4/d6CIz7P80sOOwJXQXUexkB2SFx09NfQyuueD583+5DypLgOwRqtKP2k/G909Ohrym53Gqzg06FIQWvQ79HQXuaCT0ux1srCMgXLWlkEwb8a4h5Uv7OVMhGl6GjRgSYfMzQfaWtzlr0qhB6nZJ2KRjliB9uhGLQNSFG++PiGDQCh4GNzkFPvVJsLWQyGJE6NfARJ+hRxY8gCPtYrFekX4mJKPDCwCeO65yt+8B1P+pa99G1QLi2Qj+rBuDd8Cht/owiO6zxtinWjIlTpBsZ1jw1xn7ii7f3LRrktVEfPz0F/+c+HBy6L+SxP33Dnx57gX29ZfgJQSwECAAAUAAAACAAAACEAOJ2G2D4BAAAHBAAAEwAAAAAAAAABAAAAAAAAAAAAW0NvbnRlbnRfVHlwZXNdLnhtbFBLAQIAABQAAAAIAAAAIQDyn0na6QAAAEsCAAALAAAAAAAAAAEAAAAAAG8BAABfcmVscy8ucmVsc1BLAQIAABQAAAAIAAAAIQDlRBuj1QAAACwCAAAaAAAAAAAAAAEAAAAAAIECAAB4bC9fcmVscy93b3JrYm9vay54bWwucmVsc1BLAQIAABQAAAAIAAAAIQCVJDlWGWwAAK7NAwAYAAAAAAAAAAEAAAAAAI4DAAB4bC93b3Jrc2hlZXRzL3NoZWV0MS54bWxQSwECAAAUAAAACAAAACEAgxhqJUgBAAAmAgAADwAAAAAAAAABAAAAAADdbwAAeGwvd29ya2Jvb2sueG1sUEsBAgAAFAAAAAgAAAAhAD/Y7yGxBQAAUxsAABMAAAAAAAAAAQAAAAAAUnEAAHhsL3RoZW1lL3RoZW1lMS54bWxQSwECAAAUAAAACAAAACEAywgOlf0BAAACBQAADQAAAAAAAAABAAAAAAA0dwAAeGwvc3R5bGVzLnhtbFBLAQIAABQAAAAIAAAAIQCyRZ1jJQEAAFACAAARAAAAAAAAAAEAAAAAAFx5AABkb2NQcm9wcy9jb3JlLnhtbFBLAQIAABQAAAAIAAAAIQBeuqfTdwEAABADAAAQAAAAAAAAAAEAAAAAALB6AABkb2NQcm9wcy9hcHAueG1sUEsFBgAAAAAJAAkAPgIAAFV8AAAAAA==" download="histopathology-template2020-02-18.xlsx.xlsx">
<button class="btn btn-primary"><i class="fa fa-save"></i> Download data as xlsx</button>
</a><!--/html_preserve-->








---


\pagebreak


### Final Data Summary {.appendix}  


```r
# use summarytools to generate final data summary
# summarytools::view(summarytools::dfSummary(x = mydata
#                                            , style = "markdown"))
```





---

\pagebreak



### Software and Libraries Used {.appendix}  

Why and how to cite software and packages?^[Smith AM, Katz DS, Niemeyer KE, FORCE11 Software Citation Working Group. (2016) Software Citation Principles. PeerJ Computer Science 2:e86. DOI: 10.7717/peerj-cs.86 https://www.force11.org/software-citation-principles]



```r
citation()
```

```

To cite R in publications use:

  R Core Team (2019). R: A language and environment for statistical
  computing. R Foundation for Statistical Computing, Vienna, Austria.
  URL https://www.R-project.org/.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {R: A Language and Environment for Statistical Computing},
    author = {{R Core Team}},
    organization = {R Foundation for Statistical Computing},
    address = {Vienna, Austria},
    year = {2019},
    url = {https://www.R-project.org/},
  }

We have invested a lot of time and effort in creating R, please cite it
when using it for data analysis. See also 'citation("pkgname")' for
citing R packages.
```


The jamovi project (2019). jamovi. (Version 0.9) [Computer Software]. Retrieved from https://www.jamovi.org.  
R Core Team (2018). R: A Language and envionment for statistical computing. [Computer software]. Retrieved from https://cran.r-project.org/.  
Fox, J., & Weisberg, S. (2018). car: Companion to Applied Regression. [R package]. Retrieved from https://cran.r-project.org/package=car.
Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686
Data processing was carried out with R (R Core Team, 2019) and the easystats ecosystem (Lüdecke, Waggoner, & Makowski, 2019; Makowski, Ben-Shachar, & Lüdecke, 2019)




```r
report::cite_packages(session = sessionInfo())
```

References                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Alastair Rushworth (2019). inspectdf: Inspection, Comparison and Visualisation of Data Frames. R package version 0.0.7. https://CRAN.R-project.org/package=inspectdf                                                                                                                                                                                                                                                                                                                        
Alboukadel Kassambara (2020). ggpubr: 'ggplot2' Based Publication Ready Plots. R package version 0.2.5. https://CRAN.R-project.org/package=ggpubr                                                                                                                                                                                                                                                                                                                                           
Alboukadel Kassambara, Marcin Kosinski and Przemyslaw Biecek (2019). survminer: Drawing Survival Curves using 'ggplot2'. R package version 0.4.6. https://CRAN.R-project.org/package=survminer                                                                                                                                                                                                                                                                                              
Benjamin Elbers (2020). tidylog: Logging for 'dplyr' and 'tidyr' Functions. R package version 1.0.0. https://CRAN.R-project.org/package=tidylog                                                                                                                                                                                                                                                                                                                                             
Boxuan Cui (2020). DataExplorer: Automate Data Exploration and Treatment. R package version 0.8.1. https://CRAN.R-project.org/package=DataExplorer                                                                                                                                                                                                                                                                                                                                          
Chung-hong Chan, Geoffrey CH Chan, Thomas J. Leeper, and Jason Becker (2018). rio: A Swiss-army knife for data file I/O. R package version 0.5.16.                                                                                                                                                                                                                                                                                                                                          
David Robinson and Alex Hayes (2020). broom: Convert Statistical Analysis Objects into Tidy Tibbles. R package version 0.5.4. https://CRAN.R-project.org/package=broom                                                                                                                                                                                                                                                                                                                      
Dayanand Ubrangala, Kiran R, Ravi Prasad Kondapalli and Sayan Putatunda (2020). SmartEDA: Summarize and Explore the Data. R package version 0.3.3. https://CRAN.R-project.org/package=SmartEDA                                                                                                                                                                                                                                                                                              
Dirk Eddelbuettel and Romain Francois (2011). Rcpp: Seamless R and C++ Integration. Journal of Statistical Software, 40(8), 1-18. URL http://www.jstatsoft.org/v40/i08/.                                                                                                                                                                                                                                                                                                                    
Dirk Eddelbuettel with contributions by Antoine Lucas, Jarek Tuszynski, Henrik Bengtsson, Simon Urbanek, Mario Frasca, Bryan Lewis, Murray Stokely, Hannes Muehleisen, Duncan Murdoch, Jim Hester, Wush Wu, Qiang Kou, Thierry Onkelinx, Michel Lang, Viliam Simko, Kurt Hornik, Radford Neal, Kendon Bell, Matthew de Queljoe, Ion Suruceanu and Bill Denney. (2020). digest: Create Compact Hash Digests of R Objects. R package version 0.6.24. https://CRAN.R-project.org/package=digest
Ethan Heinzen, Jason Sinnwell, Elizabeth Atkinson, Tina Gunderson and Gregory Dougherty (2020). arsenal: An Arsenal of 'R' Functions for Large-Scale Statistical Summaries. R package version 3.4.0. https://CRAN.R-project.org/package=arsenal                                                                                                                                                                                                                                             
Ewen Harrison, Tom Drake and Riinu Ots (2019). finalfit: Quickly Create Elegant Regression Results Tables and Plots when Modelling. R package version 0.9.7. https://CRAN.R-project.org/package=finalfit                                                                                                                                                                                                                                                                                    
Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of Statistical Software, 40(3), 1-25. URL http://www.jstatsoft.org/v40/i03/.                                                                                                                                                                                                                                                                                                                    
H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.                                                                                                                                                                                                                                                                                                                                                                                                    
Hadley Wickham (2019). feather: R Bindings to the Feather 'API'. R package version 0.3.5. https://CRAN.R-project.org/package=feather                                                                                                                                                                                                                                                                                                                                                        
Hadley Wickham (2019). forcats: Tools for Working with Categorical Variables (Factors). R package version 0.4.0. https://CRAN.R-project.org/package=forcats                                                                                                                                                                                                                                                                                                                                 
Hadley Wickham (2019). httr: Tools for Working with URLs and HTTP. R package version 1.4.1. https://CRAN.R-project.org/package=httr                                                                                                                                                                                                                                                                                                                                                         
Hadley Wickham (2019). modelr: Modelling Functions that Work with the Pipe. R package version 0.1.5. https://CRAN.R-project.org/package=modelr                                                                                                                                                                                                                                                                                                                                              
Hadley Wickham (2019). rvest: Easily Harvest (Scrape) Web Pages. R package version 0.3.5. https://CRAN.R-project.org/package=rvest                                                                                                                                                                                                                                                                                                                                                          
Hadley Wickham (2019). stringr: Simple, Consistent Wrappers for Common String Operations. R package version 1.4.0. https://CRAN.R-project.org/package=stringr                                                                                                                                                                                                                                                                                                                               
Hadley Wickham and Evan Miller (2019). haven: Import and Export 'SPSS', 'Stata' and 'SAS' Files. R package version 2.2.0. https://CRAN.R-project.org/package=haven                                                                                                                                                                                                                                                                                                                          
Hadley Wickham and Jennifer Bryan (2019). readxl: Read Excel Files. R package version 1.3.1. https://CRAN.R-project.org/package=readxl                                                                                                                                                                                                                                                                                                                                                      
Hadley Wickham and Lionel Henry (2020). tidyr: Tidy Messy Data. R package version 1.0.2. https://CRAN.R-project.org/package=tidyr                                                                                                                                                                                                                                                                                                                                                           
Hadley Wickham and Yihui Xie (2019). evaluate: Parsing and Evaluation Tools that Provide More Details than the Default. R package version 0.14. https://CRAN.R-project.org/package=evaluate                                                                                                                                                                                                                                                                                                 
Hadley Wickham, Jim Hester and Jeroen Ooms (2019). xml2: Parse XML. R package version 1.2.2. https://CRAN.R-project.org/package=xml2                                                                                                                                                                                                                                                                                                                                                        
Hadley Wickham, Jim Hester and Romain Francois (2018). readr: Read Rectangular Text Data. R package version 1.3.1. https://CRAN.R-project.org/package=readr                                                                                                                                                                                                                                                                                                                                 
Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2020). dplyr: A Grammar of Data Manipulation. R package version 0.8.4. https://CRAN.R-project.org/package=dplyr                                                                                                                                                                                                                                                                                                            
Jeremy Stephens, Kirill Simonov, Yihui Xie, Zhuoer Dong, Hadley Wickham, Jeffrey Horner, reikoch, Will Beasley, Brendan O'Connor and Gregory R. Warnes (2020). yaml: Methods to Convert R Data to YAML and Back. R package version 2.2.1. https://CRAN.R-project.org/package=yaml                                                                                                                                                                                                           
Jeroen Ooms (2014). The jsonlite Package: A Practical and Consistent Mapping Between JSON Data and R Objects. arXiv:1403.2805 [stat.CO] URL https://arxiv.org/abs/1403.2805.                                                                                                                                                                                                                                                                                                                
Jim Hester (2019). glue: Interpreted String Literals. R package version 1.3.1. https://CRAN.R-project.org/package=glue                                                                                                                                                                                                                                                                                                                                                                      
Jim Hester and Gábor Csárdi (2019). pak: Another Approach to Package Installation. R package version 0.1.2. https://CRAN.R-project.org/package=pak                                                                                                                                                                                                                                                                                                                                          
Jim Hester, Gábor Csárdi, Hadley Wickham, Winston Chang, Martin Morgan and Dan Tenenbaum (2020). remotes: R Package Installation from Remote Repositories, Including 'GitHub'. R package version 2.1.1. https://CRAN.R-project.org/package=remotes                                                                                                                                                                                                                                          
JJ Allaire and Yihui Xie and Jonathan McPherson and Javier Luraschi and Kevin Ushey and Aron Atkins and Hadley Wickham and Joe Cheng and Winston Chang and Richard Iannone (2020). rmarkdown: Dynamic Documents for R. R package version 2.1. URL https://rmarkdown.rstudio.com.                                                                                                                                                                                                            
JJ Allaire, Jeffrey Horner, Yihui Xie, Vicent Marti and Natacha Porte (2019). markdown: Render Markdown with the C Library 'Sundown'. R package version 1.1. https://CRAN.R-project.org/package=markdown                                                                                                                                                                                                                                                                                    
Kazuki Yoshida (2019). tableone: Create 'Table 1' to Describe Baseline Characteristics. R package version 0.10.0. https://CRAN.R-project.org/package=tableone                                                                                                                                                                                                                                                                                                                               
Kevin Ushey (2020). renv: Project Environments. R package version 0.9.3. https://CRAN.R-project.org/package=renv                                                                                                                                                                                                                                                                                                                                                                            
Kirill Müller (2017). here: A Simpler Way to Find Your Files. R package version 0.1. https://CRAN.R-project.org/package=here                                                                                                                                                                                                                                                                                                                                                                
Kirill Müller (2020). hms: Pretty Time of Day. R package version 0.5.3. https://CRAN.R-project.org/package=hms                                                                                                                                                                                                                                                                                                                                                                              
Kirill Müller and Hadley Wickham (2019). tibble: Simple Data Frames. R package version 2.1.3. https://CRAN.R-project.org/package=tibble                                                                                                                                                                                                                                                                                                                                                     
Koji Makiyama (2016). magicfor: Magic Functions to Obtain Results from for Loops. R package version 0.1.0. https://CRAN.R-project.org/package=magicfor                                                                                                                                                                                                                                                                                                                                      
Lionel Henry and Hadley Wickham (2019). purrr: Functional Programming Tools. R package version 0.3.3. https://CRAN.R-project.org/package=purrr                                                                                                                                                                                                                                                                                                                                              
Lionel Henry and Hadley Wickham (2020). rlang: Functions for Base Types and Core R and 'Tidyverse' Features. R package version 0.4.4. https://CRAN.R-project.org/package=rlang                                                                                                                                                                                                                                                                                                              
Makowski, D. & Lüdecke, D. (2019). The report package for R: Ensuring the use of best practices for results reporting. CRAN. Available from https://github.com/easystats/report. doi: .                                                                                                                                                                                                                                                                                                     
Pablo Seibelt (2017). xray: X Ray Vision on your Datasets. R package version 0.2. https://CRAN.R-project.org/package=xray                                                                                                                                                                                                                                                                                                                                                                   
Paul Hendricks (2015). describer: Describe Data in R Using Common Descriptive Statistics. R package version 0.2.0. https://CRAN.R-project.org/package=describer                                                                                                                                                                                                                                                                                                                             
Petersen AH, Ekstrøm CT (2019). "dataMaid: Your Assistant forDocumenting Supervised Data Quality Screening in R." _Journal ofStatistical Software_, *90*(6), 1-38. doi: 10.18637/jss.v090.i06 (URL:https://doi.org/10.18637/jss.v090.i06).                                                                                                                                                                                                                                                  
Rinker, T. W. (2018). wakefield: Generate Random Data. version 0.3.3. Buffalo, New York. https://github.com/trinker/wakefield                                                                                                                                                                                                                                                                                                                                                               
Rinker, T. W. & Kurkiewicz, D. (2017). pacman: Package Management for R. version 0.5.0. Buffalo, New York. http://github.com/trinker/pacman                                                                                                                                                                                                                                                                                                                                                 
Roland Krasser (2020). explore: Simplifies Exploratory Data Analysis. R package version 0.5.4. https://CRAN.R-project.org/package=explore                                                                                                                                                                                                                                                                                                                                                   
RStudio and Inc. (2019). htmltools: Tools for HTML. R package version 0.4.0. https://CRAN.R-project.org/package=htmltools                                                                                                                                                                                                                                                                                                                                                                   
Sam Firke (2020). janitor: Simple Tools for Examining and Cleaning Dirty Data. R package version 1.2.1. https://CRAN.R-project.org/package=janitor                                                                                                                                                                                                                                                                                                                                          
Simon Garnier (2018). viridis: Default Color Maps from 'matplotlib'. R package version 0.5.1. https://CRAN.R-project.org/package=viridis                                                                                                                                                                                                                                                                                                                                                    
Simon Garnier (2018). viridisLite: Default Color Maps from 'matplotlib' (Lite Version). R package version 0.3.0. https://CRAN.R-project.org/package=viridisLite                                                                                                                                                                                                                                                                                                                             
Simon Urbanek (2015). base64enc: Tools for base64 encoding. R package version 0.1-3. https://CRAN.R-project.org/package=base64enc                                                                                                                                                                                                                                                                                                                                                           
Stefan Milton Bache and Hadley Wickham (2014). magrittr: A Forward-Pipe Operator for R. R package version 1.5. https://CRAN.R-project.org/package=magrittr                                                                                                                                                                                                                                                                                                                                  
Therneau T (2015). _A Package for Survival Analysis in S_. version2.38, <URL: https://CRAN.R-project.org/package=survival>.                                                                                                                                                                                                                                                                                                                                                                 
Tierney N (2017). "visdat: Visualising Whole Data Frames." _JOSS_,*2*(16), 355. doi: 10.21105/joss.00355 (URL:https://doi.org/10.21105/joss.00355), <URL:http://dx.doi.org/10.21105/joss.00355>.                                                                                                                                                                                                                                                                                            
Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2019). shiny: Web Application Framework for R. R package version 1.4.0. https://CRAN.R-project.org/package=shiny                                                                                                                                                                                                                                                                                                    
Yihui Xie (2019). formatR: Format R Code Automatically. R package version 1.7. https://CRAN.R-project.org/package=formatR                                                                                                                                                                                                                                                                                                                                                                   
Yihui Xie (2020). knitr: A General-Purpose Package for Dynamic Report Generation in R. R package version 1.28.                                                                                                                                                                                                                                                                                                                                                                              
Yihui Xie (2020). mime: Map Filenames to MIME Types. R package version 0.9. https://CRAN.R-project.org/package=mime                                                                                                                                                                                                                                                                                                                                                                         
Yixuan Qiu and Yihui Xie (2019). highr: Syntax Highlighting for R Source Code. R package version 0.8. https://CRAN.R-project.org/package=highr                                                                                                                                                                                                                                                                                                                                              



```r
report::show_packages(session = sessionInfo()) %>% kableExtra::kable()
```



```r
# citation('tidyverse')
citation("readxl")
```

```

To cite package 'readxl' in publications use:

  Hadley Wickham and Jennifer Bryan (2019). readxl: Read Excel Files. R
  package version 1.3.1. https://CRAN.R-project.org/package=readxl

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {readxl: Read Excel Files},
    author = {Hadley Wickham and Jennifer Bryan},
    year = {2019},
    note = {R package version 1.3.1},
    url = {https://CRAN.R-project.org/package=readxl},
  }
```

```r
citation("janitor")
```

```

To cite package 'janitor' in publications use:

  Sam Firke (2020). janitor: Simple Tools for Examining and Cleaning
  Dirty Data. R package version 1.2.1.
  https://CRAN.R-project.org/package=janitor

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {janitor: Simple Tools for Examining and Cleaning Dirty Data},
    author = {Sam Firke},
    year = {2020},
    note = {R package version 1.2.1},
    url = {https://CRAN.R-project.org/package=janitor},
  }
```

```r
# citation('report')
citation("finalfit")
```

```

To cite package 'finalfit' in publications use:

  Ewen Harrison, Tom Drake and Riinu Ots (2019). finalfit: Quickly
  Create Elegant Regression Results Tables and Plots when Modelling. R
  package version 0.9.7. https://CRAN.R-project.org/package=finalfit

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {finalfit: Quickly Create Elegant Regression Results Tables and Plots when
Modelling},
    author = {Ewen Harrison and Tom Drake and Riinu Ots},
    year = {2019},
    note = {R package version 0.9.7},
    url = {https://CRAN.R-project.org/package=finalfit},
  }
```

```r
# citation('ggstatsplot')
```



```r
if (!dir.exists(here::here("bib"))) {
    dir.create(here::here("bib"))
}

knitr::write_bib(x = c(.packages(), "knitr", "shiny"), file = here::here("bib", "packages.bib"))
```


---

\pagebreak

### Session Info {.appendix} 


```r
sessionInfo()
```

```
R version 3.6.0 (2019-04-26)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS  10.15.3

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods   base     

other attached packages:
 [1] survminer_0.4.6    ggpubr_0.2.5       viridis_0.5.1      viridisLite_0.3.0 
 [5] shiny_1.4.0        survival_3.1-8     magrittr_1.5       report_0.1.0      
 [9] wakefield_0.3.3    SmartEDA_0.3.3     magicfor_0.1.0     tableone_0.10.0   
[13] arsenal_3.4.0      DataExplorer_0.8.1 xray_0.2           visdat_0.5.3      
[17] inspectdf_0.0.7    describer_0.2.0    dataMaid_1.4.0     finalfit_0.9.7    
[21] explore_0.5.4      rio_0.5.16         janitor_1.2.1      formatR_1.7       
[25] renv_0.9.3         rlang_0.4.4        glue_1.3.1         tidylog_1.0.0     
[29] broom_0.5.4        modelr_0.1.5       rvest_0.3.5        xml2_1.2.2        
[33] readxl_1.3.1       httr_1.4.1         haven_2.2.0        feather_0.3.5     
[37] lubridate_1.7.4    hms_0.5.3          forcats_0.4.0      stringr_1.4.0     
[41] tibble_2.1.3       purrr_0.3.3        readr_1.3.1        tidyr_1.0.2       
[45] dplyr_0.8.4        ggplot2_3.2.1      rmarkdown_2.1      mime_0.9          
[49] base64enc_0.1-3    jsonlite_1.6.1     knitr_1.28         htmltools_0.4.0   
[53] Rcpp_1.0.3         yaml_2.2.1         markdown_1.1       highr_0.8         
[57] digest_0.6.24      evaluate_0.14      here_0.1           pak_0.1.2         
[61] pacman_0.5.1       remotes_2.1.1     

loaded via a namespace (and not attached):
  [1] utf8_1.1.4          tidyselect_1.0.0    lme4_1.1-21        
  [4] htmlwidgets_1.5.1   grid_3.6.0          lpSolve_5.6.15     
  [7] munsell_0.5.0       effectsize_0.1.2    codetools_0.2-16   
 [10] DT_0.12.1           withr_2.1.2         ISLR_1.2           
 [13] colorspace_1.4-1    rstudioapi_0.11     robustbase_0.93-5  
 [16] ggsignif_0.6.0      labeling_0.3        KMsurv_0.1-5       
 [19] farver_2.0.3        rprojroot_1.3-2     vctrs_0.2.2        
 [22] generics_0.0.2      xfun_0.12           downloadthis_0.1.0 
 [25] R6_2.4.1            reshape_0.8.8       assertthat_0.2.1   
 [28] promises_1.1.0      networkD3_0.4       scales_1.1.0       
 [31] nnet_7.3-12         gtable_0.3.0        clisymbols_1.2.0   
 [34] whoami_1.3.0        splines_3.6.0       lazyeval_0.2.2     
 [37] acepack_1.4.1       bsplus_0.1.1        checkmate_2.0.0    
 [40] backports_1.1.5     httpuv_1.5.2        Hmisc_4.3-1        
 [43] tools_3.6.0         ellipsis_0.3.0      RColorBrewer_1.1-2 
 [46] plyr_1.8.5          jmvcore_1.2.5       progress_1.2.2     
 [49] prettyunits_1.1.1   rpart_4.1-15        sampling_2.8       
 [52] zoo_1.8-7           reactR_0.4.2        cluster_2.1.0      
 [55] fs_1.3.1            survey_3.37         data.table_1.12.8  
 [58] openxlsx_4.1.4      mitml_0.3-7         reactable_0.1.0    
 [61] xtable_1.8-4        jpeg_0.1-8.1        gridExtra_2.3      
 [64] compiler_3.6.0      mice_3.7.0          writexl_1.2        
 [67] crayon_1.3.4        minqa_1.2.4         later_1.0.0        
 [70] Formula_1.2-3       DBI_1.1.0           jmv_1.2.5          
 [73] MASS_7.3-51.5       boot_1.3-24         Matrix_1.2-18      
 [76] cli_2.0.1           mitools_2.4         parallel_3.6.0     
 [79] insight_0.8.1       pan_1.6             igraph_1.2.4.2     
 [82] pkgconfig_2.0.3     km.ci_0.5-2         foreign_0.8-75     
 [85] foreach_1.4.8       snakecase_0.11.0    parameters_0.5.0.1 
 [88] cellranger_1.1.0    survMisc_0.5.5      htmlTable_1.13.3   
 [91] curl_4.3            jomo_2.6-10         rjson_0.2.20       
 [94] nloptr_1.2.1        lifecycle_0.1.0     nlme_3.1-144       
 [97] fansi_0.4.1         labelled_2.2.2      pillar_1.4.3       
[100] ggsci_2.9           lattice_0.20-38     GGally_1.4.0       
[103] fastmap_1.0.1       DEoptimR_1.0-8      bayestestR_0.5.2   
[106] zip_2.0.4           png_0.1-7           iterators_1.0.12   
[109] pander_0.6.3        performance_0.4.4   class_7.3-15       
[112] stringi_1.4.6       ggfittext_0.8.1     latticeExtra_0.6-29
[115] e1071_1.7-3        
```




\pagebreak

---

### Loaded packages {.appendix} 


```r
pacman::p_loaded(all = TRUE)
```











---

\pagebreak



### Notes {.appendix}  

Last update on 2020-02-18 18:47:55  

Serdar Balci, MD, Pathologist  
drserdarbalci@gmail.com  
https://rpubs.com/sbalci/CV   

---

\pagebreak

## Code Appendix

**Use following chunk options to include all codes below the report.**

```r
{r, echo=TRUE, eval=FALSE, ref.label=knitr::all_labels()}
```




```r
# installing necessary packages
if (requireNamespace("magrittr", quietly = TRUE)) {
  `%>%` <- magrittr::`%>%`
}
if (!require("remotes")) install.packages("remotes")
if (!require("pacman")) install.packages("pacman")
if (!require("pak")) install.packages("pak")
if (!require("here")) install.packages("here")
source_rmd <- function(rmd_file){
  knitr::knit(rmd_file, output = tempfile(), envir = globalenv())
}

list_of_Rmd <- list.files(path = here::here("childRmd"), pattern = "Rmd")

list_of_Rmd <- list_of_Rmd[!list_of_Rmd %in% c("_19shinySurvival.Rmd")]

purrr::map(.x = here::here("childRmd", list_of_Rmd), .f = source_rmd)

knitr::opts_chunk$set(
    eval = TRUE,
    echo = TRUE,
    fig.path = here::here("figs/"),
    message = FALSE,
    warning = FALSE,
    error = FALSE,
    cache = TRUE,
    comment = NA,
    tidy = TRUE,
    fig.width = 6,
    fig.height = 4
)
# https://cran.r-project.org/web/packages/exploreR/vignettes/exploreR.html
# exploreR::reset()
source(file = here::here("R", "loadLibrary.R"))
source(file = here::here("R", "gc_fake_data.R"))
wakefield::table_heat(x = fakedata, palette = "Set1", flip = TRUE, print = TRUE)
library(readxl)
mydata <- readxl::read_excel(here::here("data", "mydata.xlsx"))
# View(mydata) # Use to view data after importing
# https://cran.r-project.org/web/packages/rio/vignettes/rio.html
# rio::install_formats()

x <- rio::import("mtcars.csv")
y <- rio::import("mtcars.rds")
z <- rio::import("mtcars.dta")

rio::import("mtcars_noext", format = "csv")

rio::export(mtcars, "mtcars.csv")
rio::export(mtcars, "mtcars.rds")
rio::export(mtcars, "mtcars.dta")

rio::export(list(mtcars = mtcars, iris = iris), "multi.xlsx")

# Dataframe report
mydata %>% 
  dplyr::select(-contains("Date")) %>%
  report::report(.)
mydata %>% explore::describe_tbl()
dput(names(mydata))
keycolumns <-  
    mydata %>%  
    sapply(., FUN = dataMaid::isKey) %>%  
    tibble::as_tibble() %>%  
    dplyr::select(  
        which(.[1, ] == TRUE)  
    ) %>%   
    names()  
keycolumns  
mydata %>% 
  dplyr::select(-keycolumns) %>% 
inspectdf::inspect_types()
mydata %>% 
    dplyr::select(-keycolumns,
           -contains("Date")) %>% 
  describer::describe() %>% 
  knitr::kable(format = "markdown")
mydata %>% 
    dplyr::select(-keycolumns) %>% 
  inspectdf::inspect_types() %>% 
  inspectdf::show_plot()
# https://github.com/ropensci/visdat
# http://visdat.njtierney.com/articles/using_visdat.html
# https://cran.r-project.org/web/packages/visdat/index.html
# http://visdat.njtierney.com/

# visdat::vis_guess(mydata)

visdat::vis_dat(mydata)
mydata %>% explore::explore_tbl()
mydata %>% 
    dplyr::select(-keycolumns) %>% 
    inspectdf::inspect_types() %>% 
    dplyr::filter(type == "character") %>% 
    dplyr::select(col_name) %>% 
    dplyr::pull() %>% 
    unlist() -> characterVariables

characterVariables
mydata %>%
    dplyr::select(-keycolumns,
                  -contains("Date")
                  ) %>%
  describer::describe() %>% 
    janitor::clean_names() %>% 
    dplyr::filter(column_type == "factor") %>% 
    dplyr::select(column_name) %>% 
    dplyr::pull() -> categoricalVariables

categoricalVariables
mydata %>%
    dplyr::select(-keycolumns,
                  -contains("Date")) %>%
  describer::describe() %>% 
    janitor::clean_names() %>% 
    dplyr::filter(column_type == "numeric" | column_type == "double") %>% 
    dplyr::select(column_name) %>% 
    dplyr::pull() -> continiousVariables

continiousVariables
mydata %>% 
    dplyr::select(-keycolumns) %>% 
inspectdf::inspect_types() %>% 
  dplyr::filter(type == "numeric") %>% 
  dplyr::select(col_name) %>% 
  dplyr::pull() %>% 
  unlist() -> numericVariables

numericVariables
mydata %>% 
    dplyr::select(-keycolumns) %>% 
inspectdf::inspect_types() %>% 
  dplyr::filter(type == "integer") %>% 
  dplyr::select(col_name) %>% 
  dplyr::pull() %>% 
  unlist() -> integerVariables

integerVariables
mydata %>% 
    dplyr::select(-keycolumns) %>% 
inspectdf::inspect_types() %>% 
  dplyr::filter(type == "list") %>% 
  dplyr::select(col_name) %>% 
  dplyr::pull() %>% 
  unlist() -> listVariables
listVariables
is_date <- function(x) inherits(x, c("POSIXct", "POSIXt"))

dateVariables <- 
names(which(sapply(mydata, FUN = is_date) == TRUE))
dateVariables
View(mydata)
reactable::reactable(data = mydata, sortable = TRUE, resizable = TRUE, filterable = TRUE, searchable = TRUE, pagination = TRUE, paginationType = "numbers", showPageSizeOptions = TRUE, highlight = TRUE, striped = TRUE, outlined = TRUE, compact = TRUE, wrap = FALSE, showSortIcon = TRUE, showSortable = TRUE)
summarytools::view(summarytools::dfSummary(mydata %>% dplyr::select(-keycolumns)))
if(!dir.exists(here::here("out"))) {dir.create(here::here("out"))}

summarytools::view(
  x = summarytools::dfSummary(
    mydata %>% 
      dplyr::select(-keycolumns)
    ),
  file = here::here("out", "mydata_summary.html")
)
if(!dir.exists(here::here("out"))) {dir.create(here::here("out"))}

dataMaid::makeDataReport(data = mydata, 
                         file = here::here("out", "dataMaid_mydata.Rmd"),
                         replace = TRUE,
                         openResult = FALSE, 
                         render = FALSE,
                         quiet = TRUE
                         )
if(!dir.exists(here::here("out"))) {dir.create(here::here("out"))}

mydata %>% 
  dplyr::select(
    -dateVariables
  ) %>% 
  explore::report(
    output_file = "mydata_report.html",
    output_dir = here::here("out") 
    )
dplyr::glimpse(mydata %>% dplyr::select(-keycolumns, -dateVariables))
mydata %>% explore::describe()
explore::explore(mydata)
mydata %>%
  explore::explore_all()
visdat::vis_expect(data = mydata,
                   expectation = ~.x == -1,
                   show_perc = TRUE)

visdat::vis_expect(mydata, ~.x >= 25)
visdat::vis_miss(airquality,
                 cluster = TRUE)
visdat::vis_miss(airquality,
         sort_miss = TRUE)
xray::anomalies(mydata)
xray::distributions(mydata)
DataExplorer::plot_str(mydata)
DataExplorer::plot_str(mydata, type = "r")
DataExplorer::introduce(mydata)
DataExplorer::plot_intro(mydata)
DataExplorer::plot_missing(mydata)
mydata2 <- DataExplorer::drop_columns(mydata, "TStage")
DataExplorer::plot_bar(mydata)
DataExplorer::plot_bar(mydata, with = "Death")
DataExplorer::plot_histogram(mydata)
if(!dir.exists(here::here("out"))) {dir.create(here::here("out"))}

# https://cran.r-project.org/web/packages/dataMaid/vignettes/extending_dataMaid.html
library("dataMaid")
dataMaid::makeDataReport(mydata,
  #add extra precheck function
  preChecks = c("isKey", "isSingular", "isSupported", "isID"),

  #Add the extra summaries - countZeros() for character, factor,
  #integer, labelled and numeric variables and meanSummary() for integer,
  #numeric and logical variables:
  summaries = setSummaries(
    character = defaultCharacterSummaries(add = "countZeros"),
    factor = defaultFactorSummaries(add = "countZeros"),
    labelled = defaultLabelledSummaries(add = "countZeros"),
    numeric = defaultNumericSummaries(add = c("countZeros", "meanSummary")),
    integer = defaultIntegerSummaries(add = c("countZeros", "meanSummary")),
    logical = defaultLogicalSummaries(add =  c("meanSummary"))
  ),

  #choose mosaicVisual() for categorical variables,
  #prettierHist() for all others:
  visuals = setVisuals(
    factor = "mosaicVisual",
    numeric = "prettierHist",
    integer = "prettierHist",
    Date = "prettierHist"
  ),

  #Add the new checkFunction, identifyColons, for character, factor and
  #labelled variables:
  checks = setChecks(
    character = defaultCharacterChecks(add = "identifyColons"),
    factor = defaultFactorChecks(add = "identifyColons"),
    labelled = defaultLabelledChecks(add = "identifyColons")
  ),

  #overwrite old versions of the report, render to html and don't
  #open the html file automatically:
  replace = TRUE,
  output = "html",
  open = FALSE,
  file = here::here("out/dataMaid_mydata.Rmd")
)
# https://cran.r-project.org/web/packages/summarytools/vignettes/Recommendations-rmarkdown.html
# https://github.com/dcomtois/summarytools
library(knitr)
opts_chunk$set(comment=NA,
               prompt=FALSE,
               cache=FALSE,
               echo=TRUE,
               results='asis' # add to individual summarytools chunks
               )
library(summarytools)
st_css()
st_options(bootstrap.css     = FALSE,       # Already part of the theme so no need for it
           plain.ascii       = FALSE,       # One of the essential settings
           style             = "rmarkdown", # Idem.
           dfSummary.silent  = TRUE,        # Suppresses messages about temporary files
           footnote          = NA,          # Keeping the results minimalistic
           subtitle.emphasis = FALSE)       # For the vignette theme, this gives
                                            # much better results. Your mileage may vary.
summarytools::freq(iris$Species, plain.ascii = FALSE, style = "rmarkdown")
summarytools::freq(iris$Species, report.nas = FALSE, headings = FALSE, cumul = TRUE, totals = TRUE)
summarytools::freq(tobacco$gender, style = 'rmarkdown')
summarytools::freq(tobacco[ ,c("gender", "age.gr", "smoker")])
print(freq(tobacco$gender), method = 'render')
view(dfSummary(iris))
dfSummary(tobacco, style = 'grid', graph.magnif = 0.75, tmp.img.dir = "/tmp")
dfSummary(tobacco, plain.ascii = FALSE, style = "grid",
          graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp")
print(dfSummary(tobacco, graph.magnif = 0.75), method = 'render')
# https://github.com/rolkra/explore
# https://cran.r-project.org/web/packages/explore/vignettes/explore.html
# https://cran.r-project.org/web/packages/explore/vignettes/explore_mtcars.html


# library(dplyr)
# library(explore)

explore::explore(mydata)

# iris %>% report(output_file = "report.html", output_dir = here::here())


# iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
# iris %>%
#   report(output_file = "report.html",
#          output_dir = here::here(),
#          target = is_versicolor
# # , split = FALSE
# )
iris %>% explore::explore_tbl()

iris %>% explore::describe_tbl()

iris %>% explore::explore(Species)
iris %>% explore::explore(Sepal.Length)
iris %>% explore::explore(Sepal.Length, target = is_versicolor)
iris %>% explore::explore(Sepal.Length, target = is_versicolor, split = FALSE)
iris %>% explore::explore(Sepal.Length, target = Species)
iris %>% explore::explore(Sepal.Length, target = Petal.Length)


%>% %>%
  explore::explore_all()

iris %>%
  dplyr::select(Sepal.Length, Sepal.Width) %>%
  explore::explore_all()

iris %>%
  dplyr::select(Sepal.Length, Sepal.Width, is_versicolor) %>%
  explore::explore_all(target = is_versicolor)

iris %>%
  dplyr::select(Sepal.Length, Sepal.Width, is_versicolor) %>%
  explore::explore_all(target = is_versicolor, split = FALSE)

iris %>%
  dplyr::select(Sepal.Length, Sepal.Width, Species) %>%
  explore::explore_all(target = Species)

iris %>%
  dplyr::select(Sepal.Length, Sepal.Width, Petal.Length) %>%
  explore::explore_all(target = Petal.Length)
iris %>%
  explore::explore_all()

knitr::opts_current(fig.height=explore::total_fig_height(iris, target = Species))

explore::total_fig_height(iris, target = Species)

iris %>% explore::explore_all(target = Species)
iris %>% explore::explore(Sepal.Length, min_val = 4.5, max_val = 7)
iris %>% explore::explore(Sepal.Length, auto_scale = FALSE)
mtcars %>% explore::describe()
# https://cran.r-project.org/web/packages/dlookr/vignettes/EDA.html

dlookr::describe(mydata
                 # ,
                 # cols = c(statistic)
                 )

# dlookr::describe(carseats, Sales, CompPrice, Income)
# dlookr::describe(carseats, Sales:Income)
# dlookr::describe(carseats, -(Sales:Income))
mydata %>%
  dlookr::describe() %>%
  dplyr::select(variable, skewness, mean, p25, p50, p75) %>%
  dplyr::filter(!is.na(skewness)) %>%
  arrange(desc(abs(skewness)))
# https://cran.r-project.org/web/packages/dlookr/vignettes/EDA.html

carseats %>%
  dlookr::eda_report(target = Sales,
                     output_format = "pdf",
             output_file = "EDA.pdf"
             )
carseats %>%
  dlookr::eda_report(target = Sales,
             output_format = "html",
             output_file = "EDA.html"
             )
# install.packages("ISLR")
library("ISLR")
# install.packages("SmartEDA")
library("SmartEDA")
## Load sample dataset from ISLR pacakge
Carseats <- ISLR::Carseats

## overview of the data;
SmartEDA::ExpData(data=Carseats,type=1)
## structure of the data
SmartEDA::ExpData(data=Carseats,type=2)


mydata %>%
  dplyr::select(-dplyr::contains("Date")) %>%
  report::report()
# cat(names(mydata), sep = " + \n")
library(arsenal)
tab1 <- arsenal::tableby(
  ~ Sex +
    Age +
    Race +
    PreinvasiveComponent +
    LVI +
    PNI +
    Death +
    Group +
    Grade +
    TStage +
    # `Anti-X-intensity` +
    # `Anti-Y-intensity` +
    LymphNodeMetastasis +
    Valid +
    Smoker +
    Grade_Level
  ,
  data = mydata 
)
summary(tab1)
library(tableone)
mydata %>% 
  dplyr::select(-keycolumns,
         -dateVariables
         ) %>% 
tableone::CreateTableOne(data = .)
# CreateTableOne(vars = myVars, data = mydata, factorVars = characterVariables)
# tab <- CreateTableOne(vars = myVars, data = pbc, factorVars = catVars)
# print(tab, showAllLevels = TRUE)
# ?print.TableOne
# summary(tab)
# print(tab, nonnormal = biomarkers)
# print(tab, nonnormal = biomarkers, exact = "stage", quote = TRUE, noSpaces = TRUE)
# tab3Mat <- print(tab3, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
# write.csv(tab3Mat, file = "myTable.csv")
mydata %>% 
  dplyr::select(
    continiousVariables,
    numericVariables,
    integerVariables
  ) %>% 
summarytools::descr(., style = 'rmarkdown')
print(summarytools::descr(mydata), method = 'render', table.classes = 'st-small')
mydata %>% 
  summarytools::descr(.,
                      stats = "common",
                      transpose = TRUE,
                      headings = FALSE
                      )
mydata %>% 
  summarytools::descr(stats = "common") %>%
  summarytools::tb()
mydata$Sex %>% 
  summarytools::freq(cumul = FALSE, report.nas = FALSE) %>%
  summarytools::tb()
mydata %>%
  explore::describe() %>%
  dplyr::filter(unique < 5)
mydata %>%
  explore::describe() %>%
  dplyr::filter(na > 0)
mydata %>% explore::describe()
source(here::here("R", "gc_desc_cat.R"))
tab <- 
  mydata %>% 
  dplyr::select(
    -keycolumns
    ) %>% 
  tableone::CreateTableOne(data = .)
?print.CatTable
tab$CatTable
race_stats <- summarytools::freq(mydata$Race) 
print(race_stats,
      report.nas = FALSE,
      totals = FALSE,
      display.type = FALSE,
      Variable.label = "Race Group"
      )
mydata %>% explore::describe(PreinvasiveComponent)
## Frequency or custom tables for categorical variables
SmartEDA::ExpCTable(
  mydata,
  Target = NULL,
  margin = 1,
  clim = 10,
  nlim = 5,
  round = 2,
  bin = NULL,
  per = T
)
inspectdf::inspect_cat(mydata)

inspectdf::inspect_cat(mydata)$levels$Group
library(summarytools)

grouped_freqs <- stby(data = mydata$Smoker, 
                      INDICES = mydata$Sex, 
                      FUN = freq, cumul = FALSE, report.nas = FALSE)

grouped_freqs %>% tb(order = 2)
summarytools::stby(
  list(x = mydata$LVI, y = mydata$LymphNodeMetastasis), 
  mydata$PNI,
  summarytools::ctable
  )
with(mydata, 
     summarytools::stby(
       list(x = LVI, y = LymphNodeMetastasis), PNI,
       summarytools::ctable
       )
     )
SmartEDA::ExpCTable(
  mydata,
  Target = "Sex",
  margin = 1,
  clim = 10,
  nlim = NULL,
  round = 2,
  bin = 4,
  per = F
)
mydata %>% 
  dplyr::select(characterVariables) %>% 
  dplyr::select(PreinvasiveComponent,
         PNI,
         LVI
         ) %>% 
reactable::reactable(data = ., groupBy = c("PreinvasiveComponent", "PNI"), columns = list(
  LVI = reactable::colDef(aggregate = "count")
))
questionr:::icut()
source(here::here("R", "gc_desc_cont.R"))
tab <- tableone::CreateTableOne(data = mydata)
# ?print.ContTable
tab$ContTable
print(tab$ContTable, nonnormal = c("Anti-X-intensity"))
mydata %>% explore::describe(Age)
mydata %>% 
  dplyr::select(continiousVariables) %>% 
SmartEDA::ExpNumStat(
  data = .,
  by = "A",
  gp = NULL,
  Qnt = seq(0, 1, 0.1),
  MesofShape = 2,
  Outlier = TRUE,
  round = 2
)
inspectdf::inspect_num(mydata, breaks = 10)
inspectdf::inspect_num(mydata)$hist$Age
inspectdf::inspect_num(mydata, breaks = 10) %>%
  inspectdf::show_plot()
grouped_descr <- summarytools::stby(data = mydata, 
                      INDICES = mydata$Sex, 
                      FUN = summarytools::descr, stats = "common")
# grouped_descr %>% summarytools::tb(order = 2)
grouped_descr %>% summarytools::tb()
mydata %>%
  group_by(US) %>% 
  dlookr::describe(Sales, Income) 

carseats %>%
  group_by(US, Urban) %>% 
  dlookr::describe(Sales, Income) 

categ <- dlookr::target_by(carseats, US)
cat_num <- dlookr::relate(categ, Sales)
cat_num
summary(cat_num)
plot(cat_num)
   summarytools::stby(data = mydata, 
                               INDICES = mydata$PreinvasiveComponent, 
                               FUN = summarytools::descr,
                      stats = c("mean", "sd", "min", "med", "max"), 
                               transpose = TRUE)
with(mydata, 
     summarytools::stby(Age, PreinvasiveComponent, summarytools::descr), 
                   stats = c("mean", "sd", "min", "med", "max"),
                   transpose = TRUE
                   )
mydata %>% 
  group_by(PreinvasiveComponent) %>% 
  summarytools::descr(stats = "fivenum")
## Summary statistics by – category
SmartEDA::ExpNumStat(
  mydata,
  by = "GA",
  gp = "PreinvasiveComponent",
  Qnt = seq(0, 1, 0.1),
  MesofShape = 2,
  Outlier = TRUE,
  round = 2
)
mydata %>% 
  janitor::tabyl(Sex) %>%
  janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
  knitr::kable()
mydata %>% 
  janitor::tabyl(Race) %>%
  janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
  knitr::kable()
mydata %>% 
  janitor::tabyl(PreinvasiveComponent) %>%
  janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
  knitr::kable()
mydata %>% 
  janitor::tabyl(LVI) %>%
  janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
  knitr::kable()
mydata %>% 
  janitor::tabyl(PNI) %>%
  janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
  knitr::kable()
mydata %>% 
  janitor::tabyl(Group) %>%
  janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
  knitr::kable()
mydata %>% 
  janitor::tabyl(Grade) %>%
  janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
  knitr::kable()
mydata %>% 
  janitor::tabyl(TStage) %>%
  janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
  knitr::kable()
mydata %>% 
  janitor::tabyl(LymphNodeMetastasis) %>%
  janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
  knitr::kable()
mydata %>% 
  janitor::tabyl(Grade_Level) %>%
  janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
  knitr::kable()
mydata %>% 
  janitor::tabyl(DeathTime) %>%
  janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
  knitr::kable()
mydata %>% 
jmv::descriptives(
    data = .,
    vars = 'Age',
    hist = TRUE,
    dens = TRUE,
    box = TRUE,
    violin = TRUE,
    dot = TRUE,
    mode = TRUE,
    sd = TRUE,
    variance = TRUE,
    skew = TRUE,
    kurt = TRUE,
    quart = TRUE)
mydata %>% 
jmv::descriptives(
    data = .,
    vars = 'AntiX_intensity',
    hist = TRUE,
    dens = TRUE,
    box = TRUE,
    violin = TRUE,
    dot = TRUE,
    mode = TRUE,
    sd = TRUE,
    variance = TRUE,
    skew = TRUE,
    kurt = TRUE,
    quart = TRUE)
mydata %>% 
jmv::descriptives(
    data = .,
    vars = 'AntiY_intensity',
    hist = TRUE,
    dens = TRUE,
    box = TRUE,
    violin = TRUE,
    dot = TRUE,
    mode = TRUE,
    sd = TRUE,
    variance = TRUE,
    skew = TRUE,
    kurt = TRUE,
    quart = TRUE)
library(finalfit)
dependent <- c("dependent1",
               "dependent2"
              )

explanatory <- c("explanatory1",
                 "explanatory2"
                 )

dependent <- "PreinvasiveComponent"

explanatory <- c("Sex", "Age", "Grade", "TStage")

source(here::here("R", "gc_table_cross.R"))
CreateTableOne(vars = myVars, strata = "columnname", data = pbc, factorVars = catVars)
print(tab, nonnormal = biomarkers, exact = "exactVariable", smd = TRUE)

write2html(
  knitr::kable(head(mockstudy)), paste0(tmpdir, "/test.kable.keep.rmd.html"),
  quiet = TRUE, # passed to rmarkdown::render
  keep.rmd = TRUE
)
ctable(tobacco$gender, tobacco$smoker, style = 'rmarkdown')
print(ctable(tobacco$gender, tobacco$smoker), method = 'render')
print(ctable(tobacco$smoker, tobacco$diseased, prop = "r"), method = "render")
with(tobacco, 
     print(ctable(smoker, diseased, prop = 'n', totals = FALSE, chisq = TRUE),
           headings = FALSE, method = "render"))
# devtools::install_github("ewenharrison/summarizer")
# library(summarizer)
# data(colon_s)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
  summary.factorlist(dependent, explanatory, p=TRUE) %>% 
    knitr::kable(row.names=FALSE, align=c("l", "l", "r", "r", "r"))

explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  summary.factorlist(dependent, explanatory) %>% 
    knitr::kable(row.names=FALSE, align=c("l", "l", "r", "r", "r"))


mydata %>%
    summary_factorlist(dependent = 'PreinvasiveComponent', 
                       explanatory = explanatory,
                       # column = TRUE,
                       total_col = TRUE,
                       p = TRUE,
                       add_dependent_label = TRUE,
                       na_include=FALSE
                       # catTest = catTestfisher
                       ) -> table

knitr::kable(table, row.names = FALSE, align = c('l', 'l', 'r', 'r', 'r'))

table1 <- arsenal::tableby(PreinvasiveComponent ~ explanatory, mydata)

summary(table1)

knitr::kable(table1,
                         row.names = FALSE,
                         align = c('l', 'l', 'r', 'r', 'r', 'r'),
                         format = 'html') %>%
                kableExtra::kable_styling(kable_input = .,
                                          bootstrap_options = 'striped',
                                          full_width = F,
                                          position = 'left')
tangram::tangram(PreinvasiveComponent ~ explanatory, mydata)

tangram::html5(tangram::tangram(PreinvasiveComponent ~ explanatory, mydata),
                    fragment = TRUE,
                    inline = 'nejm.css',
                    caption = 'Cross TablePreinvasiveComponentNEJM Style',
                    id = 'tbl3')
tangram::html5(tangram::tangram(PreinvasiveComponent ~ explanatory, mydata),
                    fragment = TRUE,
                    inline = 'lancet.css',
                    caption = 'Cross TablePreinvasiveComponentLancet Style',
                    id = 'tbl3')
mydata$int <- lubridate::interval(
  lubridate::ymd(mydata$SurgeryDate),
  lubridate::ymd(mydata$LastFollowUpDate)
  )
mydata$OverallTime <- lubridate::time_length(mydata$int, "month")
mydata$OverallTime <- round(mydata$OverallTime, digits = 1)
mydata$OverallTime <- mydata$genel_sagkalim
## Recoding mydata$Death into mydata$Outcome
mydata$Outcome <- forcats::fct_recode(as.character(mydata$Death),
               "1" = "TRUE",
               "0" = "FALSE")
mydata$Outcome <- as.numeric(as.character(mydata$Outcome))
table(mydata$Death, mydata$Outcome)
library(survival)
# data(lung)
# km <- with(lung, Surv(time, status))
km <- with(mydata, Surv(OverallTime, Outcome))
head(km,80)
plot(km)
# Drawing Survival Curves Using ggplot2
# https://rpkgs.datanovia.com/survminer/reference/ggsurvplot.html
dependentKM <- "Surv(OverallTime, Outcome)"
explanatoryKM <- "LVI"

mydata %>%
  finalfit::surv_plot(.data = .,
                      dependent = dependentKM,
                      explanatory = explanatoryKM,
                      xlab='Time (months)',
                      pval=TRUE,
                      legend = 'none',
                      break.time.by = 12,
                      xlim = c(0,60)
                      # legend.labs = c('a','b')
                      )
# Drawing Survival Curves Using ggplot2
# https://rpkgs.datanovia.com/survminer/reference/ggsurvplot.html

mydata %>%
  finalfit::surv_plot(.data = .,
                      dependent = "Surv(OverallTime, Outcome)",
                      explanatory = "LVI",
                      xlab='Time (months)',
                      pval=TRUE,
                      legend = 'none',
                      break.time.by = 12,
                      xlim = c(0,60)
                      # legend.labs = c('a','b')
                      )
library(finalfit)
library(survival)
explanatoryUni <- "LVI"
dependentUni <- "Surv(OverallTime, Outcome)"

mydata %>%
finalfit::finalfit(dependentUni, explanatoryUni) -> tUni

knitr::kable(tUni, row.names=FALSE, align=c('l', 'l', 'r', 'r', 'r', 'r'))
tUni_df <- tibble::as_tibble(tUni, .name_repair = "minimal") %>% 
  janitor::clean_names() 

tUni_df_descr <- paste0("When ",
                        tUni_df$dependent_surv_overall_time_outcome[1],
                        " is ",
                        tUni_df$x[2],
                        ", there is ",
                        tUni_df$hr_univariable[2],
                        " times risk than ",
                        "when ",
                        tUni_df$dependent_surv_overall_time_outcome[1],
                        " is ",
                        tUni_df$x[1],
                        "."
                        )
km_fit <- survfit(Surv(OverallTime, Outcome) ~ LVI, data = mydata)
km_fit
plot(km_fit)
# summary(km_fit)
km_fit_median_df <- summary(km_fit)
km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>% 
  janitor::clean_names() %>% 
  tibble::rownames_to_column()
km_fit_median_df <- summary(km_fit)
            km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>%
                tibble::rownames_to_column()
            
            names(km_fit_median_df) <- paste0("m", 1:dim(km_fit_median_df)[2])

            km_fit_median_definition <- 
                
            km_fit_median_df %>%
                dplyr::mutate(
                    description =
                        glue::glue(
                            "When {m1}, median survival is {m8} [{m9} - {m10}, 95% CI] months."
                        )
                ) %>%
                dplyr::select(description) %>%
                dplyr::pull() 
sTable <- summary(km_fit)$table
            st <- data.frame()
            
            for (i in seq_len(nrow(km_fit))) {
                if (nrow(km_fit) == 1)
                    g <- sTable
                else
                    g <- sTable[i,]
                nevents <- sum(g['events'])
                n <- g['n.max']
                ncensor <- n - nevents
                median <- g['median']
                mean <- g['*rmean']
                prop <- nevents / n
                
                print(rowNo=i, list(
                    censored=ncensor,
                    events=nevents,
                    n=n,
                    prop=nevents/n,
                    median=median,
                    mean=mean))
            }
            
            st$setStatus('complete')
            
            
            results1 <- st
km_fit
broom::tidy(km_fit)
km_fit_median_df %>% 
  dplyr::mutate(
    description = 
      glue::glue(
      "When {rowname}, median survival is {median} [{x0_95lcl} - {x0_95ucl}, 95% CI] months."
    )
  ) %>% 
  dplyr::select(description) %>% 
  dplyr::pull() -> km_fit_median_definition
summary(km_fit, times = c(12,36,60))
km_fit_summary <- summary(km_fit, times = c(12,36,60))

km_fit_df <- as.data.frame(km_fit_summary[c("strata", "time", "n.risk", "n.event", "surv", "std.err", "lower", "upper")])
km_fit_df %>% 
  dplyr::mutate(
    description = 
      glue::glue(
      "When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]."
    )
  ) %>% 
  dplyr::select(description) %>% 
  dplyr::pull() -> km_fit_definition
library(survival)
surv_fit <- survival::survfit(Surv(time, status) ~ ph.ecog, data=lung)
insight::is_model_supported(surv_fit)
insight::find_formula(surv_fit)
report::report_participants(mydata)
dependentKM <-  "Surv(OverallTime, Outcome2)"

explanatoryKM <- c("explanatory1",
               "explanatory2"
               )
source(here::here("R", "gc_survival.R"))
mydependent <-  "Surv(time, status)"
explanatory <- "Organ"

mysurvival <- function(mydata, mydependent, explanatory) {
    {{mydata}} %>%
        finalfit::surv_plot(dependent = {{mydependent}},
                            explanatory = {{explanatory}},
                            xlab='Time (months)',
                            pval=TRUE,
                            legend = 'none',
                            break.time.by = 12,
                            xlim = c(0,60)
        )
}


# library(tidyverse)
mysurvival(mydata = whippleables, mydependent = mydependent, explanatory = explanatory)

explanatory <- c("Organ", "LVI")

deneme <- purrr::map(explanatory, mysurvival, mydata = whippleables, mydependent = mydependent)

dependentKM <- "Surv(OverallTime, Outcome)"
explanatoryKM <- "TStage"

mydata %>%
  finalfit::surv_plot(.data = .,
                      dependent = dependentKM,
                      explanatory = explanatoryKM,
                      xlab='Time (months)',
                      pval=TRUE,
                      legend = 'none',
                      break.time.by = 12,
                      xlim = c(0,60)
                      # legend.labs = c('a','b')
                      )
survminer::pairwise_survdiff(
  formula = Surv(OverallTime, Outcome) ~ TStage, 
                             data = mydata,
                             p.adjust.method = "BH"
  )
km_fit
print(km_fit, 
      scale=1,
      digits = max(options()$digits - 4,3),
      print.rmean=getOption("survfit.print.rmean"),
      rmean = getOption('survfit.rmean'),
      print.median=getOption("survfit.print.median"),
      median = getOption('survfit.median')
      
      )
library(finalfit)
library(survival)
explanatoryMultivariate <- explanatoryKM
dependentMultivariate <- dependentKM

mydata %>%
  finalfit(dependentMultivariate, explanatoryMultivariate) -> tMultivariate

knitr::kable(tMultivariate, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))
# https://tidymodels.github.io/parsnip/reference/surv_reg.html
library(parsnip)
surv_reg()
#> Parametric Survival Regression Model Specification (regression)
#> # Parameters can be represented by a placeholder:
surv_reg(dist = varying())

#> Parametric Survival Regression Model Specification (regression)
#> 
#> Main Arguments:
#>   dist = varying()
#> 
model <- surv_reg(dist = "weibull")
model
#> Parametric Survival Regression Model Specification (regression)
#> 
#> Main Arguments:
#>   dist = weibull
#> update(model, dist = "lnorm")#> Parametric Survival Regression Model Specification (regression)
#> 
#> Main Arguments:
#>   dist = lnorm
#> 


# From randomForest
rf_1 <- randomForest(x, y, mtry = 12, ntree = 2000, importance = TRUE)

# From ranger
rf_2 <- ranger(
  y ~ ., 
  data = dat, 
  mtry = 12, 
  num.trees = 2000, 
  importance = 'impurity'
)

# From sparklyr
rf_3 <- ml_random_forest(
  dat, 
  intercept = FALSE, 
  response = "y", 
  features = names(dat)[names(dat) != "y"], 
  col.sample.rate = 12,
  num.trees = 2000
)




rand_forest(mtry = 12, trees = 2000) %>%
  set_engine("ranger", importance = 'impurity') %>%
  fit(y ~ ., data = dat)


rand_forest(mtry = 12, trees = 2000) %>%
  set_engine("spark") %>%
  fit(y ~ ., data = dat)



library(survival)
library(survminer)
library(finalfit)

mydata %>%
  finalfit::surv_plot('Surv(OverallTime, Outcome)', 'LVI', 
  xlab='Time (months)', pval=TRUE, legend = 'none',
    break.time.by = 12, xlim = c(0,60)

# legend.labs = c('a','b')

)
explanatoryUni <- 'LVI'
dependentUni <- 'Surv(OverallTime, Outcome)'
mydata %>%
finalfit(dependentUni, explanatoryUni) -> tUni

knitr::kable(tUni, row.names=FALSE, align=c('l', 'l', 'r', 'r', 'r', 'r'))

tUni_df <- tibble::as_tibble(tUni, .name_repair = 'minimal') %>%
janitor::clean_names(dat = ., case = 'snake')


n_level <- dim(tUni_df)[1]

tUni_df_descr <- function(n) {
    paste0(
        'When ',
        tUni_df$dependent_surv_overall_time_outcome[1],
        ' is ',
        tUni_df$x[n + 1],
        ', there is ',
        tUni_df$hr_univariable[n + 1],
        ' times risk than ',
        'when ',
        tUni_df$dependent_surv_overall_time_outcome[1],
        ' is ',
        tUni_df$x[1],
        '.'
    )

}



results5 <- purrr::map(.x = c(2:n_level-1), .f = tUni_df_descr)

print(unlist(results5))

km_fit <- survfit(Surv(OverallTime, Outcome) ~ LVI, data = mydata)
km_fit

km_fit_median_df <- summary(km_fit)
km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>%
    janitor::clean_names(dat = ., case = 'snake') %>%
    tibble::rownames_to_column(.data = ., var = 'LVI')



km_fit_median_df %>%
    dplyr::mutate(
        description =
        glue::glue(
        'When, LVI, {LVI}, median survival is {median} [{x0_95lcl} - {x0_95ucl}, 95% CI] months.'
)
    ) %>%
        dplyr::mutate(
description = gsub(pattern = 'thefactor=', replacement = ' is ', x = description)
        ) %>%
    dplyr::select(description) %>%
    dplyr::pull() -> km_fit_median_definition

km_fit_median_definition




summary(km_fit, times = c(12,36,60))

km_fit_summary <- summary(km_fit, times = c(12,36,60))

km_fit_df <- as.data.frame(km_fit_summary[c('strata', 'time', 'n.risk', 'n.event', 'surv', 'std.err', 'lower', 'upper')])

km_fit_df

     


km_fit_df %>%
    dplyr::mutate(
        description =
glue::glue(
    'When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI].'
)
    ) %>%
    dplyr::select(description) %>%
    dplyr::pull() -> km_fit_definition

km_fit_definition


summary(km_fit)$table

km_fit_median_df <- summary(km_fit)
results1html <- as.data.frame(km_fit_median_df$table) %>%
    janitor::clean_names(dat = ., case = 'snake') %>%
    tibble::rownames_to_column(.data = ., var = 'LVI')

results1html[,1] <- gsub(pattern = 'thefactor=',
 replacement = '',
 x = results1html[,1])

knitr::kable(results1html,
 row.names = FALSE,
 align = c('l', rep('r', 9)),
 format = 'html',
 digits = 1)

     

formula_p1 <- jmvcore::constructFormula(terms = self$options$overalltime)

formula_p3 <- jmvcore::constructFormula(terms = LVI)

formula_p2 <- jmvcore::constructFormula(terms = self$options$outcome)


formula_p <- paste('Surv(', formula_p1, ',',  formula_p2, ') ~ ', formula_p3)

formula_p <- as.formula(formula_p)


results8 <-
    survminer::pairwise_survdiff(
    formula = formula_p,
    data = self$data,
    p.adjust.method = 'BH'
)
projectName <- list.files(path = here::here(), pattern = "Rproj")
projectName <- gsub(pattern = ".Rproj", replacement = "", x = projectName)

analysisDate <- as.character(Sys.Date())

imageName <- paste0(projectName, analysisDate, ".RData")

save.image(file = here::here("data", imageName))

rdsName <- paste0(projectName, analysisDate, ".rds")

readr::write_rds(x = mydata, path = here::here("data", rdsName))

saveRDS(object = mydata, file = here::here("data", rdsName))

excelName <- paste0(projectName, analysisDate, ".xlsx")

rio::export(
  x = mydata,
  file = here::here("data", excelName),
  format = "xlsx"
)

# writexl::write_xlsx(mydata, here::here("data", excelName))

print(glue::glue(
    "saved data after analysis to ",
    rownames(file.info(here::here("data", excelName))),
    " : ",
    as.character(
        file.info(here::here("data", excelName))$ctime
    )
    )
)
mydata %>% 
  downloadthis::download_this(
    output_name = excelName,
    output_extension = ".csv",
    button_label = "Download data as csv",
    button_type = "default"
  )

mydata %>% 
  downloadthis::download_this(
    output_name = excelName,
    output_extension = ".xlsx",
    button_label = "Download data as xlsx",
    button_type = "primary"
  )
# pacman::p_load(here, lubridate, glue)
# here::here("data", glue("{today()}_trends.csv"))
# mydata %>% select(
#     -c(
#         rapor_yil,
#         rapor_no,
#         protokol_no,
#         istek_tarihi,
#         nux_yada_met_varsa_tarihi,
#         son_hastane_vizit_tarihi,
#         Outcome
#     )
# ) -> finalSummary
# 
# summarytools::view(summarytools::dfSummary(x = finalSummary
#                                            , style = "markdown"))
citation()
report::cite_packages(session = sessionInfo())
report::show_packages(session = sessionInfo()) %>% 
    kableExtra::kable()
# citation("tidyverse")
citation("readxl")
citation("janitor")
# citation("report")
citation("finalfit")
# citation("ggstatsplot")
if(!dir.exists(here::here("bib"))) {dir.create(here::here("bib"))}

knitr::write_bib(x = c(.packages(), "knitr", "shiny"),
                 file = here::here("bib", "packages.bib")
)
sessionInfo()
pacman::p_loaded(all = TRUE)
search()
library()
installed.packages()[1:5, c("Package", "Version")]
installed.packages()
```


---

\pagebreak


<!-- **push all changes to GitHub repository**  -->

<!-- ```{r git update} -->
<!-- source(file = here::here("R", "force_git.R")) -->
<!-- ``` -->


<!-- --- -->


# References


