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
date: "2020-02-09"
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
    urlcolor: blue
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
    cache = FALSE,
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
mydata %>% select(-contains("Date")) %>% report::report(.)
```

```
The data contains 250 observations of the following variables:
  - ID: 250 entries: 001, n = 1; 002, n = 1; 003, n = 1 and 247 others
  - Name: 249 entries: Aahaan, n = 1; Abdihamid, n = 1; Abdulkarim, n = 1 and 246 others (1 missing)
  - Sex: 2 entries: Female, n = 139; Male, n = 110 (1 missing)
  - Age: Mean = 50.09, SD = 14.54, range = [25, 73], 1 missing
  - Race: 6 entries: White, n = 165; Hispanic, n = 37; Black, n = 33 and 3 others (1 missing)
  - PreinvasiveComponent: 2 entries: Absent, n = 193; Present, n = 56 (1 missing)
  - LVI: 2 entries: Absent, n = 161; Present, n = 89
  - PNI: 2 entries: Absent, n = 169; Present, n = 80 (1 missing)
  - Death: 2 levels: FALSE (n = 64); TRUE (n = 185) and missing (n = 1)
  - Group: 2 entries: Treatment, n = 137; Control, n = 112 (1 missing)
  - Grade: 3 entries: 3, n = 112; 2, n = 71; 1, n = 66 (1 missing)
  - TStage: 4 entries: 4, n = 108; 3, n = 85; 2, n = 37 and 1 other
  - AntiX_intensity: Mean = 2.38, SD = 0.66, range = [1, 3], 1 missing
  - AntiY_intensity: Mean = 2.02, SD = 0.76, range = [1, 3], 1 missing
  - LymphNodeMetastasis: 2 entries: Absent, n = 158; Present, n = 91 (1 missing)
  - Valid: 2 levels: FALSE (n = 119); TRUE (n = 130) and missing (n = 1)
  - Smoker: 2 levels: FALSE (n = 115); TRUE (n = 134) and missing (n = 1)
  - Grade_Level: 3 entries: high, n = 110; moderate, n = 77; low, n = 62 (1 missing)
  - DeathTime: 2 entries: Within1Year, n = 148; MoreThan1Year, n = 102
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


### Find Key Columns


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


#### Find ID and key columns to exclude from analysis


See the code as function in [`R/find_key.R`](https://github.com/sbalci/histopathology-template/blob/master/R/find_key.R). 



```r
keycolumns <- mydata %>% sapply(., FUN = dataMaid::isKey) %>% as_tibble() %>% select(which(.[1, 
    ] == TRUE)) %>% names()
keycolumns
```

```
[1] "ID"   "Name"
```


### Variable Types

**Get variable types**


```r
mydata %>% select(-keycolumns) %>% inspectdf::inspect_types()
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
mydata %>% select(-keycolumns, -contains("Date")) %>% describer::describe() %>% knitr::kable(format = "markdown")
```



|.column_name         |.column_class |.column_type | .count_elements| .mean_value|  .sd_value|.q0_value     | .q25_value| .q50_value| .q75_value|.q100_value |
|:--------------------|:-------------|:------------|---------------:|-----------:|----------:|:-------------|----------:|----------:|----------:|:-----------|
|Sex                  |character     |character    |             250|          NA|         NA|Female        |         NA|         NA|         NA|Male        |
|Age                  |numeric       |double       |             250|   50.092369| 14.5439927|25            |         38|         51|         63|73          |
|Race                 |character     |character    |             250|          NA|         NA|Asian         |         NA|         NA|         NA|White       |
|PreinvasiveComponent |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|LVI                  |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|PNI                  |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|Death                |logical       |logical      |             250|          NA|         NA|FALSE         |         NA|         NA|         NA|TRUE        |
|Group                |character     |character    |             250|          NA|         NA|Control       |         NA|         NA|         NA|Treatment   |
|Grade                |character     |character    |             250|          NA|         NA|1             |         NA|         NA|         NA|3           |
|TStage               |character     |character    |             250|          NA|         NA|1             |         NA|         NA|         NA|4           |
|AntiX_intensity      |numeric       |double       |             250|    2.381526|  0.6622147|1             |          2|          2|          3|3           |
|AntiY_intensity      |numeric       |double       |             250|    2.024096|  0.7616181|1             |          1|          2|          3|3           |
|LymphNodeMetastasis  |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|Valid                |logical       |logical      |             250|          NA|         NA|FALSE         |         NA|         NA|         NA|TRUE        |
|Smoker               |logical       |logical      |             250|          NA|         NA|FALSE         |         NA|         NA|         NA|TRUE        |
|Grade_Level          |character     |character    |             250|          NA|         NA|high          |         NA|         NA|         NA|moderate    |
|DeathTime            |character     |character    |             250|          NA|         NA|MoreThan1Year |         NA|         NA|         NA|Within1Year |


**Plot variable types**


```r
mydata %>% select(-keycolumns) %>% inspectdf::inspect_types() %>% inspectdf::show_plot()
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
characterVariables <- mydata %>% select(-keycolumns) %>% inspectdf::inspect_types() %>% 
    dplyr::filter(type == "character") %>% dplyr::select(col_name) %>% pull() %>% 
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
numericVariables <- mydata %>% select(-keycolumns) %>% inspectdf::inspect_types() %>% 
    dplyr::filter(type == "numeric") %>% dplyr::select(col_name) %>% pull() %>% unlist()

numericVariables
```

```
[1] "Age"             "AntiX_intensity" "AntiY_intensity"
```


#### Find `integer` variables



```r
integerVariables <- mydata %>% select(-keycolumns) %>% inspectdf::inspect_types() %>% 
    dplyr::filter(type == "integer") %>% dplyr::select(col_name) %>% pull() %>% unlist()

integerVariables
```

```
NULL
```


#### Find `list` variables


```r
listVariables <- mydata %>% select(-keycolumns) %>% inspectdf::inspect_types() %>% 
    dplyr::filter(type == "list") %>% dplyr::select(col_name) %>% pull() %>% unlist()
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

<!--html_preserve--><div id="htmlwidget-6124b330d618d1073118" class="reactable html-widget" style="width:auto;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-6124b330d618d1073118">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"ID":["001","002","003","004","005","006","007","008","009","010","011","012","013","014","015","016","017","018","019","020","021","022","023","024","025","026","027","028","029","030","031","032","033","034","035","036","037","038","039","040","041","042","043","044","045","046","047","048","049","050","051","052","053","054","055","056","057","058","059","060","061","062","063","064","065","066","067","068","069","070","071","072","073","074","075","076","077","078","079","080","081","082","083","084","085","086","087","088","089","090","091","092","093","094","095","096","097","098","099","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250"],"Name":["Emerson","Jenella","Benn","Christiann","Zakhiya","Atef","Paulann","Mirianna","Seandra","Trequan","Lindse","Eito","Leory","Javanni","Katarina","Eura","Knoelle","Child","Jalis","Andersson","Sharlene","Alvaro","Tamirra","Moritz","Adut","Ivanya","Haston","Frica","Delena","Garey","Lachlann","Nykolas","Kazuyo","Horton","Naquanda","Allistair","Jeanne","Shiphrah","Maddisyn","Kamsiyochi","Jaqualla","Deltrick","Judette","Paisyn","Isiash","Jyquan","Sherrilynn","Dichelle","Reinna","Nefertiti","Evander","Jolleen","Lulabell","Detrell","Lizbeth","Ahnyia","Jaythan","Access","Justa","Jotaro","Lalania","Shauntai","Jamii","Taysean","Lima","Kymera","Ryanlee","Philomene","Irfan","Kyrianna","Vail","Nayele","Madee","Wilfredo","Dazhan","Daneen","Merrie","Roosvelt","Salbador","Dionne","Cybelle","Cam","Zykeem","Reynol","Jeremial","Kelse","Cliford","Jihoo","Estralita","Ahmik","Gabrialle","Keldyn","Zoeann","Somil","Keyonie","Atonio","Kaneeshia","Jayza","Tigra","Bryheem","Pieter","Amija","Yanci","Javez","Neilie","Mercedi","Ristina","Roshni","Abha","Serene","Towanda","Johnnylee","Chick","Latonza","Ender","Nyziah","Abdulkarim","Donicio","Brandel","Tomiwa","Coriene","Triston","Shanari","Zakar","Lambert","Providencia","Ureka","Kylealexander","Joevanni","Bryssia","Sandrah","Altha","Tayja","Lamine","Koketa","Azaad","Darney","Yzabel","Wafi","Lynnaya","Joletha","Ambi","Motunrayo","Marlice","Yonathan","Hyle","Ronnel","Kedron","Altaira","Jemilla","Jodiel","Kemeshia","Candina","Evyn","Brystle","Bradford","Urban","Talena","Keiasha","Lundy","Twig","Joice","Mikilah","Leathie","Eldan","Joniya","Avonlee","Raychelle","Julien","Quaylen","Taggert","Kashina","Jeanella","Sagar","Winsor","Sharissa","Joevany","Caua","Jameera","Arquita","Leontine","Alick","Lamareon","Jayveer","Sanika",null,"Quantel","Matika","Lateshia","Kimoralee","Nadeen","Kohlston","Glinda","Javanta","Equasia","Hasting","Verle","Rochella","Maiha","Emerlee","Secelia","Addisan","Abdihamid","Ozara","Daylie","Myisha","Manul","Cherlin","Mikki","Jacqualynn","Laderrick","Jorgejr","Aahaan","Kayly","Gautam","Pharyn","Sok","Nechelle","Vyctorya","Katika","Danahi","Cezanne","Nariyah","Raney","Gwyndolin","Clotine","Jedrek","Mccayla","Seja","Kennitha","Kanea","Johny","Raad","Renaud","Cella","Isreal","Gavon","Shatay","Zanobia","Oluwadarasimi","Anariyah","Kyleah","Fate","Cyree","Nylan","Jarmal","Oscarjr","Brighten","Jakiya","Ahreanna"],"Sex":["Male","Male","Female","Male","Female","Male","Female","Female","Female","Male","Male","Male","Female","Male","Female","Female","Female","Male","Female","Female","Male","Female","Female","Female","Female","Male","Female","Female","Male","Male","Female","Female","Female","Female","Female","Male","Male","Female","Male","Female","Female","Male","Female","Male","Female","Female","Female","Male","Male","Male","Male","Male","Female","Male","Female","Female","Male","Male","Female","Male","Male","Female","Male","Female","Female","Female","Female","Male","Male","Female","Male","Female","Female","Female","Female","Female","Female","Male","Male","Male","Female","Female","Female","Female","Male","Female","Male","Female","Female","Female","Female","Female","Female","Male","Female","Male","Female","Female","Female","Female","Male","Female","Female","Male","Female","Female","Female","Female","Female","Female","Male","Female","Male","Female","Female","Female","Female","Male",null,"Female","Female","Male","Male","Male","Male","Female","Female","Female","Male","Male","Male","Male","Male","Male","Female","Male","Male","Female","Female","Female","Female","Male","Female","Male","Female","Female","Female","Male","Female","Male","Female","Male","Female","Female","Male","Female","Female","Male","Male","Male","Male","Male","Male","Male","Female","Female","Female","Male","Male","Female","Female","Female","Male","Female","Male","Female","Female","Female","Female","Female","Female","Female","Female","Male","Female","Female","Male","Male","Male","Female","Female","Female","Male","Female","Female","Female","Male","Male","Female","Female","Male","Female","Male","Male","Female","Male","Male","Male","Female","Female","Female","Male","Male","Male","Male","Female","Male","Female","Female","Male","Female","Female","Male","Female","Male","Male","Male","Male","Female","Male","Female","Male","Male","Male","Male","Male","Female","Female","Male","Female","Male","Male","Male","Male","Female","Male","Female","Female","Female","Male"],"Age":[26,31,44,70,38,67,65,54,49,41,26,53,51,31,35,38,53,52,70,71,64,73,30,29,38,48,71,43,42,59,61,58,27,51,63,43,31,31,63,66,41,57,73,"NA",60,41,38,64,33,55,45,68,35,51,50,60,52,37,53,70,45,36,70,61,45,72,65,56,25,67,63,35,57,43,50,60,33,71,45,63,27,36,56,35,54,70,31,66,39,60,52,26,45,66,30,71,47,72,30,30,36,36,67,33,54,65,69,63,55,64,73,26,25,41,33,43,43,66,50,52,43,46,62,27,44,32,40,60,64,55,73,70,55,55,38,55,29,60,62,69,30,28,50,34,65,47,53,69,67,33,41,61,60,40,26,42,66,67,62,59,59,52,68,50,63,34,50,27,72,73,51,30,64,68,25,46,72,60,53,51,34,53,68,67,43,35,28,42,32,57,41,37,33,73,69,73,42,61,56,30,47,53,39,28,61,43,69,25,50,61,27,38,71,57,73,47,37,29,44,38,62,40,72,70,29,63,69,70,45,66,35,52,67,49,31,47,50,55,36,48,70,41,35,56,44,25,55,26,58,51],"Race":["White","White","White","White","White","White","Hispanic","White","Hispanic","White","White","White","Black","White","Hispanic","White","Hispanic","White","White","White","White","White","White","White","White","White","White","White","Black","White","White","White","White","Hispanic","White","Hispanic","White","Asian","White","White","White","Hispanic","White","White","White","White","White","White","White","White","White","Black","White","Hispanic","White","White","Hispanic","White","White","White","Bi-Racial","Black","White","White","White","Hispanic","White","Black","White","Black","Black","Asian","Black","White","White","White","Black","Hispanic","Hispanic","White","White","White","White","White","Black","White","White","White","White","White","White","Hispanic","White","White","Black","White","Bi-Racial","White","White","White","White","White","White","White","White","White","Hispanic","Black","White","Native","White","Black","White","Black","Black","Asian","White","White","Hispanic","Hispanic","Hispanic","White","Hispanic","White","Hispanic","White","White","White","White","Black","White","Hispanic","Hispanic","Hispanic","White","Hispanic","Black","White",null,"White","Asian","Hispanic","White","White","White","White","White","White","White","White","Black","Asian","Hispanic","Native","White","White","Black","White","Native","White","White","White","White","White","White","White","Hispanic","White","Hispanic","White","White","White","White","White","White","White","White","Hispanic","White","Black","White","White","Black","White","Black","Bi-Racial","White","White","White","White","White","White","Black","Bi-Racial","White","Hispanic","Hispanic","White","White","White","White","Black","White","White","White","White","White","Hispanic","White","White","Black","White","White","White","Hispanic","Hispanic","White","White","White","White","Hispanic","Hispanic","White","White","White","White","White","White","White","White","White","Hispanic","Black","Black","White","Black","White","Hispanic","White","Black","Black","Black","White","White","White","Asian","White","Black","Native","Black"],"PreinvasiveComponent":["Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Present","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Absent",null,"Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent"],"LVI":["Absent","Present","Absent","Present","Absent","Present","Present","Absent","Present","Absent","Absent","Present","Absent","Present","Present","Present","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Present","Absent","Present","Present","Present","Present","Present","Present","Absent","Present","Present","Absent","Absent","Present","Present","Absent","Present","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Present","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Present","Present","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Absent","Present","Present","Present","Present","Absent","Absent","Present","Absent","Present","Absent","Present","Present","Present","Absent","Absent","Present","Present","Present","Present","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent"],"PNI":["Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Present","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Present","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present",null,"Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Present","Present","Present","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Present","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Present","Present","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Present","Absent","Absent","Present","Present","Present"],"LastFollowUpDate":["2020-01-09T00:00:00","2019-03-09T00:00:00","2019-06-09T00:00:00","2019-09-09T00:00:00","2019-09-09T00:00:00","2019-06-09T00:00:00","2019-07-09T00:00:00","2020-02-09T00:00:00","2020-02-09T00:00:00","2019-10-09T00:00:00","2019-06-09T00:00:00","2019-07-09T00:00:00","2019-03-09T00:00:00","2019-11-09T00:00:00","2019-03-09T00:00:00","2019-08-09T00:00:00","2019-03-09T00:00:00","2020-02-09T00:00:00","2019-07-09T00:00:00","2019-12-09T00:00:00","2019-05-09T00:00:00","2019-09-09T00:00:00","2019-05-09T00:00:00","2019-07-09T00:00:00","2019-11-09T00:00:00","2020-02-09T00:00:00","2020-02-09T00:00:00","2019-09-09T00:00:00",null,"2019-08-09T00:00:00","2019-04-09T00:00:00","2019-12-09T00:00:00","2020-02-09T00:00:00","2020-01-09T00:00:00","2019-12-09T00:00:00","2019-10-09T00:00:00","2020-01-09T00:00:00","2019-07-09T00:00:00","2019-09-09T00:00:00","2020-02-09T00:00:00","2019-05-09T00:00:00","2020-01-09T00:00:00","2019-04-09T00:00:00","2019-09-09T00:00:00","2019-07-09T00:00:00","2019-11-09T00:00:00","2020-01-09T00:00:00","2019-05-09T00:00:00","2019-10-09T00:00:00","2019-12-09T00:00:00","2019-03-09T00:00:00","2019-08-09T00:00:00","2020-02-09T00:00:00","2020-01-09T00:00:00","2019-11-09T00:00:00","2019-05-09T00:00:00","2019-05-09T00:00:00","2019-04-09T00:00:00","2019-08-09T00:00:00","2020-01-09T00:00:00","2019-09-09T00:00:00","2019-06-09T00:00:00","2019-12-09T00:00:00","2019-06-09T00:00:00","2020-02-09T00:00:00","2019-07-09T00:00:00","2019-03-09T00:00:00","2019-06-09T00:00:00","2019-10-09T00:00:00","2019-08-09T00:00:00","2020-02-09T00:00:00","2020-02-09T00:00:00","2019-10-09T00:00:00","2019-10-09T00:00:00","2019-03-09T00:00:00","2020-01-09T00:00:00","2019-06-09T00:00:00","2020-02-09T00:00:00","2020-01-09T00:00:00","2019-06-09T00:00:00","2019-10-09T00:00:00","2020-02-09T00:00:00","2019-11-09T00:00:00","2020-02-09T00:00:00","2019-08-09T00:00:00","2019-11-09T00:00:00","2019-04-09T00:00:00","2019-10-09T00:00:00","2020-02-09T00:00:00","2020-02-09T00:00:00","2019-09-09T00:00:00","2019-03-09T00:00:00","2019-10-09T00:00:00","2020-01-09T00:00:00","2020-02-09T00:00:00","2019-11-09T00:00:00","2019-08-09T00:00:00","2019-10-09T00:00:00","2019-03-09T00:00:00","2019-03-09T00:00:00","2019-06-09T00:00:00","2019-03-09T00:00:00","2019-03-09T00:00:00","2019-11-09T00:00:00","2019-12-09T00:00:00","2020-02-09T00:00:00","2020-01-09T00:00:00","2019-07-09T00:00:00","2019-09-09T00:00:00","2019-07-09T00:00:00","2019-03-09T00:00:00","2019-08-09T00:00:00","2019-03-09T00:00:00","2019-07-09T00:00:00","2019-04-09T00:00:00","2019-10-09T00:00:00","2019-05-09T00:00:00","2019-06-09T00:00:00","2019-08-09T00:00:00","2019-06-09T00:00:00","2019-05-09T00:00:00","2019-06-09T00:00:00","2019-09-09T00:00:00","2020-02-09T00:00:00","2019-11-09T00:00:00","2019-06-09T00:00:00","2019-09-09T00:00:00","2019-07-09T00:00:00","2019-07-09T00:00:00","2019-08-09T00:00:00","2019-04-09T00:00:00","2019-08-09T00:00:00","2019-11-09T00:00:00","2020-02-09T00:00:00","2019-08-09T00:00:00","2020-02-09T00:00:00","2019-05-09T00:00:00","2019-12-09T00:00:00","2019-06-09T00:00:00","2019-04-09T00:00:00","2019-09-09T00:00:00","2019-03-09T00:00:00","2019-04-09T00:00:00","2019-09-09T00:00:00","2019-08-09T00:00:00","2019-12-09T00:00:00","2020-01-09T00:00:00","2020-01-09T00:00:00","2020-01-09T00:00:00","2019-09-09T00:00:00","2019-08-09T00:00:00","2019-07-09T00:00:00","2019-03-09T00:00:00","2019-06-09T00:00:00","2020-01-09T00:00:00","2019-03-09T00:00:00","2020-02-09T00:00:00","2020-01-09T00:00:00","2020-02-09T00:00:00","2019-04-09T00:00:00","2019-04-09T00:00:00","2019-12-09T00:00:00","2019-09-09T00:00:00","2019-07-09T00:00:00","2020-01-09T00:00:00","2020-01-09T00:00:00","2020-02-09T00:00:00","2020-02-09T00:00:00","2019-07-09T00:00:00","2019-10-09T00:00:00","2019-04-09T00:00:00","2019-04-09T00:00:00","2019-10-09T00:00:00","2019-08-09T00:00:00","2019-12-09T00:00:00","2020-02-09T00:00:00","2019-08-09T00:00:00","2019-10-09T00:00:00","2019-06-09T00:00:00","2020-01-09T00:00:00","2019-07-09T00:00:00","2019-12-09T00:00:00","2019-05-09T00:00:00","2019-03-09T00:00:00","2019-04-09T00:00:00","2019-07-09T00:00:00","2019-05-09T00:00:00","2019-07-09T00:00:00","2019-11-09T00:00:00","2019-08-09T00:00:00","2019-07-09T00:00:00","2019-05-09T00:00:00","2019-07-09T00:00:00","2019-10-09T00:00:00","2020-01-09T00:00:00","2019-11-09T00:00:00","2020-01-09T00:00:00","2019-12-09T00:00:00","2019-12-09T00:00:00","2019-05-09T00:00:00","2020-01-09T00:00:00","2019-07-09T00:00:00","2019-08-09T00:00:00","2019-03-09T00:00:00","2019-03-09T00:00:00","2020-01-09T00:00:00","2019-04-09T00:00:00","2020-01-09T00:00:00","2019-06-09T00:00:00","2019-06-09T00:00:00","2019-11-09T00:00:00","2019-03-09T00:00:00","2019-04-09T00:00:00","2019-08-09T00:00:00","2019-08-09T00:00:00","2020-02-09T00:00:00","2019-05-09T00:00:00","2019-05-09T00:00:00","2019-11-09T00:00:00","2019-09-09T00:00:00","2019-06-09T00:00:00","2019-06-09T00:00:00","2020-02-09T00:00:00","2019-08-09T00:00:00","2019-07-09T00:00:00","2019-04-09T00:00:00","2019-04-09T00:00:00","2019-06-09T00:00:00","2020-02-09T00:00:00","2019-12-09T00:00:00","2019-09-09T00:00:00","2019-05-09T00:00:00","2019-12-09T00:00:00","2019-05-09T00:00:00","2019-10-09T00:00:00","2019-11-09T00:00:00","2019-05-09T00:00:00","2019-11-09T00:00:00","2020-02-09T00:00:00","2019-09-09T00:00:00","2019-08-09T00:00:00","2019-08-09T00:00:00","2019-03-09T00:00:00","2019-08-09T00:00:00","2019-04-09T00:00:00","2019-10-09T00:00:00","2019-03-09T00:00:00","2019-12-09T00:00:00","2019-11-09T00:00:00","2019-06-09T00:00:00"],"Death":[true,true,true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,false,true,false,false,false,false,false,true,true,true,false,false,true,true,true,false,true,true,false,false,true,true,true,true,true,true,true,true,true,true,true,true,false,true,false,false,true,false,true,true,true,true,true,false,false,true,true,true,false,false,true,false,true,true,true,true,true,true,false,false,false,false,false,false,true,true,true,true,false,true,true,false,true,null,true,true,true,true,false,true,true,true,false,true,false,true,false,true,true,false,true,true,true,false,true,false,true,true,true,true,true,false,true,true,true,false,true,true,false,true,false,true,true,false,true,true,true,true,true,true,true,true,true,false,true,true,true,false,true,true,true,true,false,true,false,false,true,true,true,true,true,true,false,true,true,true,true,false,false,true,true,true,true,true,true,false,true,false,true,true,true,true,false,true,false,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,false,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,false,true,true,true,true,true,true,true,false,false,true,false,false,true,false,true,true],"Group":["Treatment","Control","Treatment","Treatment","Control","Treatment","Treatment","Control","Control","Treatment","Treatment","Control","Control","Control","Control","Control","Control","Control","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Control","Control","Treatment","Treatment","Control","Treatment","Control","Treatment","Treatment","Treatment","Control","Control","Treatment","Treatment","Treatment","Control","Control","Treatment","Control","Treatment","Treatment","Treatment","Control","Control","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Control","Treatment","Treatment","Treatment","Control","Control","Treatment","Control","Control","Treatment","Treatment","Control","Treatment","Treatment","Treatment","Control","Control","Control","Treatment","Treatment","Control","Treatment","Treatment","Control","Treatment","Treatment","Control","Control","Treatment","Control","Control","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Treatment","Treatment","Control","Control","Control","Control","Treatment","Control","Control","Treatment","Control","Control","Treatment","Treatment","Control","Control","Control","Treatment","Control","Treatment","Treatment","Treatment","Control","Control","Treatment","Control","Treatment","Treatment","Treatment","Treatment","Control","Control","Control","Control","Control","Control","Treatment","Treatment","Control","Control","Treatment","Treatment","Treatment","Treatment","Control","Control","Control","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Control","Control","Treatment","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Control","Treatment","Control","Control","Control","Control","Treatment","Control","Control","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Treatment","Control","Treatment","Control","Control","Treatment","Treatment","Control","Control","Control","Control","Control","Treatment","Treatment","Treatment","Control","Control","Treatment","Treatment","Control","Control","Treatment","Treatment","Control","Control","Control","Control","Control","Treatment","Control","Treatment","Treatment","Treatment","Control","Control",null,"Treatment","Treatment","Control","Control","Treatment","Treatment","Treatment","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Control","Control","Control","Treatment","Control","Control","Control","Treatment","Control","Treatment","Treatment","Treatment","Treatment"],"Grade":["3","3","2","3","2","1","3","3","2","3","1","2","2","3","3","3","2","3","3","2","3","1","2","2","3","3","2","3","1","2","3","2","1","3","3","1","3","3","2","3","3","2","1","1","3","1","3","3","2","3","3","2","3","2","3","1","3","3","1","1","1","3","3","2","3","3","3","1","2","2","1","1","3","1","3","3","3","1","1","1","3","3","1","3","2","3","1","3","3","3","1","1","1","3","3","3","3","3","2",null,"3","2","3","2","3","2","3","2","3","3","3","1","2","3","1","3","3","1","3","2","3","2","1","1","3","2","1","3","2","3","2","1","3","1","2","3","1","2","1","2","2","3","3","2","1","3","2","3","1","1","3","1","1","1","3","3","1","2","3","1","1","3","3","2","1","2","3","3","3","1","3","1","3","3","3","1","2","3","2","2","2","1","3","2","3","1","1","3","1","2","3","2","1","3","2","2","3","2","2","2","2","2","2","2","3","3","2","1","2","1","1","3","3","3","3","3","1","1","1","2","3","1","2","1","2","2","3","3","3","3","3","3","2","3","2","3","1","2","1","1","1","2","2","2","2","1","3","2","3","3"],"TStage":["1","4","3","4","3","4","4","4","4","3","4","2","3","4","4","3","1","2","3","4","4","2","4","2","2","3","3","3","3","2","3","3","3","4","3","4","3","4","4","4","3","4","1","4","4","2","4","4","4","4","1","4","4","4","3","2","1","4","4","4","4","2","4","4","1","3","4","2","3","2","2","3","4","4","4","3","4","3","2","4","3","4","4","4","4","3","3","4","4","3","4","3","3","4","4","4","3","4","1","3","1","1","1","4","4","3","3","4","4","3","4","4","4","3","2","4","4","3","3","2","4","2","3","2","3","3","4","1","2","4","4","3","4","4","4","2","4","3","4","4","4","3","2","4","4","4","4","2","3","4","3","3","2","1","3","1","2","3","4","3","4","4","4","2","1","1","4","4","3","4","3","3","3","3","3","3","3","2","1","3","3","3","4","2","3","4","3","4","3","1","2","3","4","4","2","4","4","3","4","4","3","3","2","4","2","3","2","3","2","3","4","2","4","3","3","3","2","3","3","4","2","4","4","3","4","3","4","3","3","2","4","3","3","4","4","4","1","1","1","3","4","2","4","4","3","3","3","4","3","4"],"AntiX_intensity":[3,2,2,3,2,2,2,2,3,3,3,1,1,1,2,2,2,3,2,3,3,1,3,3,1,2,3,2,2,3,2,2,2,3,3,2,3,3,3,3,2,3,3,3,2,3,3,2,2,3,2,2,3,3,2,2,2,3,3,3,3,3,1,3,3,3,2,3,3,3,3,2,3,3,2,2,2,2,3,1,2,3,1,3,2,2,3,3,2,2,2,3,"NA",2,2,3,2,3,2,2,2,3,3,1,3,2,3,2,2,3,2,3,3,3,3,3,1,2,1,2,2,3,3,2,3,3,2,2,3,3,2,1,3,3,2,2,3,3,2,3,3,2,3,2,3,3,3,2,1,2,3,3,3,3,1,2,3,3,2,1,2,3,3,1,2,2,2,2,1,2,3,2,3,1,2,3,3,2,3,3,2,1,3,2,3,2,3,2,2,3,2,2,1,1,3,3,2,3,3,2,2,1,2,3,3,2,2,3,1,2,3,2,2,3,2,3,2,3,3,3,2,2,2,3,3,2,3,1,2,3,2,3,3,3,2,3,1,3,3,2,2,3,3,3,2,3,3,2,2,2],"AntiY_intensity":[3,2,3,3,1,1,2,2,2,3,3,2,2,3,3,2,1,2,2,1,1,2,3,1,3,2,3,2,1,2,2,3,3,3,1,3,2,2,1,2,2,1,3,2,2,1,2,2,3,2,2,1,1,1,2,1,2,3,2,2,1,1,3,3,1,2,2,3,1,3,1,2,3,1,1,2,2,2,3,2,2,2,2,2,3,1,1,2,2,1,1,1,2,3,3,3,2,1,3,1,3,2,1,1,2,2,2,1,2,3,1,2,1,2,2,2,2,2,1,1,1,2,2,2,2,3,1,1,3,1,3,3,3,2,3,2,1,2,2,2,1,1,3,3,2,1,1,3,1,2,3,2,2,3,1,1,2,2,3,3,3,3,2,3,1,2,3,3,3,3,3,2,1,3,1,2,1,3,2,2,3,1,2,2,2,2,3,3,1,2,2,2,1,1,1,2,1,3,2,3,2,3,1,2,1,2,3,2,2,3,2,1,2,2,2,3,1,3,1,3,1,1,2,3,2,1,1,1,2,1,3,3,3,3,3,3,2,3,2,2,2,3,2,"NA",3,2,3,2,2,2],"LymphNodeMetastasis":["Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Present",null,"Present","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Present","Absent","Present","Present","Present","Absent","Absent","Present","Absent","Present","Present","Absent","Present","Present","Absent","Present","Present","Absent","Absent","Present","Absent","Present","Present","Present","Present","Present","Present","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Present","Present","Absent","Present","Present","Absent","Absent","Present","Present","Present","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Present","Present","Absent","Present","Present","Absent","Absent","Present","Present","Present","Absent","Present","Present","Present","Absent","Absent","Present"],"Valid":[true,true,false,true,false,false,false,false,false,true,false,true,false,false,true,true,false,true,false,true,true,false,true,false,true,false,true,true,true,true,true,false,true,true,false,false,false,false,true,false,false,true,true,false,true,false,false,false,false,false,false,false,true,false,false,false,true,false,false,true,true,false,true,false,true,false,false,true,true,true,false,false,false,true,true,false,false,true,true,true,false,true,false,true,true,true,true,true,false,false,false,false,false,false,true,true,false,false,true,true,false,false,false,false,true,true,false,true,true,true,false,true,true,true,true,true,true,true,false,true,true,false,false,true,false,true,true,false,true,false,false,true,null,true,false,true,true,false,false,false,true,true,false,true,false,false,true,true,true,true,true,false,false,false,false,true,false,false,false,true,true,false,true,false,false,true,false,false,true,false,false,false,true,true,true,false,true,true,true,true,true,false,true,true,true,false,false,false,false,true,false,true,false,false,true,false,false,true,true,true,false,true,false,false,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,false,false,true,false,true,true,false,true,false,false,false,false,true,false,false,true,true,false,true,true,false,false,false,false,true],"Smoker":[false,true,false,false,false,true,false,true,true,false,true,false,true,false,true,false,true,true,true,false,true,true,true,true,false,false,true,true,true,false,false,false,false,false,false,true,false,false,true,true,true,true,true,true,false,true,true,true,false,true,true,false,true,false,true,false,false,false,true,true,true,true,true,true,false,true,true,false,false,true,true,false,false,false,false,true,true,true,true,true,true,true,false,false,true,false,true,true,false,true,true,true,true,true,true,true,false,true,true,true,true,true,true,false,true,false,true,false,false,false,true,true,true,false,true,true,false,true,true,true,false,false,false,true,false,false,true,true,false,false,false,false,false,true,true,false,false,true,false,true,true,true,true,false,false,true,false,true,false,true,true,false,false,false,false,false,true,true,false,true,false,true,true,true,true,true,false,true,true,true,false,true,false,false,false,true,true,false,false,false,true,true,false,false,false,true,false,true,true,false,false,false,false,false,null,false,true,false,true,true,false,false,true,false,true,true,true,false,false,true,true,true,true,false,true,true,true,false,false,false,false,true,true,false,true,false,false,false,false,true,false,true,false,true,true,false,false,true,true,true,false,false,false,false,true,false,false,false,true,true],"Grade_Level":["low","moderate","high","low","high","moderate","high","high","low","moderate","high","low","high","moderate","high","moderate","low","high","moderate","moderate","moderate","high","high","high","moderate","high","moderate","moderate","high","low","low","moderate","moderate","moderate","low","high","high","high","high","high","high","low","low","moderate","low","high","high","moderate","high","high","high","high","high","high","high","moderate","moderate","high","moderate","high","high","low","low","low","low","low","low","high","high","high","low","high","moderate","moderate","high",null,"low","high","moderate","high","moderate","moderate","high","low","moderate","high","moderate","low","high","low","high","moderate","moderate","high","low","high","moderate","high","low","high","high","low","low","low","low","moderate","high","moderate","high","high","low","low","high","low","moderate","moderate","high","low","high","low","high","moderate","high","high","high","moderate","high","moderate","low","high","low","high","moderate","low","high","high","high","moderate","high","high","moderate","low","high","low","low","moderate","high","low","high","low","low","moderate","high","moderate","low","low","moderate","high","low","high","moderate","high","moderate","low","low","high","high","low","high","moderate","high","high","moderate","moderate","high","high","high","moderate","moderate","low","moderate","moderate","moderate","moderate","high","moderate","low","low","high","high","high","high","moderate","low","high","high","low","high","low","high","high","high","high","moderate","moderate","moderate","high","low","high","high","high","moderate","high","high","high","low","high","moderate","high","moderate","high","moderate","low","moderate","moderate","moderate","moderate","moderate","high","moderate","high","moderate","high","low","high","moderate","moderate","low","low","low","high","moderate","low","moderate","high","moderate","high","high","moderate","moderate"],"SurgeryDate":["2019-09-19T00:00:00","2018-04-08T00:00:00","2018-11-09T00:00:00","2019-06-04T00:00:00","2018-10-20T00:00:00","2018-07-29T00:00:00","2018-10-11T00:00:00",null,"2019-04-03T00:00:00","2018-12-25T00:00:00","2018-10-03T00:00:00","2019-02-05T00:00:00","2018-08-10T00:00:00","2019-08-10T00:00:00","2018-10-09T00:00:00","2018-10-27T00:00:00","2018-07-28T00:00:00","2019-04-16T00:00:00","2019-03-03T00:00:00","2019-01-11T00:00:00","2018-06-01T00:00:00","2019-04-18T00:00:00","2018-05-16T00:00:00","2019-04-07T00:00:00","2019-03-22T00:00:00","2019-06-02T00:00:00","2019-05-16T00:00:00","2019-06-10T00:00:00","2018-08-07T00:00:00","2019-02-03T00:00:00","2018-05-15T00:00:00","2019-04-14T00:00:00","2019-04-28T00:00:00","2019-07-04T00:00:00","2019-01-18T00:00:00","2019-03-05T00:00:00","2019-01-31T00:00:00","2019-03-10T00:00:00","2019-05-14T00:00:00","2019-05-03T00:00:00","2018-12-06T00:00:00","2019-02-02T00:00:00","2018-10-28T00:00:00","2019-03-03T00:00:00","2019-01-29T00:00:00","2019-04-20T00:00:00","2019-06-21T00:00:00","2018-11-06T00:00:00","2019-06-13T00:00:00","2019-07-02T00:00:00","2018-06-01T00:00:00","2018-12-25T00:00:00","2019-04-26T00:00:00","2019-06-29T00:00:00","2018-12-20T00:00:00","2019-01-16T00:00:00","2018-12-17T00:00:00","2018-05-20T00:00:00","2019-03-03T00:00:00","2019-02-06T00:00:00","2019-06-09T00:00:00","2018-12-20T00:00:00","2019-07-18T00:00:00","2018-11-28T00:00:00","2019-07-03T00:00:00","2018-12-10T00:00:00","2018-10-09T00:00:00","2019-03-11T00:00:00","2019-05-09T00:00:00","2019-03-12T00:00:00","2019-10-11T00:00:00","2019-09-18T00:00:00","2018-11-16T00:00:00","2018-11-15T00:00:00","2018-06-25T00:00:00","2019-01-28T00:00:00","2019-02-01T00:00:00","2019-06-04T00:00:00","2019-08-19T00:00:00","2019-01-23T00:00:00","2019-03-15T00:00:00","2019-05-19T00:00:00","2019-05-11T00:00:00","2019-08-04T00:00:00","2018-11-13T00:00:00","2018-11-19T00:00:00","2018-11-16T00:00:00","2018-11-22T00:00:00","2019-05-18T00:00:00","2019-03-12T00:00:00","2019-01-12T00:00:00","2018-06-01T00:00:00","2019-05-18T00:00:00","2019-05-05T00:00:00","2019-11-06T00:00:00","2019-04-15T00:00:00","2018-10-25T00:00:00","2019-05-08T00:00:00","2018-04-11T00:00:00","2018-04-06T00:00:00","2018-07-05T00:00:00","2018-04-03T00:00:00","2018-04-11T00:00:00","2019-08-05T00:00:00","2019-03-07T00:00:00","2019-08-23T00:00:00","2019-08-28T00:00:00","2019-01-06T00:00:00","2019-04-16T00:00:00","2018-07-21T00:00:00","2018-10-23T00:00:00","2018-10-03T00:00:00","2018-06-18T00:00:00","2018-11-05T00:00:00","2018-11-03T00:00:00","2019-05-09T00:00:00","2018-05-18T00:00:00","2018-07-17T00:00:00","2018-09-12T00:00:00","2019-03-08T00:00:00","2018-07-04T00:00:00","2019-01-22T00:00:00","2018-11-14T00:00:00","2019-11-08T00:00:00","2018-11-18T00:00:00","2018-09-12T00:00:00","2019-01-30T00:00:00","2018-10-18T00:00:00","2018-12-19T00:00:00","2019-04-28T00:00:00","2018-11-21T00:00:00","2018-12-04T00:00:00","2019-01-14T00:00:00","2019-10-28T00:00:00","2018-10-11T00:00:00","2019-04-20T00:00:00","2018-11-18T00:00:00","2019-02-25T00:00:00","2018-08-12T00:00:00","2018-11-08T00:00:00","2018-11-06T00:00:00","2018-07-20T00:00:00","2018-06-09T00:00:00","2019-04-19T00:00:00","2018-11-25T00:00:00","2019-05-05T00:00:00","2019-04-13T00:00:00","2019-07-18T00:00:00","2019-09-07T00:00:00","2019-02-27T00:00:00","2017-07-07T00:00:00","2016-10-08T00:00:00","2017-06-29T00:00:00","2017-09-06T00:00:00","2017-09-24T00:00:00","2017-06-03T00:00:00","2017-03-31T00:00:00","2017-04-23T00:00:00","2017-05-12T00:00:00","2018-03-21T00:00:00","2016-09-05T00:00:00","2017-06-07T00:00:00","2018-01-05T00:00:00","2018-07-13T00:00:00","2017-07-15T00:00:00","2017-08-06T00:00:00","2018-10-19T00:00:00","2018-12-24T00:00:00","2017-10-27T00:00:00","2017-05-19T00:00:00","2017-09-09T00:00:00","2018-02-28T00:00:00","2017-04-30T00:00:00","2016-10-02T00:00:00","2017-06-29T00:00:00","2017-04-22T00:00:00","2017-07-11T00:00:00","2017-06-26T00:00:00","2016-11-27T00:00:00","2018-06-26T00:00:00","2016-12-07T00:00:00","2018-07-13T00:00:00","2017-07-09T00:00:00","2017-02-11T00:00:00","2016-11-29T00:00:00","2017-06-03T00:00:00","2016-09-19T00:00:00","2018-06-26T00:00:00","2018-08-09T00:00:00","2018-03-03T00:00:00","2017-08-04T00:00:00","2017-04-23T00:00:00","2017-02-15T00:00:00","2018-10-12T00:00:00","2017-11-23T00:00:00","2017-11-07T00:00:00","2017-10-29T00:00:00","2017-03-12T00:00:00","2017-05-06T00:00:00","2016-09-20T00:00:00","2018-08-25T00:00:00","2018-06-29T00:00:00","2017-09-04T00:00:00","2017-11-07T00:00:00","2016-07-23T00:00:00","2017-11-09T00:00:00","2018-03-30T00:00:00","2018-03-31T00:00:00","2017-01-05T00:00:00","2018-04-25T00:00:00","2018-08-22T00:00:00","2017-05-25T00:00:00","2017-01-05T00:00:00","2016-09-14T00:00:00","2017-03-17T00:00:00","2017-06-30T00:00:00","2017-07-17T00:00:00","2017-08-18T00:00:00","2018-06-24T00:00:00","2017-05-13T00:00:00","2017-06-11T00:00:00","2018-04-08T00:00:00","2018-02-03T00:00:00","2018-01-08T00:00:00","2017-10-04T00:00:00","2015-12-12T00:00:00","2014-12-17T00:00:00","2014-10-05T00:00:00","2015-12-09T00:00:00","2015-12-29T00:00:00","2015-07-10T00:00:00","2015-04-15T00:00:00","2015-07-26T00:00:00","2015-05-10T00:00:00","2015-02-17T00:00:00","2015-03-23T00:00:00","2015-11-17T00:00:00","2015-05-24T00:00:00","2016-01-18T00:00:00","2015-07-13T00:00:00","2015-03-24T00:00:00","2016-03-07T00:00:00","2015-10-04T00:00:00","2014-12-16T00:00:00","2014-10-01T00:00:00","2016-08-19T00:00:00","2016-01-26T00:00:00","2015-07-12T00:00:00","2015-10-06T00:00:00","2015-06-27T00:00:00"],"DeathTime":["Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","MoreThan1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","MoreThan1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year"]},"columns":[{"accessor":"ID","name":"ID","type":"character"},{"accessor":"Name","name":"Name","type":"character"},{"accessor":"Sex","name":"Sex","type":"character"},{"accessor":"Age","name":"Age","type":"numeric"},{"accessor":"Race","name":"Race","type":"character"},{"accessor":"PreinvasiveComponent","name":"PreinvasiveComponent","type":"character"},{"accessor":"LVI","name":"LVI","type":"character"},{"accessor":"PNI","name":"PNI","type":"character"},{"accessor":"LastFollowUpDate","name":"LastFollowUpDate","type":"Date"},{"accessor":"Death","name":"Death","type":"logical"},{"accessor":"Group","name":"Group","type":"character"},{"accessor":"Grade","name":"Grade","type":"character"},{"accessor":"TStage","name":"TStage","type":"character"},{"accessor":"AntiX_intensity","name":"AntiX_intensity","type":"numeric"},{"accessor":"AntiY_intensity","name":"AntiY_intensity","type":"numeric"},{"accessor":"LymphNodeMetastasis","name":"LymphNodeMetastasis","type":"character"},{"accessor":"Valid","name":"Valid","type":"logical"},{"accessor":"Smoker","name":"Smoker","type":"logical"},{"accessor":"Grade_Level","name":"Grade_Level","type":"character"},{"accessor":"SurgeryDate","name":"SurgeryDate","type":"Date"},{"accessor":"DeathTime","name":"DeathTime","type":"character"}],"resizable":true,"filterable":true,"searchable":true,"defaultPageSize":10,"showPageSizeOptions":true,"pageSizeOptions":[10,25,50,100],"paginationType":"numbers","showPageInfo":true,"minRows":1,"highlight":true,"outlined":true,"striped":true,"compact":true,"nowrap":true,"showSortable":true,"dataKey":"359047058bf3fa3d463d6084cb7b7fa6"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->








### Overview / Exploratory Data Analysis (EDA)


**Summary of Data via summarytools 📦**



```r
summarytools::view(summarytools::dfSummary(mydata %>% select(-keycolumns)))
```



```r
if (!dir.exists(here::here("out"))) {
    dir.create(here::here("out"))
}

summarytools::view(x = summarytools::dfSummary(mydata %>% select(-keycolumns)), file = here::here("out", 
    "mydata_summary.html"))
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

mydata %>% select(-dateVariables) %>% explore::report(output_file = "mydata_report.html", 
    output_dir = here::here("out"))
```




**Glimpse of Data**



```r
glimpse(mydata %>% select(-keycolumns, -dateVariables))
```

```
Observations: 250
Variables: 17
$ Sex                  <chr> "Male", "Male", "Female", "Male", "Female", "Mal…
$ Age                  <dbl> 26, 31, 44, 70, 38, 67, 65, 54, 49, 41, 26, 53, …
$ Race                 <chr> "White", "White", "White", "White", "White", "Wh…
$ PreinvasiveComponent <chr> "Absent", "Absent", "Absent", "Present", "Absent…
$ LVI                  <chr> "Absent", "Present", "Absent", "Present", "Absen…
$ PNI                  <chr> "Absent", "Absent", "Absent", "Absent", "Present…
$ Death                <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,…
$ Group                <chr> "Treatment", "Control", "Treatment", "Treatment"…
$ Grade                <chr> "3", "3", "2", "3", "2", "1", "3", "3", "2", "3"…
$ TStage               <chr> "1", "4", "3", "4", "3", "4", "4", "4", "4", "3"…
$ AntiX_intensity      <dbl> 3, 2, 2, 3, 2, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2, …
$ AntiY_intensity      <dbl> 3, 2, 3, 3, 1, 1, 2, 2, 2, 3, 3, 2, 2, 3, 3, 2, …
$ LymphNodeMetastasis  <chr> "Absent", "Absent", "Absent", "Absent", "Absent"…
$ Valid                <lgl> TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FA…
$ Smoker               <lgl> FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, T…
$ Grade_Level          <chr> "low", "moderate", "high", "low", "high", "moder…
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
 4 Age                  dbl       1    0.4     50    25 50.1     73
 5 Race                 chr       1    0.4      7    NA NA       NA
 6 PreinvasiveComponent chr       1    0.4      3    NA NA       NA
 7 LVI                  chr       0    0        2    NA NA       NA
 8 PNI                  chr       1    0.4      3    NA NA       NA
 9 LastFollowUpDate     dat       1    0.4     13    NA NA       NA
10 Death                lgl       1    0.4      3     0  0.74     1
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
1                 Valid 250   1 0.4%   119 47.6%      0      -    0    -
2                Smoker 250   1 0.4%   115   46%      0      -    0    -
3                 Death 250   1 0.4%    64 25.6%      0      -    0    -
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
1          3   Logical               48%
2          3   Logical             46.4%
3          3   Logical               26%
4          3 Character              0.4%
5          3 Character              0.4%
6          3 Character              0.4%
7          3 Character              0.4%
8          3 Character              0.4%
9          4 Character              0.4%
10         4   Numeric              0.4%
11         4   Numeric              0.4%
12         4 Character              0.4%
13         7 Character              0.4%
14        13 Timestamp              0.4%
15        50   Numeric              0.4%
16       219 Timestamp              0.4%
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
2 AntiY_intensity   1    1    1    2    3    3    3
3             Age  25   30   38   51   63   70   73
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
  - ID: 250 entries: 001, n = 1; 002, n = 1; 003, n = 1 and 247 others
  - Name: 249 entries: Aahaan, n = 1; Abdihamid, n = 1; Abdulkarim, n = 1 and 246 others (1 missing)
  - Sex: 2 entries: Female, n = 139; Male, n = 110 (1 missing)
  - Age: Mean = 50.09, SD = 14.54, range = [25, 73], 1 missing
  - Race: 6 entries: White, n = 165; Hispanic, n = 37; Black, n = 33 and 3 others (1 missing)
  - PreinvasiveComponent: 2 entries: Absent, n = 193; Present, n = 56 (1 missing)
  - LVI: 2 entries: Absent, n = 161; Present, n = 89
  - PNI: 2 entries: Absent, n = 169; Present, n = 80 (1 missing)
  - Death: 2 levels: FALSE (n = 64); TRUE (n = 185) and missing (n = 1)
  - Group: 2 entries: Treatment, n = 137; Control, n = 112 (1 missing)
  - Grade: 3 entries: 3, n = 112; 2, n = 71; 1, n = 66 (1 missing)
  - TStage: 4 entries: 4, n = 108; 3, n = 85; 2, n = 37 and 1 other
  - AntiX_intensity: Mean = 2.38, SD = 0.66, range = [1, 3], 1 missing
  - AntiY_intensity: Mean = 2.02, SD = 0.76, range = [1, 3], 1 missing
  - LymphNodeMetastasis: 2 entries: Absent, n = 158; Present, n = 91 (1 missing)
  - Valid: 2 levels: FALSE (n = 119); TRUE (n = 130) and missing (n = 1)
  - Smoker: 2 levels: FALSE (n = 115); TRUE (n = 134) and missing (n = 1)
  - Grade_Level: 3 entries: high, n = 110; moderate, n = 77; low, n = 62 (1 missing)
  - DeathTime: 2 entries: Within1Year, n = 148; MoreThan1Year, n = 102
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
|&nbsp;&nbsp;&nbsp;Female    |   139 (55.8%)   |
|&nbsp;&nbsp;&nbsp;Male      |   110 (44.2%)   |
|**Age**                     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Mean (SD) | 50.092 (14.544) |
|&nbsp;&nbsp;&nbsp;Range     | 25.000 - 73.000 |
|**Race**                    |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Asian     |    6 (2.4%)     |
|&nbsp;&nbsp;&nbsp;Bi-Racial |    4 (1.6%)     |
|&nbsp;&nbsp;&nbsp;Black     |   33 (13.3%)    |
|&nbsp;&nbsp;&nbsp;Hispanic  |   37 (14.9%)    |
|&nbsp;&nbsp;&nbsp;Native    |    4 (1.6%)     |
|&nbsp;&nbsp;&nbsp;White     |   165 (66.3%)   |
|**PreinvasiveComponent**    |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   193 (77.5%)   |
|&nbsp;&nbsp;&nbsp;Present   |   56 (22.5%)    |
|**LVI**                     |                 |
|&nbsp;&nbsp;&nbsp;Absent    |   161 (64.4%)   |
|&nbsp;&nbsp;&nbsp;Present   |   89 (35.6%)    |
|**PNI**                     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   169 (67.9%)   |
|&nbsp;&nbsp;&nbsp;Present   |   80 (32.1%)    |
|**Death**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;FALSE     |   64 (25.7%)    |
|&nbsp;&nbsp;&nbsp;TRUE      |   185 (74.3%)   |
|**Group**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Control   |   112 (45.0%)   |
|&nbsp;&nbsp;&nbsp;Treatment |   137 (55.0%)   |
|**Grade**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;1         |   66 (26.5%)    |
|&nbsp;&nbsp;&nbsp;2         |   71 (28.5%)    |
|&nbsp;&nbsp;&nbsp;3         |   112 (45.0%)   |
|**TStage**                  |                 |
|&nbsp;&nbsp;&nbsp;1         |    20 (8.0%)    |
|&nbsp;&nbsp;&nbsp;2         |   37 (14.8%)    |
|&nbsp;&nbsp;&nbsp;3         |   85 (34.0%)    |
|&nbsp;&nbsp;&nbsp;4         |   108 (43.2%)   |
|**LymphNodeMetastasis**     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   158 (63.5%)   |
|&nbsp;&nbsp;&nbsp;Present   |   91 (36.5%)    |
|**Valid**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;FALSE     |   119 (47.8%)   |
|&nbsp;&nbsp;&nbsp;TRUE      |   130 (52.2%)   |
|**Smoker**                  |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;FALSE     |   115 (46.2%)   |
|&nbsp;&nbsp;&nbsp;TRUE      |   134 (53.8%)   |
|**Grade_Level**             |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;high      |   110 (44.2%)   |
|&nbsp;&nbsp;&nbsp;low       |   62 (24.9%)    |
|&nbsp;&nbsp;&nbsp;moderate  |   77 (30.9%)    |



**Table 1 via tableone 📦**

<!-- https://cran.r-project.org/web/packages/tableone/index.html -->
<!-- https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html -->
<!-- library(survival) -->
<!-- data(pbc) -->



```r
library(tableone)
mydata %>% select(-keycolumns, -dateVariables) %>% tableone::CreateTableOne(data = .)
```

```
                                    
                                     Overall      
  n                                    250        
  Sex = Male (%)                       110 (44.2) 
  Age (mean (SD))                    50.09 (14.54)
  Race (%)                                        
     Asian                               6 ( 2.4) 
     Bi-Racial                           4 ( 1.6) 
     Black                              33 (13.3) 
     Hispanic                           37 (14.9) 
     Native                              4 ( 1.6) 
     White                             165 (66.3) 
  PreinvasiveComponent = Present (%)    56 (22.5) 
  LVI = Present (%)                     89 (35.6) 
  PNI = Present (%)                     80 (32.1) 
  Death = TRUE (%)                     185 (74.3) 
  Group = Treatment (%)                137 (55.0) 
  Grade (%)                                       
     1                                  66 (26.5) 
     2                                  71 (28.5) 
     3                                 112 (45.0) 
  TStage (%)                                      
     1                                  20 ( 8.0) 
     2                                  37 (14.8) 
     3                                  85 (34.0) 
     4                                 108 (43.2) 
  AntiX_intensity (mean (SD))         2.38 (0.66) 
  AntiY_intensity (mean (SD))         2.02 (0.76) 
  LymphNodeMetastasis = Present (%)     91 (36.5) 
  Valid = TRUE (%)                     130 (52.2) 
  Smoker = TRUE (%)                    134 (53.8) 
  Grade_Level (%)                                 
     high                              110 (44.2) 
     low                                62 (24.9) 
     moderate                           77 (30.9) 
  DeathTime = Within1Year (%)          148 (59.2) 
```



















<!-- **Table 1 via atable 📦** -->

<!-- https://cran.r-project.org/web/packages/atable/vignettes/atable_usage.pdf -->
<!-- https://journal.r-project.org/archive/2019/RJ-2019-001/index.html -->



**Descriptive Statistics of Continuous Variables**


```r
mydata %>% select(continiousVariables, numericVariables, integerVariables) %>% summarytools::descr(., 
    style = "rmarkdown")
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
 5 Death                lgl       1    0.4      3     0  0.74     1
 6 Group                chr       1    0.4      3    NA NA       NA
 7 Grade                chr       1    0.4      4    NA NA       NA
 8 TStage               chr       0    0        4    NA NA       NA
 9 AntiX_intensity      dbl       1    0.4      4     1  2.38     3
10 AntiY_intensity      dbl       1    0.4      4     1  2.02     3
11 LymphNodeMetastasis  chr       1    0.4      3    NA NA       NA
12 Valid                lgl       1    0.4      3     0  0.52     1
13 Smoker               lgl       1    0.4      3     0  0.54     1
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
 3 Age                  dbl       1    0.4     50    25 50.1     73
 4 Race                 chr       1    0.4      7    NA NA       NA
 5 PreinvasiveComponent chr       1    0.4      3    NA NA       NA
 6 PNI                  chr       1    0.4      3    NA NA       NA
 7 LastFollowUpDate     dat       1    0.4     13    NA NA       NA
 8 Death                lgl       1    0.4      3     0  0.74     1
 9 Group                chr       1    0.4      3    NA NA       NA
10 Grade                chr       1    0.4      4    NA NA       NA
11 AntiX_intensity      dbl       1    0.4      4     1  2.38     3
12 AntiY_intensity      dbl       1    0.4      4     1  2.02     3
13 LymphNodeMetastasis  chr       1    0.4      3    NA NA       NA
14 Valid                lgl       1    0.4      3     0  0.52     1
15 Smoker               lgl       1    0.4      3     0  0.54     1
16 Grade_Level          chr       1    0.4      4    NA NA       NA
17 SurgeryDate          dat       1    0.4    219    NA NA       NA
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
 4 Age                  dbl       1    0.4     50    25 50.1     73
 5 Race                 chr       1    0.4      7    NA NA       NA
 6 PreinvasiveComponent chr       1    0.4      3    NA NA       NA
 7 LVI                  chr       0    0        2    NA NA       NA
 8 PNI                  chr       1    0.4      3    NA NA       NA
 9 LastFollowUpDate     dat       1    0.4     13    NA NA       NA
10 Death                lgl       1    0.4      3     0  0.74     1
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
Female    139  55.6%     55.8%         
Male      110  44.0%     44.2%         
NA          1  0.4%      -             

\pagebreak

#### Descriptive Statistics Race  


```r
mydata %>% janitor::tabyl(Race) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Race           n  percent   valid_percent 
----------  ----  --------  --------------
Asian          6  2.4%      2.4%          
Bi-Racial      4  1.6%      1.6%          
Black         33  13.2%     13.3%         
Hispanic      37  14.8%     14.9%         
Native         4  1.6%      1.6%          
White        165  66.0%     66.3%         
NA             1  0.4%      -             

\pagebreak

#### Descriptive Statistics PreinvasiveComponent  


```r
mydata %>% janitor::tabyl(PreinvasiveComponent) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



PreinvasiveComponent      n  percent   valid_percent 
---------------------  ----  --------  --------------
Absent                  193  77.2%     77.5%         
Present                  56  22.4%     22.5%         
NA                        1  0.4%      -             

\pagebreak

#### Descriptive Statistics LVI  


```r
mydata %>% janitor::tabyl(LVI) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



LVI          n  percent 
--------  ----  --------
Absent     161  64.4%   
Present     89  35.6%   

\pagebreak

#### Descriptive Statistics PNI  


```r
mydata %>% janitor::tabyl(PNI) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



PNI          n  percent   valid_percent 
--------  ----  --------  --------------
Absent     169  67.6%     67.9%         
Present     80  32.0%     32.1%         
NA           1  0.4%      -             

\pagebreak

#### Descriptive Statistics Group  


```r
mydata %>% janitor::tabyl(Group) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Group          n  percent   valid_percent 
----------  ----  --------  --------------
Control      112  44.8%     45.0%         
Treatment    137  54.8%     55.0%         
NA             1  0.4%      -             

\pagebreak

#### Descriptive Statistics Grade  


```r
mydata %>% janitor::tabyl(Grade) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Grade      n  percent   valid_percent 
------  ----  --------  --------------
1         66  26.4%     26.5%         
2         71  28.4%     28.5%         
3        112  44.8%     45.0%         
NA         1  0.4%      -             

\pagebreak

#### Descriptive Statistics TStage  


```r
mydata %>% janitor::tabyl(TStage) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



TStage      n  percent 
-------  ----  --------
1          20  8.0%    
2          37  14.8%   
3          85  34.0%   
4         108  43.2%   

\pagebreak

#### Descriptive Statistics LymphNodeMetastasis  


```r
mydata %>% janitor::tabyl(LymphNodeMetastasis) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



LymphNodeMetastasis      n  percent   valid_percent 
--------------------  ----  --------  --------------
Absent                 158  63.2%     63.5%         
Present                 91  36.4%     36.5%         
NA                       1  0.4%      -             

\pagebreak

#### Descriptive Statistics Grade_Level  


```r
mydata %>% janitor::tabyl(Grade_Level) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Grade_Level      n  percent   valid_percent 
------------  ----  --------  --------------
high           110  44.0%     44.2%         
low             62  24.8%     24.9%         
moderate        77  30.8%     30.9%         
NA               1  0.4%      -             

\pagebreak

#### Descriptive Statistics DeathTime  


```r
mydata %>% janitor::tabyl(DeathTime) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



DeathTime          n  percent 
--------------  ----  --------
MoreThan1Year    102  40.8%   
Within1Year      148  59.2%   

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
 Absent  = 193 (77.2%)
 Present = 56 (22.4%)
 NA      = 1 (0.4%)
```




```r
## Frequency or custom tables for categorical variables
SmartEDA::ExpCTable(mydata, Target = NULL, margin = 1, clim = 10, nlim = 5, round = 2, 
    bin = NULL, per = T)
```

```
               Variable         Valid Frequency Percent CumPercent
1                   Sex        Female       139    55.6       55.6
2                   Sex          Male       110    44.0       99.6
3                   Sex            NA         1     0.4      100.0
4                   Sex         TOTAL       250      NA         NA
5                  Race         Asian         6     2.4        2.4
6                  Race     Bi-Racial         4     1.6        4.0
7                  Race         Black        33    13.2       17.2
8                  Race      Hispanic        37    14.8       32.0
9                  Race            NA         1     0.4       32.4
10                 Race        Native         4     1.6       34.0
11                 Race         White       165    66.0      100.0
12                 Race         TOTAL       250      NA         NA
13 PreinvasiveComponent        Absent       193    77.2       77.2
14 PreinvasiveComponent            NA         1     0.4       77.6
15 PreinvasiveComponent       Present        56    22.4      100.0
16 PreinvasiveComponent         TOTAL       250      NA         NA
17                  LVI        Absent       161    64.4       64.4
18                  LVI       Present        89    35.6      100.0
19                  LVI         TOTAL       250      NA         NA
20                  PNI        Absent       169    67.6       67.6
21                  PNI            NA         1     0.4       68.0
22                  PNI       Present        80    32.0      100.0
23                  PNI         TOTAL       250      NA         NA
24                Group       Control       112    44.8       44.8
25                Group            NA         1     0.4       45.2
26                Group     Treatment       137    54.8      100.0
27                Group         TOTAL       250      NA         NA
28                Grade             1        66    26.4       26.4
29                Grade             2        71    28.4       54.8
30                Grade             3       112    44.8       99.6
31                Grade            NA         1     0.4      100.0
32                Grade         TOTAL       250      NA         NA
33               TStage             1        20     8.0        8.0
34               TStage             2        37    14.8       22.8
35               TStage             3        85    34.0       56.8
36               TStage             4       108    43.2      100.0
37               TStage         TOTAL       250      NA         NA
38  LymphNodeMetastasis        Absent       158    63.2       63.2
39  LymphNodeMetastasis            NA         1     0.4       63.6
40  LymphNodeMetastasis       Present        91    36.4      100.0
41  LymphNodeMetastasis         TOTAL       250      NA         NA
42          Grade_Level          high       110    44.0       44.0
43          Grade_Level           low        62    24.8       68.8
44          Grade_Level      moderate        77    30.8       99.6
45          Grade_Level            NA         1     0.4      100.0
46          Grade_Level         TOTAL       250      NA         NA
47            DeathTime MoreThan1Year       102    40.8       40.8
48            DeathTime   Within1Year       148    59.2      100.0
49            DeathTime         TOTAL       250      NA         NA
50      AntiX_intensity             1        25    10.0       10.0
51      AntiX_intensity             2       104    41.6       51.6
52      AntiX_intensity             3       120    48.0       99.6
53      AntiX_intensity            NA         1     0.4      100.0
54      AntiX_intensity         TOTAL       250      NA         NA
55      AntiY_intensity             1        69    27.6       27.6
56      AntiY_intensity             2       105    42.0       69.6
57      AntiY_intensity             3        75    30.0       99.6
58      AntiY_intensity            NA         1     0.4      100.0
59      AntiY_intensity         TOTAL       250      NA         NA
```




```r
inspectdf::inspect_cat(mydata)
```

```
# A tibble: 16 x 5
   col_name               cnt common      common_pcnt levels            
   <chr>                <int> <chr>             <dbl> <named list>      
 1 Death                    3 TRUE               74   <tibble [3 × 3]>  
 2 DeathTime                2 Within1Year        59.2 <tibble [2 × 3]>  
 3 Grade                    4 3                  44.8 <tibble [4 × 3]>  
 4 Grade_Level              4 high               44   <tibble [4 × 3]>  
 5 Group                    3 Treatment          54.8 <tibble [3 × 3]>  
 6 ID                     250 001                 0.4 <tibble [250 × 3]>
 7 LVI                      2 Absent             64.4 <tibble [2 × 3]>  
 8 LymphNodeMetastasis      3 Absent             63.2 <tibble [3 × 3]>  
 9 Name                   250 Aahaan              0.4 <tibble [250 × 3]>
10 PNI                      3 Absent             67.6 <tibble [3 × 3]>  
11 PreinvasiveComponent     3 Absent             77.2 <tibble [3 × 3]>  
12 Race                     7 White              66   <tibble [7 × 3]>  
13 Sex                      3 Female             55.6 <tibble [3 × 3]>  
14 Smoker                   3 TRUE               53.6 <tibble [3 × 3]>  
15 TStage                   4 4                  43.2 <tibble [4 × 3]>  
16 Valid                    3 TRUE               52   <tibble [3 × 3]>  
```

```r
inspectdf::inspect_cat(mydata)$levels$Group
```

```
# A tibble: 3 x 3
  value      prop   cnt
  <chr>     <dbl> <int>
1 Treatment 0.548   137
2 Control   0.448   112
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
mydata %>% select(characterVariables) %>% select(PreinvasiveComponent, PNI, LVI) %>% 
    reactable::reactable(data = ., groupBy = c("PreinvasiveComponent", "PNI"), columns = list(LVI = reactable::colDef(aggregate = "count")))
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
 ────────────────────────────────── 
                          Age       
 ────────────────────────────────── 
   N                          249   
   Missing                      1   
   Mean                      50.1   
   Median                    51.0   
   Mode                      70.0   
   Standard deviation        14.5   
   Variance                   212   
   Minimum                   25.0   
   Maximum                   73.0   
   Skewness               -0.0856   
   Std. error skewness      0.154   
   Kurtosis                 -1.24   
   Std. error kurtosis      0.307   
   25th percentile           38.0   
   50th percentile           51.0   
   75th percentile           63.0   
 ────────────────────────────────── 
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
   Mean                              2.38   
   Median                            2.00   
   Mode                              3.00   
   Standard deviation               0.662   
   Variance                         0.439   
   Minimum                           1.00   
   Maximum                           3.00   
   Skewness                        -0.606   
   Std. error skewness              0.154   
   Kurtosis                        -0.656   
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
   Mean                              2.02   
   Median                            2.00   
   Mode                              2.00   
   Standard deviation               0.762   
   Variance                         0.580   
   Minimum                           1.00   
   Maximum                           3.00   
   Skewness                       -0.0405   
   Std. error skewness              0.154   
   Kurtosis                         -1.27   
   Std. error kurtosis              0.307   
   25th percentile                   1.00   
   50th percentile                   2.00   
   75th percentile                   3.00   
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
  Age (mean (SD))             50.09 (14.54)
  AntiX_intensity (mean (SD))  2.38 (0.66) 
  AntiY_intensity (mean (SD))  2.02 (0.76) 
```

```r
print(tab$ContTable, nonnormal = c("Anti-X-intensity"))
```

```
                             
                              Overall      
  n                           250          
  Age (mean (SD))             50.09 (14.54)
  AntiX_intensity (mean (SD))  2.38 (0.66) 
  AntiY_intensity (mean (SD))  2.02 (0.76) 
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
q05|q95  = 27 | 71.6
q25|q75  = 38 | 63
median   = 51
mean     = 50.09237
```



```r
mydata %>% select(continiousVariables) %>% SmartEDA::ExpNumStat(data = ., by = "A", 
    gp = NULL, Qnt = seq(0, 1, 0.1), MesofShape = 2, Outlier = TRUE, round = 2)
```




```r
inspectdf::inspect_num(mydata, breaks = 10)
```

```
# A tibble: 3 x 10
  col_name        min    q1 median  mean    q3   max     sd pcnt_na hist        
  <chr>         <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl> <named list>
1 Age              25    38     51 50.1     63    73 14.5       0.4 <tibble [12…
2 AntiX_intens…     1     2      2  2.38     3     3  0.662     0.4 <tibble [12…
3 AntiY_intens…     1     1      2  2.02     3     3  0.762     0.4 <tibble [12…
```



```r
inspectdf::inspect_num(mydata)$hist$Age
```

```
# A tibble: 27 x 2
   value        prop
   <chr>       <dbl>
 1 [-Inf, 24) 0     
 2 [24, 26)   0.0201
 3 [26, 28)   0.0442
 4 [28, 30)   0.0281
 5 [30, 32)   0.0522
 6 [32, 34)   0.0321
 7 [34, 36)   0.0402
 8 [36, 38)   0.0321
 9 [38, 40)   0.0361
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
2   Age  PreinvasiveComponent:Absent 193    0     0  193      0      0        0
3   Age PreinvasiveComponent:Present  56    0     0   55      0      0        1
4   Age      PreinvasiveComponent:NA   0    0     0    0      0      0        0
  Per_of_Missing   sum min  max  mean median    SD   CV  IQR Skewness Kurtosis
1           0.40 12473  25   73 50.09     51 14.54 0.29 25.0    -0.09    -1.24
2           0.00  9572  25   73 49.60     51 14.53 0.29 26.0    -0.08    -1.27
3           1.79  2865  26   73 52.09     52 14.57 0.28 22.5    -0.14    -1.14
4            NaN     0 Inf -Inf   NaN     NA    NA   NA   NA      NaN      NaN
  0%  10%  20%  30%  40% 50%  60%  70%  80%  90% 100% LB.25% UB.75% nOutliers
1 25 30.0 35.0 41.0 45.0  51 55.0 61.0 65.4 70.0   73   0.50 100.50         0
2 25 29.2 34.4 40.0 44.0  51 55.0 60.4 65.0 69.0   73  -2.00 102.00         0
3 26 31.4 36.6 43.4 48.6  52 56.4 61.8 69.2 71.6   73   6.75  96.75         0
4 NA   NA   NA   NA   NA  NA   NA   NA   NA   NA   NA     NA     NA         0
```



\pagebreak












---

<!-- \newpage -->
<!-- \blandscape -->

<!-- ```{r crossTables, child = here::here('childRmd', '_12crossTables.Rmd')} -->
<!-- ``` -->

<!-- \elandscape -->


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
  FALSE  64   0
  TRUE    0 185
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
 [1]  3.7  11.0   7.0   3.2  10.6  10.4   8.9    NA+ 10.2   9.5   8.2   5.1 
[13]  7.0   3.0   5.0   9.4   7.3   9.8   4.2+ 10.9  11.3+  4.7+ 11.8+  3.1+
[25]  7.6+  8.2   8.8   3.0    NA+  6.2+ 10.8   7.8   9.4   6.2+ 10.7   7.1 
[37] 11.3+  4.0+  3.8   9.2   5.1  11.2   5.4   6.2   5.3   6.6   6.6   6.1 
[49]  3.9   5.2   9.3+  7.5   9.5+  6.4+ 10.6   3.8+  4.7  10.6   5.2  11.1 
[61]  3.0   5.6+  4.7+  6.4   7.2   7.0   5.0+  2.9+  5.0   4.9+  3.9   4.7 
[73] 10.8  10.8   8.4  11.4   4.3+  8.2+  4.7+  4.5+
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
LVI                                     Absent     161 (100.0)                           -                           -
                                        Present     89 (100.0)   2.04 (1.49-2.80, p<0.001)   2.04 (1.49-2.80, p<0.001)


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

When LVI is Present, there is 2.04 (1.49-2.80, p<0.001) times risk than when LVI is Absent.

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

When LVI is Present, there is 2.04 (1.49-2.80, p<0.001) times risk than when LVI is Absent.

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
LVI=Absent  158    121   20.7    13.5    26.4
LVI=Present  89     64   10.0     8.9    11.2
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
    dplyr::select(description) %>% pull()
```


<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

When LVI=Absent, median survival is 20.7 [13.5 - 26.4, 95% CI] months., When LVI=Present, median survival is 10 [8.9 - 11.2, 95% CI] months.

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

When LVI=Absent, median survival is 20.7 [13.5 - 26.4, 95% CI] months., When LVI=Present, median survival is 10 [8.9 - 11.2, 95% CI] months.

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
   12     81      58    0.600  0.0409        0.524        0.685
   36     22      47    0.211  0.0372        0.150        0.299

                LVI=Present 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
   12     17      50   0.3112  0.0576       0.2165        0.447
   36      3      12   0.0741  0.0379       0.0272        0.202
```


```r
km_fit_summary <- summary(km_fit, times = c(12, 36, 60))

km_fit_df <- as.data.frame(km_fit_summary[c("strata", "time", "n.risk", "n.event", 
    "surv", "std.err", "lower", "upper")])
```




```r
km_fit_definition <- km_fit_df %>% dplyr::mutate(description = glue::glue("When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI].")) %>% 
    dplyr::select(description) %>% pull()
```

<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

When LVI=Absent, 12 month survival is 60.0% [52.4%-68.5%, 95% CI]., When LVI=Absent, 36 month survival is 21.1% [15.0%-29.9%, 95% CI]., When LVI=Present, 12 month survival is 31.1% [21.7%-44.7%, 95% CI]., When LVI=Present, 36 month survival is 7.4% [2.7%-20.2%, 95% CI].

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

When LVI=Absent, 12 month survival is 60.0% [52.4%-68.5%, 95% CI]., When LVI=Absent, 36 month survival is 21.1% [15.0%-29.9%, 95% CI]., When LVI=Present, 12 month survival is 31.1% [21.7%-44.7%, 95% CI]., When LVI=Present, 36 month survival is 7.4% [2.7%-20.2%, 95% CI].

}
}



























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
LVI=Absent  158    121   20.7    13.5    26.4
LVI=Present  89     64   10.0     8.9    11.2
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
LVI=Absent  158    121   20.7    13.5    26.4
LVI=Present  89     64   10.0     8.9    11.2
```




### Multivariate Analysis Survival






# parsnip









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
saved data after analysis to /Users/serdarbalciold/histopathRprojects/histopathology-template/data/histopathology-template2020-02-09.xlsx : 2020-02-09 14:01:43
```



```r
mydata %>% downloadthis::download_this(output_name = excelName, output_extension = ".csv", 
    button_label = "Download data as csv", button_type = "default")
```

<!--html_preserve--><a href="data:text/csv;base64,SUQsTmFtZSxTZXgsQWdlLFJhY2UsUHJlaW52YXNpdmVDb21wb25lbnQsTFZJLFBOSSxMYXN0Rm9sbG93VXBEYXRlLERlYXRoLEdyb3VwLEdyYWRlLFRTdGFnZSxBbnRpWF9pbnRlbnNpdHksQW50aVlfaW50ZW5zaXR5LEx5bXBoTm9kZU1ldGFzdGFzaXMsVmFsaWQsU21va2VyLEdyYWRlX0xldmVsLFN1cmdlcnlEYXRlLERlYXRoVGltZSxpbnQsT3ZlcmFsbFRpbWUsT3V0Y29tZQowMDEsRW1lcnNvbixNYWxlLDI2LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDEtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMSwzLDMsQWJzZW50LFRSVUUsRkFMU0UsbG93LDIwMTktMDktMTlUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDktMTkgVVRDLS0yMDIwLTAxLTA5IFVUQywzLjcsMQowMDIsSmVuZWxsYSxNYWxlLDMxLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTAzLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDIsMixBYnNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTgtMDQtMDhUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDQtMDggVVRDLS0yMDE5LTAzLTA5IFVUQywxMSwxCjAwMyxCZW5uLEZlbWFsZSw0NCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA2LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDMsMiwzLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTgtMTEtMDlUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMDkgVVRDLS0yMDE5LTA2LTA5IFVUQyw3LDEKMDA0LENocmlzdGlhbm4sTWFsZSw3MCxXaGl0ZSxQcmVzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDktMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwzLDMsQWJzZW50LFRSVUUsRkFMU0UsbG93LDIwMTktMDYtMDRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDYtMDQgVVRDLS0yMDE5LTA5LTA5IFVUQywzLjIsMQowMDUsWmFraGl5YSxGZW1hbGUsMzgsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDktMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDMsMiwxLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTgtMTAtMjBUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMjAgVVRDLS0yMDE5LTA5LTA5IFVUQywxMC42LDEKMDA2LEF0ZWYsTWFsZSw2NyxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNi0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSw0LDIsMSxBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE4LTA3LTI5VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA3LTI5IFVUQy0tMjAxOS0wNi0wOSBVVEMsMTAuNCwxCjAwNyxQYXVsYW5uLEZlbWFsZSw2NSxIaXNwYW5pYyxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDctMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwyLDIsQWJzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxOC0xMC0xMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMC0xMSBVVEMtLTIwMTktMDctMDkgVVRDLDguOSwxCjAwOCxNaXJpYW5uYSxGZW1hbGUsNTQsV2hpdGUsUHJlc2VudCxBYnNlbnQsUHJlc2VudCwyMDIwLTAyLTA5VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsNCwyLDIsUHJlc2VudCxGQUxTRSxUUlVFLGhpZ2gsTkEsTW9yZVRoYW4xWWVhcixOQS0tTkEsTkEsMAowMDksU2VhbmRyYSxGZW1hbGUsNDksSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDIwLTAyLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDMsMixBYnNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOS0wNC0wM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNC0wMyBVVEMtLTIwMjAtMDItMDkgVVRDLDEwLjIsMQowMTAsVHJlcXVhbixNYWxlLDQxLFdoaXRlLFByZXNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEwLTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDMsMywzLEFic2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMTItMjVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTItMjUgVVRDLS0yMDE5LTEwLTA5IFVUQyw5LjUsMQowMTEsTGluZHNlLE1hbGUsMjYsV2hpdGUsUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDYtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsNCwzLDMsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE4LTEwLTAzVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEwLTAzIFVUQy0tMjAxOS0wNi0wOSBVVEMsOC4yLDEKMDEyLEVpdG8sTWFsZSw1MyxXaGl0ZSxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDctMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDIsMSwyLEFic2VudCxUUlVFLEZBTFNFLGxvdywyMDE5LTAyLTA1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAyLTA1IFVUQy0tMjAxOS0wNy0wOSBVVEMsNS4xLDEKMDEzLExlb3J5LEZlbWFsZSw1MSxCbGFjayxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTAzLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwzLDEsMixQcmVzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE4LTA4LTEwVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA4LTEwIFVUQy0tMjAxOS0wMy0wOSBVVEMsNywxCjAxNCxKYXZhbm5pLE1hbGUsMzEsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTExLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDEsMyxBYnNlbnQsRkFMU0UsRkFMU0UsbW9kZXJhdGUsMjAxOS0wOC0xMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wOC0xMCBVVEMtLTIwMTktMTEtMDkgVVRDLDMsMQowMTUsS2F0YXJpbmEsRmVtYWxlLDM1LEhpc3BhbmljLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wMy0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwyLDMsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTgtMTAtMDlUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMDkgVVRDLS0yMDE5LTAzLTA5IFVUQyw1LDEKMDE2LEV1cmEsRmVtYWxlLDM4LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA4LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywzLDIsMixQcmVzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxOC0xMC0yN1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMC0yNyBVVEMtLTIwMTktMDgtMDkgVVRDLDkuNCwxCjAxNyxLbm9lbGxlLEZlbWFsZSw1MyxIaXNwYW5pYyxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTAzLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwxLDIsMSxBYnNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOC0wNy0yOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNy0yOCBVVEMtLTIwMTktMDMtMDkgVVRDLDcuMywxCjAxOCxDaGlsZCxNYWxlLDUyLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDItMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDIsMywyLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOS0wNC0xNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNC0xNiBVVEMtLTIwMjAtMDItMDkgVVRDLDkuOCwxCjAxOSxKYWxpcyxGZW1hbGUsNzAsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDctMDlUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwzLDMsMiwyLEFic2VudCxGQUxTRSxUUlVFLG1vZGVyYXRlLDIwMTktMDMtMDNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDMtMDMgVVRDLS0yMDE5LTA3LTA5IFVUQyw0LjIsMAowMjAsQW5kZXJzc29uLEZlbWFsZSw3MSxXaGl0ZSxQcmVzZW50LEFic2VudCxQcmVzZW50LDIwMTktMTItMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsNCwzLDEsUHJlc2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTktMDEtMTFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMTEgVVRDLS0yMDE5LTEyLTA5IFVUQywxMC45LDEKMDIxLFNoYXJsZW5lLE1hbGUsNjQsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNS0wOVQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsNCwzLDEsQWJzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE4LTA2LTAxVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA2LTAxIFVUQy0tMjAxOS0wNS0wOSBVVEMsMTEuMywwCjAyMixBbHZhcm8sRmVtYWxlLDczLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA5LTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSwyLDEsMixBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTktMDQtMThUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDQtMTggVVRDLS0yMDE5LTA5LTA5IFVUQyw0LjcsMAowMjMsVGFtaXJyYSxGZW1hbGUsMzAsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA1LTA5VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDIsNCwzLDMsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTgtMDUtMTZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDUtMTYgVVRDLS0yMDE5LTA1LTA5IFVUQywxMS44LDAKMDI0LE1vcml0eixGZW1hbGUsMjksV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDctMDlUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwyLDIsMywxLEFic2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOS0wNC0wN1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNC0wNyBVVEMtLTIwMTktMDctMDkgVVRDLDMuMSwwCjAyNSxBZHV0LEZlbWFsZSwzOCxXaGl0ZSxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMTEtMDlUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMywyLDEsMyxBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE5LTAzLTIyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTIyIFVUQy0tMjAxOS0xMS0wOSBVVEMsNy42LDAKMDI2LEl2YW55YSxNYWxlLDQ4LFdoaXRlLFByZXNlbnQsUHJlc2VudCxQcmVzZW50LDIwMjAtMDItMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMiwyLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTktMDYtMDJUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDYtMDIgVVRDLS0yMDIwLTAyLTA5IFVUQyw4LjIsMQowMjcsSGFzdG9uLEZlbWFsZSw3MSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAyLTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDMsMywzLEFic2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxOS0wNS0xNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNS0xNiBVVEMtLTIwMjAtMDItMDkgVVRDLDguOCwxCjAyOCxGcmljYSxGZW1hbGUsNDMsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA5LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDMsMiwyLEFic2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxOS0wNi0xMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNi0xMCBVVEMtLTIwMTktMDktMDkgVVRDLDMsMQowMjksRGVsZW5hLE1hbGUsNDIsQmxhY2ssQWJzZW50LEFic2VudCxBYnNlbnQsTkEsRkFMU0UsQ29udHJvbCwxLDMsMiwxLEFic2VudCxUUlVFLFRSVUUsaGlnaCwyMDE4LTA4LTA3VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDgtMDcgVVRDLS1OQSxOQSwwCjAzMCxHYXJleSxNYWxlLDU5LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA4LTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMiwyLDMsMixBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wMi0wM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0wMyBVVEMtLTIwMTktMDgtMDkgVVRDLDYuMiwwCjAzMSxMYWNobGFubixGZW1hbGUsNjEsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNC0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMywyLDIsQWJzZW50LFRSVUUsRkFMU0UsbG93LDIwMTgtMDUtMTVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDUtMTUgVVRDLS0yMDE5LTA0LTA5IFVUQywxMC44LDEKMDMyLE55a29sYXMsRmVtYWxlLDU4LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTItMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsMywyLDMsUHJlc2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE5LTA0LTE0VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA0LTE0IFVUQy0tMjAxOS0xMi0wOSBVVEMsNy44LDEKMDMzLEthenV5byxGZW1hbGUsMjcsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMi0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSwzLDIsMyxQcmVzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxOS0wNC0yOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNC0yOCBVVEMtLTIwMjAtMDItMDkgVVRDLDkuNCwxCjAzNCxIb3J0b24sRmVtYWxlLDUxLEhpc3BhbmljLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDEtMDlUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwzLDQsMywzLEFic2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTktMDctMDRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDctMDQgVVRDLS0yMDIwLTAxLTA5IFVUQyw2LjIsMAowMzUsTmFxdWFuZGEsRmVtYWxlLDYzLFdoaXRlLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMi0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMywzLDEsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE5LTAxLTE4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTE4IFVUQy0tMjAxOS0xMi0wOSBVVEMsMTAuNywxCjAzNixBbGxpc3RhaXIsTWFsZSw0MyxIaXNwYW5pYyxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEwLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDIsMyxQcmVzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE5LTAzLTA1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTA1IFVUQy0tMjAxOS0xMC0wOSBVVEMsNy4xLDEKMDM3LEplYW5uZSxNYWxlLDMxLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDEtMDlUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwzLDMsMywyLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTktMDEtMzFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMzEgVVRDLS0yMDIwLTAxLTA5IFVUQywxMS4zLDAKMDM4LFNoaXBocmFoLEZlbWFsZSwzMSxBc2lhbixBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA3LTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMyw0LDMsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE5LTAzLTEwVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTEwIFVUQy0tMjAxOS0wNy0wOSBVVEMsNCwwCjAzOSxNYWRkaXN5bixNYWxlLDYzLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA5LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMywxLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOS0wNS0xNFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNS0xNCBVVEMtLTIwMTktMDktMDkgVVRDLDMuOCwxCjA0MCxLYW1zaXlvY2hpLEZlbWFsZSw2NixXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAyLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDMsMixQcmVzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE5LTA1LTAzVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA1LTAzIFVUQy0tMjAyMC0wMi0wOSBVVEMsOS4yLDEKMDQxLEphcXVhbGxhLEZlbWFsZSw0MSxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNS0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMywyLDIsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE4LTEyLTA2VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEyLTA2IFVUQy0tMjAxOS0wNS0wOSBVVEMsNS4xLDEKMDQyLERlbHRyaWNrLE1hbGUsNTcsSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMjAtMDEtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsNCwzLDEsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxOS0wMi0wMlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0wMiBVVEMtLTIwMjAtMDEtMDkgVVRDLDExLjIsMQowNDMsSnVkZXR0ZSxGZW1hbGUsNzMsV2hpdGUsUHJlc2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wNC0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDEsMSwzLDMsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxOC0xMC0yOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMC0yOCBVVEMtLTIwMTktMDQtMDkgVVRDLDUuNCwxCjA0NCxQYWlzeW4sTWFsZSxOQSxXaGl0ZSxQcmVzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDktMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsNCwzLDIsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOS0wMy0wM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMy0wMyBVVEMtLTIwMTktMDktMDkgVVRDLDYuMiwxCjA0NSxJc2lhc2gsRmVtYWxlLDYwLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA3LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMiwyLFByZXNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wMS0yOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0yOSBVVEMtLTIwMTktMDctMDkgVVRDLDUuMywxCjA0NixKeXF1YW4sRmVtYWxlLDQxLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTExLTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDIsMywxLEFic2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOS0wNC0yMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNC0yMCBVVEMtLTIwMTktMTEtMDkgVVRDLDYuNiwxCjA0NyxTaGVycmlseW5uLEZlbWFsZSwzOCxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMS0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwzLDIsUHJlc2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOS0wNi0yMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNi0yMSBVVEMtLTIwMjAtMDEtMDkgVVRDLDYuNiwxCjA0OCxEaWNoZWxsZSxNYWxlLDY0LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA1LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDIsMixBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE4LTExLTA2VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTExLTA2IFVUQy0tMjAxOS0wNS0wOSBVVEMsNi4xLDEKMDQ5LFJlaW5uYSxNYWxlLDMzLFdoaXRlLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0xMC0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDIsMyxBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE5LTA2LTEzVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA2LTEzIFVUQy0tMjAxOS0xMC0wOSBVVEMsMy45LDEKMDUwLE5lZmVydGl0aSxNYWxlLDU1LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTEyLTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMywyLFByZXNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTktMDctMDJUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDctMDIgVVRDLS0yMDE5LTEyLTA5IFVUQyw1LjIsMQowNTEsRXZhbmRlcixNYWxlLDQ1LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTAzLTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMywxLDIsMixBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTgtMDYtMDFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDYtMDEgVVRDLS0yMDE5LTAzLTA5IFVUQyw5LjMsMAowNTIsSm9sbGVlbixNYWxlLDY4LEJsYWNrLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA4LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMiwxLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTgtMTItMjVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTItMjUgVVRDLS0yMDE5LTA4LTA5IFVUQyw3LjUsMQowNTMsTHVsYWJlbGwsRmVtYWxlLDM1LFdoaXRlLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMi0wOVQwMDowMDowMFosRkFMU0UsQ29udHJvbCwzLDQsMywxLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOS0wNC0yNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNC0yNiBVVEMtLTIwMjAtMDItMDkgVVRDLDkuNSwwCjA1NCxEZXRyZWxsLE1hbGUsNTEsSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDIwLTAxLTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMiw0LDMsMSxBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE5LTA2LTI5VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA2LTI5IFVUQy0tMjAyMC0wMS0wOSBVVEMsNi40LDAKMDU1LExpemJldGgsRmVtYWxlLDUwLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTExLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywzLDIsMixBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTgtMTItMjBUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTItMjAgVVRDLS0yMDE5LTExLTA5IFVUQywxMC42LDEKMDU2LEFobnlpYSxGZW1hbGUsNjAsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNS0wOVQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsMiwyLDEsQWJzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTktMDEtMTZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMTYgVVRDLS0yMDE5LTA1LTA5IFVUQywzLjgsMAowNTcsSmF5dGhhbixNYWxlLDUyLEhpc3BhbmljLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNS0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywxLDIsMixBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE4LTEyLTE3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEyLTE3IFVUQy0tMjAxOS0wNS0wOSBVVEMsNC43LDEKMDU4LEFjY2VzcyxNYWxlLDM3LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA0LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMywzLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTgtMDUtMjBUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDUtMjAgVVRDLS0yMDE5LTA0LTA5IFVUQywxMC42LDEKMDU5LEp1c3RhLEZlbWFsZSw1MyxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA4LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDMsMixBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE5LTAzLTAzVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTAzIFVUQy0tMjAxOS0wOC0wOSBVVEMsNS4yLDEKMDYwLEpvdGFybyxNYWxlLDcwLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDIwLTAxLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDMsMixBYnNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOS0wMi0wNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0wNiBVVEMtLTIwMjAtMDEtMDkgVVRDLDExLjEsMQowNjEsTGFsYW5pYSxNYWxlLDQ1LEJpLVJhY2lhbCxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA5LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMywxLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOS0wNi0wOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNi0wOSBVVEMtLTIwMTktMDktMDkgVVRDLDMsMQowNjIsU2hhdW50YWksRmVtYWxlLDM2LEJsYWNrLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wNi0wOVQwMDowMDowMFosRkFMU0UsQ29udHJvbCwzLDIsMywxLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE4LTEyLTIwVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEyLTIwIFVUQy0tMjAxOS0wNi0wOSBVVEMsNS42LDAKMDYzLEphbWlpLE1hbGUsNzAsV2hpdGUsUHJlc2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTEyLTA5VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsNCwxLDMsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxOS0wNy0xOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNy0xOCBVVEMtLTIwMTktMTItMDkgVVRDLDQuNywwCjA2NCxUYXlzZWFuLEZlbWFsZSw2MSxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNi0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDMsMyxBYnNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOC0xMS0yOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMS0yOCBVVEMtLTIwMTktMDYtMDkgVVRDLDYuNCwxCjA2NSxMaW1hLEZlbWFsZSw0NSxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMi0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywxLDMsMSxBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wNy0wM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNy0wMyBVVEMtLTIwMjAtMDItMDkgVVRDLDcuMiwxCjA2NixLeW1lcmEsRmVtYWxlLDcyLEhpc3BhbmljLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDctMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMywyLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE4LTEyLTEwVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEyLTEwIFVUQy0tMjAxOS0wNy0wOSBVVEMsNywxCjA2NyxSeWFubGVlLEZlbWFsZSw2NSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTAzLTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMyw0LDIsMixQcmVzZW50LEZBTFNFLFRSVUUsbG93LDIwMTgtMTAtMDlUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMDkgVVRDLS0yMDE5LTAzLTA5IFVUQyw1LDAKMDY4LFBoaWxvbWVuZSxNYWxlLDU2LEJsYWNrLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDYtMDlUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwxLDIsMywzLFByZXNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTktMDMtMTFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDMtMTEgVVRDLS0yMDE5LTA2LTA5IFVUQywyLjksMAowNjksSXJmYW4sTWFsZSwyNSxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0xMC0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwzLDMsMSxOQSxUUlVFLEZBTFNFLGhpZ2gsMjAxOS0wNS0wOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNS0wOSBVVEMtLTIwMTktMTAtMDkgVVRDLDUsMQowNzAsS3lyaWFubmEsRmVtYWxlLDY3LEJsYWNrLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA4LTA5VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDIsMiwzLDMsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE5LTAzLTEyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTEyIFVUQy0tMjAxOS0wOC0wOSBVVEMsNC45LDAKMDcxLFZhaWwsTWFsZSw2MyxCbGFjayxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMi0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDEsMiwzLDEsQWJzZW50LEZBTFNFLFRSVUUsbG93LDIwMTktMTAtMTFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMTAtMTEgVVRDLS0yMDIwLTAyLTA5IFVUQywzLjksMQowNzIsTmF5ZWxlLEZlbWFsZSwzNSxBc2lhbixBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAyLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSwzLDIsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE5LTA5LTE4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA5LTE4IFVUQy0tMjAyMC0wMi0wOSBVVEMsNC43LDEKMDczLE1hZGVlLEZlbWFsZSw1NyxCbGFjayxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0xMC0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDMsMyxQcmVzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMTEtMTZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMTYgVVRDLS0yMDE5LTEwLTA5IFVUQywxMC44LDEKMDc0LFdpbGZyZWRvLEZlbWFsZSw0MyxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEwLTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMywxLEFic2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMTEtMTVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMTUgVVRDLS0yMDE5LTEwLTA5IFVUQywxMC44LDEKMDc1LERhemhhbixGZW1hbGUsNTAsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDMtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMiwxLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxOC0wNi0yNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNi0yNSBVVEMtLTIwMTktMDMtMDkgVVRDLDguNCwxCjA3NixEYW5lZW4sRmVtYWxlLDYwLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDEtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMywyLDIsQWJzZW50LEZBTFNFLFRSVUUsTkEsMjAxOS0wMS0yOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0yOCBVVEMtLTIwMjAtMDEtMDkgVVRDLDExLjQsMQowNzcsTWVycmllLEZlbWFsZSwzMyxCbGFjayxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNi0wOVQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsNCwyLDIsUHJlc2VudCxGQUxTRSxUUlVFLGxvdywyMDE5LTAyLTAxVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAyLTAxIFVUQy0tMjAxOS0wNi0wOSBVVEMsNC4zLDAKMDc4LFJvb3N2ZWx0LE1hbGUsNzEsSGlzcGFuaWMsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMi0wOVQwMDowMDowMFosRkFMU0UsQ29udHJvbCwxLDMsMiwyLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOS0wNi0wNFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNi0wNCBVVEMtLTIwMjAtMDItMDkgVVRDLDguMiwwCjA3OSxTYWxiYWRvcixNYWxlLDQ1LEhpc3BhbmljLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMS0wOVQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsMiwzLDMsQWJzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE5LTA4LTE5VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA4LTE5IFVUQy0tMjAyMC0wMS0wOSBVVEMsNC43LDAKMDgwLERpb25uZSxNYWxlLDYzLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA2LTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSw0LDEsMixQcmVzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTktMDEtMjNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMjMgVVRDLS0yMDE5LTA2LTA5IFVUQyw0LjUsMAowODEsQ3liZWxsZSxGZW1hbGUsMjcsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMTAtMDlUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMywzLDIsMixBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE5LTAzLTE1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTE1IFVUQy0tMjAxOS0xMC0wOSBVVEMsNi44LDAKMDgyLENhbSxGZW1hbGUsMzYsV2hpdGUsQWJzZW50LFByZXNlbnQsTkEsMjAyMC0wMi0wOVQwMDowMDowMFosRkFMU0UsQ29udHJvbCwzLDQsMywyLEFic2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxOS0wNS0xOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNS0xOSBVVEMtLTIwMjAtMDItMDkgVVRDLDguNywwCjA4MyxaeWtlZW0sRmVtYWxlLDU2LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTExLTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMSwyLFByZXNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE5LTA1LTExVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA1LTExIFVUQy0tMjAxOS0xMS0wOSBVVEMsNS45LDEKMDg0LFJleW5vbCxGZW1hbGUsMzUsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMjAtMDItMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMywyLEFic2VudCxUUlVFLEZBTFNFLGxvdywyMDE5LTA4LTA0VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA4LTA0IFVUQy0tMjAyMC0wMi0wOSBVVEMsNi4yLDEKMDg1LEplcmVtaWFsLE1hbGUsNTQsQmxhY2ssUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA4LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDIsMyxBYnNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTgtMTEtMTNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMTMgVVRDLS0yMDE5LTA4LTA5IFVUQyw4LjksMQowODYsS2Vsc2UsRmVtYWxlLDcwLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTEtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMiwxLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxOC0xMS0xOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMS0xOSBVVEMtLTIwMTktMTEtMDkgVVRDLDExLjcsMQowODcsQ2xpZm9yZCxNYWxlLDMxLFdoaXRlLFByZXNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNC0wOVQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsMywzLDEsQWJzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE4LTExLTE2VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTExLTE2IFVUQy0tMjAxOS0wNC0wOSBVVEMsNC44LDAKMDg4LEppaG9vLEZlbWFsZSw2NixXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0xMC0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDMsMixBYnNlbnQsVFJVRSxUUlVFLGxvdywyMDE4LTExLTIyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTExLTIyIFVUQy0tMjAxOS0xMC0wOSBVVEMsMTAuNiwxCjA4OSxFc3RyYWxpdGEsRmVtYWxlLDM5LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDIwLTAyLTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMiwyLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTktMDUtMThUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDUtMTggVVRDLS0yMDIwLTAyLTA5IFVUQyw4LjcsMQowOTAsQWhtaWssRmVtYWxlLDYwLFdoaXRlLFByZXNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAyLTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMywzLDIsMSxBYnNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOS0wMy0xMlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMy0xMiBVVEMtLTIwMjAtMDItMDkgVVRDLDEwLjksMAowOTEsR2FicmlhbGxlLEZlbWFsZSw1MixXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOS0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSw0LDIsMSxBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTktMDEtMTJUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMTIgVVRDLS0yMDE5LTA5LTA5IFVUQyw3LjksMQowOTIsS2VsZHluLEZlbWFsZSwyNixIaXNwYW5pYyxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wMy0wOVQwMDowMDowMFosTkEsQ29udHJvbCwxLDMsMywxLEFic2VudCxGQUxTRSxUUlVFLG1vZGVyYXRlLDIwMTgtMDYtMDFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDYtMDEgVVRDLS0yMDE5LTAzLTA5IFVUQyw5LjMsTkEKMDkzLFpvZWFubixGZW1hbGUsNDUsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTAtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsMyxOQSwyLFByZXNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE5LTA1LTE4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA1LTE4IFVUQy0tMjAxOS0xMC0wOSBVVEMsNC43LDEKMDk0LFNvbWlsLE1hbGUsNjYsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMS0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDIsMyxBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTktMDUtMDVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDUtMDUgVVRDLS0yMDIwLTAxLTA5IFVUQyw4LjEsMQowOTUsS2V5b25pZSxGZW1hbGUsMzAsQmxhY2ssQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMjAtMDItMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwyLDMsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxOS0xMS0wNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0xMS0wNiBVVEMtLTIwMjAtMDItMDkgVVRDLDMuMSwxCjA5NixBdG9uaW8sTWFsZSw3MSxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMS0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwzLDMsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE5LTA0LTE1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA0LTE1IFVUQy0tMjAxOS0xMS0wOSBVVEMsNi44LDEKMDk3LEthbmVlc2hpYSxGZW1hbGUsNDcsQmktUmFjaWFsLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wOC0wOVQwMDowMDowMFosRkFMU0UsQ29udHJvbCwzLDMsMiwyLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE4LTEwLTI1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEwLTI1IFVUQy0tMjAxOS0wOC0wOSBVVEMsOS41LDAKMDk4LEpheXphLEZlbWFsZSw3MixXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEwLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDMsMSxBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTktMDUtMDhUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDUtMDggVVRDLS0yMDE5LTEwLTA5IFVUQyw1LDEKMDk5LFRpZ3JhLEZlbWFsZSwzMCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTAzLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwxLDIsMyxBYnNlbnQsVFJVRSxUUlVFLGxvdywyMDE4LTA0LTExVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA0LTExIFVUQy0tMjAxOS0wMy0wOSBVVEMsMTAuOSwxCjEwMCxCcnloZWVtLEZlbWFsZSwzMCxXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wMy0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsTkEsMywyLDEsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTgtMDQtMDZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDQtMDYgVVRDLS0yMDE5LTAzLTA5IFVUQywxMS4xLDEKMTAxLFBpZXRlcixNYWxlLDM2LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA2LTA5VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsMSwyLDMsUHJlc2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOC0wNy0wNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNy0wNSBVVEMtLTIwMTktMDYtMDkgVVRDLDExLjEsMAoxMDIsQW1pamEsRmVtYWxlLDM2LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDMtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDEsMywyLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE4LTA0LTAzVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA0LTAzIFVUQy0tMjAxOS0wMy0wOSBVVEMsMTEuMiwxCjEwMyxZYW5jaSxGZW1hbGUsNjcsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wMy0wOVQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsMSwzLDEsUHJlc2VudCxGQUxTRSxUUlVFLGxvdywyMDE4LTA0LTExVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA0LTExIFVUQy0tMjAxOS0wMy0wOSBVVEMsMTAuOSwwCjEwNCxKYXZleixNYWxlLDMzLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTExLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDEsMSxBYnNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTktMDgtMDVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDgtMDUgVVRDLS0yMDE5LTExLTA5IFVUQywzLjEsMQoxMDUsTmVpbGllLEZlbWFsZSw1NCxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMi0wOVQwMDowMDowMFosRkFMU0UsQ29udHJvbCwzLDQsMywyLFByZXNlbnQsVFJVRSxUUlVFLGxvdywyMDE5LTAzLTA3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTA3IFVUQy0tMjAxOS0xMi0wOSBVVEMsOS4xLDAKMTA2LE1lcmNlZGksRmVtYWxlLDY1LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDIwLTAyLTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDMsMiwyLEFic2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTktMDgtMjNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDgtMjMgVVRDLS0yMDIwLTAyLTA5IFVUQyw1LjUsMQoxMDcsUmlzdGluYSxGZW1hbGUsNjksSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMjAtMDEtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMywzLDIsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE5LTA4LTI4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA4LTI4IFVUQy0tMjAyMC0wMS0wOSBVVEMsNC40LDEKMTA4LFJvc2huaSxGZW1hbGUsNjMsQmxhY2ssQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDctMDlUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMiw0LDIsMSxBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE5LTAxLTA2VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTA2IFVUQy0tMjAxOS0wNy0wOSBVVEMsNi4xLDAKMTA5LEFiaGEsRmVtYWxlLDU1LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA5LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDIsMixBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTktMDQtMTZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDQtMTYgVVRDLS0yMDE5LTA5LTA5IFVUQyw0LjgsMQoxMTAsU2VyZW5lLEZlbWFsZSw2NCxOYXRpdmUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDctMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMywzLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxOC0wNy0yMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNy0yMSBVVEMtLTIwMTktMDctMDkgVVRDLDExLjYsMQoxMTEsVG93YW5kYSxNYWxlLDczLFdoaXRlLFByZXNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wMy0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDIsMSxBYnNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOC0xMC0yM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMC0yMyBVVEMtLTIwMTktMDMtMDkgVVRDLDQuNSwxCjExMixKb2hubnlsZWUsRmVtYWxlLDI2LEJsYWNrLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDgtMDlUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMSw0LDMsMixQcmVzZW50LFRSVUUsVFJVRSxsb3csMjAxOC0xMC0wM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMC0wMyBVVEMtLTIwMTktMDgtMDkgVVRDLDEwLjIsMAoxMTMsQ2hpY2ssTWFsZSwyNSxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wMy0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDMsMSxQcmVzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTgtMDYtMThUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDYtMTggVVRDLS0yMDE5LTAzLTA5IFVUQyw4LjcsMQoxMTQsTGF0b256YSxGZW1hbGUsNDEsQmxhY2ssQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA3LTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMywzLDMsMixQcmVzZW50LFRSVUUsRkFMU0UsbG93LDIwMTgtMTEtMDVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMDUgVVRDLS0yMDE5LTA3LTA5IFVUQyw4LjEsMAoxMTUsRW5kZXIsRmVtYWxlLDMzLEJsYWNrLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA0LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDIsMywyLEFic2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxOC0xMS0wM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMS0wMyBVVEMtLTIwMTktMDQtMDkgVVRDLDUuMiwxCjExNixOeXppYWgsRmVtYWxlLDQzLEFzaWFuLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTAtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMywyLFByZXNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTktMDUtMDlUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDUtMDkgVVRDLS0yMDE5LTEwLTA5IFVUQyw1LDEKMTE3LEFiZHVsa2FyaW0sRmVtYWxlLDQzLFdoaXRlLFByZXNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDUtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMSwyLFByZXNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTgtMDUtMThUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDUtMTggVVRDLS0yMDE5LTA1LTA5IFVUQywxMS43LDEKMTE4LERvbmljaW8sTWFsZSw2NixXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA2LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDMsMiwyLFByZXNlbnQsVFJVRSxUUlVFLGxvdywyMDE4LTA3LTE3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA3LTE3IFVUQy0tMjAxOS0wNi0wOSBVVEMsMTAuNywxCjExOSxCcmFuZGVsLE5BLDUwLEhpc3BhbmljLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA4LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywzLDEsMSxBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTgtMDktMTJUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDktMTIgVVRDLS0yMDE5LTA4LTA5IFVUQywxMC45LDEKMTIwLFRvbWl3YSxGZW1hbGUsNTIsSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA2LTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMiwyLDIsMSxBYnNlbnQsVFJVRSxUUlVFLGxvdywyMDE5LTAzLTA4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTA4IFVUQy0tMjAxOS0wNi0wOSBVVEMsMywwCjEyMSxDb3JpZW5lLEZlbWFsZSw0MyxIaXNwYW5pYyxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNS0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDIsMSxQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE4LTA3LTA0VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA3LTA0IFVUQy0tMjAxOS0wNS0wOSBVVEMsMTAuMiwxCjEyMixUcmlzdG9uLE1hbGUsNDYsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDYtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsMiwzLDIsQWJzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTktMDEtMjJUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMjIgVVRDLS0yMDE5LTA2LTA5IFVUQyw0LjYsMQoxMjMsU2hhbmFyaSxNYWxlLDYyLEhpc3BhbmljLFByZXNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA5LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDMsMywyLFByZXNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE4LTExLTE0VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTExLTE0IFVUQy0tMjAxOS0wOS0wOSBVVEMsOS44LDEKMTI0LFpha2FyLE1hbGUsMjcsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMi0wOVQwMDowMDowMFosRkFMU0UsQ29udHJvbCwxLDIsMiwyLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOS0xMS0wOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0xMS0wOCBVVEMtLTIwMjAtMDItMDkgVVRDLDMsMAoxMjUsTGFtYmVydCxNYWxlLDQ0LEhpc3BhbmljLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTExLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywzLDMsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE4LTExLTE4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTExLTE4IFVUQy0tMjAxOS0xMS0wOSBVVEMsMTEuNywxCjEyNixQcm92aWRlbmNpYSxGZW1hbGUsMzIsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNi0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMywzLDMsUHJlc2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMDktMTJUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDktMTIgVVRDLS0yMDE5LTA2LTA5IFVUQyw4LjksMQoxMjcsVXJla2EsRmVtYWxlLDQwLFdoaXRlLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wOS0wOVQwMDowMDowMFosRkFMU0UsQ29udHJvbCwxLDQsMiwxLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOS0wMS0zMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0zMCBVVEMtLTIwMTktMDktMDkgVVRDLDcuMywwCjEyOCxLeWxlYWxleGFuZGVyLEZlbWFsZSw2MCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA3LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywxLDIsMSxBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE4LTEwLTE4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEwLTE4IFVUQy0tMjAxOS0wNy0wOSBVVEMsOC43LDEKMTI5LEpvZXZhbm5pLE1hbGUsNjQsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA3LTA5VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDIsMiwzLDMsUHJlc2VudCxUUlVFLEZBTFNFLGxvdywyMDE4LTEyLTE5VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEyLTE5IFVUQy0tMjAxOS0wNy0wOSBVVEMsNi43LDAKMTMwLEJyeXNzaWEsTWFsZSw1NSxCbGFjayxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wOC0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDMsMSxQcmVzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxOS0wNC0yOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNC0yOCBVVEMtLTIwMTktMDgtMDkgVVRDLDMuNCwxCjEzMSxTYW5kcmFoLE1hbGUsNzMsV2hpdGUsUHJlc2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA0LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMiwzLEFic2VudCxGQUxTRSxGQUxTRSxsb3csMjAxOC0xMS0yMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMS0yMSBVVEMtLTIwMTktMDQtMDkgVVRDLDQuNiwxCjEzMixBbHRoYSxNYWxlLDcwLEhpc3BhbmljLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wOC0wOVQwMDowMDowMFosRkFMU0UsQ29udHJvbCwxLDMsMSwzLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxOC0xMi0wNFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMi0wNCBVVEMtLTIwMTktMDgtMDkgVVRDLDguMiwwCjEzMyxUYXlqYSxNYWxlLDU1LEhpc3BhbmljLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0xMS0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwzLDMsUHJlc2VudCxOQSxGQUxTRSxtb2RlcmF0ZSwyMDE5LTAxLTE0VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTE0IFVUQy0tMjAxOS0xMS0wOSBVVEMsOS44LDEKMTM0LExhbWluZSxNYWxlLDU1LEhpc3BhbmljLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMi0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSw0LDMsMixBYnNlbnQsVFJVRSxUUlVFLGxvdywyMDE5LTEwLTI4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTEwLTI4IFVUQy0tMjAyMC0wMi0wOSBVVEMsMy40LDEKMTM1LEtva2V0YSxGZW1hbGUsMzgsV2hpdGUsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA4LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMiwzLFByZXNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTgtMTAtMTFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMTEgVVRDLS0yMDE5LTA4LTA5IFVUQyw5LjksMQoxMzYsQXphYWQsTWFsZSw1NSxIaXNwYW5pYyxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAyLTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDIsMiwyLFByZXNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTktMDQtMjBUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDQtMjAgVVRDLS0yMDIwLTAyLTA5IFVUQyw5LjYsMQoxMzcsRGFybmV5LE1hbGUsMjksQmxhY2ssQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNS0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSw0LDMsMSxQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE4LTExLTE4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTExLTE4IFVUQy0tMjAxOS0wNS0wOSBVVEMsNS43LDEKMTM4LFl6YWJlbCxGZW1hbGUsNjAsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTItMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDMsMywyLFByZXNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE5LTAyLTI1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAyLTI1IFVUQy0tMjAxOS0xMi0wOSBVVEMsOS41LDEKMTM5LFdhZmksRmVtYWxlLDYyLE5BLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDYtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDQsMiwyLFByZXNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE4LTA4LTEyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA4LTEyIFVUQy0tMjAxOS0wNi0wOSBVVEMsOS45LDEKMTQwLEx5bm5heWEsRmVtYWxlLDY5LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA0LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDMsMixQcmVzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE4LTExLTA4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTExLTA4IFVUQy0tMjAxOS0wNC0wOSBVVEMsNSwxCjE0MSxKb2xldGhhLEZlbWFsZSwzMCxBc2lhbixBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA5LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMywxLFByZXNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTgtMTEtMDZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMDYgVVRDLS0yMDE5LTA5LTA5IFVUQywxMC4xLDEKMTQyLEFtYmksTWFsZSwyOCxIaXNwYW5pYyxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wMy0wOVQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsMywyLDEsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxOC0wNy0yMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNy0yMCBVVEMtLTIwMTktMDMtMDkgVVRDLDcuNiwwCjE0MyxNb3R1bnJheW8sRmVtYWxlLDUwLFdoaXRlLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNC0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywyLDMsMyxQcmVzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE4LTA2LTA5VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA2LTA5IFVUQy0tMjAxOS0wNC0wOSBVVEMsMTAsMQoxNDQsTWFybGljZSxNYWxlLDM0LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA5LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMiwzLEFic2VudCxUUlVFLEZBTFNFLGxvdywyMDE5LTA0LTE5VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA0LTE5IFVUQy0tMjAxOS0wOS0wOSBVVEMsNC43LDEKMTQ1LFlvbmF0aGFuLEZlbWFsZSw2NSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA4LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDMsMixBYnNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTgtMTEtMjVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMjUgVVRDLS0yMDE5LTA4LTA5IFVUQyw4LjUsMQoxNDYsSHlsZSxGZW1hbGUsNDcsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMi0wOVQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsNCwzLDEsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOS0wNS0wNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNS0wNSBVVEMtLTIwMTktMTItMDkgVVRDLDcuMSwwCjE0NyxSb25uZWwsRmVtYWxlLDUzLFdoaXRlLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAyMC0wMS0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDIsNCwzLDEsUHJlc2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxOS0wNC0xM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNC0xMyBVVEMtLTIwMjAtMDEtMDkgVVRDLDguOSwxCjE0OCxLZWRyb24sTWFsZSw2OSxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMS0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMiwyLDMsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxOS0wNy0xOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNy0xOCBVVEMtLTIwMjAtMDEtMDkgVVRDLDUuNywxCjE0OSxBbHRhaXJhLEZlbWFsZSw2NyxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMS0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSwzLDEsMSxBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTktMDktMDdUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDktMDcgVVRDLS0yMDIwLTAxLTA5IFVUQyw0LjEsMQoxNTAsSmVtaWxsYSxNYWxlLDMzLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA5LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDIsMixBYnNlbnQsVFJVRSxUUlVFLGxvdywyMDE5LTAyLTI3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAyLTI3IFVUQy0tMjAxOS0wOS0wOSBVVEMsNi40LDEKMTUxLEpvZGllbCxGZW1hbGUsNDEsQmxhY2ssQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDgtMDlUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwzLDMsMywzLEFic2VudCxUUlVFLFRSVUUsbG93LDIwMTctMDctMDdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNy0wNyBVVEMtLTIwMTktMDgtMDkgVVRDLDI1LjEsMAoxNTIsS2VtZXNoaWEsTWFsZSw2MSxBc2lhbixBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDctMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsMywzLDIsQWJzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTYtMTAtMDhUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0xMC0wOCBVVEMtLTIwMTktMDctMDkgVVRDLDMzLDEKMTUzLENhbmRpbmEsRmVtYWxlLDYwLEhpc3BhbmljLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDMtMDlUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwxLDIsMywyLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTctMDYtMjlUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNi0yOSBVVEMtLTIwMTktMDMtMDkgVVRDLDIwLjMsMAoxNTQsRXZ5bixGZW1hbGUsNDAsTmF0aXZlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDYtMDlUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwxLDEsMywzLFByZXNlbnQsRkFMU0UsRkFMU0UsbW9kZXJhdGUsMjAxNy0wOS0wNlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA5LTA2IFVUQy0tMjAxOS0wNi0wOSBVVEMsMjEuMSwwCjE1NSxCcnlzdGxlLE1hbGUsMjYsV2hpdGUsUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDEtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMywxLDEsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE3LTA5LTI0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDktMjQgVVRDLS0yMDIwLTAxLTA5IFVUQywyNy41LDEKMTU2LEJyYWRmb3JkLEZlbWFsZSw0MixXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wMy0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywxLDIsMSxBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxNy0wNi0wM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA2LTAzIFVUQy0tMjAxOS0wMy0wOSBVVEMsMjEuMiwxCjE1NyxVcmJhbixGZW1hbGUsNjYsQmxhY2ssQWJzZW50LEFic2VudCxQcmVzZW50LDIwMjAtMDItMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsMiwzLDIsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxNy0wMy0zMVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTAzLTMxIFVUQy0tMjAyMC0wMi0wOSBVVEMsMzQuMywxCjE1OCxUYWxlbmEsTWFsZSw2NyxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAxLTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDMsMywyLEFic2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxNy0wNC0yM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA0LTIzIFVUQy0tMjAyMC0wMS0wOSBVVEMsMzIuNSwxCjE1OSxLZWlhc2hhLE1hbGUsNjIsTmF0aXZlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDItMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwyLDMsUHJlc2VudCxGQUxTRSxGQUxTRSxsb3csMjAxNy0wNS0xMlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA1LTEyIFVUQy0tMjAyMC0wMi0wOSBVVEMsMzIuOSwxCjE2MCxMdW5keSxNYWxlLDU5LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA0LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDMsMSwzLEFic2VudCxUUlVFLFRSVUUsaGlnaCwyMDE4LTAzLTIxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDMtMjEgVVRDLS0yMDE5LTA0LTA5IFVUQywxMi42LDEKMTYxLFR3aWcsTWFsZSw1OSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA0LTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSw0LDIsMyxBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE2LTA5LTA1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMDktMDUgVVRDLS0yMDE5LTA0LTA5IFVUQywzMS4xLDAKMTYyLEpvaWNlLE1hbGUsNTIsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTEyLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDMsMyxBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTctMDYtMDdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNi0wNyBVVEMtLTIwMTktMTItMDkgVVRDLDMwLjEsMQoxNjMsTWlraWxhaCxNYWxlLDY4LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDktMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwzLDIsUHJlc2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxOC0wMS0wNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTAxLTA1IFVUQy0tMjAxOS0wOS0wOSBVVEMsMjAuMSwxCjE2NCxMZWF0aGllLE1hbGUsNTAsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDctMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDIsMSwzLFByZXNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOC0wNy0xM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTA3LTEzIFVUQy0tMjAxOS0wNy0wOSBVVEMsMTEuOSwxCjE2NSxFbGRhbixGZW1hbGUsNjMsV2hpdGUsUHJlc2VudCxBYnNlbnQsUHJlc2VudCwyMDIwLTAxLTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDEsMiwxLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE3LTA3LTE1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDctMTUgVVRDLS0yMDIwLTAxLTA5IFVUQywyOS44LDEKMTY2LEpvbml5YSxGZW1hbGUsMzQsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMS0wOVQwMDowMDowMFosRkFMU0UsQ29udHJvbCwyLDEsMiwyLEFic2VudCxUUlVFLFRSVUUsaGlnaCwyMDE3LTA4LTA2VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDgtMDYgVVRDLS0yMDIwLTAxLTA5IFVUQywyOS4xLDAKMTY3LEF2b25sZWUsRmVtYWxlLDUwLEhpc3BhbmljLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDItMDlUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMyw0LDIsMyxQcmVzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxOC0xMC0xOVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTEwLTE5IFVUQy0tMjAyMC0wMi0wOSBVVEMsMTUuNywwCjE2OCxSYXljaGVsbGUsTWFsZSwyNyxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAyLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDIsMyxQcmVzZW50LEZBTFNFLFRSVUUsbG93LDIwMTgtMTItMjRUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0xMi0yNCBVVEMtLTIwMjAtMDItMDkgVVRDLDEzLjUsMQoxNjksSnVsaWVuLE1hbGUsNzIsSGlzcGFuaWMsUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDctMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMSwzLEFic2VudCxUUlVFLFRSVUUsaGlnaCwyMDE3LTEwLTI3VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTAtMjcgVVRDLS0yMDE5LTA3LTA5IFVUQywyMC40LDEKMTcwLFF1YXlsZW4sRmVtYWxlLDczLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTAtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsNCwyLDMsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxNy0wNS0xOVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA1LTE5IFVUQy0tMjAxOS0xMC0wOSBVVEMsMjguNywxCjE3MSxUYWdnZXJ0LEZlbWFsZSw1MSxXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNC0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMywzLDMsQWJzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxNy0wOS0wOVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA5LTA5IFVUQy0tMjAxOS0wNC0wOSBVVEMsMTksMQoxNzIsS2FzaGluYSxGZW1hbGUsMzAsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDQtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDMsMiwyLEFic2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOC0wMi0yOFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTAyLTI4IFVUQy0tMjAxOS0wNC0wOSBVVEMsMTMuNCwxCjE3MyxKZWFuZWxsYSxNYWxlLDY0LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTAtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMywxLEFic2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTctMDQtMzBUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNC0zMCBVVEMtLTIwMTktMTAtMDkgVVRDLDI5LjMsMQoxNzQsU2FnYXIsRmVtYWxlLDY4LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA4LTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMywzLDEsMyxBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE2LTEwLTAyVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMTAtMDIgVVRDLS0yMDE5LTA4LTA5IFVUQywzNC4yLDAKMTc1LFdpbnNvcixNYWxlLDI1LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTItMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMywyLDEsUHJlc2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNy0wNi0yOVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA2LTI5IFVUQy0tMjAxOS0xMi0wOSBVVEMsMjkuMywxCjE3NixTaGFyaXNzYSxGZW1hbGUsNDYsV2hpdGUsUHJlc2VudCxBYnNlbnQsUHJlc2VudCwyMDIwLTAyLTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSwzLDMsMixQcmVzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE3LTA0LTIyVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDQtMjIgVVRDLS0yMDIwLTAyLTA5IFVUQywzMy42LDAKMTc3LEpvZXZhbnksRmVtYWxlLDcyLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA4LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDMsMywxLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxNy0wNy0xMVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA3LTExIFVUQy0tMjAxOS0wOC0wOSBVVEMsMjQuOSwxCjE3OCxDYXVhLEZlbWFsZSw2MCxIaXNwYW5pYyxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMC0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywyLDIsMyxQcmVzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxNy0wNi0yNlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA2LTI2IFVUQy0tMjAxOS0xMC0wOSBVVEMsMjcuNCwxCjE3OSxKYW1lZXJhLEZlbWFsZSw1MyxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA2LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDEsMywyLEFic2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTYtMTEtMjdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0xMS0yNyBVVEMtLTIwMTktMDYtMDkgVVRDLDMwLjQsMQoxODAsQXJxdWl0YSxGZW1hbGUsNTEsQmxhY2ssQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMS0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwzLDMsMixBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOC0wNi0yNlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTA2LTI2IFVUQy0tMjAyMC0wMS0wOSBVVEMsMTguNSwxCjE4MSxMZW9udGluZSxGZW1hbGUsMzQsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDctMDlUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwyLDMsMiwzLEFic2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxNi0xMi0wN1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE2LTEyLTA3IFVUQy0tMjAxOS0wNy0wOSBVVEMsMzEuMSwwCjE4MixBbGljayxGZW1hbGUsNTMsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMTItMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsMywxLDEsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOC0wNy0xM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTA3LTEzIFVUQy0tMjAxOS0xMi0wOSBVVEMsMTYuOSwxCjE4MyxMYW1hcmVvbixGZW1hbGUsNjgsQmxhY2ssQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDUtMDlUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwzLDQsMywyLEFic2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTctMDctMDlUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNy0wOSBVVEMtLTIwMTktMDUtMDkgVVRDLDIyLDAKMTg0LEpheXZlZXIsTWFsZSw2NyxXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wMy0wOVQwMDowMDowMFosRkFMU0UsQ29udHJvbCwyLDIsMiwyLFByZXNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE3LTAyLTExVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDItMTEgVVRDLS0yMDE5LTAzLTA5IFVUQywyNC45LDAKMTg1LFNhbmlrYSxGZW1hbGUsNDMsQmxhY2ssUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDQtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMywzLDIsQWJzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE2LTExLTI5VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMTEtMjkgVVRDLS0yMDE5LTA0LTA5IFVUQywyOC40LDEKMTg2LE5BLEZlbWFsZSwzNSxCaS1SYWNpYWwsUHJlc2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wNy0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSw0LDIsMixBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE3LTA2LTAzVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDYtMDMgVVRDLS0yMDE5LTA3LTA5IFVUQywyNS4yLDEKMTg3LFF1YW50ZWwsTWFsZSwyOCxXaGl0ZSxQcmVzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDUtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDMsMywzLFByZXNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTYtMDktMTlUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wOS0xOSBVVEMtLTIwMTktMDUtMDkgVVRDLDMxLjcsMQoxODgsTWF0aWthLE1hbGUsNDIsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDctMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwyLDMsQWJzZW50LEZBTFNFLFRSVUUsbG93LDIwMTgtMDYtMjZUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wNi0yNiBVVEMtLTIwMTktMDctMDkgVVRDLDEyLjQsMQoxODksTGF0ZXNoaWEsTWFsZSwzMixXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMS0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDEsMywyLDEsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE4LTA4LTA5VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDgtMDkgVVRDLS0yMDE5LTExLTA5IFVUQywxNSwxCjE5MCxLaW1vcmFsZWUsRmVtYWxlLDU3LFdoaXRlLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wOC0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMSwzLDIsQWJzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE4LTAzLTAzVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDMtMDMgVVRDLS0yMDE5LTA4LTA5IFVUQywxNy4yLDEKMTkxLE5hZGVlbixGZW1hbGUsNDEsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNy0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywyLDIsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE3LTA4LTA0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDgtMDQgVVRDLS0yMDE5LTA3LTA5IFVUQywyMy4yLDEKMTkyLEtvaGxzdG9uLEZlbWFsZSwzNyxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNS0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwzLDIsMixBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTctMDQtMjNUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNC0yMyBVVEMtLTIwMTktMDUtMDkgVVRDLDI0LjUsMQoxOTMsR2xpbmRhLE1hbGUsMzMsQmxhY2ssUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDctMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDQsMSwxLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE3LTAyLTE1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDItMTUgVVRDLS0yMDE5LTA3LTA5IFVUQywyOC44LDEKMTk0LEphdmFudGEsRmVtYWxlLDczLEJpLVJhY2lhbCxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0xMC0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwxLDEsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE4LTEwLTEyVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMTAtMTIgVVRDLS0yMDE5LTEwLTA5IFVUQywxMS45LDEKMTk1LEVxdWFzaWEsRmVtYWxlLDY5LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDEtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDIsMywxLFByZXNlbnQsVFJVRSxOQSxoaWdoLDIwMTctMTEtMjNUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0xMS0yMyBVVEMtLTIwMjAtMDEtMDkgVVRDLDI1LjUsMQoxOTYsSGFzdGluZyxGZW1hbGUsNzMsSGlzcGFuaWMsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTExLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDMsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE3LTExLTA3VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTEtMDcgVVRDLS0yMDE5LTExLTA5IFVUQywyNC4xLDEKMTk3LFZlcmxlLE1hbGUsNDIsSGlzcGFuaWMsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMS0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwyLDEsQWJzZW50LEZBTFNFLFRSVUUsbG93LDIwMTctMTAtMjlUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0xMC0yOSBVVEMtLTIwMjAtMDEtMDkgVVRDLDI2LjQsMQoxOTgsUm9jaGVsbGEsTWFsZSw2MSxXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMi0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwzLDMsMyxBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTctMDMtMTJUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wMy0xMiBVVEMtLTIwMTktMTItMDkgVVRDLDMyLjksMQoxOTksTWFpaGEsRmVtYWxlLDU2LFdoaXRlLFByZXNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEyLTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMywyLEFic2VudCxUUlVFLFRSVUUsbG93LDIwMTctMDUtMDZUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNS0wNiBVVEMtLTIwMTktMTItMDkgVVRDLDMxLjEsMQoyMDAsRW1lcmxlZSxGZW1hbGUsMzAsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDUtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsNCwyLDMsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTYtMDktMjBUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wOS0yMCBVVEMtLTIwMTktMDUtMDkgVVRDLDMxLjYsMQoyMDEsU2VjZWxpYSxNYWxlLDQ3LFdoaXRlLFByZXNlbnQsQWJzZW50LFByZXNlbnQsMjAyMC0wMS0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMywyLDIsQWJzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxOC0wOC0yNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTA4LTI1IFVUQy0tMjAyMC0wMS0wOSBVVEMsMTYuNSwxCjIwMixBZGRpc2FuLEZlbWFsZSw1MyxCbGFjayxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA3LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwzLDEsMyxQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE4LTA2LTI5VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDYtMjkgVVRDLS0yMDE5LTA3LTA5IFVUQywxMi4zLDEKMjAzLEFiZGloYW1pZCxNYWxlLDM5LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDgtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsMiwyLDEsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE3LTA5LTA0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDktMDQgVVRDLS0yMDE5LTA4LTA5IFVUQywyMy4yLDEKMjA0LE96YXJhLE1hbGUsMjgsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDMtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsNCwzLDIsUHJlc2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE3LTExLTA3VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTEtMDcgVVRDLS0yMDE5LTAzLTA5IFVUQywxNi4xLDEKMjA1LERheWxpZSxGZW1hbGUsNjEsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wMy0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMiwzLDEsQWJzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE2LTA3LTIzVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMDctMjMgVVRDLS0yMDE5LTAzLTA5IFVUQywzMS41LDEKMjA2LE15aXNoYSxNYWxlLDQzLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDEtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMiwyLEFic2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxNy0xMS0wOVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTExLTA5IFVUQy0tMjAyMC0wMS0wOSBVVEMsMjYsMQoyMDcsTWFudWwsTWFsZSw2OSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA0LTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMiwyLDIsMyxBYnNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOC0wMy0zMFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTAzLTMwIFVUQy0tMjAxOS0wNC0wOSBVVEMsMTIuMywwCjIwOCxDaGVybGluLE1hbGUsMjUsSGlzcGFuaWMsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMS0wOVQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsMywzLDIsQWJzZW50LFRSVUUsRkFMU0UsbG93LDIwMTgtMDMtMzFUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wMy0zMSBVVEMtLTIwMjAtMDEtMDkgVVRDLDIxLjMsMAoyMDksTWlra2ksRmVtYWxlLDUwLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA2LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwyLDEsMixQcmVzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxNy0wMS0wNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTAxLTA1IFVUQy0tMjAxOS0wNi0wOSBVVEMsMjkuMSwxCjIxMCxKYWNxdWFseW5uLEZlbWFsZSw2MSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA2LTA5VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDEsMywyLDMsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE4LTA0LTI1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDQtMjUgVVRDLS0yMDE5LTA2LTA5IFVUQywxMy41LDAKMjExLExhZGVycmljayxGZW1hbGUsMjcsQmxhY2ssQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMS0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDEsNCwzLDIsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTgtMDgtMjJUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wOC0yMiBVVEMtLTIwMTktMTEtMDkgVVRDLDE0LjYsMQoyMTIsSm9yZ2VqcixNYWxlLDM4LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDMtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDIsMiwxLEFic2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxNy0wNS0yNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA1LTI1IFVUQy0tMjAxOS0wMy0wOSBVVEMsMjEuNCwxCjIxMyxBYWhhYW4sTWFsZSw3MSxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNC0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwyLDIsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE3LTAxLTA1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDEtMDUgVVRDLS0yMDE5LTA0LTA5IFVUQywyNy4xLDEKMjE0LEtheWx5LE1hbGUsNTcsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOC0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywzLDMsMixQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE2LTA5LTE0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMDktMTQgVVRDLS0yMDE5LTA4LTA5IFVUQywzNC44LDEKMjE1LEdhdXRhbSxNYWxlLDczLEhpc3BhbmljLFByZXNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wOC0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMywyLDIsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE3LTAzLTE3VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDMtMTcgVVRDLS0yMDE5LTA4LTA5IFVUQywyOC43LDEKMjE2LFBoYXJ5bixGZW1hbGUsNDcsSGlzcGFuaWMsUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDItMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMywzLDMsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxNy0wNi0zMFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA2LTMwIFVUQy0tMjAyMC0wMi0wOSBVVEMsMzEuMywxCjIxNyxTb2ssTWFsZSwzNyxXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNS0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSwyLDIsMSxQcmVzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTctMDctMTdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNy0xNyBVVEMtLTIwMTktMDUtMDkgVVRDLDIxLjcsMQoyMTgsTmVjaGVsbGUsRmVtYWxlLDI5LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDUtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsMywzLDMsUHJlc2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTctMDgtMThUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wOC0xOCBVVEMtLTIwMTktMDUtMDkgVVRDLDIwLjcsMQoyMTksVnljdG9yeWEsRmVtYWxlLDQ0LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTExLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSwzLDMsMSxBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTgtMDYtMjRUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wNi0yNCBVVEMtLTIwMTktMTEtMDkgVVRDLDE2LjUsMQoyMjAsS2F0aWthLE1hbGUsMzgsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOS0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDIsNCwzLDMsQWJzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxNy0wNS0xM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA1LTEzIFVUQy0tMjAxOS0wOS0wOSBVVEMsMjcuOSwxCjIyMSxEYW5haGksRmVtYWxlLDYyLEhpc3BhbmljLFByZXNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA2LTA5VDAwOjAwOjAwWixUUlVFLE5BLDMsMiwyLDEsUHJlc2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNy0wNi0xMVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA2LTExIFVUQy0tMjAxOS0wNi0wOSBVVEMsMjMuOSwxCjIyMixDZXphbm5lLEZlbWFsZSw0MCxIaXNwYW5pYyxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNi0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSw0LDIsMSxQcmVzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE4LTA0LTA4VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDQtMDggVVRDLS0yMDE5LTA2LTA5IFVUQywxNCwxCjIyMyxOYXJpeWFoLE1hbGUsNzIsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMi0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDIsMixQcmVzZW50LFRSVUUsVFJVRSxsb3csMjAxOC0wMi0wM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTAyLTAzIFVUQy0tMjAyMC0wMi0wOSBVVEMsMjQuMiwxCjIyNCxSYW5leSxGZW1hbGUsNzAsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDgtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDMsMywzLFByZXNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE4LTAxLTA4VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDEtMDggVVRDLS0yMDE5LTA4LTA5IFVUQywxOSwxCjIyNSxHd3luZG9saW4sTWFsZSwyOSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA3LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDMsMixBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE3LTEwLTA0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTAtMDQgVVRDLS0yMDE5LTA3LTA5IFVUQywyMS4yLDEKMjI2LENsb3RpbmUsTWFsZSw2MyxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA0LTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDMsMiwxLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE1LTEyLTEyVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTUtMTItMTIgVVRDLS0yMDE5LTA0LTA5IFVUQywzOS45LDEKMjI3LEplZHJlayxNYWxlLDY5LFdoaXRlLFByZXNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNC0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDMsMSxQcmVzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTQtMTItMTdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNC0xMi0xNyBVVEMtLTIwMTktMDQtMDkgVVRDLDUxLjcsMQoyMjgsTWNjYXlsYSxNYWxlLDcwLFdoaXRlLFByZXNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNi0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywzLDEsMSxQcmVzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxNC0xMC0wNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE0LTEwLTA1IFVUQy0tMjAxOS0wNi0wOSBVVEMsNTYuMSwxCjIyOSxTZWphLEZlbWFsZSw0NSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAyLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywzLDIsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE1LTEyLTA5VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTUtMTItMDkgVVRDLS0yMDIwLTAyLTA5IFVUQyw1MCwxCjIzMCxLZW5uaXRoYSxNYWxlLDY2LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTEyLTA5VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDIsMywxLEFic2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxNS0xMi0yOVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTEyLTI5IFVUQy0tMjAxOS0xMi0wOSBVVEMsNDcuMywxCjIzMSxLYW5lYSxGZW1hbGUsMzUsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOS0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDIsMyxBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTUtMDctMTBUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNS0wNy0xMCBVVEMtLTIwMTktMDktMDkgVVRDLDUwLDEKMjMyLEpvaG55LE1hbGUsNTIsSGlzcGFuaWMsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDUtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMywzLDMsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxNS0wNC0xNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTA0LTE1IFVUQy0tMjAxOS0wNS0wOSBVVEMsNDguOCwxCjIzMyxSYWFkLE1hbGUsNjcsQmxhY2ssQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMi0wOVQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDIsMywzLDMsUHJlc2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNS0wNy0yNlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTA3LTI2IFVUQy0tMjAxOS0xMi0wOSBVVEMsNTIuNCwwCjIzNCxSZW5hdWQsTWFsZSw0OSxCbGFjayxQcmVzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDUtMDlUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwzLDQsMywzLFByZXNlbnQsRkFMU0UsVFJVRSxsb3csMjAxNS0wNS0xMFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTA1LTEwIFVUQy0tMjAxOS0wNS0wOSBVVEMsNDgsMAoyMzUsQ2VsbGEsTWFsZSwzMSxXaGl0ZSxQcmVzZW50LEFic2VudCxQcmVzZW50LDIwMTktMTAtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsNCwyLDMsUHJlc2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxNS0wMi0xN1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTAyLTE3IFVUQy0tMjAxOS0xMC0wOSBVVEMsNTUuNywxCjIzNixJc3JlYWwsTWFsZSw0NyxCbGFjayxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0xMS0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDMsMyxBYnNlbnQsRkFMU0UsRkFMU0UsbW9kZXJhdGUsMjAxNS0wMy0yM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTAzLTIzIFVUQy0tMjAxOS0xMS0wOSBVVEMsNTUuNSwxCjIzNyxHYXZvbixGZW1hbGUsNTAsV2hpdGUsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA1LTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSwxLDEsMixQcmVzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTUtMTEtMTdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNS0xMS0xNyBVVEMtLTIwMTktMDUtMDkgVVRDLDQxLjcsMQoyMzgsU2hhdGF5LEZlbWFsZSw1NSxIaXNwYW5pYyxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTExLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwxLDMsMyxQcmVzZW50LFRSVUUsVFJVRSxsb3csMjAxNS0wNS0yNFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTA1LTI0IFVUQy0tMjAxOS0xMS0wOSBVVEMsNTMuNSwxCjIzOSxaYW5vYmlhLE1hbGUsMzYsV2hpdGUsTkEsQWJzZW50LEFic2VudCwyMDIwLTAyLTA5VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSwxLDMsMixBYnNlbnQsRkFMU0UsVFJVRSxsb3csMjAxNi0wMS0xOFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE2LTAxLTE4IFVUQy0tMjAyMC0wMi0wOSBVVEMsNDguNywxCjI0MCxPbHV3YWRhcmFzaW1pLEZlbWFsZSw0OCxCbGFjayxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wOS0wOVQwMDowMDowMFosVFJVRSxDb250cm9sLDEsMywyLDIsQWJzZW50LEZBTFNFLFRSVUUsbG93LDIwMTUtMDctMTNUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNS0wNy0xMyBVVEMtLTIwMTktMDktMDkgVVRDLDQ5LjksMQoyNDEsQW5hcml5YWgsTWFsZSw3MCxCbGFjayxQcmVzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDgtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsNCwyLDIsUHJlc2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNS0wMy0yNFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTAzLTI0IFVUQy0tMjAxOS0wOC0wOSBVVEMsNTIuNSwxCjI0MixLeWxlYWgsTWFsZSw0MSxCbGFjayxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA4LTA5VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDIsMiwzLDMsUHJlc2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTYtMDMtMDdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wMy0wNyBVVEMtLTIwMTktMDgtMDkgVVRDLDQxLjEsMAoyNDMsRmF0ZSxNYWxlLDM1LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTAzLTA5VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDIsNCwzLDIsUHJlc2VudCxGQUxTRSxGQUxTRSxsb3csMjAxNS0xMC0wNFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTEwLTA0IFVUQy0tMjAxOS0wMy0wOSBVVEMsNDEuMiwwCjI0NCxDeXJlZSxNYWxlLDU2LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDgtMDlUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDQsMyxOQSxBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE0LTEyLTE2VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTQtMTItMTYgVVRDLS0yMDE5LTA4LTA5IFVUQyw1NS44LDEKMjQ1LE55bGFuLEZlbWFsZSw0NCxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNC0wOVQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDIsMywyLDMsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE0LTEwLTAxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTQtMTAtMDEgVVRDLS0yMDE5LTA0LTA5IFVUQyw1NC4zLDAKMjQ2LEphcm1hbCxNYWxlLDI1LEFzaWFuLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTAtMDlUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMSwzLDMsMixQcmVzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTYtMDgtMTlUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wOC0xOSBVVEMtLTIwMTktMTAtMDkgVVRDLDM3LjcsMAoyNDcsT3NjYXJqcixGZW1hbGUsNTUsV2hpdGUsUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDMtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMywzLDMsUHJlc2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTYtMDEtMjZUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wMS0yNiBVVEMtLTIwMTktMDMtMDkgVVRDLDM3LjQsMQoyNDgsQnJpZ2h0ZW4sRmVtYWxlLDI2LEJsYWNrLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTEyLTA5VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMiw0LDIsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE1LTA3LTEyVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTUtMDctMTIgVVRDLS0yMDE5LTEyLTA5IFVUQyw1Mi45LDAKMjQ5LEpha2l5YSxGZW1hbGUsNTgsTmF0aXZlLFByZXNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0xMS0wOVQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywzLDIsMixBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE1LTEwLTA2VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTUtMTAtMDYgVVRDLS0yMDE5LTExLTA5IFVUQyw0OS4xLDEKMjUwLEFocmVhbm5hLE1hbGUsNTEsQmxhY2ssQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDYtMDlUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwyLDIsUHJlc2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxNS0wNi0yN1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTA2LTI3IFVUQy0tMjAxOS0wNi0wOSBVVEMsNDcuNCwxCg==" download="histopathology-template2020-02-09.xlsx.csv">
<button class="btn btn-default"><i class="fa fa-save"></i> Download data as csv</button>
</a><!--/html_preserve-->

```r
mydata %>% downloadthis::download_this(output_name = excelName, output_extension = ".xlsx", 
    button_label = "Download data as xlsx", button_type = "primary")
```

<!--html_preserve--><a href="data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,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" download="histopathology-template2020-02-09.xlsx.xlsx">
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

data[order(data$References), ]                                                                                                                                                                                                                    
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Ethan Heinzen, Jason Sinnwell, Elizabeth Atkinson, Tina Gunderson and Gregory Dougherty (2019). arsenal: An Arsenal of 'R' Functions for Large-Scale Statistical Summaries. R package version 3.3.0. https://CRAN.R-project.org/package=arsenal   
Ewen Harrison, Tom Drake and Riinu Ots (2020). finalfit: Quickly Create Elegant Regression Results Tables and Plots when Modelling. R package version 1.0.0. https://github.com/ewenharrison/finalfit                                             
Hadley Wickham and Jennifer Bryan (2019). readxl: Read Excel Files. R package version 1.3.1. https://CRAN.R-project.org/package=readxl                                                                                                            
Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2020). dplyr: A Grammar of Data Manipulation. R package version 0.8.4. https://CRAN.R-project.org/package=dplyr                                                                  
Jim Hester and Gábor Csárdi (2019). pak: Another Approach to Package Installation. R package version 0.1.2. https://CRAN.R-project.org/package=pak                                                                                                
Jim Hester, Gábor Csárdi, Hadley Wickham, Winston Chang, Martin Morgan and Dan Tenenbaum (2019). remotes: R Package Installation from Remote Repositories, Including 'GitHub'. R package version 2.1.0. https://CRAN.R-project.org/package=remotes
Kazuki Yoshida (2019). tableone: Create 'Table 1' to Describe Baseline Characteristics. R package version 0.10.0. https://CRAN.R-project.org/package=tableone                                                                                     
Kirill Müller (2017). here: A Simpler Way to Find Your Files. R package version 0.1. https://CRAN.R-project.org/package=here                                                                                                                      
Makowski, D. & Lüdecke, D. (2019). The report package for R: Ensuring the use of best practices for results reporting. CRAN. Available from https://github.com/easystats/report. doi: .                                                           
Rinker, T. W. & Kurkiewicz, D. (2017). pacman: Package Management for R. version 0.5.0. Buffalo, New York. http://github.com/trinker/pacman                                                                                                       
Stefan Milton Bache and Hadley Wickham (2014). magrittr: A Forward-Pipe Operator for R. R package version 1.5. https://CRAN.R-project.org/package=magrittr                                                                                        
Therneau T (2015). _A Package for Survival Analysis in S_. version2.38, <URL: https://CRAN.R-project.org/package=survival>.                                                                                                                       



```r
report::show_packages(session = sessionInfo()) %>% kableExtra::kable()
```



```r
citation("tidyverse")
```

```

  Wickham et al., (2019). Welcome to the tidyverse. Journal of Open
  Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686

A BibTeX entry for LaTeX users is

  @Article{,
    title = {Welcome to the {tidyverse}},
    author = {Hadley Wickham and Mara Averick and Jennifer Bryan and Winston Chang and Lucy D'Agostino McGowan and Romain François and Garrett Grolemund and Alex Hayes and Lionel Henry and Jim Hester and Max Kuhn and Thomas Lin Pedersen and Evan Miller and Stephan Milton Bache and Kirill Müller and Jeroen Ooms and David Robinson and Dana Paige Seidel and Vitalie Spinu and Kohske Takahashi and Davis Vaughan and Claus Wilke and Kara Woo and Hiroaki Yutani},
    year = {2019},
    journal = {Journal of Open Source Software},
    volume = {4},
    number = {43},
    pages = {1686},
    doi = {10.21105/joss.01686},
  }
```

```r
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

  Ewen Harrison, Tom Drake and Riinu Ots (2020). finalfit: Quickly
  Create Elegant Regression Results Tables and Plots when Modelling. R
  package version 1.0.0. https://github.com/ewenharrison/finalfit

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {finalfit: Quickly Create Elegant Regression Results Tables and Plots when
Modelling},
    author = {Ewen Harrison and Tom Drake and Riinu Ots},
    year = {2020},
    note = {R package version 1.0.0},
    url = {https://github.com/ewenharrison/finalfit},
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
Running under: macOS  10.15.2

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] finalfit_1.0.0  survival_3.1-8  tableone_0.10.0 arsenal_3.3.0  
 [5] readxl_1.3.1    magrittr_1.5    dplyr_0.8.4     report_0.1.0   
 [9] here_0.1        pak_0.1.2       pacman_0.5.1    remotes_2.1.0  

loaded via a namespace (and not attached):
  [1] backports_1.1.5        plyr_1.8.5             dataMaid_1.4.0        
  [4] igraph_1.2.4.2         splines_3.6.0          ggplot2_3.3.0.9000    
  [7] digest_0.6.23          foreach_1.4.7          htmltools_0.4.0.9002  
 [10] xray_0.2               fansi_0.4.1            openxlsx_4.1.4        
 [13] readr_1.3.1            magicfor_0.1.0         ggfittext_0.8.1       
 [16] lpSolve_5.6.15         prettyunits_1.1.1      colorspace_1.4-1      
 [19] mitools_2.4            pan_1.6                haven_2.2.0           
 [22] xfun_0.12              callr_3.4.1            crayon_1.3.4          
 [25] jsonlite_1.6.1         lme4_1.1-21            zoo_1.8-7             
 [28] iterators_1.0.12       glue_1.3.1             survminer_0.4.6       
 [31] gtable_0.3.0.9000      SmartEDA_0.3.3         downloadthis_0.1.0    
 [34] DEoptimR_1.0-8         ISLR_1.2               jomo_2.6-10           
 [37] scales_1.1.0           DBI_1.1.0              GGally_1.4.0          
 [40] Rcpp_1.0.3             xtable_1.8-4           progress_1.2.2        
 [43] foreign_0.8-75         km.ci_0.5-2            survey_3.37           
 [46] DT_0.12                describer_0.2.0        htmlwidgets_1.5.1.9000
 [49] sampling_2.8           httr_1.4.1             RColorBrewer_1.1-2    
 [52] ellipsis_0.3.0         mice_3.7.0             DataExplorer_0.8.1    
 [55] pkgconfig_2.0.3        reshape_0.8.8          farver_2.0.3          
 [58] nnet_7.3-12            utf8_1.1.4             janitor_1.2.1         
 [61] tidyselect_1.0.0       labeling_0.3           rlang_0.4.4.9000      
 [64] later_1.0.0.9002       munsell_0.5.0          reactR_0.4.2          
 [67] cellranger_1.1.0       tools_3.6.0            cli_2.0.1.9000        
 [70] jmvcore_1.2.5          generics_0.0.2         broom_0.5.4           
 [73] evaluate_0.14          stringr_1.4.0          fastmap_1.0.1         
 [76] yaml_2.2.1             fs_1.3.1               processx_3.4.1        
 [79] knitr_1.28             zip_2.0.4              pander_0.6.3          
 [82] robustbase_0.93-5      survMisc_0.5.5         purrr_0.3.3           
 [85] mitml_0.3-7            visdat_0.5.3           nlme_3.1-143          
 [88] jmv_1.0.8              reactable_0.1.0        mime_0.9              
 [91] formatR_1.7            compiler_3.6.0         curl_4.3              
 [94] ggsignif_0.6.0         e1071_1.7-3            tibble_2.1.3          
 [97] stringi_1.4.5          highr_0.8              ps_1.3.0              
[100] parameters_0.4.1.1     forcats_0.4.0          lattice_0.20-38       
[103] whoami_1.3.0           Matrix_1.2-18          nloptr_1.2.1          
[106] ggsci_2.9              KMsurv_0.1-5           vctrs_0.2.2           
[109] pillar_1.4.3           lifecycle_0.1.0        networkD3_0.4         
[112] inspectdf_0.0.7        bsplus_0.1.1           data.table_1.12.8     
[115] insight_0.8.1.1        httpuv_1.5.2           R6_2.4.1              
[118] wakefield_0.3.4        promises_1.1.0.9000    gridExtra_2.3         
[121] rio_0.5.16             writexl_1.2            codetools_0.2-16      
[124] boot_1.3-24            MASS_7.3-51.5          assertthat_0.2.1      
[127] rprojroot_1.3-2        rjson_0.2.20           withr_2.1.2           
[130] explore_0.5.3          bayestestR_0.5.2       parallel_3.6.0        
[133] hms_0.5.3              rpart_4.1-15           grid_3.6.0            
[136] labelled_2.2.2         minqa_1.2.4            tidyr_1.0.2           
[139] class_7.3-15           rmarkdown_2.1          snakecase_0.11.0      
[142] ggpubr_0.2.4           base64enc_0.1-3        shiny_1.4.0.9001      
[145] lubridate_1.7.4       
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

Last update on 2020-02-09 14:01:44  

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
if (!require("remotes")) install.packages("remotes")
if (!require("pacman")) install.packages("pacman")
if (!require("pak")) install.packages("pak")
if (!require("here")) install.packages("here")
knitr::opts_chunk$set(
    eval = TRUE,
    echo = TRUE,
    fig.path = here::here("figs/"),
    message = FALSE,
    warning = FALSE,
    error = FALSE,
    cache = FALSE,
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
  select(-contains("Date")) %>%
  report::report(.)
mydata %>% explore::describe_tbl()
dput(names(mydata))
keycolumns <-  
    mydata %>%  
    sapply(., FUN = dataMaid::isKey) %>%  
    as_tibble() %>%  
    select(  
        which(.[1, ] == TRUE)  
    ) %>%   
    names()  
keycolumns  
mydata %>% 
  select(-keycolumns) %>% 
inspectdf::inspect_types()
mydata %>% 
    select(-keycolumns,
           -contains("Date")) %>% 
  describer::describe() %>% 
  knitr::kable(format = "markdown")
mydata %>% 
    select(-keycolumns) %>% 
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
    select(-keycolumns) %>% 
    inspectdf::inspect_types() %>% 
    dplyr::filter(type == "character") %>% 
    dplyr::select(col_name) %>% 
    pull() %>% 
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
    select(-keycolumns) %>% 
inspectdf::inspect_types() %>% 
  dplyr::filter(type == "numeric") %>% 
  dplyr::select(col_name) %>% 
  pull() %>% 
  unlist() -> numericVariables

numericVariables
mydata %>% 
    select(-keycolumns) %>% 
inspectdf::inspect_types() %>% 
  dplyr::filter(type == "integer") %>% 
  dplyr::select(col_name) %>% 
  pull() %>% 
  unlist() -> integerVariables

integerVariables
mydata %>% 
    select(-keycolumns) %>% 
inspectdf::inspect_types() %>% 
  dplyr::filter(type == "list") %>% 
  dplyr::select(col_name) %>% 
  pull() %>% 
  unlist() -> listVariables
listVariables
is_date <- function(x) inherits(x, c("POSIXct", "POSIXt"))

dateVariables <- 
names(which(sapply(mydata, FUN = is_date) == TRUE))
dateVariables
View(mydata)
reactable::reactable(data = mydata, sortable = TRUE, resizable = TRUE, filterable = TRUE, searchable = TRUE, pagination = TRUE, paginationType = "numbers", showPageSizeOptions = TRUE, highlight = TRUE, striped = TRUE, outlined = TRUE, compact = TRUE, wrap = FALSE, showSortIcon = TRUE, showSortable = TRUE)
summarytools::view(summarytools::dfSummary(mydata %>% select(-keycolumns)))
if(!dir.exists(here::here("out"))) {dir.create(here::here("out"))}

summarytools::view(
  x = summarytools::dfSummary(
    mydata %>% 
      select(-keycolumns)
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
  select(
    -dateVariables
  ) %>% 
  explore::report(
    output_file = "mydata_report.html",
    output_dir = here::here("out") 
    )
glimpse(mydata %>% select(-keycolumns, -dateVariables))
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
  select(Sepal.Length, Sepal.Width) %>%
  explore::explore_all()

iris %>%
  select(Sepal.Length, Sepal.Width, is_versicolor) %>%
  explore::explore_all(target = is_versicolor)

iris %>%
  select(Sepal.Length, Sepal.Width, is_versicolor) %>%
  explore::explore_all(target = is_versicolor, split = FALSE)

iris %>%
  select(Sepal.Length, Sepal.Width, Species) %>%
  explore::explore_all(target = Species)

iris %>%
  select(Sepal.Length, Sepal.Width, Petal.Length) %>%
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
  select(-keycolumns,
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
  select(
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
  select(
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
  select(characterVariables) %>% 
  select(PreinvasiveComponent,
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
  select(continiousVariables) %>% 
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
  pull() -> km_fit_median_definition
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
  pull() -> km_fit_definition
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
citation("tidyverse")
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


