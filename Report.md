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
date: "2019-11-28"
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




---


[Histopathology Research Template 🔬](https://sbalci.github.io/histopathology-template/)


---



# Introduction

- **State the marker of interest, the study objectives, and hypotheses [@Knijn2015]**.^[From Table 1: Proposed items for reporting histopathology studies. Recommendations for reporting histopathology studies: a proposal Virchows Arch (2015) 466:611–615 DOI 10.1007/s00428-015-1762-3]

 


# Materials & Methods

**Describe Materials and Methods as highlighted in [@Knijn2015]**.^[From Table 1: Proposed items for reporting histopathology studies. Recommendations for reporting histopathology studies: a proposal Virchows Arch (2015) 466:611–615 DOI 10.1007/s00428-015-1762-3]

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

![](/Users/serdarbalciold/histopathology-template/figs/plot fake data-1.png)<!-- -->




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
  - Name: 249 entries: Adayah, n = 1; Adeja, n = 1; Adelaina, n = 1 and 246 others (1 missing)
  - Sex: 2 entries: Male, n = 135; Female, n = 114 (1 missing)
  - Age: Mean = 49.05, SD = 13.68, range = [25, 73], 1 missing
  - Race: 6 entries: White, n = 158; Hispanic, n = 46; Black, n = 33 and 3 others (1 missing)
  - PreinvasiveComponent: 2 entries: Absent, n = 192; Present, n = 57 (1 missing)
  - LVI: 2 entries: Absent, n = 163; Present, n = 86 (1 missing)
  - PNI: 2 entries: Absent, n = 174; Present, n = 75 (1 missing)
  - Death: 2 levels: FALSE (n = 81); TRUE (n = 168) and missing (n = 1)
  - Group: 2 entries: Treatment, n = 128; Control, n = 121 (1 missing)
  - Grade: 3 entries: 3, n = 101; 1, n = 80; 2, n = 68 (1 missing)
  - TStage: 4 entries: 4, n = 102; 3, n = 73; 2, n = 52 and 1 other (1 missing)
  - Anti-X-intensity: Mean = 2.41, SD = 0.62, range = [1, 3], 1 missing
  - Anti-Y-intensity: Mean = 1.97, SD = 0.77, range = [1, 3], 1 missing
  - LymphNodeMetastasis: 2 entries: Absent, n = 143; Present, n = 106 (1 missing)
  - Valid: 2 levels: FALSE (n = 139); TRUE (n = 110) and missing (n = 1)
  - Smoker: 2 levels: FALSE (n = 125); TRUE (n = 124) and missing (n = 1)
  - Grade_Level: 3 entries: high, n = 96; moderate, n = 79; low, n = 74 (1 missing)
  - DeathTime: 2 entries: Within1Year, n = 149; MoreThan1Year, n = 101
```



```r
mydata %>% explore::describe_tbl()
```

```
250 observations with 21 variables
19 variables containing missings (NA)
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
"TStage", "Anti-X-intensity", "Anti-Y-intensity", "LymphNodeMetastasis", 
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
|Age                  |numeric       |double       |             250|   49.048193| 13.6814894|25            |         37|         48|         61|73          |
|Race                 |character     |character    |             250|          NA|         NA|Asian         |         NA|         NA|         NA|White       |
|PreinvasiveComponent |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|LVI                  |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|PNI                  |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|Death                |logical       |logical      |             250|          NA|         NA|FALSE         |         NA|         NA|         NA|TRUE        |
|Group                |character     |character    |             250|          NA|         NA|Control       |         NA|         NA|         NA|Treatment   |
|Grade                |character     |character    |             250|          NA|         NA|1             |         NA|         NA|         NA|3           |
|TStage               |character     |character    |             250|          NA|         NA|1             |         NA|         NA|         NA|4           |
|Anti-X-intensity     |numeric       |double       |             250|    2.405623|  0.6222751|1             |          2|          2|          3|3           |
|Anti-Y-intensity     |numeric       |double       |             250|    1.967871|  0.7718405|1             |          1|          2|          3|3           |
|LymphNodeMetastasis  |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|Valid                |logical       |logical      |             250|          NA|         NA|FALSE         |         NA|         NA|         NA|TRUE        |
|Smoker               |logical       |logical      |             250|          NA|         NA|FALSE         |         NA|         NA|         NA|TRUE        |
|Grade_Level          |character     |character    |             250|          NA|         NA|high          |         NA|         NA|         NA|moderate    |
|DeathTime            |character     |character    |             250|          NA|         NA|MoreThan1Year |         NA|         NA|         NA|Within1Year |


**Plot variable types**


```r
mydata %>% select(-keycolumns) %>% inspectdf::inspect_types() %>% inspectdf::show_plot()
```

![](/Users/serdarbalciold/histopathology-template/figs/variable type plot inspectdf-1.png)<!-- -->




```r
# https://github.com/ropensci/visdat
# http://visdat.njtierney.com/articles/using_visdat.html
# https://cran.r-project.org/web/packages/visdat/index.html
# http://visdat.njtierney.com/

# visdat::vis_guess(mydata)

visdat::vis_dat(mydata)
```

![](/Users/serdarbalciold/histopathology-template/figs/variable type plot visdat-1.png)<!-- -->



```r
mydata %>% explore::explore_tbl()
```

![](/Users/serdarbalciold/histopathology-template/figs/variable type plot explore-1.png)<!-- -->



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
[1] "Age"              "Anti-X-intensity" "Anti-Y-intensity"
```


#### Find `numeric` variables




```r
numericVariables <- mydata %>% select(-keycolumns) %>% inspectdf::inspect_types() %>% 
    dplyr::filter(type == "numeric") %>% dplyr::select(col_name) %>% pull() %>% unlist()

numericVariables
```

```
[1] "Age"              "Anti-X-intensity" "Anti-Y-intensity"
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

<!--html_preserve--><div id="htmlwidget-1e686042647165acb8a8" class="reactable html-widget" style="width:auto;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-1e686042647165acb8a8">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"ID":["001","002","003","004","005","006","007","008","009","010","011","012","013","014","015","016","017","018","019","020","021","022","023","024","025","026","027","028","029","030","031","032","033","034","035","036","037","038","039","040","041","042","043","044","045","046","047","048","049","050","051","052","053","054","055","056","057","058","059","060","061","062","063","064","065","066","067","068","069","070","071","072","073","074","075","076","077","078","079","080","081","082","083","084","085","086","087","088","089","090","091","092","093","094","095","096","097","098","099","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250"],"Name":["Rameses","Eulala","Shacara","Ryly","Estefhany","Jacenda","Ilithia","Ahmira","Keawe","Derelys","Dekisha","Nicayla","Brendy","Westonia","Saranda","Rudell","Emmanuella","Trinitie","Jamerious","Aire","Kadiesha","Drexell","Tanvee","Tiffni","Jaylia","Aubriana","Labarron","Carrina","Yishai","Chiquitta","Moses","Adayah","Jasline","Persephonie","Kalyan","Shiniya","Litza","Tavio","Atarah","Amandra","Timarion","Feltus","Jennille","Draxie","Malacia","Cheril","Hanae","Marquice","Nazayah","Eveliz","Darein","Genee","Eriann","Parke","Venna","Emiliano","Xylee","Lilandra","Solveig","Chayim","Jahmez","Kenniyah","Markeysha","Elizebeth","Traiton","Torivio","Latessa","Cheryln","Itcel","Tresa","Divion","Shreyas","Trevor","Naif","Andols","Jaikob","Alecsandra","Angelissa","Latieka","Katlynne","Nervia","Phoeba","Daniella","Brette","Baasil","Tychicus","Keishun","Othell","Nellia","Eshani","Kristo","Darsha","Shawndrika","Lakenya","Maclynn","Princeten","Linder","Dinasti","Jociah","Jemario","Shataka","Shacarra","Cambree","Rahshad","Laytoya","Kaseton","Leshly","Furlon","Wendel","Maimouna","Prayer","Mariajose","Taylia","Vidisha","Cledus","Bandar","Elisabel","Neako","Mycheal","Anneisha","Telesfora","Denishia","Ellianna","Gillard","Gabreille","Haeley","Bethsaida","Winthrop","Filsan","Lolabelle","Lefty","Charleeann","Frederick","Jaewon","Deeandre","Bridgid","Cedra","Ginger","Adylee","Adelaina","Janahla","Emilliano","Gewel","Yecica","Zuhal","Skyshun","Raghav","Saith","Elli","Ellesyn","Kiersten","Celine","Isahia","Mynette","Adeja","Arieliz","Saranne","Enma","Sory","Kaare","Shafeqah","Azaraya","Cormick","Shaunette","Bla","Hickory","Majed","Genesy","Teagon","Anakah","Katelinn","Baha","Vikas","Joshualee",null,"Alegra","Ras","Timoty","Evyana","Laith","Hulbert","Ikira","Amandalee","Kyeon","Frampton","Archita","Angeles","Tyreace","Varnie","Carren","Januarie","Kash","Wojciech","Salinda","Azarel","Elsey","Cindylou","Shawana","Souren","Cleoria","Arri","Paulus","Norva","Kyriakos","Mariabella","Ziya","Olajumoke","Alyssamarie","Sosie","Jaycob","Dulcia","Shaunece","Tanicha","Valinda","Yisselle","Pasqual","Shakeita","Jaylun","Sharlan","Amarieon","Melverine","Katiya","Ketan","Mecayla","Kiyami","Kereen","Nikeesha","Antiona","Zane","Seigo","Jolian","Hennry","Shykeria","Dixie","Jenne","Tanyha","Mikalah","Aquileo","Deshun","Jesselynn","Kisten","Masud","Condy","Gent","Shadarian","Miyla","Yanay","Luevertha","Jetziel","Enyssa"],"Sex":["Male","Female","Female","Male","Male","Male","Male","Female","Male","Male","Female","Male","Male","Male","Female","Male","Male","Female","Male","Male","Female","Female","Male","Female","Female","Male","Female","Male","Male","Female","Male","Male","Female","Male","Male","Male","Male","Female","Female","Male","Male","Female","Male","Male","Male","Female","Female","Female","Male","Male","Male","Female","Male","Female","Female","Male","Male","Male","Male","Male","Male","Male","Male","Female","Female","Female","Female","Male","Male","Female","Female","Male","Female","Female","Male","Male","Male","Female","Male","Female","Female","Male","Male","Female","Male","Female","Male","Female","Male","Female","Female","Male","Male",null,"Female","Female","Female","Male","Female","Female","Female","Male","Female","Male","Male","Male","Male","Male","Male","Female","Female","Male","Male","Female","Female","Female","Male","Female","Female","Female","Female","Male","Male","Male","Male","Female","Female","Female","Female","Male","Male","Male","Female","Male","Male","Male","Male","Male","Female","Female","Male","Female","Male","Male","Female","Male","Male","Male","Male","Male","Female","Male","Female","Female","Female","Male","Male","Female","Male","Female","Female","Male","Female","Female","Female","Male","Male","Male","Female","Male","Female","Male","Male","Male","Male","Female","Female","Male","Male","Female","Male","Female","Female","Female","Female","Female","Female","Male","Male","Female","Male","Male","Female","Female","Male","Female","Female","Male","Male","Male","Female","Female","Female","Male","Female","Female","Female","Female","Female","Male","Male","Male","Female","Male","Male","Male","Male","Male","Male","Female","Female","Male","Male","Female","Female","Male","Male","Male","Female","Male","Female","Male","Female","Male","Female","Male","Female","Female","Female","Female","Male","Male","Female","Male","Male","Male","Male","Female","Female","Female"],"Age":[29,47,56,67,68,69,69,63,54,41,48,31,65,54,41,39,42,60,39,58,55,61,54,28,31,49,47,40,47,45,64,28,71,60,30,49,26,64,44,48,26,54,49,44,73,32,68,37,46,65,59,43,36,26,52,47,43,45,48,73,31,42,66,47,70,25,57,27,61,39,43,70,72,65,53,56,57,34,59,68,32,50,39,57,34,60,32,43,33,69,32,43,45,37,36,53,43,41,32,35,39,31,65,43,59,41,51,37,67,53,64,41,57,42,29,58,35,32,71,61,35,28,49,57,50,61,72,66,56,58,69,43,39,33,51,41,54,28,47,73,65,55,30,57,39,51,45,34,65,54,41,36,27,49,73,54,62,49,69,65,34,52,35,66,32,62,62,63,26,43,70,48,52,30,40,60,34,34,45,30,46,63,54,65,37,67,61,35,37,72,70,64,39,46,62,"NA",38,42,63,63,26,42,69,31,62,35,32,67,72,50,61,68,67,59,64,40,32,48,64,43,53,34,65,45,53,43,45,41,34,46,42,66,25,65,27,57,33,60,58,30,40,72,62,37,39,68,50,29,26,53],"Race":["White","White","White","White","White","Hispanic","White","White","White","Hispanic","Hispanic","Black","White","Black","Black","Hispanic","White","White","White","Black","Black","White","White","Black","Hispanic","White","White","White","White","White","White","White","White","Hispanic","White","Hispanic","White","White","Hispanic","White","White","Hispanic","White","Black","White","White","Asian","Hispanic","White","Other","White","Hispanic","Hispanic","White","White","Hispanic","White","White","White","White","White","Asian","Hispanic","Hispanic","White","White","White","White","White","White","Hispanic","White","White","White","White","White","Asian","Hispanic","White","White","White","White","Hispanic","White","Black","White","Hispanic","White","White","White","Hispanic","Hispanic","White","Asian","White","Hispanic","White","White","White","White","White","White","White","White","Hispanic","White","Black","Hispanic","White","White","White","White","White","Bi-Racial","White","White","White","Hispanic","White","White","White","White","White","Hispanic","White","White","Hispanic","Hispanic","White","White","White","White","Hispanic","White","White","Black","Black","Hispanic","White","Black","White","White","Hispanic","White","Hispanic","Hispanic","Black","Black","White","Black","White","White","White","Hispanic","White","White","White","White","Bi-Racial","White","White","White","White","White","Asian","Black","Asian","White","White","Black","Hispanic","Hispanic","Hispanic","Black","Black","White","White","Black",null,"Hispanic","Black","White","White","White","White","White","White","White","White","White","White","Black","White","White","White","White","Hispanic","Hispanic","White","White","Black","White","White","Black","White","Asian","Hispanic","White","Black","White","White","White","White","Black","White","White","White","Black","White","Bi-Racial","White","White","White","White","White","Hispanic","Black","White","White","White","Hispanic","White","White","Hispanic","Hispanic","Black","Black","Hispanic","White","Black","White","White","White","White","Black","Bi-Racial","White","Black","White","White"],"PreinvasiveComponent":["Absent","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent",null,"Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Present","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent"],"LVI":["Present","Absent","Absent","Present","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Absent","Present","Present","Present","Absent","Absent","Present","Present","Absent","Absent","Present","Absent","Present","Present","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Absent","Present","Absent","Present","Present","Present","Present","Absent","Absent","Absent","Present","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Present","Present","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Present","Present","Present","Present","Absent","Present","Present","Present","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Present","Present","Present","Present","Absent","Present","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent",null,"Absent","Present","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent"],"PNI":["Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present",null,"Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Present","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent"],"LastFollowUpDate":["2019-05-28T00:00:00","2019-05-28T00:00:00","2019-06-28T00:00:00","2019-07-28T00:00:00","2019-03-28T00:00:00","2019-08-28T00:00:00","2019-08-28T00:00:00","2019-07-28T00:00:00","2019-02-28T00:00:00","2019-11-28T00:00:00","2019-01-28T00:00:00","2019-11-28T00:00:00","2019-01-28T00:00:00","2019-07-28T00:00:00","2019-05-28T00:00:00","2019-07-28T00:00:00","2019-08-28T00:00:00","2019-01-28T00:00:00","2019-08-28T00:00:00","2019-11-28T00:00:00","2019-08-28T00:00:00","2019-02-28T00:00:00","2018-12-28T00:00:00","2019-08-28T00:00:00","2019-05-28T00:00:00","2019-02-28T00:00:00","2019-02-28T00:00:00","2019-07-28T00:00:00","2019-01-28T00:00:00","2019-03-28T00:00:00","2018-12-28T00:00:00","2019-07-28T00:00:00","2019-05-28T00:00:00","2019-01-28T00:00:00","2019-03-28T00:00:00","2019-07-28T00:00:00","2019-11-28T00:00:00","2019-03-28T00:00:00","2018-12-28T00:00:00","2018-12-28T00:00:00","2019-09-28T00:00:00","2019-03-28T00:00:00","2019-03-28T00:00:00","2019-08-28T00:00:00","2019-03-28T00:00:00","2019-10-28T00:00:00","2019-08-28T00:00:00","2019-11-28T00:00:00","2019-11-28T00:00:00","2019-03-28T00:00:00","2019-10-28T00:00:00","2019-04-28T00:00:00","2019-04-28T00:00:00","2019-01-28T00:00:00","2019-11-28T00:00:00","2019-02-28T00:00:00","2019-04-28T00:00:00","2019-03-28T00:00:00","2019-04-28T00:00:00","2019-11-28T00:00:00","2019-08-28T00:00:00","2019-04-28T00:00:00","2019-07-28T00:00:00","2019-05-28T00:00:00","2019-09-28T00:00:00","2019-11-28T00:00:00","2019-03-28T00:00:00","2019-01-28T00:00:00","2019-07-28T00:00:00","2019-01-28T00:00:00","2019-05-28T00:00:00","2019-06-28T00:00:00","2019-02-28T00:00:00","2019-02-28T00:00:00","2019-08-28T00:00:00","2019-11-28T00:00:00","2019-04-28T00:00:00","2019-11-28T00:00:00","2019-07-28T00:00:00","2019-06-28T00:00:00","2019-01-28T00:00:00","2019-03-28T00:00:00","2019-03-28T00:00:00","2019-11-28T00:00:00","2019-09-28T00:00:00","2019-07-28T00:00:00","2019-11-28T00:00:00","2019-06-28T00:00:00","2019-11-28T00:00:00","2019-02-28T00:00:00","2019-04-28T00:00:00","2018-12-28T00:00:00","2019-07-28T00:00:00","2019-09-28T00:00:00","2019-03-28T00:00:00","2019-01-28T00:00:00","2019-07-28T00:00:00","2019-10-28T00:00:00","2019-06-28T00:00:00","2019-11-28T00:00:00","2019-11-28T00:00:00","2019-06-28T00:00:00","2019-01-28T00:00:00","2019-07-28T00:00:00","2019-11-28T00:00:00","2019-02-28T00:00:00","2019-10-28T00:00:00","2018-12-28T00:00:00","2019-09-28T00:00:00","2019-05-28T00:00:00","2019-03-28T00:00:00","2019-02-28T00:00:00","2019-08-28T00:00:00","2019-03-28T00:00:00","2019-07-28T00:00:00","2019-03-28T00:00:00","2019-01-28T00:00:00","2019-04-28T00:00:00","2019-10-28T00:00:00","2019-01-28T00:00:00","2019-08-28T00:00:00","2018-12-28T00:00:00","2018-12-28T00:00:00","2018-12-28T00:00:00","2019-11-28T00:00:00","2018-12-28T00:00:00","2019-11-28T00:00:00","2019-11-28T00:00:00","2019-01-28T00:00:00","2019-09-28T00:00:00","2018-12-28T00:00:00","2019-05-28T00:00:00","2019-09-28T00:00:00","2019-06-28T00:00:00","2019-05-28T00:00:00","2019-07-28T00:00:00","2019-03-28T00:00:00","2019-09-28T00:00:00","2019-08-28T00:00:00","2019-04-28T00:00:00","2019-08-28T00:00:00","2019-11-28T00:00:00","2019-07-28T00:00:00","2018-12-28T00:00:00","2019-11-28T00:00:00","2019-02-28T00:00:00","2019-09-28T00:00:00","2019-02-28T00:00:00","2019-04-28T00:00:00","2019-08-28T00:00:00","2019-08-28T00:00:00","2019-02-28T00:00:00","2019-07-28T00:00:00","2019-07-28T00:00:00","2019-08-28T00:00:00","2019-10-28T00:00:00","2019-08-28T00:00:00","2019-09-28T00:00:00","2019-09-28T00:00:00","2019-05-28T00:00:00","2019-06-28T00:00:00","2019-08-28T00:00:00","2019-03-28T00:00:00","2019-11-28T00:00:00","2019-10-28T00:00:00","2019-01-28T00:00:00","2019-08-28T00:00:00","2019-02-28T00:00:00","2019-03-28T00:00:00","2019-01-28T00:00:00","2019-10-28T00:00:00","2019-07-28T00:00:00",null,"2019-10-28T00:00:00","2019-09-28T00:00:00","2019-06-28T00:00:00","2019-03-28T00:00:00","2019-07-28T00:00:00","2019-09-28T00:00:00","2019-11-28T00:00:00","2019-02-28T00:00:00","2018-12-28T00:00:00","2019-01-28T00:00:00","2019-10-28T00:00:00","2019-07-28T00:00:00","2019-01-28T00:00:00","2019-05-28T00:00:00","2019-03-28T00:00:00","2019-06-28T00:00:00","2019-05-28T00:00:00","2019-01-28T00:00:00","2019-08-28T00:00:00","2019-02-28T00:00:00","2019-11-28T00:00:00","2019-02-28T00:00:00","2019-01-28T00:00:00","2019-01-28T00:00:00","2019-10-28T00:00:00","2019-04-28T00:00:00","2019-04-28T00:00:00","2019-03-28T00:00:00","2018-12-28T00:00:00","2019-06-28T00:00:00","2019-06-28T00:00:00","2019-07-28T00:00:00","2019-05-28T00:00:00","2019-09-28T00:00:00","2019-07-28T00:00:00","2019-08-28T00:00:00","2019-02-28T00:00:00","2019-10-28T00:00:00","2019-04-28T00:00:00","2019-03-28T00:00:00","2019-09-28T00:00:00","2019-09-28T00:00:00","2019-09-28T00:00:00","2019-05-28T00:00:00","2018-12-28T00:00:00","2019-02-28T00:00:00","2019-01-28T00:00:00","2019-10-28T00:00:00","2019-05-28T00:00:00","2019-05-28T00:00:00","2019-11-28T00:00:00","2019-08-28T00:00:00","2019-01-28T00:00:00","2019-09-28T00:00:00","2018-12-28T00:00:00","2019-09-28T00:00:00","2019-04-28T00:00:00","2019-07-28T00:00:00","2019-01-28T00:00:00","2019-07-28T00:00:00","2019-06-28T00:00:00","2019-06-28T00:00:00","2019-10-28T00:00:00","2019-10-28T00:00:00","2019-02-28T00:00:00","2019-11-28T00:00:00","2019-08-28T00:00:00","2019-08-28T00:00:00","2019-02-28T00:00:00","2019-01-28T00:00:00","2019-03-28T00:00:00","2019-10-28T00:00:00","2019-08-28T00:00:00","2019-06-28T00:00:00","2019-09-28T00:00:00","2019-03-28T00:00:00","2019-10-28T00:00:00"],"Death":[true,true,true,true,true,true,false,false,true,true,true,true,true,true,false,false,false,true,true,true,false,false,true,false,false,true,true,true,false,true,false,true,false,true,true,true,false,true,false,false,true,true,false,true,false,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,true,true,false,false,true,true,false,true,true,true,true,false,true,true,true,false,true,true,true,true,false,true,false,false,false,true,true,false,false,false,false,false,true,true,true,false,true,true,false,false,false,true,true,true,true,true,true,true,true,true,true,false,true,true,true,false,true,true,true,false,true,true,false,true,false,false,false,true,false,true,false,true,false,false,false,false,true,true,false,true,false,true,true,true,true,true,true,true,true,true,true,false,false,false,true,false,true,true,true,false,true,true,true,false,true,true,false,false,true,false,true,true,true,false,true,false,true,true,true,false,false,true,true,true,false,true,true,true,true,false,true,true,true,false,true,false,true,true,true,false,true,true,false,true,true,true,true,true,true,true,true,true,true,true,null,false,false,true,false,true,true,false,false,true,false,true,true,true,true,false,true,false,true,true,false,true,true,false,true,true,true,true],"Group":["Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Control","Control","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Control","Treatment","Control","Control","Treatment","Control","Control","Treatment","Treatment","Control","Control","Treatment","Control","Treatment","Control","Treatment","Treatment","Treatment","Control","Control","Treatment","Control","Control","Control","Control","Treatment","Treatment","Treatment","Control","Treatment","Control","Control","Control","Treatment","Treatment","Control","Control","Treatment","Control","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Treatment","Control","Control","Treatment","Control","Control","Control","Treatment","Control","Control","Control","Control","Treatment","Treatment","Treatment","Treatment","Control","Control","Treatment","Treatment","Control","Control","Control","Treatment","Control","Treatment","Treatment","Treatment","Control","Treatment","Control","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Control","Control","Control","Control","Treatment","Control","Control","Treatment","Treatment","Control","Control","Treatment","Treatment","Treatment","Control","Treatment","Control","Control","Treatment","Treatment","Control","Control","Control","Treatment","Control","Treatment","Control","Control","Treatment","Control","Control","Treatment","Treatment","Treatment","Treatment","Control","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Control","Control","Treatment","Control","Treatment","Treatment","Control","Control","Control","Treatment","Control","Treatment","Treatment","Treatment","Control","Control","Control","Control","Control","Treatment","Treatment","Control","Treatment","Treatment","Control","Control","Control","Treatment","Treatment",null,"Control","Treatment","Control","Control","Treatment","Control","Control","Treatment","Control","Treatment","Treatment","Control","Treatment","Treatment","Treatment","Control","Control","Treatment","Treatment","Control","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Control","Control","Treatment","Control","Treatment","Control","Treatment","Control","Treatment","Treatment","Treatment","Control","Treatment","Control","Treatment","Treatment","Control","Treatment","Treatment","Control","Treatment","Control","Treatment","Control","Control","Control","Control","Treatment","Control","Control","Treatment","Control","Control","Control","Treatment","Control","Control","Treatment","Treatment","Control","Control","Treatment","Control","Treatment","Treatment","Control","Treatment"],"Grade":["2","1","3","1","3","2","2","3","2","3","1","1","1","3","1","2","3","2","3","1","1","1","3","1","1","3","2","3","1","3","1","2","2","2","3","1","3","2","1","2","1","2","3","3","3","3","2","1","3","2","1","1","1","3","3","1","1","1","2","2","2","3","3","3","2","3","3","2","3","3","3","3","2","1","3","1","3","3","3","1",null,"1","1","1","3","3","1","3","3","2","1","1","1","2","3","2","3","3","1","2","3","2","1","1","3","1","1","2","3","1","1","1","3","3","1","1","1","3","2","1","2","2","1","3","2","3","1","2","3","3","3","3","3","1","1","2","3","3","1","2","3","2","1","3","2","3","1","2","3","1","2","1","2","3","3","3","3","2","3","1","2","2","3","1","2","3","3","3","1","3","1","1","3","3","1","3","2","3","1","3","1","3","3","2","3","3","3","3","2","1","3","3","2","3","3","1","2","2","2","1","1","2","2","1","1","3","2","2","1","1","3","2","2","3","3","3","3","2","2","1","2","2","3","3","1","2","1","1","3","3","1","1","1","2","2","3","1","2","2","2","2","3","3","3","3","3","1","1","3","2"],"TStage":["4","2","1","4","4","4","1","3","4","1","3","4","1","3","4","4","4","4","2","1","4","4","4","3","4","4","3","3","4","3","3","2","4","4","4","3","3","3","2","4","4","4","3","1","4","2","2","3","4","2","3","3","2","4","1","4","2","4","4","2","3","1","4","1","4","3","4","4","4","4","2","4","4","4","4","4","4","3","3","2","4","4","4","2","3","4","3","4","4","2","4","2","2","4","4","4","4","4","4","1","4","3","3","4","4","3","2","3","3","4","2","2","3","4","2","1","4","3","4","4","4","4","2","3","3","4","4","2","1","4","2","4","2","1","2","4","4","4","4","4","2","4","3","1","4","4","2","4","4","2","4","3","4","2","3","2","2","4","4","3","3","3","4","2","2","3","3","4","2","4","3","2","3","3","3","3","1","3","4","3","4","3","4","4","4","3","3","3","3","2","4","1","1","2","4","4","3","3","4","2","4","3","2","2","3","3","1","3","3","3","4","4","2","3","2","4","4","2","2","2","3","3","3","1","3","2","1","2",null,"3","4","4","3","2","3","4","3","3","1","3","3","3","2","3","3","2","2","4","1","2"],"Anti-X-intensity":[3,2,3,2,3,3,2,2,2,3,3,3,1,2,3,3,3,2,3,2,3,2,1,2,2,1,3,2,2,3,3,2,2,1,2,1,3,3,2,2,3,2,3,3,2,2,2,3,2,2,2,3,2,3,3,3,2,3,2,3,3,2,3,2,2,3,2,3,3,2,3,3,3,2,2,2,1,2,3,1,3,2,3,2,3,3,"NA",3,3,2,3,2,2,3,3,3,3,1,3,2,1,3,1,3,2,2,2,3,3,3,1,2,2,2,3,2,2,3,3,2,2,3,2,2,3,3,1,3,3,2,2,3,3,2,2,3,2,2,2,3,2,2,2,3,2,3,3,2,3,3,2,2,2,2,3,2,3,2,3,3,1,2,2,2,3,2,3,3,3,2,2,3,3,3,2,2,2,2,3,3,3,3,3,3,2,3,2,3,3,2,2,2,1,1,3,3,3,2,1,3,2,3,3,3,2,3,2,3,2,1,3,2,2,3,3,3,2,2,2,3,2,3,2,3,2,3,3,2,3,2,3,3,2,1,3,3,3,2,3,3,3,2,3,2,2,2,2,3,2,2],"Anti-Y-intensity":[3,3,1,3,2,2,"NA",1,1,1,2,1,2,3,3,2,1,1,2,3,1,1,2,2,1,3,2,3,1,1,1,2,2,2,3,1,1,1,1,2,3,2,1,3,3,3,2,2,2,3,3,2,2,2,1,2,3,2,1,3,1,2,3,3,1,1,3,1,1,2,2,2,1,3,3,2,2,3,2,2,1,3,3,1,2,2,2,2,2,2,2,2,3,1,3,3,2,3,1,2,2,1,3,1,1,1,1,2,2,3,3,2,3,2,2,3,3,1,1,1,1,2,3,3,3,2,1,2,3,2,2,1,2,1,2,2,2,1,1,1,3,3,3,1,2,2,3,2,1,3,3,3,1,2,2,1,2,1,2,3,1,1,2,2,2,2,3,1,2,1,1,2,2,3,1,1,3,2,2,2,1,2,1,2,1,1,2,3,2,1,2,3,2,2,3,2,3,2,3,1,2,3,2,1,2,3,1,2,2,3,1,2,1,3,2,1,2,1,2,1,2,3,2,2,1,3,1,3,3,3,1,1,1,3,3,1,2,2,2,2,2,3,3,2,3,1,1,3,2,2],"LymphNodeMetastasis":["Present","Present","Present","Present","Present","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Present","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Present","Present","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Present","Present","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Present","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Present","Present","Present","Present","Present","Present","Present","Present","Absent","Present","Present","Present","Absent","Absent","Absent","Absent","Present","Present","Present","Absent","Absent","Present","Absent","Present","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Absent","Present","Present","Present","Absent","Absent","Absent","Present","Present","Present","Absent","Absent","Present","Present","Present","Present","Absent","Present","Absent","Absent","Absent","Present",null,"Absent","Absent","Present","Absent","Present","Absent","Present"],"Valid":[true,false,false,false,false,false,true,false,true,false,true,false,true,true,true,false,false,false,false,false,false,false,true,true,true,true,false,false,true,false,true,true,false,false,false,false,true,true,false,true,false,false,null,true,true,false,true,true,false,false,false,false,false,false,false,true,false,true,true,false,false,false,true,true,true,true,true,false,false,false,false,true,true,false,true,false,true,true,true,false,false,true,true,true,true,false,false,false,false,true,false,true,true,true,true,true,false,false,true,false,true,false,true,true,true,false,false,true,false,true,true,false,false,true,true,false,false,true,true,false,true,false,true,false,false,false,false,false,true,true,false,false,true,false,false,true,true,false,false,false,false,true,false,true,false,false,false,false,true,true,true,false,false,false,true,true,true,true,false,false,true,false,false,false,true,false,true,true,true,false,false,false,false,false,false,false,false,false,false,true,true,false,false,false,true,false,true,false,false,false,false,true,false,false,true,false,false,false,false,false,true,true,false,true,false,false,false,true,true,true,true,true,true,false,true,true,false,true,true,false,false,true,true,false,true,true,false,false,false,false,true,true,false,false,false,false,false,true,true,false,true,true,false,false,false,true,false,false,true,true],"Smoker":[true,true,true,false,true,true,false,false,false,false,false,true,false,false,false,false,true,false,false,false,false,false,true,true,true,true,true,true,true,false,true,false,true,true,true,true,true,false,true,false,false,false,false,false,true,false,false,true,false,false,true,false,true,true,false,false,false,true,false,true,true,true,false,false,false,true,true,true,false,true,true,false,true,false,true,false,true,true,true,true,false,true,false,true,true,false,false,false,false,false,true,true,true,true,null,false,false,true,false,false,false,true,false,false,true,true,true,true,false,true,true,true,false,true,true,false,true,false,true,false,true,true,true,true,true,false,true,true,true,true,true,false,true,false,true,false,true,true,true,true,false,true,false,true,true,true,false,false,false,false,false,false,true,false,false,false,true,true,true,false,false,true,false,false,true,true,false,false,false,true,false,true,true,false,false,false,true,false,false,false,true,false,true,false,false,true,false,true,true,false,false,true,true,true,false,true,true,true,true,true,false,false,true,false,false,false,true,true,true,true,false,true,false,true,false,false,false,false,true,false,false,false,false,false,true,true,false,true,false,true,false,false,true,false,false,false,true,false,true,true,true,false,true,false,false,false,true,false,false,false],"Grade_Level":["moderate","high","low","moderate","high","high","low","moderate","high","high","moderate","low","moderate","high","low","high","low","high","low","low","moderate","moderate","moderate","moderate","moderate","high","moderate","low","moderate","low","high","high","low","moderate","high","low","high","moderate","low","low","high","high","moderate","moderate","low","low","moderate","high","moderate","high","moderate","moderate","low","moderate","high","low","high","low","moderate","low","high","moderate","high","moderate","moderate","high","high","high","high","moderate","moderate","high","high","high","high","high","high","low","low","moderate","low","high","high","high","moderate","moderate","low","moderate","low","low","moderate","moderate","moderate","moderate","high","high","high","high","high","low","high","low","moderate","low","moderate","high","high","high","high","low","low","low","low","high","moderate","low","high","high","high","moderate","low","low","high","moderate","high","low","high","moderate","low","high","moderate","moderate","low","low","moderate","high","low","low","moderate","low","high","high","low","high","moderate","high","high","moderate","high","moderate","moderate","high","high","high","low",null,"high","high","low","low","moderate","moderate","low","low","low","moderate","low","moderate","low","high","high","high","moderate","moderate","low","low","moderate","high","moderate","moderate","low","low","moderate","low","low","low","high","moderate","moderate","low","high","low","low","low","moderate","moderate","moderate","high","high","moderate","high","low","moderate","low","low","moderate","low","moderate","high","high","moderate","moderate","high","low","moderate","high","high","low","moderate","high","low","moderate","high","high","high","moderate","high","high","moderate","high","high","high","high","high","high","high","low","high","moderate","moderate","low","moderate","low","high","high","low","high","moderate","high","low"],"SurgeryDate":["2019-02-16T00:00:00","2018-10-07T00:00:00","2018-12-19T00:00:00","2019-01-09T00:00:00","2018-06-17T00:00:00","2018-11-24T00:00:00","2019-02-14T00:00:00","2018-09-14T00:00:00","2018-06-23T00:00:00","2019-03-08T00:00:00","2018-02-10T00:00:00","2018-12-19T00:00:00","2018-09-02T00:00:00","2019-01-05T00:00:00","2018-06-10T00:00:00","2019-01-21T00:00:00","2019-01-23T00:00:00","2018-07-15T00:00:00","2019-05-19T00:00:00","2019-02-08T00:00:00","2018-09-15T00:00:00","2018-06-29T00:00:00","2018-03-17T00:00:00","2018-09-30T00:00:00","2018-12-25T00:00:00","2018-06-04T00:00:00","2018-11-13T00:00:00","2018-10-28T00:00:00","2018-10-02T00:00:00","2018-10-02T00:00:00","2018-02-22T00:00:00","2019-04-06T00:00:00","2018-09-18T00:00:00","2018-06-19T00:00:00","2018-12-25T00:00:00","2019-02-01T00:00:00","2019-03-29T00:00:00","2018-08-15T00:00:00","2018-03-12T00:00:00","2018-09-06T00:00:00","2018-11-23T00:00:00","2018-12-01T00:00:00","2018-06-25T00:00:00","2019-02-10T00:00:00","2018-08-20T00:00:00","2019-05-14T00:00:00","2019-03-16T00:00:00","2019-08-03T00:00:00","2019-03-03T00:00:00","2018-07-04T00:00:00","2019-07-21T00:00:00","2018-09-28T00:00:00","2018-06-07T00:00:00","2018-07-09T00:00:00","2019-03-10T00:00:00","2018-09-15T00:00:00","2018-05-13T00:00:00","2018-06-06T00:00:00",null,"2019-01-14T00:00:00","2019-01-11T00:00:00","2018-07-28T00:00:00","2018-12-04T00:00:00","2018-07-08T00:00:00","2019-04-29T00:00:00","2019-05-20T00:00:00","2018-11-19T00:00:00","2018-09-16T00:00:00","2018-12-13T00:00:00","2018-07-20T00:00:00","2018-09-11T00:00:00","2018-09-28T00:00:00","2018-07-26T00:00:00","2018-08-31T00:00:00","2019-03-04T00:00:00","2019-08-29T00:00:00","2018-08-23T00:00:00","2019-03-11T00:00:00","2019-02-07T00:00:00","2018-11-22T00:00:00","2018-04-30T00:00:00","2018-04-02T00:00:00","2018-10-22T00:00:00","2019-06-12T00:00:00","2019-04-22T00:00:00","2018-09-21T00:00:00","2019-04-11T00:00:00","2019-02-18T00:00:00","2019-08-07T00:00:00","2018-10-05T00:00:00","2018-09-03T00:00:00","2018-02-27T00:00:00","2019-01-05T00:00:00","2019-02-02T00:00:00","2018-04-24T00:00:00","2018-05-14T00:00:00","2018-11-10T00:00:00","2019-06-07T00:00:00","2018-07-30T00:00:00","2019-07-24T00:00:00","2019-07-10T00:00:00","2018-09-06T00:00:00","2018-07-08T00:00:00","2018-12-20T00:00:00","2019-03-08T00:00:00","2018-03-14T00:00:00","2019-07-24T00:00:00","2018-08-17T00:00:00","2019-01-25T00:00:00","2018-11-10T00:00:00","2018-11-04T00:00:00","2018-11-15T00:00:00","2019-05-23T00:00:00","2018-05-28T00:00:00","2018-12-23T00:00:00","2018-05-02T00:00:00","2018-09-29T00:00:00","2018-12-10T00:00:00","2019-02-22T00:00:00","2018-07-28T00:00:00","2019-01-01T00:00:00","2018-04-12T00:00:00","2018-02-21T00:00:00","2018-02-14T00:00:00","2019-08-11T00:00:00","2018-01-15T00:00:00","2019-04-08T00:00:00","2019-02-14T00:00:00","2018-03-26T00:00:00","2019-01-31T00:00:00","2018-03-13T00:00:00","2019-01-12T00:00:00","2018-12-13T00:00:00","2019-02-13T00:00:00","2018-09-23T00:00:00","2019-01-02T00:00:00","2018-08-04T00:00:00","2018-12-30T00:00:00","2019-04-01T00:00:00","2018-05-27T00:00:00","2019-01-01T00:00:00","2019-01-03T00:00:00","2019-04-02T00:00:00","2018-06-15T00:00:00","2019-06-18T00:00:00","2018-09-05T00:00:00","2018-11-11T00:00:00","2018-07-03T00:00:00","2018-12-06T00:00:00","2018-09-21T00:00:00","2016-12-03T00:00:00","2017-03-20T00:00:00","2017-03-13T00:00:00","2017-06-29T00:00:00","2018-04-18T00:00:00","2017-05-08T00:00:00","2017-12-29T00:00:00","2017-07-23T00:00:00","2018-07-24T00:00:00","2016-09-22T00:00:00","2016-10-27T00:00:00","2016-10-21T00:00:00","2017-11-16T00:00:00","2017-11-15T00:00:00","2018-10-27T00:00:00","2017-04-12T00:00:00","2017-11-05T00:00:00","2017-09-10T00:00:00","2017-02-19T00:00:00","2016-06-27T00:00:00","2018-05-10T00:00:00","2018-06-03T00:00:00","2017-03-08T00:00:00","2017-11-01T00:00:00","2017-05-18T00:00:00","2017-03-02T00:00:00","2016-10-09T00:00:00","2018-05-01T00:00:00","2017-01-30T00:00:00","2017-02-22T00:00:00","2017-07-09T00:00:00","2017-10-29T00:00:00","2016-02-22T00:00:00","2016-12-14T00:00:00","2017-08-03T00:00:00","2016-12-19T00:00:00","2017-12-23T00:00:00","2016-05-17T00:00:00","2017-07-21T00:00:00","2017-10-30T00:00:00","2017-05-30T00:00:00","2018-04-16T00:00:00","2016-09-22T00:00:00","2018-12-02T00:00:00","2017-04-28T00:00:00","2017-06-10T00:00:00","2017-10-22T00:00:00","2017-07-05T00:00:00","2016-11-11T00:00:00","2017-09-28T00:00:00","2017-04-12T00:00:00","2016-02-13T00:00:00","2018-05-11T00:00:00","2017-06-18T00:00:00","2017-05-28T00:00:00","2017-01-09T00:00:00","2017-09-13T00:00:00","2018-01-08T00:00:00","2018-04-14T00:00:00","2017-09-24T00:00:00","2017-08-29T00:00:00","2017-06-13T00:00:00","2018-02-11T00:00:00","2018-08-18T00:00:00","2017-06-30T00:00:00","2017-02-26T00:00:00","2016-07-13T00:00:00","2016-05-30T00:00:00","2016-06-29T00:00:00","2017-05-31T00:00:00","2018-05-21T00:00:00","2016-11-21T00:00:00","2017-07-08T00:00:00","2018-10-10T00:00:00","2018-04-20T00:00:00","2015-04-12T00:00:00","2016-05-29T00:00:00","2015-11-29T00:00:00","2015-01-10T00:00:00","2015-10-18T00:00:00","2016-04-23T00:00:00","2014-06-16T00:00:00","2014-10-21T00:00:00","2016-04-23T00:00:00","2015-12-18T00:00:00","2016-01-05T00:00:00","2015-07-22T00:00:00","2015-08-14T00:00:00","2016-01-02T00:00:00","2016-09-06T00:00:00","2016-01-22T00:00:00","2015-11-14T00:00:00","2014-12-10T00:00:00","2014-08-08T00:00:00","2016-03-24T00:00:00","2015-08-24T00:00:00","2015-05-23T00:00:00","2015-08-22T00:00:00","2015-07-29T00:00:00","2016-08-19T00:00:00"],"DeathTime":["Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","MoreThan1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year"]},"columns":[{"accessor":"ID","name":"ID","type":"character"},{"accessor":"Name","name":"Name","type":"character"},{"accessor":"Sex","name":"Sex","type":"character"},{"accessor":"Age","name":"Age","type":"numeric"},{"accessor":"Race","name":"Race","type":"character"},{"accessor":"PreinvasiveComponent","name":"PreinvasiveComponent","type":"character"},{"accessor":"LVI","name":"LVI","type":"character"},{"accessor":"PNI","name":"PNI","type":"character"},{"accessor":"LastFollowUpDate","name":"LastFollowUpDate","type":"Date"},{"accessor":"Death","name":"Death","type":"logical"},{"accessor":"Group","name":"Group","type":"character"},{"accessor":"Grade","name":"Grade","type":"character"},{"accessor":"TStage","name":"TStage","type":"character"},{"accessor":"Anti-X-intensity","name":"Anti-X-intensity","type":"numeric"},{"accessor":"Anti-Y-intensity","name":"Anti-Y-intensity","type":"numeric"},{"accessor":"LymphNodeMetastasis","name":"LymphNodeMetastasis","type":"character"},{"accessor":"Valid","name":"Valid","type":"logical"},{"accessor":"Smoker","name":"Smoker","type":"logical"},{"accessor":"Grade_Level","name":"Grade_Level","type":"character"},{"accessor":"SurgeryDate","name":"SurgeryDate","type":"Date"},{"accessor":"DeathTime","name":"DeathTime","type":"character"}],"resizable":true,"filterable":true,"searchable":true,"defaultPageSize":10,"showPageSizeOptions":true,"pageSizeOptions":[10,25,50,100],"paginationType":"numbers","showPageInfo":true,"minRows":1,"highlight":true,"outlined":true,"striped":true,"compact":true,"nowrap":true,"showSortable":true,"dataKey":"19becc3ef648c0abfdd32963d645dd65"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->








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
$ Sex                  <chr> "Male", "Female", "Female", "Male", "Male", "Mal…
$ Age                  <dbl> 29, 47, 56, 67, 68, 69, 69, 63, 54, 41, 48, 31, …
$ Race                 <chr> "White", "White", "White", "White", "White", "Hi…
$ PreinvasiveComponent <chr> "Absent", "Absent", "Absent", "Present", "Absent…
$ LVI                  <chr> "Present", "Absent", "Absent", "Present", "Prese…
$ PNI                  <chr> "Present", "Absent", "Absent", "Absent", "Absent…
$ Death                <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE…
$ Group                <chr> "Treatment", "Treatment", "Treatment", "Treatmen…
$ Grade                <chr> "2", "1", "3", "1", "3", "2", "2", "3", "2", "3"…
$ TStage               <chr> "4", "2", "1", "4", "4", "4", "1", "3", "4", "1"…
$ `Anti-X-intensity`   <dbl> 3, 2, 3, 2, 3, 3, 2, 2, 2, 3, 3, 3, 1, 2, 3, 3, …
$ `Anti-Y-intensity`   <dbl> 3, 3, 1, 3, 2, 2, NA, 1, 1, 1, 2, 1, 2, 3, 3, 2,…
$ LymphNodeMetastasis  <chr> "Present", "Present", "Present", "Present", "Pre…
$ Valid                <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, F…
$ Smoker               <lgl> TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALS…
$ Grade_Level          <chr> "moderate", "high", "low", "moderate", "high", "…
$ DeathTime            <chr> "Within1Year", "Within1Year", "Within1Year", "Wi…
```




```r
mydata %>% explore::describe()
```

```
               variable type na na_pct unique min  mean max
1                    ID  chr  0    0.0    250  NA    NA  NA
2                  Name  chr  1    0.4    250  NA    NA  NA
3                   Sex  chr  1    0.4      3  NA    NA  NA
4                   Age  dbl  1    0.4     50  25 49.05  73
5                  Race  chr  1    0.4      7  NA    NA  NA
6  PreinvasiveComponent  chr  1    0.4      3  NA    NA  NA
7                   LVI  chr  1    0.4      3  NA    NA  NA
8                   PNI  chr  1    0.4      3  NA    NA  NA
9      LastFollowUpDate  dat  1    0.4     13  NA    NA  NA
10                Death  lgl  1    0.4      3   0  0.67   1
11                Group  chr  1    0.4      3  NA    NA  NA
12                Grade  chr  1    0.4      4  NA    NA  NA
13               TStage  chr  1    0.4      5  NA    NA  NA
14     Anti-X-intensity  dbl  1    0.4      4   1  2.41   3
15     Anti-Y-intensity  dbl  1    0.4      4   1  1.97   3
16  LymphNodeMetastasis  chr  1    0.4      3  NA    NA  NA
17                Valid  lgl  1    0.4      3   0  0.44   1
18               Smoker  lgl  1    0.4      3   0  0.50   1
19          Grade_Level  chr  1    0.4      4  NA    NA  NA
20          SurgeryDate  dat  1    0.4    231  NA    NA  NA
21            DeathTime  chr  0    0.0      2  NA    NA  NA
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

![](/Users/serdarbalciold/histopathology-template/figs/missing values visdat-1.png)<!-- -->



```r
visdat::vis_miss(airquality, sort_miss = TRUE)
```

![](/Users/serdarbalciold/histopathology-template/figs/missing values visdat 2-1.png)<!-- -->





```r
xray::anomalies(mydata)
```

```
$variables
               Variable   q qNA  pNA qZero pZero qBlank pBlank qInf pInf
1                 Valid 250   1 0.4%   139 55.6%      0      -    0    -
2                Smoker 250   1 0.4%   125   50%      0      -    0    -
3                 Death 250   1 0.4%    81 32.4%      0      -    0    -
4                   Sex 250   1 0.4%     0     -      0      -    0    -
5  PreinvasiveComponent 250   1 0.4%     0     -      0      -    0    -
6                   LVI 250   1 0.4%     0     -      0      -    0    -
7                   PNI 250   1 0.4%     0     -      0      -    0    -
8                 Group 250   1 0.4%     0     -      0      -    0    -
9   LymphNodeMetastasis 250   1 0.4%     0     -      0      -    0    -
10                Grade 250   1 0.4%     0     -      0      -    0    -
11     Anti-X-intensity 250   1 0.4%     0     -      0      -    0    -
12     Anti-Y-intensity 250   1 0.4%     0     -      0      -    0    -
13          Grade_Level 250   1 0.4%     0     -      0      -    0    -
14               TStage 250   1 0.4%     0     -      0      -    0    -
15                 Race 250   1 0.4%     0     -      0      -    0    -
16     LastFollowUpDate 250   1 0.4%     0     -      0      -    0    -
17                  Age 250   1 0.4%     0     -      0      -    0    -
18          SurgeryDate 250   1 0.4%     0     -      0      -    0    -
19                 Name 250   1 0.4%     0     -      0      -    0    -
20            DeathTime 250   0    -     0     -      0      -    0    -
21                   ID 250   0    -     0     -      0      -    0    -
   qDistinct      type anomalous_percent
1          3   Logical               56%
2          3   Logical             50.4%
3          3   Logical             32.8%
4          3 Character              0.4%
5          3 Character              0.4%
6          3 Character              0.4%
7          3 Character              0.4%
8          3 Character              0.4%
9          3 Character              0.4%
10         4 Character              0.4%
11         4   Numeric              0.4%
12         4   Numeric              0.4%
13         4 Character              0.4%
14         5 Character              0.4%
15         7 Character              0.4%
16        13 Timestamp              0.4%
17        50   Numeric              0.4%
18       231 Timestamp              0.4%
19       250 Character              0.4%
20         2 Character                 -
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

![](/Users/serdarbalciold/histopathology-template/figs/xray 2-1.png)<!-- -->![](/Users/serdarbalciold/histopathology-template/figs/xray 2-2.png)<!-- -->![](/Users/serdarbalciold/histopathology-template/figs/xray 2-3.png)<!-- -->![](/Users/serdarbalciold/histopathology-template/figs/xray 2-4.png)<!-- -->

```
[1] "Ignoring variable LastFollowUpDate: Unsupported type for visualization."
```

![](/Users/serdarbalciold/histopathology-template/figs/xray 2-5.png)<!-- -->

```
[1] "Ignoring variable SurgeryDate: Unsupported type for visualization."
```

![](/Users/serdarbalciold/histopathology-template/figs/xray 2-6.png)<!-- -->

```
          Variable p_1 p_10 p_25 p_50 p_75 p_90 p_99
1 Anti-X-intensity   1    2    2    2    3    3    3
2 Anti-Y-intensity   1    1    1    2    3    3    3
3              Age  26   31   37   48   61   68   73
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

![](/Users/serdarbalciold/histopathology-template/figs/DataExplorer 4-1.png)<!-- -->



```r
DataExplorer::plot_missing(mydata)
```

![](/Users/serdarbalciold/histopathology-template/figs/DataExplorer 5-1.png)<!-- -->

**Drop columns**


```r
mydata2 <- DataExplorer::drop_columns(mydata, "TStage")
```



```r
DataExplorer::plot_bar(mydata)
```

![](/Users/serdarbalciold/histopathology-template/figs/DataExplorer 7-1.png)<!-- -->![](/Users/serdarbalciold/histopathology-template/figs/DataExplorer 7-2.png)<!-- -->




```r
DataExplorer::plot_bar(mydata, with = "Death")
```

![](/Users/serdarbalciold/histopathology-template/figs/DataExplorer 8-1.png)<!-- -->![](/Users/serdarbalciold/histopathology-template/figs/DataExplorer 8-2.png)<!-- -->



```r
DataExplorer::plot_histogram(mydata)
```

![](/Users/serdarbalciold/histopathology-template/figs/DataExplorer 9-1.png)<!-- -->




---



<!-- extracodes are below -->

<!-- ### dataMaid -->




















































































---

# Statistical Analysis

**Learn these tests as highlighted in [@Schmidt2017].**^[Statistical Literacy Among Academic Pathologists: A Survey Study to Gauge Knowledge of Frequently Used Statistical Tests Among Trainees and Faculty. Archives of Pathology & Laboratory Medicine: February 2017, Vol. 141, No. 2, pp. 279-287. https://doi.org/10.5858/arpa.2016-0200-OA]


---

# Results

**Write results as described in [@Knijn2015]**^[From Table 1: Proposed items for reporting histopathology studies. Recommendations for reporting histopathology studies: a proposal Virchows Arch (2015) 466:611–615 DOI 10.1007/s00428-015-1762-3]

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
  - Name: 249 entries: Adayah, n = 1; Adeja, n = 1; Adelaina, n = 1 and 246 others (1 missing)
  - Sex: 2 entries: Male, n = 135; Female, n = 114 (1 missing)
  - Age: Mean = 49.05, SD = 13.68, range = [25, 73], 1 missing
  - Race: 6 entries: White, n = 158; Hispanic, n = 46; Black, n = 33 and 3 others (1 missing)
  - PreinvasiveComponent: 2 entries: Absent, n = 192; Present, n = 57 (1 missing)
  - LVI: 2 entries: Absent, n = 163; Present, n = 86 (1 missing)
  - PNI: 2 entries: Absent, n = 174; Present, n = 75 (1 missing)
  - Death: 2 levels: FALSE (n = 81); TRUE (n = 168) and missing (n = 1)
  - Group: 2 entries: Treatment, n = 128; Control, n = 121 (1 missing)
  - Grade: 3 entries: 3, n = 101; 1, n = 80; 2, n = 68 (1 missing)
  - TStage: 4 entries: 4, n = 102; 3, n = 73; 2, n = 52 and 1 other (1 missing)
  - Anti-X-intensity: Mean = 2.41, SD = 0.62, range = [1, 3], 1 missing
  - Anti-Y-intensity: Mean = 1.97, SD = 0.77, range = [1, 3], 1 missing
  - LymphNodeMetastasis: 2 entries: Absent, n = 143; Present, n = 106 (1 missing)
  - Valid: 2 levels: FALSE (n = 139); TRUE (n = 110) and missing (n = 1)
  - Smoker: 2 levels: FALSE (n = 125); TRUE (n = 124) and missing (n = 1)
  - Grade_Level: 3 entries: high, n = 96; moderate, n = 79; low, n = 74 (1 missing)
  - DeathTime: 2 entries: Within1Year, n = 149; MoreThan1Year, n = 101
```


**Table 1 via arsenal 📦**


```r
# cat(names(mydata), sep = ' + \n')
library(arsenal)
tab1 <- arsenal::tableby(~Sex + Age + Race + PreinvasiveComponent + LVI + PNI + Death + 
    Group + Grade + TStage + `Anti-X-intensity` + `Anti-Y-intensity` + LymphNodeMetastasis + 
    Valid + Smoker + Grade_Level, data = mydata)
summary(tab1)
```



|                            | Overall (N=250) |
|:---------------------------|:---------------:|
|**Sex**                     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Female    |   114 (45.8%)   |
|&nbsp;&nbsp;&nbsp;Male      |   135 (54.2%)   |
|**Age**                     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Mean (SD) | 49.048 (13.681) |
|&nbsp;&nbsp;&nbsp;Range     | 25.000 - 73.000 |
|**Race**                    |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Asian     |    7 (2.8%)     |
|&nbsp;&nbsp;&nbsp;Bi-Racial |    4 (1.6%)     |
|&nbsp;&nbsp;&nbsp;Black     |   33 (13.3%)    |
|&nbsp;&nbsp;&nbsp;Hispanic  |   46 (18.5%)    |
|&nbsp;&nbsp;&nbsp;Other     |    1 (0.4%)     |
|&nbsp;&nbsp;&nbsp;White     |   158 (63.5%)   |
|**PreinvasiveComponent**    |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   192 (77.1%)   |
|&nbsp;&nbsp;&nbsp;Present   |   57 (22.9%)    |
|**LVI**                     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   163 (65.5%)   |
|&nbsp;&nbsp;&nbsp;Present   |   86 (34.5%)    |
|**PNI**                     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   174 (69.9%)   |
|&nbsp;&nbsp;&nbsp;Present   |   75 (30.1%)    |
|**Death**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;FALSE     |   81 (32.5%)    |
|&nbsp;&nbsp;&nbsp;TRUE      |   168 (67.5%)   |
|**Group**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Control   |   121 (48.6%)   |
|&nbsp;&nbsp;&nbsp;Treatment |   128 (51.4%)   |
|**Grade**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;1         |   80 (32.1%)    |
|&nbsp;&nbsp;&nbsp;2         |   68 (27.3%)    |
|&nbsp;&nbsp;&nbsp;3         |   101 (40.6%)   |
|**TStage**                  |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;1         |    22 (8.8%)    |
|&nbsp;&nbsp;&nbsp;2         |   52 (20.9%)    |
|&nbsp;&nbsp;&nbsp;3         |   73 (29.3%)    |
|&nbsp;&nbsp;&nbsp;4         |   102 (41.0%)   |
|**Anti-X-intensity**        |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Mean (SD) |  2.406 (0.622)  |
|&nbsp;&nbsp;&nbsp;Range     |  1.000 - 3.000  |
|**Anti-Y-intensity**        |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Mean (SD) |  1.968 (0.772)  |
|&nbsp;&nbsp;&nbsp;Range     |  1.000 - 3.000  |
|**LymphNodeMetastasis**     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   143 (57.4%)   |
|&nbsp;&nbsp;&nbsp;Present   |   106 (42.6%)   |
|**Valid**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;FALSE     |   139 (55.8%)   |
|&nbsp;&nbsp;&nbsp;TRUE      |   110 (44.2%)   |
|**Smoker**                  |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;FALSE     |   125 (50.2%)   |
|&nbsp;&nbsp;&nbsp;TRUE      |   124 (49.8%)   |
|**Grade_Level**             |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;high      |   96 (38.6%)    |
|&nbsp;&nbsp;&nbsp;low       |   74 (29.7%)    |
|&nbsp;&nbsp;&nbsp;moderate  |   79 (31.7%)    |



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
  Sex = Male (%)                       135 (54.2) 
  Age (mean (SD))                    49.05 (13.68)
  Race (%)                                        
     Asian                               7 ( 2.8) 
     Bi-Racial                           4 ( 1.6) 
     Black                              33 (13.3) 
     Hispanic                           46 (18.5) 
     Other                               1 ( 0.4) 
     White                             158 (63.5) 
  PreinvasiveComponent = Present (%)    57 (22.9) 
  LVI = Present (%)                     86 (34.5) 
  PNI = Present (%)                     75 (30.1) 
  Death = TRUE (%)                     168 (67.5) 
  Group = Treatment (%)                128 (51.4) 
  Grade (%)                                       
     1                                  80 (32.1) 
     2                                  68 (27.3) 
     3                                 101 (40.6) 
  TStage (%)                                      
     1                                  22 ( 8.8) 
     2                                  52 (20.9) 
     3                                  73 (29.3) 
     4                                 102 (41.0) 
  Anti-X-intensity (mean (SD))        2.41 (0.62) 
  Anti-Y-intensity (mean (SD))        1.97 (0.77) 
  LymphNodeMetastasis = Present (%)    106 (42.6) 
  Valid = TRUE (%)                     110 (44.2) 
  Smoker = TRUE (%)                    124 (49.8) 
  Grade_Level (%)                                 
     high                               96 (38.6) 
     low                                74 (29.7) 
     moderate                           79 (31.7) 
  DeathTime = Within1Year (%)          149 (59.6) 
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
               variable type na na_pct unique min mean max
1                   Sex  chr  1    0.4      3  NA   NA  NA
2  PreinvasiveComponent  chr  1    0.4      3  NA   NA  NA
3                   LVI  chr  1    0.4      3  NA   NA  NA
4                   PNI  chr  1    0.4      3  NA   NA  NA
5                 Death  lgl  1    0.4      3   0 0.67   1
6                 Group  chr  1    0.4      3  NA   NA  NA
7                 Grade  chr  1    0.4      4  NA   NA  NA
8      Anti-X-intensity  dbl  1    0.4      4   1 2.41   3
9      Anti-Y-intensity  dbl  1    0.4      4   1 1.97   3
10  LymphNodeMetastasis  chr  1    0.4      3  NA   NA  NA
11                Valid  lgl  1    0.4      3   0 0.44   1
12               Smoker  lgl  1    0.4      3   0 0.50   1
13          Grade_Level  chr  1    0.4      4  NA   NA  NA
14            DeathTime  chr  0    0.0      2  NA   NA  NA
```



```r
mydata %>% explore::describe() %>% dplyr::filter(na > 0)
```

```
               variable type na na_pct unique min  mean max
1                  Name  chr  1    0.4    250  NA    NA  NA
2                   Sex  chr  1    0.4      3  NA    NA  NA
3                   Age  dbl  1    0.4     50  25 49.05  73
4                  Race  chr  1    0.4      7  NA    NA  NA
5  PreinvasiveComponent  chr  1    0.4      3  NA    NA  NA
6                   LVI  chr  1    0.4      3  NA    NA  NA
7                   PNI  chr  1    0.4      3  NA    NA  NA
8      LastFollowUpDate  dat  1    0.4     13  NA    NA  NA
9                 Death  lgl  1    0.4      3   0  0.67   1
10                Group  chr  1    0.4      3  NA    NA  NA
11                Grade  chr  1    0.4      4  NA    NA  NA
12               TStage  chr  1    0.4      5  NA    NA  NA
13     Anti-X-intensity  dbl  1    0.4      4   1  2.41   3
14     Anti-Y-intensity  dbl  1    0.4      4   1  1.97   3
15  LymphNodeMetastasis  chr  1    0.4      3  NA    NA  NA
16                Valid  lgl  1    0.4      3   0  0.44   1
17               Smoker  lgl  1    0.4      3   0  0.50   1
18          Grade_Level  chr  1    0.4      4  NA    NA  NA
19          SurgeryDate  dat  1    0.4    231  NA    NA  NA
```






```r
mydata %>% explore::describe()
```

```
               variable type na na_pct unique min  mean max
1                    ID  chr  0    0.0    250  NA    NA  NA
2                  Name  chr  1    0.4    250  NA    NA  NA
3                   Sex  chr  1    0.4      3  NA    NA  NA
4                   Age  dbl  1    0.4     50  25 49.05  73
5                  Race  chr  1    0.4      7  NA    NA  NA
6  PreinvasiveComponent  chr  1    0.4      3  NA    NA  NA
7                   LVI  chr  1    0.4      3  NA    NA  NA
8                   PNI  chr  1    0.4      3  NA    NA  NA
9      LastFollowUpDate  dat  1    0.4     13  NA    NA  NA
10                Death  lgl  1    0.4      3   0  0.67   1
11                Group  chr  1    0.4      3  NA    NA  NA
12                Grade  chr  1    0.4      4  NA    NA  NA
13               TStage  chr  1    0.4      5  NA    NA  NA
14     Anti-X-intensity  dbl  1    0.4      4   1  2.41   3
15     Anti-Y-intensity  dbl  1    0.4      4   1  1.97   3
16  LymphNodeMetastasis  chr  1    0.4      3  NA    NA  NA
17                Valid  lgl  1    0.4      3   0  0.44   1
18               Smoker  lgl  1    0.4      3   0  0.50   1
19          Grade_Level  chr  1    0.4      4  NA    NA  NA
20          SurgeryDate  dat  1    0.4    231  NA    NA  NA
21            DeathTime  chr  0    0.0      2  NA    NA  NA
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
Female    114  45.6%     45.8%         
Male      135  54.0%     54.2%         
NA          1  0.4%      -             

\pagebreak

#### Descriptive Statistics Race  


```r
mydata %>% janitor::tabyl(Race) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Race           n  percent   valid_percent 
----------  ----  --------  --------------
Asian          7  2.8%      2.8%          
Bi-Racial      4  1.6%      1.6%          
Black         33  13.2%     13.3%         
Hispanic      46  18.4%     18.5%         
Other          1  0.4%      0.4%          
White        158  63.2%     63.5%         
NA             1  0.4%      -             

\pagebreak

#### Descriptive Statistics PreinvasiveComponent  


```r
mydata %>% janitor::tabyl(PreinvasiveComponent) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



PreinvasiveComponent      n  percent   valid_percent 
---------------------  ----  --------  --------------
Absent                  192  76.8%     77.1%         
Present                  57  22.8%     22.9%         
NA                        1  0.4%      -             

\pagebreak

#### Descriptive Statistics LVI  


```r
mydata %>% janitor::tabyl(LVI) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



LVI          n  percent   valid_percent 
--------  ----  --------  --------------
Absent     163  65.2%     65.5%         
Present     86  34.4%     34.5%         
NA           1  0.4%      -             

\pagebreak

#### Descriptive Statistics PNI  


```r
mydata %>% janitor::tabyl(PNI) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



PNI          n  percent   valid_percent 
--------  ----  --------  --------------
Absent     174  69.6%     69.9%         
Present     75  30.0%     30.1%         
NA           1  0.4%      -             

\pagebreak

#### Descriptive Statistics Group  


```r
mydata %>% janitor::tabyl(Group) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Group          n  percent   valid_percent 
----------  ----  --------  --------------
Control      121  48.4%     48.6%         
Treatment    128  51.2%     51.4%         
NA             1  0.4%      -             

\pagebreak

#### Descriptive Statistics Grade  


```r
mydata %>% janitor::tabyl(Grade) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Grade      n  percent   valid_percent 
------  ----  --------  --------------
1         80  32.0%     32.1%         
2         68  27.2%     27.3%         
3        101  40.4%     40.6%         
NA         1  0.4%      -             

\pagebreak

#### Descriptive Statistics TStage  


```r
mydata %>% janitor::tabyl(TStage) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



TStage      n  percent   valid_percent 
-------  ----  --------  --------------
1          22  8.8%      8.8%          
2          52  20.8%     20.9%         
3          73  29.2%     29.3%         
4         102  40.8%     41.0%         
NA          1  0.4%      -             

\pagebreak

#### Descriptive Statistics LymphNodeMetastasis  


```r
mydata %>% janitor::tabyl(LymphNodeMetastasis) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



LymphNodeMetastasis      n  percent   valid_percent 
--------------------  ----  --------  --------------
Absent                 143  57.2%     57.4%         
Present                106  42.4%     42.6%         
NA                       1  0.4%      -             

\pagebreak

#### Descriptive Statistics Grade_Level  


```r
mydata %>% janitor::tabyl(Grade_Level) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Grade_Level     n  percent   valid_percent 
------------  ---  --------  --------------
high           96  38.4%     38.6%         
low            74  29.6%     29.7%         
moderate       79  31.6%     31.7%         
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
 Absent  = 192 (76.8%)
 Present = 57 (22.8%)
 NA      = 1 (0.4%)
```




```r
## Frequency or custom tables for categorical variables
SmartEDA::ExpCTable(mydata, Target = NULL, margin = 1, clim = 10, nlim = 5, round = 2, 
    bin = NULL, per = T)
```

```
               Variable         Valid Frequency Percent CumPercent
1                   Sex        Female       114    45.6       45.6
2                   Sex          Male       135    54.0       99.6
3                   Sex            NA         1     0.4      100.0
4                   Sex         TOTAL       250      NA         NA
5                  Race         Asian         7     2.8        2.8
6                  Race     Bi-Racial         4     1.6        4.4
7                  Race         Black        33    13.2       17.6
8                  Race      Hispanic        46    18.4       36.0
9                  Race            NA         1     0.4       36.4
10                 Race         Other         1     0.4       36.8
11                 Race         White       158    63.2      100.0
12                 Race         TOTAL       250      NA         NA
13 PreinvasiveComponent        Absent       192    76.8       76.8
14 PreinvasiveComponent            NA         1     0.4       77.2
15 PreinvasiveComponent       Present        57    22.8      100.0
16 PreinvasiveComponent         TOTAL       250      NA         NA
17                  LVI        Absent       163    65.2       65.2
18                  LVI            NA         1     0.4       65.6
19                  LVI       Present        86    34.4      100.0
20                  LVI         TOTAL       250      NA         NA
21                  PNI        Absent       174    69.6       69.6
22                  PNI            NA         1     0.4       70.0
23                  PNI       Present        75    30.0      100.0
24                  PNI         TOTAL       250      NA         NA
25                Group       Control       121    48.4       48.4
26                Group            NA         1     0.4       48.8
27                Group     Treatment       128    51.2      100.0
28                Group         TOTAL       250      NA         NA
29                Grade             1        80    32.0       32.0
30                Grade             2        68    27.2       59.2
31                Grade             3       101    40.4       99.6
32                Grade            NA         1     0.4      100.0
33                Grade         TOTAL       250      NA         NA
34               TStage             1        22     8.8        8.8
35               TStage             2        52    20.8       29.6
36               TStage             3        73    29.2       58.8
37               TStage             4       102    40.8       99.6
38               TStage            NA         1     0.4      100.0
39               TStage         TOTAL       250      NA         NA
40  LymphNodeMetastasis        Absent       143    57.2       57.2
41  LymphNodeMetastasis            NA         1     0.4       57.6
42  LymphNodeMetastasis       Present       106    42.4      100.0
43  LymphNodeMetastasis         TOTAL       250      NA         NA
44          Grade_Level          high        96    38.4       38.4
45          Grade_Level           low        74    29.6       68.0
46          Grade_Level      moderate        79    31.6       99.6
47          Grade_Level            NA         1     0.4      100.0
48          Grade_Level         TOTAL       250      NA         NA
49            DeathTime MoreThan1Year       101    40.4       40.4
50            DeathTime   Within1Year       149    59.6      100.0
51            DeathTime         TOTAL       250      NA         NA
52     Anti-X-intensity             1        18     7.2        7.2
53     Anti-X-intensity             2       112    44.8       52.0
54     Anti-X-intensity             3       119    47.6       99.6
55     Anti-X-intensity            NA         1     0.4      100.0
56     Anti-X-intensity         TOTAL       250      NA         NA
57     Anti-Y-intensity             1        78    31.2       31.2
58     Anti-Y-intensity             2       101    40.4       71.6
59     Anti-Y-intensity             3        70    28.0       99.6
60     Anti-Y-intensity            NA         1     0.4      100.0
61     Anti-Y-intensity         TOTAL       250      NA         NA
```




```r
inspectdf::inspect_cat(mydata)
```

```
# A tibble: 16 x 5
   col_name               cnt common      common_pcnt levels            
   <chr>                <int> <chr>             <dbl> <named list>      
 1 Death                    3 TRUE               67.2 <tibble [3 × 3]>  
 2 DeathTime                2 Within1Year        59.6 <tibble [2 × 3]>  
 3 Grade                    4 3                  40.4 <tibble [4 × 3]>  
 4 Grade_Level              4 high               38.4 <tibble [4 × 3]>  
 5 Group                    3 Treatment          51.2 <tibble [3 × 3]>  
 6 ID                     250 001                 0.4 <tibble [250 × 3]>
 7 LVI                      3 Absent             65.2 <tibble [3 × 3]>  
 8 LymphNodeMetastasis      3 Absent             57.2 <tibble [3 × 3]>  
 9 Name                   250 Adayah              0.4 <tibble [250 × 3]>
10 PNI                      3 Absent             69.6 <tibble [3 × 3]>  
11 PreinvasiveComponent     3 Absent             76.8 <tibble [3 × 3]>  
12 Race                     7 White              63.2 <tibble [7 × 3]>  
13 Sex                      3 Male               54   <tibble [3 × 3]>  
14 Smoker                   3 FALSE              50   <tibble [3 × 3]>  
15 TStage                   5 4                  40.8 <tibble [5 × 3]>  
16 Valid                    3 FALSE              55.6 <tibble [3 × 3]>  
```

```r
inspectdf::inspect_cat(mydata)$levels$Group
```

```
# A tibble: 3 x 3
  value      prop   cnt
  <chr>     <dbl> <int>
1 Treatment 0.512   128
2 Control   0.484   121
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
SmartEDA::ExpCTable(mydata, Target = "Sex", margin = 1, clim = 10, nlim = NULL, round = 2, 
    bin = 4, per = F)
```

```
               VARIABLE      CATEGORY Sex:Female Sex:Male Sex:NA TOTAL
1                  Race         Asian          3        3      1     7
2                  Race     Bi-Racial          2        2      0     4
3                  Race         Black          9       24      0    33
4                  Race      Hispanic         24       22      0    46
5                  Race            NA          0        1      0     1
6                  Race         Other          0        1      0     1
7                  Race         White         76       82      0   158
8                  Race         TOTAL        114      135      1   250
9  PreinvasiveComponent        Absent         86      106      0   192
10 PreinvasiveComponent            NA          0        0      1     1
11 PreinvasiveComponent       Present         28       29      0    57
12 PreinvasiveComponent         TOTAL        114      135      1   250
13                  LVI        Absent         75       87      1   163
14                  LVI            NA          0        1      0     1
15                  LVI       Present         39       47      0    86
16                  LVI         TOTAL        114      135      1   250
17                  PNI        Absent         77       97      0   174
18                  PNI            NA          1        0      0     1
19                  PNI       Present         36       38      1    75
20                  PNI         TOTAL        114      135      1   250
21                Group       Control         60       61      0   121
22                Group            NA          1        0      0     1
23                Group     Treatment         53       74      1   128
24                Group         TOTAL        114      135      1   250
25                Grade             1         38       42      0    80
26                Grade             2         37       30      1    68
27                Grade             3         38       63      0   101
28                Grade            NA          1        0      0     1
29                Grade         TOTAL        114      135      1   250
30               TStage             1         12       10      0    22
31               TStage             2         23       29      0    52
32               TStage             3         34       39      0    73
33               TStage             4         44       57      1   102
34               TStage            NA          1        0      0     1
35               TStage         TOTAL        114      135      1   250
36  LymphNodeMetastasis        Absent         68       74      1   143
37  LymphNodeMetastasis            NA          1        0      0     1
38  LymphNodeMetastasis       Present         45       61      0   106
39  LymphNodeMetastasis         TOTAL        114      135      1   250
40          Grade_Level          high         36       60      0    96
41          Grade_Level           low         39       35      0    74
42          Grade_Level      moderate         39       39      1    79
43          Grade_Level            NA          0        1      0     1
44          Grade_Level         TOTAL        114      135      1   250
45            DeathTime MoreThan1Year         51       50      0   101
46            DeathTime   Within1Year         63       85      1   149
47            DeathTime         TOTAL        114      135      1   250
48     Anti-X-intensity             1          8       10      0    18
49     Anti-X-intensity             2         50       62      0   112
50     Anti-X-intensity             3         56       62      1   119
51     Anti-X-intensity            NA          0        1      0     1
52     Anti-X-intensity         TOTAL        114      135      1   250
53     Anti-Y-intensity             1         38       39      1    78
54     Anti-Y-intensity             2         51       50      0   101
55     Anti-Y-intensity             3         25       45      0    70
56     Anti-Y-intensity            NA          0        1      0     1
57     Anti-Y-intensity         TOTAL        114      135      1   250
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
 ───────────────────────────────── 
                          Age      
 ───────────────────────────────── 
   N                         249   
   Missing                     1   
   Mean                     49.0   
   Median                   48.0   
   Mode                     43.0   
   Standard deviation       13.7   
   Variance                  187   
   Minimum                  25.0   
   Maximum                  73.0   
   Skewness               0.0330   
   Std. error skewness     0.154   
   Kurtosis                -1.20   
   Std. error kurtosis     0.307   
   25th percentile          37.0   
   50th percentile          48.0   
   75th percentile          61.0   
 ───────────────────────────────── 
```

![](/Users/serdarbalciold/histopathology-template/figs/Descriptive Statistics Age-1.png)<!-- -->![](/Users/serdarbalciold/histopathology-template/figs/Descriptive Statistics Age-2.png)<!-- -->

\pagebreak

**Descriptive Statistics Anti-X-intensity** 


```r
mydata %>% jmv::descriptives(data = ., vars = "Anti-X-intensity", hist = TRUE, dens = TRUE, 
    box = TRUE, violin = TRUE, dot = TRUE, mode = TRUE, sd = TRUE, variance = TRUE, 
    skew = TRUE, kurt = TRUE, quart = TRUE)
```

```

 DESCRIPTIVES

 Descriptives                                
 ─────────────────────────────────────────── 
                          Anti-X-intensity   
 ─────────────────────────────────────────── 
   N                                   249   
   Missing                               1   
   Mean                               2.41   
   Median                             2.00   
   Mode                               3.00   
   Standard deviation                0.622   
   Variance                          0.387   
   Minimum                            1.00   
   Maximum                            3.00   
   Skewness                         -0.548   
   Std. error skewness               0.154   
   Kurtosis                         -0.608   
   Std. error kurtosis               0.307   
   25th percentile                    2.00   
   50th percentile                    2.00   
   75th percentile                    3.00   
 ─────────────────────────────────────────── 
```

![](/Users/serdarbalciold/histopathology-template/figs/Descriptive Statistics Anti-X-intensity-1.png)<!-- -->![](/Users/serdarbalciold/histopathology-template/figs/Descriptive Statistics Anti-X-intensity-2.png)<!-- -->

\pagebreak

**Descriptive Statistics Anti-Y-intensity** 


```r
mydata %>% jmv::descriptives(data = ., vars = "Anti-Y-intensity", hist = TRUE, dens = TRUE, 
    box = TRUE, violin = TRUE, dot = TRUE, mode = TRUE, sd = TRUE, variance = TRUE, 
    skew = TRUE, kurt = TRUE, quart = TRUE)
```

```

 DESCRIPTIVES

 Descriptives                                
 ─────────────────────────────────────────── 
                          Anti-Y-intensity   
 ─────────────────────────────────────────── 
   N                                   249   
   Missing                               1   
   Mean                               1.97   
   Median                             2.00   
   Mode                               2.00   
   Standard deviation                0.772   
   Variance                          0.596   
   Minimum                            1.00   
   Maximum                            3.00   
   Skewness                         0.0552   
   Std. error skewness               0.154   
   Kurtosis                          -1.32   
   Std. error kurtosis               0.307   
   25th percentile                    1.00   
   50th percentile                    2.00   
   75th percentile                    3.00   
 ─────────────────────────────────────────── 
```

![](/Users/serdarbalciold/histopathology-template/figs/Descriptive Statistics Anti-Y-intensity-1.png)<!-- -->![](/Users/serdarbalciold/histopathology-template/figs/Descriptive Statistics Anti-Y-intensity-2.png)<!-- -->

\pagebreak





```r
tab <- tableone::CreateTableOne(data = mydata)
# ?print.ContTable
tab$ContTable
```

```
                              
                               Overall      
  n                            250          
  Age (mean (SD))              49.05 (13.68)
  Anti-X-intensity (mean (SD))  2.41 (0.62) 
  Anti-Y-intensity (mean (SD))  1.97 (0.77) 
```

```r
print(tab$ContTable, nonnormal = c("Anti-X-intensity"))
```

```
                                 
                                  Overall           
  n                               250               
  Age (mean (SD))                 49.05 (13.68)     
  Anti-X-intensity (median [IQR])  2.00 [2.00, 3.00]
  Anti-Y-intensity (mean (SD))     1.97 (0.77)      
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
q05|q95  = 28 | 70
q25|q75  = 37 | 61
median   = 48
mean     = 49.04819
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
1 Age              25    37     48 49.0     61    73 13.7       0.4 <tibble [12…
2 Anti-X-inten…     1     2      2  2.41     3     3  0.622     0.4 <tibble [12…
3 Anti-Y-inten…     1     1      2  1.97     3     3  0.772     0.4 <tibble [12…
```



```r
inspectdf::inspect_num(mydata)$hist$Age
```

```
# A tibble: 27 x 2
   value         prop
   <chr>        <dbl>
 1 [-Inf, 24) 0      
 2 [24, 26)   0.00803
 3 [26, 28)   0.0361 
 4 [28, 30)   0.0281 
 5 [30, 32)   0.0402 
 6 [32, 34)   0.0482 
 7 [34, 36)   0.0562 
 8 [36, 38)   0.0361 
 9 [38, 40)   0.0402 
10 [40, 42)   0.0482 
# … with 17 more rows
```



```r
inspectdf::inspect_num(mydata, breaks = 10) %>% inspectdf::show_plot()
```

![](/Users/serdarbalciold/histopathology-template/figs/inspectdf 5-1.png)<!-- -->





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
2   Age  PreinvasiveComponent:Absent 192    0     0  191      0      0        1
3   Age PreinvasiveComponent:Present  57    0     0   57      0      0        0
4   Age      PreinvasiveComponent:NA   0    0     0    0      0      0        0
  Per_of_Missing   sum min  max  mean median    SD   CV  IQR Skewness Kurtosis
1           0.40 12213  25   73 49.05     48 13.68 0.28 24.0     0.03    -1.20
2           0.52  9143  25   73 47.87     47 13.43 0.28 22.5     0.12    -1.12
3           0.00  3033  27   73 53.21     58 13.86 0.26 24.0    -0.33    -1.24
4            NaN     0 Inf -Inf   NaN     NA    NA   NA   NA      NaN      NaN
  0%  10% 20% 30% 40% 50% 60% 70% 80%  90% 100% LB.25% UB.75% nOutliers
1 25 31.0  35  40  43  48  54  59  64 68.0   73   1.00  97.00         0
2 25 30.0  34  39  43  47  52  57  62 67.0   73   3.75  93.75         0
3 27 32.6  37  45  49  58  60  63  66 69.4   73   5.00 101.00         0
4 NA   NA  NA  NA  NA  NA  NA  NA  NA   NA   NA     NA     NA         0
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
  FALSE  81   0
  TRUE    0 168
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
 [1]  3.4   7.7   6.3   6.6   9.4   9.1   6.5+ 10.5+  8.2   8.7  11.6  11.3 
[13]  4.8   6.7  11.6+  6.2+  7.2+  6.4   3.3   9.7  11.4+  8.0+  9.4  10.9+
[25]  5.1+  8.9   3.5   9.0   3.8+  5.8  10.2+  3.7   8.3+  7.3   3.1   5.9 
[37]  8.0+  7.4   9.5+  3.7+ 10.2   3.9   9.1+  6.6   7.3+  5.5   5.4   3.8 
[49]  8.8+  8.8   3.2   7.0  10.7   6.6   8.6   5.5  11.5   9.7    NA  10.5 
[61]  7.5   9.0   7.8  10.6   5.0   6.3   4.3   4.4+  7.5   6.3   8.5+  9.0+
[73]  7.1   6.0   5.8+  3.0   8.2   8.6   5.7   7.2+
```

```r
plot(km)
```

![](/Users/serdarbalciold/histopathology-template/figs/km-1.png)<!-- -->



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

![](/Users/serdarbalciold/histopathology-template/figs/Kaplan-Meier Plot Log-Rank Test-1.png)<!-- -->




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

![](/Users/serdarbalciold/histopathology-template/figs/Kaplan-Meier Plot Log-Rank Test 2-1.png)<!-- -->




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
LVI                                     Absent     163 (100.0)                          NA                          NA
                                        Present     86 (100.0)   1.09 (0.77-1.54, p=0.613)   1.09 (0.77-1.54, p=0.613)


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

When LVI is Present, there is 1.09 (0.77-1.54, p=0.613) times risk than when LVI is Absent.

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

When LVI is Present, there is 1.09 (0.77-1.54, p=0.613) times risk than when LVI is Absent.

}
}



### Kaplan-Meier Median Survival


```r
km_fit <- survfit(Surv(OverallTime, Outcome) ~ LVI, data = mydata)
km_fit
```

```
Call: survfit(formula = Surv(OverallTime, Outcome) ~ LVI, data = mydata)

   4 observations deleted due to missingness 
              n events median 0.95LCL 0.95UCL
LVI=Absent  161    119   17.6    13.6    23.5
LVI=Present  85     46   10.6     9.4    26.0
```

```r
plot(km_fit)
```

![](/Users/serdarbalciold/histopathology-template/figs/Median Survivals-1.png)<!-- -->

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

When LVI=Absent, median survival is 17.6 [13.6 - 23.5, 95% CI] months., When LVI=Present, median survival is 10.6 [9.4 - 26, 95% CI] months.

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

When LVI=Absent, median survival is 17.6 [13.6 - 23.5, 95% CI] months., When LVI=Present, median survival is 10.6 [9.4 - 26, 95% CI] months.

}
}




### 1-3-5-yr survival
      

```r
summary(km_fit, times = c(12, 36, 60))
```

```
Call: survfit(formula = Surv(OverallTime, Outcome) ~ LVI, data = mydata)

4 observations deleted due to missingness 
                LVI=Absent 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
   12     81      61    0.587  0.0409        0.513        0.673
   36     18      45    0.211  0.0378        0.148        0.300

                LVI=Present 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
   12     15      38    0.409  0.0671        0.297        0.564
   36      6       4    0.273  0.0721        0.163        0.458
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

When LVI=Absent, 12 month survival is 58.7% [51.3%-67%, 95% CI]., When LVI=Absent, 36 month survival is 21.1% [14.8%-30%, 95% CI]., When LVI=Present, 12 month survival is 40.9% [29.7%-56%, 95% CI]., When LVI=Present, 36 month survival is 27.3% [16.3%-46%, 95% CI].

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

When LVI=Absent, 12 month survival is 58.7% [51.3%-67%, 95% CI]., When LVI=Absent, 36 month survival is 21.1% [14.8%-30%, 95% CI]., When LVI=Present, 12 month survival is 40.9% [29.7%-56%, 95% CI]., When LVI=Present, 36 month survival is 27.3% [16.3%-46%, 95% CI].

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

![](/Users/serdarbalciold/histopathology-template/figs/Kaplan-Meier Plot Log-Rank Test TStage-1.png)<!-- -->








### Multivariate Analysis Survival








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
saved data after analysis to /Users/serdarbalciold/histopathology-template/data/histopathology-template2019-11-28.xlsx : 2019-11-28 19:15:48
```






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
Ewen Harrison, Tom Drake and Riinu Ots (2019). finalfit: Quickly Create Elegant Regression Results Tables and Plots when Modelling. R package version 0.9.6. https://github.com/ewenharrison/finalfit                                             
Hadley Wickham and Jennifer Bryan (2019). readxl: Read Excel Files. R package version 1.3.1. https://CRAN.R-project.org/package=readxl                                                                                                            
Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2019). dplyr: A Grammar of Data Manipulation. R package version 0.8.3. https://CRAN.R-project.org/package=dplyr                                                                  
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

  Sam Firke (2019). janitor: Simple Tools for Examining and Cleaning
  Dirty Data. R package version 1.2.0.
  https://CRAN.R-project.org/package=janitor

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {janitor: Simple Tools for Examining and Cleaning Dirty Data},
    author = {Sam Firke},
    year = {2019},
    note = {R package version 1.2.0},
    url = {https://CRAN.R-project.org/package=janitor},
  }
```

```r
citation("report")
```

```

To cite in publications use:

  Makowski, D. & Lüdecke, D. (2019). The report package for R: Ensuring
  the use of best practices for results reporting. CRAN. Available from
  https://github.com/easystats/report. doi: .

A BibTeX entry for LaTeX users is

  @Article{,
    title = {The report package for R: Ensuring the use of best practices for results reporting},
    author = {{Makowski} and {Dominique} and {Lüdecke} and {Daniel}},
    journal = {CRAN},
    year = {2019},
    note = {R package},
    url = {https://github.com/easystats/report},
  }
```

```r
citation("finalfit")
```

```

To cite package 'finalfit' in publications use:

  Ewen Harrison, Tom Drake and Riinu Ots (2019). finalfit: Quickly
  Create Elegant Regression Results Tables and Plots when Modelling. R
  package version 0.9.6. https://github.com/ewenharrison/finalfit

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {finalfit: Quickly Create Elegant Regression Results Tables and Plots when
Modelling},
    author = {Ewen Harrison and Tom Drake and Riinu Ots},
    year = {2019},
    note = {R package version 0.9.6},
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
Running under: macOS  10.15.1

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods   base     

other attached packages:
 [1] finalfit_0.9.6  survival_3.1-6  tableone_0.10.0 arsenal_3.3.0  
 [5] readxl_1.3.1    magrittr_1.5    dplyr_0.8.3     report_0.1.0   
 [9] here_0.1        pak_0.1.2       pacman_0.5.1    remotes_2.1.0  

loaded via a namespace (and not attached):
  [1] backports_1.1.5        Hmisc_4.3-0            plyr_1.8.4            
  [4] dataMaid_1.3.2         igraph_1.2.4.1         lazyeval_0.2.2        
  [7] splines_3.6.0          ggplot2_3.2.1          digest_0.6.22         
 [10] foreach_1.4.7          htmltools_0.4.0        xray_0.2              
 [13] fansi_0.4.0            checkmate_1.9.4        cluster_2.1.0         
 [16] openxlsx_4.1.3         readr_1.3.1            magicfor_0.1.0        
 [19] ggfittext_0.8.1        lpSolve_5.6.13.3       prettyunits_1.0.2     
 [22] colorspace_1.4-1       mitools_2.4            pan_1.6               
 [25] haven_2.2.0            xfun_0.11              crayon_1.3.4          
 [28] jsonlite_1.6           lme4_1.1-21            zeallot_0.1.0         
 [31] zoo_1.8-6              iterators_1.0.12       glue_1.3.1            
 [34] survminer_0.4.6        gtable_0.3.0           SmartEDA_0.3.2        
 [37] DEoptimR_1.0-8         jomo_2.6-10            scales_1.1.0          
 [40] DBI_1.0.0              GGally_1.4.0           Rcpp_1.0.3            
 [43] htmlTable_1.13.2       xtable_1.8-4           progress_1.2.2        
 [46] foreign_0.8-72         km.ci_0.5-2            Formula_1.2-3         
 [49] survey_3.36            DT_0.10                describer_0.2.0       
 [52] htmlwidgets_1.5.1      sampling_2.8           httr_1.4.1            
 [55] RColorBrewer_1.1-2     acepack_1.4.1          ellipsis_0.3.0        
 [58] mice_3.6.0             DataExplorer_0.8.0     pkgconfig_2.0.3       
 [61] reshape_0.8.8          farver_2.0.1           nnet_7.3-12           
 [64] utf8_1.1.4             janitor_1.2.0          tidyselect_0.2.5      
 [67] labeling_0.3           rlang_0.4.1            later_1.0.0           
 [70] munsell_0.5.0          reactR_0.4.1           cellranger_1.1.0      
 [73] tools_3.6.0            cli_1.1.0              jmvcore_1.0.8         
 [76] generics_0.0.2         broom_0.5.2            evaluate_0.14         
 [79] stringr_1.4.0          fastmap_1.0.1          yaml_2.2.0            
 [82] knitr_1.26             zip_2.0.4              pander_0.6.3          
 [85] robustbase_0.93-5      survMisc_0.5.5         purrr_0.3.3           
 [88] mitml_0.3-7            visdat_0.5.3           nlme_3.1-142          
 [91] jmv_1.0.8              reactable_0.1.0        mime_0.7              
 [94] formatR_1.7            rstudioapi_0.10.0-9003 compiler_3.6.0        
 [97] curl_4.2               ggsignif_0.6.0         e1071_1.7-2           
[100] tibble_2.1.3           stringi_1.4.3          highr_0.8             
[103] parameters_0.2.5.1     forcats_0.4.0          lattice_0.20-38       
[106] whoami_1.3.0           Matrix_1.2-17          nloptr_1.2.1          
[109] ggsci_2.9              KMsurv_0.1-5           vctrs_0.2.0           
[112] pillar_1.4.2           lifecycle_0.1.0        networkD3_0.4         
[115] inspectdf_0.0.7        data.table_1.12.6      insight_0.7.0         
[118] httpuv_1.5.2           R6_2.4.1               latticeExtra_0.6-28   
[121] wakefield_0.3.4        promises_1.1.0         renv_0.8.3            
[124] gridExtra_2.3          rio_0.5.16             codetools_0.2-16      
[127] boot_1.3-23            MASS_7.3-51.4          assertthat_0.2.1      
[130] rprojroot_1.3-2        rjson_0.2.20           withr_2.1.2           
[133] explore_0.5.1          bayestestR_0.4.0.1     parallel_3.6.0        
[136] hms_0.5.2              rpart_4.1-15           grid_3.6.0            
[139] labelled_2.2.1         minqa_1.2.4            tidyr_1.0.0           
[142] class_7.3-15           rmarkdown_1.17         snakecase_0.11.0      
[145] ggpubr_0.2.4           shiny_1.4.0            lubridate_1.7.4       
[148] base64enc_0.1-3       
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

Last update on 2019-11-28 19:15:49  

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
    `Anti-X-intensity` +
    `Anti-Y-intensity` +
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
    vars = 'Anti-X-intensity',
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
    vars = 'Anti-Y-intensity',
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
library(finalfit)
library(survival)
explanatoryMultivariate <- explanatoryKM
dependentMultivariate <- dependentKM

mydata %>%
  finalfit(dependentMultivariate, explanatoryMultivariate) -> tMultivariate

knitr::kable(tMultivariate, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))
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
citation("report")
citation("finalfit")
# citation("ggstatsplot")
if(!dir.exists(here::here("bib"))) {dir.create(here::here("bib"))}

knitr::write_bib(x = c(.packages(), "knitr", "shiny"),
                 file = here::here("bib", "packages.bib")
)
sessionInfo()
pacman::p_loaded(all = TRUE)
```


---

\pagebreak


<!-- **push all changes to GitHub repository**  -->

<!-- ```{r git update} -->
<!-- source(file = here::here("R", "force_git.R")) -->
<!-- ``` -->


<!-- --- -->


# References


