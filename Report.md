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
date: "2020-02-26"
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
- \usepackage{float}
- \usepackage{svg}
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
  prettydoc::html_pretty:
    theme: leonids
    highlight: vignette
    toc: true
    number_sections: yes
    css: css/style.css
    includes:
      after_body: _footer.html
  rmarkdown::html_vignette: 
    css: 
    - !expr system.file("rmarkdown/templates/html_vignette/resources/vignette.css", package = "rmarkdown")
  redoc::redoc:
    highlight_outputs: TRUE
    margins: 1 
    line_numbers: FALSE 
  distill::distill_article:
    toc: true
  pdf_document: 
    fig_caption: yes
    highlight: kate
    number_sections: yes
    toc: yes
    latex_engine: lualatex
    toc_depth: 5
    keep_tex: yes
    includes:
      in_header: highlight_echo.tex    
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

$[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3635430.svg)](https://doi.org/10.5281/zenodo.3635430)$

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


**Setup global chunk settings**^[Change `echo = FALSE` to hide codes after knitting and Change `cache = TRUE` to knit quickly. Change `error=TRUE` to continue rendering while errors are present.]



```r
knitr::opts_chunk$set(
    eval = TRUE,
    echo = TRUE,
    fig.path = here::here("figs/"),
    message = FALSE,
    warning = FALSE,
    error = TRUE,
    cache = TRUE,
    comment = NA,
    tidy = TRUE,
    fig.width = 6,
    fig.height = 4
)
```



```r
library(knitr)
hook_output = knit_hooks$get("output")
knit_hooks$set(output = function(x, options) {
    # this hook is used only when the linewidth option is not NULL
    if (!is.null(n <- options$linewidth)) {
        x = knitr:::split_lines(x)
        # any lines wider than n should be wrapped
        if (any(nchar(x) > n)) 
            x = strwrap(x, width = n)
        x = paste(x, collapse = "\n")
    }
    hook_output(x, options)
})
```



```css
  pre:not([class]) {
    color: #333333;
    background-color: #cccccc;
  }
```


<style type="text/css">
  pre:not([class]) {
    color: #333333;
    background-color: #cccccc;
  }
</style>









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
  - Name: 249 entries: Aceyn, n = 1; Adalaide, n = 1; Adidas, n = 1 and 246 others (1 missing)
  - Sex: 2 entries: Male, n = 127; Female, n = 122 (1 missing)
  - Age: Mean = 49.54, SD = 14.16, Median = , MAD = 17.79, range: [25, 73], Skewness = 0.00, Kurtosis = -1.15, 1 missing
  - Race: 7 entries: White, n = 158; Hispanic, n = 38; Black, n = 30 and 4 others (1 missing)
  - PreinvasiveComponent: 2 entries: Absent, n = 203; Present, n = 46 (1 missing)
  - LVI: 2 entries: Absent, n = 147; Present, n = 102 (1 missing)
  - PNI: 2 entries: Absent, n = 171; Present, n = 78 (1 missing)
  - Death: 2 levels: FALSE (n = 83, 33.20%); TRUE (n = 166, 66.40%) and missing (n = 1, 0.40%)
  - Group: 2 entries: Treatment, n = 131; Control, n = 118 (1 missing)
  - Grade: 3 entries: 3, n = 109; 1, n = 78; 2, n = 62 (1 missing)
  - TStage: 4 entries: 4, n = 118; 3, n = 65; 2, n = 43 and 1 other (0 missing)
  - AntiX_intensity: Mean = 2.39, SD = 0.66, Median = , MAD = 1.48, range: [1, 3], Skewness = -0.63, Kurtosis = -0.65, 1 missing
  - AntiY_intensity: Mean = 2.02, SD = 0.80, Median = , MAD = 1.48, range: [1, 3], Skewness = -0.03, Kurtosis = -1.42, 1 missing
  - LymphNodeMetastasis: 2 entries: Absent, n = 144; Present, n = 105 (1 missing)
  - Valid: 2 levels: FALSE (n = 116, 46.40%); TRUE (n = 133, 53.20%) and missing (n = 1, 0.40%)
  - Smoker: 2 levels: FALSE (n = 130, 52.00%); TRUE (n = 119, 47.60%) and missing (n = 1, 0.40%)
  - Grade_Level: 3 entries: high, n = 109; low, n = 77; moderate, n = 63 (1 missing)
  - DeathTime: 2 entries: Within1Year, n = 149; MoreThan1Year, n = 101 (0 missing)
```



```r
mydata %>% explore::describe_tbl()
```

```
250 observations with 21 variables
18 variables containing missings (NA)
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
|Age                  |numeric       |double       |             250|   49.538153| 14.1595015|25            |         37|         49|         61|73          |
|Race                 |character     |character    |             250|          NA|         NA|Asian         |         NA|         NA|         NA|White       |
|PreinvasiveComponent |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|LVI                  |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|PNI                  |character     |character    |             250|          NA|         NA|Absent        |         NA|         NA|         NA|Present     |
|Death                |logical       |logical      |             250|          NA|         NA|FALSE         |         NA|         NA|         NA|TRUE        |
|Group                |character     |character    |             250|          NA|         NA|Control       |         NA|         NA|         NA|Treatment   |
|Grade                |character     |character    |             250|          NA|         NA|1             |         NA|         NA|         NA|3           |
|TStage               |character     |character    |             250|          NA|         NA|1             |         NA|         NA|         NA|4           |
|AntiX_intensity      |numeric       |double       |             250|    2.389558|  0.6636071|1             |          2|          2|          3|3           |
|AntiY_intensity      |numeric       |double       |             250|    2.016064|  0.7980211|1             |          1|          2|          3|3           |
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

<!--html_preserve--><div id="htmlwidget-af3967207f198bb3c442" class="reactable html-widget" style="width:auto;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-af3967207f198bb3c442">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"ID":["001","002","003","004","005","006","007","008","009","010","011","012","013","014","015","016","017","018","019","020","021","022","023","024","025","026","027","028","029","030","031","032","033","034","035","036","037","038","039","040","041","042","043","044","045","046","047","048","049","050","051","052","053","054","055","056","057","058","059","060","061","062","063","064","065","066","067","068","069","070","071","072","073","074","075","076","077","078","079","080","081","082","083","084","085","086","087","088","089","090","091","092","093","094","095","096","097","098","099","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250"],"Name":["Ariatna","Jahzlynn","Keylani","Proctor","Ibette","Jerame","Sharenna","Guin","Hollylynn","Eleazar","Kyleen","Tashya","Jud","Kaizleigh","Kylea","Altia","Secilia","Nikolle","Elroy","Mardis","Xitlali","Mckail","Edgel","Shaynah","Sirenity","Asil","Ruston","Syndi","Erdene","Shantrel","Estil","Raygine","Arieah","Kelaya","Murland","Kolia","Nataylia","Nancylee","Iayana","Wynnell","Chasia","Vignesh","Kahri","Enaya","Zephyn","Kimar","Tyzhane","Chong","Tayte","Travail","Sujei","Sundra","Emmett","Dashun","Sujeiry","Eimaan","Yatharth","Cedrina","Nicteha","Nigel","Karmisha","Darleane","Lynnie","Yaretcy","Elion","Naydia","Bevely","Kaleem","Oluwatoyosi","Makston","Eldridge","Melah","Poppie","Lavonia","Jeremey","Kaos","Alaija","Malaila","Travante","Gevonte","Sherridan","Berra","Oluwafikayomi","Kynsie","Cylena","Yarizmar","Nada","Wilford","Regine","Kyheim","Clarese","Sylvania","Brandiss","Orlin","Shineka","Deserai","Chemika","Emelia","Johnisha","Sehajveer","Jacoria","Marquavion","Deema","Larencia","Jostein","Jeffery","Drennen","Dakotah","Wynonah","Valicia","Vihaa","Aceyn","Arbaz","Nyella","Caelen","Reather","Thuytrang","Jahla","Ihla","Ruqaya","Nataliz","Deylani","Kokoro","Niviah","Ladanian","Adalaide","Haim","Daveda","Stasi","Quian","Calven","Bradli","Katarina","Jameis","Cennie","Estoria","Jayceyon","Kemori","Constanc","Christianson","Saprina","Medeline","Kynzley","Eribella","Naleigha","Taidyn","Takanori","Ulissa","Makamae","Kristianne","Tracilynn","Terressa","Dorrance","Girlee","Brilliant","Toie","Fredonia","Ryllie","Damion","Theryn","Keneka","Charmella","Ronicia","Aramis","Infantof","Kandus","Dezon","Shambrica","Nealy","Shanitta","Delba","Orphia","Lamariya","Eliuth",null,"Shanekqa","Moretta","Suleica","Kumiko","Zainub","Versia","Dhane","Minnetta","Jaron","Dache","Janat","Maralou","Rhodney","Jazlyne","Camerynn","Dakota","Ramla","Dicki","Loic","Eygpt","Maita","Alicha","Jaleen","Rikeisha","Kentay","Takwon","Nadeane","Karneisha","Helenann","Jathziry","Jeune","Crosslyn","Bonna","Tamilla","Keshawnda","Correna","Immer","Naomigrace","Paitlynn","Jannice","Phillipmichael","Nessie","Keyson","Kyanna","Lillyth","Quanasia","Prisicilla","Teraji","Ehud","Jayva","Rosiland","Anastasia","Hydeia","Kaylen","Angelena","Eadon","Zaiyah","Sathvik","Adidas","Daniale","Santez","Harker","Bricia","Reyaansh","Deklyn","Trestin","Taylan","Elyzza","Krissandra","Kindsay","Railynne","Danixa","Sam","Nylen","Jaricka"],"Sex":["Female","Female","Female","Female","Male","Female","Female","Male","Male","Female","Male","Female","Female","Male","Female","Male","Female","Female","Female","Male","Female","Female","Male","Female","Female","Female","Male","Female","Male","Male","Female","Male","Female","Female","Female","Female","Female","Male","Female","Female","Male","Male","Male","Male","Female","Male","Male","Male","Male","Male","Female","Male","Female","Female","Male","Female","Female","Female","Female","Female","Male","Male","Male","Female","Female","Female","Male","Male","Male","Male","Female","Male","Male","Female","Male","Female","Male","Male","Female","Female","Female","Female","Female","Female","Male","Male","Female","Male","Female","Male","Female","Female","Male","Male","Male","Male","Male","Male","Female","Male","Male","Male","Female","Female","Female","Male","Female","Male","Male","Female","Male","Female","Female","Female","Male","Male","Female","Male","Male","Female","Female","Female","Male","Male","Female","Male","Female","Female","Male","Female","Female","Male","Female","Male","Female","Male","Female","Male","Female","Female","Female","Female","Male","Female","Male","Female","Male","Female","Male","Male","Male","Female","Female","Male","Male","Female","Female","Male","Male","Male","Male","Female","Male","Female","Male","Male","Male","Female","Female","Female","Female","Male","Female","Male","Female","Female","Female","Male","Male","Male","Male","Female","Male","Male","Female","Male","Female","Female","Male","Male","Female","Male","Female","Male","Male","Female","Female","Female","Female","Male","Male","Female","Male","Male","Male","Male","Female","Female","Male","Female","Male","Male","Male","Female","Male","Female","Male","Male","Male","Female","Male","Male","Male","Female","Male","Female","Male","Female","Male","Female","Female","Female","Male","Male","Female",null,"Female","Male","Female","Male","Male","Male","Female","Male","Male","Female","Male","Male","Male","Male"],"Age":[30,32,53,57,47,58,59,54,35,27,53,55,72,51,46,65,58,34,54,45,59,26,65,44,49,25,72,26,54,73,63,40,44,58,62,51,61,60,29,32,61,68,68,44,72,51,40,62,32,40,53,28,53,59,55,51,57,48,28,33,42,43,"NA",65,60,26,52,46,40,32,32,70,72,49,30,71,42,49,53,55,72,54,68,47,67,36,54,72,64,65,48,32,56,51,66,54,43,30,52,42,58,33,43,38,56,61,42,46,28,37,54,65,53,27,73,70,32,48,39,68,52,34,47,28,49,43,29,67,41,44,31,73,29,72,73,68,32,41,36,67,61,36,30,73,41,49,35,60,41,71,69,35,41,46,41,38,27,44,70,67,71,61,68,36,41,33,31,46,31,72,72,60,58,33,54,33,34,34,35,56,39,47,62,40,66,72,71,73,46,59,34,69,29,40,59,31,45,49,68,26,55,52,42,58,59,43,45,33,32,25,36,64,63,34,35,59,70,52,44,43,58,48,25,61,57,25,50,59,71,63,46,67,31,54,71,59,40,48,55,60,48,40,57,50,25,28,72,54,66,34],"Race":["White","White","White","Hispanic","White","White","Black","White","White","Native","White","Asian","White","White","White","Hispanic","White","White","White","White","Hispanic","White","White","White","Black","White","Black","White","White","Black","White","Hispanic","White","White","White","White","Asian","White","White","White","White","White","White","Black","White","Hispanic","White","Asian","Hispanic","Native","White","White","Black","Asian","White","White","Hispanic","Hispanic","White","White","White","White","Black","White","White","Bi-Racial","White","Asian","White","White","Black","White","Hispanic","Black","White","White","Hispanic","Bi-Racial","Black","White","White","White","White","Hispanic","Hispanic","Hispanic","White","White","White","Black","Black","Black","White","Black","Black","Black","White","White","Black","Hispanic","White","White","Hispanic","Black","Asian","Black","White","Bi-Racial","White","White","White","Bi-Racial","Asian","White","Hispanic","Hispanic","White","Hispanic","White","White","White","White","White","White","White","Bi-Racial","Asian","White","Black","White","White","Hispanic","White","White","Hispanic","White","White","White","Black","White","White","Black","Hispanic","White","Hispanic","White","White","White","Other","White","White","White","White","Hispanic","Asian","White","Hispanic","White","White","White","White","White","White","White","White","White","Asian","White","Black","White","White","White","White","White","White","Asian","Black","Black","White","White","Black","Hispanic","White","White","White","White","Asian","White","Hispanic","White","Black","White","White","White","Hispanic","Hispanic","Asian","White","White","Hispanic","White","Hispanic","White","White","White","White","Hispanic","White","White","Hispanic","White",null,"White","White","White","White","Hispanic","White","Black","White","White","White","White","White","White","White","White","White","Black","Asian","White","White","White","White","White","White","Asian","Hispanic","White","White","Hispanic","Hispanic","White","Black","White","Hispanic","White","White","White","Hispanic"],"PreinvasiveComponent":["Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Present",null,"Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Absent","Absent"],"LVI":["Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Present","Present","Absent","Absent","Present","Present","Absent","Present","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Present","Absent","Present","Present","Present","Absent","Present","Absent","Present","Absent","Present","Present","Present","Present","Present","Present","Present","Present","Present","Present","Absent","Absent","Present","Present","Present","Absent","Absent","Present","Present","Present","Present","Absent","Present","Present","Present","Present","Absent","Absent","Present","Present","Present","Present","Present","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Present","Present","Present","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Present","Absent","Present","Present","Present","Present","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent",null,"Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent"],"PNI":["Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Present","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Present","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Present","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent",null,"Absent","Present","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Absent","Present"],"LastFollowUpDate":["2019-09-26T00:00:00","2019-04-26T00:00:00","2019-10-26T00:00:00","2019-05-26T00:00:00","2019-08-26T00:00:00","2019-05-26T00:00:00","2019-04-26T00:00:00","2019-07-26T00:00:00","2019-06-26T00:00:00","2019-10-26T00:00:00","2019-12-26T00:00:00","2019-04-26T00:00:00","2019-11-26T00:00:00","2019-11-26T00:00:00","2019-10-26T00:00:00","2019-09-26T00:00:00","2019-11-26T00:00:00","2019-04-26T00:00:00","2019-04-26T00:00:00","2019-07-26T00:00:00","2019-03-26T00:00:00","2019-03-26T00:00:00","2020-01-26T00:00:00","2019-03-26T00:00:00","2020-02-26T00:00:00","2019-04-26T00:00:00","2019-06-26T00:00:00","2019-04-26T00:00:00","2020-02-26T00:00:00","2019-07-26T00:00:00","2019-10-26T00:00:00","2019-12-26T00:00:00","2020-01-26T00:00:00","2019-12-26T00:00:00","2019-08-26T00:00:00","2019-04-26T00:00:00","2019-06-26T00:00:00","2019-08-26T00:00:00","2020-01-26T00:00:00","2020-01-26T00:00:00","2020-02-26T00:00:00","2019-12-26T00:00:00","2019-11-26T00:00:00","2019-10-26T00:00:00","2019-10-26T00:00:00","2019-10-26T00:00:00","2019-08-26T00:00:00","2019-08-26T00:00:00","2019-09-26T00:00:00","2019-03-26T00:00:00","2019-06-26T00:00:00","2019-05-26T00:00:00","2019-04-26T00:00:00","2019-08-26T00:00:00","2019-03-26T00:00:00","2019-09-26T00:00:00","2019-11-26T00:00:00","2019-12-26T00:00:00","2019-03-26T00:00:00","2019-07-26T00:00:00","2020-02-26T00:00:00","2019-06-26T00:00:00","2019-09-26T00:00:00","2019-06-26T00:00:00","2019-06-26T00:00:00","2019-11-26T00:00:00","2020-02-26T00:00:00","2020-01-26T00:00:00","2020-02-26T00:00:00","2019-06-26T00:00:00","2019-04-26T00:00:00","2020-02-26T00:00:00","2019-07-26T00:00:00","2019-06-26T00:00:00","2019-10-26T00:00:00","2019-08-26T00:00:00","2019-05-26T00:00:00","2019-07-26T00:00:00","2019-07-26T00:00:00","2019-08-26T00:00:00","2019-04-26T00:00:00","2020-01-26T00:00:00","2019-12-26T00:00:00","2019-10-26T00:00:00","2020-02-26T00:00:00","2019-04-26T00:00:00","2019-09-26T00:00:00","2019-08-26T00:00:00","2019-12-26T00:00:00","2019-03-26T00:00:00","2019-06-26T00:00:00","2019-07-26T00:00:00","2019-07-26T00:00:00","2019-04-26T00:00:00","2020-02-26T00:00:00","2020-02-26T00:00:00","2019-08-26T00:00:00","2019-12-26T00:00:00","2019-06-26T00:00:00","2019-11-26T00:00:00","2019-11-26T00:00:00","2019-09-26T00:00:00","2019-03-26T00:00:00","2019-09-26T00:00:00","2019-11-26T00:00:00","2019-04-26T00:00:00","2020-02-26T00:00:00","2019-07-26T00:00:00","2019-07-26T00:00:00","2019-05-26T00:00:00","2019-12-26T00:00:00","2019-05-26T00:00:00","2020-02-26T00:00:00","2019-03-26T00:00:00","2020-01-26T00:00:00","2019-08-26T00:00:00","2019-05-26T00:00:00","2019-10-26T00:00:00","2019-06-26T00:00:00","2019-12-26T00:00:00","2019-06-26T00:00:00","2019-11-26T00:00:00","2019-09-26T00:00:00","2019-12-26T00:00:00","2020-01-26T00:00:00","2019-12-26T00:00:00","2019-10-26T00:00:00","2019-12-26T00:00:00","2019-05-26T00:00:00","2019-04-26T00:00:00","2019-08-26T00:00:00","2019-10-26T00:00:00","2019-06-26T00:00:00","2019-11-26T00:00:00","2019-11-26T00:00:00","2019-06-26T00:00:00","2019-03-26T00:00:00","2019-03-26T00:00:00","2019-11-26T00:00:00","2019-07-26T00:00:00","2019-08-26T00:00:00","2019-12-26T00:00:00","2019-11-26T00:00:00","2019-12-26T00:00:00","2019-07-26T00:00:00","2019-05-26T00:00:00","2020-01-26T00:00:00","2019-05-26T00:00:00","2019-10-26T00:00:00","2019-07-26T00:00:00","2019-10-26T00:00:00","2019-12-26T00:00:00","2019-08-26T00:00:00","2019-11-26T00:00:00","2019-09-26T00:00:00","2019-03-26T00:00:00","2019-12-26T00:00:00","2020-02-26T00:00:00","2019-11-26T00:00:00","2019-09-26T00:00:00","2019-06-26T00:00:00","2019-08-26T00:00:00","2019-11-26T00:00:00","2019-03-26T00:00:00","2019-12-26T00:00:00","2019-07-26T00:00:00","2020-01-26T00:00:00","2019-10-26T00:00:00","2019-04-26T00:00:00","2019-09-26T00:00:00","2019-03-26T00:00:00","2019-11-26T00:00:00","2019-10-26T00:00:00","2019-06-26T00:00:00","2019-11-26T00:00:00","2020-01-26T00:00:00","2019-08-26T00:00:00","2019-03-26T00:00:00","2019-06-26T00:00:00","2020-02-26T00:00:00","2019-10-26T00:00:00","2020-02-26T00:00:00","2019-12-26T00:00:00","2020-01-26T00:00:00","2020-01-26T00:00:00","2019-06-26T00:00:00","2019-03-26T00:00:00","2020-01-26T00:00:00","2019-08-26T00:00:00","2020-02-26T00:00:00","2019-04-26T00:00:00","2019-08-26T00:00:00","2019-07-26T00:00:00","2020-01-26T00:00:00","2019-10-26T00:00:00","2019-06-26T00:00:00","2020-01-26T00:00:00","2019-10-26T00:00:00","2019-11-26T00:00:00","2019-03-26T00:00:00","2019-05-26T00:00:00","2019-10-26T00:00:00","2019-04-26T00:00:00","2019-10-26T00:00:00","2020-02-26T00:00:00","2019-09-26T00:00:00","2019-04-26T00:00:00","2019-12-26T00:00:00","2019-09-26T00:00:00","2019-11-26T00:00:00","2019-03-26T00:00:00","2019-07-26T00:00:00","2019-03-26T00:00:00","2019-06-26T00:00:00","2019-07-26T00:00:00","2019-07-26T00:00:00","2019-08-26T00:00:00","2019-05-26T00:00:00","2019-03-26T00:00:00","2019-06-26T00:00:00","2019-05-26T00:00:00","2019-10-26T00:00:00","2020-02-26T00:00:00","2019-09-26T00:00:00","2019-08-26T00:00:00","2020-01-26T00:00:00","2020-02-26T00:00:00","2020-01-26T00:00:00","2019-09-26T00:00:00","2019-10-26T00:00:00","2019-03-26T00:00:00","2019-05-26T00:00:00","2020-01-26T00:00:00","2019-12-26T00:00:00","2019-08-26T00:00:00","2019-06-26T00:00:00",null,"2019-05-26T00:00:00","2019-09-26T00:00:00","2020-02-26T00:00:00","2020-02-26T00:00:00","2020-02-26T00:00:00","2019-07-26T00:00:00","2019-03-26T00:00:00","2020-02-26T00:00:00","2019-04-26T00:00:00","2020-01-26T00:00:00","2019-10-26T00:00:00","2019-12-26T00:00:00","2019-08-26T00:00:00"],"Death":[false,true,true,true,true,false,false,true,true,true,true,true,true,true,true,true,true,null,true,true,false,false,false,false,true,true,false,true,true,true,true,false,true,true,false,false,true,true,true,true,true,true,false,true,true,true,true,false,true,false,false,false,true,false,true,false,true,true,true,true,true,true,true,true,false,true,false,false,true,true,true,true,true,true,true,true,true,false,false,true,true,true,false,true,false,false,true,true,false,false,true,false,true,true,true,false,true,false,false,true,false,false,true,true,false,true,false,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true,false,true,true,false,false,true,true,true,false,true,false,true,true,true,true,true,false,true,true,false,true,false,false,true,true,true,true,false,false,true,true,false,true,true,false,true,false,false,true,true,true,false,false,false,true,true,false,false,true,false,true,true,true,true,true,true,true,false,false,false,false,false,true,true,false,false,true,true,false,false,true,false,true,true,true,true,true,false,true,true,true,false,true,true,true,true,true,true,true,false,true,true,false,false,false,false,true,false,true,true,true,true,true,true,true,false,true,false,true,true,true,true,true,true,true,true,false,false,true,false,true,true,true,false,true,false,true,true],"Group":["Control","Control","Control","Control","Control","Control","Control","Control","Treatment","Control","Control","Treatment","Control","Treatment","Treatment","Control","Control","Treatment","Treatment","Treatment","Control","Treatment","Treatment","Control","Treatment","Control","Treatment","Treatment","Control","Treatment","Control","Treatment","Treatment","Treatment","Control","Control","Treatment","Treatment","Control","Control","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Control","Treatment","Treatment","Treatment","Treatment","Control","Control","Treatment","Control","Treatment","Control","Treatment","Treatment","Control","Control","Treatment","Control","Control","Treatment","Control","Control","Control","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Control","Control","Control","Treatment","Control","Control","Treatment","Treatment","Control","Control","Control","Treatment","Treatment","Control","Control","Treatment","Treatment","Control","Treatment","Control","Control","Control","Treatment","Control","Treatment","Treatment","Control","Treatment","Control","Treatment","Treatment","Treatment","Control","Treatment","Control","Treatment","Control","Control","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Control","Treatment","Control","Control","Treatment","Control","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Control","Treatment","Control","Control","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Control","Treatment","Control","Control","Treatment","Control","Treatment","Treatment","Control","Control","Control","Treatment","Treatment","Treatment","Control","Control","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Control","Control","Control","Control","Control","Control","Control","Control","Treatment","Control","Control","Treatment","Treatment","Treatment","Treatment","Control","Control","Treatment","Treatment","Control","Treatment","Control","Control","Control","Control","Control","Treatment","Treatment","Treatment","Control","Treatment","Treatment","Control","Control","Control","Control","Control","Treatment","Treatment","Control","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Control","Treatment","Treatment","Treatment","Treatment","Treatment","Treatment","Control",null,"Control","Control","Control","Control","Control","Control","Treatment","Treatment","Control","Treatment","Control","Treatment","Treatment"],"Grade":["1","1","2","1","2","2","3","1","2","1","2","3","3","3","2","3","3","3","3","3","3","1","1","1","3","3","2","3","3","3","1","3","1","3","1","3","3","3","3","2","3","3","3","1","2","1","1","3","3","2","2","1","1","3","1","3","2","1","3","3","3","3","3","1","3","3","1","1","3","3","1","3","2","1","3","1","3","3","3","1","2","1","2","2","3","2","1","1","3","2","1","3","2","1","2","1","3","3",null,"1","2","1","3","3","3","3","1","3","3","1","2","2","2","3","2","2","3","1","1","3","2","3","3","2","3","1","2","1","1","1","3","3","3","1","2","3","1","1","3","2","1","2","2","1","3","2","1","1","2","2","3","3","3","2","2","1","3","2","3","1","2","1","3","1","3","1","2","3","3","1","2","1","1","1","3","3","1","1","2","1","3","2","1","1","2","1","3","3","3","3","3","1","2","2","3","1","3","2","2","1","3","3","3","3","3","2","1","3","1","3","3","3","3","2","2","2","3","2","2","1","3","2","1","1","1","3","3","3","3","2","1","2","2","3","1","2","2","1","1","1","3","3","3","3","2","3","1","3","3","3"],"TStage":["4","4","3","3","1","3","3","3","4","4","4","3","2","4","2","3","3","4","4","2","2","4","1","4","3","4","3","4","4","4","4","4","4","4","3","4","2","2","4","3","4","1","4","1","4","4","2","4","4","4","4","4","2","4","4","4","1","2","4","4","4","4","4","3","4","4","4","4","1","3","4","4","4","4","4","2","4","3","2","4","4","3","3","1","4","4","2","4","4","2","2","4","1","4","4","3","3","4","2","4","4","4","4","4","3","4","4","1","3","3","3","4","4","3","3","4","2","3","4","4","4","4","4","1","4","2","3","2","3","3","2","4","4","4","4","2","4","4","3","2","4","4","4","4","3","2","4","4","4","4","1","2","3","4","3","3","2","1","4","2","3","2","4","3","3","1","3","4","4","2","4","3","4","3","2","1","1","3","4","3","1","3","3","2","4","4","3","3","3","4","2","3","4","1","4","3","1","4","1","1","1","1","2","3","4","3","3","4","4","4","3","4","2","2","3","3","4","4","3","2","4","3","3","3","4","1","3","1","4","3","2","2","4","4","2","3","3","4","2","3","4","2","2","2","4","3","2","4","2","4"],"AntiX_intensity":[2,2,2,2,3,1,1,3,2,3,2,3,1,3,1,2,3,2,3,3,3,3,2,3,2,2,3,3,3,2,3,3,2,2,1,3,2,3,3,2,3,2,3,2,3,2,3,1,3,1,2,3,2,2,2,2,2,3,2,3,2,3,3,2,1,3,2,3,3,3,2,3,3,3,3,3,3,3,2,2,3,2,3,3,3,2,3,1,3,1,2,3,3,3,3,2,3,3,2,"NA",3,3,3,3,3,2,3,2,3,3,3,3,2,3,2,1,3,2,2,2,2,2,1,1,3,3,3,3,3,2,3,3,2,2,3,3,3,3,2,2,2,2,3,2,2,3,2,2,3,3,2,1,2,3,2,2,3,3,2,3,2,1,2,3,2,3,3,1,2,2,3,3,2,2,3,3,3,3,2,2,2,3,2,2,3,3,2,3,2,2,2,2,1,1,1,3,1,2,3,3,1,2,2,3,1,2,3,3,3,2,1,3,2,3,3,3,3,3,3,3,2,2,2,2,3,2,2,3,1,3,2,2,3,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,2,1],"AntiY_intensity":[2,2,2,3,2,1,2,3,3,1,1,2,1,3,1,2,1,3,2,1,1,3,1,1,2,1,2,3,2,1,1,3,2,3,3,2,1,3,3,3,2,3,3,2,2,3,2,3,2,2,2,"NA",3,2,1,3,3,3,2,3,2,2,3,1,2,2,3,3,3,1,1,2,2,1,1,1,3,2,2,1,1,2,3,3,1,3,1,2,1,2,1,3,2,1,3,2,2,2,2,2,2,2,1,2,3,1,3,1,1,3,3,3,3,3,2,3,2,2,3,1,3,3,2,1,1,3,1,3,3,2,1,3,2,1,3,2,2,3,2,2,3,3,3,2,3,1,2,3,2,1,1,2,3,3,2,3,1,1,1,1,1,1,2,3,3,3,2,2,1,2,2,1,1,1,2,1,3,1,3,2,1,3,2,2,1,3,1,2,2,3,2,2,2,1,3,2,1,3,3,2,1,3,3,3,3,1,3,2,1,3,1,1,2,1,2,2,2,2,2,3,2,1,3,1,1,2,2,2,1,2,1,3,1,1,1,3,2,1,2,2,1,3,1,1,2,3,2,2,3,1],"LymphNodeMetastasis":["Present","Absent","Present","Present","Present","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present",null,"Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Present","Absent","Absent","Present","Present","Absent","Absent","Present","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Present","Present","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Present","Present","Present","Absent","Absent","Absent","Absent","Absent","Present","Present","Present","Present","Absent","Absent","Absent","Present","Present","Present","Absent","Present","Absent","Present","Present","Present","Absent","Present","Present","Present","Present","Present","Absent","Absent","Present","Present","Absent","Present","Absent","Present","Present","Absent","Present","Absent","Present","Absent","Present","Absent","Present","Absent","Present","Present","Present","Present","Present","Present","Absent","Present","Present","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Present","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Present","Present","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Present","Present","Absent","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Present","Absent","Absent","Absent","Absent","Present","Absent","Present","Absent","Present","Absent","Absent","Absent","Present","Absent","Absent","Absent","Present","Present","Absent","Absent","Present","Present","Present","Present","Absent","Present","Present","Present","Absent","Absent","Absent"],"Valid":[true,false,true,true,false,false,true,true,true,true,false,true,false,false,false,true,true,false,false,true,false,false,false,false,false,false,true,false,false,true,true,true,false,true,false,false,false,false,false,false,true,true,true,true,true,false,false,true,false,false,false,true,true,true,false,false,true,false,true,true,true,true,true,false,true,false,false,true,false,false,true,false,true,true,true,true,false,false,false,true,false,false,true,true,false,false,false,false,true,false,false,true,true,false,true,true,true,true,true,true,false,false,false,false,true,false,true,true,true,false,true,true,true,false,false,false,true,true,true,false,true,true,false,false,true,false,true,true,true,true,false,false,false,true,false,true,true,false,true,true,null,false,false,false,true,false,false,true,true,true,true,true,true,false,false,false,true,true,true,false,true,true,true,false,true,true,false,true,true,true,true,true,false,false,false,true,true,false,false,true,false,true,false,false,false,true,true,false,false,true,false,false,false,true,true,true,false,true,false,true,false,false,true,false,false,true,true,false,true,true,false,true,false,true,true,true,true,true,true,false,false,false,false,true,true,true,false,true,true,false,true,true,false,false,false,false,true,true,true,true,true,true,true,true,true,true,false,false,false,false],"Smoker":[true,true,false,false,false,false,true,true,true,true,true,false,true,true,true,false,false,false,true,false,true,true,true,false,false,false,true,true,false,true,true,false,true,false,true,false,false,false,true,true,true,false,false,false,true,true,false,true,false,false,true,false,false,false,true,false,false,true,true,false,false,true,false,null,true,true,false,false,true,true,true,false,false,false,true,true,false,false,true,true,true,true,true,true,false,false,true,true,true,true,false,false,true,false,true,false,true,false,false,false,true,true,true,true,true,true,true,true,false,true,false,true,false,true,false,true,true,false,true,true,false,true,false,false,false,false,false,false,true,false,true,false,true,true,false,true,false,true,false,true,false,false,false,true,true,false,true,true,false,false,true,false,true,true,true,false,true,true,false,true,false,true,false,false,true,false,false,false,false,false,false,false,true,false,true,true,false,false,false,false,true,false,true,true,false,true,false,true,false,true,false,true,false,true,false,false,true,false,true,false,true,false,true,false,true,false,false,false,false,false,false,true,true,false,true,false,false,false,true,false,false,false,false,true,true,false,false,false,false,true,true,true,false,true,false,true,false,true,false,false,true,true,true,false,false,true,false,false,false,false],"Grade_Level":["moderate","moderate","high","low","high","moderate","high","high","high","moderate","high","high","moderate","low","high","high","low",null,"high","low","high","low","low","moderate","low","high","moderate","moderate","high","high","high","low","low","high","high","high","low","low","moderate","moderate","low","high","high","low","low","high","low","high","high","low","moderate","low","low","low","high","low","low","moderate","high","high","high","high","high","low","moderate","low","high","high","low","high","low","high","low","high","low","moderate","high","moderate","high","low","high","moderate","high","moderate","low","moderate","high","high","moderate","moderate","moderate","high","low","low","low","high","high","high","moderate","low","low","moderate","moderate","moderate","moderate","high","high","high","high","moderate","high","high","moderate","moderate","moderate","low","moderate","moderate","low","low","moderate","moderate","high","high","low","high","high","moderate","low","moderate","high","low","low","high","low","moderate","moderate","high","moderate","moderate","low","high","high","low","moderate","low","low","moderate","high","high","high","low","high","high","low","high","high","high","moderate","high","low","high","high","high","moderate","high","low","moderate","high","high","high","low","low","high","high","high","low","moderate","low","moderate","high","high","low","high","moderate","moderate","low","high","low","low","moderate","high","high","high","high","low","low","high","high","high","low","moderate","low","moderate","moderate","moderate","moderate","moderate","high","high","high","moderate","low","high","low","low","moderate","low","moderate","low","high","high","low","high","low","moderate","low","low","moderate","high","high","high","high","low","low","high","high","high","high","moderate","high","low","moderate","high","low","high","high","high","low","low"],"SurgeryDate":["2019-05-10T00:00:00","2018-09-03T00:00:00","2019-03-22T00:00:00","2018-09-28T00:00:00","2018-10-07T00:00:00","2018-10-28T00:00:00","2018-08-15T00:00:00","2018-08-27T00:00:00","2019-03-10T00:00:00","2019-03-06T00:00:00","2019-04-13T00:00:00","2018-10-25T00:00:00",null,"2019-02-11T00:00:00","2018-11-20T00:00:00","2018-10-06T00:00:00","2019-02-21T00:00:00","2018-09-09T00:00:00","2018-12-23T00:00:00","2019-03-05T00:00:00","2018-06-03T00:00:00","2018-07-16T00:00:00","2019-07-25T00:00:00","2018-10-12T00:00:00","2019-08-13T00:00:00","2018-05-13T00:00:00","2019-03-02T00:00:00","2018-06-20T00:00:00","2019-11-26T00:00:00","2019-01-14T00:00:00","2018-11-16T00:00:00","2019-06-09T00:00:00","2019-04-05T00:00:00","2019-06-04T00:00:00","2019-05-16T00:00:00","2018-05-19T00:00:00","2018-11-02T00:00:00","2019-01-25T00:00:00","2019-07-16T00:00:00","2019-03-19T00:00:00","2019-07-25T00:00:00","2019-01-19T00:00:00","2019-02-06T00:00:00","2019-04-01T00:00:00","2019-07-23T00:00:00","2019-07-08T00:00:00","2019-01-02T00:00:00","2018-11-11T00:00:00","2019-03-25T00:00:00","2018-05-14T00:00:00","2018-07-19T00:00:00","2019-02-18T00:00:00","2018-09-15T00:00:00","2018-11-21T00:00:00","2018-05-28T00:00:00","2018-10-19T00:00:00","2019-01-25T00:00:00","2019-07-14T00:00:00","2018-06-10T00:00:00","2019-02-14T00:00:00","2019-08-30T00:00:00","2018-10-15T00:00:00","2019-05-22T00:00:00","2018-09-19T00:00:00","2018-11-17T00:00:00","2019-05-09T00:00:00","2019-07-27T00:00:00","2019-05-06T00:00:00","2019-10-26T00:00:00","2019-02-24T00:00:00","2018-06-05T00:00:00","2019-10-07T00:00:00","2018-12-30T00:00:00","2018-12-09T00:00:00","2019-05-17T00:00:00","2018-12-26T00:00:00","2018-08-18T00:00:00","2018-11-14T00:00:00","2018-11-07T00:00:00","2018-12-01T00:00:00","2018-05-01T00:00:00","2019-09-16T00:00:00","2019-03-29T00:00:00","2018-11-04T00:00:00","2019-03-05T00:00:00","2018-10-27T00:00:00","2018-11-30T00:00:00","2019-05-28T00:00:00","2019-01-30T00:00:00","2018-09-28T00:00:00","2018-08-20T00:00:00","2019-04-18T00:00:00","2019-02-06T00:00:00","2018-12-14T00:00:00","2019-11-22T00:00:00","2019-08-06T00:00:00","2018-10-07T00:00:00","2019-08-30T00:00:00","2019-01-23T00:00:00","2019-02-20T00:00:00","2019-05-18T00:00:00","2019-05-23T00:00:00","2018-07-03T00:00:00","2018-12-24T00:00:00","2019-04-19T00:00:00","2018-09-17T00:00:00","2019-03-03T00:00:00","2018-12-10T00:00:00","2018-10-16T00:00:00","2019-02-20T00:00:00","2019-09-17T00:00:00","2018-08-13T00:00:00","2019-05-29T00:00:00","2018-11-30T00:00:00","2019-06-21T00:00:00","2019-04-21T00:00:00","2018-10-05T00:00:00","2018-12-12T00:00:00","2018-12-29T00:00:00","2019-01-21T00:00:00","2018-07-26T00:00:00","2019-01-28T00:00:00","2019-01-12T00:00:00","2019-01-29T00:00:00","2019-03-13T00:00:00","2019-03-01T00:00:00","2019-03-30T00:00:00","2019-04-21T00:00:00","2018-06-22T00:00:00","2019-01-22T00:00:00","2018-10-21T00:00:00","2019-01-04T00:00:00","2018-07-07T00:00:00","2019-06-14T00:00:00","2019-02-26T00:00:00","2018-08-28T00:00:00","2018-04-03T00:00:00","2018-08-13T00:00:00","2019-07-20T00:00:00","2019-04-17T00:00:00","2019-02-07T00:00:00","2019-09-02T00:00:00","2019-03-12T00:00:00","2019-07-05T00:00:00","2019-01-06T00:00:00","2018-07-21T00:00:00","2019-10-08T00:00:00","2018-10-11T00:00:00","2019-01-20T00:00:00","2018-10-23T00:00:00","2017-08-04T00:00:00","2018-02-23T00:00:00","2017-09-07T00:00:00","2018-04-08T00:00:00","2018-01-14T00:00:00","2016-04-13T00:00:00","2017-07-13T00:00:00","2018-08-15T00:00:00","2017-05-23T00:00:00","2017-07-03T00:00:00","2016-10-21T00:00:00","2017-01-14T00:00:00","2017-01-13T00:00:00","2017-05-21T00:00:00","2018-04-20T00:00:00","2017-11-21T00:00:00","2017-03-09T00:00:00","2018-02-26T00:00:00","2017-10-13T00:00:00","2017-03-08T00:00:00","2017-12-15T00:00:00","2017-11-17T00:00:00","2016-11-16T00:00:00","2016-10-23T00:00:00","2018-10-18T00:00:00","2018-06-04T00:00:00","2017-09-03T00:00:00","2016-08-26T00:00:00","2018-04-18T00:00:00","2017-11-16T00:00:00","2017-07-05T00:00:00","2018-12-05T00:00:00","2017-07-21T00:00:00","2018-01-13T00:00:00","2018-07-29T00:00:00","2017-11-07T00:00:00","2016-08-29T00:00:00","2018-07-16T00:00:00","2017-09-20T00:00:00","2019-02-04T00:00:00","2017-11-04T00:00:00","2017-10-23T00:00:00","2018-07-12T00:00:00","2017-07-26T00:00:00","2017-08-30T00:00:00","2018-05-03T00:00:00","2018-06-05T00:00:00","2017-10-04T00:00:00","2017-06-10T00:00:00","2017-03-08T00:00:00","2017-09-01T00:00:00","2018-06-17T00:00:00","2017-12-19T00:00:00","2018-08-24T00:00:00","2018-06-13T00:00:00","2017-08-21T00:00:00","2017-01-07T00:00:00","2017-11-20T00:00:00","2016-11-16T00:00:00","2018-01-27T00:00:00","2016-12-21T00:00:00","2016-09-27T00:00:00","2017-07-18T00:00:00","2016-08-15T00:00:00","2018-06-14T00:00:00","2016-08-29T00:00:00","2018-02-21T00:00:00","2016-08-16T00:00:00","2018-02-14T00:00:00","2017-08-28T00:00:00","2017-03-28T00:00:00","2017-10-25T00:00:00","2017-06-05T00:00:00","2017-08-25T00:00:00","2016-11-30T00:00:00","2016-05-09T00:00:00","2015-12-07T00:00:00","2016-12-16T00:00:00","2015-11-16T00:00:00","2015-05-23T00:00:00","2014-07-21T00:00:00","2016-04-21T00:00:00","2017-01-12T00:00:00","2015-09-01T00:00:00","2016-03-04T00:00:00","2015-01-09T00:00:00","2015-10-23T00:00:00","2015-09-12T00:00:00","2015-12-08T00:00:00","2016-05-02T00:00:00","2016-06-20T00:00:00","2015-12-18T00:00:00","2015-06-28T00:00:00","2016-04-08T00:00:00","2016-12-03T00:00:00","2014-06-22T00:00:00","2015-09-13T00:00:00","2015-01-27T00:00:00","2016-08-11T00:00:00","2015-01-26T00:00:00"],"DeathTime":["Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","MoreThan1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","Within1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year","MoreThan1Year"]},"columns":[{"accessor":"ID","name":"ID","type":"character"},{"accessor":"Name","name":"Name","type":"character"},{"accessor":"Sex","name":"Sex","type":"character"},{"accessor":"Age","name":"Age","type":"numeric"},{"accessor":"Race","name":"Race","type":"character"},{"accessor":"PreinvasiveComponent","name":"PreinvasiveComponent","type":"character"},{"accessor":"LVI","name":"LVI","type":"character"},{"accessor":"PNI","name":"PNI","type":"character"},{"accessor":"LastFollowUpDate","name":"LastFollowUpDate","type":"Date"},{"accessor":"Death","name":"Death","type":"logical"},{"accessor":"Group","name":"Group","type":"character"},{"accessor":"Grade","name":"Grade","type":"character"},{"accessor":"TStage","name":"TStage","type":"character"},{"accessor":"AntiX_intensity","name":"AntiX_intensity","type":"numeric"},{"accessor":"AntiY_intensity","name":"AntiY_intensity","type":"numeric"},{"accessor":"LymphNodeMetastasis","name":"LymphNodeMetastasis","type":"character"},{"accessor":"Valid","name":"Valid","type":"logical"},{"accessor":"Smoker","name":"Smoker","type":"logical"},{"accessor":"Grade_Level","name":"Grade_Level","type":"character"},{"accessor":"SurgeryDate","name":"SurgeryDate","type":"Date"},{"accessor":"DeathTime","name":"DeathTime","type":"character"}],"resizable":true,"filterable":true,"searchable":true,"defaultPageSize":10,"showPageSizeOptions":true,"pageSizeOptions":[10,25,50,100],"paginationType":"numbers","showPageInfo":true,"minRows":1,"highlight":true,"outlined":true,"striped":true,"compact":true,"nowrap":true,"showSortable":true,"dataKey":"d702932b3c8d62ad0a262b06ff754689"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->








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
$ Age                  <dbl> 30, 32, 53, 57, 47, 58, 59, 54, 35, 27, 53, 55, …
$ Race                 <chr> "White", "White", "White", "Hispanic", "White", …
$ PreinvasiveComponent <chr> "Absent", "Absent", "Absent", "Absent", "Absent"…
$ LVI                  <chr> "Present", "Absent", "Absent", "Present", "Absen…
$ PNI                  <chr> "Absent", "Absent", "Absent", "Present", "Absent…
$ Death                <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRU…
$ Group                <chr> "Control", "Control", "Control", "Control", "Con…
$ Grade                <chr> "1", "1", "2", "1", "2", "2", "3", "1", "2", "1"…
$ TStage               <chr> "4", "4", "3", "3", "1", "3", "3", "3", "4", "4"…
$ AntiX_intensity      <dbl> 2, 2, 2, 2, 3, 1, 1, 3, 2, 3, 2, 3, 1, 3, 1, 2, …
$ AntiY_intensity      <dbl> 2, 2, 2, 3, 2, 1, 2, 3, 3, 1, 1, 2, 1, 3, 1, 2, …
$ LymphNodeMetastasis  <chr> "Present", "Absent", "Present", "Present", "Pres…
$ Valid                <lgl> TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRU…
$ Smoker               <lgl> TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TR…
$ Grade_Level          <chr> "moderate", "moderate", "high", "low", "high", "…
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
 4 Age                  dbl       1    0.4     50    25 49.5     73
 5 Race                 chr       1    0.4      8    NA NA       NA
 6 PreinvasiveComponent chr       1    0.4      3    NA NA       NA
 7 LVI                  chr       1    0.4      3    NA NA       NA
 8 PNI                  chr       1    0.4      3    NA NA       NA
 9 LastFollowUpDate     dat       1    0.4     13    NA NA       NA
10 Death                lgl       1    0.4      3     0  0.67     1
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
1                Smoker 250   1 0.4%   130   52%      0      -    0    -
2                 Valid 250   1 0.4%   116 46.4%      0      -    0    -
3                 Death 250   1 0.4%    83 33.2%      0      -    0    -
4                   Sex 250   1 0.4%     0     -      0      -    0    -
5  PreinvasiveComponent 250   1 0.4%     0     -      0      -    0    -
6                   LVI 250   1 0.4%     0     -      0      -    0    -
7                   PNI 250   1 0.4%     0     -      0      -    0    -
8                 Group 250   1 0.4%     0     -      0      -    0    -
9   LymphNodeMetastasis 250   1 0.4%     0     -      0      -    0    -
10                Grade 250   1 0.4%     0     -      0      -    0    -
11      AntiX_intensity 250   1 0.4%     0     -      0      -    0    -
12      AntiY_intensity 250   1 0.4%     0     -      0      -    0    -
13          Grade_Level 250   1 0.4%     0     -      0      -    0    -
14                 Race 250   1 0.4%     0     -      0      -    0    -
15     LastFollowUpDate 250   1 0.4%     0     -      0      -    0    -
16                  Age 250   1 0.4%     0     -      0      -    0    -
17          SurgeryDate 250   1 0.4%     0     -      0      -    0    -
18                 Name 250   1 0.4%     0     -      0      -    0    -
19            DeathTime 250   0    -     0     -      0      -    0    -
20               TStage 250   0    -     0     -      0      -    0    -
21                   ID 250   0    -     0     -      0      -    0    -
   qDistinct      type anomalous_percent
1          3   Logical             52.4%
2          3   Logical             46.8%
3          3   Logical             33.6%
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
14         8 Character              0.4%
15        13 Timestamp              0.4%
16        50   Numeric              0.4%
17       233 Timestamp              0.4%
18       250 Character              0.4%
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
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/xray 2-5.png)<!-- -->

```
[1] "Ignoring variable SurgeryDate: Unsupported type for visualization."
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/xray 2-6.png)<!-- -->

```
         Variable p_1 p_10 p_25 p_50 p_75 p_90 p_99
1 AntiX_intensity   1  1.8    2    2    3    3    3
2 AntiY_intensity   1    1    1    2    3    3    3
3             Age  25 30.8   37   49   61   70   73
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


## Data Dictionary


**Codes for generating data dictionary**.^[See [`childRmd/_08dataDictionary.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_08dataDictionary.Rmd) file for other codes]





---


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


<!-- ```{r descriptives, child = here::here('childRmd', '_11descriptives.Rmd')} -->
<!-- ``` -->


---

\newpage
\blandscape

<!-- ```{r crossTables, child = here::here('childRmd', '_12crossTables.Rmd')} -->
<!-- ``` -->

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


<!-- ```{r survival, child = here::here('childRmd', '_18survival.Rmd')} -->
<!-- ``` -->


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

<!-- ```{r footer, child = here::here('childRmd', '_23footer.Rmd')} -->
<!-- ``` -->

---

\pagebreak


<!-- **push all changes to GitHub repository**  -->

<!-- ```{r git update} -->
<!-- source(file = here::here("R", "force_git.R")) -->
<!-- ``` -->


<!-- --- -->


# References


