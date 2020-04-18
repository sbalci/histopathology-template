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
date: "2020-04-18"
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
always_allow_html: yes
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
    css: css/style.css
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

## Statistical Methods






----


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
# linewidth css
  pre:not([class]) {
    color: #333333;
    background-color: #cccccc;
  }
```


<style type="text/css">
# linewidth css
  pre:not([class]) {
    color: #333333;
    background-color: #cccccc;
  }
</style>



```r
# linewidth css
```



```css
pre.jamovitable{
  color:black;
  background-color: white;
  margin-bottom: 35px;  
}
```


<style type="text/css">
pre.jamovitable{
  color:black;
  background-color: white;
  margin-bottom: 35px;  
}
</style>




```r
jtable <- function(jobject, digits = 3) {
    snames <- sapply(jobject$columns, function(a) a$title)
    asDF <- jobject$asDF
    tnames <- unlist(lapply(names(asDF), function(n) snames[[n]]))
    names(asDF) <- tnames
    kableExtra::kable(asDF, "html", table.attr = "class=\"jmv-results-table-table\"", 
        row.names = F, digits = 3)
}
```











<div class="rmdnote">
<p>Block rmdnote</p>
</div>



<div class="rmdtip">
<p>Block rmdtip</p>
</div>




<div class="rmdwarning">
<p>Block warning</p>
</div>






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


## Clean and Recode Data


**Codes for clean and recode data**.^[See [`childRmd/_09cleanRecode.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_09cleanRecode.Rmd) file for other codes]


















questionr::irec()

questionr::iorder()

questionr::icut()


iris %>% mutate(sumVar = rowSums(.[1:4]))


iris %>% 
  mutate(sumVar = rowSums(select(., contains("Sepal")))) %>% 
  head 

iris %>% 
  mutate(sumVar = select(., contains("Sepal")) %>% rowSums()) %>% 
  head





































































iRenameColumn.R  

iSelectColumn.R  

























































```
<= 22 Low
>= 23 & <= 41 Average 
>=42 High
```













---

## Impute Missing Data


## impute

**Codes for missing data and impute**.^[See [`childRmd/_10impute.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_10impute.Rmd) file for other codes]


- Multiple imputation support in Finalfit  
https://www.datasurg.net/2019/09/25/multiple-imputation-support-in-finalfit/

- Missing data  
https://finalfit.org/articles/missing.html






### Missing Data


**Plot missing data**


```r
visdat::vis_miss(mydata)
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/visdat-1.png)<!-- -->









### impute continious









### impute categorical





### impute outlier










## transform


### min -max 




### skewness





### log





## binning








### optimal binning




### standardize









## data transformation report






## inspectdf





<!-- References: -->
<!-- Multiple imputation support in Finalfit -->
<!-- https://www.datasurg.net/2019/09/25/multiple-imputation-support-in-finalfit/ -->
<!-- Survival analysis with strata, clusters, frailties and competing risks in in Finalfit -->
<!-- https://www.datasurg.net/2019/09/12/survival-analysis-with-strata-clusters-frailties-and-competing-risks-in-in-finalfit/ -->


















---

\pagebreak


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
|&nbsp;&nbsp;&nbsp;Mean (SD) | 49.538 (14.160) |
|&nbsp;&nbsp;&nbsp;Range     | 25.000 - 73.000 |
|**Race**                    |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Asian     |    15 (6.0%)    |
|&nbsp;&nbsp;&nbsp;Bi-Racial |    5 (2.0%)     |
|&nbsp;&nbsp;&nbsp;Black     |   30 (12.0%)    |
|&nbsp;&nbsp;&nbsp;Hispanic  |   38 (15.3%)    |
|&nbsp;&nbsp;&nbsp;Native    |    2 (0.8%)     |
|&nbsp;&nbsp;&nbsp;Other     |    1 (0.4%)     |
|&nbsp;&nbsp;&nbsp;White     |   158 (63.5%)   |
|**PreinvasiveComponent**    |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   203 (81.5%)   |
|&nbsp;&nbsp;&nbsp;Present   |   46 (18.5%)    |
|**LVI**                     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   147 (59.0%)   |
|&nbsp;&nbsp;&nbsp;Present   |   102 (41.0%)   |
|**PNI**                     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   171 (68.7%)   |
|&nbsp;&nbsp;&nbsp;Present   |   78 (31.3%)    |
|**Death**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;FALSE     |   83 (33.3%)    |
|&nbsp;&nbsp;&nbsp;TRUE      |   166 (66.7%)   |
|**Group**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Control   |   118 (47.4%)   |
|&nbsp;&nbsp;&nbsp;Treatment |   131 (52.6%)   |
|**Grade**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;1         |   78 (31.3%)    |
|&nbsp;&nbsp;&nbsp;2         |   62 (24.9%)    |
|&nbsp;&nbsp;&nbsp;3         |   109 (43.8%)   |
|**TStage**                  |                 |
|&nbsp;&nbsp;&nbsp;1         |    24 (9.6%)    |
|&nbsp;&nbsp;&nbsp;2         |   43 (17.2%)    |
|&nbsp;&nbsp;&nbsp;3         |   65 (26.0%)    |
|&nbsp;&nbsp;&nbsp;4         |   118 (47.2%)   |
|**LymphNodeMetastasis**     |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;Absent    |   144 (57.8%)   |
|&nbsp;&nbsp;&nbsp;Present   |   105 (42.2%)   |
|**Valid**                   |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;FALSE     |   116 (46.6%)   |
|&nbsp;&nbsp;&nbsp;TRUE      |   133 (53.4%)   |
|**Smoker**                  |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;FALSE     |   130 (52.2%)   |
|&nbsp;&nbsp;&nbsp;TRUE      |   119 (47.8%)   |
|**Grade_Level**             |                 |
|&nbsp;&nbsp;&nbsp;N-Miss    |        1        |
|&nbsp;&nbsp;&nbsp;high      |   109 (43.8%)   |
|&nbsp;&nbsp;&nbsp;low       |   77 (30.9%)    |
|&nbsp;&nbsp;&nbsp;moderate  |   63 (25.3%)    |



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
  Age (mean (SD))                    49.54 (14.16)
  Race (%)                                        
     Asian                              15 ( 6.0) 
     Bi-Racial                           5 ( 2.0) 
     Black                              30 (12.0) 
     Hispanic                           38 (15.3) 
     Native                              2 ( 0.8) 
     Other                               1 ( 0.4) 
     White                             158 (63.5) 
  PreinvasiveComponent = Present (%)    46 (18.5) 
  LVI = Present (%)                    102 (41.0) 
  PNI = Present (%)                     78 (31.3) 
  Death = TRUE (%)                     166 (66.7) 
  Group = Treatment (%)                131 (52.6) 
  Grade (%)                                       
     1                                  78 (31.3) 
     2                                  62 (24.9) 
     3                                 109 (43.8) 
  TStage (%)                                      
     1                                  24 ( 9.6) 
     2                                  43 (17.2) 
     3                                  65 (26.0) 
     4                                 118 (47.2) 
  AntiX_intensity (mean (SD))         2.39 (0.66) 
  AntiY_intensity (mean (SD))         2.02 (0.80) 
  LymphNodeMetastasis = Present (%)    105 (42.2) 
  Valid = TRUE (%)                     133 (53.4) 
  Smoker = TRUE (%)                    119 (47.8) 
  Grade_Level (%)                                 
     high                              109 (43.8) 
     low                                77 (30.9) 
     moderate                           63 (25.3) 
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
 3 LVI                  chr       1    0.4      3    NA NA       NA
 4 PNI                  chr       1    0.4      3    NA NA       NA
 5 Death                lgl       1    0.4      3     0  0.67     1
 6 Group                chr       1    0.4      3    NA NA       NA
 7 Grade                chr       1    0.4      4    NA NA       NA
 8 TStage               chr       0    0        4    NA NA       NA
 9 AntiX_intensity      dbl       1    0.4      4     1  2.39     3
10 AntiY_intensity      dbl       1    0.4      4     1  2.02     3
11 LymphNodeMetastasis  chr       1    0.4      3    NA NA       NA
12 Valid                lgl       1    0.4      3     0  0.53     1
13 Smoker               lgl       1    0.4      3     0  0.48     1
14 Grade_Level          chr       1    0.4      4    NA NA       NA
15 DeathTime            chr       0    0        2    NA NA       NA
```



```r
mydata %>% explore::describe() %>% dplyr::filter(na > 0)
```

```
# A tibble: 18 x 8
   variable             type     na na_pct unique   min  mean   max
   <chr>                <chr> <int>  <dbl>  <int> <dbl> <dbl> <dbl>
 1 Name                 chr       1    0.4    250    NA NA       NA
 2 Sex                  chr       1    0.4      3    NA NA       NA
 3 Age                  dbl       1    0.4     50    25 49.5     73
 4 Race                 chr       1    0.4      8    NA NA       NA
 5 PreinvasiveComponent chr       1    0.4      3    NA NA       NA
 6 LVI                  chr       1    0.4      3    NA NA       NA
 7 PNI                  chr       1    0.4      3    NA NA       NA
 8 LastFollowUpDate     dat       1    0.4     13    NA NA       NA
 9 Death                lgl       1    0.4      3     0  0.67     1
10 Group                chr       1    0.4      3    NA NA       NA
11 Grade                chr       1    0.4      4    NA NA       NA
12 AntiX_intensity      dbl       1    0.4      4     1  2.39     3
13 AntiY_intensity      dbl       1    0.4      4     1  2.02     3
14 LymphNodeMetastasis  chr       1    0.4      3    NA NA       NA
15 Valid                lgl       1    0.4      3     0  0.53     1
16 Smoker               lgl       1    0.4      3     0  0.48     1
17 Grade_Level          chr       1    0.4      4    NA NA       NA
18 SurgeryDate          dat       1    0.4    233    NA NA       NA
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
Asian         15  6.0%      6.0%          
Bi-Racial      5  2.0%      2.0%          
Black         30  12.0%     12.0%         
Hispanic      38  15.2%     15.3%         
Native         2  0.8%      0.8%          
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
Absent                  203  81.2%     81.5%         
Present                  46  18.4%     18.5%         
NA                        1  0.4%      -             

\pagebreak

#### Descriptive Statistics LVI  


```r
mydata %>% janitor::tabyl(LVI) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



LVI          n  percent   valid_percent 
--------  ----  --------  --------------
Absent     147  58.8%     59.0%         
Present    102  40.8%     41.0%         
NA           1  0.4%      -             

\pagebreak

#### Descriptive Statistics PNI  


```r
mydata %>% janitor::tabyl(PNI) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



PNI          n  percent   valid_percent 
--------  ----  --------  --------------
Absent     171  68.4%     68.7%         
Present     78  31.2%     31.3%         
NA           1  0.4%      -             

\pagebreak

#### Descriptive Statistics Group  


```r
mydata %>% janitor::tabyl(Group) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Group          n  percent   valid_percent 
----------  ----  --------  --------------
Control      118  47.2%     47.4%         
Treatment    131  52.4%     52.6%         
NA             1  0.4%      -             

\pagebreak

#### Descriptive Statistics Grade  


```r
mydata %>% janitor::tabyl(Grade) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Grade      n  percent   valid_percent 
------  ----  --------  --------------
1         78  31.2%     31.3%         
2         62  24.8%     24.9%         
3        109  43.6%     43.8%         
NA         1  0.4%      -             

\pagebreak

#### Descriptive Statistics TStage  


```r
mydata %>% janitor::tabyl(TStage) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



TStage      n  percent 
-------  ----  --------
1          24  9.6%    
2          43  17.2%   
3          65  26.0%   
4         118  47.2%   

\pagebreak

#### Descriptive Statistics LymphNodeMetastasis  


```r
mydata %>% janitor::tabyl(LymphNodeMetastasis) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



LymphNodeMetastasis      n  percent   valid_percent 
--------------------  ----  --------  --------------
Absent                 144  57.6%     57.8%         
Present                105  42.0%     42.2%         
NA                       1  0.4%      -             

\pagebreak

#### Descriptive Statistics Grade_Level  


```r
mydata %>% janitor::tabyl(Grade_Level) %>% janitor::adorn_pct_formatting(rounding = "half up", 
    digits = 1) %>% knitr::kable()
```



Grade_Level      n  percent   valid_percent 
------------  ----  --------  --------------
high           109  43.6%     43.8%         
low             77  30.8%     30.9%         
moderate        63  25.2%     25.3%         
NA               1  0.4%      -             

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
 Absent  = 203 (81.2%)
 Present = 46 (18.4%)
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
5                  Race         Asian        15     6.0        6.0
6                  Race     Bi-Racial         5     2.0        8.0
7                  Race         Black        30    12.0       20.0
8                  Race      Hispanic        38    15.2       35.2
9                  Race            NA         1     0.4       35.6
10                 Race        Native         2     0.8       36.4
11                 Race         Other         1     0.4       36.8
12                 Race         White       158    63.2      100.0
13                 Race         TOTAL       250      NA         NA
14 PreinvasiveComponent        Absent       203    81.2       81.2
15 PreinvasiveComponent            NA         1     0.4       81.6
16 PreinvasiveComponent       Present        46    18.4      100.0
17 PreinvasiveComponent         TOTAL       250      NA         NA
18                  LVI        Absent       147    58.8       58.8
19                  LVI            NA         1     0.4       59.2
20                  LVI       Present       102    40.8      100.0
21                  LVI         TOTAL       250      NA         NA
22                  PNI        Absent       171    68.4       68.4
23                  PNI            NA         1     0.4       68.8
24                  PNI       Present        78    31.2      100.0
25                  PNI         TOTAL       250      NA         NA
26                Group       Control       118    47.2       47.2
27                Group            NA         1     0.4       47.6
28                Group     Treatment       131    52.4      100.0
29                Group         TOTAL       250      NA         NA
30                Grade             1        78    31.2       31.2
31                Grade             2        62    24.8       56.0
32                Grade             3       109    43.6       99.6
33                Grade            NA         1     0.4      100.0
34                Grade         TOTAL       250      NA         NA
35               TStage             1        24     9.6        9.6
36               TStage             2        43    17.2       26.8
37               TStage             3        65    26.0       52.8
38               TStage             4       118    47.2      100.0
39               TStage         TOTAL       250      NA         NA
40  LymphNodeMetastasis        Absent       144    57.6       57.6
41  LymphNodeMetastasis            NA         1     0.4       58.0
42  LymphNodeMetastasis       Present       105    42.0      100.0
43  LymphNodeMetastasis         TOTAL       250      NA         NA
44          Grade_Level          high       109    43.6       43.6
45          Grade_Level           low        77    30.8       74.4
46          Grade_Level      moderate        63    25.2       99.6
47          Grade_Level            NA         1     0.4      100.0
48          Grade_Level         TOTAL       250      NA         NA
49            DeathTime MoreThan1Year       101    40.4       40.4
50            DeathTime   Within1Year       149    59.6      100.0
51            DeathTime         TOTAL       250      NA         NA
52      AntiX_intensity             1        25    10.0       10.0
53      AntiX_intensity             2       102    40.8       50.8
54      AntiX_intensity             3       122    48.8       99.6
55      AntiX_intensity            NA         1     0.4      100.0
56      AntiX_intensity         TOTAL       250      NA         NA
57      AntiY_intensity             1        77    30.8       30.8
58      AntiY_intensity             2        91    36.4       67.2
59      AntiY_intensity             3        81    32.4       99.6
60      AntiY_intensity            NA         1     0.4      100.0
61      AntiY_intensity         TOTAL       250      NA         NA
```




```r
inspectdf::inspect_cat(mydata)
```

```
# A tibble: 16 x 5
   col_name               cnt common      common_pcnt levels            
   <chr>                <int> <chr>             <dbl> <named list>      
 1 Death                    3 TRUE               66.4 <tibble [3 × 3]>  
 2 DeathTime                2 Within1Year        59.6 <tibble [2 × 3]>  
 3 Grade                    4 3                  43.6 <tibble [4 × 3]>  
 4 Grade_Level              4 high               43.6 <tibble [4 × 3]>  
 5 Group                    3 Treatment          52.4 <tibble [3 × 3]>  
 6 ID                     250 001                 0.4 <tibble [250 × 3]>
 7 LVI                      3 Absent             58.8 <tibble [3 × 3]>  
 8 LymphNodeMetastasis      3 Absent             57.6 <tibble [3 × 3]>  
 9 Name                   250 Aceyn               0.4 <tibble [250 × 3]>
10 PNI                      3 Absent             68.4 <tibble [3 × 3]>  
11 PreinvasiveComponent     3 Absent             81.2 <tibble [3 × 3]>  
12 Race                     8 White              63.2 <tibble [8 × 3]>  
13 Sex                      3 Male               50.8 <tibble [3 × 3]>  
14 Smoker                   3 FALSE              52   <tibble [3 × 3]>  
15 TStage                   4 4                  47.2 <tibble [4 × 3]>  
16 Valid                    3 TRUE               53.2 <tibble [3 × 3]>  
```

```r
inspectdf::inspect_cat(mydata)$levels$Group
```

```
# A tibble: 3 x 3
  value      prop   cnt
  <chr>     <dbl> <int>
1 Treatment 0.524   131
2 Control   0.472   118
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
 ────────────────────────────────── 
                          Age       
 ────────────────────────────────── 
   N                          249   
   Missing                      1   
   Mean                      49.5   
   Median                    49.0   
   Mode                      72.0   
   Standard deviation        14.2   
   Variance                   200   
   Minimum                   25.0   
   Maximum                   73.0   
   Skewness               0.00389   
   Std. error skewness      0.154   
   Kurtosis                 -1.15   
   Std. error kurtosis      0.307   
   25th percentile           37.0   
   50th percentile           49.0   
   75th percentile           61.0   
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
   Mean                              2.39   
   Median                            2.00   
   Mode                              3.00   
   Standard deviation               0.664   
   Variance                         0.440   
   Minimum                           1.00   
   Maximum                           3.00   
   Skewness                        -0.631   
   Std. error skewness              0.154   
   Kurtosis                        -0.640   
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
   Standard deviation               0.798   
   Variance                         0.637   
   Minimum                           1.00   
   Maximum                           3.00   
   Skewness                       -0.0289   
   Std. error skewness              0.154   
   Kurtosis                         -1.43   
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
  Age (mean (SD))             49.54 (14.16)
  AntiX_intensity (mean (SD))  2.39 (0.66) 
  AntiY_intensity (mean (SD))  2.02 (0.80) 
```

```r
print(tab$ContTable, nonnormal = c("Anti-X-intensity"))
```

```
                             
                              Overall      
  n                           250          
  Age (mean (SD))             49.54 (14.16)
  AntiX_intensity (mean (SD))  2.39 (0.66) 
  AntiY_intensity (mean (SD))  2.02 (0.80) 
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
q05|q95  = 28 | 72
q25|q75  = 37 | 61
median   = 49
mean     = 49.53815
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
1 Age              25    37     49 49.5     61    73 14.2       0.4 <tibble [12…
2 AntiX_intens…     1     2      2  2.39     3     3  0.664     0.4 <tibble [12…
3 AntiY_intens…     1     1      2  2.02     3     3  0.798     0.4 <tibble [12…
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
 3 [26, 28)   0.0281
 4 [28, 30)   0.0361
 5 [30, 32)   0.0361
 6 [32, 34)   0.0602
 7 [34, 36)   0.0482
 8 [36, 38)   0.0241
 9 [38, 40)   0.0161
10 [40, 42)   0.0602
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
2   Age  PreinvasiveComponent:Absent 203    0     0  203      0      0        0
3   Age PreinvasiveComponent:Present  46    0     0   45      0      0        1
4   Age      PreinvasiveComponent:NA   0    0     0    0      0      0        0
  Per_of_Missing   sum min  max  mean median    SD   CV  IQR Skewness Kurtosis
1           0.40 12335  25   73 49.54     49 14.16 0.29 24.0     0.00    -1.16
2           0.00 10117  25   73 49.84     51 14.34 0.29 23.5    -0.02    -1.20
3           2.17  2170  25   72 48.22     49 13.55 0.28 22.0     0.08    -0.98
4            NaN     0 Inf -Inf   NaN     NA    NA   NA   NA      NaN      NaN
  0%  10%  20%  30%  40% 50%  60%  70% 80%  90% 100% LB.25% UB.75% nOutliers
1 25 30.8 34.0 40.4 45.0  49 54.0 59.0  64 70.0   73   1.00  97.00         0
2 25 31.0 34.0 40.6 45.0  51 54.0 59.0  65 70.8   73   2.25  96.25         0
3 25 30.8 34.8 40.2 43.6  49 51.8 56.8  59 68.6   72   3.00  91.00         0
4 NA   NA   NA   NA   NA  NA   NA   NA  NA   NA   NA     NA     NA         0
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
# dependent <- c('dependent1', 'dependent2' )

# explanatory <- c('explanatory1', 'explanatory2' )

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
Sex                               Female        104 (51.2)     17 (37.8)    121 (48.8)  0.102 
                                  Male           99 (48.8)     28 (62.2)    127 (51.2)        
Age                               Mean (SD)    49.8 (14.3)   48.2 (13.6)   49.5 (14.2)  0.492 
Grade                             1              68 (33.7)      9 (19.6)     77 (31.0)  0.100 
                                  2              46 (22.8)     16 (34.8)     62 (25.0)        
                                  3              88 (43.6)     21 (45.7)    109 (44.0)        
TStage                            1               18 (8.9)      6 (13.0)      24 (9.6)  0.117 
                                  2              38 (18.7)       4 (8.7)     42 (16.9)        
                                  3              48 (23.6)     17 (37.0)     65 (26.1)        
                                  4              99 (48.8)     19 (41.3)    118 (47.4)        

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


### chi-square posthoc pairwise 

#### rmngb















#### RVAideMemoire










\newpage
\blandscape







\elandscape







\elandscape



## Plots

**Codes for generating Plots**.^[See [`childRmd/_13plots.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_13plots.Rmd) file for other codes]

## Categorical Variables






































































































































<!-- ## Continious Variables -->


<!-- ```{r} -->
<!-- mydataContinious <- mydata %>%  -->
<!--     select( -->
<!--         Yas, -->
<!--         TumorCapi -->
<!--     ) -->
<!-- ``` -->



<!-- ```{r include=FALSE} -->
<!-- source(here::here("R", "gc_plot_cont.R")) -->
<!-- ``` -->



<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeContiniousPlots.Rmd')} -->
<!-- ``` -->


### Plots

<!-- ## Categorical Variables -->


<!-- ```{r} -->
<!-- dependent <- c("Lauren", -->
<!--                "CD44", -->
<!--                "Her2", -->
<!--                "MMR2", -->
<!--                  "TumorPDL1gr1", -->
<!--                  "TumorPDL1gr5", -->
<!--                  "inflPDL1gr1", -->
<!--                  "inflPDL1gr5" -->
<!--                  ) -->

<!-- explanatory <- c("Cinsiyet", -->
<!--                  # "TumorYerlesimi", -->
<!--                  "Lauren", -->
<!--                  "Grade", -->
<!--                  "T_stage", -->
<!--                  "N_stage", -->
<!--                  "M_stage", -->
<!--                  "CD44", -->
<!--                  "Her2", -->
<!--                  "MMR2", -->
<!--                  "TumorPDL1gr1", -->
<!--                  "TumorPDL1gr5", -->
<!--                  "inflPDL1gr1", -->
<!--                  "inflPDL1gr5", -->
<!--                  "LVI", -->
<!--                  "PNI", -->
<!--                  "LenfNoduMetastazi") -->
<!-- ``` -->




<!-- ```{r} -->
<!-- mydataCategorical <- mydata %>%  -->
<!--     select(-Yas, -->
<!--            -TumorCapi, -->
<!--            -CerrahiTarih, -->
<!--            -genel_sagkalim, -->
<!--            -SonTarih, -->
<!--            -TNM -->
<!--     ) -->
<!-- ``` -->



<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- explanatory[1] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[2] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->

<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->



<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[3] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->



<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[4] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[5] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[6] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[7] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[8] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[9] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[10] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[11] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[12] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[13] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[14] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->

<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[15] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r include=FALSE} -->
<!-- mydataCategorical_variable <- NA -->
<!-- dependent2 <- NA -->
<!-- mydataCategorical_variable <- explanatory[16] -->
<!-- dependent2 <- dependent[!dependent %in% mydataCategorical_variable] -->
<!-- source(here::here("R", "gc_plot_cat.R")) -->
<!-- ``` -->


<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeCategoricalPlots.Rmd')} -->
<!-- ``` -->




## Continious Variables






<!-- ```{r include=FALSE} -->
<!-- source(here::here("R", "gc_plot_cont.R")) -->
<!-- ``` -->



<!-- ```{r generatedCode, child = here::here('childRmd', 'generatedCodeContiniousPlots.Rmd')} -->
<!-- ``` -->


<!-- ```{r} -->
<!-- mydata %>%  -->
<!--     select(starts_with("Slide")) %>%  -->
<!--     pivot_longer(cols = everything()) %>%  -->
<!-- ggstatsplot::ggwithinstats(data = ., x = name, y = value) -->
<!-- ``` -->












<!-- ```{r webgl=TRUE, results='hide'} -->
<!-- # library -->
<!-- library(rgl) -->

<!-- # This is to output a rgl plot in a rmarkdown document. Note that you must add webgl=TRUE, results='hide' in the chunck header -->
<!-- library(knitr) -->
<!-- knit_hooks$set(webgl = hook_webgl) -->

<!-- # Data: the iris data is provided by R -->
<!-- # data <- iris -->

<!-- # Add a new column with color -->
<!-- # mycolors <- c('royalblue1', 'darkcyan', 'oldlace', 'red', 'orange') -->
<!-- # data$color <- mycolors[ as.numeric(dxchanges$Slide1_infiltrative) ] -->

<!-- # Plot -->
<!-- par(mar=c(0,0,0,0)) -->
<!-- plot3d(  -->
<!--   x=dxchanges$Slide1_infiltrative, y=dxchanges$Slide2_Medium, z=dxchanges$Slide3_Demarcated,  -->
<!--   col = dxchanges$Slide1_infiltrative, -->
<!--   type = 's',  -->
<!--   radius = .1, -->
<!--   xlab="Slide1_infiltrative", ylab="Sepal Width", zlab="Petal Length") -->

<!-- writeWebGL(filename = here::here("HtmlWidget/3dscatter2.html") ,  width=600, height=600) -->

<!-- ``` -->




<!-- ```{r} -->
<!-- library(ggparallel) -->
<!-- ggparallel(vars = list("Slide1_infiltrative", "Slide2_Medium", "Slide3_Demarcated"),  -->
<!--            data = dxchanges) -->

<!-- ``` -->















### Interactive graphics {#interactive}

***

R allows to build any type of [interactive graphic](https://www.r-graph-gallery.com/interactive-charts/). My favourite library is [plotly](https://www.r-graph-gallery.com/get-the-best-from-ggplotly/) that will turn any of your ggplot2 graphic interactive in one supplementary line of code. Try to hover points, to select a zone, to click on the legend.
<br><br>


```r
library(ggplot2)
library(plotly)
library(gapminder)

p <- gapminder %>% filter(year == 1977) %>% ggplot(aes(gdpPercap, lifeExp, size = pop, 
    color = continent)) + geom_point() + scale_x_log10() + theme_bw()

ggplotly(p)
```



---


```r
scales::show_col(colours(), cex_label = 0.35)
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/unnamed-chunk-54-1.png)<!-- -->



```r
embedgist <- gistr::gist("https://gist.github.com/sbalci/834ebc154c0ffcb7d5899c42dd3ab75e") %>% 
    gistr::embed()
```


<script src="https://gist.github.com/sbalci/834ebc154c0ffcb7d5899c42dd3ab75e.js"></script>


<script src="https://gist.github.com/sbalci/834ebc154c0ffcb7d5899c42dd3ab75e.js"></script>



---

### Alluvial


```r
# https://stackoverflow.com/questions/43053375/weighted-sankey-alluvial-diagram-for-visualizing-discrete-and-continuous-panel/48133004

library(tidyr)
library(dplyr)
library(alluvial)
library(ggplot2)
library(forcats)

set.seed(42)
individual <- rep(LETTERS[1:10], each = 2)
timeperiod <- paste0("time_", rep(1:2, 10))
discretechoice <- factor(paste0("choice_", sample(letters[1:3], 20, replace = T)))
continuouschoice <- ceiling(runif(20, 0, 100))
d <- data.frame(individual, timeperiod, discretechoice, continuouschoice)
```


```r
# stacked bar diagram of discrete choice by individual
g <- ggplot(data = d, aes(timeperiod, fill = fct_rev(discretechoice)))
g + geom_bar(position = "stack") + guides(fill = guide_legend(title = NULL))
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/unnamed-chunk-56-1.png)<!-- -->



```r
# alluvial diagram of discrete choice by individual
d_alluvial <- d %>% select(individual, timeperiod, discretechoice) %>% spread(timeperiod, 
    discretechoice) %>% group_by(time_1, time_2) %>% summarize(count = n()) %>% ungroup()
```

```
Error in UseMethod("ungroup"): no applicable method for 'ungroup' applied to an object of class "list"
```

```r
alluvial(select(d_alluvial, -count), freq = d_alluvial$count)
```

```
Error in log_select(.data, .fun = dplyr::select, .funname = "select", : object 'd_alluvial' not found
```



```r
# stacked bar diagram of discrete choice, weighting by continuous choice
g + geom_bar(position = "stack", aes(weight = continuouschoice))
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/unnamed-chunk-58-1.png)<!-- -->



```r
library(ggalluvial)
ggplot(data = d, aes(x = timeperiod, stratum = discretechoice, alluvium = individual, 
    y = continuouschoice)) + geom_stratum(aes(fill = discretechoice)) + geom_flow()
```

![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/unnamed-chunk-59-1.png)<!-- -->









  

```r
CD44changes <- mydata %>% dplyr::select(TumorCD44, TomurcukCD44, PeritumoralTomurcukGr4) %>% 
    dplyr::filter(complete.cases(.)) %>% dplyr::group_by(TumorCD44, TomurcukCD44, 
    PeritumoralTomurcukGr4) %>% dplyr::tally()
```

```
Error: Can't subset columns that don't exist.
[31mx[39m The column `TumorCD44` doesn't exist.
```

```r
library(ggalluvial)

ggplot(data = CD44changes, aes(axis1 = TumorCD44, axis2 = TomurcukCD44, y = n)) + 
    scale_x_discrete(limits = c("TumorCD44", "TomurcukCD44"), expand = c(0.1, 0.05)) + 
    xlab("Tumor Tomurcuk") + geom_alluvium(aes(fill = PeritumoralTomurcukGr4, colour = PeritumoralTomurcukGr4)) + 
    geom_stratum(alpha = 0.5) + geom_text(stat = "stratum", infer.label = TRUE) + 
    # geom_text(stat = 'alluvium', infer.label = TRUE) +
theme_minimal() + ggtitle("Changes in CD44")
```

```
Error in ggplot(data = CD44changes, aes(axis1 = TumorCD44, axis2 = TomurcukCD44, : object 'CD44changes' not found
```

  








**Codes for generating paired tests**.^[See [`childRmd/_14pairedTests.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_14pairedTests.Rmd) file for other codes]


<!-- https://cran.r-project.org/web/packages/arsenal/vignettes/paired.html -->






















**Codes for generating hypothesis tests**.^[See [`childRmd/_15hypothesisTests.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_15hypothesisTests.Rmd) file for other codes]


# Hypothesis Tests

## Tests of Normality









## jamovi




```r
mytable <- jmv::ttestIS(formula = HindexCTLA4 ~ PeritumoralTomurcukGr4, data = mydata, 
    vars = HindexCTLA4, students = FALSE, mann = TRUE, norm = TRUE, meanDiff = TRUE, 
    desc = TRUE, plots = TRUE)
```

```
Error: Argument 'vars' contains 'HindexCTLA4' which is not present in the dataset
```





```r
cat("<pre class='jamovitable'>")
```

<pre class='jamovitable'>

```r
print(jtable(mytable$ttest))
```

```
Error in lapply(X = X, FUN = FUN, ...): object 'mytable' not found
```

```r
cat("</pre>")
```

</pre>




## Categorical







### Chi-Square Cramer Association Predictive Power












## Continious




https://stat.ethz.ch/R-manual/R-devel/library/stats/html/t.test.html

t.test(mtcars$mpg ~ mtcars$am) %>% 
  report::report()


report(t.test(iris$Sepal.Length, iris$Petal.Length))



## Odds








# Frequently Used Statistical Tests By Pathologists

Frequently Used Statistical Tests^[Statistical Literacy Among Academic Pathologists: A Survey Study to Gauge Knowledge of Frequently Used Statistical Tests Among Trainees and Faculty. Archives of Pathology & Laboratory Medicine: February 2017, Vol. 141, No. 2, pp. 279-287. https://doi.org/10.5858/arpa.2016-0200-OA] by [@Schmidt2017]

- Student t test
- Regression/ANOVA
- Chi-square test
- Mann-Whitney test (rank sum)
- Fisher exact test
- Survival analysis
    - Kaplan-Meier/log-rank
    - Cox regression
- Multiple comparison adjustment
    - Tukey
    - Bonferroni
    - Newman-Keuls
- Kappa Statistic
- ROC analysis
- Logistic regression
- Spearman rank correlation
- Kruskal-Wallis test
- Pearson correlation statistic
- Normality test
- McNemar test

# Consider Adding:
- https://cran.r-project.org/web/packages/sm/sm.pdf
- https://cran.r-project.org/web/packages/Rfit/Rfit.pdf


\newpage
\blandscape




**Codes for ROC**.^[See [`childRmd/_16ROC.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_16ROC.Rmd) file for other codes]


# ROC











































**Codes for Decision Tree**.^[See [`childRmd/_17decisionTree.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_17decisionTree.Rmd)]


# Decision Tree







**Explore**


```r
explore::explore(mydata)
```








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
  FALSE  83   0
  TRUE    0 166
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
 [1]  4.5+  7.8   7.1   7.9  10.6   6.9+  8.4+ 11.0   3.5   7.6   8.4   6.0 
[13]   NA   9.5  11.2  11.7   9.2   7.6?  4.1   4.7   9.7+  8.3+  6.0+  5.5+
[25]  6.4  11.4   3.8+ 10.2   3.0   6.4  11.3   6.5+  9.7   6.7   3.3+ 11.2+
[37]  7.8   7.0   6.3  10.2   7.0  11.2   9.7+  6.8   3.1   3.6   7.8   9.5+
[49]  6.0  10.4+ 11.2+  3.3+  7.4   9.2+  9.9  11.2+ 10.0   5.4   9.5   5.4 
[61]  5.9   8.4   4.1   9.2   7.3+  6.6   7.0+  8.6+  4.0   4.1  10.7   4.7 
[73]  6.9   6.6   5.3   8.0   9.3   8.4+  8.6+  8.8 
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

knitr::kable(tUni[, 1:4], row.names = FALSE, align = c("l", "l", "r", "r", "r", "r"))
```



Dependent: Surv(OverallTime, Outcome)                      all            HR (univariable)
--------------------------------------  --------  ------------  --------------------------
LVI                                     Absent     147 (100.0)                           -
                                        Present    102 (100.0)   1.59 (1.15-2.20, p=0.005)


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

When LVI is Present, there is 1.59 (1.15-2.20, p=0.005) times risk than when LVI is Absent.

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

$ When LVI is Present, there is 1.59 (1.15-2.20, p=0.005) times risk than when LVI is Absent. $

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
LVI=Absent  144    100   22.0    14.3    31.0
LVI=Present 102     64   10.5     9.9    13.8
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

When LVI=Absent, median survival is 22 [14.3 - 31, 95% CI] months., When LVI=Present, median survival is 10.5 [9.9 - 13.8, 95% CI] months.

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

When LVI=Absent, median survival is 22 [14.3 - 31, 95% CI] months., When LVI=Present, median survival is 10.5 [9.9 - 13.8, 95% CI] months.

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
   12     75      52    0.617  0.0421        0.539        0.705
   36     19      35    0.252  0.0452        0.177        0.358

                LVI=Present 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
   12     23      49    0.383  0.0566       0.2870        0.512
   36      4      12    0.134  0.0488       0.0657        0.274
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

When LVI=Absent, 12 month survival is 62% [54%-70.5%, 95% CI]., When LVI=Absent, 36 month survival is 25% [18%-35.8%, 95% CI]., When LVI=Present, 12 month survival is 38% [29%-51.2%, 95% CI]., When LVI=Present, 36 month survival is 13% [7%-27.4%, 95% CI].

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

When LVI=Absent, 12 month survival is 62% [54%-70.5%, 95% CI]., When LVI=Absent, 36 month survival is 25% [18%-35.8%, 95% CI]., When LVI=Present, 12 month survival is 38% [29%-51.2%, 95% CI]., When LVI=Present, 36 month survival is 13% [7%-27.4%, 95% CI].

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
tUni <- mydata %>% finalfit(dependentUni, explanatoryUni, metrics = TRUE)

knitr::kable(tUni[, 1:4], row.names = FALSE, align = c("l", "l", "r", "r", "r", "r"))
```

```
Error in tUni[, 1:4]: incorrect number of dimensions
```

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
[1] "When  is c(\"Absent\", \"Present\"), there is  times risk than when  is c(\"LVI\", \"\")."
```

\pagebreak

**Median Survival**


```r
km_fit <- survfit(Surv(OverallTime, Outcome) ~ LVI, data = mydata)
km_fit
```

```
Call: survfit(formula = Surv(OverallTime, Outcome) ~ LVI, data = mydata)

   4 observations deleted due to missingness 
              n events median 0.95LCL 0.95UCL
LVI=Absent  144    100   22.0    14.3    31.0
LVI=Present 102     64   10.5     9.9    13.8
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
When, LVI, LVI=Absent, median survival is 22 [14.3 - 31, 95% CI] months.
When, LVI, LVI=Present, median survival is 10.5 [9.9 - 13.8, 95% CI] months.
```

**1-3-5-yr survival**


```r
summary(km_fit, times = c(12, 36, 60))
```

```
Call: survfit(formula = Surv(OverallTime, Outcome) ~ LVI, data = mydata)

4 observations deleted due to missingness 
                LVI=Absent 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
   12     75      52    0.617  0.0421        0.539        0.705
   36     19      35    0.252  0.0452        0.177        0.358

                LVI=Present 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
   12     23      49    0.383  0.0566       0.2870        0.512
   36      4      12    0.134  0.0488       0.0657        0.274
```

```r
km_fit_summary <- summary(km_fit, times = c(12, 36, 60))

km_fit_df <- as.data.frame(km_fit_summary[c("strata", "time", "n.risk", "n.event", 
    "surv", "std.err", "lower", "upper")])

km_fit_df
```

```
       strata time n.risk n.event      surv    std.err      lower     upper
1  LVI=Absent   12     75      52 0.6165782 0.04211739 0.53931696 0.7049078
2  LVI=Absent   36     19      35 0.2520087 0.04515881 0.17737163 0.3580528
3 LVI=Present   12     23      49 0.3833784 0.05662684 0.28701265 0.5120993
4 LVI=Present   36      4      12 0.1340646 0.04881983 0.06566707 0.2737036
```

```r
km_fit_definition <- km_fit_df %>% dplyr::mutate(description = glue::glue("When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI].")) %>% 
    dplyr::select(description) %>% dplyr::pull()

km_fit_definition
```

```
When LVI=Absent, 12 month survival is 62% [54%-70.5%, 95% CI].
When LVI=Absent, 36 month survival is 25% [18%-35.8%, 95% CI].
When LVI=Present, 12 month survival is 38% [29%-51.2%, 95% CI].
When LVI=Present, 36 month survival is 13% [7%-27.4%, 95% CI].
```

\pagebreak


```r
summary(km_fit)$table
```

```
            records n.max n.start events   *rmean *se(rmean) median 0.95LCL
LVI=Absent      144   144     144    100 24.71341   1.571856   22.0    14.3
LVI=Present     102   102     102     64 17.48672   1.904576   10.5     9.9
            0.95UCL
LVI=Absent     31.0
LVI=Present    13.8
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
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 24.7 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 22.0 </td>
   <td style="text-align:right;"> 14.3 </td>
   <td style="text-align:right;"> 31.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LVI=Present </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:right;"> 17.5 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 10.5 </td>
   <td style="text-align:right;"> 9.9 </td>
   <td style="text-align:right;"> 13.8 </td>
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

   4 observations deleted due to missingness 
              n events median 0.95LCL 0.95UCL
LVI=Absent  144    100   22.0    14.3    31.0
LVI=Present 102     64   10.5     9.9    13.8
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

   4 observations deleted due to missingness 
              n events median 0.95LCL 0.95UCL
LVI=Absent  144    100   22.0    14.3    31.0
LVI=Present 102     64   10.5     9.9    13.8
```




### Multivariate Analysis Survival






explanatory = c("age.factor", "sex.factor", 
  "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  hr_plot(dependent, explanatory)
  
  
  


<!-- # parsnip -->














```r
library(survival)
library(survminer)
library(finalfit)
mb_followup %>%
  finalfit::surv_plot('Surv(OverallTime, Outcome)', 'Operation', 
  xlab='Time (months)', pval=TRUE, legend = 'none',
  # pval.coord
    break.time.by = 12, xlim = c(0,60), ylim = c(0.8, 1)

# legend.labs = c('a','b')

)
```

**Univariate Cox-Regression**


```r
explanatoryUni <- "Operation"
dependentUni <- "Surv(OverallTime, Outcome)"
tUni <- mb_followup %>% finalfit(dependentUni, explanatoryUni)

knitr::kable(tUni[, 1:4], row.names = FALSE, align = c("l", "l", "r", "r", "r", "r"))
```

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


\pagebreak

**Median Survival**



```r
km_fit <- survfit(Surv(OverallTime, Outcome) ~ Operation, data = mb_followup)

# km_fit

# summary(km_fit)

km_fit_median_df <- summary(km_fit)
km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>% janitor::clean_names(dat = ., 
    case = "snake") %>% tibble::rownames_to_column(.data = ., var = "Derece")

km_fit_median_df

# km_fit_median_df %>% knitr::kable(format = 'latex') %>%
# kableExtra::kable_styling(latex_options='scale_down')

km_fit_median_definition <- km_fit_median_df %>% dplyr::mutate(description = glue::glue("When, Derece, {Derece}, median survival is {median} [{x0_95lcl} - {x0_95ucl}, 95% CI] months.")) %>% 
    dplyr::mutate(description = gsub(pattern = "thefactor=", replacement = " is ", 
        x = description)) %>% dplyr::select(description) %>% dplyr::pull()

# km_fit_median_definition
```

**1-3-5-yr survival**


```r
summary(km_fit, times = c(12, 36, 60))

km_fit_summary <- summary(km_fit, times = c(12, 36, 60))

km_fit_df <- as.data.frame(km_fit_summary[c("strata", "time", "n.risk", "n.event", 
    "surv", "std.err", "lower", "upper")])

km_fit_df %>% knitr::kable(format = "latex") %>% kableExtra::kable_styling(latex_options = "scale_down")





km_fit_definition <- km_fit_df %>% dplyr::mutate(description = glue::glue("When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI].")) %>% 
    dplyr::select(description) %>% dplyr::pull()

km_fit_definition
```

\pagebreak

**Pairwise Comparisons**


```r
survminer::pairwise_survdiff(formula = Surv(OverallTime, Outcome) ~ Operation, data = mb_followup, 
    p.adjust.method = "BH")
```

\pagebreak


---


```r
library(gt)
library(gtsummary)

library(survival)
fit1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
tbl_strata_ex1 <- tbl_survival(fit1, times = c(12, 24), label = "{time} Months")

fit2 <- survfit(Surv(ttdeath, death) ~ 1, trial)
tbl_nostrata_ex2 <- tbl_survival(fit2, probs = c(0.1, 0.2, 0.5), header_estimate = "**Months**")
```









---

# Interactive Survival Analysis



**Codes for generating Survival Analysis**.^[See [`childRmd/_18survival.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_18survival.Rmd) file for other codes]

**Codes for generating Shiny Survival Analysis**.^[See [`childRmd/_19shinySurvival.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_19shinySurvival.Rmd) file for other codes]















---

\elandscape


# Correlation



## Correlation Analysis

**Codes for generating correlation analysis**.^[See [`childRmd/_20correlation.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_20correlation.Rmd) file for other codes]











  
  

  
            
            









https://stat.ethz.ch/R-manual/R-patched/library/stats/html/cor.test.html

  






















```r
https://neuropsychology.github.io/psycho.R/2018/05/20/correlation.html

devtools::install_github("neuropsychology/psycho.R")  # Install the newest version

remove.packages("psycho")
renv::install("neuropsychology/psycho.R@0.4.0")
# devtools::install_github("neuropsychology/psycho.R@0.4.0")

library(psycho)
<!-- library(tidyverse) -->

cor <- psycho::affective %>% 
  correlation()

summary(cor)


plot(cor)


print(cor)
```

  






```r
summary(cor) %>% 
  knitr::kable(format = "latex") %>% 
  kableExtra::kable_styling(latex_options="scale_down")


ggplot(mydata, aes(x = tx_zamani_verici_yasi, y = trombosit)) +
  geom_point() + 
  geom_smooth(method = lm, size = 1)
```


<!-- References: -->
<!-- Automated Interpretation of Metrics and Effect Sizes -->
<!-- https://easystats.github.io/report/articles/interpret_metrics.html -->












# Models



**Codes used in models**^[See [`childRmd/_21models.Rmd`](https://github.com/sbalci/histopathology-template/blob/master/childRmd/_21models.Rmd) file for other codes]  
**Use [these descriptions](https://easystats.github.io/report/articles/supporting_new_models.html) to add autoreporting of new models**  


### Linear Model

**generate automatic reporting of model via [easystats/report](https://easystats.github.io/report/) 📦**


```r
library(report)
model <- lm(Sepal.Length ~ Species, data = iris)
report::report(model)
```

```
We fitted a linear model (estimated using OLS) to predict Sepal.Length with Species (formula = Sepal.Length ~ Species). Standardized parameters were obtained by fitting the model on a standardized version of the dataset. Effect sizes were labelled following Funder's (2019) recommendations.

The model explains a significant and substantial proportion of variance (R2 = 0.62, F(2, 147) = 119.26, p < .001, adj. R2 = 0.61). The model's intercept, corresponding to Sepal.Length = 0 and Species = setosa, is at 5.01 (SE = 0.07, 95% CI [4.86, 5.15], p < .001). Within this model:

  - The effect of Speciesversicolor is positive and can be considered as very large and significant (beta = 1.12, SE = 0.12, 95% CI [0.88, 1.37], std. beta = 1.12, p < .001).
  - The effect of Speciesvirginica is positive and can be considered as very large and significant (beta = 1.91, SE = 0.12, 95% CI [1.66, 2.16], std. beta = 1.91, p < .001).
```

**Table report for a linear model**




```r
model <- lm(Sepal.Length ~ Petal.Length + Species, data=iris)
r <- report(model)
to_text(r)
to_table(r)
```



### General Linear Models (GLMs)

https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html

```r
model <- glm(vs ~ mpg + cyl, data=mtcars, family="binomial")
r <- report(model)

to_fulltext(r)
to_fulltable(r)
```






```r
Where a multivariable model contains a subset of the variables specified in the full univariable set, this can be specified.

explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
dependent = 'mort_5yr'
colon_s %>%
  summarizer(dependent, explanatory, explanatory.multi)

Random effects.

e.g. lme4::glmer(dependent ~ explanatory + (1 | random_effect), family="binomial")

explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
random.effect = "hospital"
dependent = 'mort_5yr'
colon_s %>%
  summarizer(dependent, explanatory, explanatory.multi, random.effect)

metrics=TRUE provides common model metrics.

colon_s %>%
  summarizer(dependent, explanatory, explanatory.multi,  metrics=TRUE)

Cox proportional hazards

e.g. survival::coxph(dependent ~ explanatory)

explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"

colon_s %>%
	summarizer(dependent, explanatory)

Rather than going all-in-one, any number of subset models can be manually added on to a summary.factorlist() table using summarizer.merge(). This is particularly useful when models take a long-time to run or are complicated.

Note requirement for glm.id=TRUE. fit2df is a subfunction extracting most common models to a dataframe.

explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
random.effect = "hospital"
dependent = 'mort_5yr'

# Separate tables
colon_s %>%
  summary.factorlist(dependent, explanatory, glm.id=TRUE) -> example.summary

colon_s %>%
  glmuni(dependent, explanatory) %>%
  fit2df(estimate.suffix=" (univariable)") -> example.univariable

colon_s %>%
  glmmulti(dependent, explanatory) %>%
  fit2df(estimate.suffix=" (multivariable)") -> example.multivariable


colon_s %>%
  glmmixed(dependent, explanatory, random.effect) %>%
  fit2df(estimate.suffix=" (multilevel") -> example.multilevel

# Pipe together
example.summary %>%
  summarizer.merge(example.univariable) %>%
  summarizer.merge(example.multivariable) %>%
  summarizer.merge(example.multilevel) %>%
  select(-c(glm.id, index)) -> example.final
example.final

Cox Proportional Hazards example with separate tables merged together.

explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory.multi = c("age.factor", "obstruct.factor")
dependent = "Surv(time, status)"

# Separate tables
colon_s %>%
	summary.factorlist(dependent, explanatory, glm.id=TRUE) -> example2.summary

colon_s %>%
	coxphuni(dependent, explanatory) %>%
	fit2df(estimate.suffix=" (univariable)") -> example2.univariable

colon_s %>%
  coxphmulti(dependent, explanatory.multi) %>%
  fit2df(estimate.suffix=" (multivariable)") -> example2.multivariable

# Pipe together
example2.summary %>%
	summarizer.merge(example2.univariable) %>%
	summarizer.merge(example2.multivariable) %>%
	select(-c(glm.id, index)) -> example2.final
example2.final







# OR plot
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or.plot(dependent, explanatory)
# Previously fitted models (`glmmulti()` or `glmmixed()`) can be provided directly to `glmfit`

# HR plot (not fully tested)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"
colon_s %>%
  hr.plot(dependent, explanatory, dependent_label = "Survival")
# Previously fitted models (`coxphmulti`) can be provided directly using `coxfit`
```



### ANOVA







### Bayesian

```r
# Full report for a Bayesian logistic mixed model with effect sizes
library(rstanarm)

stan_glmer(vs ~ mpg + (1|cyl), data=mtcars, family="binomial") %>% 
  report(standardize="smart", effsize="cohen1988") %>% 
  to_fulltext()
```


### lme4: Mixed-effects models in R

https://github.com/lme4/lme4/


###  indices of model quality and goodness of fit

**Test if your model is a good model**

https://easystats.github.io/performance/








---

\pagebreak




<!-- https://holtzy.github.io/Pimp-my-rmd/#highlight_a_piece_of_text -->
<!-- https://www.overleaf.com/learn/latex/Using_colours_in_LaTeX -->
<!-- Create Awesome LaTeX Table with knitr::kable andkableExtra -->
<!-- https://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf -->

<!-- header-includes: -->
<!-- - \usepackage{pdflscape} -->
<!-- - \newcommand{\blandscape}{\begin{landscape}} -->
<!-- - \newcommand{\elandscape}{\end{landscape}} -->
<!-- - \usepackage{xcolor} -->
<!-- - \usepackage{afterpage} -->





<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

Some Text ile sağkalım açısından bir ilişki bulunmamıştır (p = 0.22).

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

Some Text ile sağkalım açısından bir ilişki bulunmamıştır (p = 0.22).

}
}








```r
my_text <- kableExtra::text_spec("Some Text", color = "red", background = "yellow")
# `r my_text`
```






\pagebreak






<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">


</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{


}
}

\pagebreak


<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">

Text Here

</div>


\noindent\colorbox{yellow}{
\parbox{\dimexpr\linewidth-2\fboxsep}{

Text Here

}
}


\pagebreak

\pagecolor{yellow}\afterpage{\nopagecolor}


\pagebreak


---



<img src="/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/unnamed-chunk-77-1.png" width="50%" /><img src="/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/unnamed-chunk-77-2.png" width="50%" />



---

<div class = "row">
<div class = "col-md-4">
<br><br>Since R Markdown use the [bootstrap framework](https://getbootstrap.com/docs/4.0/layout/grid/) under the hood. It is possible to benefit its powerful grid system. Basically, you can consider that your row is divided in 12 subunits of same width. You can then choose to use only a few of this subunits.
</div>
<div class = "col-md-4">
<br><br>Here, I use 3 subunits of size 4 (4x3=12). The last column is used for a plot. You can read more about the grid system [here](bootstrap grid system). I got this result showing the following code in my R Markdown document.
</div>
<div class = "col-md-4">
![](/Users/serdarbalciold/histopathRprojects/histopathology-template/figs/unnamed-chunk-78-1.png)<!-- -->
</div>
</div>


---



# Tabs for sub-chapters {#buttons .tabset .tabset-fade .tabset-pills}

## First

content of sub-chapter #1

## Second

content of sub-chapter #2

## Third

content of sub-chapter #3





<div class="rmdnote">
<p>Block rmdnote</p>
</div>



<div class="rmdtip">
<p>Block rmdtip</p>
</div>



<div class="warning">
<p>Block warning</p>
</div>


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
saved data after analysis to /Users/serdarbalciold/histopathRprojects/histopathology-template/data/histopathology-template2020-02-26.xlsx : 2020-02-26 15:31:04
```



```r
mydata %>% downloadthis::download_this(output_name = excelName, output_extension = ".csv", 
    button_label = "Download data as csv", button_type = "default")
```

<!--html_preserve--><a href="data:text/csv;base64,SUQsTmFtZSxTZXgsQWdlLFJhY2UsUHJlaW52YXNpdmVDb21wb25lbnQsTFZJLFBOSSxMYXN0Rm9sbG93VXBEYXRlLERlYXRoLEdyb3VwLEdyYWRlLFRTdGFnZSxBbnRpWF9pbnRlbnNpdHksQW50aVlfaW50ZW5zaXR5LEx5bXBoTm9kZU1ldGFzdGFzaXMsVmFsaWQsU21va2VyLEdyYWRlX0xldmVsLFN1cmdlcnlEYXRlLERlYXRoVGltZSxpbnQsT3ZlcmFsbFRpbWUsT3V0Y29tZQowMDEsQXJpYXRuYSxGZW1hbGUsMzAsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDktMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMSw0LDIsMixQcmVzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE5LTA1LTEwVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA1LTEwIFVUQy0tMjAxOS0wOS0yNiBVVEMsNC41LDAKMDAyLEphaHpseW5uLEZlbWFsZSwzMixXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA0LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDIsMixBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE4LTA5LTAzVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA5LTAzIFVUQy0tMjAxOS0wNC0yNiBVVEMsNy44LDEKMDAzLEtleWxhbmksRmVtYWxlLDUzLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTAtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDMsMiwyLFByZXNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTktMDMtMjJUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDMtMjIgVVRDLS0yMDE5LTEwLTI2IFVUQyw3LjEsMQowMDQsUHJvY3RvcixGZW1hbGUsNTcsSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA1LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSwzLDIsMyxQcmVzZW50LFRSVUUsRkFMU0UsbG93LDIwMTgtMDktMjhUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDktMjggVVRDLS0yMDE5LTA1LTI2IFVUQyw3LjksMQowMDUsSWJldHRlLE1hbGUsNDcsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOC0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMSwzLDIsUHJlc2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTgtMTAtMDdUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMDcgVVRDLS0yMDE5LTA4LTI2IFVUQywxMC42LDEKMDA2LEplcmFtZSxGZW1hbGUsNTgsV2hpdGUsUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDUtMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMiwzLDEsMSxBYnNlbnQsRkFMU0UsRkFMU0UsbW9kZXJhdGUsMjAxOC0xMC0yOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMC0yOCBVVEMtLTIwMTktMDUtMjYgVVRDLDYuOSwwCjAwNyxTaGFyZW5uYSxGZW1hbGUsNTksQmxhY2ssUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA0LTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsMywxLDIsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTgtMDgtMTVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDgtMTUgVVRDLS0yMDE5LTA0LTI2IFVUQyw4LjQsMAowMDgsR3VpbixNYWxlLDU0LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDctMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDMsMywzLEFic2VudCxUUlVFLFRSVUUsaGlnaCwyMDE4LTA4LTI3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA4LTI3IFVUQy0tMjAxOS0wNy0yNiBVVEMsMTEsMQowMDksSG9sbHlseW5uLE1hbGUsMzUsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDYtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsNCwyLDMsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTktMDMtMTBUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDMtMTAgVVRDLS0yMDE5LTA2LTI2IFVUQywzLjUsMQowMTAsRWxlYXphcixGZW1hbGUsMjcsTmF0aXZlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTAtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDQsMywxLFByZXNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTktMDMtMDZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDMtMDYgVVRDLS0yMDE5LTEwLTI2IFVUQyw3LjYsMQowMTEsS3lsZWVuLE1hbGUsNTMsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTItMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDQsMiwxLFByZXNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTktMDQtMTNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDQtMTMgVVRDLS0yMDE5LTEyLTI2IFVUQyw4LjQsMQowMTIsVGFzaHlhLEZlbWFsZSw1NSxBc2lhbixBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNC0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywzLDMsMixQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE4LTEwLTI1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEwLTI1IFVUQy0tMjAxOS0wNC0yNiBVVEMsNiwxCjAxMyxKdWQsRmVtYWxlLDcyLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTEtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDIsMSwxLFByZXNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSxOQSxNb3JlVGhhbjFZZWFyLE5BLS1OQSxOQSwxCjAxNCxLYWl6bGVpZ2gsTWFsZSw1MSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTExLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMywzLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE5LTAyLTExVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAyLTExIFVUQy0tMjAxOS0xMS0yNiBVVEMsOS41LDEKMDE1LEt5bGVhLEZlbWFsZSw0NixXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMC0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwyLDEsMSxBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTgtMTEtMjBUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMjAgVVRDLS0yMDE5LTEwLTI2IFVUQywxMS4yLDEKMDE2LEFsdGlhLE1hbGUsNjUsSGlzcGFuaWMsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOS0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMywyLDIsQWJzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE4LTEwLTA2VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEwLTA2IFVUQy0tMjAxOS0wOS0yNiBVVEMsMTEuNywxCjAxNyxTZWNpbGlhLEZlbWFsZSw1OCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTExLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywzLDMsMSxBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wMi0yMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0yMSBVVEMtLTIwMTktMTEtMjYgVVRDLDkuMiwxCjAxOCxOaWtvbGxlLEZlbWFsZSwzNCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA0LTI2VDAwOjAwOjAwWixOQSxUcmVhdG1lbnQsMyw0LDIsMyxBYnNlbnQsRkFMU0UsRkFMU0UsTkEsMjAxOC0wOS0wOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wOS0wOSBVVEMtLTIwMTktMDQtMjYgVVRDLDcuNixOQQowMTksRWxyb3ksRmVtYWxlLDU0LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDQtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwzLDIsUHJlc2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOC0xMi0yM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMi0yMyBVVEMtLTIwMTktMDQtMjYgVVRDLDQuMSwxCjAyMCxNYXJkaXMsTWFsZSw0NSxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNy0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywyLDMsMSxBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wMy0wNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMy0wNSBVVEMtLTIwMTktMDctMjYgVVRDLDQuNywxCjAyMSxYaXRsYWxpLEZlbWFsZSw1OSxIaXNwYW5pYyxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTAzLTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsMiwzLDEsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE4LTA2LTAzVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA2LTAzIFVUQy0tMjAxOS0wMy0yNiBVVEMsOS43LDAKMDIyLE1ja2FpbCxGZW1hbGUsMjYsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wMy0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsNCwzLDMsQWJzZW50LEZBTFNFLFRSVUUsbG93LDIwMTgtMDctMTZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDctMTYgVVRDLS0yMDE5LTAzLTI2IFVUQyw4LjMsMAowMjMsRWRnZWwsTWFsZSw2NSxXaGl0ZSxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMjAtMDEtMjZUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwxLDEsMiwxLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE5LTA3LTI1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA3LTI1IFVUQy0tMjAyMC0wMS0yNiBVVEMsNiwwCjAyNCxTaGF5bmFoLEZlbWFsZSw0NCxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wMy0yNlQwMDowMDowMFosRkFMU0UsQ29udHJvbCwxLDQsMywxLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE4LTEwLTEyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEwLTEyIFVUQy0tMjAxOS0wMy0yNiBVVEMsNS41LDAKMDI1LFNpcmVuaXR5LEZlbWFsZSw0OSxCbGFjayxQcmVzZW50LFByZXNlbnQsQWJzZW50LDIwMjAtMDItMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMywyLDIsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE5LTA4LTEzVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA4LTEzIFVUQy0tMjAyMC0wMi0yNiBVVEMsNi40LDEKMDI2LEFzaWwsRmVtYWxlLDI1LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDQtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMiwxLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTgtMDUtMTNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDUtMTMgVVRDLS0yMDE5LTA0LTI2IFVUQywxMS40LDEKMDI3LFJ1c3RvbixNYWxlLDcyLEJsYWNrLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNi0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDIsMywzLDIsQWJzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE5LTAzLTAyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTAyIFVUQy0tMjAxOS0wNi0yNiBVVEMsMy44LDAKMDI4LFN5bmRpLEZlbWFsZSwyNixXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNC0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDMsMyxBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE4LTA2LTIwVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA2LTIwIFVUQy0tMjAxOS0wNC0yNiBVVEMsMTAuMiwxCjAyOSxFcmRlbmUsTWFsZSw1NCxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAyMC0wMi0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwzLDIsUHJlc2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTktMTEtMjZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMTEtMjYgVVRDLS0yMDIwLTAyLTI2IFVUQywzLDEKMDMwLFNoYW50cmVsLE1hbGUsNzMsQmxhY2ssQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNy0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDIsMSxOQSxUUlVFLFRSVUUsaGlnaCwyMDE5LTAxLTE0VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTE0IFVUQy0tMjAxOS0wNy0yNiBVVEMsNi40LDEKMDMxLEVzdGlsLEZlbWFsZSw2MyxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMC0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDEsNCwzLDEsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE4LTExLTE2VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTExLTE2IFVUQy0tMjAxOS0xMC0yNiBVVEMsMTEuMywxCjAzMixSYXlnaW5lLE1hbGUsNDAsSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTEyLTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMyw0LDMsMyxBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wNi0wOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNi0wOSBVVEMtLTIwMTktMTItMjYgVVRDLDYuNSwwCjAzMyxBcmllYWgsRmVtYWxlLDQ0LFdoaXRlLFByZXNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAxLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMiwyLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE5LTA0LTA1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA0LTA1IFVUQy0tMjAyMC0wMS0yNiBVVEMsOS43LDEKMDM0LEtlbGF5YSxGZW1hbGUsNTgsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTItMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwyLDMsQWJzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE5LTA2LTA0VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA2LTA0IFVUQy0tMjAxOS0xMi0yNiBVVEMsNi43LDEKMDM1LE11cmxhbmQsRmVtYWxlLDYyLFdoaXRlLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wOC0yNlQwMDowMDowMFosRkFMU0UsQ29udHJvbCwxLDMsMSwzLEFic2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOS0wNS0xNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNS0xNiBVVEMtLTIwMTktMDgtMjYgVVRDLDMuMywwCjAzNixLb2xpYSxGZW1hbGUsNTEsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNC0yNlQwMDowMDowMFosRkFMU0UsQ29udHJvbCwzLDQsMywyLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTgtMDUtMTlUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDUtMTkgVVRDLS0yMDE5LTA0LTI2IFVUQywxMS4yLDAKMDM3LE5hdGF5bGlhLEZlbWFsZSw2MSxBc2lhbixBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDYtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMiwyLDEsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE4LTExLTAyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTExLTAyIFVUQy0tMjAxOS0wNi0yNiBVVEMsNy44LDEKMDM4LE5hbmN5bGVlLE1hbGUsNjAsV2hpdGUsUHJlc2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA4LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDIsMywzLEFic2VudCxGQUxTRSxGQUxTRSxsb3csMjAxOS0wMS0yNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0yNSBVVEMtLTIwMTktMDgtMjYgVVRDLDcsMQowMzksSWF5YW5hLEZlbWFsZSwyOSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAxLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDMsMyxBYnNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE5LTA3LTE2VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA3LTE2IFVUQy0tMjAyMC0wMS0yNiBVVEMsNi4zLDEKMDQwLFd5bm5lbGwsRmVtYWxlLDMyLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDIwLTAxLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwzLDIsMyxQcmVzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOS0wMy0xOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMy0xOSBVVEMtLTIwMjAtMDEtMjYgVVRDLDEwLjIsMQowNDEsQ2hhc2lhLE1hbGUsNjEsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMjAtMDItMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMywyLFByZXNlbnQsVFJVRSxUUlVFLGxvdywyMDE5LTA3LTI1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA3LTI1IFVUQy0tMjAyMC0wMi0yNiBVVEMsNywxCjA0MixWaWduZXNoLE1hbGUsNjgsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMi0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywxLDIsMyxBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTktMDEtMTlUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMTkgVVRDLS0yMDE5LTEyLTI2IFVUQywxMS4yLDEKMDQzLEthaHJpLE1hbGUsNjgsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMS0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsNCwzLDMsUHJlc2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxOS0wMi0wNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0wNiBVVEMtLTIwMTktMTEtMjYgVVRDLDkuNywwCjA0NCxFbmF5YSxNYWxlLDQ0LEJsYWNrLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMC0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSwxLDIsMixBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wNC0wMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNC0wMSBVVEMtLTIwMTktMTAtMjYgVVRDLDYuOCwxCjA0NSxaZXBoeW4sRmVtYWxlLDcyLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTEwLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMywyLEFic2VudCxUUlVFLFRSVUUsbG93LDIwMTktMDctMjNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDctMjMgVVRDLS0yMDE5LTEwLTI2IFVUQywzLjEsMQowNDYsS2ltYXIsTWFsZSw1MSxIaXNwYW5pYyxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMC0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSw0LDIsMyxQcmVzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE5LTA3LTA4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA3LTA4IFVUQy0tMjAxOS0xMC0yNiBVVEMsMy42LDEKMDQ3LFR5emhhbmUsTWFsZSw0MCxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wOC0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSwyLDMsMixQcmVzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE5LTAxLTAyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTAyIFVUQy0tMjAxOS0wOC0yNiBVVEMsNy44LDEKMDQ4LENob25nLE1hbGUsNjIsQXNpYW4sQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOC0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsNCwxLDMsQWJzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTgtMTEtMTFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMTEgVVRDLS0yMDE5LTA4LTI2IFVUQyw5LjUsMAowNDksVGF5dGUsTWFsZSwzMixIaXNwYW5pYyxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA5LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDMsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE5LTAzLTI1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTI1IFVUQy0tMjAxOS0wOS0yNiBVVEMsNiwxCjA1MCxUcmF2YWlsLE1hbGUsNDAsTmF0aXZlLFByZXNlbnQsQWJzZW50LEFic2VudCwyMDE5LTAzLTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDIsNCwxLDIsUHJlc2VudCxGQUxTRSxGQUxTRSxsb3csMjAxOC0wNS0xNFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNS0xNCBVVEMtLTIwMTktMDMtMjYgVVRDLDEwLjQsMAowNTEsU3VqZWksRmVtYWxlLDUzLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA2LTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMiw0LDIsMixQcmVzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOC0wNy0xOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNy0xOSBVVEMtLTIwMTktMDYtMjYgVVRDLDExLjIsMAowNTIsU3VuZHJhLE1hbGUsMjgsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDUtMjZUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwxLDQsMyxOQSxBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wMi0xOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0xOCBVVEMtLTIwMTktMDUtMjYgVVRDLDMuMywwCjA1MyxFbW1ldHQsRmVtYWxlLDUzLEJsYWNrLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA0LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDIsMiwzLEFic2VudCxUUlVFLEZBTFNFLGxvdywyMDE4LTA5LTE1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA5LTE1IFVUQy0tMjAxOS0wNC0yNiBVVEMsNy40LDEKMDU0LERhc2h1bixGZW1hbGUsNTksQXNpYW4sQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOC0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsNCwyLDIsQWJzZW50LFRSVUUsRkFMU0UsbG93LDIwMTgtMTEtMjFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMjEgVVRDLS0yMDE5LTA4LTI2IFVUQyw5LjIsMAowNTUsU3VqZWlyeSxNYWxlLDU1LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTAzLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDIsMSxBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTgtMDUtMjhUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDUtMjggVVRDLS0yMDE5LTAzLTI2IFVUQyw5LjksMQowNTYsRWltYWFuLEZlbWFsZSw1MSxXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOS0yNlQwMDowMDowMFosRkFMU0UsQ29udHJvbCwzLDQsMiwzLEFic2VudCxGQUxTRSxGQUxTRSxsb3csMjAxOC0xMC0xOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMC0xOSBVVEMtLTIwMTktMDktMjYgVVRDLDExLjIsMAowNTcsWWF0aGFydGgsRmVtYWxlLDU3LEhpc3BhbmljLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMS0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwxLDIsMyxBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wMS0yNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0yNSBVVEMtLTIwMTktMTEtMjYgVVRDLDEwLDEKMDU4LENlZHJpbmEsRmVtYWxlLDQ4LEhpc3BhbmljLE5BLEFic2VudCxQcmVzZW50LDIwMTktMTItMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDIsMywzLEFic2VudCxGQUxTRSxUUlVFLG1vZGVyYXRlLDIwMTktMDctMTRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDctMTQgVVRDLS0yMDE5LTEyLTI2IFVUQyw1LjQsMQowNTksTmljdGVoYSxGZW1hbGUsMjgsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wMy0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDIsMixBYnNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOC0wNi0xMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNi0xMCBVVEMtLTIwMTktMDMtMjYgVVRDLDkuNSwxCjA2MCxOaWdlbCxGZW1hbGUsMzMsV2hpdGUsUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDctMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMywzLFByZXNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTktMDItMTRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDItMTQgVVRDLS0yMDE5LTA3LTI2IFVUQyw1LjQsMQowNjEsS2FybWlzaGEsTWFsZSw0MixXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAyLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMiwyLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxOS0wOC0zMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wOC0zMCBVVEMtLTIwMjAtMDItMjYgVVRDLDUuOSwxCjA2MixEYXJsZWFuZSxNYWxlLDQzLFdoaXRlLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wNi0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDMsMixQcmVzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTgtMTAtMTVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMTUgVVRDLS0yMDE5LTA2LTI2IFVUQyw4LjQsMQowNjMsTHlubmllLE1hbGUsTkEsQmxhY2ssUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA5LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDMsMyxQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE5LTA1LTIyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA1LTIyIFVUQy0tMjAxOS0wOS0yNiBVVEMsNC4xLDEKMDY0LFlhcmV0Y3ksRmVtYWxlLDY1LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA2LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSwzLDIsMSxQcmVzZW50LEZBTFNFLE5BLGxvdywyMDE4LTA5LTE5VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA5LTE5IFVUQy0tMjAxOS0wNi0yNiBVVEMsOS4yLDEKMDY1LEVsaW9uLEZlbWFsZSw2MCxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNi0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsNCwxLDIsUHJlc2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxOC0xMS0xN1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMS0xNyBVVEMtLTIwMTktMDYtMjYgVVRDLDcuMywwCjA2NixOYXlkaWEsRmVtYWxlLDI2LEJpLVJhY2lhbCxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0xMS0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwzLDIsQWJzZW50LEZBTFNFLFRSVUUsbG93LDIwMTktMDUtMDlUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDUtMDkgVVRDLS0yMDE5LTExLTI2IFVUQyw2LjYsMQowNjcsQmV2ZWx5LE1hbGUsNTIsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMjAtMDItMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMSw0LDIsMyxBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE5LTA3LTI3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA3LTI3IFVUQy0tMjAyMC0wMi0yNiBVVEMsNywwCjA2OCxLYWxlZW0sTWFsZSw0NixBc2lhbixBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAxLTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSw0LDMsMyxBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTktMDUtMDZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDUtMDYgVVRDLS0yMDIwLTAxLTI2IFVUQyw4LjYsMAowNjksT2x1d2F0b3lvc2ksTWFsZSw0MCxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAyMC0wMi0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMSwzLDMsQWJzZW50LEZBTFNFLFRSVUUsbG93LDIwMTktMTAtMjZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMTAtMjYgVVRDLS0yMDIwLTAyLTI2IFVUQyw0LDEKMDcwLE1ha3N0b24sTWFsZSwzMixXaGl0ZSxQcmVzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDYtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMywxLFByZXNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTktMDItMjRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDItMjQgVVRDLS0yMDE5LTA2LTI2IFVUQyw0LjEsMQowNzEsRWxkcmlkZ2UsRmVtYWxlLDMyLEJsYWNrLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA0LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDIsMSxQcmVzZW50LFRSVUUsVFJVRSxsb3csMjAxOC0wNi0wNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNi0wNSBVVEMtLTIwMTktMDQtMjYgVVRDLDEwLjcsMQowNzIsTWVsYWgsTWFsZSw3MCxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAyMC0wMi0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDMsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE5LTEwLTA3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTEwLTA3IFVUQy0tMjAyMC0wMi0yNiBVVEMsNC43LDEKMDczLFBvcHBpZSxNYWxlLDcyLEhpc3BhbmljLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wNy0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDMsMixBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOC0xMi0zMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMi0zMCBVVEMtLTIwMTktMDctMjYgVVRDLDYuOSwxCjA3NCxMYXZvbmlhLEZlbWFsZSw0OSxCbGFjayxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA2LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMywxLFByZXNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTgtMTItMDlUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTItMDkgVVRDLS0yMDE5LTA2LTI2IFVUQyw2LjYsMQowNzUsSmVyZW1leSxNYWxlLDMwLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTEwLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMywxLFByZXNlbnQsVFJVRSxUUlVFLGxvdywyMDE5LTA1LTE3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA1LTE3IFVUQy0tMjAxOS0xMC0yNiBVVEMsNS4zLDEKMDc2LEthb3MsRmVtYWxlLDcxLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA4LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSwyLDMsMSxBYnNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTgtMTItMjZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTItMjYgVVRDLS0yMDE5LTA4LTI2IFVUQyw4LDEKMDc3LEFsYWlqYSxNYWxlLDQyLEhpc3BhbmljLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA1LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMywzLFByZXNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE4LTA4LTE4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA4LTE4IFVUQy0tMjAxOS0wNS0yNiBVVEMsOS4zLDEKMDc4LE1hbGFpbGEsTWFsZSw0OSxCaS1SYWNpYWwsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA3LTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsMywzLDIsQWJzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMTEtMTRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMTQgVVRDLS0yMDE5LTA3LTI2IFVUQyw4LjQsMAowNzksVHJhdmFudGUsRmVtYWxlLDUzLEJsYWNrLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wNy0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDMsMiwyLDIsUHJlc2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOC0xMS0wN1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMS0wNyBVVEMtLTIwMTktMDctMjYgVVRDLDguNiwwCjA4MCxHZXZvbnRlLEZlbWFsZSw1NSxXaGl0ZSxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDgtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsNCwyLDEsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxOC0xMi0wMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMi0wMSBVVEMtLTIwMTktMDgtMjYgVVRDLDguOCwxCjA4MSxTaGVycmlkYW4sRmVtYWxlLDcyLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA0LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMywxLEFic2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOC0wNS0wMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNS0wMSBVVEMtLTIwMTktMDQtMjYgVVRDLDExLjgsMQowODIsQmVycmEsRmVtYWxlLDU0LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDIwLTAxLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDMsMiwyLEFic2VudCxGQUxTRSxUUlVFLG1vZGVyYXRlLDIwMTktMDktMTZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDktMTYgVVRDLS0yMDIwLTAxLTI2IFVUQyw0LjMsMQowODMsT2x1d2FmaWtheW9taSxGZW1hbGUsNjgsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTItMjZUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwyLDMsMywzLEFic2VudCxUUlVFLFRSVUUsaGlnaCwyMDE5LTAzLTI5VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTI5IFVUQy0tMjAxOS0xMi0yNiBVVEMsOC45LDAKMDg0LEt5bnNpZSxGZW1hbGUsNDcsSGlzcGFuaWMsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTAtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsMSwzLDMsQWJzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE4LTExLTA0VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTExLTA0IFVUQy0tMjAxOS0xMC0yNiBVVEMsMTEuNywxCjA4NSxDeWxlbmEsTWFsZSw2NyxIaXNwYW5pYyxQcmVzZW50LFByZXNlbnQsQWJzZW50LDIwMjAtMDItMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMyw0LDMsMSxQcmVzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE5LTAzLTA1VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTA1IFVUQy0tMjAyMC0wMi0yNiBVVEMsMTEuNywwCjA4NixZYXJpem1hcixNYWxlLDM2LEhpc3BhbmljLFByZXNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDQtMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMiw0LDIsMyxQcmVzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMTAtMjdUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMjcgVVRDLS0yMDE5LTA0LTI2IFVUQyw2LDAKMDg3LE5hZGEsRmVtYWxlLDU0LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA5LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSwyLDMsMSxBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTgtMTEtMzBUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTEtMzAgVVRDLS0yMDE5LTA5LTI2IFVUQyw5LjksMQowODgsV2lsZm9yZCxNYWxlLDcyLFdoaXRlLFByZXNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wOC0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDEsNCwxLDIsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE5LTA1LTI4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA1LTI4IFVUQy0tMjAxOS0wOC0yNiBVVEMsMi45LDEKMDg5LFJlZ2luZSxGZW1hbGUsNjQsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTEyLTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMyw0LDMsMSxBYnNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTktMDEtMzBUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMzAgVVRDLS0yMDE5LTEyLTI2IFVUQywxMC45LDAKMDkwLEt5aGVpbSxNYWxlLDY1LEJsYWNrLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTAzLTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDIsMiwxLDIsUHJlc2VudCxGQUxTRSxUUlVFLG1vZGVyYXRlLDIwMTgtMDktMjhUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDktMjggVVRDLS0yMDE5LTAzLTI2IFVUQyw1LjksMAowOTEsQ2xhcmVzZSxGZW1hbGUsNDgsQmxhY2ssUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA2LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSwyLDIsMSxBYnNlbnQsRkFMU0UsRkFMU0UsbW9kZXJhdGUsMjAxOC0wOC0yMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wOC0yMCBVVEMtLTIwMTktMDYtMjYgVVRDLDEwLjIsMQowOTIsU3lsdmFuaWEsRmVtYWxlLDMyLEJsYWNrLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA3LTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMyw0LDMsMyxBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTktMDQtMThUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDQtMTggVVRDLS0yMDE5LTA3LTI2IFVUQywzLjMsMAowOTMsQnJhbmRpc3MsTWFsZSw1NixXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNy0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwxLDMsMixBYnNlbnQsVFJVRSxUUlVFLGxvdywyMDE5LTAyLTA2VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAyLTA2IFVUQy0tMjAxOS0wNy0yNiBVVEMsNS42LDEKMDk0LE9ybGluLE1hbGUsNTEsQmxhY2ssQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA0LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDMsMSxBYnNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTgtMTItMTRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTItMTQgVVRDLS0yMDE5LTA0LTI2IFVUQyw0LjQsMQowOTUsU2hpbmVrYSxNYWxlLDY2LEJsYWNrLEFic2VudCxQcmVzZW50LEFic2VudCwyMDIwLTAyLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDMsMyxBYnNlbnQsVFJVRSxUUlVFLGxvdywyMDE5LTExLTIyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTExLTIyIFVUQy0tMjAyMC0wMi0yNiBVVEMsMy4xLDEKMDk2LERlc2VyYWksTWFsZSw1NCxCbGFjayxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMjAtMDItMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMSwzLDIsMixBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTktMDgtMDZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDgtMDYgVVRDLS0yMDIwLTAyLTI2IFVUQyw2LjcsMAowOTcsQ2hlbWlrYSxNYWxlLDQzLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA4LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDMsMywyLEFic2VudCxUUlVFLFRSVUUsaGlnaCwyMDE4LTEwLTA3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEwLTA3IFVUQy0tMjAxOS0wOC0yNiBVVEMsMTAuNiwxCjA5OCxFbWVsaWEsTWFsZSwzMCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEyLTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMyw0LDMsMixQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE5LTA4LTMwVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA4LTMwIFVUQy0tMjAxOS0xMi0yNiBVVEMsMy45LDAKMDk5LEpvaG5pc2hhLEZlbWFsZSw1MixCbGFjayxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDYtMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsTkEsMiwyLDIsQWJzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxOS0wMS0yM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0yMyBVVEMtLTIwMTktMDYtMjYgVVRDLDUuMSwwCjEwMCxTZWhhanZlZXIsTWFsZSw0MixIaXNwYW5pYyxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMTEtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDQsTkEsMixBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wMi0yMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0yMCBVVEMtLTIwMTktMTEtMjYgVVRDLDkuMiwxCjEwMSxKYWNvcmlhLE1hbGUsNTgsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTEtMjZUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwyLDQsMywyLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE5LTA1LTE4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA1LTE4IFVUQy0tMjAxOS0xMS0yNiBVVEMsNi4zLDAKMTAyLE1hcnF1YXZpb24sTWFsZSwzMyxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wOS0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsNCwzLDIsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOS0wNS0yM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNS0yMyBVVEMtLTIwMTktMDktMjYgVVRDLDQuMSwwCjEwMyxEZWVtYSxGZW1hbGUsNDMsSGlzcGFuaWMsUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDMtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMywxLFByZXNlbnQsRkFMU0UsVFJVRSxtb2RlcmF0ZSwyMDE4LTA3LTAzVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA3LTAzIFVUQy0tMjAxOS0wMy0yNiBVVEMsOC43LDEKMTA0LExhcmVuY2lhLEZlbWFsZSwzOCxCbGFjayxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA5LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMywyLEFic2VudCxGQUxTRSxUUlVFLG1vZGVyYXRlLDIwMTgtMTItMjRUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTItMjQgVVRDLS0yMDE5LTA5LTI2IFVUQyw5LjEsMQoxMDUsSm9zdGVpbixGZW1hbGUsNTYsQXNpYW4sQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTExLTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsMywzLDMsUHJlc2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxOS0wNC0xOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNC0xOSBVVEMtLTIwMTktMTEtMjYgVVRDLDcuMiwwCjEwNixKZWZmZXJ5LE1hbGUsNjEsQmxhY2ssQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA0LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDIsMSxQcmVzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE4LTA5LTE3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA5LTE3IFVUQy0tMjAxOS0wNC0yNiBVVEMsNy4zLDEKMTA3LERyZW5uZW4sRmVtYWxlLDQyLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDIwLTAyLTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDEsNCwzLDMsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE5LTAzLTAzVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAzLTAzIFVUQy0tMjAyMC0wMi0yNiBVVEMsMTEuOCwwCjEwOCxEYWtvdGFoLE1hbGUsNDYsQmktUmFjaWFsLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wNy0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywxLDIsMSxBYnNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOC0xMi0xMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMi0xMCBVVEMtLTIwMTktMDctMjYgVVRDLDcuNSwxCjEwOSxXeW5vbmFoLE1hbGUsMjgsV2hpdGUsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA3LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywzLDMsMSxBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTgtMTAtMTZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMTYgVVRDLS0yMDE5LTA3LTI2IFVUQyw5LjMsMQoxMTAsVmFsaWNpYSxGZW1hbGUsMzcsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDUtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsMywzLDMsQWJzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOS0wMi0yMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0yMCBVVEMtLTIwMTktMDUtMjYgVVRDLDMuMiwxCjExMSxWaWhhYSxNYWxlLDU0LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTEyLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDMsMywzLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxOS0wOS0xN1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wOS0xNyBVVEMtLTIwMTktMTItMjYgVVRDLDMuMywxCjExMixBY2V5bixGZW1hbGUsNjUsQmktUmFjaWFsLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDUtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDQsMywzLEFic2VudCxUUlVFLFRSVUUsaGlnaCwyMDE4LTA4LTEzVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA4LTEzIFVUQy0tMjAxOS0wNS0yNiBVVEMsOS40LDEKMTEzLEFyYmF6LEZlbWFsZSw1MyxBc2lhbixQcmVzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMi0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDIsMyxQcmVzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxOS0wNS0yOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNS0yOSBVVEMtLTIwMjAtMDItMjYgVVRDLDguOSwxCjExNCxOeWVsbGEsRmVtYWxlLDI3LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTAzLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywzLDMsMyxQcmVzZW50LEZBTFNFLFRSVUUsbW9kZXJhdGUsMjAxOC0xMS0zMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMS0zMCBVVEMtLTIwMTktMDMtMjYgVVRDLDMuOSwxCjExNSxDYWVsZW4sTWFsZSw3MyxIaXNwYW5pYyxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMjAtMDEtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsMywyLDIsUHJlc2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE5LTA2LTIxVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA2LTIxIFVUQy0tMjAyMC0wMS0yNiBVVEMsNy4yLDEKMTE2LFJlYXRoZXIsTWFsZSw3MCxIaXNwYW5pYyxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wOC0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDIsNCwxLDMsUHJlc2VudCxGQUxTRSxUUlVFLGxvdywyMDE5LTA0LTIxVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA0LTIxIFVUQy0tMjAxOS0wOC0yNiBVVEMsNC4yLDAKMTE3LFRodXl0cmFuZyxGZW1hbGUsMzIsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNS0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywyLDMsMixBYnNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTgtMTAtMDVUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMDUgVVRDLS0yMDE5LTA1LTI2IFVUQyw3LjcsMQoxMTgsSmFobGEsTWFsZSw0OCxIaXNwYW5pYyxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMC0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDEsMywyLDIsQWJzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxOC0xMi0xMlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMi0xMiBVVEMtLTIwMTktMTAtMjYgVVRDLDEwLjUsMQoxMTksSWhsYSxNYWxlLDM5LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDYtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsNCwyLDMsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxOC0xMi0yOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMi0yOSBVVEMtLTIwMTktMDYtMjYgVVRDLDUuOSwxCjEyMCxSdXFheWEsRmVtYWxlLDY4LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTEyLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDIsMSxQcmVzZW50LEZBTFNFLFRSVUUsbG93LDIwMTktMDEtMjFUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDEtMjEgVVRDLS0yMDE5LTEyLTI2IFVUQywxMS4yLDEKMTIxLE5hdGFsaXosRmVtYWxlLDUyLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDYtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsNCwyLDMsUHJlc2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMDctMjZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDctMjYgVVRDLS0yMDE5LTA2LTI2IFVUQywxMSwxCjEyMixEZXlsYW5pLEZlbWFsZSwzNCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTExLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDIsMyxQcmVzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE5LTAxLTI4VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTI4IFVUQy0tMjAxOS0xMS0yNiBVVEMsOS45LDEKMTIzLEtva29ybyxNYWxlLDQ3LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDktMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMyw0LDEsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE5LTAxLTEyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTEyIFVUQy0tMjAxOS0wOS0yNiBVVEMsOC41LDAKMTI0LE5pdmlhaCxNYWxlLDI4LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTEyLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwxLDEsMSxQcmVzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxOS0wMS0yOVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0yOSBVVEMtLTIwMTktMTItMjYgVVRDLDEwLjksMQoxMjUsTGFkYW5pYW4sRmVtYWxlLDQ5LFdoaXRlLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAyMC0wMS0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDMsMSxBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOS0wMy0xM1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMy0xMyBVVEMtLTIwMjAtMDEtMjYgVVRDLDEwLjQsMQoxMjYsQWRhbGFpZGUsTWFsZSw0MyxCaS1SYWNpYWwsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTEyLTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSwyLDMsMyxQcmVzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxOS0wMy0wMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMy0wMSBVVEMtLTIwMTktMTItMjYgVVRDLDkuOCwwCjEyNyxIYWltLEZlbWFsZSwyOSxBc2lhbixBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMC0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDIsMywzLDEsUHJlc2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxOS0wMy0zMFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMy0zMCBVVEMtLTIwMTktMTAtMjYgVVRDLDYuOSwwCjEyOCxEYXZlZGEsRmVtYWxlLDY3LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTEyLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDIsMywzLFByZXNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE5LTA0LTIxVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA0LTIxIFVUQy0tMjAxOS0xMi0yNiBVVEMsOC4yLDEKMTI5LFN0YXNpLE1hbGUsNDEsQmxhY2ssQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDUtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsMywzLDMsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxOC0wNi0yMlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0wNi0yMiBVVEMtLTIwMTktMDUtMjYgVVRDLDExLjEsMQoxMzAsUXVpYW4sRmVtYWxlLDQ0LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA0LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSwzLDIsMixQcmVzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxOS0wMS0yMlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0yMiBVVEMtLTIwMTktMDQtMjYgVVRDLDMuMSwxCjEzMSxDYWx2ZW4sRmVtYWxlLDMxLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDgtMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMywyLDMsMSxQcmVzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE4LTEwLTIxVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTEwLTIxIFVUQy0tMjAxOS0wOC0yNiBVVEMsMTAuMiwwCjEzMixCcmFkbGksTWFsZSw3MyxIaXNwYW5pYyxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0xMC0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDMsMyxQcmVzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE5LTAxLTA0VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTA0IFVUQy0tMjAxOS0xMC0yNiBVVEMsOS43LDEKMTMzLEthdGFyaW5hLEZlbWFsZSwyOSxXaGl0ZSxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDYtMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMyw0LDIsMixQcmVzZW50LEZBTFNFLFRSVUUsbG93LDIwMTgtMDctMDdUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDctMDcgVVRDLS0yMDE5LTA2LTI2IFVUQywxMS42LDAKMTM0LEphbWVpcyxNYWxlLDcyLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTEtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDQsMiwxLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxOS0wNi0xNFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNi0xNCBVVEMtLTIwMTktMTEtMjYgVVRDLDUuNCwxCjEzNSxDZW5uaWUsRmVtYWxlLDczLEhpc3BhbmljLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0xMS0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDMsMyxBYnNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTktMDItMjZUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDItMjYgVVRDLS0yMDE5LTExLTI2IFVUQyw5LDEKMTM2LEVzdG9yaWEsTWFsZSw2OCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA2LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywyLDMsMixBYnNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTgtMDgtMjhUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDgtMjggVVRDLS0yMDE5LTA2LTI2IFVUQyw5LjksMQoxMzcsSmF5Y2V5b24sRmVtYWxlLDMyLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDMtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsNCwzLDIsUHJlc2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMDQtMDNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDQtMDMgVVRDLS0yMDE5LTAzLTI2IFVUQywxMS43LDEKMTM4LEtlbW9yaSxNYWxlLDQxLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTAzLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMywzLFByZXNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTgtMDgtMTNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMDgtMTMgVVRDLS0yMDE5LTAzLTI2IFVUQyw3LjQsMQoxMzksQ29uc3RhbmMsRmVtYWxlLDM2LEJsYWNrLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTExLTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMywzLDIsMixBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE5LTA3LTIwVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA3LTIwIFVUQy0tMjAxOS0xMS0yNiBVVEMsNC4yLDAKMTQwLENocmlzdGlhbnNvbixGZW1hbGUsNjcsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNy0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwyLDIsMixQcmVzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE5LTA0LTE3VDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA0LTE3IFVUQy0tMjAxOS0wNy0yNiBVVEMsMy4zLDEKMTQxLFNhcHJpbmEsRmVtYWxlLDYxLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDgtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDQsMiwzLEFic2VudCxOQSxGQUxTRSxsb3csMjAxOS0wMi0wN1QwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMi0wNyBVVEMtLTIwMTktMDgtMjYgVVRDLDYuNiwxCjE0MixNZWRlbGluZSxGZW1hbGUsMzYsQmxhY2ssQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTItMjZUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwyLDQsMiwzLFByZXNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE5LTA5LTAyVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTA5LTAyIFVUQy0tMjAxOS0xMi0yNiBVVEMsMy44LDAKMTQzLEt5bnpsZXksTWFsZSwzMCxIaXNwYW5pYyxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMS0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDIsNCwzLDMsUHJlc2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTktMDMtMTJUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTktMDMtMTIgVVRDLS0yMDE5LTExLTI2IFVUQyw4LjUsMQoxNDQsRXJpYmVsbGEsRmVtYWxlLDczLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTEyLTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSw0LDIsMixBYnNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOS0wNy0wNVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wNy0wNSBVVEMtLTIwMTktMTItMjYgVVRDLDUuNywwCjE0NSxOYWxlaWdoYSxNYWxlLDQxLEhpc3BhbmljLFByZXNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA3LTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsMywyLDMsUHJlc2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxOS0wMS0wNlQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0wMS0wNiBVVEMtLTIwMTktMDctMjYgVVRDLDYuNiwwCjE0NixUYWlkeW4sRmVtYWxlLDQ5LFdoaXRlLFByZXNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNS0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMiwzLDEsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE4LTA3LTIxVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE4LTA3LTIxIFVUQy0tMjAxOS0wNS0yNiBVVEMsMTAuMiwxCjE0NyxUYWthbm9yaSxNYWxlLDM1LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDIwLTAxLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMiwyLFByZXNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOS0xMC0wOFQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOS0xMC0wOCBVVEMtLTIwMjAtMDEtMjYgVVRDLDMuNiwxCjE0OCxVbGlzc2EsRmVtYWxlLDYwLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA1LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMiwzLEFic2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxOC0xMC0xMVQwMDowMDowMFosV2l0aGluMVllYXIsMjAxOC0xMC0xMSBVVEMtLTIwMTktMDUtMjYgVVRDLDcuNSwxCjE0OSxNYWthbWFlLE1hbGUsNDEsT3RoZXIsUHJlc2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0xMC0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDMsMixQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE5LTAxLTIwVDAwOjAwOjAwWixXaXRoaW4xWWVhciwyMDE5LTAxLTIwIFVUQy0tMjAxOS0xMC0yNiBVVEMsOS4yLDEKMTUwLEtyaXN0aWFubmUsTWFsZSw3MSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA3LTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMiw0LDMsMSxBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTgtMTAtMjNUMDA6MDA6MDBaLFdpdGhpbjFZZWFyLDIwMTgtMTAtMjMgVVRDLS0yMDE5LTA3LTI2IFVUQyw5LjEsMAoxNTEsVHJhY2lseW5uLE1hbGUsNjksV2hpdGUsUHJlc2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTEwLTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsMSwyLDEsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE3LTA4LTA0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDgtMDQgVVRDLS0yMDE5LTEwLTI2IFVUQywyNi43LDAKMTUyLFRlcnJlc3NhLEZlbWFsZSwzNSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEyLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDIsMSwyLEFic2VudCxUUlVFLEZBTFNFLGxvdywyMDE4LTAyLTIzVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDItMjMgVVRDLS0yMDE5LTEyLTI2IFVUQywyMi4xLDEKMTUzLERvcnJhbmNlLEZlbWFsZSw0MSxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wOC0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMywyLDMsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE3LTA5LTA3VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDktMDcgVVRDLS0yMDE5LTA4LTI2IFVUQywyMy42LDEKMTU0LEdpcmxlZSxNYWxlLDQ2LEhpc3BhbmljLFByZXNlbnQsQWJzZW50LEFic2VudCwyMDE5LTExLTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMiw0LDMsMyxQcmVzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE4LTA0LTA4VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDQtMDggVVRDLS0yMDE5LTExLTI2IFVUQywxOS42LDAKMTU1LEJyaWxsaWFudCxNYWxlLDQxLEFzaWFuLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wOS0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMywyLDIsUHJlc2VudCxGQUxTRSxUUlVFLGxvdywyMDE4LTAxLTE0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDEtMTQgVVRDLS0yMDE5LTA5LTI2IFVUQywyMC40LDEKMTU2LFRvaWUsRmVtYWxlLDM4LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDMtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDMsMiwzLFByZXNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE2LTA0LTEzVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMDQtMTMgVVRDLS0yMDE5LTAzLTI2IFVUQywzNS40LDEKMTU3LEZyZWRvbmlhLEZlbWFsZSwyNyxIaXNwYW5pYyxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEyLTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMywyLDMsMSxQcmVzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTctMDctMTNUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNy0xMyBVVEMtLTIwMTktMTItMjYgVVRDLDI5LjQsMAoxNTgsUnlsbGllLE1hbGUsNDQsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMi0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMSwzLDEsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE4LTA4LTE1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDgtMTUgVVRDLS0yMDIwLTAyLTI2IFVUQywxOC40LDEKMTU5LERhbWlvbixNYWxlLDcwLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTExLTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMyw0LDIsMSxBYnNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE3LTA1LTIzVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDUtMjMgVVRDLS0yMDE5LTExLTI2IFVUQywzMC4xLDAKMTYwLFRoZXJ5bixNYWxlLDY3LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA5LTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSwyLDMsMSxQcmVzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE3LTA3LTAzVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDctMDMgVVRDLS0yMDE5LTA5LTI2IFVUQywyNi44LDAKMTYxLEtlbmVrYSxNYWxlLDcxLFdoaXRlLEFic2VudCxQcmVzZW50LFByZXNlbnQsMjAxOS0wNi0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMywyLDEsUHJlc2VudCxUUlVFLEZBTFNFLGxvdywyMDE2LTEwLTIxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMTAtMjEgVVRDLS0yMDE5LTA2LTI2IFVUQywzMi4yLDEKMTYyLENoYXJtZWxsYSxGZW1hbGUsNjEsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDgtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDIsMSwxLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxNy0wMS0xNFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTAxLTE0IFVUQy0tMjAxOS0wOC0yNiBVVEMsMzEuNCwxCjE2MyxSb25pY2lhLE1hbGUsNjgsV2hpdGUsUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTEtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMiwyLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNy0wMS0xM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTAxLTEzIFVUQy0tMjAxOS0xMS0yNiBVVEMsMzQuNCwxCjE2NCxBcmFtaXMsRmVtYWxlLDM2LFdoaXRlLEFic2VudCxOQSxBYnNlbnQsMjAxOS0wMy0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsMywzLDMsQWJzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxNy0wNS0yMVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA1LTIxIFVUQy0tMjAxOS0wMy0yNiBVVEMsMjIuMiwwCjE2NSxJbmZhbnRvZixNYWxlLDQxLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTEyLTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMywzLDIsMyxBYnNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTgtMDQtMjBUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wNC0yMCBVVEMtLTIwMTktMTItMjYgVVRDLDIwLjIsMAoxNjYsS2FuZHVzLE1hbGUsMzMsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNy0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsMSwzLDMsQWJzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE3LTExLTIxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTEtMjEgVVRDLS0yMDE5LTA3LTI2IFVUQywyMC4yLDAKMTY3LERlem9uLE1hbGUsMzEsQXNpYW4sQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMS0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMywzLDIsUHJlc2VudCxGQUxTRSxGQUxTRSxsb3csMjAxNy0wMy0wOVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTAzLTA5IFVUQy0tMjAyMC0wMS0yNiBVVEMsMzQuNSwxCjE2OCxTaGFtYnJpY2EsRmVtYWxlLDQ2LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTAtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMSwyLEFic2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMDItMjZUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wMi0yNiBVVEMtLTIwMTktMTAtMjYgVVRDLDIwLDEKMTY5LE5lYWx5LEZlbWFsZSwzMSxCbGFjayxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA0LTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMyw0LDIsMSxBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTctMTAtMTNUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0xMC0xMyBVVEMtLTIwMTktMDQtMjYgVVRDLDE4LjQsMAoxNzAsU2hhbml0dGEsRmVtYWxlLDcyLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDktMjZUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwxLDIsMiwyLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNy0wMy0wOFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTAzLTA4IFVUQy0tMjAxOS0wOS0yNiBVVEMsMzAuNiwwCjE3MSxEZWxiYSxGZW1hbGUsNzIsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDMtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDIsNCwzLDIsUHJlc2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNy0xMi0xNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTEyLTE1IFVUQy0tMjAxOS0wMy0yNiBVVEMsMTUuNCwxCjE3MixPcnBoaWEsTWFsZSw2MCxXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMS0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDEsMywzLDEsUHJlc2VudCxUUlVFLEZBTFNFLGxvdywyMDE3LTExLTE3VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTEtMTcgVVRDLS0yMDE5LTExLTI2IFVUQywyNC4zLDAKMTczLExhbWFyaXlhLEZlbWFsZSw1OCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTEwLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDIsMSxBYnNlbnQsRkFMU0UsVFJVRSxsb3csMjAxNi0xMS0xNlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE2LTExLTE2IFVUQy0tMjAxOS0xMC0yNiBVVEMsMzUuMywxCjE3NCxFbGl1dGgsTWFsZSwzMyxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA2LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDMsMiwxLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTYtMTAtMjNUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0xMC0yMyBVVEMtLTIwMTktMDYtMjYgVVRDLDMyLjEsMQoxNzUsTkEsRmVtYWxlLDU0LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTEtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMiwzLDIsUHJlc2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxOC0xMC0xOFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTEwLTE4IFVUQy0tMjAxOS0xMS0yNiBVVEMsMTMuMywxCjE3NixTaGFuZWtxYSxGZW1hbGUsMzMsQXNpYW4sQWJzZW50LEFic2VudCxQcmVzZW50LDIwMjAtMDEtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMSwzLDEsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE4LTA2LTA0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDYtMDQgVVRDLS0yMDIwLTAxLTI2IFVUQywxOS43LDEKMTc3LE1vcmV0dGEsRmVtYWxlLDM0LEJsYWNrLFByZXNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA4LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDEsMywzLFByZXNlbnQsVFJVRSxGQUxTRSxsb3csMjAxNy0wOS0wM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA5LTAzIFVUQy0tMjAxOS0wOC0yNiBVVEMsMjMuNywxCjE3OCxTdWxlaWNhLE1hbGUsMzQsQmxhY2ssUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDMtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDEsMywzLDEsQWJzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTYtMDgtMjZUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wOC0yNiBVVEMtLTIwMTktMDMtMjYgVVRDLDMxLDEKMTc5LEt1bWlrbyxNYWxlLDM1LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA2LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMiwzLEFic2VudCxGQUxTRSxGQUxTRSxsb3csMjAxOC0wNC0xOFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTA0LTE4IFVUQy0tMjAxOS0wNi0yNiBVVEMsMTQuMywxCjE4MCxaYWludWIsTWFsZSw1NixXaGl0ZSxQcmVzZW50LEFic2VudCxQcmVzZW50LDIwMjAtMDItMjZUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwxLDMsMiwyLEFic2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTctMTEtMTZUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0xMS0xNiBVVEMtLTIwMjAtMDItMjYgVVRDLDI3LjMsMAoxODEsVmVyc2lhLE1hbGUsMzksQmxhY2ssQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMTAtMjZUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwzLDEsMiwxLFByZXNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTctMDctMDVUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wNy0wNSBVVEMtLTIwMTktMTAtMjYgVVRDLDI3LjcsMAoxODIsRGhhbmUsRmVtYWxlLDQ3LEhpc3BhbmljLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDItMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMiwzLDMsMyxQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE4LTEyLTA1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMTItMDUgVVRDLS0yMDIwLTAyLTI2IFVUQywxNC43LDAKMTgzLE1pbm5ldHRhLE1hbGUsNjIsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMi0yNlQwMDowMDowMFosRkFMU0UsQ29udHJvbCwxLDMsMiwyLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE3LTA3LTIxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDctMjEgVVRDLS0yMDE5LTEyLTI2IFVUQywyOS4yLDAKMTg0LEphcm9uLE1hbGUsNDAsV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMjAtMDEtMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMSwyLDIsMixBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTgtMDEtMTNUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wMS0xMyBVVEMtLTIwMjAtMDEtMjYgVVRDLDI0LjQsMAoxODUsRGFjaGUsRmVtYWxlLDY2LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDEtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDQsMywxLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE4LTA3LTI5VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDctMjkgVVRDLS0yMDIwLTAxLTI2IFVUQywxNy45LDEKMTg2LEphbmF0LE1hbGUsNzIsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA2LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSw0LDMsMyxQcmVzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE3LTExLTA3VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTEtMDcgVVRDLS0yMDE5LTA2LTI2IFVUQywxOS42LDEKMTg3LE1hcmFsb3UsRmVtYWxlLDcxLEFzaWFuLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDMtMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMywzLDIsMSxBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxNi0wOC0yOVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE2LTA4LTI5IFVUQy0tMjAxOS0wMy0yNiBVVEMsMzAuOSwwCjE4OCxSaG9kbmV5LEZlbWFsZSw3MyxXaGl0ZSxBYnNlbnQsUHJlc2VudCxQcmVzZW50LDIwMjAtMDEtMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMywzLDMsMixBYnNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTgtMDctMTZUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wNy0xNiBVVEMtLTIwMjAtMDEtMjYgVVRDLDE4LjMsMAoxODksSmF6bHluZSxNYWxlLDQ2LEhpc3BhbmljLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDgtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDMsMiwyLEFic2VudCxGQUxTRSxGQUxTRSxsb3csMjAxNy0wOS0yMFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA5LTIwIFVUQy0tMjAxOS0wOC0yNiBVVEMsMjMuMiwxCjE5MCxDYW1lcnlubixNYWxlLDU5LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDItMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMiwzLFByZXNlbnQsVFJVRSxUUlVFLGxvdywyMDE5LTAyLTA0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTktMDItMDQgVVRDLS0yMDIwLTAyLTI2IFVUQywxMi44LDEKMTkxLERha290YSxGZW1hbGUsMzQsQmxhY2ssQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDQtMjZUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwzLDIsMiwyLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE3LTExLTA0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTEtMDQgVVRDLS0yMDE5LTA0LTI2IFVUQywxNy43LDAKMTkyLFJhbWxhLE1hbGUsNjksV2hpdGUsUHJlc2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA4LTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDEsMywyLDIsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE3LTEwLTIzVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTAtMjMgVVRDLS0yMDE5LTA4LTI2IFVUQywyMi4xLDAKMTkzLERpY2tpLEZlbWFsZSwyOSxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA3LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDEsMixQcmVzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxOC0wNy0xMlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTA3LTEyIFVUQy0tMjAxOS0wNy0yNiBVVEMsMTIuNSwxCjE5NCxMb2ljLE1hbGUsNDAsV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMjAtMDEtMjZUMDA6MDA6MDBaLEZBTFNFLFRyZWF0bWVudCwyLDEsMSwxLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxNy0wNy0yNlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA3LTI2IFVUQy0tMjAyMC0wMS0yNiBVVEMsMzAsMAoxOTUsRXlncHQsTWFsZSw1OSxIaXNwYW5pYyxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMC0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDEsMyxQcmVzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE3LTA4LTMwVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDgtMzAgVVRDLS0yMDE5LTEwLTI2IFVUQywyNS45LDEKMTk2LE1haXRhLEZlbWFsZSwzMSxIaXNwYW5pYyxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0wNi0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSwzLDMsMixBYnNlbnQsVFJVRSxGQUxTRSxsb3csMjAxOC0wNS0wM1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTA1LTAzIFVUQy0tMjAxOS0wNi0yNiBVVEMsMTMuOCwxCjE5NyxBbGljaGEsRmVtYWxlLDQ1LEFzaWFuLEFic2VudCxQcmVzZW50LEFic2VudCwyMDIwLTAxLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDEsMSwxLFByZXNlbnQsRkFMU0UsVFJVRSxsb3csMjAxOC0wNi0wNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTA2LTA1IFVUQy0tMjAyMC0wMS0yNiBVVEMsMTkuNywxCjE5OCxKYWxlZW4sRmVtYWxlLDQ5LFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTEwLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiw0LDIsMyxBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTctMTAtMDRUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0xMC0wNCBVVEMtLTIwMTktMTAtMjYgVVRDLDI0LjcsMQoxOTksUmlrZWlzaGEsRmVtYWxlLDY4LFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTEtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDEsMywzLEFic2VudCxGQUxTRSxUUlVFLGhpZ2gsMjAxNy0wNi0xMFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA2LTEwIFVUQy0tMjAxOS0xMS0yNiBVVEMsMjkuNSwxCjIwMCxLZW50YXksTWFsZSwyNixIaXNwYW5pYyxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTAzLTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSwxLDMsMixBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTctMDMtMDhUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wMy0wOCBVVEMtLTIwMTktMDMtMjYgVVRDLDI0LjYsMAoyMDEsVGFrd29uLE1hbGUsNTUsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTA1LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDEsMSwxLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE3LTA5LTAxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDktMDEgVVRDLS0yMDE5LTA1LTI2IFVUQywyMC44LDEKMjAyLE5hZGVhbmUsRmVtYWxlLDUyLEhpc3BhbmljLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTEwLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMywxLDIsMyxQcmVzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTgtMDYtMTdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wNi0xNyBVVEMtLTIwMTktMTAtMjYgVVRDLDE2LjMsMQoyMDMsS2FybmVpc2hhLE1hbGUsNDIsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNC0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywyLDIsMyxBYnNlbnQsVFJVRSxUUlVFLGxvdywyMDE3LTEyLTE5VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMTItMTkgVVRDLS0yMDE5LTA0LTI2IFVUQywxNi4yLDEKMjA0LEhlbGVuYW5uLE1hbGUsNTgsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMC0yNlQwMDowMDowMFosRkFMU0UsQ29udHJvbCwzLDMsMywzLEFic2VudCxGQUxTRSxGQUxTRSxtb2RlcmF0ZSwyMDE4LTA4LTI0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDgtMjQgVVRDLS0yMDE5LTEwLTI2IFVUQywxNC4xLDAKMjA1LEphdGh6aXJ5LE1hbGUsNTksV2hpdGUsQWJzZW50LEFic2VudCxQcmVzZW50LDIwMjAtMDItMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMSwzLEFic2VudCxGQUxTRSxUUlVFLG1vZGVyYXRlLDIwMTgtMDYtMTNUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wNi0xMyBVVEMtLTIwMjAtMDItMjYgVVRDLDIwLjQsMQoyMDYsSmV1bmUsTWFsZSw0MyxXaGl0ZSxQcmVzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDktMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDMsMiwxLEFic2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTctMDgtMjFUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wOC0yMSBVVEMtLTIwMTktMDktMjYgVVRDLDI1LjIsMQoyMDcsQ3Jvc3NseW4sRmVtYWxlLDQ1LEhpc3BhbmljLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA0LTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMSwzLDMsMyxQcmVzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxNy0wMS0wN1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTAxLTA3IFVUQy0tMjAxOS0wNC0yNiBVVEMsMjcuNiwxCjIwOCxCb25uYSxGZW1hbGUsMzMsV2hpdGUsQWJzZW50LFByZXNlbnQsUHJlc2VudCwyMDE5LTEyLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMyw0LDMsMixQcmVzZW50LEZBTFNFLEZBTFNFLG1vZGVyYXRlLDIwMTctMTEtMjBUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0xMS0yMCBVVEMtLTIwMTktMTItMjYgVVRDLDI1LjIsMQoyMDksVGFtaWxsYSxNYWxlLDMyLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA5LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMywxLFByZXNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTYtMTEtMTZUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0xMS0xNiBVVEMtLTIwMTktMDktMjYgVVRDLDM0LjMsMQoyMTAsS2VzaGF3bmRhLEZlbWFsZSwyNSxIaXNwYW5pYyxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMS0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDIsMyxBYnNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTgtMDEtMjdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxOC0wMS0yNyBVVEMtLTIwMTktMTEtMjYgVVRDLDIyLDEKMjExLENvcnJlbmEsTWFsZSwzNixXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wMy0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywzLDEsMSxBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE2LTEyLTIxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMTItMjEgVVRDLS0yMDE5LTAzLTI2IFVUQywyNy4yLDEKMjEyLEltbWVyLE1hbGUsNjQsTkEsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNy0yNlQwMDowMDowMFosRkFMU0UsQ29udHJvbCwzLDQsMywxLFByZXNlbnQsVFJVRSxUUlVFLG1vZGVyYXRlLDIwMTYtMDktMjdUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wOS0yNyBVVEMtLTIwMTktMDctMjYgVVRDLDM0LDAKMjEzLE5hb21pZ3JhY2UsTWFsZSw2MyxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTAzLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDIsMiwyLEFic2VudCxGQUxTRSxUUlVFLGxvdywyMDE3LTA3LTE4VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDctMTggVVRDLS0yMDE5LTAzLTI2IFVUQywyMC4zLDEKMjE0LFBhaXRseW5uLEZlbWFsZSwzNCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA2LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDIsMywxLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNi0wOC0xNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE2LTA4LTE1IFVUQy0tMjAxOS0wNi0yNiBVVEMsMzQuNCwxCjIxNSxKYW5uaWNlLE1hbGUsMzUsV2hpdGUsUHJlc2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA3LTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDIsMywzLDIsQWJzZW50LFRSVUUsVFJVRSxsb3csMjAxOC0wNi0xNFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTA2LTE0IFVUQy0tMjAxOS0wNy0yNiBVVEMsMTMuNCwwCjIxNixQaGlsbGlwbWljaGFlbCxGZW1hbGUsNTksV2hpdGUsQWJzZW50LFByZXNlbnQsQWJzZW50LDIwMTktMDctMjZUMDA6MDA6MDBaLEZBTFNFLENvbnRyb2wsMiwzLDMsMixQcmVzZW50LFRSVUUsRkFMU0UsbG93LDIwMTYtMDgtMjlUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wOC0yOSBVVEMtLTIwMTktMDctMjYgVVRDLDM0LjksMAoyMTcsTmVzc2llLE1hbGUsNzAsSGlzcGFuaWMsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA4LTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsNCwzLDIsQWJzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxOC0wMi0yMVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE4LTAyLTIxIFVUQy0tMjAxOS0wOC0yNiBVVEMsMTguMiwwCjIxOCxLZXlzb24sTWFsZSw1MixXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNS0yNlQwMDowMDowMFosRkFMU0UsQ29udHJvbCwyLDQsMywyLEFic2VudCxUUlVFLEZBTFNFLGxvdywyMDE2LTA4LTE2VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMDgtMTYgVVRDLS0yMDE5LTA1LTI2IFVUQywzMy4zLDAKMjE5LEt5YW5uYSxNYWxlLDQ0LEJsYWNrLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTAzLTI2VDAwOjAwOjAwWixUUlVFLENvbnRyb2wsMiwzLDMsMixQcmVzZW50LFRSVUUsVFJVRSxtb2RlcmF0ZSwyMDE4LTAyLTE0VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTgtMDItMTQgVVRDLS0yMDE5LTAzLTI2IFVUQywxMy40LDEKMjIwLExpbGx5dGgsRmVtYWxlLDQzLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA2LTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMSwyLDMsMyxBYnNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTctMDgtMjhUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wOC0yOCBVVEMtLTIwMTktMDYtMjYgVVRDLDIxLjksMAoyMjEsUXVhbmFzaWEsTWFsZSw1OCxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wNS0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMyw0LDIsMixBYnNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE3LTAzLTI4VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDMtMjggVVRDLS0yMDE5LTA1LTI2IFVUQywyNS45LDEKMjIyLFByaXNpY2lsbGEsTWFsZSw0OCxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0xMC0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDIsMywyLDEsQWJzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxNy0xMC0yNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTEwLTI1IFVUQy0tMjAxOS0xMC0yNiBVVEMsMjQsMQoyMjMsVGVyYWppLE1hbGUsMjUsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMi0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDEsMywyLDMsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE3LTA2LTA1VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTctMDYtMDUgVVRDLS0yMDIwLTAyLTI2IFVUQywzMi43LDEKMjI0LEVodWQsRmVtYWxlLDYxLFdoaXRlLEFic2VudCxQcmVzZW50LEFic2VudCwyMDE5LTA5LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDMsMiwxLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxNy0wOC0yNVQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE3LTA4LTI1IFVUQy0tMjAxOS0wOS0yNiBVVEMsMjUsMQoyMjUsSmF5dmEsTWFsZSw1NyxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA4LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDQsMywxLEFic2VudCxUUlVFLFRSVUUsbG93LDIwMTYtMTEtMzBUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0xMS0zMCBVVEMtLTIwMTktMDgtMjYgVVRDLDMyLjksMQoyMjYsUm9zaWxhbmQsRmVtYWxlLDI1LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDIwLTAxLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDEsMiwyLFByZXNlbnQsVFJVRSxGQUxTRSxtb2RlcmF0ZSwyMDE2LTA1LTA5VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMDUtMDkgVVRDLS0yMDIwLTAxLTI2IFVUQyw0NC41LDEKMjI3LEFuYXN0YXNpYSxNYWxlLDUwLFdoaXRlLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDItMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMywyLDIsQWJzZW50LEZBTFNFLEZBTFNFLGxvdywyMDE1LTEyLTA3VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTUtMTItMDcgVVRDLS0yMDIwLTAyLTI2IFVUQyw1MC43LDEKMjI4LEh5ZGVpYSxGZW1hbGUsNTksV2hpdGUsUHJlc2VudCxQcmVzZW50LEFic2VudCwyMDIwLTAxLTI2VDAwOjAwOjAwWixGQUxTRSxUcmVhdG1lbnQsMywxLDMsMixQcmVzZW50LFRSVUUsRkFMU0UsbG93LDIwMTYtMTItMTZUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0xMi0xNiBVVEMtLTIwMjAtMDEtMjYgVVRDLDM3LjMsMAoyMjksS2F5bGVuLE1hbGUsNzEsQmxhY2ssQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOS0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDMsNCwxLDEsQWJzZW50LFRSVUUsRkFMU0UsbW9kZXJhdGUsMjAxNS0xMS0xNlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTExLTE2IFVUQy0tMjAxOS0wOS0yNiBVVEMsNDYuMywxCjIzMCxBbmdlbGVuYSxGZW1hbGUsNjMsQXNpYW4sQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0xMC0yNlQwMDowMDowMFosRkFMU0UsVHJlYXRtZW50LDIsMywzLDIsQWJzZW50LEZBTFNFLFRSVUUsaGlnaCwyMDE1LTA1LTIzVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTUtMDUtMjMgVVRDLS0yMDE5LTEwLTI2IFVUQyw1My4xLDAKMjMxLEVhZG9uLEZlbWFsZSw0NixXaGl0ZSxBYnNlbnQsUHJlc2VudCxOQSwyMDE5LTAzLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDIsMiwxLEFic2VudCxUUlVFLFRSVUUsaGlnaCwyMDE0LTA3LTIxVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTQtMDctMjEgVVRDLS0yMDE5LTAzLTI2IFVUQyw1Ni4yLDEKMjMyLFphaXlhaCxGZW1hbGUsNjcsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNS0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiwyLDIsMyxQcmVzZW50LFRSVUUsVFJVRSxoaWdoLDIwMTYtMDQtMjFUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wNC0yMSBVVEMtLTIwMTktMDUtMjYgVVRDLDM3LjIsMQoyMzMsU2F0aHZpayxNYWxlLDMxLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDIwLTAxLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwyLDQsMywxLEFic2VudCxGQUxTRSxGQUxTRSxoaWdoLDIwMTctMDEtMTJUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNy0wMS0xMiBVVEMtLTIwMjAtMDEtMjYgVVRDLDM2LjUsMQoyMzQsQWRpZGFzLE1hbGUsNTQsV2hpdGUsUHJlc2VudCxBYnNlbnQsQWJzZW50LDIwMTktMTItMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsNCwyLDEsQWJzZW50LEZBTFNFLFRSVUUsbG93LDIwMTUtMDktMDFUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNS0wOS0wMSBVVEMtLTIwMTktMTItMjYgVVRDLDUxLjgsMQoyMzUsRGFuaWFsZSxGZW1hbGUsNzEsV2hpdGUsQWJzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wOC0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMSwyLDIsMSxBYnNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTYtMDMtMDRUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wMy0wNCBVVEMtLTIwMTktMDgtMjYgVVRDLDQxLjcsMQoyMzYsU2FudGV6LE5BLDU5LFdoaXRlLFByZXNlbnQsUHJlc2VudCxQcmVzZW50LDIwMTktMDYtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwyLDMsMiwzLFByZXNlbnQsRkFMU0UsVFJVRSxoaWdoLDIwMTUtMDEtMDlUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNS0wMS0wOSBVVEMtLTIwMTktMDYtMjYgVVRDLDUzLjYsMQoyMzcsSGFya2VyLEZlbWFsZSw0MCxBc2lhbixQcmVzZW50LEFic2VudCxBYnNlbnQsTkEsVFJVRSxOQSwyLDMsMiwyLFByZXNlbnQsVFJVRSxGQUxTRSxoaWdoLDIwMTUtMTAtMjNUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNS0xMC0yMyBVVEMtLU5BLE5BLDEKMjM4LEJyaWNpYSxNYWxlLDQ4LEhpc3BhbmljLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMTktMDUtMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwxLDQsMiwxLEFic2VudCxUUlVFLFRSVUUsaGlnaCwyMDE1LTA5LTEyVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTUtMDktMTIgVVRDLS0yMDE5LTA1LTI2IFVUQyw0NC41LDEKMjM5LFJleWFhbnNoLEZlbWFsZSw1NSxXaGl0ZSxBYnNlbnQsQWJzZW50LFByZXNlbnQsMjAxOS0wOS0yNlQwMDowMDowMFosRkFMU0UsQ29udHJvbCwxLDIsMiwyLEFic2VudCxUUlVFLEZBTFNFLGhpZ2gsMjAxNS0xMi0wOFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTEyLTA4IFVUQy0tMjAxOS0wOS0yNiBVVEMsNDUuNiwwCjI0MCxEZWtseW4sTWFsZSw2MCxXaGl0ZSxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAyLTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDEsMywyLDIsUHJlc2VudCxUUlVFLEZBTFNFLG1vZGVyYXRlLDIwMTYtMDUtMDJUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wNS0wMiBVVEMtLTIwMjAtMDItMjYgVVRDLDQ1LjgsMAoyNDEsVHJlc3RpbixNYWxlLDQ4LEhpc3BhbmljLEFic2VudCxBYnNlbnQsQWJzZW50LDIwMjAtMDItMjZUMDA6MDA6MDBaLFRSVUUsQ29udHJvbCwzLDQsMiwxLFByZXNlbnQsVFJVRSxUUlVFLGhpZ2gsMjAxNi0wNi0yMFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE2LTA2LTIwIFVUQy0tMjAyMC0wMi0yNiBVVEMsNDQuMiwxCjI0MixUYXlsYW4sTWFsZSw0MCxIaXNwYW5pYyxBYnNlbnQsQWJzZW50LEFic2VudCwyMDIwLTAyLTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsMiwzLDMsUHJlc2VudCxUUlVFLFRSVUUsbG93LDIwMTUtMTItMThUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNS0xMi0xOCBVVEMtLTIwMjAtMDItMjYgVVRDLDUwLjMsMAoyNDMsRWx5enphLEZlbWFsZSw1NyxXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAxOS0wNy0yNlQwMDowMDowMFosVFJVRSxDb250cm9sLDMsMiwzLDEsUHJlc2VudCxUUlVFLFRSVUUsbW9kZXJhdGUsMjAxNS0wNi0yOFQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTA2LTI4IFVUQy0tMjAxOS0wNy0yNiBVVEMsNDguOSwxCjI0NCxLcmlzc2FuZHJhLE1hbGUsNTAsQmxhY2ssQWJzZW50LEFic2VudCxQcmVzZW50LDIwMTktMDMtMjZUMDA6MDA6MDBaLFRSVUUsVHJlYXRtZW50LDMsMiwzLDEsQWJzZW50LFRSVUUsRkFMU0UsaGlnaCwyMDE2LTA0LTA4VDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTYtMDQtMDggVVRDLS0yMDE5LTAzLTI2IFVUQywzNS42LDEKMjQ1LEtpbmRzYXksTWFsZSwyNSxXaGl0ZSxQcmVzZW50LEFic2VudCxBYnNlbnQsMjAyMC0wMi0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMiw0LDMsMixQcmVzZW50LFRSVUUsRkFMU0UsbG93LDIwMTYtMTItMDNUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0xMi0wMyBVVEMtLTIwMjAtMDItMjYgVVRDLDM4LjgsMQoyNDYsUmFpbHlubmUsRmVtYWxlLDI4LEhpc3BhbmljLFByZXNlbnQsQWJzZW50LEFic2VudCwyMDE5LTA0LTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsMywzLDMsUHJlc2VudCxUUlVFLFRSVUUsaGlnaCwyMDE0LTA2LTIyVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTQtMDYtMjIgVVRDLS0yMDE5LTA0LTI2IFVUQyw1OC4xLDAKMjQ3LERhbml4YSxNYWxlLDcyLFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDIwLTAxLTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwxLDIsMywyLFByZXNlbnQsRkFMU0UsRkFMU0UsaGlnaCwyMDE1LTA5LTEzVDAwOjAwOjAwWixNb3JlVGhhbjFZZWFyLDIwMTUtMDktMTMgVVRDLS0yMDIwLTAxLTI2IFVUQyw1Mi40LDEKMjQ4LFNhbSxNYWxlLDU0LFdoaXRlLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTEwLTI2VDAwOjAwOjAwWixGQUxTRSxDb250cm9sLDMsNCwzLDIsQWJzZW50LEZBTFNFLEZBTFNFLGhpZ2gsMjAxNS0wMS0yN1QwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTAxLTI3IFVUQy0tMjAxOS0xMC0yNiBVVEMsNTcsMAoyNDksTnlsZW4sTWFsZSw2NixXaGl0ZSxBYnNlbnQsUHJlc2VudCxBYnNlbnQsMjAxOS0xMi0yNlQwMDowMDowMFosVFJVRSxUcmVhdG1lbnQsMywyLDIsMyxBYnNlbnQsRkFMU0UsRkFMU0UsbG93LDIwMTYtMDgtMTFUMDA6MDA6MDBaLE1vcmVUaGFuMVllYXIsMjAxNi0wOC0xMSBVVEMtLTIwMTktMTItMjYgVVRDLDQwLjUsMQoyNTAsSmFyaWNrYSxNYWxlLDM0LEhpc3BhbmljLEFic2VudCxBYnNlbnQsUHJlc2VudCwyMDE5LTA4LTI2VDAwOjAwOjAwWixUUlVFLFRyZWF0bWVudCwzLDQsMSwxLEFic2VudCxGQUxTRSxGQUxTRSxsb3csMjAxNS0wMS0yNlQwMDowMDowMFosTW9yZVRoYW4xWWVhciwyMDE1LTAxLTI2IFVUQy0tMjAxOS0wOC0yNiBVVEMsNTUsMQo=" download="histopathology-template2020-02-26.xlsx.csv">
<button class="btn btn-default"><i class="fa fa-save"></i> Download data as csv</button>
</a><!--/html_preserve-->

```r
mydata %>% downloadthis::download_this(output_name = excelName, output_extension = ".xlsx", 
    button_label = "Download data as xlsx", button_type = "primary")
```

<!--html_preserve--><a href="data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,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" download="histopathology-template2020-02-26.xlsx.xlsx">
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
 [19] farver_2.0.3        rprojroot_1.3-2     vctrs_0.2.3        
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

Last update on $ 2020-04-18 13:58:57 $  

[Serdar Balci, MD, Pathologist](https://www.serdarbalci.com/)  
serdarbalci@serdarbalci.com  
https://rpubs.com/sbalci/CV   
https://github.com/sbalci  
https://sbalci.github.io/  
[Patoloji Notları](http://www.patolojinotlari.com/)  
[ParaPathology](http://www.parapathology.com/)  
https://twitter.com/serdarbalci  


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

source(file = here::here("R", "force_git.R"))
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
library(knitr)
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})
# linewidth css
  pre:not([class]) {
    color: #333333;
    background-color: #cccccc;
  }
# linewidth css
pre.jamovitable{
  color:black;
  background-color: white;
  margin-bottom: 35px;  
}
 jtable<-function(jobject,digits=3) {
  snames<-sapply(jobject$columns,function(a) a$title)
  asDF<-jobject$asDF
  tnames<-unlist(lapply(names(asDF) ,function(n) snames[[n]]))
  names(asDF)<-tnames
  kableExtra::kable(asDF,"html",
                    table.attr='class="jmv-results-table-table"',
                    row.names = F,
                    digits=3)
}
# https://cran.r-project.org/web/packages/exploreR/vignettes/exploreR.html
# exploreR::reset()
Block rmdnote

Block rmdtip

Block warning

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


# iris %>% explore::data_dict_md(output_dir = here::here())
# description <- data.frame(
#                   variable = c("Species"), 
#                   description = c("Species of Iris flower"))

# explore::data_dict_md(data = mydata, 
#              title = "Data Set", 
#              # description =  description, 
#              output_file = "data_dict.md",
#              output_dir = here::here("out"))
mydata <- janitor::clean_names(mydata)
# cat(names(mydata), sep = ",\n")
# names(mydata) <- c(names(mydata)[1:21], paste0("Soru", 1:30))
iris %>% 
  explore::clean_var(data = ., 
                     var = Sepal.Length,  
            min_val = 4.5, 
            max_val = 7.0, 
            na = 5.8, 
            name = "sepal_length") %>% 
  describe()
summarytools::view(summarytools::dfSummary(mydata))
dplyr::glimpse(mydata)
library(finalfit)
# https://www.datasurg.net/2019/10/15/jama-retraction-after-miscoding-new-finalfit-function-to-check-recoding/
# intentionally miscoded
colon_s %>%
  mutate(
    sex.factor2 = forcats::fct_recode(sex.factor,
      "F" = "Male",
      "M" = "Female")
  ) %>%
  count(sex.factor, sex.factor2)
# Install
# devtools::install_github('ewenharrison/finalfit')
library(finalfit)
library(dplyr)
# Recode example
colon_s_small = colon_s %>%
  select(-id, -rx, -rx.factor) %>%
  mutate(
    age.factor2 = forcats::fct_collapse(age.factor,
      "<60 years" = c("<40 years", "40-59 years")),
    sex.factor2 = forcats::fct_recode(sex.factor,
    # Intentional miscode
      "F" = "Male",
      "M" = "Female")
  )
# Check
colon_s_small %>%
  finalfit::check_recode()
out = colon_s_small %>%
  select(-extent, -extent.factor,-time, -time.years) %>% # choose to exclude variables
  check_recode(include_numerics = TRUE)
## Recoding mydata$cinsiyet into mydata$Cinsiyet
mydata$Cinsiyet <- recode(mydata$cinsiyet,
               "K" = "Kadin",
               "E" = "Erkek")
mydata$Cinsiyet <- factor(mydata$Cinsiyet)
## Recoding mydata$tumor_yerlesimi into mydata$TumorYerlesimi
mydata$TumorYerlesimi <- recode(mydata$tumor_yerlesimi,
               "proksimal" = "Proksimal",
               "distal" = "Distal",
               "yaygın" = "Yaygin",
               "gö bileşke" = "GEJ",
               "antrum" = "Antrum")
mydata$TumorYerlesimi <- factor(mydata$TumorYerlesimi)

## Reordering mydata$TumorYerlesimi
mydata$TumorYerlesimi <- factor(mydata$TumorYerlesimi, levels=c("GEJ", "Proksimal", "Antrum", "Distal", "Yaygin"))
## Recoding mydata$histolojik_alt_tip into mydata$HistolojikAltTip
mydata$HistolojikAltTip <- recode(mydata$histolojik_alt_tip,
               "medüller benzeri" = "meduller benzeri")
mydata$HistolojikAltTip <- factor(mydata$HistolojikAltTip)

## Recoding mydata$lauren_siniflamasi into mydata$Lauren
mydata$Lauren <- recode(mydata$lauren_siniflamasi,
               "diffüz" = "diffuse",
               "???" = "medullary")
mydata$Lauren <- factor(mydata$Lauren)

## Recoding mydata$histolojik_derece into mydata$Grade
mydata$Grade <- recode(mydata$histolojik_derece,
               "az diferansiye" = "az",
               "iyi diferansiye" = "iyi",
               "orta diferansiye" = "orta")
mydata$Grade <- factor(mydata$Grade)

## Reordering mydata$Grade
mydata$Grade <- factor(mydata$Grade, levels=c("iyi", "orta", "az"))
mydata$Tstage <- stringr::str_match(mydata$patolojik_evre, paste('(.+)', "N", sep=''))[,2]

mydata$Nstage <- paste0("N",
    stringr::str_match(mydata$patolojik_evre, paste( "N", '(.+)', "M", sep=''))[,2]
    )

mydata$Mstage <- paste0("M", 
    stringr::str_match(mydata$patolojik_evre, paste("M", '(.+)', sep=''))[,2]
)
mydata <- mydata %>% 
    dplyr::mutate(
        T_stage = dplyr::case_when(
            grepl(pattern = "T1", x = .$Tstage) == TRUE ~ "T1",
            grepl(pattern = "T2", x = .$Tstage) == TRUE ~ "T2",
            grepl(pattern = "T3", x = .$Tstage) == TRUE ~ "T3",
            grepl(pattern = "T4", x = .$Tstage) == TRUE ~ "T4",
            TRUE ~ "Tx"
        )
    ) %>% 
dplyr::mutate(
        N_stage = dplyr::case_when(
            grepl(pattern = "N0", x = .$Nstage) == TRUE ~ "N0",
            grepl(pattern = "N1", x = .$Nstage) == TRUE ~ "N1",
            grepl(pattern = "N2", x = .$Nstage) == TRUE ~ "N2",
            grepl(pattern = "N3", x = .$Nstage) == TRUE ~ "N3",
            TRUE ~ "Nx"
        )
    ) %>% 
dplyr::mutate(
        M_stage = dplyr::case_when(
            grepl(pattern = "M0", x = .$Mstage) == TRUE ~ "M0",
            grepl(pattern = "M1", x = .$Mstage) == TRUE ~ "M1",
            TRUE ~ "Mx"
        )
    )


## Recoding mydata$cd44_oran into mydata$CD44
mydata$CD44 <- recode(mydata$cd44_oran,
               "2" = "positive",
               "0" = "negative",
               "1" = "negative",
               "3" = "positive")
mydata$CD44 <- factor(mydata$CD44)

## Recoding mydata$her2_skor into mydata$Her2
mydata$Her2 <- recode(mydata$her2_skor,
               "+3" = "3",
               "+1" = "1",
               "+2" = "2")
mydata$Her2 <- factor(mydata$Her2)
## Reordering mydata$Her2
mydata$Her2 <- factor(mydata$Her2, levels=c("0", "1", "2", "3"))
## Recoding mydata$msi into mydata$MMR
mydata$MMR <- recode(mydata$msi,
               "MSS" = "pMMR",
               "MSİ(PMS2,MLH1)" = "dMMR(PMS2,MLH1)",
               "MSİ(MSH2,MSH6)" = "dMMR(MSH2,MSH6)",
               "MSİ(PMS2)" = "dMMR(PMS2)")
mydata$MMR <- factor(mydata$MMR)

## Recoding mydata$msi into mydata$MMR2
mydata$MMR2 <- recode(mydata$msi,
               "MSS" = "pMMR",
               "MSİ(PMS2,MLH1)" = "dMMR",
               "MSİ(MSH2,MSH6)" = "dMMR",
               "MSİ(PMS2)" = "dMMR")
mydata$MMR2 <- factor(mydata$MMR2)


mydata <- mydata %>% 
    dplyr::mutate(
TumorPDL1gr1 = dplyr::case_when(
        t_pdl1 < 1 ~ "kucuk1",
        t_pdl1 >= 1 ~ "buyukesit1"
    )
    ) %>% 
dplyr::mutate(
TumorPDL1gr5 = dplyr::case_when(
        t_pdl1 < 5 ~ "kucuk5",
        t_pdl1 >= 5 ~ "buyukesit5"
    )
    )   %>% 
dplyr::mutate(
inflPDL1gr1 = dplyr::case_when(
        i_pdl1 < 1 ~ "kucuk1",
        i_pdl1 >= 1 ~ "buyukesit1"
    )
    ) %>% 
dplyr::mutate(
inflPDL1gr5 = dplyr::case_when(
        i_pdl1 < 5 ~ "kucuk5",
        i_pdl1 >= 5 ~ "buyukesit5"
    )
    )

## Recoding mydata$lvi into mydata$LVI
mydata$LVI <- recode(mydata$lvi,
               "var" = "Var",
               "yok" = "Yok")
mydata$LVI <- factor(mydata$LVI)
## Reordering mydata$LVI
mydata$LVI <- factor(mydata$LVI, levels=c("Yok", "Var"))
## Recoding mydata$pni into mydata$PNI
mydata$PNI <- recode(mydata$pni,
               "var" = "Var",
               "yok" = "Yok")
mydata$PNI <- factor(mydata$PNI)
## Reordering mydata$PNI
mydata$PNI <- factor(mydata$PNI, levels=c("Yok", "Var"))
## Recoding mydata$ln into mydata$LenfNoduMetastazi
mydata$LenfNoduMetastazi <- recode(mydata$ln,
               "var" = "Var",
               "yok" = "Yok")
mydata$LenfNoduMetastazi <- factor(mydata$LenfNoduMetastazi)
## Reordering mydata$LenfNoduMetastazi
mydata$LenfNoduMetastazi <- factor(mydata$LenfNoduMetastazi, levels=c("Yok", "Var"))
mydata$sontarih <- janitor::excel_numeric_to_date(as.numeric(mydata$olum_tarihi))
mydata$Outcome <- "Dead"
mydata$Outcome[mydata$olum_tarihi == "yok"] <- "Alive"
# cat(names(mydata), sep = ",\n")

mydata <- mydata %>% 
    select(
# sira_no,
# no,
# x3,
# hasta_biyopsi_no,
# cinsiyet,
        Cinsiyet,
        Yas = hasta_yasi,
        TumorYerlesimi,
        TumorCapi = tumor_capi,
HistolojikAltTip,
Lauren,
Grade,
TNM = patolojik_evre,
Tstage,
T_stage,
Nstage,
N_stage,
Mstage,
M_stage,
CD44,
Her2,
MMR,
MMR2,
TumorPDL1gr1,
TumorPDL1gr5,
inflPDL1gr1,
inflPDL1gr5,
LVI,
PNI,
LenfNoduMetastazi,
Outcome,        
# tumor_yerlesimi,

# histolojik_alt_tip,
# lauren_siniflamasi,
# histolojik_derece,
# cd44_oran,
# cd44_intense,
# her2_skor,
# msi,
# t_pdl1,
# i_pdl1,
# lvi,
# pni,
# ln,
CerrahiTarih = cerrahi_tarih,
# olum_tarihi,
genel_sagkalim,
SonTarih = sontarih
    )

mydata <- janitor::clean_names(mydata)
# cat(names(mydata), sep = ",\n")

names(mydata) <- c(names(mydata)[1:21], paste0("Soru", 1:30))

library(arsenal)
tab1 <- tableby(~ katilim_durumu
                ,
                data = mydata
)
summary(tab1)
mydata <- mydata %>% 
  filter(katilim_durumu == "katılmış ve tamamlamış")
# summarytools::view(summarytools::dfSummary(mydata))
# dplyr::glimpse(mydata)
# mydata %>%
#   select(starts_with("Soru")) %>% 
#   pivot_longer(everything()) %>% 
#   select(value) %>% 
#   pull() %>% 
#   unique() %>% 
#   cat(sep = "\n")
## Recoding mydata$x3_yasiniz_nedir into mydata$YasGrup
mydata$YasGrup <- factor(mydata$x3_yasiniz_nedir)
## Reordering mydata$YasGrup
mydata$YasGrup <- factor(mydata$YasGrup, levels=c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"))
## Recoding mydata$x4_cinsiyetiniz_nedir into mydata$Cinsiyet
mydata$Cinsiyet <- recode(mydata$x4_cinsiyetiniz_nedir,
               "Kadın" = "Kadin")
mydata$Cinsiyet <- factor(mydata$Cinsiyet)

## Recoding mydata$x5_kac_yildir_genel_cerrahi_uzmanisiniz into mydata$UzmanlikSuresi
mydata$UzmanlikSuresi <- recode(mydata$x5_kac_yildir_genel_cerrahi_uzmanisiniz,
               "43739" = "10-19")
mydata$UzmanlikSuresi <- factor(mydata$UzmanlikSuresi)

## Reordering mydata$UzmanlikSuresi
mydata$UzmanlikSuresi <- factor(mydata$UzmanlikSuresi, levels=c("0-9", "10-19", "20-29", "30-39", "40-49"))


## Recoding mydata$x6_unvaniniz_nedir into mydata$Unvan
mydata$Unvan <- factor(mydata$x6_unvaniniz_nedir)

## Reordering mydata$Unvan
mydata$Unvan <- factor(mydata$Unvan, levels=c("Op.Dr.", "Doktor Öğretim Üyesi", "Doç.Dr.", "Prof.Dr"))

## Recoding mydata$x8_hangi_kurumda_calisiyorsunuz into mydata$Kurum
mydata$Kurum <- recode(mydata$x8_hangi_kurumda_calisiyorsunuz,
               "Eğitim Araştırma Hastanesi" = "Eğitim Araştırma",
               "İlçe Devlet Hastanesi" = "İlçe Devlet",
               "Üniversite Hastanesi" = "Üniversite",
               "İl Devlet Hastanesi" = "İl Devlet",
               "Özel Hastane ve Kurumlar" = "Özel")
mydata$Kurum <- factor(mydata$Kurum)

## Reordering mydata$Kurum
mydata$Kurum <- factor(mydata$Kurum, levels=c("Özel", "İlçe Devlet", "İl Devlet", "Eğitim Araştırma", "Üniversite"))
tersSorular <- c("Soru1",
                 "Soru4",
                 "Soru15",
                 "Soru17",
                 "Soru29")

CSS <- c(
  "Soru3",
  "Soru6",
  "Soru12",
  "Soru16",
  "Soru18",
  "Soru20",
  "Soru22",
  "Soru24",
  "Soru27",
  "Soru30"
)


BS <- c(
  "Soru1",
  "Soru4",
  "Soru8",
  "Soru10",
  "Soru15",
  "Soru17",
  "Soru19",
  "Soru21",
  "Soru26",
  "Soru29"
)


STSS <- c(
  "Soru2",
  "Soru5",
  "Soru7",
  "Soru9",
  "Soru11",
  "Soru13",
  "Soru14",
  "Soru23",
  "Soru25",
  "Soru28"
)
recode_numberize <- function(x, ...) {
  dplyr::recode(
    x,
    "Bazı zamanlar" = 3,
    "Çoksık" = 5,
    "Hiçbir zaman" = 1,
    "Nadiren" = 2,
    "Sık sık" = 4,
    "Sıkça" = 4,
"Bazı zamanlarda" = 3,
"Çok sık" = 5,
"Sıksık" = 4
    )
}


mydata <- mydata %>% 
    mutate_at(.tbl = .,
              .vars = vars(starts_with("Soru"), -tersSorular),
              .funs = recode_numberize
    )



recode_numberize_ters <- function(x, ...) {
  recode(
    x,
    "Bazı zamanlar" = 3,
"Çoksık" = 1,
"Hiçbir zaman" = 5,
"Nadiren" = 4,
"Sık sık" = 2,
"Sıkça" = 2,
"Bazı zamanlarda" = 3,
"Çok sık" = 1,
"Sıksık" = 2
        )
}


mydata <- mydata %>% 
    mutate_at(.tbl = .,
              .vars = vars(tersSorular),
              .funs = recode_numberize
    )

mydata <- mydata %>% 
  # böyle yazınca missing olunca hesaplamıyor
  # mutate(
  #   CSS_total = rowSums(select(., CSS), na.rm = FALSE)
  # ) %>% 
  mutate(
    CSS_total = rowSums(select(., CSS), na.rm = TRUE)
  ) %>% 
mutate(
    BS_total = rowSums(select(., BS), na.rm = TRUE)
  ) %>% 
  mutate(
    STSS_total = rowSums(select(., STSS), na.rm = TRUE)
  )


mydata <- mydata %>% 
  naniar::replace_with_na_at(
    .vars = vars(ends_with("_total")),
    condition = ~.x == 0
    )

mydata <- mydata %>% 
  mutate_at(.tbl = .,
            .vars = vars(ends_with("_total")),
            .funs = list(Gr = 
                           ~ case_when(
                             . <= 22 ~ "Low",
                             . >= 23 & . <= 41 ~ "Average",
                             . >= 42 ~ "High",
                             TRUE ~ NA_character_
                           )
                           )
    
  ) %>% 
  mutate_at(.tbl = .,
            .vars = vars(ends_with("_Gr")),
            .funs = ~ factor(., levels=c("Low", "Average", "High"))
              )

# ## Reordering mydata$CSS_total_Gr
# mydata$CSS_total_Gr <- factor(mydata$CSS_total_Gr, )
# 
# ## Reordering mydata$BS_total_Gr
# mydata$BS_total_Gr <- factor(mydata$BS_total_Gr, levels=c("Low", "Average", "High"))
# 
# 
# ## Reordering mydata$STSS_total_Gr
# mydata$STSS_total_Gr <- factor(mydata$STSS_total_Gr, levels=c("Low", "Average", "High"))

visdat::vis_miss(mydata)
visdat::vis_miss(airquality,
                 cluster = TRUE)
visdat::vis_miss(airquality,
         sort_miss = TRUE)
# https://cran.r-project.org/web/packages/dlookr/vignettes/transformation.html

income <- dlookr::imputate_na(carseats, Income, US, method = "rpart")
income
attr(income,"var_type")
attr(income,"method")
attr(income,"na_pos")
attr(income,"type")
attr(income,"message")
attr(income,"success")
attr(income,"class")

summary(income)

plot(income)
carseats %>%
  mutate(Income_imp = dlookr::imputate_na(carseats, Income, US, method = "knn")) %>%
  group_by(US) %>%
  summarise(orig = mean(Income, na.rm = TRUE),
    imputation = mean(Income_imp))
library(mice)
urban <- dlookr::imputate_na(carseats, Urban, US, method = "mice")
urban 
summary(urban)
plot(urban)
price <- dlookr::imputate_outlier(carseats, Price, method = "capping")
price
summary(price)
plot(price)
carseats %>%
  mutate(Price_imp = dlookr::imputate_outlier(carseats, Price, method = "capping")) %>%
  group_by(US) %>%
  summarise(orig = mean(Price, na.rm = TRUE),
    imputation = mean(Price_imp, na.rm = TRUE))
carseats %>% 
  mutate(Income_minmax = dlookr::transform(carseats$Income, method = "minmax"),
    Sales_minmax = dlookr::transform(carseats$Sales, method = "minmax")) %>% 
  select(Income_minmax, Sales_minmax) %>% 
  boxplot()
dlookr::find_skewness(carseats)

dlookr::find_skewness(carseats, index = FALSE)

dlookr::find_skewness(carseats, value = TRUE)

dlookr::find_skewness(carseats, value = TRUE, thres = 0.1)
Advertising_log = transform(carseats$Advertising, method = "log")
# Advertising_log <- transform(carseats$Advertising, method = "log+1")
head(Advertising_log)
summary(Advertising_log)
plot(Advertising_log)
bin <- dlookr::binning(carseats$Income)
bin <- binning(carseats$Income, nbins = 4,
              labels = c("LQ1", "UQ1", "LQ3", "UQ3"))
binning(carseats$Income, nbins = 5, type = "equal")
binning(carseats$Income, nbins = 5, type = "pretty")
binning(carseats$Income, nbins = 5, type = "kmeans")
binning(carseats$Income, nbins = 5, type = "bclust")

bin
summary(bin)
plot(bin)



carseats %>%
 mutate(Income_bin = dlookr::binning(carseats$Income)) %>%
 group_by(ShelveLoc, Income_bin) %>%
 summarise(freq = n()) %>%
 arrange(desc(freq)) %>%
 head(10)

bin <- dlookr::binning_by(carseats, "US", "Advertising")
bin
summary(bin)
attr(bin, "iv") # information value 
attr(bin, "ivtable") # information value table

plot(bin, sub = "bins of Advertising variable")

# https://cran.r-project.org/web/packages/exploreR/vignettes/exploreR.html

(regressResults <- exploreR::masslm(iris,
                                   "Sepal.Length",
                                   ignore = "Species")
)
 
exploreR::massregplot(iris, "Sepal.Length", ignore = "Species")

(stand.Petals <- exploreR::standardize(iris,
                                      c("Petal.Width", "Petal.Length"))
)
carseats %>%
  dlookr::transformation_report(target = US)

carseats %>%
  dlookr::transformation_report(target = US, output_format = "html", 
    output_file = "transformation.html")

inspectdf::inspect_na(starwars)

inspectdf::inspect_na(starwars) %>% inspectdf::show_plot()

inspectdf::inspect_na(star_1, star_2)

inspectdf::inspect_na(star_1, star_2) %>% inspectdf::show_plot()
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
# dependent <- c("dependent1",
#                "dependent2"
#               )

# explanatory <- c("explanatory1",
#                  "explanatory2"
#                  )

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


library("rmngb")
# rmngb::pairwise.chisq.test(mydata$StageGr2, mydata$Ki67Gr)
rmngb::pairwise.fisher.test(mydata$StageGr2, mydata$Ki67Gr)

# rmngb::pairwise.chisq.test(mydata$LiverDistantMets, mydata$Ki67Gr, p.adj = "BH")
rmngb::pairwise.fisher.test(mydata$LiverDistantMets, mydata$Ki67Gr, p.adj = "BH")
# rmngb::pairwise.chisq.test(mydata$PNI, mydata$Ki67Gr, p.adj = "BH")
rmngb::pairwise.fisher.test(mydata$PNI, mydata$Ki67Gr, p.adj = "BH")
# rmngb::pairwise.chisq.test(mydata$LVI, mydata$Ki67Gr, p.adj = "BH")
rmngb::pairwise.fisher.test(mydata$LVI, mydata$Ki67Gr, p.adj = "BH")
MBStudy <- 
tibble::tribble(
           ~Grup,                           ~Diagnosis,   ~Number,
   "\"Grup1\"",           "\"Diseased\"", 1383L,
  "\"Grup2A\"",           "\"Diseased\"",   58L,
  "\"Grup2B\"",           "\"Diseased\"",  349L,
   "\"Grup3\"",           "\"Diseased\"", 5217L,
   "\"Grup1\"", "\"Stromal   Diseased\"",   13L,
  "\"Grup2A\"", "\"Stromal   Diseased\"",    2L,
  "\"Grup2B\"", "\"Stromal   Diseased\"",   47L,
   "\"Grup3\"", "\"Stromal   Diseased\"",  476L,
   "\"Grup1\"",   "\"Inflammation fibrosis\"",   56L,
  "\"Grup2A\"",   "\"Inflammation fibrosis\"",   52L,
  "\"Grup2B\"",   "\"Inflammation fibrosis\"",  267L,
   "\"Grup3\"",   "\"Inflammation fibrosis\"", 1387L
  )


MBStudy <- 
  tibble::tribble(
    ~Grup,                           ~Diagnosis,   ~Number,
    "\"Grup1\"",           "\"Diseased\"", 1383L,
    "\"Grup2A\"",           "\"Diseased\"",   58L,
    "\"Grup2B\"",           "\"Diseased\"",  349L,
    "\"Grup3\"",           "\"Diseased\"", 5217L,
    "\"Grup1\"", "\"Stromal   Diseased\"",   13L,
    "\"Grup2A\"", "\"Stromal   Diseased\"",    2L,
    "\"Grup2B\"", "\"Stromal   Diseased\"",   47L,
    "\"Grup3\"", "\"Stromal   Diseased\"",  476L,
    "\"Grup1\"",   "\"Inflammation fibrosis\"",   56L,
    "\"Grup2A\"",   "\"Inflammation fibrosis\"",   52L,
    "\"Grup2B\"",   "\"Inflammation fibrosis\"",  267L,
    "\"Grup3\"",   "\"Inflammation fibrosis\"", 1387L
  )


MBStudy <- 
data.frame(
  stringsAsFactors = FALSE,
                V1 = c("\"Grup1\"","\"Grup2A\"",
                       "\"Grup2B\"","\"Grup3\"","\"Grup1\"","\"Grup2A\"",
                       "\"Grup2B\"","\"Grup3\"","\"Grup1\"","\"Grup2A\"",
                       "\"Grup2B\"","\"Grup3\""),
                V2 = c("\"Diseased\"",
                       "\"Diseased\"","\"Diseased\"","\"Diseased\"",
                       "\"Stromal   Diseased\"","\"Stromal   Diseased\"",
                       "\"Stromal   Diseased\"",
                       "\"Stromal   Diseased\"","\"Inflammation fibrosis\"",
                       "\"Inflammation fibrosis\"","\"Inflammation fibrosis\"",
                       "\"Inflammation fibrosis\""),
                V3 = c(1383L,58L,349L,5217L,13L,
                       2L,47L,476L,56L,52L,267L,1387L)
)

MBStudy <- matrix(c(
1383L,                    13L,                    56L,
58L,                     2L,                    52L,
349L,                    47L,                   267L,
5217L,                   476L,                  1387L
  ), byrow = TRUE, nrow = 4, dimnames = list(c("Grup1", "Grup2A", "Grup2B", "Grup3"), c("Diseased", "Stromal Diseased", "Inflammation")))


RVAideMemoire::chisq.multcomp(MBStudy)
MBStudy
MB_table <- RVAideMemoire::fisher.multcomp(tab.cont = MBStudy)

MB_table$p.value %>% 
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Grup") %>% 
  gt::gt(.) %>% 
  gt::fmt_number(., columns = dplyr::contains("Diseased"), decimals = 4)

rmngb::pairwise.fisher.test.table(MBStudy)
  
MBStudy2 <- matrix(c(
13L, 	53L,
9L,	5L,
3L,	26L),
byrow = TRUE,
nrow = 3,
dimnames = list(
c("Diseased", "Inflammation", "Fibrosis"),
c("sw", "cds")
))

MBStudy2

MBStudy2_analysis <-  RVAideMemoire::fisher.multcomp(tab.cont = t(MBStudy2))

MBStudy2_analysis$p.value

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
dependent <- c("dependent1",
               "dependent2"
                 )

explanatory <- c("explanatory1",
                 "explanatory2"
                 )
mydataCategorical <- mydata %>% 
    select(-var1,
           -var2
    )
mydataCategorical_variable <- explanatory[1]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[2]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[3]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[4]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[5]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[6]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[7]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[8]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[9]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[10]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[11]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[12]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[13]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[14]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[15]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
mydataCategorical_variable <- NA
dependent2 <- NA
mydataCategorical_variable <- explanatory[16]
dependent2 <- dependent[!dependent %in% mydataCategorical_variable]
source(here::here("R", "gc_plot_cat.R"))
## column chart
SmartEDA::ExpCatViz(
  Carseats,
  target = "Urban",
  fname = NULL,
  clim = 10,
  col = NULL,
  margin = 2,
  Page = c(2, 1),
  sample = 2
)
## Stacked bar graph
SmartEDA::ExpCatViz(
  Carseats,
  target = "Urban",
  fname = NULL,
  clim = 10,
  col = NULL,
  margin = 2,
  Page = c(2, 1),
  sample = 2
)
## Variable importance graph using information values
SmartEDA::ExpCatStat(
  Carseats,
  Target = "Urban",
  result = "Stat",
  Pclass = "Yes",
  plot = TRUE,
  top = 20,
  Round = 2
)
inspectdf::inspect_cat(starwars) %>% inspectdf::show_plot()
inspectdf::inspect_cat(starwars) %>% 
  inspectdf::show_plot(high_cardinality = 1)
inspectdf::inspect_cat(star_1, star_2) %>% inspectdf::show_plot()
# mydataContinious
mydata %>%
    select(institution, starts_with("Slide")) %>%
    pivot_longer(cols = starts_with("Slide")) %>%
    ggplot(., aes(name, value)) -> p
p + geom_jitter() 
p + geom_jitter(aes(colour = institution)) 
dxchanges <- mydata %>%
    select(bx_no, starts_with("Slide")) %>% 
    filter(complete.cases(.)) %>%
    group_by(Slide1_infiltrative, Slide2_Medium, Slide3_Demarcated) %>% 
    tally()

library(ggalluvial)

ggplot(data = dxchanges,
       aes(axis1 = Slide1_infiltrative, axis2 = Slide2_Medium, axis3 = Slide3_Demarcated,
           y = n)) +
  scale_x_discrete(limits = c("Slide1", "Slide2", "Slide3"),
                   expand = c(.1, .05)
                   ) +
  xlab("Slide") +
  geom_alluvium(aes(fill = Slide1_infiltrative,
                    colour = Slide1_infiltrative
                    )) +
  geom_stratum() +
  geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  ggtitle("PanNET")

## Generate Boxplot by category
SmartEDA::ExpNumViz(
  mtcars,
  target = "gear",
  type = 2,
  nlim = 25,
  fname = file.path(here::here(), "Mtcars2"),
  Page = c(2, 2)
)
## Generate Density plot
SmartEDA::ExpNumViz(
  mtcars,
  target = NULL,
  type = 3,
  nlim = 25,
  fname = file.path(here::here(), "Mtcars3"),
  Page = c(2, 2)
)
## Generate Scatter plot
SmartEDA::ExpNumViz(
  mtcars,
  target = "carb",
  type = 3,
  nlim = 25,
  fname = file.path(here::here(), "Mtcars4"),
  Page = c(2, 2)
)
SmartEDA::ExpNumViz(mtcars, target = "am", scatter = TRUE)
library(ggplot2)
library(plotly)
library(gapminder)
 
p <- gapminder %>%
  filter(year==1977) %>%
  ggplot( aes(gdpPercap, lifeExp, size = pop, color=continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw()
 
ggplotly(p)
scales::show_col(colours(), cex_label = .35)
gistr::gist("https://gist.github.com/sbalci/834ebc154c0ffcb7d5899c42dd3ab75e") %>% 
  gistr::embed() -> embedgist

# https://stackoverflow.com/questions/43053375/weighted-sankey-alluvial-diagram-for-visualizing-discrete-and-continuous-panel/48133004

library(tidyr)
library(dplyr)
library(alluvial)
library(ggplot2)
library(forcats)

set.seed(42)
individual <- rep(LETTERS[1:10],each=2)
timeperiod <- paste0("time_",rep(1:2,10))
discretechoice <- factor(paste0("choice_",sample(letters[1:3],20, replace=T)))
continuouschoice <- ceiling(runif(20, 0, 100))
d <- data.frame(individual, timeperiod, discretechoice, continuouschoice)

# stacked bar diagram of discrete choice by individual
g <- ggplot(data=d,aes(timeperiod,fill=fct_rev(discretechoice)))
g + geom_bar(position="stack") + guides(fill=guide_legend(title=NULL))
# alluvial diagram of discrete choice by individual
d_alluvial <- d %>%
  select(individual,timeperiod,discretechoice) %>%
  spread(timeperiod,discretechoice) %>%
  group_by(time_1,time_2) %>%
  summarize(count=n()) %>%
  ungroup()
alluvial(select(d_alluvial,-count),freq=d_alluvial$count)
# stacked bar diagram of discrete choice, weighting by continuous choice
g + geom_bar(position="stack",aes(weight=continuouschoice))
library(ggalluvial)
ggplot(
  data = d,
  aes(
    x = timeperiod,
    stratum = discretechoice,
    alluvium = individual,
    y = continuouschoice
  )
) +
  geom_stratum(aes(fill = discretechoice)) +
  geom_flow()
 # use of strata and labels
ggplot(as.data.frame(Titanic),
       aes(y = Freq,axis1 = Class, axis2 = Sex, axis3 = Age)) +
  geom_flow() +
  scale_x_discrete(limits = c("Class", "Sex", "Age")) +
  geom_stratum() + 
  geom_text(stat = "stratum", infer.label = TRUE) +
  ggtitle("Alluvial plot of Titanic passenger demographic data")

# use of facets
ggplot(as.data.frame(Titanic),aes(y = Freq,axis1 = Class, axis2 = Sex)) +geom_flow(aes(fill = Age), width = .4) +geom_stratum(width = .4) +geom_text(stat = "stratum", infer.label = TRUE, size = 3) +scale_x_discrete(limits = c("Class", "Sex")) +facet_wrap(~ Survived, scales = "fixed")
# time series alluvia of WorldPhones 
wph <- as.data.frame(as.table(WorldPhones))
names(wph) <- c("Year", "Region", "Telephones")
ggplot(wph,aes(x = Year, alluvium = Region, y = Telephones)) +geom_flow(aes(fill = Region, colour = Region), width = 0)
# rightward flow aesthetics for vaccine survey datad
data(vaccinations)
levels(vaccinations$response) <- rev(levels(vaccinations$response))

ggplot(vaccinations,
       aes(x = survey, 
           stratum = response, 
           alluvium = subject,
           y = freq, 
           fill = response 
           label = round(a, 3)
           )
       ) +
  geom_lode() + 
  geom_flow() +
  geom_stratum(alpha = 0) +
  geom_text(stat = "stratum")

CD44changes <- mydata %>%
    dplyr::select(TumorCD44, TomurcukCD44, PeritumoralTomurcukGr4) %>% 
    dplyr::filter(complete.cases(.)) %>%
    dplyr::group_by(TumorCD44, TomurcukCD44, PeritumoralTomurcukGr4) %>% 
    dplyr::tally()

library(ggalluvial)

ggplot(data = CD44changes,
       aes(axis1 = TumorCD44, axis2 = TomurcukCD44,
           y = n)) +
  scale_x_discrete(limits = c("TumorCD44", "TomurcukCD44"),
                   expand = c(.1, .05)
                   ) +
  xlab("Tumor Tomurcuk") +
  geom_alluvium(aes(fill = PeritumoralTomurcukGr4,
                    colour = PeritumoralTomurcukGr4                    )) +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", infer.label = TRUE) +
  # geom_text(stat = 'alluvium', infer.label = TRUE) +
  theme_minimal() +
  ggtitle("Changes in CD44")
library(arsenal)
dat <- data.frame(
  tp = paste0("Time Point ", c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2)),
  id = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 6),
  Cat = c("A", "A", "A", "B", "B", "B", "B", "A", NA, "B"),
  Fac = factor(c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A")),
  Num = c(1, 2, 3, 4, 4, 3, 3, 4, 0, NA),
  Ord = ordered(c("I", "II", "II", "III", "III", "III", "I", "III", "II", "I")),
  Lgl = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
  Dat = as.Date("2018-05-01") + c(1, 1, 2, 2, 3, 4, 5, 6, 3, 4),
  stringsAsFactors = FALSE
)


p <- paired(tp ~ Cat + Fac + Num + Ord + Lgl + Dat, data = dat, id = id, signed.rank.exact = FALSE)
summary(p)
dlookr::normality(carseats)
dlookr::normality(carseats, Sales, CompPrice, Income)
dlookr::normality(carseats, Sales:Income)
dlookr::normality(carseats, -(Sales:Income))
carseats %>%
  dlookr::normality() %>%
  dplyr::filter(p_value <= 0.01) %>% 
  arrange(abs(p_value))
carseats %>%
  group_by(ShelveLoc, US) %>%
  dlookr::normality(Income) %>% 
  arrange(desc(p_value))
carseats %>%
  mutate(log_income = log(Income)) %>%
  group_by(ShelveLoc, US) %>%
  dlookr::normality(log_income) %>%
  dplyr::filter(p_value > 0.01)
dlookr::plot_normality(carseats, Sales, CompPrice)
carseats %>%
  dplyr::filter(ShelveLoc == "Good") %>%
  group_by(US) %>%
  dlookr::plot_normality(Income)

mytable <- jmv::ttestIS(
    formula = HindexCTLA4 ~ PeritumoralTomurcukGr4,
    data = mydata,
    vars = HindexCTLA4,
    students = FALSE,
    mann = TRUE,
    norm = TRUE,
    meanDiff = TRUE,
    desc = TRUE,
    plots = TRUE)

cat("<pre class='jamovitable'>")
print(jtable(mytable$ttest))
cat("</pre>")
categ <- dlookr::target_by(carseats, US)
cat_cat <- dlookr::relate(categ, ShelveLoc)
cat_cat
summary(cat_cat)
plot(cat_cat)
## Summary statistics of categorical variables
SmartEDA::ExpCatStat(
  Carseats,
  Target = "Urban",
  result = "Stat",
  clim = 10,
  nlim = 5,
  Pclass = "Yes"
)
inspectdf::inspect_cat(star_1, star_2)
num <- dlookr::target_by(carseats, Sales)
num_num <- dlookr::relate(num, Price)
num_num
summary(num_num)
plot(num_num)
plot(num_num, hex_thres = 350)
## Inforamtion value and Odds value
SmartEDA::ExpCatStat(
  Carseats,
  Target = "Urban",
  result = "IV",
  clim = 10,
  nlim = 5,
  Pclass = "Yes"
)
# library(OptimalCutpoints)
# https://tidymodels.github.io/yardstick/reference/roc_curve.html

roc_fit <- yardstick::roc_curve(mydata,
                                truth = "classification", 
                                estimate = "test",
                                na_rm = TRUE,
                                  options = list(
                                    smooth = FALSE,
                                    print.auc = TRUE,
                                    ret = c("all_coords")
                                    )
                                )
                                
ggplot2::autoplot(roc_fit)

library(pROC)

m1 <- pROC::roc(mydata,
          "classification",
          "test",
          auc = TRUE, 
          ci = TRUE,
          # plot = TRUE,
          # percent=TRUE, 
          na.rm=TRUE,
          # smooth = TRUE,
          ret = "all_coords",
          # ret = "roc",
          quiet = FALSE,
          legacy.axes = TRUE,
          print.auc = TRUE,
          # xlab = "False Positive",
          # ylab = "True Positive"
          )

m1

pROC::roc(mydata,
          "polyp_rec",
          "size",
          auc = TRUE, 
          ci = TRUE,
          # plot = TRUE,
          # percent=TRUE, 
          na.rm=TRUE,
          # smooth = TRUE,
          # ret = "all_coords",
          ret = "roc",
          quiet = FALSE,
          legacy.axes = TRUE,
          print.auc = TRUE,
          # xlab = "False Positive",
          # ylab = "True Positive"
          )


which.max(m1$youden)
m1[which.max(m1$youden),]

roc_obj <- pROC::roc(polyp_rec ~ size,
          data = mydata,
          auc = TRUE,
          ci = TRUE,
          plot = TRUE,
          # percent=TRUE, 
          na.rm=TRUE,
          # smooth = TRUE,
          # ret = "all_coords",
          ret = "roc",
          quiet = FALSE,
          legacy.axes = TRUE,
          print.auc = TRUE,
          xlab = "False Positive",
          ylab = "True Positive"
          )
# devtools::install_github("sachsmc/plotROC")
library(plotROC)
# shiny_plotROC()


iris %>% explore::explain_tree(target = Species)


iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
iris %>% select(-Species) %>% explain_tree(target = is_versicolor)

iris %>% explain_tree(target = Sepal.Length)
explore::explore(mydata)
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

knitr::kable(tUni[, 1:4], row.names=FALSE, align=c('l', 'l', 'r', 'r', 'r', 'r'))
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
  finalfit(dependentMultivariate, explanatoryMultivariate, metrics=TRUE) -> tMultivariate

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



mb_followup$OverallTime <- mb_followup$months
mb_followup$Outcome <- mb_followup$`rec(1,0)`
mb_followup$Operation <- mb_followup$`op type (1,2,3)`

## Recoding mb_followup$Operation
mb_followup$Operation <- as.character(mb_followup$Operation)
mb_followup$Operation <- forcats::fct_recode(mb_followup$Operation,
               "Type3" = "3",
               "Type2" = "2",
               "Type1" = "1")


## Reordering mb_followup$Operation
mb_followup$Operation <- factor(mb_followup$Operation, levels=c("Type3", "Type2", "Type1"))

library(magrittr)
mb_followup %$% table(Operation, `op type (1,2,3)`) 

library(survival)
library(survminer)
library(finalfit)
mb_followup %>%
  finalfit::surv_plot('Surv(OverallTime, Outcome)', 'Operation', 
  xlab='Time (months)', pval=TRUE, legend = 'none',
  # pval.coord
    break.time.by = 12, xlim = c(0,60), ylim = c(0.8, 1)

# legend.labs = c('a','b')

)
explanatoryUni <- 'Operation'
dependentUni <- 'Surv(OverallTime, Outcome)'
mb_followup %>%
finalfit(dependentUni, explanatoryUni) -> tUni

knitr::kable(tUni[, 1:4], row.names=FALSE, align=c('l', 'l', 'r', 'r', 'r', 'r'))

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

km_fit <- survfit(Surv(OverallTime, Outcome) ~ Operation, data = mb_followup)

# km_fit

# summary(km_fit)

km_fit_median_df <- summary(km_fit)
km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>%
    janitor::clean_names(dat = ., case = 'snake') %>%
    tibble::rownames_to_column(.data = ., var = 'Derece')

km_fit_median_df

# km_fit_median_df %>% 
#   knitr::kable(format = "latex") %>% 
#   kableExtra::kable_styling(latex_options="scale_down")

km_fit_median_df %>%
    dplyr::mutate(
        description =
        glue::glue(
        'When, Derece, {Derece}, median survival is {median} [{x0_95lcl} - {x0_95ucl}, 95% CI] months.'
)
    ) %>%
        dplyr::mutate(
description = gsub(pattern = 'thefactor=', replacement = ' is ', x = description)
        ) %>%
    dplyr::select(description) %>%
    dplyr::pull() -> km_fit_median_definition

# km_fit_median_definition




summary(km_fit, times = c(12,36,60))

km_fit_summary <- summary(km_fit, times = c(12,36,60))

km_fit_df <- as.data.frame(km_fit_summary[c('strata', 'time', 'n.risk', 'n.event', 'surv', 'std.err', 'lower', 'upper')])

km_fit_df %>% 
  knitr::kable(format = "latex") %>% 
  kableExtra::kable_styling(latex_options="scale_down")

     


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

    survminer::pairwise_survdiff(
    formula = Surv(OverallTime, Outcome) ~ Operation,
    data = mb_followup,
    p.adjust.method = 'BH'
)
library(gt)
library(gtsummary)

library(survival)
fit1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
tbl_strata_ex1 <-
  tbl_survival(
    fit1,
    times = c(12, 24),
    label = "{time} Months"
  )

fit2 <- survfit(Surv(ttdeath, death) ~ 1, trial)
tbl_nostrata_ex2 <-
  tbl_survival(
    fit2,
    probs = c(0.1, 0.2, 0.5),
    header_estimate = "**Months**"
  )





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
finalfit(dependentUni, explanatoryUni, metrics=TRUE) -> tUni

knitr::kable(tUni[, 1:4], row.names=FALSE, align=c('l', 'l', 'r', 'r', 'r', 'r'))

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

    survminer::pairwise_survdiff(
    formula = formula_p,
    data = self$data,
    p.adjust.method = 'BH'
)
library("shiny")
library("dplyr")
library("magrittr")
library("viridis")
library("readxl")
library("survival")
library("survminer")
library("finalfit")
library("glue")
mydata <- readxl::read_excel(here::here("data", "mydata.xlsx"))
mydata$int <- lubridate::interval(
  lubridate::ymd(mydata$SurgeryDate),
  lubridate::ymd(mydata$LastFollowUpDate)
  )
mydata$OverallTime <- lubridate::time_length(mydata$int, "month")
mydata$OverallTime <- round(mydata$OverallTime, digits = 1)

mydata$Outcome <- forcats::fct_recode(as.character(mydata$Death),
               "1" = "TRUE",
               "0" = "FALSE")

mydata$Outcome <- as.numeric(as.character(mydata$Outcome))

mydata %>% 
  select(-ID,
         -Name) %>% 
  inspectdf::inspect_types() %>% 
  dplyr::filter(type == "character") %>% 
  dplyr::select(col_name) %>% 
  pull() %>% 
  unlist() -> characterVariables
selectInput(
  inputId = "Factor",
  label = "Choose a Factor Affecting Survival",
  choices = characterVariables,
  selected = "LVI"
)


dependentKM <- "Surv(OverallTime, Outcome)"


renderPrint({

  print(input$Factor)
  
  
})

tags$b("Kaplan-Meier Plot, Log-Rank Test")
tags$br()

renderPlot({

    
  mydata %>%
    finalfit::surv_plot(
      .data = .,
      dependent = dependentKM,
      explanatory = input$Factor,
      xlab = 'Time (months)',
      pval = TRUE,
      legend = 'none',
      break.time.by = 12,
      xlim = c(0, 60)
    )
  
})



tags$b("Univariate Cox-Regression")
tags$br()


renderPrint({

  mydata %>%
    finalfit::finalfit(dependentKM, input$Factor) -> tUni
  
  knitr::kable(tUni[, 1:4],
               row.names = FALSE,
               align = c('l', 'l', 'r', 'r', 'r', 'r'))
  
  
})




tags$b("Median Survival")
tags$br()


renderPrint({
  
  formula_text <- paste0("Surv(OverallTime, Outcome) ~ ",input$Factor)
  
  km_fit <- survfit(as.formula(formula_text),
                              data = mydata)
  
  km_fit
  
})




tags$b("1-3-5-yr Survival")
tags$br()


renderPrint({

  formula_text <- paste0("Surv(OverallTime, Outcome) ~ ",input$Factor)
  
  km_fit <- survfit(as.formula(formula_text),
                              data = mydata)

  summary(km_fit, times = c(12, 36, 60))
  
})


renderPrint({

  
  formula_text <- paste0("Surv(OverallTime, Outcome) ~ ",input$Factor)
  
  km_fit <- survfit(as.formula(formula_text),
                              data = mydata)
  
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
  pull()

})

#  https://easystats.github.io/correlation/
# install.packages("devtools")
# devtools::install_github("easystats/correlation")
library("correlation")
correlation::correlation(iris)
library(dplyr)

iris %>% 
  select(Species, starts_with("Sepal")) %>% 
  group_by(Species) %>% 
  correlation::correlation() %>% 
  filter(r < 0.9)

correlation::correlation(select(iris, Species, starts_with("Sepal")),
            select(iris, Species, starts_with("Petal")),
            partial=TRUE)

correlation(iris, bayesian=TRUE)
library(report)
iris %>% 
  select(starts_with("Sepal")) %>% 
  correlation::correlation(bayesian=TRUE) %>% 
  report()
report::report(cor.test(iris$Sepal.Length, iris$Petal.Length))

iris %>% 
  group_by(Species) %>% 
  correlation() %>% 
  report() %>% 
  to_table()
iris %>% explore(Sepal.Length, Petal.Length)

iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
iris %>% explore(Sepal.Length, Petal.Length, target = is_versicolor)
dlookr::correlate(carseats)
dlookr::correlate(carseats, Sales, CompPrice, Income)
dlookr::correlate(carseats, Sales:Income)
dlookr::correlate(carseats, -(Sales:Income))
carseats %>%
  dlookr::correlate(Sales:Income) %>%
  dplyr::filter(as.integer(var1) > as.integer(var2))
carseats %>%
  dplyr::filter(ShelveLoc == "Good") %>%
  group_by(Urban, US) %>%
  dlookr::correlate(Sales) %>%
  dplyr::filter(abs(coef_corr) > 0.5)
dlookr::plot_correlate(carseats)
dlookr::plot_correlate(carseats, Sales, Price)
carseats %>%
  dplyr::filter(ShelveLoc == "Good") %>%
  dplyr::group_by(Urban, US) %>%
  dlookr::plot_correlate(Sales)
## Summary statistics by – overall with correlation
SmartEDA::ExpNumStat(
  Carseats,
  by = "A",
  gp = "Price",
  Qnt = seq(0, 1, 0.1),
  MesofShape = 1,
  Outlier = TRUE,
  round = 2
)
# https://alastairrushworth.github.io/inspectdf/articles/pkgdown/inspect_cor_exampes.html
inspectdf::inspect_cor(storms)

inspectdf::inspect_cor(storms) %>% inspectdf::show_plot()

inspectdf::inspect_cor(storms, storms[-c(1:200), ])

inspectdf::inspect_cor(storms, storms[-c(1:200), ]) %>% 
  slice(1:20) %>%
  inspectdf::show_plot()

cor %>% 
    report::to_values()
mydata %>%
  select(continiousVariables,
         -dateVariables) %>% 
visdat::vis_cor()
library(report)
model <- lm(Sepal.Length ~ Species, data = iris)
report::report(model)
# Table report for a linear model
lm(Sepal.Length ~ Petal.Length + Species, data=iris) %>% 
  report::report() %>% 
  report::to_table() %>% 
  kableExtra::kable()
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  summarizer(dependent, explanatory)
num_cat <- dlookr::relate(num, ShelveLoc)
num_cat
summary(num_cat)
plot(num_cat)

my_text <- kableExtra::text_spec("Some Text", 
                                 color = "red",
                                 background = "yellow"
                                 )
# `r my_text`
mylongtext <- paste("İstatistik Metod:

Sürekli verilerin ortalama, standart sapma, median, minimum ve maksimum değerleri verildi. Kategorik veriler ve gruplanan sürekli veriler için frekans tabloları oluşturuldu. Genel sağkalım analizinde ölüm tarihi ve son başvuru tarihi hasta dosyalarından elde edildi. 
Sağkalım analizinde Kaplan-Meier grafikleri, Log-rank testi ve Cox-Regresyon testleri uygulandı. Analizler R-project (version 3.6.0) ve RStudio ile survival ve finalfit paketleri kullanılarak yapıldı. p değeri 0.05 düzeyinde anlamlı olarak kabul edildi.


R Core Team (2019). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Therneau T (2015). A Package for Survival Analysis in S. version 2.38, https://CRAN.R-project.org/package=survival

Terry M. Therneau, Patricia M. Grambsch (2000). Modeling Survival Data: Extending the Cox Model. Springer, New York. ISBN 0-387-98784-3.

Ewen Harrison, Tom Drake and Riinu Ots (2019). finalfit: Quickly Create Elegant Regression Results Tables and Plots when Modelling. R package version 0.9.6. https://github.com/ewenharrison/finalfit"
)


mylongtext <- strwrap(mylongtext)

# `r mylongtext`

boxplot(1:10)
plot(rnorm(10))
ggplot2::ggplot(mtcars,
                ggplot2::aes(x=mpg)
                ) + 
ggplot2::geom_histogram(fill="skyblue", alpha=0.5) + 
ggplot2::theme_minimal()
Block rmdnote

Block rmdtip

Block warning

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


**push all changes to GitHub repository**


```r
source(file = here::here("R", "force_git.R"))
```


---


# References


