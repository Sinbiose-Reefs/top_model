Topic modeling project, ReefSYN
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

Repository containing the data and scripts used in the article of Osmar
Luiz.

<!-- badges: start -->
<!-- badges: end -->

#### This paper was produced using the following software and associated packages:

    ## R version 4.3.0 (2023-04-21 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=Portuguese_Brazil.utf8  LC_CTYPE=Portuguese_Brazil.utf8   
    ## [3] LC_MONETARY=Portuguese_Brazil.utf8 LC_NUMERIC=C                      
    ## [5] LC_TIME=Portuguese_Brazil.utf8    
    ## 
    ## time zone: America/Sao_Paulo
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] ggrepel_0.9.3            gridExtra_2.3            viridis_0.6.3           
    ##  [4] viridisLite_0.4.2        rasterVis_0.51.5         lattice_0.21-8          
    ##  [7] sna_2.7-1                network_1.18.1           statnet.common_4.9.0    
    ## [10] maps_3.4.1               ggplot2_3.4.2            rnaturalearth_0.3.2     
    ## [13] CoordinateCleaner_2.0-20 rgdal_1.6-6              rgeos_0.6-2             
    ## [16] raster_3.6-20            sp_1.6-0                 magick_2.7.4            
    ## [19] dplyr_1.1.2              httr_1.4.6               XML_3.99-0.14           
    ## [22] stringr_1.5.0            here_1.0.1              
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.3        xfun_0.39           latticeExtra_0.6-30
    ##  [4] vctrs_0.6.2         tools_4.3.0         generics_0.1.3     
    ##  [7] parallel_4.3.0      tibble_3.2.1        proxy_0.4-27       
    ## [10] fansi_1.0.4         pkgconfig_2.0.3     KernSmooth_2.23-20 
    ## [13] data.table_1.14.8   RColorBrewer_1.1-3  lifecycle_1.0.3    
    ## [16] deldir_1.0-6        compiler_4.3.0      munsell_0.5.0      
    ## [19] terra_1.7-29        codetools_0.2-19    htmltools_0.5.5    
    ## [22] class_7.3-21        yaml_2.3.7          lazyeval_0.2.2     
    ## [25] hexbin_1.28.3       pillar_1.9.0        whisker_0.4.1      
    ## [28] classInt_0.4-9      tidyselect_1.2.0    digest_0.6.31      
    ## [31] stringi_1.7.12      sf_1.0-12           rprojroot_2.0.3    
    ## [34] fastmap_1.1.1       grid_4.3.0          colorspace_2.1-0   
    ## [37] cli_3.6.1           magrittr_2.0.3      utf8_1.2.3         
    ## [40] e1071_1.7-13        withr_2.5.0         scales_1.2.1       
    ## [43] oai_0.4.0           rmarkdown_2.21      rgbif_3.7.7        
    ## [46] jpeg_0.1-10         interp_1.1-4        zoo_1.8-12         
    ## [49] png_0.1-8           coda_0.19-4         evaluate_0.21      
    ## [52] knitr_1.42          rlang_1.1.1         Rcpp_1.0.10        
    ## [55] glue_1.6.2          geosphere_1.5-18    DBI_1.1.3          
    ## [58] xml2_1.3.4          rstudioapi_0.14     jsonlite_1.8.4     
    ## [61] R6_2.5.1            plyr_1.8.8          units_0.8-2
