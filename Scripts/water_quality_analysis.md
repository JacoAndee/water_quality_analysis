**Author: Jacob Anderson**

    ### Load packages
    ### Check ArcGIS Pro license

    library(arcgisbinding)

    ## *** Please call arc.check_product() to define a desktop license.

    library(brms)

    ## Warning: package 'brms' was built under R version 4.4.3

    ## Loading required package: Rcpp

    ## Warning: package 'Rcpp' was built under R version 4.4.3

    ## Loading 'brms' package (version 2.22.0). Useful instructions
    ## can be found by typing help('brms'). A more detailed introduction
    ## to the package is available through vignette('brms_overview').

    ## 
    ## Attaching package: 'brms'

    ## The following object is masked from 'package:stats':
    ## 
    ##     ar

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 4.4.3

    library(ggspatial)

    ## Warning: package 'ggspatial' was built under R version 4.4.3

    library(sf)

    ## Warning: package 'sf' was built under R version 4.4.3

    ## Linking to GEOS 3.13.0, GDAL 3.10.1, PROJ 9.5.1; sf_use_s2() is TRUE

    arc.check_product()

    ## product: ArcGIS Pro (13.5.0.57366)
    ## license: Advanced
    ## version: 1.0.1.311

    ### Check the current directory

    getwd()

    ## [1] "C:/Users/ja090/Desktop/Files/JHU/SU25_STATS/Water_Quality/water_quality_analysis/Scripts"

    ### Read the csv and store as data frame

    df_rec <- read.csv(
      "C:/Users/ja090/Desktop/Files/JHU/SU25_STATS/Water_Quality/water_quality_analysis/Data/RFEOutput2_v22_RRecs_20240301145200.csv")

    ### Inspect the structure of the data frame

    head(df_rec)

    ##   MonitoringLocationIdentifier    Pollutant X48mo YYYYMM Samples Rejected
    ## 1            1083919_WQX-26.1W Enterococcus            0       5        0
    ## 2            1083919_WQX-26.1W Enterococcus       201706       1        0
    ## 3            1083919_WQX-26.1W Enterococcus       201707       1        0
    ## 4            1083919_WQX-26.1W Enterococcus       201708       1        0
    ## 5            1083919_WQX-26.1W Enterococcus       201709       1        0
    ## 6            1083919_WQX-26.1W Enterococcus       201710       1        0
    ##   Maximum Median MedianW X70. X70.W X90. X90.W Crits.304a MaxMedian
    ## 1     609    199     199  207   209  449   609          4       609
    ## 2      63     63      63   63    63   63    63          0        NA
    ## 3     199    199     199  199   199  199   199          1        NA
    ## 4     609    609     609  609   609  609   609          1        NA
    ## 5      75     75      75   75    75   75    75          1        NA
    ## 6     209    209     209  209   209  209   209          1        NA

    ### Detection Limit for Enterococcus in water samples
    ### CFU per 100 mL

    DL <- 10

    ### Subset the Data for the Enterococcus pollutant, at least 1 rejected samples, and for June 2023

    df_subset <- subset(df_rec, Pollutant == "Enterococcus" & Rejected > 0 & YYYYMM == 202306)

    ### Use the dim function to retrieve the dimensions of the data frame subset

    dim(df_subset)

    ## [1] 1627   15

    ### Dimensions: 1627 by 15

    ### Read the csv and store as data frame

    df_locs <- read.csv(
      "C:/Users/ja090/Desktop/Files/JHU/SU25_STATS/Quantitative_Assignment06/Files/RFEOutput2_v24_MonLocIdInfo_20240301145200.csv")

    ### Inspect the structure of the data frame

    head(df_locs)

    ##   MonitoringLocationIdentifier                        OrganizationFormalName
    ## 1            1083919_WQX-26.1W  ronx River Alliance (Volunteer)(1083919_WQX)
    ## 2            1083919_WQX-26.2W Bronx River Alliance (Volunteer)(1083919_WQX)
    ## 3              1083919_WQX-28E Bronx River Alliance (Volunteer)(1083919_WQX)
    ## 4          1083919_WQX-BR-GS-1 Bronx River Alliance (Volunteer)(1083919_WQX)
    ## 5          1083919_WQX-BxR-009 Bronx River Alliance (Volunteer)(1083919_WQX)
    ## 6        1083919_WQX-Moturis-1 Bronx River Alliance (Volunteer)(1083919_WQX)
    ##                    MonitoringLocationName LatitudeMeasure LongitudeMeasure
    ## 1                           Piermont Pier        41.04316        -73.89550
    ## 2                  Piermont- Pirelli Park        41.04342        -73.91370
    ## 3     Kingsland Pt. Park- Pocantico River        41.09154        -73.87070
    ## 4 Sprain  Brook at Palmer and Millard Ave        40.94241        -73.84534
    ## 5          Soundview Park, HP-009 outfall        40.81440        -73.87080
    ## 6           Moturis- DS of Oak Tree Rd Br        41.01586        -73.93757
    ##   CEightDigitCode StateCode CountyCode MonitoringLocationTypeName WaterBodyType
    ## 1        02030101        36        087               River/Stream             R
    ## 2        02030101        36        087               River/Stream             R
    ## 3        02030101        36        119               River/Stream             R
    ## 4        02030102        36        119               River/Stream             R
    ## 5        02030102        36        005               River/Stream             R
    ## 6        02030101        36        087               River/Stream             R
    ##   EcoregionIdentifier
    ## 1                 XIV
    ## 2                 XIV
    ## 3                 XIV
    ## 4                 XIV
    ## 5                 XIV
    ## 6                 XIV

    ### Inner join the two tables with matching field (Monitoring Location ID)

    df_subset_w_locs <- inner_join(
      df_subset,
      df_locs,
      by = "MonitoringLocationIdentifier"
    )

    ### Inspect the structure of the data frame

    head(df_subset_w_locs)

    ##   MonitoringLocationIdentifier    Pollutant          X48mo YYYYMM Samples
    ## 1         11113300-BCHBASNHMCR Enterococcus Enterococcus48 202306       1
    ## 2         11113300-BCHBASNHMLF Enterococcus Enterococcus48 202306       0
    ## 3         11113300-BCHBASNHMRT Enterococcus Enterococcus48 202306       2
    ## 4         11113300-BCHBSPHAMCR Enterococcus Enterococcus48 202306       0
    ## 5        11113300-BCHBSPHAMLCR Enterococcus Enterococcus48 202306       1
    ## 6         11113300-BCHBSPHAMLF Enterococcus Enterococcus48 202306       1
    ##   Rejected Maximum Median MedianW X70. X70.W X90. X90.W Crits.304a MaxMedian
    ## 1        1      10     10      10   10    10   10    10          0        NA
    ## 2        2       0      0       0    0     0    0     0          0        NA
    ## 3        1      20     15      20   17    20   19    20          0        NA
    ## 4        4       0      0       0    0     0    0     0          0        NA
    ## 5        3      10     10      10   10    10   10    10          0        NA
    ## 6        3      20     20      20   20    20   20    20          0        NA
    ##                                                 OrganizationFormalName
    ## 1 New Hampshire Department Of Environmental Services (BEACH)(11113300)
    ## 2 New Hampshire Department Of Environmental Services (BEACH)(11113300)
    ## 3 New Hampshire Department Of Environmental Services (BEACH)(11113300)
    ## 4 New Hampshire Department Of Environmental Services (BEACH)(11113300)
    ## 5 New Hampshire Department Of Environmental Services (BEACH)(11113300)
    ## 6 New Hampshire Department Of Environmental Services (BEACH)(11113300)
    ##    MonitoringLocationName LatitudeMeasure LongitudeMeasure CEightDigitCode
    ## 1       BASS BEACH-CENTER        42.96870        -70.77155        01060003
    ## 2         BASS BEACH-LEFT        42.96923        -70.77098        01060003
    ## 3        BASS BEACH-RIGHT        42.96784        -70.77205        01060003
    ## 4      NORTH BEACH-CENTER        42.93434        -70.79649        00000000
    ## 5 NORTH BEACH-LEFT CENTER        42.93731        -70.79531        00000000
    ## 6        NORTH BEACH-LEFT        42.94042        -70.79384        00000000
    ##   StateCode CountyCode MonitoringLocationTypeName WaterBodyType
    ## 1        33        015   BEACH Program Site-Ocean             S
    ## 2        33        015   BEACH Program Site-Ocean             S
    ## 3        33        015   BEACH Program Site-Ocean             S
    ## 4        33        015   BEACH Program Site-Ocean             S
    ## 5        33        015   BEACH Program Site-Ocean             S
    ## 6        33        015   BEACH Program Site-Ocean             S
    ##   EcoregionIdentifier
    ## 1                 XIV
    ## 2                 XIV
    ## 3                 XIV
    ## 4                 XIV
    ## 5                 XIV
    ## 6                 XIV

    ### Convert the data frame into an sf data frame

    sf_df <-st_as_sf(
      df_subset_w_locs,
      coords = c("LongitudeMeasure", "LatitudeMeasure"),
      crs = 4326
    )

    ### Convert data frame to web mercator

    sf_df_3857 <- st_transform(sf_df, 3857)

    ### Make a GGPLOT map of the sf data frame

    ggplot() +
      annotation_map_tile(type = "osm") +
      geom_sf(
        data = sf_df_3857,
        color = "deeppink",
        size = 1
      ) +
      coord_sf(crs = 3857, expand = T) +
      labs(
        title = "Enterococcus Samples (Rejected)",
        x = NULL,
        y = NULL
      ) +
      theme_minimal()

    ## Zoom: 2

![](water_quality_analysis_files/figure-markdown_strict/Map%20the%20WQI%20Data-1.png)

    ### Observed data: detected values remain; censored values set to DL

    df_subset["Censored Conc."] <- ifelse(df_subset$Maximum<DL, DL, df_subset$Maximum)

    df_subset["Flag"] <- ifelse(df_subset$Maximum<DL, "left", "none")

    df_subset["DetectionLimit"] <- DL

    ### brms formula: | cens(Censored, DetectionLimit) ~ 1 means intercept-only model

    fit <- brm(
      bf(Maximum | cens(Flag, DetectionLimit) ~ 1),
      data = df_subset,
      family = gaussian(),
      chains = 2, 
      iter = 2000,
      seed = 10
    )

    ## Compiling Stan program...

    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000537 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 5.37 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 8.292 seconds (Warm-up)
    ## Chain 1:                1.101 seconds (Sampling)
    ## Chain 1:                9.393 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000234 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 2.34 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 5.323 seconds (Warm-up)
    ## Chain 2:                0.975 seconds (Sampling)
    ## Chain 2:                6.298 seconds (Total)
    ## Chain 2:

    summary(fit)

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: Maximum | cens(Flag, DetectionLimit) ~ 1 
    ##    Data: df_subset (Number of observations: 1627) 
    ##   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup draws = 2000
    ## 
    ## Regression Coefficients:
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept  -304.63     35.16  -374.37  -233.57 1.00     1262     1105
    ## 
    ## Further Distributional Parameters:
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma  1238.39     27.40  1185.41  1293.62 1.00      987     1229
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

    ### Convert brms fit to data frame

    fit_df <- as_draws_df(fit)

    ### Plot the posterior probability distribution

    ggplot(fit_df, aes(x = b_Intercept)) +
      geom_density(fill = "deeppink", alpha = 0.5) +
      labs(
        title = "Posterior Distribution of the Intercept",
        x = "Mean Enterococcus Concentration",
        y = "Density")

![](water_quality_analysis_files/figure-markdown_strict/Visualizing%20the%20posterior-1.png)

Using Bayesian censored regression with the brms package, we estimated
the mean distribution of Enterococcus samples while accounting for
left-censored values below the detection limit. The posterior
distribution provides a full representation of uncertainty around this
estimate, which would not be captured by a standard regression that
ignores censoring.
