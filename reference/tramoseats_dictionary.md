# TRAMO-SEATS dictionary

Functions to provide information for all output objects (series,
diagnostics, parameters) available with
[`tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)
function.

## Usage

``` r
tramoseats_dictionary()

tramoseats_full_dictionary()
```

## Value

`tramoseats_dictionary()` returns a character vector containing the
names of all output objects (series, diagnostics, parameters) available
with the
[`tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)
function, whereas `tramoseats_full_dictionary()` returns a `data.frame`
with format and description, for all the output objects.

## Details

These functions provide lists of output names (series, diagnostics,
parameters) available with the
[`tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)
function. These names can be used to generate customized outputs with
the userdefined option of the
[`tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)
function (see examples). The `tramoseats_full_dictionary` function
provides additional information on object format and description.

## Examples

``` r
# Visualize the dictionary
print(tramoseats_dictionary())
#>   [1] "period"                              
#>   [2] "span.start"                          
#>   [3] "span.end"                            
#>   [4] "span.n"                              
#>   [5] "span.missing"                        
#>   [6] "log"                                 
#>   [7] "adjust"                              
#>   [8] "likelihood.ll"                       
#>   [9] "likelihood.adjustedll"               
#>  [10] "likelihood.ssqerr"                   
#>  [11] "likelihood.aic"                      
#>  [12] "likelihood.bic"                      
#>  [13] "likelihood.aicc"                     
#>  [14] "likelihood.bicc"                     
#>  [15] "likelihood.bic2"                     
#>  [16] "likelihood.hannanquinn"              
#>  [17] "likelihood.nparams"                  
#>  [18] "likelihood.nobs"                     
#>  [19] "likelihood.neffectiveobs"            
#>  [20] "likelihood.df"                       
#>  [21] "arima.p"                             
#>  [22] "arima.d"                             
#>  [23] "arima.q"                             
#>  [24] "arima.bp"                            
#>  [25] "arima.bd"                            
#>  [26] "arima.bq"                            
#>  [27] "arima.theta(*)"                      
#>  [28] "arima.phi(*)"                        
#>  [29] "arima.btheta(*)"                     
#>  [30] "arima.bphi(*)"                       
#>  [31] "regression.espan.start"              
#>  [32] "regression.espan.end"                
#>  [33] "regression.espan.n"                  
#>  [34] "regression.espan.missing"            
#>  [35] "regression.mean"                     
#>  [36] "regression.nlp"                      
#>  [37] "regression.ntd"                      
#>  [38] "regression.leaster"                  
#>  [39] "regression.nmh"                      
#>  [40] "regression.nout"                     
#>  [41] "regression.nao"                      
#>  [42] "regression.nls"                      
#>  [43] "regression.ntc"                      
#>  [44] "regression.nso"                      
#>  [45] "regression.nusers"                   
#>  [46] "regression.mu"                       
#>  [47] "regression.lp"                       
#>  [48] "regression.td(*)"                    
#>  [49] "regression.td-derived"               
#>  [50] "regression.td-ftest"                 
#>  [51] "regression.easter"                   
#>  [52] "regression.outlier(*)"               
#>  [53] "regression.user(*)"                  
#>  [54] "regression.missing(*)"               
#>  [55] "residuals.res"                       
#>  [56] "residuals.tsres"                     
#>  [57] "residuals.n"                         
#>  [58] "residuals.df"                        
#>  [59] "residuals.dfc"                       
#>  [60] "residuals.ser"                       
#>  [61] "residuals.ser_ml"                    
#>  [62] "residuals.type"                      
#>  [63] "residuals.mean"                      
#>  [64] "residuals.skewness"                  
#>  [65] "residuals.kurtosis"                  
#>  [66] "residuals.doornikhansen"             
#>  [67] "residuals.lb"                        
#>  [68] "residuals.bp"                        
#>  [69] "residuals.lb2"                       
#>  [70] "residuals.bp2"                       
#>  [71] "residuals.seaslb"                    
#>  [72] "residuals.seasbp"                    
#>  [73] "residuals.nruns"                     
#>  [74] "residuals.lruns"                     
#>  [75] "residuals.nudruns"                   
#>  [76] "residuals.ludruns"                   
#>  [77] "regression.ml.parameters"            
#>  [78] "regression.ml.pcovar"                
#>  [79] "regression.ml.pcovar-ml"             
#>  [80] "regression.ml.pcorr"                 
#>  [81] "regression.ml.pscore"                
#>  [82] "regression.details.description"      
#>  [83] "regression.details.type"             
#>  [84] "regression.details.coefficients"     
#>  [85] "regression.details.covar"            
#>  [86] "regression.details.covar-ml"         
#>  [87] "y"                                   
#>  [88] "y_f(?)"                              
#>  [89] "y_ef(?)"                             
#>  [90] "y_b(?)"                              
#>  [91] "y_eb(?)"                             
#>  [92] "yc"                                  
#>  [93] "yc_f(?)"                             
#>  [94] "yc_b(?)"                             
#>  [95] "ylin"                                
#>  [96] "ylin_f(?)"                           
#>  [97] "ylin_b(?)"                           
#>  [98] "det"                                 
#>  [99] "det_f(?)"                            
#> [100] "det_b(?)"                            
#> [101] "cal"                                 
#> [102] "cal_f(?)"                            
#> [103] "cal_b(?)"                            
#> [104] "ycal"                                
#> [105] "ycal_f(?)"                           
#> [106] "ycal_b(?)"                           
#> [107] "tde"                                 
#> [108] "tde_f(?)"                            
#> [109] "tde_b(?)"                            
#> [110] "ee"                                  
#> [111] "ee_f(?)"                             
#> [112] "ee_b(?)"                             
#> [113] "omhe"                                
#> [114] "omhe_f(?)"                           
#> [115] "omhe_b(?)"                           
#> [116] "mhe"                                 
#> [117] "mhe_f(?)"                            
#> [118] "mhe_b(?)"                            
#> [119] "out"                                 
#> [120] "out_f(?)"                            
#> [121] "out_b(?)"                            
#> [122] "reg"                                 
#> [123] "reg_f(?)"                            
#> [124] "reg_b(?)"                            
#> [125] "l"                                   
#> [126] "l_f(?)"                              
#> [127] "l_b(?)"                              
#> [128] "full_res"                            
#> [129] "out_t"                               
#> [130] "out_s"                               
#> [131] "out_i"                               
#> [132] "reg_t"                               
#> [133] "reg_s"                               
#> [134] "reg_i"                               
#> [135] "reg_sa"                              
#> [136] "reg_u"                               
#> [137] "reg_y"                               
#> [138] "det_t"                               
#> [139] "det_s"                               
#> [140] "det_i"                               
#> [141] "out_t_f(?)"                          
#> [142] "out_s_f(?)"                          
#> [143] "out_i_f(?)"                          
#> [144] "reg_t_f(?)"                          
#> [145] "reg_s_f(?)"                          
#> [146] "reg_i_f(?)"                          
#> [147] "reg_sa_f(?)"                         
#> [148] "reg_u_f(?)"                          
#> [149] "reg_y_f(?)"                          
#> [150] "det_t_f(?)"                          
#> [151] "det_s_f(?)"                          
#> [152] "det_i_f(?)"                          
#> [153] "out_t_b(?)"                          
#> [154] "out_s_b(?)"                          
#> [155] "out_i_b(?)"                          
#> [156] "reg_t_b(?)"                          
#> [157] "reg_s_b(?)"                          
#> [158] "reg_i_b(?)"                          
#> [159] "reg_sa_b(?)"                         
#> [160] "reg_u_b(?)"                          
#> [161] "reg_y_b(?)"                          
#> [162] "det_t_b(?)"                          
#> [163] "det_s_b(?)"                          
#> [164] "det_i_b(?)"                          
#> [165] "mode"                                
#> [166] "seasonal"                            
#> [167] "sa"                                  
#> [168] "t"                                   
#> [169] "s"                                   
#> [170] "i"                                   
#> [171] "sa_f"                                
#> [172] "t_f"                                 
#> [173] "s_f"                                 
#> [174] "i_f"                                 
#> [175] "decomposition.y_lin"                 
#> [176] "decomposition.sa_lin"                
#> [177] "decomposition.t_lin"                 
#> [178] "decomposition.s_lin"                 
#> [179] "decomposition.i_lin"                 
#> [180] "decomposition.sa_lin_e"              
#> [181] "decomposition.t_lin_e"               
#> [182] "decomposition.s_lin_e"               
#> [183] "decomposition.i_lin_e"               
#> [184] "decomposition.y_lin_f"               
#> [185] "decomposition.sa_lin_f"              
#> [186] "decomposition.t_lin_f"               
#> [187] "decomposition.s_lin_f"               
#> [188] "decomposition.i_lin_f"               
#> [189] "decomposition.y_lin_ef"              
#> [190] "decomposition.sa_lin_ef"             
#> [191] "decomposition.t_lin_ef"              
#> [192] "decomposition.s_lin_ef"              
#> [193] "decomposition.i_lin_ef"              
#> [194] "decomposition.y_lin_b"               
#> [195] "decomposition.sa_lin_b"              
#> [196] "decomposition.t_lin_b"               
#> [197] "decomposition.s_lin_b"               
#> [198] "decomposition.i_lin_b"               
#> [199] "decomposition.y_lin_eb"              
#> [200] "decomposition.sa_lin_eb"             
#> [201] "decomposition.t_lin_eb"              
#> [202] "decomposition.s_lin_eb"              
#> [203] "decomposition.i_lin_eb"              
#> [204] "decomposition.y_cmp"                 
#> [205] "decomposition.y_cmp_f"               
#> [206] "decomposition.y_cmp_b"               
#> [207] "decomposition.sa_cmp"                
#> [208] "decomposition.t_cmp"                 
#> [209] "decomposition.s_cmp"                 
#> [210] "decomposition.i_cmp"                 
#> [211] "diagnostics.seas-lin-combined"       
#> [212] "diagnostics.seas-lin-evolutive"      
#> [213] "diagnostics.seas-lin-stable"         
#> [214] "diagnostics.seas-si-combined"        
#> [215] "diagnostics.seas-si-combined3"       
#> [216] "diagnostics.seas-si-evolutive"       
#> [217] "diagnostics.seas-si-stable"          
#> [218] "diagnostics.seas-res-combined"       
#> [219] "diagnostics.seas-res-combined3"      
#> [220] "diagnostics.seas-res-evolutive"      
#> [221] "diagnostics.seas-res-stable"         
#> [222] "diagnostics.seas-sa-combined"        
#> [223] "diagnostics.seas-sa-combined3"       
#> [224] "diagnostics.seas-sa-evolutive"       
#> [225] "diagnostics.seas-sa-stable"          
#> [226] "diagnostics.seas-i-combined"         
#> [227] "diagnostics.seas-i-combined3"        
#> [228] "diagnostics.seas-i-evolutive"        
#> [229] "diagnostics.seas-i-stable"           
#> [230] "diagnostics.seas-lin-qs"             
#> [231] "diagnostics.seas-lin-f"              
#> [232] "diagnostics.seas-lin-friedman"       
#> [233] "diagnostics.seas-lin-kw"             
#> [234] "diagnostics.seas-lin-periodogram"    
#> [235] "diagnostics.seas-lin-spectralpeaks"  
#> [236] "diagnostics.seas-res-qs"             
#> [237] "diagnostics.seas-res-f"              
#> [238] "diagnostics.seas-res-friedman"       
#> [239] "diagnostics.seas-res-kw"             
#> [240] "diagnostics.seas-res-periodogram"    
#> [241] "diagnostics.seas-res-spectralpeaks"  
#> [242] "diagnostics.seas-sa-qs"              
#> [243] "diagnostics.seas-sa-f"               
#> [244] "diagnostics.seas-sa-friedman"        
#> [245] "diagnostics.seas-sa-kw"              
#> [246] "diagnostics.seas-sa-periodogram"     
#> [247] "diagnostics.seas-sa-spectralpeaks"   
#> [248] "diagnostics.seas-i-qs"               
#> [249] "diagnostics.seas-i-f"                
#> [250] "diagnostics.seas-i-friedman"         
#> [251] "diagnostics.seas-i-kw"               
#> [252] "diagnostics.seas-i-periodogram"      
#> [253] "diagnostics.seas-i-spectralpeaks"    
#> [254] "diagnostics.seas-sa-ac1"             
#> [255] "diagnostics.td-res-all"              
#> [256] "diagnostics.td-res-last"             
#> [257] "diagnostics.td-sa-all"               
#> [258] "diagnostics.td-sa-last"              
#> [259] "diagnostics.td-i-all"                
#> [260] "diagnostics.td-i-last"               
#> [261] "diagnostics.fcast-insample-mean"     
#> [262] "diagnostics.fcast-outsample-mean"    
#> [263] "diagnostics.fcast-outsample-variance"
#> [264] "variancedecomposition.cycle"         
#> [265] "variancedecomposition.seasonality"   
#> [266] "variancedecomposition.irregular"     
#> [267] "variancedecomposition.tdh"           
#> [268] "variancedecomposition.others"        
#> [269] "variancedecomposition.total"         
#> [270] "quality.summary"                     
#> [271] "benchmarking.original"               
#> [272] "benchmarking.target"                 
#> [273] "benchmarking.result"                 
#> attr(,"class")
#> [1] "JD3_DICTIONARY"
summary(tramoseats_dictionary())
#>         Length          Class           Mode 
#>            273 JD3_DICTIONARY      character 

# first 10 lines
head(tramoseats_full_dictionary(), n = 10)
#>                     name
#> 1                 period
#> 2             span.start
#> 3               span.end
#> 4                 span.n
#> 5           span.missing
#> 6                    log
#> 7                 adjust
#> 8          likelihood.ll
#> 9  likelihood.adjustedll
#> 10     likelihood.ssqerr
#>                                                    description detail
#> 1                                         period of the series       
#> 2                     start of the considered (partial) series       
#> 3                       end of the considered (partial) series       
#> 4         number of periods in the considered (partial) series       
#> 5  number of missing values in the considered (partial) series       
#> 6                                            log-transformtion       
#> 7                                 pre-adjustment for leap year       
#> 8                                               log-likelihood       
#> 9                                      adjusted log-likelihood       
#> 10                                              sum of squares       
#>                                         output   type              fullname
#> 1                            java.lang.Integer Normal                period
#> 2  jdplus.toolkit.base.api.timeseries.TsPeriod Normal            span.start
#> 3  jdplus.toolkit.base.api.timeseries.TsPeriod Normal              span.end
#> 4                            java.lang.Integer Normal                span.n
#> 5                            java.lang.Integer Normal          span.missing
#> 6                            java.lang.Integer Normal                   log
#> 7                             java.lang.String Normal                adjust
#> 8                             java.lang.Double Normal         likelihood.ll
#> 9                             java.lang.Double Normal likelihood.adjustedll
#> 10                            java.lang.Double Normal     likelihood.ssqerr
# For more structured information call `View(tramoseats_full_dictionary())`

# Extract names of output of interest
user_defined_output <- tramoseats_dictionary()[c(65, 95, 135)]
user_defined_output
#> [1] "residuals.kurtosis" "ylin"               "reg_sa"            

# Generate the corresponding output in an estimation
y <- rjd3toolkit::ABS$X0.2.09.10.M
# \donttest{
m <- tramoseats(y, "rsafull", userdefined=user_defined_output)

# Retrieve user defined output
tail(m$user_defined$ylin)
#>           Mar      Apr      May      Jun      Jul      Aug
#> 2017 1389.905 1492.068 1460.957 1540.453 1468.642 1279.390
m$user_defined$residuals.kurtosis
#> Value: 3.495232 
#> P-Value: 0.0402 
m$user_defined$sa_f
#> NULL
# }
```
