![image](https://user-images.githubusercontent.com/37065157/233836694-5312496e-4ada-47cb-bc09-3bf8c00be135.png)

<!---
When public
![image](https://raw.githubusercontent.com/Simulation-Decomposition/simdec-python/main/docs/_static/simdec_presentation.png)
-->

**Simulation decomposition** or **SimDec** is an uncertainty and sensitivity
analysis method, which is based on Monte Carlo simulation. SimDec consists of
three major parts:

1. computing significance indices,
2. creating multi-variable scenarios and mapping the output values to them, and
3. visualizing the scenarios on the output distribution by color-coding its segments.

Lets have a look at how it works in R!

### Load data 
First the simulated `inputs` and the `output` need to be specified. They can result from a Monte Carlo simulation arranged directly in matlab, or conducted elsewhere and then loaded through a file, like in this example. Lets use the example data that will come with the R package.  

```
data(example_data)
output    <- example_data[,1]
inputs    <- example_data[,2:5] 
```

### Compute significance indices
Function `significance` computes first-order effects `FOE` (main individual effect of every input variable), second-order effects `SOE` (interaction effects between pairs of variables and combined sensitivity indices `SI`. 

```
sig <- significance(output, inputs)
SI  <- sig[[2]] # Saving SI as a separate for later use
print(sig)
```

Here is the result it returns:

$factor_names
[1] "X1" "X2" "X3" "X4"

$SI
[1] 0.04086515 0.51550451 0.09554875 0.35061248

$FOE
[1] 0.0366771 0.4910688 0.1068750 0.2777653

$SOE
     [,1]        [,2]         [,3]        [,4]
[1,]    0 0.003428407  0.001484804 0.003462889
[2,]    0 0.000000000 -0.060462825 0.105905875
[3,]    0 0.000000000  0.000000000 0.036325580
[4,]    0 0.000000000  0.000000000 0.000000000


Each value shows what portion of the variance of the output is explained (negative SOE values indicate correlation). In this example, SI shows that the most significant inputs are X2 (52%) and X4 (35%). SOE points out that there is interaction between X2 and X3 (11%) and correlation between X2 and X3 (-6%).

Lets look at the sum of all the significance indices: 

```
print(sum(SI)
```

We get:

[1] 1.002531

In total, 100% of the output variance is explained (the extra can be attributed to noise).

### Run decomposition
Function `decomposition` chooses the most important input variables, breaks them down into states, forms scenarios out of all combinations of those states and maps the scenarios onto the output values.

```
# Initialize decomposition
dec_limit       <-  0.8 # cummulative significance threshold; % (used to decide how many variables to take for decomposition)
threshold_type  <-  2   # 1 for 'percentile-based' (same amount of observations in each state), 2 for 'median-based' (equaly-spaced ranges)
output_name     <-  colnames(example_data[,1])
var_names       <-  colnames(inputs)
dec             <-  decomposition(output, inputs, SI, dec_limit = 0.8,
                                  manual_vars = NULL, manual_thresholds = NULL,
                                  manual_states = NULL, threshold_type = 2,
                                  var_names = colnames(inputs))
scenario        <- dec[[1]]
scenario_legend <- dec[[2]]
var_names_dec   <- dec[[4]]
print(dec)
```

And this returns: 

$scenarios
   [1] 1 5 8 6 3 2 6 3 5 4 3 3 7 4 6 3 2 3 8 3 6 2 3 7 1 3 1 4 5 2 1 3 6 6 3 2 2 1 1 2 3 4 2 1 3 3 1 5 3 3 6 8
  [53] 1 2 1 7 2 2 1 1 4 2 1 3 1 1 4 6 4 3 2 3 1 2 1 6 3 1 3 3 6 3 1 2 1 2 6 2 3 3 6 2 3 4 1 8 4 3 1 3 1 5 9 3
 [105] 1 3 2 3 3 1 1 1 4 2 6 3 3 2 1 9 3 6 7 4 7 2 6 1 3 4 6 3 1 1 6 5 1 6 9 7 6 6 5 7 4 3 2 1 1 6 6 1 1 2 4 6
 [157] 6 2 3 6 9 6 1 1 3 7 2 2 3 5 6 2 2 1 3 6 2 8 3 4 8 8 2 1 3 1 6 3 2 1 3 4 8 3 6 1 4 4 7 3 8 4 1 3 4 2 4 2
 [209] 1 4 4 6 7 8 3 3 9 2 3 3 5 2 1 1 3 6 1 9 3 9 3 3 6 4 1 6 1 9 4 3 6 3 3 3 3 1 2 3 5 1 3 5 2 1 9 5 3 1 2 3
 [261] 6 6 3 3 4 3 1 8 6 4 9 2 7 2 1 3 4 6 1 1 2 1 9 5 4 7 1 7 7 5 2 9 3 8 7 7 3 3 7 5 2 3 1 1 2 2 8 1 9 5 3 5
 [313] 1 3 3 4 8 9 3 3 3 8 6 8 2 1 2 5 1 1 4 2 2 6 2 2 2 3 3 4 9 1 3 1 3 3 3 3 2 3 2 2 2 7 8 1 6 6 3 3 7 9 6 1
 [365] 6 3 3 6 1 9 8 3 6 4 1 3 3 3 3 3 3 3 6 3 9 9 3 1 6 3 3 6 1 1 1 3 6 8 9 2 2 6 3 5 3 1 3 9 2 4 3 4 9 3 1 9
 [417] 1 2 4 3 2 3 9 4 9 3 1 2 3 3 3 9 7 7 4 1 5 9 1 2 4 2 4 1 9 2 1 1 7 2 3 2 2 7 1 9 3 3 3 6 8 6 1 2 6 9 1 8
 [469] 8 3 3 8 1 6 7 3 1 5 6 8 7 6 3 1 5 5 3 3 3 2 6 3 3 3 2 2 3 1 7 9 3 3 5 3 6 3 1 4 3 3 4 7 9 3 9 6 1 7 8 2
 [521] 4 1 3 2 3 2 1 4 4 3 4 2 6 3 9 3 2 6 2 4 1 3 3 2 4 2 2 8 3 3 3 3 7 3 6 1 3 1 9 3 2 4 6 3 1 3 7 3 2 3 1 1
 [573] 1 1 5 6 6 5 6 1 3 5 4 3 3 6 7 4 9 3 8 3 9 6 3 4 5 1 3 2 6 3 9 6 6 1 8 3 4 6 6 1 2 4 3 4 6 2 6 2 6 9 3 2
 [625] 1 9 7 1 1 2 3 2 3 8 5 9 1 9 1 3 2 6 3 1 1 1 3 3 2 3 2 3 6 3 6 2 9 9 6 5 3 7 5 2 2 2 3 6 7 9 3 6 4 1 4 6
 [677] 7 2 6 3 6 7 9 3 9 4 6 3 4 9 3 3 1 3 1 2 4 1 3 1 6 3 1 5 7 3 6 4 6 3 9 6 1 6 1 6 8 9 2 5 4 5 4 2 5 2 2 1
 [729] 2 9 8 1 7 4 2 6 5 8 1 3 9 3 1 3 4 3 9 7 1 2 6 3 1 1 1 4 8 1 2 1 8 3 3 1 1 4 2 3 9 4 3 6 1 9 5 5 2 3 6 2
 [781] 3 6 7 3 3 5 8 2 3 6 1 3 2 3 7 2 7 3 9 6 1 2 3 1 9 5 3 2 2 6 3 1 7 3 1 1 3 4 3 3 3 1 1 6 3 1 1 3 4 4 3 1
 [833] 3 3 3 5 3 6 7 3 1 2 4 2 1 3 3 3 2 2 1 2 2 6 6 2 1 4 2 2 3 6 6 6 2 9 4 6 2 3 2 7 9 1 3 1 3 6 1 2 7 7 3 5
 [885] 1 7 3 9 3 1 1 9 2 6 4 3 4 9 4 3 1 7 9 4 3 2 3 3 3 3 3 2 3 8 3 6 9 1 2 1 3 3 5 4 2 6 3 1 5 6 9 9 7 4 6 5
 [937] 5 5 2 1 3 4 3 3 1 6 4 3 2 6 7 2 4 5 5 9 3 1 2 4 3 1 2 2 6 1 4 9 4 6 3 4 1 1 6 3 2 3 9 5 9 5 1 6 4 1 7 3
 [989] 1 8 7 7 3 8 4 3 2 4 1 1
 [ reached getOption("max.print") -- omitted 9000 entries ]

$scen_legend
      [,1] [,2] [,3]      [,4]     [,5]     [,6]   [,7]
 [1,]    1    1    1  11.19251 282.0775 460.0744 0.1932
 [2,]    2    1    2  67.53142 407.7922 622.3533 0.1247
 [3,]    3    1    3 237.13240 541.3223 819.4127 0.2556
 [4,]    4    2    1 350.30101 434.8982 523.8420 0.0863
 [5,]    5    2    2 398.42255 485.7198 650.9752 0.0561
 [6,]    6    2    3 414.20952 534.1939 814.4319 0.1122
 [7,]    7    3    1 630.23618 703.9046 794.8068 0.0553
 [8,]    8    3    2 656.33806 725.1508 816.4755 0.0372
 [9,]    9    3    3 668.50405 755.6516 850.9973 0.0794

$thresholds_out
     [,1]       [,2] [,3]        [,4]
[1,]   NA -399.89836   NA -1.19998572
[2,]   NA   50.03854   NA -0.56667382
[3,]   NA  499.97544   NA  0.06663808
[4,]   NA  949.91235   NA  0.69994998

$var_names_dec
[1] "X2" "X4"

### Visualize
The SimDec graph and the corresponding legend is created with the function `build_simdec_chart`.

```
# Initializing plot for tic aesthetics
axistitle   <- c()
main_colors <- c()
visuals     <- build_simdec_chart(output, scenario, scenario_legend,
                                  main_colors, axistitle, var_names_dec)
SimDec_Plot   <- visuals[[1]]
Legdend_Table <- visuals[[2]]
print(SimDec_Plot)
print(Legend_Table
```

And this returns: 

![image](https://github.com/Simulation-Decomposition/simdec-R/assets/131595527/2cac4819-3c8c-42d6-904a-10a9b25a8c3a)

![image](https://github.com/Simulation-Decomposition/simdec-R/assets/131595527/18fb8228-f605-4b41-84c4-9480d49365e3)



SimDec reveals the nature of causalities and interaction effects in the model.
See our [publications](https://www.simdec.fi/publications) and join our
[discord community](https://discord.gg/54SFcNsZS4).

...

## Citations

The algorithms and visualizations used in this package came primarily out of
research at LUT University, Lappeenranta, Finland, and Stanford University,
California, U.S., supported with grants from Business Finland, Wihuri
Foundation, and Finnish Foundation for Economic Education.

If you use SimDec in your research we would appreciate a citation to the
following publications:

- Kozlova, M., & Yeomans, J. S. (2022). Monte Carlo Enhancement via Simulation
  Decomposition: A “Must-Have” Inclusion for Many Disciplines. _INFORMS
  Transactions on Education, 22_(3), 147-159. DOI:10.1287/ited.2019.0240.
- Kozlova, M., Moss, R. J., Yeomans, J. S., & Caers, J. (forthcoming).
  Uncovering Heterogeneous Effects in Computational Models for Sustainable
  Decision-making. _Environmental Modelling & Software_.
- Kozlova, M., Moss, R. J., Roy, P., Alam, A., & Yeomans, J. S. (forthcoming).
  SimDec algorithm. In M. Kozlova & J. S. Yeomans (Eds.), _Sensitivity Analysis
  for Business, Technology, and Policymaking Made Easy with Simulation
  Decomposition_. Routledge.
