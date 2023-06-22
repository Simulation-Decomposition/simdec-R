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
FOE <- sig[[3]]
SOE <- sig[[4]]
print(SI)
print(FOE)
print(FOE)
```

Here is the result it returns:

SI =

    0.0409
    0.5155
    0.0955
    0.3506

FOE =

    0.0367
    0.4910
    0.1069
    0.2777

SOE =

         0    0.0034    0.0015    0.0035
         0         0   -0.0605    0.1059
         0         0         0    0.0363
         0         0         0         0


Each value shows what portion of the variance of the output is explained (negative SOE values indicate correlation). In this example, SI shows that the most significant inputs are X2 (52%) and X4 (35%). SOE points out that there is interaction between X2 and X3 (11%) and correlation between X2 and X3 (-6%).

Lets look at the sum of all the significance indices: 

```
print(sum(SI))
```

We get:

1.002531

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
print(SI)
print(secnario_legend)
print(var_names_dec)
```

And this returns: 

scenario_legend
     
     1    1    1  11.19251 282.0775 460.0744 0.1932
     2    1    2  67.53142 407.7922 622.3533 0.1247
     3    1    3 237.13240 541.3223 819.4127 0.2556
     4    2    1 350.30101 434.8982 523.8420 0.0863
     5    2    2 398.42255 485.7198 650.9752 0.0561
     6    2    3 414.20952 534.1939 814.4319 0.1122
     7    3    1 630.23618 703.9046 794.8068 0.0553
     8    3    2 656.33806 725.1508 816.4755 0.0372
     9    3    3 668.50405 755.6516 850.9973 0.0794



var_names_dec

    "X2" "X4"

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
print(Legend_Table)
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
