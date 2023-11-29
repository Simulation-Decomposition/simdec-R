> **Warning**
> This library is under active development and things can change at anytime! Suggestions and help are greatly appreciated.

![image](https://user-images.githubusercontent.com/37065157/233836694-5312496e-4ada-47cb-bc09-3bf8c00be135.png)

<!---
When public
![image](https://raw.githubusercontent.com/Simulation-Decomposition/simdec-python/main/docs/_static/simdec_presentation.png)
-->

**Simulation decomposition** or **SimDec** is an uncertainty and sensitivity
analysis method, which is based on Monte Carlo simulation. SimDec consists of
three major parts:

1. computing sensitivity indices,
2. creating multi-variable scenarios and mapping the output values to them, and
3. visualizing the scenarios on the output distribution by color-coding its segments.

SimDec reveals the nature of causalities and interaction effects in the model. Lets have a look at how it works in R!
### Installing the package from Github

```
# install.packages("devtools")
library(devtools)
install_github("Simulation-Decomposition/simdec-R")
library(SimDec)
```

### Load data 
First the simulated `inputs` and the `output` need to be specified. They can result from a Monte Carlo simulation arranged directly in matlab, or conducted elsewhere and then loaded through a file, like in this example. Lets use the example data that will come with the R package.  

```
rm(list=ls())                                                                             # Clearing the environment
library(Simdec)                                                                           # Loading Simdec
data(example_data)                                                                        # Loading the example data
output <- example_data[,1]                                                                # Defining the output variable
inputs <- example_data[,2:5]                                                              # Definging the input Variables
```

### Compute sensitivity indices
Function `sensitivity_indices` computes first-order effects `FOE` (main individual effect of every input variable), second-order effects `SOE` (interaction effects between pairs of variables and combined sensitivity indices `SI`. 

```
sen    <- sensitivity_indices(output, inputs)                                             # Storing results in an object can "sen"
SI     <- sen$SI                                                                          # Extracting/saving calculated sensitivity indices
print(SI)                                                                                 # Viewing calculated sensitivity indices
print(sen$FOE)                                                                            # Viewing calculated first order effects
print(sen$FOE)                                                                            # Viewing calculated second order effects
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

### Visualize

Function simdec_visualization.R

1. Chooses the most important input variables
2. Breaks them down into states
3. Forms scenarios out of all combinations of those states
4. Maps the scenarios onto the output values
5. Visualizes these scenarios by color-coding the distribution of the output.

```
auto_vis    <- simdec_visualization(output, inputs, SI)                                   # Storing results in an object calls "auto_vis"
auto_vis$simdec_plot                                                                      # Viewing the plot
auto_vis$legend_table                                                                     # Viewing the legend table
```

![image](https://github.com/Simulation-Decomposition/simdec-R/assets/131595527/49cd157d-f4d3-4402-8dba-c444d4a108cf)


![image](https://github.com/Simulation-Decomposition/simdec-R/assets/131595527/91ba105a-f57b-4ff0-93e1-094404bf8e1f)

That's it, your SimDec analysis is completed!

But you can customize it furhter.

And feel free to go an extra step in your reporting, - name the states (i.e., low, medium, high) and merge the cells of the legend with the same state. The help to make those automatic in would be greatly approeciated!

### Customize

The simdec_visualization.m function has numerious optional arguments that can be used to polish the outlook of the results, tune and play with the decomposition set-up.

### Here is how you can create a custom decomposition

```
order_of_variables_m   <- c(0, 2, 1, 0)                                                   # Specifying the order of variables for decomposition,
                                                                                          # use 0 to exclude. In this example, we set that the
                                                                                          # third input variable to be used first, and then
                                                                                          # the second variable.

number_of_states_m     <- c(0, 3, 2, 0)                                                   # Specifying the number of states for each variable. The
                                                                                          # position corresponds to the original order of inputs.

state_boundaries_m     <- matrix(c(NA, min(inputs[,2]), min(inputs[,3]),                  # Specifying numeric thresholds for every state
                                   NA, NA, 100, 657.5, NA,  
                                   NA, 650, max(inputs[,3]),
                                   NA, NA, max(inputs[,2]), NA, NA),
                                 nrow = max(number_of_states_m)+1,
                                 ncol = length(order_of_variables_m),
                                 byrow = TRUE)  

main_colors_m          <- c('#8c5eff', '#ffe252', '#0dd189')                              # Specifying the main colors to be used

custom_vis             <- simdec_visualization(output, inputs, SI,                        # Storing the results in an object called "custom_vis"
                                               order_of_variables = order_of_variables_m,
                                               number_of_states   = number_of_states_m,
                                               state_boundaries   = state_boundaries_m,
                                               main_colors        = main_colors_m)
custom_vis$simdec_plot                                                                    # Viewing the plot
custom_vis$legend_table                                                                   # viewing the legend table
```

And this returns: 

![image](https://github.com/Simulation-Decomposition/simdec-R/assets/131595527/f2a834b2-0949-4234-bad4-b9235142bf18)


![image](https://github.com/Simulation-Decomposition/simdec-R/assets/131595527/4db4425d-39b3-47b6-a1b6-546dfce07e09)

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

- Kozlova, M., & Yeomans, J. S. (2022). Monte Carlo Enhancement via Simulation Decomposition:
  A “Must-Have” Inclusion for Many Disciplines. INFORMS Transactions on Education, 22(3), 147-159. Available here.

- Kozlova, M., Moss, R. J., Yeomans, J. S., & Caers, J. (2024). Uncovering Heterogeneous Effects in Computational
  Models for Sustainable Decision-making. Environmental Modelling & Software, 171, 105898.
  [https://doi.org/10.1016/j.envsoft.2023.105898](https://doi.org/10.1016/j.envsoft.2023.105898)

- Kozlova, M., Moss, R. J., Roy, P., Alam, A., & Yeomans, J. S. (forthcoming). SimDec algorithm and guidelines
  for its usage and interpretation. In M. Kozlova & J. S. Yeomans (Eds.), Sensitivity Analysis for Business,
  Technology, and Policymaking. Made Easy with Simulation Decomposition. Routledge.
