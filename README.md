
<!-- README.md is generated from README.Rmd. Please edit that file -->

**Warning**

This library is under active development and things can change at
anytime! Suggestions and help are greatly appreciated.

# SimDec

![](man/figures/logo.png)

**Simulation decomposition** or **SimDec** is an uncertainty and
sensitivity analysis method, which is based on Monte Carlo simulation.
SimDec consists of three major parts:

1.  computing sensitivity indices,
2.  creating multi-variable scenarios and mapping the output values to
    them, and
3.  visualizing the scenarios on the output distribution by color-coding
    its segments.

**SimDec** reveals the nature of causalities and interaction effects in
the model. Lets have a look at how it works in R!

### Installation

``` r
devtools::install_github("Simulation-Decomposition/simdec-R")
```

    ── R CMD build ─────────────────────────────────────────────────────────────────
             checking for file 'C:\Users\abidn\AppData\Local\Temp\RtmpmQPXHc\remotes12a430d62650\Simulation-Decomposition-simdec-R-cd66c8c/DESCRIPTION' ...  ✔  checking for file 'C:\Users\abidn\AppData\Local\Temp\RtmpmQPXHc\remotes12a430d62650\Simulation-Decomposition-simdec-R-cd66c8c/DESCRIPTION'
          ─  preparing 'SimDec':
       checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
          ─  checking for LF line-endings in source and make files and shell scripts
          ─  checking for empty or unneeded directories
         NB: this package now depends on R (>=        NB: this package now depends on R (>= 3.5.0)
           WARNING: Added dependency on R >= 3.5.0 because serialized objects in
         serialize/load version 3 cannot be read in older versions of R.
         File(s) containing such objects:
           'SimDec/data/example_data_2.rda'
       building 'SimDec_0.1.0.tar.gz'  ─  building 'SimDec_0.1.0.tar.gz'
         

### Loading Data

First the simulated ‘inputs’ and the ‘output’ need to be specified. They
can result from a Monte Carlo simulation arranged directly in R, or
conducted elsewhere and then loaded through a file, like in this
example. Lets use the the first example data that comes with the R
package.

``` r
data(example_data)                                              
output <- example_data[,1]                                      
inputs <- example_data[,2:5]                                    
```

### Compute Sensitivity Indices

Function `sensitivity_indices` computes first-order effects `FOE` (main
individual effect of every input variable), second-order effects `SOE`
(interaction effects between pairs of variables and combined sensitivity
indices `SI`.

``` r
sen    <- sensitivity_indices(output, inputs)     
SI     <- sen$SI                                  
print(round(SI, 4))                                         
```

    [1] 0.0409 0.5155 0.0955 0.3506

``` r
print(round(sen$FOE, 4))                                    
```

    [1] 0.0367 0.4911 0.1069 0.2778

``` r
print(round(sen$SOE, 4))                                    
```

         [,1]   [,2]    [,3]   [,4]
    [1,]    0 0.0034  0.0015 0.0035
    [2,]    0 0.0000 -0.0605 0.1059
    [3,]    0 0.0000  0.0000 0.0363
    [4,]    0 0.0000  0.0000 0.0000

Each value shows what portion of the variance of the output is explained
(negative SOE values indicate correlation). In this example, SI shows
that the most significant inputs are X2 (52%) and X4 (35%). SOE points
out that there is interaction between X2 and X3 (11%) and correlation
between X2 and X3 (-6%).

### Visualize

The function ‘simdec_visualization’

1.  Chooses the most important input variables
2.  Breaks them down into states
3.  Forms scenarios out of all combinations of those states
4.  Maps the scenarios onto the output values
5.  Visualizes these scenarios by color-coding the distribution of the
    output.

#### Default Visualization (One-Output Stacked Histogram)

``` r
auto_vis    <- simdec_visualization(output, inputs, SI)
print(auto_vis$simdec_plot)
```

<img src="man/figuresunnamed-chunk-6-1.png" width="100%" />

``` r
#auto_vis$legend_table                                 # un-comment and run
```

<p align="center">
<img src="man/figures/auto_vis_legend_table.png" width="600" />
</p>

That’s it, your ‘SimDec’ analysis is completed!

But you can customize it further.

And feel free to go an extra step in your reporting, - name the states
(i.e., low, medium, high) and merge the cells of the legend with the
same state. The help to make those automatic in would be greatly
appreciated!

#### Custom Visualization (One-Output Stacked Histogram)

The ‘simdec_visualization’ function has numerous optional arguments that
can be used to polish the outlook of the results, tune and play with the
decomposition set-up.

Here is how you can create a custom decomposition

``` r
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
print(custom_vis$simdec_plot)                                                             # Viewing the plot
```

<img src="man/figuresunnamed-chunk-7-1.png" width="100%" />

``` r
#custom_vis$legend_table                                                                  # un-comment and run
```

<p align="center">
<img src="man/figures/custom_vis_legend_table.png" width="600" />
</p>

#### Boxplot

Adding option plot_type = “Boxplot” in the ‘simdec_visualization’
function displays the results in the form of boxplot instead of stacked
histogram.

``` r
vis_boxplot <- simdec_visualization(output, inputs, SI, plot_type = "Boxplot")
print(vis_boxplot$box_plot)
```

<img src="man/figuresunnamed-chunk-8-1.png" width="100%" />

``` r
# vis_boxplot$legend_table                           # un-comment and run
```

<p align="center">
<img src="man/figures/boxplot_legend.png" width="600" />
</p>

The boxplot visualization presents exactly the same decomposition and
contains the same scenarios, color-coded in the same way as in the
stacked histogram.

A boxplot is handy when some scenarios have little data and are poorly
visible on the histogram.

#### Two-Output Scatterhist

If relationship between two output variables is in question, the
‘simdec_visualization’ function can build a scatter plot with two
corresponding histograms on the top and right side, all decomposed and
colored by the logic created for the main output variable.

To create the scatterhist one needs to specify the second output
variable for the argument ‘otput_2’, additional optional arguments
include ‘Scatter_Fraction’, which defines how many dots are shown on the
scatter plot (1 - all, 0.5 - every second). For other optional arguments
run ‘?simdec_visualization’.

``` r
data("example_data_2")
output_1        <- example_data_2[, 1]
output_2        <- example_data_2[, 2]
inputs          <- example_data_2[, 3:10]
sen    <- sensitivity_indices(output, inputs)     
SI     <- sen$SI

# un-comment and run 

# vis_scatterhist <- simdec_visualization(output_1, inputs, SI, output_2, Scatter_Fraction = 0.005)
# vis_scatterhist$scatter_hist
# vis_scatterhist$legend_table
```

<p align="center">
<img src="man/figures/scatterhist.png" width="1000" />
</p>
<p align="center">
<img src="man/figures/scatterhist_legend.png" width="600" />
</p>

We can change x- and y-axes limits for both histograms, the scatter plot
scales accordingly.

``` r
# custom_scatterhist <- simdec_visualization(output   = output_1,
#                                            inputs   = inputs,
#                                            SI       = SI,
#                                            output_2 = output_2,
#                                            XLim     = c(1000, 3000),
#                                            YLim     = c(0, 4), 
#                                            Scatter_Fraction = 0.005)
# custom_scatterhist$scatter_hist            # un-comment and run 
```

<p align="center">
<img src="man/figures/custom_scatterhist.png" />
</p>

(The graph demonstrates that not only Output1 axis has been changed as
specified in the code above (scatter plot x-axis), but the second
histogram has been automatically truncated as well (x-axis of the
rotated right histogram and the corresponding y-axis of the scatter
plot) to avoid empty space in the scatter plot.)

If the full control over the both axes of the scatterplot is needed,
both ‘XLim’ and ‘XLim2’ should be specified.

``` r
# un-comment and run

# custom_scatterhist_2 <- simdec_visualization(output   = output_1,
#                                              inputs   = inputs,
#                                              SI       = SI,
#                                              output_2 = output_2,
#                                              XLim     = c(1000, 3000),
#                                              XLim2     = c(0, 1000),
#                                              Scatter_Fraction = 0.005)
# custom_scatterhist_2$scatter_hist
```

<p align="center">
<img src="man/figures/custom_scatterhist_2.png"  />
</p>
