# MultiresDecomp

To Install

```
# Install package
devtools::install_github("willdrysdale/wsdmiscr")
```

An implementation of the Multiresolution Decomposition of a time series, using the methods presented in The Cospectral Gap and Turbulent Flux Calculations, [Vickers and Marht 2003](https://doi.org/10.1175/1520-0426(2003)20<660:TCGATF>2.0.CO;2). 

```
## Reproduce example from Vickers and Marht 2003

# load example data
dat = data.frame(example = c(1.00,3.00,2.00,5.00,1.00,2.00,1.00,3.00))

# perform decompostion
mr = multi_res_decomp(dat,col1 = "example",time_res = 1,inc_w_n = T)

mr$mr_spectra %$% plot(scale,d_w,log = "x", type = "l")

## Example of multi file analysis

dat = list(df1 = data.frame(param1 = rnorm(1000),
                            param2 = rnorm(1000)
                            ),
            df2 = data.frame(param1 = rnorm(1000),
                             param2 = rnorm(1000)
                            )
            )
            
mr = mrd_and_summarise(dat,
                       id = "example",
                       col1 = "param1",
                       col2 = "param2",
                       time_res = 1,
                       group_names = "p1_p2")

mr$p1_p2 %$% plot(scale,d_w,log = "x",type = "l")

```
