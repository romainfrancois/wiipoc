
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jupyter.widgets.controls

<!-- badges: start -->

[![R-CMD-check](https://github.com/romainfrancois/jupyter.widgets.controls/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/romainfrancois/jupyter.widgets.controls/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

jupyter.widgets.controls is a proof of concept package for jupyter
widgets in the [xeus-r](https://github.com/jupyter-xeus/xeus-r) kernel

## Installation

You can install the development version of jupyter.widgets.controls from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("romainfrancois/jupyter.widgets.controls")
```

## Example

``` r
library(jupyter.widgets.controls)
slider_style <- SliderStyle(handle_color = "blue")
x <- IntSlider(style = slider_style)
x
```

``` r
button_style <- ButtonStyle(
    font_family = "Courrier New", 
    font_size = "15px", 
    font_variant = "small-caps", 
    font_weight = "bolder", 
    text_color = "red", 
    text_decoration = "underline"
)
b <- Button(style = button_style, description = "update slider", tooltip = "click the button to update the slider")
b$on("click", function(){
    x$value <- round(runif(1, min = 0, max = 100))
})
b
```

``` r
box <- Box(list(x, b))
box
```

<figure>
<video
src="https://private-user-images.githubusercontent.com/2625526/426546880-b879df15-9453-47b6-8122-2558685f196e.mov?jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbSIsImtleSI6ImtleTUiLCJleHAiOjE3NDI5MDk0NTAsIm5iZiI6MTc0MjkwOTE1MCwicGF0aCI6Ii8yNjI1NTI2LzQyNjU0Njg4MC1iODc5ZGYxNS05NDUzLTQ3YjYtODEyMi0yNTU4Njg1ZjE5NmUubW92P1gtQW16LUFsZ29yaXRobT1BV1M0LUhNQUMtU0hBMjU2JlgtQW16LUNyZWRlbnRpYWw9QUtJQVZDT0RZTFNBNTNQUUs0WkElMkYyMDI1MDMyNSUyRnVzLWVhc3QtMSUyRnMzJTJGYXdzNF9yZXF1ZXN0JlgtQW16LURhdGU9MjAyNTAzMjVUMTMyNTUwWiZYLUFtei1FeHBpcmVzPTMwMCZYLUFtei1TaWduYXR1cmU9YmJiYTY4NDFjNTI2ODBjZGE5ZDY1ZmM0MDliODAxZDI4MGQwNzQyMzNlNmFlNzlmM2ZlZDYxN2VhY2ZmODc3OSZYLUFtei1TaWduZWRIZWFkZXJzPWhvc3QifQ.bKKTWrnb4ur-6pE1H6vl6N8nFFWvHhzF59-BC-0uk68"
controls=""><a
href="https://private-user-images.githubusercontent.com/2625526/426546880-b879df15-9453-47b6-8122-2558685f196e.mov?jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbSIsImtleSI6ImtleTUiLCJleHAiOjE3NDI5MDk0NTAsIm5iZiI6MTc0MjkwOTE1MCwicGF0aCI6Ii8yNjI1NTI2LzQyNjU0Njg4MC1iODc5ZGYxNS05NDUzLTQ3YjYtODEyMi0yNTU4Njg1ZjE5NmUubW92P1gtQW16LUFsZ29yaXRobT1BV1M0LUhNQUMtU0hBMjU2JlgtQW16LUNyZWRlbnRpYWw9QUtJQVZDT0RZTFNBNTNQUUs0WkElMkYyMDI1MDMyNSUyRnVzLWVhc3QtMSUyRnMzJTJGYXdzNF9yZXF1ZXN0JlgtQW16LURhdGU9MjAyNTAzMjVUMTMyNTUwWiZYLUFtei1FeHBpcmVzPTMwMCZYLUFtei1TaWduYXR1cmU9YmJiYTY4NDFjNTI2ODBjZGE5ZDY1ZmM0MDliODAxZDI4MGQwNzQyMzNlNmFlNzlmM2ZlZDYxN2VhY2ZmODc3OSZYLUFtei1TaWduZWRIZWFkZXJzPWhvc3QifQ.bKKTWrnb4ur-6pE1H6vl6N8nFFWvHhzF59-BC-0uk68">Demo</a></video>
<figcaption aria-hidden="true">Demo</figcaption>
</figure>

``` r
library(jupyter.widgets.controls)

progress <- IntProgress(value=0, min = 0, max = 100)
progress
```

``` r
for (i in 1:100) {
    Sys.sleep(.2)
    progress$value <- i
}
```

<figure>
<video
src="https://private-user-images.githubusercontent.com/2625526/429974422-061496f6-473b-4428-b61c-76458e0de06e.mov?jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbSIsImtleSI6ImtleTUiLCJleHAiOjE3NDM2ODczMzEsIm5iZiI6MTc0MzY4NzAzMSwicGF0aCI6Ii8yNjI1NTI2LzQyOTk3NDQyMi0wNjE0OTZmNi00NzNiLTQ0MjgtYjYxYy03NjQ1OGUwZGUwNmUubW92P1gtQW16LUFsZ29yaXRobT1BV1M0LUhNQUMtU0hBMjU2JlgtQW16LUNyZWRlbnRpYWw9QUtJQVZDT0RZTFNBNTNQUUs0WkElMkYyMDI1MDQwMyUyRnVzLWVhc3QtMSUyRnMzJTJGYXdzNF9yZXF1ZXN0JlgtQW16LURhdGU9MjAyNTA0MDNUMTMzMDMxWiZYLUFtei1FeHBpcmVzPTMwMCZYLUFtei1TaWduYXR1cmU9YzE3YWI4MWQyNGQ1NDdiMzNmNmI5OTA0MTI5ZDgwNzA0OGNmMTMzNzc0MDg1YWIxZWY2NWU0ZDFkODQ2YjRjZCZYLUFtei1TaWduZWRIZWFkZXJzPWhvc3QifQ.s2Ks8GuUN7NCxpM89nsvKweYM14nAnTVDnOyXfm7bgU"
controls=""><a
href="https://private-user-images.githubusercontent.com/2625526/429974422-061496f6-473b-4428-b61c-76458e0de06e.mov?jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbSIsImtleSI6ImtleTUiLCJleHAiOjE3NDM2ODczMzEsIm5iZiI6MTc0MzY4NzAzMSwicGF0aCI6Ii8yNjI1NTI2LzQyOTk3NDQyMi0wNjE0OTZmNi00NzNiLTQ0MjgtYjYxYy03NjQ1OGUwZGUwNmUubW92P1gtQW16LUFsZ29yaXRobT1BV1M0LUhNQUMtU0hBMjU2JlgtQW16LUNyZWRlbnRpYWw9QUtJQVZDT0RZTFNBNTNQUUs0WkElMkYyMDI1MDQwMyUyRnVzLWVhc3QtMSUyRnMzJTJGYXdzNF9yZXF1ZXN0JlgtQW16LURhdGU9MjAyNTA0MDNUMTMzMDMxWiZYLUFtei1FeHBpcmVzPTMwMCZYLUFtei1TaWduYXR1cmU9YzE3YWI4MWQyNGQ1NDdiMzNmNmI5OTA0MTI5ZDgwNzA0OGNmMTMzNzc0MDg1YWIxZWY2NWU0ZDFkODQ2YjRjZCZYLUFtei1TaWduZWRIZWFkZXJzPWhvc3QifQ.s2Ks8GuUN7NCxpM89nsvKweYM14nAnTVDnOyXfm7bgU">Demo</a></video>
<figcaption aria-hidden="true">Demo</figcaption>
</figure>

Currently the proof of concept includes these widgets and styles:

``` r
library("jupyter.widgets.controls")
style_widgets <- ls("package:jupyter.widgets.controls", pattern = "^[^j].*Style")
style_widgets
#> [1] "ButtonStyle"       "CheckboxStyle"     "HTMLMathStyle"    
#> [4] "HTMLStyle"         "LabelStyle"        "ProgressStyle"    
#> [7] "SliderStyle"       "TextStyle"         "ToggleButtonStyle"
```

``` r

dom_widgets <- setdiff(ls("package:jupyter.widgets.controls", pattern = "^[^j].*"), style_widgets)
dom_widgets
#>  [1] "Accordion"            "BoundedFloatText"     "BoundedIntText"      
#>  [4] "Box"                  "Button"               "Checkbox"            
#>  [7] "ColorPicker"          "ColorsInput"          "Combobox"            
#> [10] "DatePicker"           "Datetime"             "FloatLogSlider"      
#> [13] "FloatProgress"        "FloatRangeSlider"     "FloatSlider"         
#> [16] "FloatText"            "GridBox"              "HBox"                
#> [19] "HTML"                 "HTMLMath"             "IntProgress"         
#> [22] "IntRangeSlider"       "IntSlider"            "IntText"             
#> [25] "Label"                "NaiveDatetime"        "Password"            
#> [28] "Select"               "SelectionRangeSlider" "SelectionSlider"     
#> [31] "SelectMultiple"       "Stack"                "Tab"                 
#> [34] "Text"                 "Textarea"             "Time"                
#> [37] "ToggleButton"         "VBox"
```

Each factory function, e.g. `Button` corresponds to an R6 class,
e.g. `jupyter.widget.Button`:

``` r
Button
#> function(
#>   description = "",
#>   button_style = "",
#>   disabled = FALSE,
#>   icon = "",
#>   style = ButtonStyle(),
#>   ...,
#>   error_call = current_env()
#> ){
#>   jupyter.widget.Button$new(
#>     description = description,
#>     button_style = button_style,
#>     disabled = disabled,
#>     icon = icon,
#>     style = style,
#>     ...,
#>     error_call = error_call
#>   )
#> }
#> <bytecode: 0x1578e27a8>
#> <environment: namespace:jupyter.widgets.controls>
```

``` r
jupyter.widget.Button
#> <jupyter.widget.Button> object generator
#>   Inherits from: <jupyter.widget.DOMWidget>
#>   Public:
#>     initialize: function (description = "", button_style = "", disabled = FALSE, 
#>     clone: function (deep = FALSE) 
#>   Active bindings:
#>     description: function (x) 
#>     button_style: function (x) 
#>     disabled: function (x) 
#>     icon: function (x) 
#>   Private:
#>     before_comm_open: function () 
#>   Parent env: <environment: namespace:jupyter.widgets.controls>
#>   Locked objects: TRUE
#>   Locked class: FALSE
#>   Portable: TRUE
```
