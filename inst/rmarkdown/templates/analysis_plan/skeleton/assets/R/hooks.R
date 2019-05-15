# hooks.R - knitr hooks to automatically add some HTML styling to chunk output.
# created by Paul Stevenson, 2018-09-27

# Adds kable styling to data.frame outptu when including chunk option "table = T"
# applies formatting to source hook, optimal results with "results = 'hide'"
default_source_hook <- knit_hooks$get('source')
knit_hooks$set(
  source = function(x, options) {
    if(is.null(options$table))
      default_source_hook(x, options)
    else {
      eval(parse(text = x)) %>%
        kable("html") %>%
        kable_styling("hover", full_width = FALSE)
    }

  }
)

# Adds image classes to figures when including chunk option "out.extra = 'figure'"
opts_hooks$set(out.extra = function(options) {
  if (options$out.extra == "figure")
    options$out.extra = "class='img-fluid center'"
  options
})
