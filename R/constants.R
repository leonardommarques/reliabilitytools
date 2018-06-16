###############################+
# Contants and variables
# Convenient parameter
###############################+

########+
# ggplot constants
########+

theme_MASTER = ggplot2::theme(axis.text.x=element_text(angle=90, vjust=.5, size = 13, hjust=1
                                                       # ,family="serif", fontface='plain'
)
, axis.title=element_text(size=14)
, axis.text.y=element_text(size = 7)
, panel.grid.major.x = element_line(colour="gray", size=0.3)
, panel.grid.minor.x = element_blank()
)


######+
# theme for lifecurves
# these objects change the grids and the breaks of the grids.
######+

# -- Grid theme -- #
theme_LIFECURVE = ggplot2::theme(axis.text.x=element_text(hjust=.5, size = 13, vjust=1
                                                          # ,family="serif", fontface='plain'
)
, axis.title=element_text(size=14)
, axis.text.y=element_text(size = 13)
, panel.grid.major.x = element_line(colour="gray", size=0.3)
, panel.grid.major.y = element_line(colour="gray", size=0.3))


# -- x breaks -- #
scale_x_LIFECURVE = ggplot2::scale_x_continuous(
  breaks = function(x) pretty(x,n=10)# function(x) my_break_fun(x,50000)
  , minor_breaks =function(x) pretty(pretty(x,n=10),n=50)# function(x) my_break_fun(x,50000/5)
  , labels = trans_format(function(x) x/10^3
                          , math_format(.x*k))
)

# -- y breaks -- #
scale_y_LIFECURVE = ggplot2::scale_y_continuous(
  labels = percent
  , breaks = function(x) pretty(x,n=10)
)


