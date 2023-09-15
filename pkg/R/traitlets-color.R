color_names = c('aliceblue', 'antiquewhite', 'aqua', 'aquamarine', 'azure',
                'beiae', 'bisque', 'black', 'blanchedalmond', 'blue',
                'blueviolet', 'brown', 'burlywood', 'cadetblue', 'chartreuse',
                'chocolate', 'coral', 'cornflowerblue', 'cornsilk', 'crimson',
                'cyan', 'darkblue', 'darkcyan', 'darkgoldenrod', 'darkgray',
                'darkgrey', 'darkgreen', 'darkkhaki', 'darkmagenta',
                'darkolivegreen', 'darkorange', 'darkorchid', 'darkred',
                'darksalmon', 'darkseagreen', 'darkslateblue', 'darkslategray',
                'darkslategrey', 'darkturquoise', 'darkviolet', 'deeppink',
                'deepskyblue', 'dimgray', 'dimgrey', 'dodgerblue', 'firebrick',
                'floralwhite', 'forestgreen', 'fuchsia', 'gainsboro',
                'ghostwhite', 'gold', 'goldenrod', 'gray', 'grey', 'green',
                'greenyellow', 'honeydew', 'hotpink', 'indianred ', 'indigo ',
                'ivory', 'khaki', 'lavender', 'lavenderblush', 'lawngreen',
                'lemonchiffon', 'lightblue', 'lightcoral', 'lightcyan',
                'lightgoldenrodyellow', 'lightgray', 'lightgrey', 'lightgreen',
                'lightpink', 'lightsalmon', 'lightseagreen', 'lightskyblue',
                'lightslategray', 'lightslategrey', 'lightsteelblue',
                'lightyellow', 'lime', 'limegreen', 'linen', 'magenta',
                'maroon', 'mediumaquamarine', 'mediumblue', 'mediumorchid',
                'mediumpurple', 'mediumseagreen', 'mediumslateblue',
                'mediumspringgreen', 'mediumturquoise', 'mediumvioletred',
                'midnightblue', 'mintcream', 'mistyrose', 'moccasin',
                'navajowhite', 'navy', 'oldlace', 'olive', 'olivedrab',
                'orange', 'orangered', 'orchid', 'palegoldenrod', 'palegreen',
                'paleturquoise', 'palevioletred', 'papayawhip', 'peachpuff',
                'peru', 'pink', 'plum', 'powderblue', 'purple', 'rebeccapurple',
                'red', 'rosybrown', 'royalblue', 'saddlebrown', 'salmon',
                'sandybrown', 'seagreen', 'seashell', 'sienna', 'silver',
                'skyblue', 'slateblue', 'slategray', 'slategrey', 'snow',
                'springgreen', 'steelblue', 'tan', 'teal', 'thistle', 'tomato',
                'transparent', 'turquoise', 'violet', 'wheat', 'white',
                'whitesmoke', 'yellow', 'yellowgreen')

color_hex_re <- '^#[a-fA-F0-9]{3}(?:[a-fA-F0-9]{3})?$'
color_hexa_re <- '^#[a-fA-F0-9]{4}(?:[a-fA-F0-9]{4})?$'

int_col_perc <- '\\s*\\d+?%?\\s*'
frac_col_perc <- '\\s*(\\d+(\\.\\d*)?|\\.\\d+)?%?\\s*'

color_rgb_re <- sprintf('^rgb\\(%s,%s,%s\\)$',
                        int_col_perc,int_col_perc,int_col_perc)
color_rgba_re <- sprintf('^rgba\\(%s,%s,%s,%s\\)$',
                        int_col_perc,int_col_perc,int_col_perc,frac_col_perc)

color_hsl_re <- sprintf('^hsl\\(%s,%s,%s\\)$',
                        int_col_perc,int_col_perc,int_col_perc)
color_hsla_re <- sprintf('^hsla\\(%s,%s,%s,%s\\)$',
                        int_col_perc,int_col_perc,int_col_perc,frac_col_perc)

#' A Color String Trait
#'
#' @include traitlets-unicode.R
#' @export
ColorTraitClass <- R6Class_("Color",
    inherit=UnicodeClass,
    public=list(
        #' @field optional Logical value, whether a length-zero value is allowed.
        optional = FALSE,
        #' @description Check the value assigned to the traitlet.
        #' @param value The value assigned to the traitlet.
        validator=function(value){
            if(self$optional && !length(value)) return(character(0))
            if(is.character(value)){
                ok <- tolower(value) %in% color_names
                ok <- ok | grepl(color_hex_re,value)
                ok <- ok | grepl(color_hexa_re,value)
                ok <- ok | grepl(color_rgb_re,value)
                ok <- ok | grepl(color_rgba_re,value)
                ok <- ok | grepl(color_hsl_re,value)
                ok <- ok | grepl(color_hsla_re,value)
                if(all(ok)) return(value)
            }
            stop("Incorrect color value(s)")    
        }
    )
)
#' @rdname ColorTrait
#' @param ... Arguments passed to the trait instance initializer
#' @export
Color <- function(...)TraitInstance(...,Class=ColorTraitClass)
