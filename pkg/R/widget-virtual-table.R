#' @importFrom uuid UUIDgenerate
#' @include scrolling-table.R
#' @include widget-slider.R
#' @include widget-integer.R
#' @include widget-button.R
#' @include widget-box.R
#' @include widget-string.R

.virtable_widget <- new.env()
.virtable_widget$inited <- FALSE

get_virtable_widget_css <- function(){
    virtable_widget_css <- readLines(system.file("css/virtable-widget.css",
                                            package="RKernel"))
    virtable_widget_css <- paste0(virtable_widget_css,collapse="\n")
    virtable_widget_css <- paste("<style>",virtable_widget_css,"</style>",sep="\n")
    .virtable_widget$inited <- TRUE
    raw_html(virtable_widget_css)
}

#' @export
virtable_widget <- function(x,
        pagesize=getOption("rkernel_view_size",c(10,10))){
    row_pagesize <- min(pagesize[1],nrow(x))
    col_pagesize <- min(pagesize[2],ncol(x))
    if(is.na(col_pagesize))
        col_pagesize <- ncol(x)

    row_control <- FloatSlider(value=100,min=0,max=100,
                               orientation="vertical",
                               readout=FALSE,
                               continuous_update=FALSE)
    col_control <- FloatSlider(value=0,min=0,max=100,
                               readout=FALSE,
                               continuous_update=FALSE)
    
    scroll_up_button <- Button(icon="angle-up")
    scroll_up_button$add_class("scroll-up-button")
    scroll_down_button <- Button(icon="angle-down")
    scroll_down_button$add_class("scroll-down-button")

    scroll_left_button <- Button(icon="angle-left")
    scroll_left_button$add_class("scroll-left-button")
    scroll_right_button <- Button(icon="angle-right")
    scroll_right_button$add_class("scroll-right-button")   
    
    page_up_button <- Button(icon="angle-double-up")
    page_up_button$add_class("page-up-button")
    page_down_button <- Button(icon="angle-double-down")
    page_down_button$add_class("page-down-button")

    page_left_button <- Button(icon="angle-double-left")
    page_left_button$add_class("page-left-button")
    page_right_button <- Button(icon="angle-double-right")
    page_right_button$add_class("page-right-button")   
    
    col_plus_button <- Button(icon="plus")
    col_plus_button$add_class("col-plus-button")
    col_minus_button <- Button(icon="minus")
    col_minus_button$add_class("col-minus-button")
    
    row_plus_button <- Button(icon="plus")
    row_plus_button$add_class("row-plus-button")
    row_minus_button <- Button(icon="minus")
    row_minus_button$add_class("row-minus-button")
    
    row_scrollbar <- VBox(row_plus_button,
                          row_minus_button,
                          page_up_button,
                          scroll_up_button,
                          row_control,
                          scroll_down_button,
                          page_down_button)
    
    col_scrollbar <- HBox(col_plus_button,
                          col_minus_button,
                          page_left_button,
                          scroll_left_button,
                          col_control,
                          scroll_right_button,
                          page_right_button)
    
    row_pagesize_control <- BoundedIntText(value=row_pagesize,
                                           min=1L,max=nrow(x),
                                           description="Rows",
                                           description_tooltip = 
                                               "Number of rows being shown")
    col_pagesize_control <- BoundedIntText(value=col_pagesize,
                                           min=1L,max=ncol(x),
                                           description="Columns",
                                           description_tooltip = 
                                               "Number of columns shown")
    
    tab_container <- HTML(layout=Layout(overflow="auto"))
    
    page_control_box <- VBox(row_pagesize_control,col_pagesize_control)
    
    message_widget <- Label()

    page_control_button <- ToggleButton(icon="lock",
                               tooltip="Show/hide controls for page size")
    page_control_button$add_class("scroll-lock")

    innerbox <- GridBox(list(tab_container,row_scrollbar,col_scrollbar,page_control_button),
                       layout=Layout(
                            grid_template_columns='auto 24px',
                            grid_template_rows='auto 24px',
                            grid_gap='0px 0px')
                            )

    
    outerbox <- VBox(message_widget,page_control_box,innerbox,
                     layout=Layout(overflow='hidden'))
    outerbox$add_class("scrolling-table")
    
    toggle_page_controls <- function(...){
        show_page_controls <- page_control_button$value
        if(show_page_controls){
            page_control_box$layout$display <- "flex"
            page_control_button$icon <- "unlock"
        }
        else{
            page_control_box$layout$display <- "none"
            page_control_button$icon <- "lock"
        }
    }

    page_control_button$on_change(toggle_page_controls)
    
    id <- UUIDgenerate()
    
    scroll_left <- function(...){
        col_pagesize <- min(col_pagesize_control$value,ncol(x))
        col_pagesize <- max(col_pagesize,1L)
        col_prop <- col_control$value/100
        col_pos_old <- as.integer(round((ncol(x) - col_pagesize)*col_prop + col_pagesize))
        col_pos_new <- col_pos_old - 1L
        col_prop_new <- (col_pos_new - col_pagesize)/(ncol(x) - col_pagesize)
        col_control$value <- 100*col_prop_new
    }
    scroll_right <- function(...){
        col_pagesize <- min(col_pagesize_control$value,ncol(x))
        col_pagesize <- max(col_pagesize,1L)
        col_prop <- col_control$value/100
        col_pos_old <- as.integer(round((ncol(x) - col_pagesize)*col_prop + col_pagesize))
        col_pos_new <- col_pos_old + 1L
        col_prop_new <- (col_pos_new - col_pagesize)/(ncol(x) - col_pagesize)
        col_control$value <- 100*col_prop_new
    }
    scroll_up <- function(...){
        row_pagesize <- min(row_pagesize_control$value,nrow(x))
        row_pagesize <- max(row_pagesize,1L)
        row_prop <- row_control$value/100
        row_pos_old <- as.integer(round((nrow(x) - row_pagesize)*row_prop + row_pagesize))
        row_pos_new <- row_pos_old + 1L
        row_prop_new <- (row_pos_new - row_pagesize)/(nrow(x) - row_pagesize)
        row_control$value <- 100*row_prop_new
    }
    scroll_down <- function(...){
        row_pagesize <- min(row_pagesize_control$value,nrow(x))
        row_pagesize <- max(row_pagesize,1L)
        row_prop <- row_control$value/100
        row_pos_old <- as.integer(round((nrow(x) - row_pagesize)*row_prop + row_pagesize))
        row_pos_new <- row_pos_old - 1L
        row_prop_new <- (row_pos_new - row_pagesize)/(nrow(x) - row_pagesize)
        row_control$value <- 100*row_prop_new
    }
    
    page_left <- function(...){
        col_pagesize <- min(col_pagesize_control$value,ncol(x))
        col_pagesize <- max(col_pagesize,1L)
        col_prop <- col_control$value/100
        col_pos_old <- as.integer(round((ncol(x) - col_pagesize)*col_prop + col_pagesize))
        col_pos_new <- col_pos_old - col_pagesize
        col_prop_new <- (col_pos_new - col_pagesize)/(ncol(x) - col_pagesize)
        col_control$value <- 100*col_prop_new
    }
    page_right <- function(...){
        col_pagesize <- min(col_pagesize_control$value,ncol(x))
        col_pagesize <- max(col_pagesize,1L)
        col_prop <- col_control$value/100
        col_pos_old <- as.integer(round((ncol(x) - col_pagesize)*col_prop + col_pagesize))
        col_pos_new <- col_pos_old + col_pagesize
        col_prop_new <- (col_pos_new - col_pagesize)/(ncol(x) - col_pagesize)
        col_control$value <- 100*col_prop_new
    }
    page_up <- function(...){
        row_pagesize <- min(row_pagesize_control$value,nrow(x))
        row_pagesize <- max(row_pagesize,1L)
        row_prop <- row_control$value/100
        row_pos_old <- as.integer(round((nrow(x) - row_pagesize)*row_prop + row_pagesize))
        row_pos_new <- row_pos_old + row_pagesize
        row_prop_new <- (row_pos_new - row_pagesize)/(nrow(x) - row_pagesize)
        row_control$value <- 100*row_prop_new
    }
    page_down <- function(...){
        row_pagesize <- min(row_pagesize_control$value,nrow(x))
        row_pagesize <- max(row_pagesize,1L)
        row_prop <- row_control$value/100
        row_pos_old <- as.integer(round((nrow(x) - row_pagesize)*row_prop + row_pagesize))
        row_pos_new <- row_pos_old - row_pagesize
        row_prop_new <- (row_pos_new - row_pagesize)/(nrow(x) - row_pagesize)
        row_control$value <- 100*row_prop_new
    }
    
    col_pagesize_increase <- function(...){
        col_pagesize_control$value <- min(col_pagesize_control$value + 1L,ncol(x))
    }
    col_pagesize_decrease <- function(...){
        col_pagesize_control$value <- max(col_pagesize_control$value - 1L,1L)
    }
    row_pagesize_increase <- function(...){
        row_pagesize_control$value <- min(row_pagesize_control$value + 1L,nrow(x))
    }
    row_pagesize_decrease <- function(...){
        row_pagesize_control$value <- max(row_pagesize_control$value - 1L,1L)
    }
    
    update_content <- function(...){
        row_pagesize <- min(row_pagesize_control$value,nrow(x))
        row_pagesize <- max(row_pagesize,1L)
        row_prop <- 1 - row_control$value/100
        row.to <- as.integer(round((nrow(x) - row_pagesize)*row_prop + row_pagesize))
        row.from <- as.integer(row.to - row_pagesize + 1)
        
        row.from <- max(row.from,1L)
        row.to <- min(row.to,nrow(x))
        
        col_pagesize <- min(col_pagesize_control$value,ncol(x))
        col_pagesize <- max(col_pagesize,1L)
        col_prop <- col_control$value/100
        col.to <- as.integer(round((ncol(x) - col_pagesize)*col_prop + col_pagesize))
        col.from <- as.integer(col.to - col_pagesize + 1)
        
        col.from <- max(col.from,1L)
        col.to <- min(col.to,ncol(x))
        
        i <- seq.int(from=row.from,to=row.to)
        j <- seq.int(from=col.from,to=col.to)
        
        st <- grid_table(fmt_tab_section(x,i,j),id=id,include_css=FALSE)
        tab_container$value <- st$data[["text/html"]]
        
        paging_message <- NULL
        if(row_pagesize < nrow(x)){
            row_message <- sprintf("rows %d-%d of %d",
                                   row.from,row.to,nrow(x))
            paging_message <- c(paging_message,row_message)
        }
        if(col_pagesize < ncol(x)){
            col_message <- sprintf("columns %d-%d of %d",
                                   col.from,col.to,ncol(x))
            paging_message <- c(paging_message,col_message)
        }
        if(length(paging_message)>0)
            paging_message <- paste("Showing",paste(paging_message,collapse=", "))
        message_widget$value <- paging_message
    
        if(col_pagesize >= ncol(x))
            col_scrollbar$add_class("scrollbar-inactive") 
        else
            col_scrollbar$remove_class("scrollbar-inactive") 


        if(row_pagesize >= nrow(x))
            row_scrollbar$add_class("scrollbar-inactive") 
        else
            row_scrollbar$remove_class("scrollbar-inactive") 

    }
    
    row_control$on_change(update_content)
    col_control$on_change(update_content)
    
    row_pagesize_control$on_change(update_content)
    col_pagesize_control$on_change(update_content)
    
    scroll_left_button$on_click(scroll_left)
    scroll_right_button$on_click(scroll_right)

    scroll_up_button$on_click(scroll_up)
    scroll_down_button$on_click(scroll_down)    
    
    page_left_button$on_click(page_left)
    page_right_button$on_click(page_right)

    page_up_button$on_click(page_up)
    page_down_button$on_click(page_down)    
    
    col_plus_button$on_click(col_pagesize_increase)
    col_minus_button$on_click(col_pagesize_decrease)

    row_plus_button$on_click(row_pagesize_increase)
    row_minus_button$on_click(row_pagesize_decrease)
     
    display(get_grid_table_css())
    display(get_virtable_widget_css())

    toggle_page_controls()
    update_content()
    
    outerbox
}

#' @export
fmt_tab_section <- function(x,i,j) UseMethod("fmt_tab_section")

#' @export
fmt_tab_section.default <- function(x,i,j) format(x[i,j])

#' @export
fmt_tab_section.tbl_df <- function(x,i,j)  
    format(
        structure(
            as.data.frame(x[i,j]),
            row.names=as.character(i)))
