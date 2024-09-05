.photoFile <- "PhotoFile"

collated <- character(0)
collated[.photoFile] <- "character"

#' @importClassesFrom iSEE Panel
#' @export
setClass("AaronLun", contains="Panel", slots=collated)

#' Aaron Lun
#' 
#' @name AaronLun-class
#' 
#' @aliases 
#' initialize,AaronLun-method
#' .refineParameters,AaronLun-method
#' .panelColor,AaronLun-method
#' .fullName,AaronLun-method
#' .defineOutput,AaronLun-method
#' .renderOutput,AaronLun-method
NULL

#' Aaron Lun
#' 
#' @export
#' @examples
#' library(SummarizedExperiment)
#' library(iSEE)
#' 
#' se <- SummarizedExperiment()
#' 
#' iSEE(se, initial = list(AaronLun()))
AaronLun <- function(...) {
    new("AaronLun", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "AaronLun", function(.Object, ...) {
    args <- list(...)
    
    args <- .emptyDefault(args, .photoFile, system.file(package = "iSEEtheTeam", "images", "aaron-lun.png"))

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "AaronLun", function(x, se) {
    x <- callNextMethod()
    
    if (is.null(x)) {
        return(NULL)
    }
    
    x
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".panelColor", "AaronLun", function(x) {
    "#dc3545"
})

#' @export
setMethod(".fullName", "AaronLun", function(x) "Aaron Lun")

#' @importFrom shiny imageOutput
#' @importFrom shinyWidgets addSpinner
#' @export
setMethod(".defineOutput", "AaronLun", function(x) {
    plot_name <- .getEncodedName(x)
    addSpinner(
        imageOutput(plot_name, height=paste0(slot(x, .organizationHeight), "px")),
        color=.panelColor(x)
    )
})

#' @export
#' @importFrom shiny renderImage tagList
setMethod(".renderOutput", "AaronLun", function(x, se, output, pObjects, rObjects) {
    plot_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.
    
    # width  <- session$clientData[[paste0("output_", plot_name, "_width")]]
    # height <- session$clientData[[paste0("output_", plot_name, "_height")]]

    # nocov start
    output[[plot_name]] <- renderImage({
        list(src = x[[.photoFile]],
            width = "100%",
            # height = "100%",
            alt = "Aaron Lun")
    }, deleteFile = FALSE)
    # nocov end

    callNextMethod()
})
