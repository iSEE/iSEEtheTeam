.photoFile <- "PhotoFile"

collated <- character(0)
collated[.photoFile] <- "character"

#' @importClassesFrom iSEE Panel
#' @export
setClass("KevinRue", contains="Panel", slots=collated)

#' Kevin Rue
#' 
#' @name KevinRue-class
#' 
#' @aliases 
#' initialize,KevinRue-method
#' .refineParameters,KevinRue-method
#' .panelColor,KevinRue-method
#' .fullName,KevinRue-method
#' .defineOutput,KevinRue-method
#' .renderOutput,KevinRue-method
NULL

#' Kevin Rue
#' 
#' @export
#' @examples
#' library(SummarizedExperiment)
#' library(iSEE)
#' 
#' se <- SummarizedExperiment()
#' 
#' iSEE(se, initial = list(
#'   KevinRue(PanelWidth=3L, PanelHeight=400L),
#'   FedericoMarini(PanelWidth=3L, PanelHeight=400L),
#'   CharlotteSoneson(PanelWidth=3L, PanelHeight=400L),
#'   AaronLun(PanelWidth=3L, PanelHeight=400L)
#' ))
KevinRue <- function(...) {
    new("KevinRue", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "KevinRue", function(.Object, ...) {
    args <- list(...)
    
    args <- .emptyDefault(args, .photoFile, system.file(package = "iSEEtheTeam", "images", "kevin-rue-albrecht.jpeg"))

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "KevinRue", function(x, se) {
    x <- callNextMethod()
    
    if (is.null(x)) {
        return(NULL)
    }
    
    x
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".panelColor", "KevinRue", function(x) {
    "#007bff"
})

#' @export
setMethod(".fullName", "KevinRue", function(x) "Kevin Rue")

#' @importFrom shiny imageOutput
#' @importFrom shinyWidgets addSpinner
#' @export
setMethod(".defineOutput", "KevinRue", function(x) {
    plot_name <- .getEncodedName(x)
    addSpinner(
        imageOutput(plot_name, height=paste0(slot(x, .organizationHeight), "px")),
        color=.panelColor(x)
    )
})

#' @export
#' @importFrom shiny renderImage tagList
setMethod(".renderOutput", "KevinRue", function(x, se, output, pObjects, rObjects) {
    plot_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.
    
    # width  <- session$clientData[[paste0("output_", plot_name, "_width")]]
    # height <- session$clientData[[paste0("output_", plot_name, "_height")]]

    # nocov start
    output[[plot_name]] <- renderImage({
        list(src = x[[.photoFile]],
            width = "100%",
            # height = "100%",
            alt = "Kevin Rue")
    }, deleteFile = FALSE)
    # nocov end

    callNextMethod()
})
