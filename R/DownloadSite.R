utils::globalVariables(c("MatchID_7M", "MatchID_NG", "Round", "KODate", "Home", "Away", "FTHG", "FTAG", "HTHG", "HTAG"))
#' @title Read a Rmodel query
#'
#' @description download soccer matches from Gooooal.com.
#'
#' @details This function download the soccer matches from Gooooal.com.
#'
#' @seealso \url{https://www.github.com/englianhu/Rmodel} for more details.
#' @examples
#' \dontrun{ downloadSite(http://en.gooooal.com/soccer/result/2013/league_20130905.html) }
#'
#' @import XML
#' @param filename Relative filepath to the current working directory. This must be (*.zip) or it will throw an error.
#' @return A list of data frame.
#' @family Rmodel functions
#' @export
#'
library(XML)
downloadSite = function(URL, web.ext = c("html", "shtml"),
                        pattern = "", verbose = getOption("verbose")) {
  web.rec = tempfile()
  writeLines(URL, web.rec)
  # top directory
  URL.top = if (length(grep("/$", URL)))
    URL
  else paste(dirname(URL), "/", sep = "")
  fullInfoErrorHandler = function(msg, code, domain, line,
                                  col, level, file) {
    # level tells how significant the error is
    #   These are 0, 1, 2, 3 for WARNING, ERROR, FATAL
    #     meaning simple warning, recoverable error and fatal/unrecoverable error.
    #  See XML:::xmlErrorLevel
    #
    # code is an error code, See the values in XML:::xmlParserErrors
    #  XML_HTML_UNKNOWN_TAG, XML_ERR_DOCUMENT_EMPTY
    #
    # domain tells what part of the library raised this error.
    #  See XML:::xmlErrorDomain
    codeMsg = switch(level, "warning", "recoverable error",
                     "fatal error")
    if (verbose)
      message("(!)There was a ", codeMsg, " at line ",
              line, " column ", col, "\n(!)", msg, appendLF = FALSE)
  }
  listLinks = function(URL, web.ext = web.ext, pattern = pattern) {
    if (verbose)
      timestamp()
    URL.dir = if (length(grep("/$", URL)))
      URL
    else paste(dirname(URL), "/", sep = "")
    if (verbose)
      message("Parsing ", URL)
    if (inherits(try(con <- url(URL, open = "r")), "try-error")) {
      if (verbose)
        message("Found an invalid link: ", URL, "\n")
      return(NULL)
    }
    else {
      close(con)
      doc = htmlParse(URL, error = fullInfoErrorHandler)
      nodes = getNodeSet(doc, "//a[@href]")
      # all links
      hrefs = sapply(sapply(nodes, function(x) xmlGetAttr(x,
                                                          "href")), URLdecode)
      if (verbose)
        message("Found ", length(hrefs), " links in total...")
      hrefs = gsub("^[[:space:]]+|[[:space:]]+$", "", hrefs,
                   useBytes = TRUE)
      # links to top URL
      hrefs = sub("^/", URL.top, hrefs, useBytes = TRUE)
      # remove links to other domains
      hrefs = hrefs[!(substr(hrefs, 1, 7) == "http://" &
                        substr(hrefs, 1, nchar(URL.top)) != URL.top)]
      if (verbose)
        message(length(hrefs), " link(s) within the domain...")
      # remove email links
      if (length(mail.idx <- grep("mailto.*@.*\\..*", hrefs,
                                  useBytes = TRUE)))
        hrefs = hrefs[-mail.idx]
      if (verbose)
        message(length(hrefs), " link(s) left when I removed email links...")
      # get relative paths
      hrefs = gsub(URL.dir, "", hrefs, fixed = TRUE, useBytes = TRUE)
      # match specified patterns
      hrefs = hrefs[grep(pattern, hrefs, useBytes = TRUE)]
      if (verbose)
        message(length(hrefs), " link(s) matched with the pattern \"",
                pattern, "\"...")
      # get absolute paths
      abs.hrefs = if (length(hrefs) == 0)
        NULL
      else {
        gsub("\\.\\./", "/", paste(URL.dir, hrefs, sep = ""),
             useBytes = TRUE)
      }
      # find out links to web pages (*.html/shtml)
      web.idx = grep(paste("\\.", web.ext, "$", sep = "",
                           collapse = "|"), hrefs, useBytes = TRUE)
      if (verbose)
        message("Found ", length(web.idx), " further webpage links in the current page...")
      old.rec = readLines(web.rec)
      web.addr = setdiff(abs.hrefs[web.idx], c(old.rec,
                                               ""))
      cat(web.addr, file = web.rec, sep = "\n", append = TRUE)
      if (length(web.idx) == 0) {
        if (verbose)
          message("No further webpage links can be found...\n")
        return(abs.hrefs)
      }
      else {
        if (verbose) {
          message("I'll look for links in further pages...\n")
          flush.console()
        }
        return(c(abs.hrefs, unlist(sapply(web.addr, listLinks,
                                          web.ext = web.ext, pattern = pattern))))
      }
    }
  }
  all.links = unique(c(URL, listLinks(URL, web.ext, pattern)))
  all.links }

# -------------------------

#options(verbose = TRUE)
#lnk = downloadSite("http://en.gooooal.com/soccer/result/2013/league_20130905.html")
# -------------------------

