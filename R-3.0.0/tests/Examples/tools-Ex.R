pkgname <- "tools"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('tools')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("HTMLheader")
### * HTMLheader

flush(stderr()); flush(stdout())

### Name: HTMLheader
### Title: Generate a standard HTML header for R help
### Aliases: HTMLheader
### Keywords: utilities documentation

### ** Examples

cat(HTMLheader("This is a sample header"), sep="\n")



cleanEx()
nameEx("Rd2HTML")
### * Rd2HTML

flush(stderr()); flush(stdout())

### Name: Rd2HTML
### Title: Rd Converters
### Aliases: Rd2txt Rd2HTML Rd2ex Rd2latex
### Keywords: documentation

### ** Examples
cleanEx()
nameEx("Rd2txt_options")
### * Rd2txt_options

flush(stderr()); flush(stdout())

### Name: Rd2txt_options
### Title: Set formatting options for text help
### Aliases: Rd2txt_options
### Keywords: documentation

### ** Examples

saveOpts <- Rd2txt_options()
saveOpts
Rd2txt_options(minIndent = 4)
Rd2txt_options()
Rd2txt_options(saveOpts)
Rd2txt_options()



cleanEx()
nameEx("Rdutils")
### * Rdutils

flush(stderr()); flush(stdout())

### Name: Rdutils
### Title: Rd Utilities
### Aliases: Rd_db
### Keywords: utilities documentation

### ** Examples


cleanEx()
nameEx("bibstyle")
### * bibstyle

flush(stderr()); flush(stdout())

### Name: bibstyle
### Title: Select or define a bibliography style.
### Aliases: bibstyle getBibstyle
### Keywords: utilties documentation

### ** Examples

## Don't show: 
options(useFancyQuotes = FALSE)
## End Don't show
refs <-
c(bibentry(bibtype = "manual",
    title = "R: A Language and Environment for Statistical Computing",
    author = person("R Core Team"),
    organization = "R Foundation for Statistical Computing",
    address = "Vienna, Austria",
    year = 2013,
    url = "http://www.R-project.org"),
  bibentry(bibtype = "article",
    author = c(person(c("George", "E.", "P."), "Box"),
               person(c("David",  "R."),      "Cox")),
    year = 1964,
    title = "An Analysis of Transformations",
    journal = "Journal of the Royal Statistical Society, Series B",
    volume = 26,
    pages = "211-252"))

bibstyle("unsorted", sortKeys = function(refs) seq_along(refs),
    fmtPrefix = function(paper) paste0("[", paper$.index, "]"),
       .init = TRUE)
print(refs, .bibstyle = "unsorted")



cleanEx()
nameEx("charsets")
### * charsets

flush(stderr()); flush(stdout())

### Name: charsets
### Title: Conversion Tables between Character Sets
### Aliases: Adobe_glyphs charset_to_Unicode
### Keywords: datasets

### ** Examples

## find Adobe names for ISOLatin2 chars.
latin2 <- charset_to_Unicode[, "ISOLatin2"]
aUnicode <- as.numeric(paste0("0x", Adobe_glyphs$unicode))
keep <- aUnicode %in% latin2
aUnicode <- aUnicode[keep]
aAdobe <- Adobe_glyphs[keep, 1]
## first match
aLatin2 <- aAdobe[match(latin2, aUnicode)]
## all matches
bLatin2 <- lapply(1:256, function(x) aAdobe[aUnicode == latin2[x]])
format(bLatin2, justify = "none")



cleanEx()
nameEx("checkFF")
### * checkFF

flush(stderr()); flush(stdout())

### Name: checkFF
### Title: Check Foreign Function Calls
### Aliases: checkFF print.checkFF
### Keywords: programming utilities

### ** Examples


cleanEx()
nameEx("checkPoFiles")
### * checkPoFiles

flush(stderr()); flush(stdout())

### Name: checkPoFiles
### Title: Check translation files for inconsistent format strings.
### Aliases: checkPoFile checkPoFiles
### Keywords: utilities

### ** Examples

## Not run: 
##D checkPoFiles("de", "/path/to/R/src/directory")
## End(Not run)



cleanEx()
nameEx("checkRdaFiles")
### * checkRdaFiles

flush(stderr()); flush(stdout())

### Name: checkRdaFiles
### Title: Report on Details of Saved Images or Re-saves them
### Aliases: checkRdaFiles resaveRdaFiles
### Keywords: utilities

### ** Examples
## Not run: 
##D ## from a package top-level source directory
##D paths <- sort(Sys.glob(c("data/*.rda", "data/*.RData")))
##D (res <- checkRdaFiles(paths))
##D ## pick out some that may need attention
##D bad <- is.na(res$ASCII) | res$ASCII | (res$size > 1e4 & res$compress == "none")
##D res[bad, ]
## End(Not run)


cleanEx()
nameEx("delimMatch")
### * delimMatch

flush(stderr()); flush(stdout())

### Name: delimMatch
### Title: Delimited Pattern Matching
### Aliases: delimMatch
### Keywords: character

### ** Examples

x <- c("\\value{foo}", "function(bar)")
delimMatch(x)
delimMatch(x, c("(", ")"))



cleanEx()
nameEx("dependsOnPkgs")
### * dependsOnPkgs

flush(stderr()); flush(stdout())

### Name: dependsOnPkgs
### Title: Find Reverse Dependencies
### Aliases: dependsOnPkgs
### Keywords: utilities

### ** Examples

## there are few dependencies in a vanilla R installation
dependsOnPkgs("lattice")



cleanEx()
nameEx("encoded")
### * encoded

flush(stderr()); flush(stdout())

### Name: encoded_text_to_latex
### Title: Translate non-ASCII Text to LaTeX Escapes
### Aliases: encoded_text_to_latex
### Keywords: utilities

### ** Examples

x <- "fa\xE7ile"
encoded_text_to_latex(x, "latin1")
## Not run: 
##D ## create a tex file to show the upper half of 8-bit charsets
##D x <- rawToChar(as.raw(160:255), multiple = TRUE)
##D (x <- matrix(x, ncol = 16, byrow = TRUE))
##D xx <- x
##D xx[] <- encoded_text_to_latex(x, "latin1") # or latin2 or latin9
##D xx <- apply(xx, 1, paste, collapse = "&")
##D con <- file("test-encoding.tex", "w")
##D header <- c(
##D "\\documentclass{article}",
##D "\\usepackage[T1]{fontenc}",
##D "\\usepackage{Rd}",
##D "\\begin{document}",
##D "\\HeaderA{test}{}{test}",
##D "\\begin{Details}\relax",
##D "\\Tabular{cccccccccccccccc}{")
##D trailer <- c("}", "\\end{Details}", "\\end{document}")
##D writeLines(header, con)
##D writeLines(paste0(xx, "\\"), con)
##D writeLines(trailer, con)
##D close(con)
##D ## and some UTF_8 chars
##D x <- intToUtf8(as.integer(
##D     c(160:383,0x0192,0x02C6,0x02C7,0x02CA,0x02D8,
##D       0x02D9, 0x02DD, 0x200C, 0x2018, 0x2019, 0x201C,
##D       0x201D, 0x2020, 0x2022, 0x2026, 0x20AC)),
##D                multiple = TRUE)
##D x <- matrix(x, ncol = 16, byrow = TRUE)
##D xx <- x
##D xx[] <- encoded_text_to_latex(x, "UTF-8")
##D xx <- apply(xx, 1, paste, collapse = "&")
##D con <- file("test-utf8.tex", "w")
##D writeLines(header, con)
##D writeLines(paste(xx, "\\", sep = ""), con)
##D writeLines(trailer, con)
##D close(con)
## End(Not run)


cleanEx()
nameEx("fileutils")
### * fileutils

flush(stderr()); flush(stdout())

### Name: fileutils
### Title: File Utilities
### Aliases: file_ext file_path_as_absolute file_path_sans_ext
###   list_files_with_exts list_files_with_type
### Keywords: file

### ** Examples


cleanEx()
nameEx("getDepList")
### * getDepList

flush(stderr()); flush(stdout())

### Name: getDepList
### Title: Functions to Retrieve Dependency Information
### Aliases: getDepList pkgDepends
### Keywords: utilities

### ** Examples

pkgDepends("tools", local = FALSE)



cleanEx()
nameEx("installFoundDepends")
### * installFoundDepends

flush(stderr()); flush(stdout())

### Name: installFoundDepends
### Title: A function to install unresolved dependencies
### Aliases: installFoundDepends
### Keywords: utilities

### ** Examples

## Set up a temporary directory to install packages to
tmp <- tempfile()
dir.create(tmp)

pDL <- pkgDepends("tools", local = FALSE)
installFoundDepends(pDL$Found, destdir = tmp)



cleanEx()
nameEx("md5sum")
### * md5sum

flush(stderr()); flush(stdout())

### Name: md5sum
### Title: Compute MD5 Checksums
### Aliases: md5sum
### Keywords: utilities

### ** Examples

as.vector(md5sum(dir(R.home(), pattern = "^COPY", full.names = TRUE)))



cleanEx()
nameEx("package_dependencies")
### * package_dependencies

flush(stderr()); flush(stdout())

### Name: package_dependencies
### Title: Computations on the Dependency Hierarchy of Packages
### Aliases: package_dependencies
### Keywords: utilities

### ** Examples

## Not run: 
##D pdb <- available.packages()
##D deps <- package_dependencies(packages = "MASS", pdb,
##D                     which = c("Depends", "Imports", "LinkingTo", "Suggests"),
##D                     recursive = TRUE, reverse = TRUE)
##D length(deps$MASS)
## End(Not run)



cleanEx()
nameEx("parseLatex")
### * parseLatex

flush(stderr()); flush(stdout())

### Name: parseLatex
### Title: These experimental functions work with a subset of LaTeX code.
### Aliases: parseLatex deparseLatex latexToUtf8
### Keywords: utilities documentation

### ** Examples


cleanEx()
nameEx("print.via.format")
### * print.via.format

flush(stderr()); flush(stdout())

### Name: .print.via.format
### Title: Printing Utilities
### Aliases: .print.via.format
### Keywords: utilities

### ** Examples

## The function is simply defined as
 function (x, ...) {
    writeLines(format(x, ...))
    invisible(x)
 }

## is used for simple print methods in R, and as prototype for new methods.



cleanEx()
nameEx("pskill")
### * pskill

flush(stderr()); flush(stdout())

### Name: pskill
### Title: Kill a Process
### Aliases: pskill SIGHUP SIGINT SIGQUIT SIGKILL SIGTERM SIGSTOP SIGTSTP
###   SIGCONT SIGCHLD SIGUSR1 SIGUSR2
### Keywords: utilities

### ** Examples
## Not run: 
##D pskill(c(237, 245), SIGKILL)
## End(Not run)


cleanEx()
nameEx("readNEWS")
### * readNEWS

flush(stderr()); flush(stdout())

### Name: readNEWS
### Title: Read R's NEWS file or a similar one
### Aliases: readNEWS checkNEWS
### Keywords: file documentation

### ** Examples
# Check the current NEWS file

stopifnot(checkNEWS())




cleanEx()
nameEx("showNonASCII")
### * showNonASCII

flush(stderr()); flush(stdout())

### Name: showNonASCII
### Title: Pick Out Non-ASCII Characters
### Aliases: showNonASCII showNonASCIIfile
### Keywords: utilities

### ** Examples

out <- c(
"fa\xE7ile test of showNonASCII():",
"\\details{",
"   This is a good line",
"   This has an \xfcmlaut in it.",
"   OK again.",
"}")
f <- tempfile()
cat(out, file = f, sep = "\n")

showNonASCIIfile(f)
unlink(f)



cleanEx()
nameEx("toHTML")
### * toHTML

flush(stderr()); flush(stdout())

### Name: toHTML
### Title: Display an object in HTML.
### Aliases: toHTML toHTML.packageIQR toHTML.news_db
### Keywords: utilities documentation

### ** Examples

cat(toHTML(demo(package = "base")), sep = "\n")



cleanEx()
nameEx("undoc")
### * undoc

flush(stderr()); flush(stdout())

### Name: undoc
### Title: Find Undocumented Objects
### Aliases: undoc print.undoc
### Keywords: documentation

### ** Examples

undoc("tools")                  # Undocumented objects in 'tools'



cleanEx()
nameEx("vignetteDepends")
### * vignetteDepends

flush(stderr()); flush(stdout())

### Name: vignetteDepends
### Title: Retrieve Dependency Information for a Vignette
### Aliases: vignetteDepends
### Keywords: utilities

### ** Examples

## This may not be installed
gridEx <- system.file("doc", "grid.Rnw", package = "grid")
vignetteDepends(gridEx)



cleanEx()
nameEx("vignetteEngine")
### * vignetteEngine

flush(stderr()); flush(stdout())

### Name: vignetteEngine
### Title: Set or get a vignette processing engine.
### Aliases: vignetteEngine
### Keywords: utilities documentation

### ** Examples

str(vignetteEngine("Sweave"))



cleanEx()
nameEx("writePACKAGES")
### * writePACKAGES

flush(stderr()); flush(stdout())

### Name: write_PACKAGES
### Title: Generate PACKAGES files
### Aliases: write_PACKAGES
### Keywords: file utilities

### ** Examples

## Not run: 
##D write_PACKAGES("c:/myFolder/myRepository")  # on Windows
##D write_PACKAGES("/pub/RWin/bin/windows/contrib/2.9",
##D                type = "win.binary")  # on Linux
## End(Not run)


cleanEx()
nameEx("xgettext")
### * xgettext

flush(stderr()); flush(stdout())

### Name: xgettext
### Title: Extract Translatable Messages from R Files in a Package
### Aliases: xgettext xngettext xgettext2pot
### Keywords: utilities

### ** Examples
## Not run: 
##D ## in a source-directory build of R:
##D xgettext(file.path(R.home(), "src", "library", "splines"))
## End(Not run)


### * <FOOTER>
###
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
