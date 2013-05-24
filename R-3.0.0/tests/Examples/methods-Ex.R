pkgname <- "methods"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('methods')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("GenericFunctions")
### * GenericFunctions

flush(stderr()); flush(stdout())

### Name: GenericFunctions
### Title: Tools for Managing Generic Functions
### Aliases: GenericFunctions isGeneric isGroup removeGeneric getGenerics
###   dumpMethod findFunction dumpMethods removeMethods signature
###   setReplaceMethod
### Keywords: programming classes methods

### ** Examples

require(stats) # for lm

## get the function "myFun" -- throw an error if 0 or > 1 versions visible:
findFuncStrict <- function(fName) {
  allF <- findFunction(fName)
  if(length(allF) == 0)
    stop("No versions of ",fName," visible")
  else if(length(allF) > 1)
    stop(fName," is ambiguous: ", length(allF), " versions")
  else
    get(fName, allF[[1]])
}

try(findFuncStrict("myFun"))# Error: no version
lm <- function(x) x+1
try(findFuncStrict("lm"))#    Error: 2 versions
findFuncStrict("findFuncStrict")# just 1 version
rm(lm)

## Don't show: 
## because nosegfault runs standardGeneric w/o the methods package, nothing
## really gets tested.  The following check that it catches some errors
mustDie <- function(expr)
   stopifnot(is(tryCatch(expr, error=function(e)e), "error"))

mustDie(standardGeneric()) # 3 tests of requiring a single string
mustDie(standardGeneric(NULL))
mustDie(standardGeneric(""))
mustDie(standardGeneric("notAGenericFunction"))
mustDie(standardGeneric("show"))  # a generic, but not called from its body
## End Don't show

## method dumping ------------------------------------

setClass("A", representation(a="numeric"))
setMethod("plot", "A", function(x,y,...){ cat("A meth\n") })
dumpMethod("plot","A", file="")
## Not run: 
##D setMethod("plot", "A",
##D function (x, y, ...)
##D {
##D     cat("AAAAA\n")
##D }
##D )
## End(Not run)
tmp <- tempfile()
dumpMethod("plot","A", file=tmp)
## now remove, and see if we can parse the dump
stopifnot(removeMethod("plot", "A"))
source(tmp)
stopifnot(is(getMethod("plot", "A"), "MethodDefinition"))

## same with dumpMethods() :
setClass("B", contains="A")
setMethod("plot", "B", function(x,y,...){ cat("B ...\n") })
dumpMethods("plot", file=tmp)
stopifnot(removeMethod("plot", "A"),
          removeMethod("plot", "B"))
source(tmp)
stopifnot(is(getMethod("plot", "A"), "MethodDefinition"),
          is(getMethod("plot", "B"), "MethodDefinition"))



cleanEx()
nameEx("Methods")
### * Methods

flush(stderr()); flush(stdout())

### Name: Methods
### Title: General Information on Methods
### Aliases: Methods
### Keywords: programming classes methods

### ** Examples

## A class that extends a registered S3 class inherits that class' S3
## methods.

setClass("myFrame", contains = "data.frame",
    representation(timestamps = "POSIXt"))

df1 <- data.frame(x = 1:10, y = rnorm(10), z = sample(letters,10))

mydf1 <- new("myFrame", df1, timestamps = Sys.time())

## "myFrame" objects inherit "data.frame" S3 methods; e.g., for `[`

mydf1[1:2, ] # a data frame object (with extra attributes)

## a method explicitly for "myFrame" class


setMethod("[",
    signature(x = "myFrame"),
    function (x, i, j, ..., drop = TRUE)
    {
        S3Part(x) <- callNextMethod()
        x@timestamps <- c(Sys.time(), as.POSIXct(x@timestamps))
        x
    }
)



setClass("myDateTime", contains = "POSIXt")

now <- Sys.time() # class(now) is c("POSIXct", "POSIXt")
nowLt <- as.POSIXlt(now)# class(nowLt) is c("POSIXlt", "POSIXt")

mCt <- new("myDateTime", now)
mLt <- new("myDateTime", nowLt)

## S3 methods for an S4 object will be selected using S4 inheritance
## Objects mCt and mLt have different S3Class() values, but this is
## not used.
f3 <- function(x)UseMethod("f3") # an S3 generic to illustrate inheritance

f3.POSIXct <- function(x) "The POSIXct result"
f3.POSIXlt <- function(x) "The POSIXlt result"
f3.POSIXt <- function(x) "The POSIXt result"

stopifnot(identical(f3(mCt), f3.POSIXt(mCt)))
stopifnot(identical(f3(mLt), f3.POSIXt(mLt)))



## An S4 object selects S3 methods according to its S4 "inheritance"


setClass("classA", contains = "numeric",
   representation(realData = "numeric"))

Math.classA <- function(x) {(getFunction(.Generic))(x@realData)}
setMethod("Math", "classA", Math.classA)


x <- new("classA", log(1:10), realData = 1:10)

stopifnot(identical(abs(x), 1:10))

setClass("classB", contains = "classA")

y <- new("classB", x)

stopifnot(identical(abs(y), 1:10)) # (version 2.9.0 or earlier fails here)

## an S3 generic: just for demonstration purposes
f3 <- function(x, ...) UseMethod("f3")

f3.default <- function(x, ...) "Default f3"

## S3 method (only) for classA
f3.classA <- function(x, ...) "Class classA for f3"

## S3 and S4 method for numeric
f3.numeric <- function(x, ...) "Class numeric for f3"
setMethod("f3", "numeric", f3.numeric)

## The S3 method for classA and the closest inherited S3 method for classB
## are not found.

f3(x); f3(y) # both choose "numeric" method

## to obtain the natural inheritance, set identical S3 and S4 methods
setMethod("f3", "classA", f3.classA)

f3(x); f3(y) # now both choose "classA" method

## Need to define an S3 as well as S4 method to use on an S3 object
## or if called from a package without the S4 generic

MathFun <- function(x) { # a smarter "data.frame" method for Math group
  for (i in seq(length = ncol(x))[sapply(x, is.numeric)])
    x[, i] <- (getFunction(.Generic))(x[, i])
  x
}
setMethod("Math", "data.frame", MathFun)

## S4 method works for an S4 class containing data.frame,
## but not for data.frame objects (not S4 objects)

try(logIris <- log(iris)) #gets an error from the old method

## Define an S3 method with the same computation

Math.data.frame <- MathFun

logIris <- log(iris)




## Don't show: 
removeClass("classA"); removeClass("classB"); rm(x,y)
removeGeneric("f3")
removeClass("myDateTime")
removeMethod("Math", "data.frame"); rm(Math.data.frame, MathFun, logIris)
## End Don't show




cleanEx()
nameEx("NextMethod")
### * NextMethod

flush(stderr()); flush(stdout())

### Name: callNextMethod
### Title: Call an Inherited Method
### Aliases: callNextMethod
### Keywords: programming classes methods

### ** Examples


## some class definitions with simple inheritance
setClass("B0" , representation(b0 = "numeric"))

setClass("B1", representation(b1 = "character"), contains = "B0")

setClass("B2", representation(b2 = "logical"), contains = "B1")

## and a rather silly function to illustrate callNextMethod

f <- function(x) class(x)

setMethod("f", "B0", function(x) c(x@b0^2, callNextMethod()))
setMethod("f", "B1", function(x) c(paste(x@b1,":"), callNextMethod()))
setMethod("f", "B2", function(x) c(x@b2, callNextMethod()))

b1 <- new("B1", b0 = 2, b1 = "Testing")

b2 <- new("B2", b2 = FALSE, b1 = "More testing", b0 = 10)

f(b2)
stopifnot(identical(f(b2), c(b2@b2, paste(b2@b1,":"), b2@b0^2, "B2")))

f(b1)

## a sneakier method: the *changed* x is used:
setMethod("f", "B2",
          function(x) {x@b0 <- 111; c(x@b2, callNextMethod())})
f(b2)
stopifnot(identical(f(b2), c(b2@b2, paste(b2@b1,":"), 111^2, "B2")))

## Don't show: 
## a version of the example with 1 more layer of nesting

## next methods calling next methods, with arguments; using group generics
setMethod("Ops", "B2",
    function(e1, e2) callNextMethod())
setMethod("Ops", c("B0"),
    function(e1, e2) callNextMethod(e1@b0, e2))

b2 + 1 # 11

b1 == 2 # TRUE

removeClass("B2"); removeClass("B1"); removeClass("B0")

removeGeneric("f")

removeMethods("Ops")

## tests of multiple callNextMethod
setClass("m1", representation(count = "numeric"), contains = "matrix",
         prototype = prototype(count = 0))
mm1 <- new("m1", matrix(1:12, 3,4))
setMethod("[", "m1", function(x, i, j, ..., drop) callNextMethod())

setClass("m2", representation(sum = "numeric"), contains = "m1")

setMethod("Ops", c("m1", "m1"), function(e1, e2) {
    as(e1, "matrix") <- callNextMethod()
    e1@count <- max(e1@count, e2@count)+1
    e1})

mm2 <- new("m2", matrix(1:12, 3, 4), sum = sum(1:12))

stopifnot(identical(mm2[,2], 4:6))

setClass("m3", representation(rowtags = "character"),contains = "m2")

setMethod("[", signature(x="m3", i = "character", j = "missing",
                         drop = "missing"),
          function(x, i,j, ..., drop) {
              xx <- callNextMethod(x, match(i, x@rowtags),)
              x@.Data <- xx
              x@rowtags <- x@rowtags[match(i, x@rowtags)]
              x})

tm <- matrix(1:12, 4, 3)

mm3 <- new("m3", tm, rowtags = letters[1:4])

mmm <- mm3[c("b", "d")]

stopifnot(identical(mmm,
      new("m3", tm[c(2, 4),], rowtags = c("b", "d"))))

removeClass("m3")
removeClass("m2")
removeClass("m1")

removeMethods("[")
## End Don't show




cleanEx()
nameEx("RClassUtils")
### * RClassUtils

flush(stderr()); flush(stdout())

### Name: RClassUtils
### Title: Utilities for Managing Class Definitions
### Aliases: completeSubclasses newClassRepresentation
###   print.classRepresentation setExtendsMetaData setSubclassMetaData
###   subclassesMetaName extendsMetaName classPrototypeDef-class .classEnv
###   classLabel testVirtual makePrototypeFromClassDef newEmptyObject
###   completeClassDefinition getAllSuperClasses superClassDepth
###   isVirtualClass assignClassDef newBasic makeExtends
###   reconcilePropertiesAndPrototype tryNew empty.dump showClass
###   showExtends possibleExtends completeExtends classMetaName
###   methodsPackageMetaName metaNameUndo requireMethods checkAtAssignment
###   checkSlotAssignment defaultPrototype isClassDef validSlotNames
###   getDataPart setDataPart .BasicClasses .BasicVectorClasses
###   .InitBasicClasses .InitMethodsListClass .setCoerceGeneric
###   conditionalExtension-class
### Keywords: internal

### ** Examples

typeof(defaultPrototype()) #-> "S4"

## .classEnv()
meth.ns <- asNamespace("methods")
if(get4 <- !any("package:stats4" == search()))
   require("stats4")
stopifnot(TRUE
 , identical(.classEnv("data.frame"), meth.ns)
 , identical(.classEnv(class(new("data.frame"))), meth.ns)
 , identical(.classEnv(class(new("mle"))), asNamespace("stats4"))
 )
if(get4) detach("package:stats4")




cleanEx()
nameEx("RMethodUtils")
### * RMethodUtils

flush(stderr()); flush(stdout())

### Name: RMethodUtils
### Title: Method Utilities
### Aliases: asMethodDefinition standardGeneric-class
###   standardGenericWithTrace-class nonstandardGeneric-class
###   nonstandardGenericFunction-class
###   nonstandardGroupGenericFunction-class OptionalFunction-class
###   PossibleMethod-class optionalMethod-class derivedDefaultMethod-class
###   substituteFunctionArgs makeGeneric makeStandardGeneric
###   generic.skeleton defaultDumpName doPrimitiveMethod conformMethod
###   getGeneric getGroup getGroupMembers getMethodsMetaData
###   assignMethodsMetaData matchSignature findUnique MethodAddCoerce
###   .saveImage cacheMetaData cacheGenericsMetaData setPrimitiveMethods
###   missingArg balanceMethodsList sigToEnv rematchDefinition
###   unRematchDefinition addNextMethod,MethodDefinition-method
###   addNextMethod,MethodWithNext-method addNextMethod .valueClassTest
###   insertClassMethods .ShortPrimitiveSkeletons .EmptyPrimitiveSkeletons
### Keywords: internal

### ** Examples

getGroup("exp")
getGroup("==", recursive = TRUE)

getGroupMembers("Arith")
getGroupMembers("Math")
getGroupMembers("Ops") # -> its sub groups



cleanEx()
nameEx("S3Part")
### * S3Part

flush(stderr()); flush(stdout())

### Name: S3Part
### Title: S3-style Objects and S4-class Objects
### Aliases: S3Part S3Part<- S3Class S3Class<- isXS3Class slotsFromS3 S4 S3
###   coerce,ANY,S3-method coerce,oldClass,S3-method coerce,ANY,S4-method
###   S3-class
### Keywords: programming classes

### ** Examples

## two examples extending S3 class "lm", class "xlm" directly
## and "ylm" indirectly
setClass("xlm", representation(eps = "numeric"), contains = "lm")
setClass("ylm", representation(header = "character"), contains = "xlm")
## Don't show: 
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
## End Don't show
## lm.D9 is as computed in the example for stats::lm
y1 = new("ylm", lm.D9, header = "test", eps = .1)
xx = new("xlm", lm.D9, eps =.1)
y2 = new("ylm", xx, header = "test")
stopifnot(inherits(y2, "lm"))
stopifnot(identical(y1, y2))
stopifnot(identical(S3Part(y1, strict = TRUE), lm.D9))

## note the these classes can insert an S3 subclass of "lm" as the S3 part:
myData <- data.frame(time = 1:10, y = (1:10)^.5)
myLm <- lm(cbind(y, y^3)  ~ time, myData) # S3 class: c("mlm", "lm")
ym1 = new("ylm", myLm, header = "Example", eps = 0.)

##similar classes to "xlm" and "ylm", but extending S3 class c("mlm", "lm")
setClass("xmm", representation(eps = "numeric"), contains = "mlm")
setClass("ymm", representation(header="character"), contains = "xmm")

ym2 <- new("ymm", myLm, header = "Example2", eps = .001)

# but for class "ymm", an S3 part of class "lm" is an error:
try(new("ymm", lm.D9, header = "Example2", eps = .001))

setClass("dataFrameD", representation(date = "Date"),
         contains = "data.frame")
myDD <- new("dataFrameD", myData, date = Sys.Date())

## S3Part() applied to classes with a data part (.Data slot)

setClass("NumX", contains="numeric", representation(id="character"))
nn = new("NumX", 1:10, id="test")
stopifnot(identical(1:10, S3Part(nn, strict = TRUE)))

m1 = cbind(group, weight)
setClass("MatX", contains = "matrix", representation(date = "Date"))
mx1 = new("MatX", m1, date = Sys.Date())
stopifnot(identical(m1, S3Part(mx1, strict = TRUE)))

## Don't show: 

for(cl in c("ylm", "xlm", "ymm", "xmm", "dataFrameD", "NumX", "MatX"))
    removeClass(cl)

## End Don't show



cleanEx()
nameEx("S4groupGeneric")
### * S4groupGeneric

flush(stderr()); flush(stdout())

### Name: S4groupGeneric
### Title: S4 Group Generic Functions
### Aliases: S4groupGeneric GroupGenericFunctions Math Ops Summary Arith
###   Logic Compare Complex Math2
### Keywords: methods

### ** Examples

setClass("testComplex", representation(zz = "complex"))
## method for whole group "Complex"
setMethod("Complex", "testComplex",
          function(z) c("groupMethod", callGeneric(z@zz)))
## exception for Arg() :
setMethod("Arg", "testComplex",
          function(z) c("ArgMethod", Arg(z@zz)))
z1 <- 1+2i
z2 <- new("testComplex", zz = z1)
stopifnot(identical(Mod(z2), c("groupMethod", Mod(z1))))
stopifnot(identical(Arg(z2), c("ArgMethod", Arg(z1))))
## Don't show: 
removeMethods("Complex")
removeMethods("Arg")
## End Don't show


cleanEx()
nameEx("StructureClasses")
### * StructureClasses

flush(stderr()); flush(stdout())

### Name: StructureClasses
### Title: Classes Corresponding to Basic Structures
### Aliases: structure-class matrix-class array-class ts-class
###   Math,structure-method Ops,structure,vector-method
###   Ops,structure,structure-method Ops,structure,array-method
###   Ops,vector,structure-method Ops,array,structure-method
###   Ops,array,array-method initialize,array-method
###   initialize,matrix-method initialize,ts-method initialize,mts-method
###   show,ts-method
### Keywords: classes

### ** Examples

showClass("structure")

## explore a bit :
showClass("ts")
(ts0 <- new("ts"))
str(ts0)

showMethods("Ops") # six methods from these classes, but maybe many more



cleanEx()
nameEx("as")
### * as

flush(stderr()); flush(stdout())

### Name: as
### Title: Force an Object to Belong to a Class
### Aliases: as as<- coerce coerce<- setAs coerce-methods
###   coerce,ANY,array-method coerce,ANY,call-method
###   coerce,ANY,character-method coerce,ANY,complex-method
###   coerce,ANY,environment-method coerce,ANY,expression-method
###   coerce,ANY,function-method coerce,ANY,integer-method
###   coerce,ANY,list-method coerce,ANY,logical-method
###   coerce,ANY,matrix-method coerce,ANY,name-method
###   coerce,ANY,numeric-method coerce,ANY,single-method
###   coerce,ANY,ts-method coerce,ANY,vector-method coerce,ANY,NULL-method
### Keywords: programming classes methods

### ** Examples

## using the definition of class "track" from setClass

## Don't show: 
setClass("track",
         representation(x="numeric", y="numeric"))
setClass("trackCurve",
         representation("track", smooth = "numeric"))
## End Don't show

setAs("track", "numeric", function(from) from@y)

t1 <- new("track", x=1:20, y=(1:20)^2)

as(t1, "numeric")

## The next example shows:
##  1. A virtual class to define setAs for several classes at once.
##  2. as() using inherited information

setClass("ca", representation(a = "character", id = "numeric"))

setClass("cb", representation(b = "character", id = "numeric"))

setClass("id")
setIs("ca", "id")
setIs("cb", "id")


setAs("id", "numeric", function(from) from@id)

CA <- new("ca", a = "A", id = 1)
CB <- new("cb", b = "B", id = 2)

setAs("cb", "ca", function(from, to )new(to, a=from@b, id = from@id))

as(CB, "numeric")

## Don't show: 
## should generate an error (should have been a function of one argument)
try(setAs("track", "numeric", function(x, y,z)x@y))
## End Don't show



cleanEx()
nameEx("callGeneric")
### * callGeneric

flush(stderr()); flush(stdout())

### Name: callGeneric
### Title: Call the Current Generic Function from a Method
### Aliases: callGeneric
### Keywords: programming classes methods

### ** Examples

## the method for group generic function Ops
## for signature( e1="structure", e2="vector")
function (e1, e2)
{
    value <- callGeneric(e1@.Data, e2)
    if (length(value) == length(e1)) {
        e1@.Data <- value
        e1
    }
    else value
}

## For more examples
## Not run: 
##D showMethods("Ops", includeDefs = TRUE)
## End(Not run)




cleanEx()
nameEx("canCoerce")
### * canCoerce

flush(stderr()); flush(stdout())

### Name: canCoerce
### Title: Can an Object be Coerced to a Certain S4 Class?
### Aliases: canCoerce
### Keywords: classes methods

### ** Examples

m <- matrix(pi, 2,3)
canCoerce(m, "numeric") # TRUE
canCoerce(m, "array")   # TRUE



cleanEx()
nameEx("cbind2")
### * cbind2

flush(stderr()); flush(stdout())

### Name: cbind2
### Title: Combine two Objects by Columns or Rows
### Aliases: cbind2 rbind2 cbind2-methods cbind2,ANY,ANY-method
###   cbind2,ANY,missing-method rbind2-methods rbind2,ANY,ANY-method
###   rbind2,ANY,missing-method
### Keywords: array manip

### ** Examples

cbind2(1:3, 4)
m <- matrix(3:8, 2,3, dimnames=list(c("a","b"), LETTERS[1:3]))
cbind2(1:2, m) # keeps dimnames from m

### Note: Use the following activation if you want cbind() to work
### ----  on S4 objects -- be careful otherwise!

methods:::bind_activation(on = TRUE)
trace("cbind2")
cbind(a=1:3)# no call to cbind2()
cbind(a=1:3, four=4, 7:9)# calling cbind2() twice
untrace("cbind2")

## Don't show: 
cbind(m,m+1,m+2)
cbind(m,a=1, ch=c("D","E"))
cbind(1,a=1:3, m) # ok with a warning
cbind(A=1, B=3, m, C=4)
## End Don't show

cbind(m, a=1, b=3)

## turn off the `special cbind()' :
methods:::bind_activation(FALSE)



cleanEx()
nameEx("className")
### * className

flush(stderr()); flush(stdout())

### Name: className
### Title: Class names including the corresponding package
### Aliases: className multipleClasses className-class
### Keywords: classes programming

### ** Examples

## Not run: 
##D className("vector") # will be found, from package "methods"
##D className("vector", "magic") # OK, even though the class doesn't exist
##D 
##D 
##D className("An unknown class") # Will cause an error
## End(Not run)



cleanEx()
nameEx("classesToAM")
### * classesToAM

flush(stderr()); flush(stdout())

### Name: classesToAM
### Title: Compute an Adjacency Matrix for Superclasses of Class
###   Definitions
### Aliases: classesToAM
### Keywords: classes programming

### ** Examples


## the super- and subclasses of "standardGeneric"
## and "derivedDefaultMethod"
am <- classesToAM(list(class(show), class(getMethod(show))), TRUE)
am

## Not run: 
##D ## the following function depends on the Bioconductor package Rgraphviz
##D plotInheritance <- function(classes, subclasses = FALSE, ...) {
##D     if(!require("Rgraphviz", quietly=TRUE))
##D       stop("Only implemented if Rgraphviz is available")
##D     mm <- classesToAM(classes, subclasses)
##D     classes <- rownames(mm); rownames(mm) <- colnames(mm)
##D     graph <-  new("graphAM", mm, "directed", ...)
##D     plot(graph)
##D     cat("Key:\n", paste(abbreviate(classes), " = ", classes, ", ",
##D         sep = ""),  sep = "", fill = TRUE)
##D     invisible(graph)
##D }
##D 
##D ## The plot of the class inheritance of the package "graph"
##D require(graph)
##D plotInheritance(getClasses("package:graph"))
##D 
## End(Not run)



cleanEx()
nameEx("dotsMethods")
### * dotsMethods

flush(stderr()); flush(stdout())

### Name: dotsMethods
### Title: The Use of '...' in Method Signatures
### Aliases: dotsMethods
### Keywords: programming classes methods

### ** Examples

cc <- function(...)c(...)

setGeneric("cc")

setMethod("cc", "character", function(...)paste(...))

setClassUnion("Number", c("numeric", "complex"))

setMethod("cc", "Number", function(...) sum(...))

setClass("cdate", contains = "character", representation(date = "Date"))

setClass("vdate", contains = "vector", representation(date = "Date"))

cd1 <- new("cdate", "abcdef", date = Sys.Date())

cd2 <- new("vdate", "abcdef", date = Sys.Date())

stopifnot(identical(cc(letters, character(), cd1),
           paste(letters, character(), cd1))) # the "character" method

stopifnot(identical(cc(letters, character(), cd2),
                    c(letters, character(), cd2)))
# the default, because "vdate" doesn't extend "character"

stopifnot(identical(cc(1:10, 1+1i), sum(1:10, 1+1i))) # the "Number" method

stopifnot(identical(cc(1:10, 1+1i, TRUE), c(1:10, 1+1i, TRUE))) # the default

stopifnot(identical(cc(), c())) # no arguments implies the default method

setGeneric("numMax", function(...)standardGeneric("numMax"))

setMethod("numMax", "numeric", function(...)max(...))
# won't work for complex data
setMethod("numMax", "Number", function(...) paste(...))
# should not be selected w/o complex args

stopifnot(identical(numMax(1:10, pi, 1+1i), paste(1:10, pi, 1+1i)))
stopifnot(identical(numMax(1:10, pi, 1), max(1:10, pi, 1)))

try(numMax(1:10, pi, TRUE)) # should be an error:  no default method

## A generic version of paste(), dispatching on the "..." argument:
setGeneric("paste", signature = "...")

setMethod("paste", "Number", function(..., sep, collapse) c(...))

stopifnot(identical(paste(1:10, pi, 1), c(1:10, pi, 1)))

## Don't show: 
for(gen in c("numMax", "cc", "paste")) removeGeneric(gen)
for(cl in c("Number", "vdate", "cdate")) removeClass(cl)
## End Don't show



cleanEx()
nameEx("evalSource")
### * evalSource

flush(stderr()); flush(stdout())

### Name: evalSource
### Title: Use Function Definitions from a Source File without Reinstalling
###   a Package
### Aliases: evalSource insertSource sourceEnvironment-class
### Keywords: programming methods

### ** Examples

## Not run: 
##D ## Suppose package P0 has a source file "all.R"
##D ## First, evaluate the source, and from it
##D ## insert the revised version of methods for summary()
##D   env <- insertSource("./P0/R/all.R", package = "P0",
##D      methods = "summary")
##D ## now test one of the methods, tracing  the version from the source
##D   trace("summary", signature = "myMat", browser, edit = env)
##D ## After testing, remove the browser() call but keep the source
##D   trace("summary", signature = "myMat", edit = env)
##D ## Now insert all the (other) revised functions and methods
##D ## without re-evaluating the source file.
##D ## The package name is included in the object env.
##D   insertSource(env)
## End(Not run)



cleanEx()
nameEx("findMethods")
### * findMethods

flush(stderr()); flush(stdout())

### Name: findMethods
### Title: Description of the Methods Defined for a Generic Function
### Aliases: findMethods findMethodSignatures hasMethods getMethods
###   listOfMethods-class
### Keywords: programming classes methods

### ** Examples

mm <-  findMethods("Ops")
findMethodSignatures(methods = mm)



cleanEx()
nameEx("getClass")
### * getClass

flush(stderr()); flush(stdout())

### Name: getClass
### Title: Get Class Definition
### Aliases: getClass getClassDef
### Keywords: programming classes

### ** Examples

getClass("numeric") ## a built in class

cld <- getClass("thisIsAnUndefinedClass", .Force = TRUE)
cld ## a NULL prototype
## If you are really curious:
utils::str(cld)
## Whereas these generate errors:
try(getClass("thisIsAnUndefinedClass"))
try(getClassDef("thisIsAnUndefinedClass"))



cleanEx()
nameEx("getMethod")
### * getMethod

flush(stderr()); flush(stdout())

### Name: getMethod
### Title: Get or Test for the Definition of a Method
### Aliases: getMethod findMethod existsMethod selectMethod hasMethod
### Keywords: programming classes methods

### ** Examples

setGeneric("testFun", function(x)standardGeneric("testFun"))
setMethod("testFun", "numeric", function(x)x+1)
hasMethod("testFun", "numeric")
## Not run: [1] TRUE
hasMethod("testFun", "integer") #inherited
## Not run: [1] TRUE
existsMethod("testFun", "integer")
## Not run: [1] FALSE
hasMethod("testFun") # default method
## Not run: [1] FALSE
hasMethod("testFun", "ANY")
## Not run: [1] FALSE
## Don't show: 
stopifnot(isGeneric("testFun"),
          hasMethod("testFun", "numeric"),
          hasMethod("testFun", "integer"),
          !existsMethod("testFun", "integer"),
          !hasMethod("testFun"),
          !hasMethod("testFun", "ANY") )
removeGeneric("testFun")
## End Don't show



cleanEx()
nameEx("getPackageName")
### * getPackageName

flush(stderr()); flush(stdout())

### Name: getPackageName
### Title: The Name associated with a Given Package
### Aliases: getPackageName setPackageName packageSlot packageSlot<-
### Keywords: programming

### ** Examples

## all the following usually return "base"
getPackageName(length(search()))
getPackageName(baseenv())
getPackageName(asNamespace("base"))
getPackageName("package:base")




cleanEx()
nameEx("hasArg")
### * hasArg

flush(stderr()); flush(stdout())

### Name: hasArg
### Title: Look for an Argument in the Call
### Aliases: hasArg
### Keywords: programming

### ** Examples

ftest <- function(x1, ...) c(hasArg(x1), hasArg("y2"))

ftest(1) ## c(TRUE, FALSE)
ftest(1, 2)  ## c(TRUE, FALSE)
ftest(y2 = 2)   ## c(FALSE, TRUE)
ftest(y = 2)    ## c(FALSE, FALSE) (no partial matching)
ftest(y2 = 2, x = 1)  ## c(TRUE, TRUE) partial match x1



cleanEx()
nameEx("implicitGeneric")
### * implicitGeneric

flush(stderr()); flush(stdout())

### Name: implicitGeneric
### Title: Manage Implicit Versions of Generic Functions
### Aliases: implicitGeneric setGenericImplicit prohibitGeneric
###   registerImplicitGenerics 'implicit generic'
### Keywords: programming methods

### ** Examples


### How we would make the function with() into a generic:

## Since the second argument, 'expr' is used literally, we want
## with() to only have "data" in the signature.

## Note that 'methods'-internal code now has already extended  with()
## to do the equivalent of the following
## Not run: 
##D setGeneric("with", signature = "data")
##D ## Now we could predefine methods for "with" if we wanted to.
##D 
##D ## When ready, we store the generic as implicit, and restore the original
##D setGenericImplicit("with")
##D 
##D ## (This example would only work if we "owned" function with(),
##D ##  but it is in base.)
## End(Not run)

implicitGeneric("with")



cleanEx()
nameEx("inheritedSlotNames")
### * inheritedSlotNames

flush(stderr()); flush(stdout())

### Name: inheritedSlotNames
### Title: Names of Slots Inherited From a Super Class
### Aliases: inheritedSlotNames
### Keywords: classes methods

### ** Examples

.srch <- search()
library(stats4)
inheritedSlotNames("mle")

## Not run: 
##D if(require("Matrix")) {
##D   print( inheritedSlotNames("Matrix") ) # NULL
##D   ## whereas
##D   print( inheritedSlotNames("sparseMatrix") ) # --> Dim & Dimnames
##D   ##  i.e. inherited from "Matrix" class
##D 
##D   print( cl <- getClass("dgCMatrix") ) # six slots, etc
##D 
##D   print( inheritedSlotNames(cl) ) # *all* six slots are inherited
##D }
##D 
##D 
##D ## detach package we've attached above:
##D for(n in rev(which(is.na(match(search(), .srch)))))
##D    try( detach(pos = n) )
## End(Not run)



cleanEx()
nameEx("is")
### * is

flush(stderr()); flush(stdout())

### Name: is
### Title: Is an Object from a Class?
### Aliases: is extends setIs
### Keywords: programming classes methods

### ** Examples

## Don't show: 
## A simple class with two slots
setClass("track",
         representation(x="numeric", y="numeric"))
## A class extending the previous, adding one more slot
## End Don't show
## Two examples of setIs() with coerce= and replace= arguments
## The first one works fairly well, because neither class has many
## inherited methods do be disturbed by the new inheritance

## The second example does NOT work well, because the new superclass,
## "factor", causes methods to be inherited that should not be.

## First example:
## a class definition (see setClass for class "track")
setClass("trackCurve", contains = "track",
         representation( smooth = "numeric"))
## A class similar to "trackCurve", but with different structure
## allowing matrices for the "y" and "smooth" slots
setClass("trackMultiCurve",
         representation(x="numeric", y="matrix", smooth="matrix"),
         prototype = structure(list(), x=numeric(), y=matrix(0,0,0),

                               smooth= matrix(0,0,0)))
## Automatically convert an object from class "trackCurve" into
## "trackMultiCurve", by making the y, smooth slots into 1-column matrices
setIs("trackCurve",
      "trackMultiCurve",
      coerce = function(obj) {
        new("trackMultiCurve",
            x = obj@x,
            y = as.matrix(obj@y),
            smooth = as.matrix(obj@smooth))
      },
      replace = function(obj, value) {
        obj@y <- as.matrix(value@y)
        obj@x <- value@x
        obj@smooth <- as.matrix(value@smooth)
        obj})


## Don't show: 
removeClass("trackMultiCurve")
removeClass("trackCurve")
removeClass("track")
## End Don't show

## Second Example:
## A class that adds a slot to "character"
setClass("stringsDated", contains = "character",
         representation(stamp="POSIXt"))

## Convert automatically to a factor by explicit coerce
setIs("stringsDated", "factor",
      coerce = function(from) factor(from@.Data),
      replace= function(from, value) {
                  from@.Data <- as.character(value); from })
## Don't show: 
set.seed(750)
## End Don't show
ll <- sample(letters, 10, replace = TRUE)
ld <- new("stringsDated", ll, stamp = Sys.time())

levels(as(ld, "factor"))
levels(ld) # will be NULL--see comment in section on inheritance above.

## In contrast, a class that simply extends "factor"
## has no such ambiguities
setClass("factorDated", contains = "factor",
         representation(stamp="POSIXt"))
fd <- new("factorDated", factor(ll), stamp = Sys.time())
identical(levels(fd), levels(as(fd, "factor")))



cleanEx()
nameEx("isSealedMethod")
### * isSealedMethod

flush(stderr()); flush(stdout())

### Name: isSealedMethod
### Title: Check for a Sealed Method or Class
### Aliases: isSealedMethod isSealedClass
### Keywords: programming classes classes methods

### ** Examples

## these are both TRUE
isSealedMethod("+", c("numeric", "character"))
isSealedClass("matrix")

setClass("track",
            representation(x="numeric", y="numeric"))
## but this is FALSE
isSealedClass("track")
## and so is this
isSealedClass("A Name for an undefined Class")
## and so are these, because only one of the two arguments is basic
isSealedMethod("+", c("track", "numeric"))
isSealedMethod("+", c("numeric", "track"))

## Don't show: 
removeClass("track")
## End Don't show



cleanEx()
nameEx("localRefClass")
### * localRefClass

flush(stderr()); flush(stdout())

### Name: LocalReferenceClasses
### Title: Localized Objects based on Reference Classes
### Aliases: LocalReferenceClasses localRefClass-class
###   $<-,localRefClass-method
### Keywords: programming classes

### ** Examples

## class "myIter" has a BigData field for the real (big) data
## and a "twiddle" field for some parameters that it twiddles
## ( for some reason)

myIter <- setRefClass("myIter", contains = "localRefClass",
  fields = list(BigData = "numeric", twiddle = "numeric"))

tw <- rnorm(3)
x1 <- myIter(BigData = rnorm(1000), twiddle = tw) # OK, not REALLY big

twiddler <- function(x, n) {
  x$ensureLocal() # see the Details.  Not really needed in this example
  for(i in seq(length = n)) {
      x$twiddle <- x$twiddle + rnorm(length(x$twiddle))
      ## then do something ....
      ## Snooping in gdb, etc will show that x$BigData is not copied
  }
  return(x)
}

x2 <- twiddler(x1, 10)

stopifnot(identical(x1$twiddle, tw), !identical(x1$twiddle, x2$twiddle))




cleanEx()
nameEx("method.skeleton")
### * method.skeleton

flush(stderr()); flush(stdout())

### Name: method.skeleton
### Title: Create a Skeleton File for a New Method
### Aliases: method.skeleton
### Keywords: programming methods

### ** Examples

## Don't show: 
oWD <- setwd(tempdir())
## End Don't show
setClass("track", representation(x ="numeric", y="numeric"))
method.skeleton("show", "track")            ## writes show_track.R
method.skeleton("Ops", c("track", "track")) ## writes "Ops_track_track.R"

## write multiple method skeletons to one file
con <- file("./Math_track.R", "w")
method.skeleton("Math", "track", con)
method.skeleton("exp", "track", con)
method.skeleton("log", "track", con)
close(con)
## Don't show: 
setwd(oWD)
## End Don't show



cleanEx()
nameEx("new")
### * new

flush(stderr()); flush(stdout())

### Name: new
### Title: Generate an Object from a Class
### Aliases: new initialize
### Keywords: programming classes

### ** Examples

## using the definition of class "track" from setClass

## Don't show: 
setClass("track",
         representation(x="numeric", y="numeric"))
setClass("trackCurve",
         representation("track", smooth = "numeric"))

ydata <- stats::rnorm(10); ysmooth <- 1:10
## End Don't show

## a new object with two slots specified
t1 <- new("track", x = seq_along(ydata), y = ydata)

# a new object including an object from a superclass, plus a slot
t2 <- new("trackCurve", t1, smooth = ysmooth)

### define a method for initialize, to ensure that new objects have
### equal-length x and y slots.

setMethod("initialize",
          "track",
          function(.Object, x = numeric(0), y = numeric(0)) {
            if(nargs() > 1) {
              if(length(x) != length(y))
                stop("specified x and y of different lengths")
              .Object@x <- x
              .Object@y <- y
            }
            .Object
          })

### the next example will cause an error (x will be numeric(0)),
### because we didn't build in defaults for x,
### although we could with a more elaborate method for initialize

try(new("track", y = sort(stats::rnorm(10))))

## a better way to implement the previous initialize method.
## Why?  By using callNextMethod to call the default initialize method
## we don't inhibit classes that extend "track" from using the general
## form of the new() function.  In the previous version, they could only
## use x and y as arguments to new, unless they wrote their own
## initialize method.

setMethod("initialize", "track", function(.Object, ...) {
    .Object <- callNextMethod()
    if(length(.Object@x) != length(.Object@y))
     stop("specified x and y of different lengths")
    .Object
  })




cleanEx()
nameEx("nonStructure-class")
### * nonStructure-class

flush(stderr()); flush(stdout())

### Name: nonStructure-class
### Title: A non-structure S4 Class for basic types
### Aliases: nonStructure-class Math,nonStructure-method
###   Math2,nonStructure-method Ops,vector,nonStructure-method
###   Ops,nonStructure,vector-method Ops,nonStructure,nonStructure-method
### Keywords: classes

### ** Examples

setClass("NumericNotStructure", contains = c("numeric","nonStructure"))
xx <- new("NumericNotStructure", 1:10)
xx + 1 # vector
log(xx) # vector
sample(xx) # vector
## Don't show: 
removeClass("NumericNotStructure")
## End Don't show



cleanEx()
nameEx("promptClass")
### * promptClass

flush(stderr()); flush(stdout())

### Name: promptClass
### Title: Generate a Shell for Documentation of a Formal Class
### Aliases: promptClass
### Keywords: programming classes

### ** Examples

## Don't show: 
## from setClass
## A simple class with two slots
setClass("track",
         representation(x="numeric", y="numeric"))
## A class extending the previous, adding one more slot
setClass("trackCurve",
         representation("track", smooth = "numeric"))
## A class similar to "trackCurve", but with different structure
## allowing matrices for the "y" and "smooth" slots
setClass("trackMultiCurve",
         representation(x="numeric", y="matrix", smooth="matrix"),
         prototype = list(x=numeric(), y=matrix(0,0,0), smooth= matrix(0,0,0)))

setIs("trackMultiCurve", "trackCurve",
  test = function(obj) {ncol(slot(obj, "y")) == 1},
  coerce = function(obj) { new("trackCurve", x = slot(obj, "x"),
        y = as.numeric(slot(obj,"y")), smooth = as.numeric(slot(obj, "smooth")))})

## from setMethod
require(graphics)

setMethod("plot", "track",
 function(x, y, ...) plot(slot(x, "y"), y,  ...)
)
setMethod("plot", c("trackCurve", "missing"),
function(x, y, ...) {
  plot(as(x, "track"))
  if(length(slot(x, "smooth") > 0))
    lines(slot(x, "x"), slot(x, "smooth"))
  }
)

promptClass("trackMultiCurve", stdout())

promptClass("track", stdout())
## End Don't show
## Not run: 
##D > promptClass("track")
##D A shell of class documentation has been written to the
##D file "track-class.Rd".
## End(Not run)
## Don't show: 
removeMethods("plot")
## End Don't show


cleanEx()
nameEx("refClass")
### * refClass

flush(stderr()); flush(stdout())

### Name: ReferenceClasses
### Title: Objects With Fields Treated by Reference (OOP-style)
### Aliases: ReferenceClasses setRefClass getRefClass initFieldArgs
###   initRefFields activeBindingFunction-class
###   defaultBindingFunction-class uninitializedField-class
###   refClassRepresentation-class refObjectGenerator-class
###   refGeneratorSlot-class refClass-class refObject-class
###   refMethodDef-class refMethodDefWithTrace-class SuperClassMethod-class
###   show,envRefClass-method show,refMethodDef-method
###   show,refClassRepresentation-method
### Keywords: programming classes

### ** Examples

## a simple editor for matrix objects.  Method  $edit() changes some
## range of values; method $undo() undoes the last edit.
mEdit <- setRefClass("mEdit",
      fields = list( data = "matrix",
        edits = "list"),
      methods = list(
     edit = function(i, j, value) {
       ## the following string documents the edit method
       'Replaces the range [i, j] of the
        object by value.
        '
         backup <-
             list(i, j, data[i,j])
         data[i,j] <<- value
         edits <<- c(edits, list(backup))
         invisible(value)
     },
     undo = function() {
       'Undoes the last edit() operation
        and update the edits field accordingly.
        '
         prev <- edits
         if(length(prev)) prev <- prev[[length(prev)]]
         else stop("No more edits to undo")
         edit(prev[[1]], prev[[2]], prev[[3]])
         ## trim the edits list
         length(edits) <<- length(edits) - 2
         invisible(prev)
     },
     show = function() {
       'Method for automatically printing matrix editors'
       cat("Reference matrix editor object of class",
          classLabel(class(.self)), "\n")
       cat("Data: \n")
       methods::show(data)
       cat("Undo list is of length", length(edits), "\n")
     }
     ))

xMat <- matrix(1:12,4,3)
xx <- mEdit(data = xMat)
xx$edit(2, 2, 0)
xx
xx$undo()
mEdit$help("undo")
stopifnot(all.equal(xx$data, xMat))

utils::str(xx) # show fields and names of non-trivial methods

## add a method to save the object
mEdit$methods(
     save = function(file) {
       'Save the current object on the file
        in R external object format.
       '
         base::save(.self, file = file)
     }
)

tf <- tempfile()
xx$save(tf)
## Don't show: 
load(tf)
unlink(tf)
stopifnot(identical(xx$data, .self$data))
## End Don't show

## Not run: 
##D ## Inheriting a reference class:  a matrix viewer
##D mv <- setRefClass("matrixViewer",
##D     fields = c("viewerDevice", "viewerFile"),
##D     contains = "mEdit",
##D     methods = list( view = function() {
##D         dd <- dev.cur(); dev.set(viewerDevice)
##D         devAskNewPage(FALSE)
##D         matplot(data, main = paste("After",length(edits),"edits"))
##D         dev.set(dd)},
##D         edit = # invoke previous method, then replot
##D           function(i, j, value) {
##D             callSuper(i, j, value)
##D             view()
##D           }))
##D 
##D ## initialize and finalize methods
##D mv$methods( initialize =
##D   function(file = "./matrixView.pdf", ...) {
##D     viewerFile <<- file
##D     pdf(viewerFile)
##D     viewerDevice <<- dev.cur()
##D     dev.set(dev.prev())
##D     callSuper(...)
##D   },
##D   finalize = function() {
##D     dev.off(viewerDevice)
##D   })
##D 
##D ## debugging an object: call browser() in method $edit()
##D xx$trace(edit, browser)
##D 
##D ## debugging all objects from class mEdit in method $undo()
##D mEdit$trace(undo, browser)
## End(Not run)
## Don't show: 
removeClass("mEdit")
resetGeneric("$")
resetGeneric("initialize")
## End Don't show 



cleanEx()
nameEx("representation")
### * representation

flush(stderr()); flush(stdout())

### Name: representation
### Title: Construct a Representation or a Prototype for a Class Definition
### Aliases: representation prototype
### Keywords: programming classes

### ** Examples

## representation for a new class with a directly define slot "smooth"
## which should be a "numeric" object, and extending class "track"
representation("track", smooth ="numeric")
## Don't show: 
prev <- getClassDef("class3")
setClass("class1", representation(a="numeric", b = "character"))
setClass("class2", representation(a2 = "numeric", b = "numeric"))
try(setClass("class3", representation("class1", "class2")))
{if(is.null(prev))
  stopifnot(!isClass("class3"))
else
  stopifnot(identical(getClassDef("class3"), prev))}
## End Don't show

setClass("Character",representation("character"))
setClass("TypedCharacter",representation("Character",type="character"),
          prototype(character(0),type="plain"))
ttt <- new("TypedCharacter", "foo", type = "character")
## Don't show: 
stopifnot(identical(as(ttt, "character"), "foo"))
## End Don't show

setClass("num1", representation(comment = "character"),
         contains = "numeric",
         prototype = prototype(pi, comment = "Start with pi"))

## Don't show: 
stopifnot(identical(new("num1"), new("num1", pi, comment = "Start with pi")))
for(cl in c("num1", "TypedCharacter", "Character", "class2", "class1"))
    removeClass(cl)
## End Don't show




cleanEx()
nameEx("selectSuperClasses")
### * selectSuperClasses

flush(stderr()); flush(stdout())

### Name: selectSuperClasses
### Title: Super Classes (of Specific Kinds) of a Class
### Aliases: selectSuperClasses .selectSuperClasses
### Keywords: programming classes

### ** Examples

setClass("Root")
setClass("Base", contains = "Root", representation(length = "integer"))
setClass("A", contains = "Base", representation(x = "numeric"))
setClass("B", contains = "Base", representation(y = "character"))
setClass("C", contains = c("A", "B"))

extends("C")   #-->  "C"  "A" "B"  "Base" "Root"
selectSuperClasses("C") # "A" "B"
selectSuperClasses("C", direct=FALSE) # "A" "B"  "Base"  "Root"
selectSuperClasses("C", dropVirt = TRUE, direct=FALSE)# ditto w/o "Root"



cleanEx()
nameEx("setClass")
### * setClass

flush(stderr()); flush(stdout())

### Name: setClass
### Title: Create a Class Definition
### Aliases: setClass classGeneratorFunction-class
### Keywords: programming classes methods

### ** Examples

## Don't show: 
 if(isClass("trackMultiCurve")) removeClass("trackMultiCurve")
 if(isClass("trackCurve"))      removeClass("trackCurve")
 if(isClass("track"))           removeClass("track")
## End Don't show
## A simple class with two slots
track <- setClass("track",
         slots = c(x="numeric", y="numeric"))
## an object from the class
t1 <- track(x = 1:10, y = 1:10 + rnorm(10))

## A class extending the previous, adding one more slot
trackCurve <- setClass("trackCurve",
    slots = c(smooth = "numeric"),
    contains = "track")

## an object containing a superclass object
t1s <- trackCurve(t1, smooth = 1:10)

## A class similar to "trackCurve", but with different structure
## allowing matrices for the "y" and "smooth" slots
setClass("trackMultiCurve",
         slots = c(x="numeric", y="matrix", smooth="matrix"),
         prototype = list(x=numeric(), y=matrix(0,0,0),
                          smooth= matrix(0,0,0)))
## See ?setIs for further examples using these classes

## A class that extends the built-in data type "numeric"

numWithId <- setClass("numWithId", slots = c(id = "character"),
         contains = "numeric")

numWithId(1:3, id = "An Example")

## inherit from reference object of type "environment"
stampedEnv <-setClass("stampedEnv", contains = "environment",
      slots = c(update = "POSIXct"))
setMethod("[[<-", c("stampedEnv", "character", "missing"),
   function(x, i, j, ..., value) {
       ev <- as(x, "environment")
       ev[[i]] <- value  #update the object in the environment
       x@update <- Sys.time() # and the update time
       x})


e1 <- stampedEnv(update = Sys.time())

e1[["noise"]] <- rnorm(10)

## Don't show: 
tMC <- new("trackMultiCurve")
is.matrix(slot(tMC, "y"))
is.matrix(slot(tMC, "smooth"))
setClass("myMatrix", "matrix", prototype = matrix(0,0,0))
nrow(new("myMatrix")) # 0
nrow(new("matrix")) # 1
## simple test of prototype data
xxx <- stats::rnorm(3)
setClass("xNum", slots = c(x = "numeric"), prototype = list(x = xxx))
stopifnot(identical(new("xNum")@x, xxx))

removeClass("xNum")
removeClass("myMatrix")

## The following should not be needed.  But make check removes all files
## between example files, in a crude way that does not cause the class
## information to be reset.  There seems no way to detect this, so we
## have to remove classes ourselves

removeClass("trackMultiCurve")
removeClass("trackCurve")
removeClass("track")
## End Don't show



cleanEx()
nameEx("setClassUnion")
### * setClassUnion

flush(stderr()); flush(stdout())

### Name: setClassUnion
### Title: Classes Defined as the Union of Other Classes
### Aliases: setClassUnion isClassUnion ClassUnionRepresentation-class
### Keywords: programming classes

### ** Examples

## a class for either numeric or logical data
setClassUnion("maybeNumber", c("numeric", "logical"))

## use the union as the data part of another class
setClass("withId", representation("maybeNumber", id = "character"))

w1 <- new("withId", 1:10, id = "test 1")
w2 <- new("withId", sqrt(w1)%%1 < .01, id = "Perfect squares")

## add class "complex" to the union "maybeNumber"
setIs("complex", "maybeNumber")

w3 <- new("withId", complex(real = 1:10, imaginary = sqrt(1:10)))

## a class union containing the existing class  union "OptionalFunction"
setClassUnion("maybeCode",
    c("expression", "language", "OptionalFunction"))

is(quote(sqrt(1:10)), "maybeCode")  ## TRUE
## Don't show: 
## The following test is less trivial than it looks.
## It depends on the assignment of the data part NOT performing a
## strict coerce to "numeric" on the way to satisfying
## is(ttt, "maybeNumber").
stopifnot(identical(w1@.Data, 1:10))
removeClass("withId")
removeClass("maybeNumber")
## End Don't show




cleanEx()
nameEx("setGeneric")
### * setGeneric

flush(stderr()); flush(stdout())

### Name: setGeneric
### Title: Define a New Generic Function
### Aliases: setGeneric setGroupGeneric
### Keywords: programming methods

### ** Examples

## Don't show: 
setClass("track", representation(x="numeric", y="numeric"))
## End Don't show

## create a new generic function, with a default method
setGeneric("props", function(object) attributes(object))

## A new generic function with no default method
setGeneric("increment",
  function(object, step, ...)
    standardGeneric("increment")
)


###   A non-standard generic function.  It insists that the methods
###   return a non-empty character vector (a stronger requirement than
###    valueClass = "character" in the call to setGeneric)

setGeneric("authorNames",
    function(text) {
      value <- standardGeneric("authorNames")
      if(!(is(value, "character") && any(nchar(value)>0)))
        stop("authorNames methods must return non-empty strings")
      value
      })

## Don't show: 
setMethod("authorNames", "character", function(text)text)

tryIt <- function(expr) tryCatch(expr, error = function(e) e)
stopifnot(identical(authorNames(c("me", "you")), c("me", "you")),
          is(tryIt(authorNames(character())), "error"), # empty value
          is(tryIt(authorNames(NULL)), "error"))        # no default method
## End Don't show

## An example of group generic methods, using the class
## "track"; see the documentation of setClass for its definition

## define a method for the Arith group

setMethod("Arith", c("track", "numeric"),
 function(e1, e2) {
  e1@y <- callGeneric(e1@y , e2)
  e1
})

setMethod("Arith", c("numeric", "track"),
 function(e1, e2) {
  e2@y <- callGeneric(e1, e2@y)
  e2
})

## now arithmetic operators  will dispatch methods:

t1 <- new("track", x=1:10, y=sort(stats::rnorm(10)))

t1 - 100
1/t1

## Don't show: 
removeGeneric("authorNames")
removeClass("track")
removeMethods("Arith")
removeGeneric("props")
removeGeneric("increment")
## End Don't show



cleanEx()
nameEx("setLoadActions")
### * setLoadActions

flush(stderr()); flush(stdout())

### Name: setLoadActions
### Title: Set Actions For Package Loading
### Aliases: setLoadAction setLoadActions getLoadActions hasLoadAction
###   evalOnLoad evalqOnLoad
### Keywords: package

### ** Examples

## Not run: 
##D ## in the code for some package
##D 
##D ## ... somewhere else
##D setLoadActions(function(attach)
##D    cat(c("Loaded", "Unloaded")[attach], "at", Sys.time(), "\n"),
##D   setCount = function(ns) assign("myCount", 1, envir = ns),
##D   function(ns) assign("myPointer", getMyExternalPointer(), envir = ns))
##D   ... somewhere later
##D if(countShouldBe0)
##D   setLoadAction(function(ns) assign("myCount", 0, envir = ns), "setCount")
## End(Not run)



cleanEx()
nameEx("setMethod")
### * setMethod

flush(stderr()); flush(stdout())

### Name: setMethod
### Title: Create and Save a Method
### Aliases: setMethod removeMethod
### Keywords: programming classes methods

### ** Examples

## Don't show: 
  require(stats)
  setClass("track",
    representation(x="numeric", y = "numeric"))
  setClass("trackCurve", representation("track",
    smooth = "numeric"))
  setClass("trackMultiCurve", representation(x="numeric", y="matrix", smooth="matrix"),
          prototype = list(x=numeric(), y=matrix(0,0,0), smooth=
  matrix(0,0,0)))
## End Don't show

require(graphics)
## methods for plotting track objects (see the example for setClass)
##
## First, with only one object as argument:
setMethod("plot", signature(x="track", y="missing"),
  function(x,  y, ...) plot(slot(x, "x"), slot(x, "y"), ...)
)
## Second, plot the data from the track on the y-axis against anything
## as the x data.
setMethod("plot", signature(y = "track"),
 function(x, y, ...) plot(x, slot(y, "y"), ...)
)
## and similarly with the track on the x-axis (using the short form of
## specification for signatures)
setMethod("plot", "track",
 function(x, y, ...) plot(slot(x, "y"), y,  ...)
)
t1 <- new("track", x=1:20, y=(1:20)^2)
tc1 <- new("trackCurve", t1)
slot(tc1, "smooth") <- smooth.spline(slot(tc1, "x"), slot(tc1, "y"))$y #$
plot(t1)
plot(qnorm(ppoints(20)), t1)
## An example of inherited methods, and of conforming method arguments
## (note the dotCurve argument in the method, which will be pulled out
## of ... in the generic.
setMethod("plot", c("trackCurve", "missing"),
function(x, y, dotCurve = FALSE, ...) {
  plot(as(x, "track"))
  if(length(slot(x, "smooth") > 0))
    lines(slot(x, "x"), slot(x, "smooth"),
         lty = if(dotCurve) 2 else 1)
  }
)
## the plot of tc1 alone has an added curve; other uses of tc1
## are treated as if it were a "track" object.
plot(tc1, dotCurve = TRUE)
plot(qnorm(ppoints(20)), tc1)

## defining methods for a special function.
## Although "[" and "length" are not ordinary functions
## methods can be defined for them.
setMethod("[", "track",
  function(x, i, j, ..., drop) {
    x@x <- x@x[i]; x@y <- x@y[i]
    x
  })
plot(t1[1:15])

setMethod("length", "track", function(x)length(x@y))
length(t1)

## methods can be defined for missing arguments as well
setGeneric("summary") ## make the function into a generic

## A method for summary()
## The method definition can include the arguments, but
## if they're omitted, class "missing" is assumed.

setMethod("summary", "missing", function() "<No Object>")

## Don't show: 

stopifnot(identical(summary(), "<No Object>"))

removeMethods("summary")

## for the primitives
## inherited methods

length(tc1)
tc1[-1]

## make sure old-style methods still work.
t11 <- t1[1:15]
identical(t1@y[1:15], t11@y)

## S3 methods, with nextMethod
form <- y ~ x
form[1]

## S3 arithmetic methods
ISOdate(1990, 12, 1)- ISOdate(1980, 12, 1)

## group methods

setMethod("Arith", c("track", "numeric"), function(e1, e2){e1@y <-
  callGeneric(e1@y , e2); e1})


t1  - 100.

t1/2


## check it hasn't screwed up S3 methods
ISOdate(1990, 12, 1)- ISOdate(1980, 12, 1)

## test the .Generic mechanism

setMethod("Compare", signature("track", "track"),
  function(e1,e2) {
  switch(.Generic,
   "==" = e1@y == e2@y,
  NA)
 })

#stopifnot(all(t1==t1))
#stopifnot(identical(t1<t1, NA))


## A test of nested calls to "[" with matrix-style arguments
## applied to data.frames (S3 methods)

setMethod("[", c("trackMultiCurve", "numeric", "numeric"), function(x, i, j, ..., drop) {
### FIXME:  a better version has only 1st arg in signature
### and uses callNextMethod, when this works with primitives.
    x@y <- x@y[i, j, drop=FALSE]
    x@x <- x@x[i]
    x
})


"testFunc" <-
function(cur) {
    sorted <- cur[order(cur[,1]),]
    sorted[ !is.na(sorted[,1]), ]
}

Nrow <- 1000 # at one time, values this large triggered a bug in gc/protect
## the loop here was a trigger for the bug
Niter <- 10
for(i in 1:Niter)  {
    yy <- matrix(stats::rnorm(10*Nrow), 10, Nrow)
    tDF <- as.data.frame(yy)
    testFunc(tDF)
}


tMC <- new("trackMultiCurve", x=seq_len(Nrow), y = yy)
## not enough functions have methods for this class to use testFunc

stopifnot(identical(tMC[1:10, 1:10]@y, yy[1:10, 1:10]))


## verify we can remove methods and generic

removeMethods("-")
removeMethod("length", "track")
removeMethods("Arith")
removeMethods("Compare")

## repeat the test one more time on the primitives

length(ISOdate(1990, 12, 1)- ISOdate(1980, 12, 1))

removeMethods("length")

## methods for %*%, which isn't done by the same C code as other ops

setClass("foo", representation(m="matrix"))
m1 <- matrix(1:12,3,4)
f1 = new("foo", m=m1)
f2 = new("foo", m=t(m1))

setMethod("%*%", c("foo", "foo"),
 function(x,y)callGeneric(x@m, y@m))

stopifnot(identical(f1%*%f2, m1%*% t(m1)))

removeMethods("%*%")

removeMethods("plot")

## Hold until removeMethods revised: stopifnot(existsFunction("plot", FALSE) && !isGeneric("plot", 1))

## methods for plotData
plotData <- function(x, y, ...) plot(x, y, ...)

setGeneric("plotData")

setMethod("plotData", signature(x="track", y="missing"),
  function(x,  y, ...) plot(slot(x, "x"), slot(x, "y"), ...)
)
## and now remove the whole generic
removeGeneric("plotData")

stopifnot(!exists("plotData", 1))

##  Tests of method inheritance & multiple dispatch
setClass("A0", representation(a0 = "numeric"))

setClass("A1", representation("A0", a1 = "character"))

setClass("B0" ,representation(b0 = "numeric"))

setClass("B1", "B0")

setClass("B2", representation("B1", b2 = "logical"))

setClass("AB0", representation("A1", "B2", ab0 = "matrix"))

f1 <- function(x,  y)"ANY"

setGeneric("f1")

setMethod("f1", c("A0", "B1"), function(x, y)"A0 B1")
setMethod("f1", c("B1", "A0"), function(x, y)"B1 A0")

a0 <- new("A0")
a1 <- new("A1")
b0 <- new("B0")
b1 <- new("B1")
b2 <- new("B2")

deparseText <- function(expr)
    paste(deparse(expr), collapse = "\  ")

mustEqual <- function(e1, e2){
    if(!identical(e1, e2))
        stop(paste("!identical(", deparseText(substitute(e1)),
                   ", ", deparseText(substitute(e2)), ")", sep=""))
}

mustEqual(f1(a0, b0), "ANY")
mustEqual(f1(a1,b0), "ANY")
mustEqual(f1(a1,b1), "A0 B1")
mustEqual(f1(b1,a1), "B1 A0")
mustEqual(f1(b1,b1), "ANY")

## remove classes:  order matters so as not to undefine earlier classes
for(.cl in c("AB0", "A1", "A0", "B2", "B1", "B0"))
    removeClass(.cl)

removeGeneric("f1")

## test of nonstandard generic definition

setGeneric("doubleAnything", function(x) {
  methodValue <- standardGeneric("doubleAnything")
  c(methodValue, methodValue)
})

setMethod("doubleAnything", "ANY", function(x)x)

setMethod("doubleAnything", "character",
function(x)paste("<",x,">",sep=""))

mustEqual(doubleAnything(1:10), c(1:10, 1:10))
mustEqual(doubleAnything("junk"), rep("<junk>",2))

removeGeneric("doubleAnything")


## End Don't show



cleanEx()
nameEx("setOldClass")
### * setOldClass

flush(stderr()); flush(stdout())

### Name: setOldClass
### Title: Register Old-Style (S3) Classes and Inheritance
### Aliases: setOldClass .setOldIs POSIXct-class POSIXlt-class POSIXt-class
###   aov-class maov-class anova-class anova.glm-class anova.glm.null-class
###   Date-class data.frame-class data.frameRowLabels-class density-class
###   dump.frames-class factor-class formula-class glm-class glm.null-class
###   hsearch-class integrate-class libraryIQR-class lm-class logLik-class
###   mlm-class mtable-class mts-class ordered-class packageIQR-class
###   packageInfo-class recordedplot-class rle-class socket-class
###   summaryDefault-class summary.table-class oldClass-class
###   .OldClassesList table-class initialize,data.frame-method
###   initialize,factor-method initialize,ordered-method
###   initialize,table-method initialize,summary.table-method
### Keywords: programming methods

### ** Examples

require(stats)
setOldClass(c("mlm", "lm"))
setGeneric("dfResidual", function(model)standardGeneric("dfResidual"))
setMethod("dfResidual", "lm", function(model)model$df.residual)

## dfResidual will work on mlm objects as well as lm objects
myData <- data.frame(time = 1:10, y = (1:10)^.5)
myLm <- lm(cbind(y, y^3)  ~ time, myData)

showClass("data.frame")# to see the predefined S4 "oldClass"

## two examples extending S3 class "lm", class "xlm" directly
## and "ylm" indirectly
setClass("xlm", representation(eps = "numeric"), contains = "lm")
setClass("ylm", representation(header = "character"), contains = "xlm")
ym1 = new("ylm", myLm, header = "Example", eps = 0.)
## for more examples, see ?S3Class.

utils::str(.OldClassesList)

## Don't show: 
stopifnot(identical(dfResidual(myLm), myLm$df.residual))
removeClass("ylm"); removeClass("xlm")
rm(myData, myLm)
removeGeneric("dfResidual")
## End Don't show

## Examples of S3 classes with guaranteed attributes
## an S3 class "stamped" with a vector and  a "date" attribute
## Here is a generator function and an S3 print method.
## NOTE:  it's essential that the generator checks the attribute classes
stamped <- function(x, date = Sys.time()) {
    if(!inherits(date, "POSIXt"))
      stop("bad date argument")
    if(!is.vector(x))
      stop("x must be a vector")
    attr(x, "date") <- date
    class(x) <- "stamped"
    x
}

print.stamped <- function(x, ...) {
    print(as.vector(x))
    cat("Date: ",  format(attr(x,"date")), "\n")
}

## Now, an S4 class with the same structure:
setClass("stamped4", contains = "vector", representation(date = "POSIXt"))

## We can use the S4 class to register "stamped", with its attributes:
setOldClass("stamped", S4Class = "stamped4")
selectMethod("show", "stamped")
## and then remove "stamped4" to clean up
removeClass("stamped4")
## Don't show: 
set.seed(113)
## End Don't show
someLetters <- stamped(sample(letters, 10),
                       ISOdatetime(2008, 10, 15, 12, 0, 0))

st <- new("stamped", someLetters)
st
# show() method prints the object's class, then calls the S3 print method.

stopifnot(identical(S3Part(st, TRUE), someLetters))

# creating the S4 object directly from its data part and slots
new("stamped", 1:10, date = ISOdatetime(1976, 5, 5, 15, 10, 0))

## Not run: 
##D ## The code in R that defines "ts" as an S4 class
##D setClass("ts", contains = "structure",
##D       representation(tsp = "numeric"),
##D       prototype(NA, tsp = rep(1,3)))
##D # prototype to be a legal S3 time-series
##D ## and now registers it as an S3 class
##D     setOldClass("ts", S4Class = "ts", where = envir)
## End(Not run)

## Don't show: 
  removeClass("stamped")
  rm(someLetters, st)
## End Don't show




cleanEx()
nameEx("show")
### * show

flush(stderr()); flush(stdout())

### Name: show
### Title: Show an Object
### Aliases: show show-methods show,ANY-method show,traceable-method
###   show,ObjectsWithPackage-method show,MethodDefinition-method
###   show,MethodWithNext-method show,genericFunction-method
###   show,classRepresentation-method
### Keywords: programming

### ** Examples

## following the example shown in the setMethod documentation ...
setClass("track",
         representation(x="numeric", y="numeric"))
setClass("trackCurve",
         representation("track", smooth = "numeric"))

t1 <- new("track", x=1:20, y=(1:20)^2)

tc1 <- new("trackCurve", t1)

setMethod("show", "track",
  function(object)print(rbind(x = object@x, y=object@y))
)
## The method will now be used for automatic printing of t1

t1

## Not run: 
##D   [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
##D x    1    2    3    4    5    6    7    8    9    10    11    12
##D y    1    4    9   16   25   36   49   64   81   100   121   144
##D   [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20]
##D x    13    14    15    16    17    18    19    20
##D y   169   196   225   256   289   324   361   400
## End(Not run)
## and also for tc1, an object of a class that extends "track"
tc1

## Not run: 
##D   [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
##D x    1    2    3    4    5    6    7    8    9    10    11    12
##D y    1    4    9   16   25   36   49   64   81   100   121   144
##D   [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20]
##D x    13    14    15    16    17    18    19    20
##D y   169   196   225   256   289   324   361   400
## End(Not run)



cleanEx()
nameEx("showMethods")
### * showMethods

flush(stderr()); flush(stdout())

### Name: showMethods
### Title: Show all the methods for the specified function(s)
### Aliases: showMethods
### Keywords: methods

### ** Examples

require(graphics)
## Don't show: 
 setClass("track",
          representation(x="numeric", y="numeric"))
 ## First, with only one object as argument:
 setMethod("plot", signature(x="track", y="missing"),
           function(x,  y, ...) plot(slot(x, "x"), slot(x, "y"), ...))
 ## Second, plot the data from the track on the y-axis against anything
 ## as the x data.
 setMethod("plot", signature(y = "track"),
           function(x, y, ...) plot(x, slot(y, "y"), ...))
 setMethod("plot", "track",
           function(x, y, ...) plot(slot(x, "y"), y,  ...))
## End Don't show
## Assuming the methods for plot
## are set up as in the example of help(setMethod),
## print (without definitions) the methods that involve class "track":
showMethods("plot", classes = "track")
## Not run: 
##D # Function "plot":
##D # x = ANY, y = track
##D # x = track, y = missing
##D # x = track, y = ANY
##D 
##D require("Matrix")
##D showMethods("%*%")# many!
##D     methods(class = "Matrix")# nothing
##D showMethods(class = "Matrix")# everything
##D showMethods(Matrix:::isDiagonal) # a non-exported generic
## End(Not run)

not.there <- !any("package:stats4" == search())
if(not.there) library(stats4)
showMethods(classes = "mle")
if(not.there) detach("package:stats4")



cleanEx()
nameEx("slot")
### * slot

flush(stderr()); flush(stdout())

### Name: slot
### Title: The Slots in an Object from a Formal Class
### Aliases: slot .hasSlot slot<- slotNames .slotNames getSlots
### Keywords: programming classes

### ** Examples

## Don't show: 
if(isClass("track")) removeClass("track")
## End Don't show

setClass("track", representation(x="numeric", y="numeric"))
myTrack <- new("track", x = -4:4, y = exp(-4:4))
slot(myTrack, "x")
slot(myTrack, "y") <- log(slot(myTrack, "y"))
utils::str(myTrack)

getSlots("track") # or
getSlots(getClass("track"))
slotNames(class(myTrack)) # is the same as
slotNames(myTrack)

## Don't show: 
removeClass("track")##  should not be needed... see ./setClass.Rd
## End Don't show



cleanEx()
nameEx("testInheritedMethods")
### * testInheritedMethods

flush(stderr()); flush(stdout())

### Name: testInheritedMethods
### Title: Test for and Report about Selection of Inherited Methods
### Aliases: testInheritedMethods MethodSelectionReport-class .Other-class
### Keywords: programming classes methods

### ** Examples

## if no other attached packages have methods for `+` or its group
## generic functions, this returns a 16 by 2 matrix of selection
## patterns (in R 2.9.0)
testInheritedMethods("+")



cleanEx()
nameEx("validObject")
### * validObject

flush(stderr()); flush(stdout())

### Name: validObject
### Title: Test the Validity of an Object
### Aliases: validObject getValidity setValidity
### Keywords: programming classes

### ** Examples

setClass("track",
          representation(x="numeric", y = "numeric"))
t1 <- new("track", x=1:10, y=sort(stats::rnorm(10)))
## A valid "track" object has the same number of x, y values
validTrackObject <- function(object) {
    if(length(object@x) == length(object@y)) TRUE
    else paste("Unequal x,y lengths: ", length(object@x), ", ",
               length(object@y), sep="")
}
## assign the function as the validity method for the class
setValidity("track", validTrackObject)
## t1 should be a valid "track" object
validObject(t1)
## Now we do something bad
t2 <- t1
t2@x <- 1:20
## This should generate an error
## Not run: try(validObject(t2))
## Don't show: 
stopifnot(is(try(validObject(t2)), "try-error"))
## End Don't show

setClass("trackCurve",
         representation("track", smooth = "numeric"))

## all superclass validity methods are used when validObject
## is called from initialize() with arguments, so this fails
## Not run: trynew("trackCurve", t2)
## Don't show: 
stopifnot(is(try(new("trackCurve", t2)), "try-error"))
## End Don't show

setClass("twoTrack", representation(tr1 = "track", tr2 ="track"))

## validity tests are not applied recursively by default,
## so this object is created (invalidly)
tT  <- new("twoTrack", tr2 = t2)

## A stricter test detects the problem
## Not run: try(validObject(tT, complete = TRUE))
## Don't show: 
stopifnot(is(try(validObject(tT, complete = TRUE)), "try-error"))
## End Don't show



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
