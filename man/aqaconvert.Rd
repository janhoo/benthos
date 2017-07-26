\name{aqaconvert}
\encoding{UTF-8}
\alias{aqaconvert}
\alias{afdm2afdms}
\alias{afdm2dm}
\alias{afdm2wm}
\alias{c2afdm}
\alias{c2dm}
\alias{c2n}
\alias{c2p}
\alias{carbohydr2dm}
\alias{dm2dms}
\alias{dm2wm}
\alias{j2afdm}
\alias{j2c}
\alias{j2dm}
\alias{j2dms}
\alias{j2n}
\alias{j2wm}
\alias{j2wms}
\alias{lipid2dm}
\alias{n2afdm}
\alias{n2dm}
\alias{n2p}
\alias{p2afdm}
\alias{p2dm}
\alias{prot2dm}
\alias{wm2wms}
\title{
Mass and Composition Conversion of Aquatic Species
}
\description{
\code{aqaconvert} 
taxon or higher level specific conversion factors for aquatic species composition / weights using emirical data. 

\code{aqaconvert} is the base level function of all of the following specialised functions, which search the conversion database (\code{conv}):

\tabular{ll}{
	
\code{afdm2afdms} \tab the average ratio of ash-free-dry-mass and ash-free-dry-mass plus shell, \cr

\code{afdm2dm} \tab  the average ratio of ash-free-dry-mass and dry mass,  \cr

\code{afdm2wm} \tab  the average ratio of ash-free-dry-mass and wet mass,  \cr

\code{c2afdm} \tab  the average  ratio of carbon mass and ash-free-dry-mass, \cr

\code{c2dm} \tab  the average ratio of carbon mass and dry mass, \cr

\code{c2n} \tab  the average ratio of carbon and nitrogen masses, \cr

\code{c2p} \tab  the average ratio of carbon and phospherous masses, \cr

\code{carbohydr2dm} \tab  the average carbohydrate mass and dry mass, \cr

\code{dm2dms} \tab  the average ratio of dry mass and dry mass plus shell, \cr

\code{dm2wm} \tab  the average ratio of dry mass and wet mass, \cr

\code{j2afdm} \tab  the average ratio of joule  and miligram, \cr

\code{j2c} \tab  the average ratio of joule and miligram carbon, \cr

\code{j2dm} \tab  the average ratio of joule and miligram dry mass,\cr

\code{j2dms} \tab  the average ratio of joule and miligram dry mass plus shell,\cr

\code{j2n} \tab  the average ratio of joule and miligram nitrogen,\cr

\code{j2wm} \tab  the average ratio of joule and miligram wet mass,\cr

\code{j2wms} \tab  the average ratio of joule and miligram wet mass plus shell,\cr

\code{lipid2dm} \tab  the average ratio of lipid mass and dry mass,\cr

\code{n2afdm} \tab  the average ratio of nitrogen mass and ash-free-dry-mass,\cr

\code{n2dm} \tab  the average ratio of  nitrogen mass and dry mass,\cr

\code{n2p} \tab  the average ratio of nitrogen and phospherous masses,\cr

\code{p2afdm} \tab  the average ratio of phospherous mass and ash-free-dry-mass,\cr

\code{p2dm} \tab  the average ratio of phospherous mass and dry mass,\cr

\code{prot2dm} \tab  the average ratio of protein mass and dry mass,\cr

\code{wm2wms} \tab  the average ratio of wet mass and wet mass plus shell.\cr
}
catalog


}
\usage{
aqaconvert(x, stage = "adult", depth = 3, method, catalog = TRUE, relative = TRUE, verbose = FALSE)

dm2dms(x, stage = "adult", depth = 3, catalog = TRUE, relative = TRUE, verbose = FALSE)
afdm2afdms(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE)
dm2wm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE)
afdm2dm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE)
afdm2wm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE)
prot2dm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
carbohydr2dm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
lipid2dm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE)
c2dm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
n2dm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
p2dm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
c2afdm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
n2afdm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
p2afdm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE)
j2wm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE)
j2wms(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
j2dm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
j2dms(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
j2afdm(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
j2c(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
j2n(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
c2n(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 
c2p(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE)
n2p(x, stage = "adult", depth = 3,  catalog = TRUE, relative = TRUE, verbose = FALSE) 





}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
character vector or factor with species names or higher taxonomic units.
}
  \item{stage}{
Word. One of "adult", "juvenil", "larvae", "all", "nonadult" "nolarvae". Default is "adult".
}
  \item{method}{
Integer. Units to be converted. see Details.
}
  \item{depth}{
Integer. number of recursions in the taxonomic tree search \code{depth=1} refers to \code{species} or source taxonomic level (see \code{relative}). Default is  \code{depth=3}.
}
  \item{relative}{
Logical. Determins how argument \code{depth} is evaluated: If \code{TRUE}, the taxonomic level of the search determins search depth. If \code{FALSE}, depth count always starts at taxonomic level \code{species}. Default is \code{relative=TRUE}.
}
  \item{catalog}{
Logical. Lookup taxonomic tree information in a database in order to perform a search of higher taxonomic levels in conversion database if input is not found in conversion database}

  \item{verbose}{
Logical. Gives some output while working
}
}
\details{
Maybe you have measured dry mass of several aqatic species and need to know ash-free-dry-mass this can be achived by \code{myspeciesdryweights*afdm2dm(myspeciesnames) }. 
Behind the curtain, this function trys to find the given species names or higher taxonomic units in the conversion database of literature data  and averages over all occurrences where the ratio of dry mass to afdm was determined. If the species is not in the database and \code{catalog=TRUE}, it is looked up in the the database (\code{data(itis)}) and the respective higer taxonomic units are looked up in the conversion database. How far upwards the taxonomic tree we look for empirical data, is determined by \code{depth}. \code{depth=1} means we need an exact mach of given taxonomic units, \code{depth=3} means for, e.g. species, (since it is \code{species, genus, family, suborder, order, class, phylum, kingdom} in conversion database) that we would accept an average value of the respective family but not higher up the taxonomic tree.

EXAMPLE:

Say you have  \sQuote{brachyura} \sQuote{callianassa tyrrhena} \sQuote{galathea intermedia} in you \code{specieslist}, two species and a higher level taxonomic unit, e.g., \code{afdm2dm(specieslist)} will look in the conversion database for \sQuote{brachyura}, find 134 entries under \code{suborder} of which 67 contain published empirical data regarding the ratio of dry to ash-free-dry mass. The other entries regard other conversions. The average value is returned. Same for \sQuote{galathea intermedia} which is in conversion database. In contrast, \sQuote{callianassa tyrrhena} is not found in the conversion database, so, since \code{catalog=TRUE} by default, \sQuote{callianassa tyrrhena} is looked up and found in the ITIS database and its taxonomic tree retrieved. \sQuote{callianassa tyrrhena} is of genus \sQuote{callianassa} which is then searched for but not found in conversion. Further, it is of subclass \sQuote{callianassidae} and here we get one hit in conversion where it is at family level. The respective value is returned.

\code{depth} controlles (together with \code{relative}) how far upward we climb the taxonomic tree. So far, the existing taxonomic hierarchy in conversion is: \code{species}, \code{genus}, \code{family}, \code{suborder}, \code{order}, \code{class}, \code{phylum}, \code{kingdom}. 

Locating a taxonomic unit within the conversion database does not mean that the data we want is available. The higher we move up the taxonomic tree, the more likely we will get the required data (at the cost of higher uncertainty). If \code{relative=TRUE} we will accept to search \code{depth} taxonomic levels upward from the level of the given taxonomic unit (including that level. So, if {species} and \code{depth=3}, we'll search \code{species}, \code{genus} and \code{family}; if \code{depth=5}, we'll search \code{species}, \code{genus}, \code{family}, \code{suborder} and \code{order}; if the given the given taxonomic unit is of level \code{family} and \code{depth=3}, we will search \code{family}, \code{suborder} and \code{order}. \code{depth=0} is not allowed!). 

If \code{relative=FALSE}, wa always anchor at species level, no matter what taxonomic level the taxonomic unit that search for is located. So, if \code{relative=FALSE, depth=3} we never search beyond family. One application of \code{relative=TRUE} is to prevent low level averages of e.g., kingdom to be found, when the search is populated with heavily heterogenous taxonomiv level like (species, family, species, order,...)
}
\references{
T. Brey, C. Müller-Wiegmann, Z. Zittier, W. Hagen, 2010. Body composition in aquatic organisms--a global data bank of relationships between mass, element composition 

Brey T, 2001. Population dynamics in benthic invertebrates. http://www.thomas-brey.de/science/
}
\author{
Jan M Holstein <holstein@uni-bremen.de>
}
\note{
Data in \code{conv} compiled by Thomas Brey, Corinna Müller-Wiegmann, Zora Zittier, Wilhelm Hagen
}


\examples{
	myspecies<-data.frame(name=c("brachyura", 
		"callianassa tyrrhena", "galathea intermedia"),
		afdw=c(12.37,45.03,11.64))
	myspecies$afdw/afdm2dm(myspecies$name) #drymass
}

