## EXAMPLE OF BATCH GENERATING HTML REPORTS FOR SEVERAL
## DIRECTORIES OF IMAGES

## Load the package
library(uasimg)

## Create a character vector of the directory names
img_dirs <- c("D:/Pix4D/PtPinole/imgs/eg/eg_flt1_p4p_sw",
              "D:/Pix4D/PtPinole/imgs/eg/eg_flt1_p4p_se",
              "D:/Pix4D/PtPinole/imgs/eg/eg_flt2_m2p",
              "D:/Pix4D/PtPinole/imgs/eg/eg_flt3_m2p",
              "D:/Pix4D/PtPinole/imgs/wg/wg_flt01",
              "D:/Pix4D/PtPinole/imgs/wg/wg_flt02",
              "D:/Pix4D/PtPinole/imgs/wg/wg_flt03")

dir_out <- "c:/temp/uasimg_test/kmls"

#for (i in 1:length(img_dirs)) {

  ## Grab the ith directory
i <- 2
(img_dir <- img_dirs[i]); file.exists(img_dir)

## Extract the metadata, saving the results in the cache in case it was already processed
flt_imgs <- uas_info(img_dirs[i], cache = TRUE)

x <- flt_imgs$`D:/Pix4D/PtPinole/imgs/eg/eg_flt1_p4p_se`$mcp
class(x)
names(x)

library(sf)
st_write(x, dsn = file.path(dir_out, paste0(basename(img_dir), "2.kml")), layer = "")


xmlns_gx <- "http://www.google.com/kml/ext/2.2"

kml_visibility <- TRUE
kml_open_main_folder <- TRUE
kml_open_ind_folder <- TRUE

icon.url <- "http://maps.google.com/mapfiles/kml/pal2/icon18.png"

##############################################
### START A NEW KML
##############################################
kml_xsd <- "http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd"
xmlns <- "http://www.opengis.net/kml/2.2/"
kml_visibility <- TRUE
kml_open_main_folder <- TRUE

## Start a new XML object
kml.out <- XML::newXMLNode("kml", attrs = c(version = "1.0"),
                           namespaceDefinitions = c(xsd = kml_xsd, xmlns = xmlns))

## Start a new node
doc.parent <- XML::newXMLNode("Document", parent=kml.out)
class(doc.parent)

kml_open_ind_folder <- TRUE
if (!kml_open_ind_folder) {
  doc.style <- XML::newXMLNode("Style", parent=doc.parent, attrs=list(id="check-hide-children"))
  doc.liststyle <- XML::newXMLNode("ListStyle", parent=doc.style)
  doc.litype <- XML::newXMLNode("listItemType", parent=doc.liststyle, text="checkHideChildren")
}

icon.url <- "http://maps.google.com/mapfiles/kml/pal2/icon18.png"

###############################
library(XML)
# just a sample kml file... needs to be well-formed
z <-
  '<kml xmlns="http://www.opengis.net/kml/2.2">
      <Document>
         <Folder>
            <name>Some Name</name>
            <Placemark>
               <name>Jackson</name>
               <Polygon><outerBoundaryIs><LinearRing><coordinates>-91.151982,44.136728 -91.152627,44.158372 -91.152677,44.170618 -91.152732,44.183928 -91.152372,44.187182 -91.15244,44.187307 -91.152495,44.208702 -91.152892,44.222784 -91.153829,44.24758 -91.165786,44.247429 -91.165936,44.2546 -91.166159,44.272973 -91.166311,44.303743 -91.166469,44.306118 -91.166056,44.30943 -91.165995,44.33161 -91.16614,44.334932 -91.166128,44.335102 -91.16648,44.339077 -91.165909,44.343314 -91.166149,44.34968 -91.165604,44.37626 -91.165545,44.378794 -91.164813,44.418752 -91.164768,44.42248 -91.164873,44.451251 -91.164858,44.45132 -91.164915,44.451544 -91.165155,44.473148 -91.165187,44.480189 -91.165197,44.482528 -91.165636,44.496874 -91.1657,44.498476 -91.16588,44.509756 -91.165916,44.528032 -91.165696,44.550109 -91.165666,44.565612 -91.165408,44.567894 -91.165713,44.567989 -91.165619,44.596987 -91.130969,44.596692 -91.125674,44.596666 -91.119816,44.596285 -91.114632,44.596343 -91.043609,44.596609 -91.026595,44.596426 -91.009307,44.59663 -90.992352,44.596619 -90.990775,44.596447 -90.972986,44.596516 -90.94268,44.596582 -90.92235,44.596293 -90.922637,44.591081 -90.922579,44.580701 -90.922618,44.543798 -90.922775,44.512564 -90.923104,44.509838 -90.801852,44.509573 -90.80194,44.458502 -90.801949,44.429518 -90.801928,44.422301 -90.764289,44.422345 -90.760977,44.422336 -90.72215,44.422293 -90.719005,44.422213 -90.705154,44.422289 -90.680591,44.422218 -90.619465,44.422127 -90.558746,44.42221 -90.497221,44.422613 -90.436472,44.422546 -90.397588,44.423279 -90.31605,44.424673 -90.317397,44.337429 -90.31766,44.291998 -90.317938,44.249963 -90.318107,44.248791 -90.312035,44.248758 -90.31257,44.240933 -90.3125,44.194598 -90.312225,44.19244 -90.312083,44.187037 -90.31249,44.184046 -90.312575,44.155284 -90.435728,44.161022 -90.461889,44.16082 -90.488949,44.160944 -90.49033,44.16076 -90.546446,44.160284 -90.553421,44.160309 -90.591977,44.160163 -90.653047,44.159585 -90.672674,44.159319 -90.774673,44.159056 -90.792783,44.158862 -90.817517,44.158692 -90.817713,44.158644 -90.887873,44.158429 -90.888823,44.158484 -90.8929,44.158323 -90.906152,44.158316 -90.912893,44.154445 -90.916033,44.153313 -90.919834,44.150576 -90.922496,44.147593 -90.927157,44.14646 -90.930865,44.14648 -90.936741,44.147456 -90.939122,44.147337 -90.942552,44.144407 -90.945184,44.146673 -90.947547,44.147332 -90.948945,44.146008 -90.950352,44.143296 -90.952723,44.141954 -90.956902,44.141823 -90.958111,44.140704 -90.958909,44.137424 -90.962404,44.136491 -90.9641,44.137405 -90.966729,44.136248 -90.96894,44.133218 -90.97016,44.129621 -90.971909,44.128667 -90.976753,44.129869 -90.978639,44.127998 -90.978398,44.12666 -90.97364,44.124266 -90.971168,44.122493 -90.967644,44.119582 -90.968004,44.118533 -90.970286,44.117542 -90.973565,44.116895 -90.972764,44.070775 -91.023637,44.071134 -91.04639,44.071139 -91.057154,44.071141 -91.090641,44.071239 -91.091691,44.071192 -91.111766,44.071094 -91.151768,44.071014 -91.151843,44.079656 -91.151901,44.095396 -91.151975,44.114529 -91.151982,44.136728</coordinates></LinearRing></outerBoundaryIs></Polygon>
            </Placemark>
         </Folder>
      </Document>
    </kml>'
xmlDoc <- xmlInternalTreeParse(z)

# this is the important bit...
ns <- c(gx="http://www.google.com/kml/ext/2.2",
        kml="http://www.opengis.net/kml/2.2",
        atom="http://www.w3.org/2005/Atom")
ensureNamespace(xmlDoc, ns)
# save it...
saveXML(xmlDoc, file.path(dir_out, "sample.kml"))



