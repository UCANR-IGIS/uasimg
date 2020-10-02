
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

# Drone Image Utilities

`uasimg` helps manage images taken from an unmanned aerial vehicle
(drone) that have been collected with the intent to stitch into high
resolution orthomosaics and other data products. The package does
**not** stitch images, but helps you create catalogs of your images,
visualize their locations, export image centroids and estimated
footprints as GIS files, and create world files so individual images can
be viewed in GIS software.

## Applications

`uasimg` was developed to help with the following common data management
tasks:

1.  Doing a quick check in the field of photos after a flight, assessing
    their locations and estimated image overlap. This can help a pilot
    determine if a flight was successful, or needs to be redone.

2.  Subsetting images for further processing with a photogrammetry
    (stitching) program like Pix4D or Agisoft. Omitting images with an
    extreme amount of overlap can reduce processing time and improve
    results.

3.  Creating Flight Summary HTML pages, as the backbone of an image
    catalog.

4.  Creating world files for individual drone images, using the image
    EXIF data to model the ground footprint and rotation, so they can
    imported in GIS software and appear in their approximate location.

**Note**: image locations and footprints are based on the metadata saved
in the image files (e.g., relative altitude above the launch point,
camera compass direction). Thus they should taken as *estimates only*.

## Installation

`uasimg` is not on CRAN (yet), but you can install it from GitHub.

*Note*: Windows users need *RTools* to build packages from source files
(which is basically what you do when you install from GitHub). RTools is
not a R package, rather its a set of utilities that you install
separately. You can download and install RTools from
<https://cran.r-project.org/bin/windows/Rtools/>. Alternately, you can
install RTools from within R by running:

``` r
install.packages('installr')
installr::install.Rtools()
```

After RTools is installed, you can install `uasimg` with:

``` r
## install.packages("remotes") 
library (remotes)
remotes::install_github("ucanr-igis/uasimg")
```

### Dependent Packages

`uasimg` requires several dependent packages, including *sf*, *dplyr*,
*tidyr*, *leaflet*, *htmltools* and others (see the DESCRIPTION file for
a complete list). If you get an error message about missing dependent
packages, install the dependent packages separately (i.e., from the
‘Packages’ pane in RStudio) then run
`remotes::install_github("ucanr-igis/uasimg", dependencies=FALSE)`.

### exiftool

To read the EXIF data from the image files, `uasimg` requires an
external command line tool called ‘exiftool’. exiftool can be installed
in four steps:

1.  download the file for your OS from:
    <http://www.sno.phy.queensu.ca/~phil/exiftool/>
2.  uncompress / unzip
3.  rename the executable file from *exiftool(-k).exe* to
    *exiftool.exe*  
    **Note**: if you have file extensions hidden in Windows Explorer,
    you won’t see *.exe* in the filename. In that case, just rename
    ‘*exiftool(-k)*’ to ‘*exiftool*’.
4.  move the executable file to a directory on the path (e.g.,
    c:\\windows). Note: putting it in c:\\windows\\system32 does *not*
    seem to work.

## Supported Cameras

To see a list of known cameras (sensors), run `uas_cameras()` with no
arguments. If your camera is not listed, you may submit an
[issue](https://github.com/ucanr-igis/uasimg/issues) on GitHub to have
it added, or pass the camera parameters in as a csv file. For details
see the help page (`?uas_cameras`) or contact the package author.

## Data Requirements

Most functions in `uasimg` use location data saved in the EXIF data
(header) of image files themselves. This assumes the camera saves the
location in the images using a GPS coordinate from the drone or the
camera itself. To compute footprints, the package also needs to know the
height at which images were taken. Some drones (including many DJI
drones) record the relative flight altitude (above the launch point) in
the image file. Flight height can also be entered manually as an
argument.

Requirements for `uasimg` include:

  - images must have been taken by a camera that saved the GPS
    coordinates  
  - image files should be grouped in directories folders (typically each
    directory containing the images from one flight)
  - the camera model must be one of the ones known by the `uasimg`
    package (see below)

Additional requirements to generate estimated footprints:

  - the height above ground level must be saved in the image files, or
    passed as an argument. If passed as an argument, the assumption is
    that all images were taken from the same height.  
  - it is presumed that images were taken at nadir (camera pointing
    straight down)

# Usage Overview

You always start with `uas_info()`, feeding it or more directories of
drone images. This function extracts the image metadata, computes
footprints, reads supplemental metadata, etc.

The object returned by `uas_info()` is not very useful by itself. The
results are generally saved to a variable then fed into other functions
that do useful things:

  - `uas_report()` creates ‘Flight Summaries’ as standalone HTML pages,
    with options to create image thumbnails. SAMPLE HERE

  - `uas_toc()` creates a Table of Contents page for several Flight
    Summaries, with options to copy all the catalog files to a single
    folder so the catalog is in one place.

  - `uas_exp_shp()` exports the centroids and footprints of individual
    images as Shapefiles, as well as the overall flight arrea.

  - `uas_worldfile()` creates small external files that allow images to
    be imported into GIS software and appear in their approximate
    footprint.

For more info, see the `uasimg` Vignette(s) XXXXXXXXXXXXXXXXX and
function help pages.

## Example

The general usage is to first create a metadata object for one or more
directories of images using the *uas\_info()* function, saving the
result to a variable.

``` r
library(uasimg)
hast_ft1_info <- uas_info("c:/Drone_Projects/Hastings/Flt01_1443_1446_250ft")
```

Once an image collection metadata object has been created, you can
generate outputs.

``` r
## Generate a flight summary
uas_report(hast_ft1_info)

## Export image centroid, footprints, and minimum convex polygon as Shapefiles
uas_exp_shp(hast_ft1_info)

## Generate estimated world files so the images can be imported into ArcGIS or QGIS
uas_worldfile(hast_ft1_info)
```

## World Files

**MOVE TO A VIGNETTE**

Drone images typically save the coordinates of the camera, but do not
include the width, length, or compass angle. A “world file” is a small
external file that contains these additional parameters, so that when
you import an individual image into a GIS program like ArcGIS or QGIS,
the image will appear in its approximate footprint on the ground.

You can create world files, readable by ArcGIS and QGIS, with
`uas_worldfile()`. `uas_worldfile()` can create three types of world
files, including `aux.xml`, `jpw` and `tfw`, and `prj` files. `aux.xml`
is the most recognized format and the default. See the function help
page for details.

# Bugs, Questions, and Feature Requests

To report a bug, add your camera to the package, or suggest a new
feature, please create an
[issue](https://github.com/ucanr-igis/uasimg/issues) on GitHub, or
contact the package author.
