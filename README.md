
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

For more details and examples, see
<https://ucanr-igis.github.io/uasimg/>

# Drone Image Utilities

`uasimg` helps manage images taken from an unmanned aerial vehicle
(drone) that have been collected with the intent to stitch into 2D amd
3D products. The package does **not** stitch images, but helps you
create catalogs of your image data, examine their locations and
properties, export image centroids and estimated footprints as GIS
files, and create world files so individual images can be viewed in GIS
software.

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

3.  Creating individual Flight Summary pages in HTML, as the backbone of
    an image catalog.

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
## You can use the the following two commands to install RTools 
## (as an alternative to downloading and running the setup file yourself)

install.packages('installr')
installr::install.Rtools()
```

After RTools is installed, you can install `uasimg` with:

``` r
library (remotes)

## If the line above causes a 'package not found' error, then you need to install the
## remotes package (or devtools). You can run the command below (uncomment first) to install it, 
## or use the 'Packages' tab in RStudio.
## install.packages("remotes") 

remotes::install_github("ucanr-igis/uasimg")
```

### Dependent Packages

`uasimg` requires several dependent packages, including *sf*, *dplyr*,
*tidyr*, *leaflet*, *htmltools* and others (see the
[DESCRIPTION](https://github.com/UCANR-IGIS/uasimg/blob/master/DESCRIPTION)
file for a complete list). Normally missing packages are installed at
the same time, but if you get an error message about a missing
package(s), install the dependent packages separately (i.e., from the
‘Packages’ pane in RStudio) then run
`remotes::install_github("ucanr-igis/uasimg", dependencies=FALSE)`.

### exiftool

To read the EXIF data from the image files, `uasimg` requires an
external command line tool called ‘exiftool’. exiftool can be installed
in four steps:

1.  download the **Windows Executable** or **MacOS Package** from:
    <http://www.sno.phy.queensu.ca/~phil/exiftool/>

2.  uncompress / unzip

3.  Windows:

<!-- end list -->

  - rename the executable file from *exiftool(-k).exe* to *exiftool.exe*
    
    **Note**: if you have [file extensions
    hidden](https://support.winzip.com/hc/en-us/articles/115011457948-How-to-configure-Windows-to-show-file-extensions-and-hidden-files)
    in Windows Explorer, you won’t see *.exe* in the filename. In that
    case, just rename ‘*exiftool(-k)*’ to ‘*exiftool*’.

  - move the executable file to a directory on the path (e.g.,
    c:\\windows). Note: putting it in c:\\windows\\system32 does *not*
    seem to work.

## Supported Cameras

To see a list of known cameras (sensors), run `uas_cameras()` with no
arguments. If your camera is not listed, you may submit an
[issue](https://github.com/ucanr-igis/uasimg/issues) on GitHub to have
it added, or pass the camera parameters in as a csv file. For details
see the help page (`?uas_cameras`) or contact the package author.

## Data Requirements

Virtually all of the functions in `uasimg` use location data saved in
the EXIF data (header) of image files themselves. This assumes the
camera saves the location in the images using a GPS coordinate from the
drone or the camera itself. To compute footprints, the package also
needs to know the above-ground height at which images were taken. Some
drones (including many DJI drones) record the relative flight altitude
(above the launch point) in the image file, but others don’t (notably
many multispectral cameras). Flight height can also be entered manually
as an argument when you run `uas_info()`.

Requirements for `uasimg` therefore include:

  - images must have GPS coordinates saved in them  
  - image files should be grouped in folders (typically one flight per
    folder)
  - the camera must be one of the ones known by the `uasimg` package
    (see below)

Additional requirements to generate estimated footprints:

  - the height above ground level must be saved in the image files, or
    passed as an argument. If passed as an argument, the assumption is
    that all images were taken from the same height.  
  - it is presumed that images were taken at nadir (camera pointing
    straight down)

# Usage Overview

You always start with `uas_info()`, feeding it or more folders of drone
images. This function extracts image information, computes footprints,
reads supplemental flight metadata, etc.

The object returned by `uas_info()` is not very useful by itself. The
results are generally saved to a variable then fed into other functions
that do useful things, such as:

  - `uas_report()` creates ‘Flight Summaries’ as standalone HTML pages,
    with options to create image thumbnails
    ([sample](https://ucanr-igis.github.io/uasimg/samples/hrec/hrec_wtrshd2_2017_flt1_report.html)).

  - `uas_toc()` creates a Table of Contents page for several Flight
    Summaries, with options to copy all the catalog files to a single
    folder so the catalog is in one place
    ([sample](https://ucanr-igis.github.io/uasimg/samples/hrec/index.html)).

  - `uas_exp_shp()` and `uas_exp_kml()` exports flight geometries (image
    centroids, image footprints, and/or flight area) as Shapefiles or
    KML files. KML files can be imported into most flight planning
    software to refly the same area and/or plan a flight for an adjacent
    area.

  - `uas_worldfile()` creates small external XML files that allow images
    to be imported into GIS software and appear in their approximate
    footprint.

For more info, see the [Managing Drone Images with
uasimg](https://ucanr-igis.github.io/uasimg/articles/uasimg.html)
Vignette and function help pages.

## Example

The general usage is to first create a flight info object for one or
more directories of images using the *uas\_info()* function. Save the
result to a variable:

``` r
library(uasimg)
hast_ft1_info <- uas_info("c:/Drone_Projects/Hastingsx/Flt01_1443_1446_250ft")
```

If you want to record additional metadata about the flight as a whole,
you can create a metadata.txt file in the image folder with
`uas_metadata_make()`. Edit the metadata.txt file in Notepad (or another
text editor), save it, then run `uas_info()` again.

``` r
uas_metadata_make(hast_ft1_info, open = TRUE)
```

Once a flight info object has been created, you can start to generate
outputs.

``` r
## Generate a flight summary
uas_report(hast_ft1_info)

## Export image centroid, footprints, and minimum convex polygon as Shapefiles
uas_exp_shp(hast_ft1_info)

## Generate estimated world files so the images can be imported into ArcGIS or QGIS
uas_worldfile(hast_ft1_info)
```

# Utilities for Single Image Analysis

The following utilities can help visualize and analyze individual
images. These functions are based on the modeled image footprints, which
as described above require the camera to record the relative altitude
above ground.

## World Files

Drone images typically save the coordinates of the camera, but do not
include the width, length, or compass angle. A “world file” is a small
external text file for each images that contains these additional
parameters, so that when you import an individual image into a GIS
program like ArcGIS or QGIS, the image will appear in its approximate
footprint on the ground.

You can create world files, readable by ArcGIS and QGIS, with
`uas_worldfile()`. `uas_worldfile()` can create three types of world
files, including `aux.xml`, `jpw` and `tfw`, and `prj` files. `aux.xml`
is the most recognized format and hence the default. See the
`uas_worldfile()` help page for details.

## Cropping out the Cneter of Images

Sometimes images will simply not stitch, forcing you to do your analysis
and visualization with individual images. `uas_cropctr()` crops out the
center part of each image (which normally has the least amount of
distortion, at least if the image was taken at nadir). The function will
also produce a world file for the cropped center, so they can be
visualized together in a GIS software as a kind of crude mosaic.
`uas_cropctr()` provides arguments to specify how tall and wide to make
the crop, which you can set to the average forward distance between
images and the average side distance between flight lines. The resulting
mosaic will not be orthorectified, but may be good enough for
visualization and/or object detection particularly if the area is flat.

# Bugs, Questions, and Feature Requests

To report a bug, add your camera to the package, or suggest a new
feature, please create an
[issue](https://github.com/ucanr-igis/uasimg/issues) on GitHub, or
contact the package author.
