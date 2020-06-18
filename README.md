
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Drone Image Utilities

`uasimg` helps manage images taken from an Unmanned Aerial System (i.e.,
drone) that have been taken with the intent to stitch together to make
high resoultion orthomosaics. The package does not stitch images, but
helps you create catalogs of your images, visulize their locations,
export image centroids and estmiated footprints as GIS files, and create
world files so images can be viewed in GIS software.

##### Applications

`uasimg` was designed to help in the following applications:

1.  Doing a quick check in the field to review the distribution of a set
    of images, and the estimated image overlap. This (along with
    checking the images for bluriness) can help a pilot determine if a
    flight was successful or needs to be redone.

2.  Subsetting images for further processing with a photogrammetry
    (stitching) program like Pix4D or Agisoft. Omitting images with an
    extreme amount of overlap can improve results and reduce processing
    time.

3.  Creating HTML reports for individual flights, to serve as the pages
    for a catalog of drone images.

4.  Creating auxillary world files to view individual images in GIS
    software.

**Note**: image locations and footprints are based on the metadata saved
in the image files (e.g., height above the launch point, compass
direction of the camera). Thus they should considered as *estimates
only*.

##### Data Requirements

The package uses image location data saved in the EXIF data (header) of
image files themselves. This will only work if the camera on the drone
saved the GPS coordinates in the image files. To compute footprints, the
package also needs to know the height at which images were taken. Some
drones (including many DJI drones) save the relative flight altitude
(above the launch point) in the image file. Flight height can also be
entered manually as an argument.

Requirements for using the package include:

  - the images must have been taken by a camera that saved the GPS
    coordinates
  - the images to be analyzed should all be in one folder (preferably
    containing one flight only)
  - the camera model must be one of the ones known by the package (see
    below)
  - the height above ground level must be saved in the image files, or
    passed as an argument. If passed as an argument, the assumption is
    that all images were taken from the same height.
  - images were taken at nadir (camera pointing straight down)

##### Accuracy of the Estimated GSD and Image Footprints

The computed image footprints and GSD are based on the recorded height
above ground level (usually taken to be the launch point). If the study
area was flat and the flight lines were all at the same elevation, using
the RelativeAltitude from the image headers, or passing the flight
height as an argument, should be relatively accurate. In hilly areas or
uneven flights, image footprints and GSD will be under-estimated (i.e.,
smaller than reality) whereever the distance between the drone and the
ground was actually greater than the recorded flight height.

## Installation

This package is not yet on CRAN, but you can install it from GitHub.

Note: if you’re using a Windows machine, you must have RTools installed
to build packages from source files (which is what you do when you
install from GitHub). RTools is not a R package, rather its a set of
utilities. You can download the setup file from
<https://cran.r-project.org/bin/windows/Rtools/> OR install it from
within R by running:

``` r
install.packages('installr')
installr::install.Rtools()
```

With RTools installed, you can install `uasimg` with a function from the
devtools package.

``` r
install.packages("devtools") 
devtools::install_github("ucanr-igis/uasimg")
```

If you get an error message about dependent packages not being
available, see the note about dependencies below.

### Dependencies

The package requires the *dplyr*, *sf*, and *leaflet* packages. If you
get an error message when installing *uasimg*, install the dependent
packages separately (i.e., from the ‘Packages’ pane in RStudio). Then
run `install_github("ucanr-igis/uasimg", dependencies=FALSE)`.

### Exiftool

To read the EXIF data from the image files, the package requires a free
command line tool called ’exiftool. This can be installed in three
steps:

1.  download the file from
    <http://www.sno.phy.queensu.ca/~phil/exiftool/>
2.  uncompress / unzip
3.  rename the executable file from *exiftool(-k).exe* to
    *exiftool.exe*  
    Note: if you have file extensions hidden in Windows Explorer, you
    won’t see .exe in the filename. Ub that case, just rename
    ‘exiftool(-k)’ to ‘exiftool’.
4.  move the executable file to a directory on the path (e.g.,
    c:\\windows). Note: putting it in c:\\windows\\system32 does *not*
    seem to work.

## Usage

To see a list of known cameras (sensors), run `cameras()` with no
arguments. If your camera is not listed, see the help page (`?cameras`)
or contact the package author.

There are three main functions you’ll use to manage your image data:

`uas_info()` returns a ‘metadata object’ for one or more directories of
images. You always start with this.

`uas_report()` takes a metadata object and generates a HTML report(s).

`uas_exp()` takes a metadata object and exports the image centroids and
footprints as Shapefiles.

For more info about arguments and options for each function, see their
help pages.

**Example**

The general usage is to first create a metadata object for one or more
directories of images using the *uas\_info()* function.

``` r
library(uasimg)
mydir <- "c:/Drone_Projects/Hastings/Flt01_1443_1446_250ft"
file.exists(mydir)
uas_img_metadata <- uas_info(mydir)
summary(uas_img_metadata)
```

Once a metadata object has been created, you can then generate outputs.

``` r
## Generate an HTML report of the images in the catalog
uas_report(uas_img_metadata)

## Export image centroid, footprints, and minimum convex polygon
uas_exp(uas_img_metadata)

## Generate estimated world files so the images can be imported into ArcGIS or QGIS
uas_worldfile(uas_img_metadata)
```

## HTML Catalog

The HTML catalog created by `uas_report()` includes an embedded
interactive leaflet map
([sample](https://ucanr-igis.github.io/webassets/hrec_watershed1_rpt.html)).
There is also an option to generate a PNG thumbnail. The PNG thumbnail
however requires passing a Google Maps API key. See the help page for
details.

## Questions, Bug Reports, and Feature Requests

Please start a new issue on GitHub, or contact the package author.
