# uasimg 1.9.4 (2025-01-13)

* `uas_info()`: added `pattern` argument to provide more options for specifying images to read in directories with data from mixed sensors; fixed a bug with the error trapping; updated help page.

# uasimg 1.9.3 (2025-01-11)

Note: there's a slight chance this update could break existing code, because the `ext` argument in `uas_info()` now has a non-NULL default value. Moving forward, it is recommended to always specify the image file name extensions using `ext`.

* `uas_toc()`: fixed an error that occurred if flight summaries were in different UTM zones (#13)
* `uas_report()`: fixed a dependency bug in when `tbm_use = TRUE`
* `uas_info()`: make the `ext` argument a vector, changed the default value to common image extensions, updated checks for files with missing EXIF data (#12), added a check for multiple sensor types in one directory
* `ggplot2` added to Suggests (used when creating whole flight thumbnails)

# uasimg 1.9.2 (2024-09-13)

* added the XT2 RGB image to the camera database

# uasimg 1.9.1 (2024-08-05)

* `uas_report()`: fixed a bug
* `uas_info()`: trapped errors when Make and Model tags are blank, or if there not enough images to compute a MCP

# uasimg 1.9.0 (2024-08-01)

This is a fairly significant update. Key changes include:

1. rotated thumbnail images in flight summary reports
2. The ability to export individual images as pseudo-rectified GeoTIFFs
3. Repairing the ability to download thumbnail images of flights from Google Maps or Stadia Maps (via ggmap) for flight summary reports, and several changes to the arguments in `uas_report()` that control this.

To use rotated thumbnail images for the flight summary reports, you must create the thumbnail images using `uas_thumbnails_make()` (and set the `rotate` argument to `TRUE`). If you don't do this but ask for thumbnail images to be included in a flight summary report, `uas_report()` will still create thumbnail images on-the-fly, but there isn't a rotation option (and in a future update `uas_report()` will no longer generate thumbnails on-the-fly).

Changes were also made to several arguments in `uas_report()` that control downloading a thumbnail image for the entire flight (aka thumbnail map). See the help page for the new arguments (`pts_col`, `tbm_use`, `tbm_overwrite`, `tbm_size`, `tbm_src`, `tbm_api_key`, and `tbm_exp`) and deprecated arguments (`png_map`, `overwrite_png`, `png_exp`, `google_api`, and `col`). Some of these are simply name changes, while others are new or have different behavior. In a future update, downloading thumbnail images of the flight area will be moved to a separate function.

Along the same lines, both Google Maps and Stadia Maps (which replaced the defunct Stamen Maps API) require passing an API key (see the `uas_report()` help page for info about how to get an API key) . Your preferred service should be passed as `tbm_src`, and your API key must be passed as `tbm_api_key`. If you use Google Maps, you'll get a satellite image, while Stadia Maps will download a terrain image (similar to Stamen). Thumbnail maps are still downloaded using `ggmap`, however you can no longer 'preload' your API key in `.Renviron`; you must pass it in explicitly with `tbm_api_key`. If this present problems contact the package author.

Other changes include:

* `uas_exp_geotiff()`: new function to create a pseudo-georectified image (using the modeled footprint) as a GeoTIFF (#7)
* `uas_thumbnails_make()`: new argument `rotated` will create rotated thumbnails (FALSE by default for backward compatibility)
* `uas_worldfile()`: added new argument `flt` for the flight index (because a uas_info object can contain metadata for multiple flights / image collections). Improved error messages.
* `terra` added as a suggested dependency (to support the new `uas_exp_geotiff()` function)
* `magick` moved from imports to suggests
* `uas_convert()`: checks added to see if `magick` is installed
* `leaflet.extras` added to imports (to add a full screen control to the leaflet map on flight summary reports)
* `lifecycle` package added to imports (to communicate renamed arguments)  
* `uas_exp_kml()`, `uas_exp_shp()`, `uas_thumbnails_make()`, `uas_move()`, `uas_rename()`, `uas_report()`: argument `flt` added for consistency across functions; `flt_idx` deprecated. Improved error messages (#6)  
* `uas_report()`: `units` argument added to set whether to use imperial or metrics units for the flight area, altitude AGL, and GSD (#11). Several arguments related to downloading thumbnail images of flight areas have been deprecated, renamed, or added, for consistency and to deal with changes in ggmap.  
* `uas_report.Rmd`: modified so the thumbnail image height is automatically set by the browser (to accommodate thumbnail images with different aspect ratios, including rotated images); added a button to display the map full screen; added code to support `units` argument
* `uas_info()`: the cache is auto-refreshed if `fp=TRUE` and `cache=TRUE` but the cache doesn't contain footprints (#6); added an extra sort by datetimeoriginal to deal with the odd case where sorting images by their filenames doesn't result in a sequential order (#9). 
* `cameras.csv`: added DJI Zenmuse P1 (#8), Autel Evo2 RGB
* `uas_cameras()`: updated to import and view the flight-yaw tag column

# uasimg 1.8.3 (2022-11-11)

* `uas_worldfile()`: updated function documentation (#6)

# uasimg 1.8.2 (2022-11-08)

* cameras.csv: updated EXIF fields for Phantom 4 MS RGB (added tag_elev_agl) (#6)
* `uas_info()`: updated status message (to include the optional ext argument)

# uasimg 1.8.1 (2022-04-12)

* `uas_rename()`: new function to rename images based on a name template with placeholders for image and flight metadata
* `uas_info()`: updated documentation
* `uas_metadata_make()`: updated default flight metadata template
* `uas_report.Rmd`: changed the default Window title to the short version of the flight name 

# uasimg 1.8.0 (2022-03-20)

* `uas_info()`: argument `ext` added (filter by file type); minor code cleaning, 
* cameras.csv: Phantom 4 Pro RTK and Phantom 4 Multispectral added

# uasimg 1.7.9 (2021-11-11)

* `uas_info()`: added argument `path2name_fun`; modified to support new `date_time` column in cameras.csv
* `uas_path2name_fun()`: new function that returns a function to parse a directory path and concatenate user-specified elements to form a flight name; designed to be passed as the value of the `path2name_fun` argument in
`uas_info()` to construct default flight names
* `uas_report()` and `uas_toc()`: arguments `header_html` and `footer_html` can now accept URLs
* `magrittr` package added to imports (needed to use the results of `uas_path2name_fun()`)
* `cameras.csv` added `date_time` column (to accommodate the cameras like the Airphen that don't have a DateTimeOrginal EXIF tag)
* `uas_readcameras()` added `date_time` column to import

# uasimg 1.7.8 (2021-10-25)

* `uas_info()`: fix a bug that occurred when gpslatitude and gpslongitude were not found in EXIF data

# uasimg 1.7.7 (2021-09-15)

* added Airphen to the camera database
* `uas_info()`: modified such that a generic camera is used if the camera is not detected

# uasimg 1.7.6 (2021-07-31)

* added MicaSense Altum and MicaSence RedEdge-MX to the camera database

# uasimg 1.7.5 (2021-07-28)

* added Phantom 4 Pro V2 (FC6310S) to the camera database

# uasimg 1.7.4 (2021-06-10)

* `uas_toc()`: fixed a bug when output_dir = ".", gather_dir = NULL, and input reports are on a different volume

# uasimg 1.7.3 (2021-05-23)

* `uas_report.Rmd`: fixed a bug when `group_img = FALSE`

# uasimg 1.7.2 (2021-05-14)

* cameras.csv: added Mavic Air 2 (precision dimensions of sensor not available, hence footprints should be taken as estimates only)
* `uas_move()`: renamed `outdir_root` to `outdir_base`.
* updates to Readme and vignettes

# uasimg 1.7.1 (2021-05-10)

* `uas_report()`: fixed a bug parsing the results of `uas_thumbnails_make()`
* `uas_report.Rmd`: added default value for parameter nomap

# uasimg 1.7.0 (2021-05-09)

NOTE: AFTER INSTALLING THIS UPDATE YOU SHOULD RUN `uas_clearcache()`

* `uas_toc()`: added uas_report.css to the files that get gathered 
* `stringr` added to imports (i.e. required)
* cameras.csv and `uas_cameras()`: added `camera_abbrev` columns
* `uas_readcameras`: added `camera_abbrev` 
* `uas_info()`: updated to find exiftool executable installed by exiftoolr; `camera_name` and `camera_abbrev` added as an attribute fields to accommodate multi-sensor flights
* `print.uas_info()` and `report.Rmd`: updated to report the number of images per camera for multi-sensor flights
* `uas_report()`: changed default filename suffix to `_rpt`; updated for multi-folder flights
* `uas_move()`: overhauled
* `uas_worldfile()`, `uas_cropctr()`: updated for multi-folder flights
* `uas_exp_shp()`, `uas_exp_kml()`: updated for multi-folder flights; argument `img_dir` replaced with `flt_idx`
* `uas_thumbnails_make()`: argument `img_dir` replaced with `flt_idx`
* hex logo added

# uasimg 1.6.9 (2021-05-05)

* `uas_dirs_find()`: fixed a case where images in tb folders were not excluded 

# uasimg 1.6.8 (2021-05-05)

* `uas_dirs_find()`: path column in the tibble is now wrapped in `normalizePath()` (primarily for readability with network paths)
* `uas_report()`: returned HTML file names are now wrapped in `normalizePath()`; `show_gps_coord` deprecated
* `uas_toc()`: fixed an error where the libs subfolder was not being gathered
* `uas_toc.Rmd`: added error trapping code if metadata fields are missing
* `uas_report.Rmd`: modified the balloon text to show GPS coords

# uasimg 1.6.7 (2021-05-04)

* `uas_thumbnails_make()`: modified to automatically switch to the magick package functions (with a message) if thumbnails from TIFs are being created *and* `imager:::has.magick()` is FALSE.

# uasimg 1.6.6 (2021-05-03)

* `uas_thumbnails_make()`: enabled `use_magick` argument

# uasimg 1.6.5 (2021-05-02)

* `uas_report()`: added checks for directory existence and access
* `uas_metadata_make()`: argument `md_template` can now be a URL
* added Flight Metadata Vignette

# uasimg 1.6.4 (2021-05-01)

* `uas_dirs_find()`: new function to find all sub-directories with images
* `uas_thumbnails_make()`: added `stats` argument to display the amount of time taken per thumbnail
* `uas_report()`: added `use_tmpdir` argument to render the flight summary report in temp folder (can prevent Pandoc errors when images are on a network drive)

# uasimg 1.6.3 (2021-04-30)

* `uas_cropctr()`: removed a leftover debugging statement

# uasimg 1.6.2 (2021-04-28)

* `uas_metadata_make()`: added `use_system_editor` argument
* `uas_report()`: added `header_html` and `footer_html` arguments

# uasimg 1.6.1 (2021-04-26)

* `uas_metadata_make()`: appended `file:` to metadata files on network paths to get it to open in notepad; added more comments in template

# uasimg 1.6.0 (2021-04-24)

* `uas_convert()`: this new function converts images between file formats (e.g., DNG to JPG), while preserving EXIF data  
* `magick` and `exiftoolr` added as dependent packages (as imports)
* `uas_info()`: added argument for `exiftoolr_use`

# uasimg 1.5.2 (2020-12-02)

* `uas_getoemflds()`: added 'uav' to the list of default flight metadata fields
* updated camera database:  (added senseFly Aeria X and DJI M100 X3Z)
* `uas_report()`, `uas_exp_kml()`, and `uas_exp_shp()`: added checks after dir.create()

# uasimg 1.5.1 (2020-11-14)

* `uas_metadata_make()`: additional comments inserted when creating a brand new new metadata file 
* `uas_getflds()`: exported
* `uas_thumbnails_make()`: increased the number of bytes read to create a unique filename from 500 to 2000
* `uas_info()`: updated to always rename yaw field even when fp = FALSE
* `uas_clearcache()`: created

# uasimg 1.5.0 (2020-11-01)

* `uas_report()` and `uas_report.Rmd`: arguments `report_title` and `kml_ctr` added; trapped an error if metadata exists but without an image collection name; thumbnails disabled for grouped images (until a better visualization is implemented); additional <span id="..."> tags added to facilitate web scraping
* `uas_info()`: deafult value of `fp` changed to `FALSE`; default value of `cache` changed to `TRUE`; a flight id value saved in return value (used primarily to name files if metadata$name_short is missing)
* `uas_flds_oem()`: `name_short` and `notes` added to default fields; `collection_name` removed
* `uas_toc()` and `uas_toc.Rmd`: check that html files exist added; all code migrated from XML to xml2
* `uas_exp()` renamed (back to) `uas_exp_shp()`;
* `uas_exp_kml()` created
* `uas_thumbnails_make()` created
* *xml2*, *tibble* and *imager* added to Imports (required packages); *XML* and *magick* package removed

# uasimg 1.4.0 (2020-10-01)

* `uas_toc()` and `uas_toc.Rmd` updated - new `summary_map` argument displays an interactive mission footprint map
* `uas_report.Rmd` updated - flight MCP converted to base64 and encoded in a meta tag
* `uas_report.R` updated - argument `local_dir` renamed `show_local_dir`; `gps_coord` renamed `show_gps_coord`
* `uas_exp()` renamed `uas_exp_shp()`

# uasimg 1.3.8 (2020-09-28)

* added `kml_mcp` to `uas_report()`

# uasimg 1.3.7 (2020-09-27)

* improved help page for `uas_info()` and `uas_cameras()`
* added `name_only` argument to `uas_cameras()`
* renamed argument `csv` to `exif_csv` in `uas_info()`
* added argument `gps_coord` to `uas_report()` & modified `uas_report.Rmd` to show the GPS coordinates in the pop-up windows

# uasimg 1.3.6 (2020-09-26)

* Added DJI ZenMuse X7 to camera database
* `uas_info()` edited to i) check the number of images in the directory before computing forward overlap, ii) add bullet characters to feedback messages

# uasimg 1.3.5 (2020-09-24)

* Added a `NEWS.md` file to track changes to the package.  
* `geo2utm()` modified to return a EPSG number (to comply with recent changes to rgdal and proj4). After installing this update, users are advised to delete their old cache (see `uas_getcache()`) to prevent warning messages about unknown datums in a proj4string.
* `uas_cropctr()` added - crops around the center of each image for a better photo mosaic

# Changes in version 1.3.4 (2020-08-25)

* `uas_report()` modified to return the file name of HTML files that already exist and overwrite = FALSE; corrected the extension for tif thumbnail files.  
* Added `uas_metadata_make()`.  
* Modified `uas_toc()` and `uas_toc.Rmd` to support `gather = '.'`, handle duplicate HTML names, a description, and default footer.  
* Corrected a bug in `uas_info()` when no external metadata was found or passed as an argument. 
* Other minor fixes and enhancements.  

# Changes in version 1.3.3 (2020-08-20)

* Added an option in `uas_report()` to create thumbnails previews that appear in the popup windows of the image collection catalog.  
* Added `combine_dirs` argument to `uas_grp_flt()` to support multi-directory flight parsing.  
* Added custom print functions for uas_info and uas_grp objects.  
* Numerous small enhacements and bug fixes.  

# Changes in version 1.3.2 (2020-08-08)

* `uas_info(()` modified to support file names metadata.txt expanded to metadata*.txt.  

# Changes in version 1.3.1 (2020-08-07)

* Renamed `uas_getcache()` and `uas_setcache()`.  
* Added `uas_grp_flt()` and `uas_grps2col()`.  
* Added `uas_getflds()` and `uas_setflds()`.  
* Added Mavic 2 Pro to camera database.  
* renamed `meta_extra` argument to `metadata` in `uas_info()`.  

# Changes in version 1.3 (2020-07-04)

* Added `uas_getcachedir()` and `uas_setcachedir()` to manage cache directory for EXIF data.  
* Renamed cache arguments on `uas_info()` to `cache` and `update_cache`.  
* Added `uas_toc()`.  

# Changes in version 1.2.7 (2020-06-30)

* Modified `uas_report()` and uas_report.Rmd, metadata now encoded in HTML.  
* Argument `toc_csv` deprecated.  
* `summary.uas_info()` replaced with `print.uas_info()` (generic print method).  

# Changes in version 1.2.6 (2020-06-20)

* Added 'description' element to `meta_extra` argument to `uas_info()`.  
* Added `cache_dir` and `cache_update` to `uas_info()`.    
* Split `overwrite` argument in `uas_report()` into `overwrite_html` and `overwrite_png`.    

# Changes in version 1.2.5 (2020-06-14)

* Added `meta_extra` argument to `uas_info()`.    
* Added `toc_csv` argument to `uas_report()`.    

# Changes in version 1.2.4 (2020-04-03)

* Initial release of `uasimg`  
* Key differences from older `uavimg`:  
    * All prefixes changed from `uavimg_` to `uas_`
    * Converted all functions to the `sf` package
    * Added `overwrite` argument to `uas_exp()` and `uas_report()`
    * Added support for paths with spaces: `uas_info()`
    * Added `sf`, `tidyr` and `crayon` packages as dependencies
    * Fixed a bug in `findonpath()` when path values had trailing slashes
