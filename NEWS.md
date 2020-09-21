# uasimg 1.3.5 (2020-09-xx)

* Added a `NEWS.md` file to track changes to the package.  
* `geo2utm()` modified to return a EPSG number (to comply with recent changes to rgdal and proj4). After installing this update, users are advised to delete their old cache (see `uas_getcache()`) to prevent warning messages about unknown datums in a proj4string.

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
