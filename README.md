

# google-location-history-converter

The Google Location History converter is a command line tool that converts a JSON output from Google Takeout and converts it into the KML format for use in tools like ExifTool which you can use to geolocate your images. If you have a higher end camera, it probably doesn't have a GPS built in, but you likely have your phone with you.

## Instructions
1. Enable Google Location History on your phone.
2. Download your Google Location History from Google Takeout:
    1. Go to https://takeout.google.com 
    2. 
	 - [ ] Under "Create a new export" select the data you wish to include,
       in our case we want to click "Deselect All" and then find
       "Location History (Timeline)", and then select just that
       checkbox.

![GoogleTakeout1](https://github.com/rickprice/google-location-history-converter/assets/1013199/5c57d3b1-2b94-4590-8a92-b9feb69e22b6).

 - [ ] Once you have selected that checkbox, the "Next step" button will be available at the bottom of the page. 
 
 - [ ] You will now be presented with some options - Choose file type, frequency & destination. I usually set this to "Export Once", ".tgz", and "50gb."

![GoogleTakeout2](https://github.com/rickprice/google-location-history-converter/assets/1013199/a7b36c1e-204b-48ab-938a-56c93ad08ba8)

 - [ ] Click "Create export"

Your export may take some time to finish. You will receive an email from Google when this export is complete and ready to be downloaded.
 2. When you receive the email from Google or you see that it is available. Download the file and remember the file path you used. Google will most likely ask you for your password, this is normal.
    

## Convert your data using Google-Location-History-Converter:

   1. Locate your newly downloaded Google location data. This will be the same path you chose earlier. Usually by default this will be your 
    2. Run the google-location-history-converter

   ```bash
    glconverter --inputFile="<The filepath to the downloaded Google location data>" --outputFile "<the filename you want for the KML data, ending in .kml>" --filterMoreThanDays 15
  ```

   `--inputFile=''` -  The input filename, this is the Google Location Data file, in compressed TGZ format
    `--outputFile`- This is where to output the KML file, if you omit this, it will send the data to stdout
    `--filterMoreThanDays`- This is how many days back from today to export. Running the command without this will process all available data. Use this flag alongside a whole number to limit the number of days you wish to export. For example, passing `--filterMoreThanDays 15` will return up to 15 days from today.

   If you know that you will only have one Google Location history file in the directory, you can also convert it like this. Which gives you 30 days of location data:
```bash
glconverter --inputFile=$(ls *.tgz) --outputFile ./locationHistory.kml -filterMoreThanDays=30
```
    
   3. After running, the output .KML file will be available at the location specified.

## Geolocating your images using ExifTool

 1. Locate your exported KML file and copy the file path. For our example, assume that it is at "./locationHistory.kml". 
 2. Put your images in a directory, In our case, we will use the current directory - "."
 3.  Run ExifTool on the images in your directory:
 
```bash
	LOCATION_HISTORY_FILE_KML=./locationHistory.kml
	IMAGE_FILE_DIRECTORY=.
	exiftool -v5 -geotag "$LOCATION_HISTORY_FILE_KML" '-geotime<${DateTimeOriginal}-05:00' . -api GeoMaxIntSecs=1800 -overwrite_original_in_place $IMAGE_FILE_DIRECTORY
```
You can also sort your photos by creating a new folder for each day they were taken:
```bash
exiftool '-Directory<DateTimeOriginal' -d "$IMAGE_FILE_DIRECTORY/%Y-%m-%d" $IMAGE_FILE_DIRECTORY
```

For addditional information on ExifTool, you can visit their website:
https://exiftool.org/#running
