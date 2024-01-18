# google-location-history-converter

* Description
The Google Location History converter is a command line tool that converts the JSON that Google makes available via their Takeout interface, and converts it into KML that works with Exiftool to geolocate your images. If you have a higher end camera, it probably doesn't have a GPS built in, but you likely have your phone with you.

* Instructions
1. Enable Google Location History on your phone
2. Download your Google Location History from Google Takeout
    1. Go to https://takeout.google.com 
    2. Under "Create a new export" select the data you wish to include, in our case we want to click "Deselect All" and then find "Location History (Timeline)", and then select just that checkbox. Don't worry about "Multiple formats", it seems that Google only allows you to download JSON now.
    3. Once you have selected that checkbox, the "Next step" button should now be clickable, click the "Next stop" button.
    4. You will now be presented with the "2 Choose file type, frequency & destination" set of options. I leave destination as "Send download link via email", and Frequency as "Export once", I change "File type" to be .tgz and leave the "File size" alone at 50g.
    5. Click "Create export"
    6. Your export will now be in progress, and there will be a message at the bottom saying that Google is creating a copy of your data. You will now need to wait until Google notifties you that your data is ready.
    7. Wait for Google to create a copy of your data.
    8. When you receive the email from Google or you see that it is available, download it to your hard drive. Google will most likely ask you for your password, this is normal.
3. Convert your data using Google-Location-history-converter
    1. Locate your newly downloaded Google location data, it is usually in your browser's downloads folder. You will need to know the filepath to the data from the command line.
    2. Run the google-location-history-converter
        --inputFile => The input filename, this is the Google Location Data file, in compressed TGZ format
        --outputFile => This is where to output the KML file, if you omit this, it will send the data to stdout
        --filterMoreThanDays => This is how many days back from today to export, if you omit it, it will convert all your data, if you enter 15, it will give you 15 days back from today.
    ```bash
    glconverter --inputFile="<The filepath to the downloaded Google location data>" --outputFile "<the filename you want for the KML data, ending in .kml>" --filterMoreThanDays 15
    ```
    If you know that you will only have one Google Location history file in the directory, you can also convert it like this. Which gives you 30 days of location data:
    ```bash
    glconverter --inputFile=$(ls *.tgz) --outputFile ./locationHistory.kml --filterMoreThanDays=30 
    ```
    3. Your KML file is now in the file you specified.
4. Geolocate your images using Exiftool
    1. Locate your exported KML file, we will assume its here: ./locationHistory.kml
    2. Put your images in a directory, we will use the current directory "."
    4 Run Exiftool on your images
```bash
LOCATION_HISTORY_FILE_KML=./locationHistory.kml
IMAGE_FILE_DIRECTORY=.
exiftool -v5 -geotag "$LOCATION_HISTORY_FILE_KML" '-geotime<${DateTimeOriginal}-05:00' . -api GeoMaxIntSecs=1800 -overwrite_original_in_place $IMAGE_FILE_DIRECTORY
```
5. Optionally sort your images into directories based on when you took the photo
```bash
exiftool '-Directory<DateTimeOriginal' -d "$IMAGE_FILE_DIRECTORY/%Y-%m-%d" $IMAGE_FILE_DIRECTORY
```
