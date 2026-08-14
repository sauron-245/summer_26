INSTRUCTIONS -- combine_log_df.R

this is used to update HOBO .csv files. Because we’re working with data from multiple HOBOs at several sites (and because the unformatted data is super messy),
I decided it would be best to maintain ‘running’ logs of all available data, then append new data onto those running logs. To use this script:

- Download new .xlsx hobo files

- Drop new .xlsx files downloaded from HOBOs into folder hobos/hobos_upload

- Rename these files so the script can access them

- In body of script, add all filenames in 'hobos/hobos_upload' to the 'hobos_week' vector in quotation marks (e.g. hobos_week = c("P1", "P3", "P5_creek")). If you're
  updating data for all monitors, change both 'for' loops at the bottom of the script to iterate over 'hobos_all' instead of 'hobos_week'.

- Source the code, let it format and append the new data, then check that everything came out correctly by looking at the graphs it spits out at the end.

All the instructions and code to edit are highlighted in the script outline! This should be used every time anyone downloads data from the hobos.
The script drops the formatted files into a folder called ‘hobos_running’, from where they can be exported/referenced/etc. I’ve been copying all the formatted files
into a data backup folder stored next door in case anything goes wrong with the data for whatever reason.

Hobo files should be named using the following conventions:
- Piezometers: 'P1', 'P2', 'P3', 'P4', and 'P5'
- Creek temperature sensors: 'P1_creek', 'P2_creek', 'P4_creek,' and 'P5_creek'
- Creek conductivity sensors: 'Dwnstrm' (downstream of P1), 'Mdstrm' (downstream of P3), 'Spring' (in the spring), and 'P5_creek_cond' (in the creek next to P5)


INSTRUCTIONS -- xle_handling.R
This is used to parse the .xle files produced by the Solinst leveloggers and barologgers. Not totally sure if it’s strictly necessary to download these files in .xle format
rather than .csv, but this script does a nice job of cleaning up some standing issues in these data. 

What it does: 

- Parses .xle files as if they were .xmls and stitches them into usable CSVs

- Runs a Hampel filter (moving window to detect observations outside a certain deviation) to clean up massive outliers where the probe recorded a pressure reading while removed from the water 

- Applies a correction to address an ongoing issue where the probe can be reinserted into a different place in the water column, usually due to the wire it hangs off of getting knotted/snagged.
  Because it calculates water level using pressure, the leveloggers have to dangle at the same depth to get consistent readings, and will run into off-by-a-constant errors when inserted at the wrong depth. This script corrects this by identifying times when there’s a jump in water level larger than could realistically occur between measurements (say, 20 cm), storing that difference, then applying it as a correction to all following values until it encounters another large jump. 

To use it:

- Download new .xle files

- Drop new .xle files into ‘dtw_upload’ and rename them as ‘P1_baro’ for the barologger, and ‘Bridge’, ‘P1’, and ‘P5’ as appropriate for leveloggers

- Open up the script and source it

- Find updated and formatted .csv files in ‘dtw_formatted’


