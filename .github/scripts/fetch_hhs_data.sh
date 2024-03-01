#!/bin/bash
#-------------------------------------------------------------------------------
# Script     : fetch_hhs_data.sh.
# Description: Fetch data from healthdata.gov.
#              This script called within the automated-hhs-data-sync github
#              action. It is performed every Wednesday and Friday at 10am PST.
#-------------------------------------------------------------------------------

readonly STATUS_OK=0
readonly STATUS_ERROR=1
status=$STATUS_ERROR

if [ -d "data" ]; then
    DATA_DIR="data"
else
    echo "'data' directory does not exist"
    exit "$status"
fi

API="https://healthdata.gov/resource/g62h-syeh.csv";
COLS="date,state,previous_day_admission_influenza_confirmed,previous_day_admission_influenza_confirmed_coverage,previous_day_deaths_influenza,previous_day_deaths_influenza_coverage,previous_day_admission_adult_covid_confirmed,previous_day_admission_adult_covid_confirmed_coverage,previous_day_admission_pediatric_covid_confirmed,previous_day_admission_pediatric_covid_confirmed_coverage,deaths_covid,deaths_covid_coverage";
ORDER="date";
LIMIT="1000000";  # Must be set to some arbitrary *large* upper bound
API_QUERY="${API}?\$select=${COLS}&\$order=${ORDER}&\$limit=${LIMIT}";

time {
    echo
    echo "----------------------"
    echo "EXECUTING API REQUEST:"
    echo "----------------------"
    echo "$API_QUERY";
    echo

    headers="$DATA_DIR/headers.txt"
    response=$(curl -D "$headers"  "$API_QUERY");
    res_code=$(grep "^HTTP" "$headers" | awk '{print $2}')

    if [ "$res_code" -eq 200 ]; then
        # GET response headers contains a Last-Modified key-value pair that
        # corresponds to when the data returned was last updated (as reported
        # by healthdata.gov. This date must be reformatted to: YYmmddHHMMSS, so
        # as dataset metadata.
        timestamp=$(grep "^Last-Modified" "$headers" | awk -F ": " '{print $2}' | awk '{month_abv = $3; months = "JanFebMarAprMayJunJulAugSepOctNovDec"; month_num = (index(months, month_abv) + 2) / 3; printf "%s%02d%s%s", substr($4, 3, 2), month_num, $2, $5}' | tr -d ':')

        filename="$DATA_DIR/HHS_daily-hosp_state__$timestamp.csv"
        echo "$response" | awk '{gsub("T00:00:00.000", "", $1); print}' > "$filename"

        if [ -f "$filename" ]; then
            echo
            echo "Data Saved At: $filename"
            echo

            echo "-----------------------"
            echo "REMOVING EXISTING DATA:"
            echo "-----------------------"


            for file in data/*HHS_daily-hosp_state*; do
                if [[ $file != "$filename" ]]; then
                    rm $file
                    echo "Removing File: $file"
                fi
            done
        fi

        status=$STATUS_OK

    else
        echo
        echo "$response"
    fi

    echo
    echo "----------------------"
    echo "CLEANING UP TEMP FILES"
    echo "----------------------"

    if [ -f "$DATA_DIR/headers.txt" ]; then
        rm "$DATA_DIR/headers.txt"
        echo "Removing File: $DATA_DIR/headers.txt"
    fi

    echo
    echo "---------------------"
    echo "SCRIPT EXECUTION TIME"
    echo "---------------------"
}

exit "$status"

#for file in data/*HHS_daily-hosp_state__*; do
#    date_raw=$(echo "$file" | grep -oE '[0-9]{12}')
#    date_fmt="20${date_raw:4:2}-${date_raw:2:2}-${date_raw:0:2} ${date_raw:6:2}:${date_raw:8:2}:${date_raw:10:2}"
#    file_date=$(date -j -f "%Y-%m-%d %H:%M:%S" "$date_fmt" +"%s")
#    echo $date_fmt
#    echo $file_date
#    # You can perform operations on each file here
#done
