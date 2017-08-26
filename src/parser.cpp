#include "parser.h"

#include <iostream>

#include "constants.h"
#include "csv.h"


using namespace std;
using namespace io;

vector<Entry> retrieveData(const char* filePath)
{
    // Instantiate CSV reader with three active columns and
    CSVReader
        < 3
        , trim_chars<' ', '\t'>
        , no_quote_escape<';'>
        > inputData(filePath);

    try {
        inputData.read_header
            ( ignore_extra_column
            , "number"
            , "duration"
            , "type"
            );
    }
    catch(const exception& e)
    {
        // Here should be better error handling in case of bigger app.
        cerr << e.what();
        exit(EXIT_FAILURE);
    }

    vector<Entry> data;

    string number;
    int duration;
    string stringifiedType;
    while(inputData.read_row(number, duration, stringifiedType))
    {
        Entry entry;
        entry.number = number;
        entry.duration = duration;

        if (stringifiedType.compare(INTERNAL_CALL_STR))
        {
            entry.channelType = Call;
            entry.destinationType = Internal;
        }
        else if (stringifiedType.compare(EXTERNAL_CALL_STR))
        {
            entry.channelType = Call;
            entry.destinationType = External;
        }
        else if (stringifiedType.compare(INTERNAL_SMS_STR))
        {
            entry.channelType = SMS;
            entry.destinationType = Internal;
        }
        else if (stringifiedType.compare(EXTERNAL_SMS_STR))
        {
            entry.channelType = SMS;
            entry.destinationType = External;
        }
        else
        {
            // Here should be better error handling in case of bigger app.
            cerr << "Unrecognized entry type: " << stringifiedType << endl;
            exit(EXIT_FAILURE);
        }

        data.push_back(entry);
    }

    return data;
}
