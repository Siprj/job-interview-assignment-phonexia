#include <algorithm>
#include <iostream>
#include <stdlib.h>
#include <vector>

#include "csv.h"


using namespace std;
using namespace io;

#define EXTERNAL_CALL_STR "VM"
#define EXTERNAL_SMS_STR "SM"
#define INTERNAL_CALL_STR "VS"
#define INTERNAL_SMS_STR "SS"

#define FEE_EXTERNAL_CALL 3.5
#define FEE_EXTERNAL_SMS 2.0
#define FEE_INTERNAL_CALL 1.5
#define FEE_INTERNAL_SMS 1.0
#define FREE_MINUTES 100
#define FREE_NUMBERS "+420732563345", "+420707325673"
#define FREE_SMS 10
#define MONTHLY_FEE 900

enum EntryType
    { InternalCall
    , ExternalCall
    , InternalSMS
    , ExternalSMS
    };

class Entry
{
public:
    string number;
    int duration;
    EntryType type;
};

class Free
{
public:
   static vector<string> numbers;
};

vector<string> Free::numbers = {FREE_NUMBERS};

class CallStatistics
{
public:
    CallStatistics()
    {
        totalCost = 0;
        internalCallTime = 0;
        internalCallCost = 0;
        externalCallTime = 0;
        externalCallCost = 0;
        internalSMSAmount = 0;
        internalSMSCost = 0;
        externalSMSAmount = 0;
        externalSMSCost = 0;
    }

    float totalCost;
    int internalCallTime;
    float internalCallCost;
    int externalCallTime;
    float externalCallCost;
    int internalSMSAmount;
    float internalSMSCost;
    int externalSMSAmount;
    float externalSMSCost;
};

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
            entry.type = InternalCall;
        }
        else if (stringifiedType.compare(EXTERNAL_CALL_STR))
        {
            entry.type = ExternalCall;
        }
        else if (stringifiedType.compare(INTERNAL_SMS_STR))
        {
            entry.type = InternalSMS;
        }
        else if (stringifiedType.compare(EXTERNAL_SMS_STR))
        {
            entry.type = ExternalCall;
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

CallStatistics collectStatistics(vector<Entry> data)
{
    CallStatistics stats;
    float freeMinutes = FREE_MINUTES;
    float freeSMS = FREE_SMS;

    for (auto item: data)
    {
        if (find(Free::numbers.begin(), Free::numbers.end(), item.number)
                != Free::numbers.end())
        {
            // Numbers free of charge
            cout << "Number: " << item.number << " duration: " << item.duration << endl;
        }
        else
        {
        }
    }
    return stats;
}

int main()
{
    auto data = retrieveData("data/data.csv");
    auto statistics = collectStatistics(data);

    return 0;
}
