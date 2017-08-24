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

#define FEE_EXTERNAL_CALL (3.5 / 60.0)
#define FEE_EXTERNAL_SMS 2.0
#define FEE_INTERNAL_CALL (1.5 / 60.0)
#define FEE_INTERNAL_SMS 1.0
#define FREE_MINUTES 100
#define FREE_NUMBERS "+420732563345", "+420707325673"
#define FREE_SMSS 10
#define MONTHLY_FEE 900

enum ChannelType
    { Call
    , SMS
    };

enum DestinationType
    { Internal
    , External
    };

class Entry
{
public:
    string number;
    int duration;
    ChannelType channelType;
    DestinationType destinationType;
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
        calls = 0;
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
    int calls;
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

int rampSeconds(int secs)
{
    if (secs < 60)
    {
        return 60;
    }
    return secs;
}

CallStatistics collectStatistics(vector<Entry> data)
{
    CallStatistics statistics;
    int freeCallSeconds = FREE_MINUTES * 60;
    int freeSMSs = FREE_SMSS;

    for (auto item: data)
    {
        if (find(Free::numbers.begin(), Free::numbers.end(), item.number)
                != Free::numbers.end())
        {
            // Numbers free of charge
            cout << "Number: " << item.number << " duration: " << item.duration << endl;
            switch (item.channelType)
            {
            case Call:
                statistics.calls++;

                switch (item.destinationType)
                {
                case Internal:
                    statistics.internalCallTime += item.duration;
                    break;
                case External:
                    statistics.externalCallTime += item.duration;
                    break;
                }
                break;
            case SMS:
                switch (item.destinationType)
                {
                case Internal:
                    statistics.internalSMSAmount++;
                    break;
                case External:
                    statistics.externalSMSAmount++;
                    break;
                }
                break;
            }
        }
        else
        {
            switch (item.channelType)
            {
            case Call:
                statistics.calls++;

                if (freeCallSeconds > 0)
                {
                    freeCallSeconds -= item.duration;
                    int residualSeconds = 0 - freeCallSeconds;
                    if (residualSeconds > 0)
                    {
                        switch (item.destinationType)
                        {
                        case Internal:
                            statistics.internalCallCost +=
                                rampSeconds(residualSeconds) * FEE_INTERNAL_CALL;
                            break;
                        case External:
                            statistics.externalCallCost +=
                                rampSeconds(residualSeconds) * FEE_EXTERNAL_CALL;
                            break;
                        }
                    }

                    switch (item.destinationType)
                    {
                    case Internal:
                        statistics.internalCallTime += item.duration;
                        break;
                    case External:
                        statistics.externalCallTime += item.duration;
                        break;
                    }
                }
                else
                {
                    switch (item.destinationType)
                    {
                    case Internal:
                        statistics.internalCallTime += item.duration;
                        statistics.internalCallCost +=
                            rampSeconds(item.duration) * FEE_INTERNAL_CALL;
                        break;
                    case External:
                        statistics.externalCallTime += item.duration;
                        statistics.externalCallCost +=
                            rampSeconds(item.duration) * FEE_EXTERNAL_CALL;
                        break;
                    }
                }
                break;
            case SMS:
                if (freeSMSs > 0)
                {
                    freeSMSs--;

                    switch (item.destinationType)
                    {
                    case Internal:
                        statistics.internalSMSAmount++;
                        break;
                    case External:
                        statistics.externalSMSAmount++;
                        break;
                    }
                }
                else
                {
                    switch (item.destinationType)
                    {
                    case Internal:
                        statistics.internalSMSAmount++;
                        statistics.internalSMSCost += FEE_INTERNAL_SMS;
                        break;
                    case External:
                        statistics.externalSMSAmount++;
                        statistics.externalSMSCost += FEE_EXTERNAL_SMS;
                        break;
                    }
                }
                break;
            }
        }
    }

    statistics.totalCost = MONTHLY_FEE
        + statistics.internalCallCost
        + statistics.externalCallCost
        + statistics.internalSMSCost
        + statistics.externalSMSCost;

    return statistics;
}

int main()
{
    auto data = retrieveData("data/data.csv");
    auto statistics = collectStatistics(data);

    cout << "Monthly fee: " << MONTHLY_FEE << endl;
    cout << "Number of calls: " << statistics.calls << endl;
    cout << "Number of SMSs: " << statistics.internalSMSAmount
        + statistics.externalSMSAmount << endl;
    cout << "Total cost: " << statistics.totalCost << endl;

    cout << "Call cost: " << statistics.internalCallCost
        + statistics.externalCallCost << endl;
    cout << "SMS cost: " << statistics.internalSMSCost
        + statistics.externalSMSCost << endl;

    return 0;
}
