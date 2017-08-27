#pragma once

#include <vector>

#include "parser.h"


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

    bool operator == (const CallStatistics &b) const;
};

extern CallStatistics collectStatistics(std::vector<Entry> data);
