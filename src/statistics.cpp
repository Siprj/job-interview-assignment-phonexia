#include "statistics.h"

#include <algorithm>
#include <iostream>

#include "constants.h"


using namespace std;

bool CallStatistics::operator == (const CallStatistics &b) const
{
    // floats shouldn't be compared like this bu for the sake of test...
    return
        ( this->totalCost == b.totalCost
        && this->internalCallTime == b.internalCallTime
        && this->internalCallCost == b.internalCallCost
        && this->externalCallTime == b.externalCallTime
        && this->externalCallCost == b.externalCallCost
        && this->internalSMSAmount == b.internalSMSAmount
        && this->internalSMSCost == b.internalSMSCost
        && this->externalSMSAmount == b.externalSMSAmount
        && this->externalSMSCost == b.externalSMSCost
        && this->calls == b.calls
        );
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
