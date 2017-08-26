#include <algorithm>
#include <iostream>
#include <stdlib.h>
#include <vector>

#include "parser.h"
#include "statistics.h"
#include "constants.h"


using namespace std;

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
