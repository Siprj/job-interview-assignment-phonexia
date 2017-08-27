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

    cout << pretty(statistics);

    return 0;
}
