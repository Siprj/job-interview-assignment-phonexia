#include <algorithm>
#include <iostream>
#include <stdlib.h>
#include <vector>

#include "parser.h"
#include "statistics.h"
#include "constants.h"
#include "cmdline.h"


using namespace std;
using namespace cmdline;

int main(int argc, char *argv[])
{
    parser cmd;
    cmd.add<string>("file", 'f', "Path to data file", true, "");

    cmd.parse_check(argc, argv);


    auto data = retrieveData(cmd.get<string>("file").c_str());
    auto statistics = collectStatistics(data);

    cout << pretty(statistics);

    return 0;
}
