#include <vector>
#include <sstream>

#include "catch.hpp"

#include "statistics.h"

namespace Catch {
    template<> std::string toString<CallStatistics>(CallStatistics const& value)
    {
        std::ostringstream oss;
        oss << "totalCost: " << value.totalCost << std::endl;
        oss << "internalCallTime: " << value.internalCallTime << std::endl;
        oss << "internalCallCost: " << value.internalCallCost << std::endl;
        oss << "externalCallTime: " << value.externalCallTime << std::endl;
        oss << "externalCallCost: " << value.externalCallCost << std::endl;
        oss << "internalSMSAmount: " << value.internalSMSAmount << std::endl;
        oss << "internalSMSCost: " << value.internalSMSCost << std::endl;
        oss << "externalSMSAmount: " << value.externalSMSAmount << std::endl;
        oss << "externalSMSCost: " << value.externalSMSCost << std::endl;
        oss << "calls: " << value.calls << std::endl;
        return oss.str();
    }
}

TEST_CASE("Statistics") {
    SECTION("Internal SMS")
    {
        // Test data
        std::vector<Entry> data =
            { Entry("+420123456789", 0, SMS, Internal)
            , Entry("+420732563345", 0, SMS, Internal)
            , Entry("+420123456789", 0, SMS, Internal)
            , Entry("+420123456789", 0, SMS, Internal)
            , Entry("+420123456789", 0, SMS, Internal)
            , Entry("+420123456789", 0, SMS, Internal)
            , Entry("+420123456789", 0, SMS, Internal)
            , Entry("+420123456789", 0, SMS, Internal)
            , Entry("+420123456789", 0, SMS, Internal)
            , Entry("+420123456789", 0, SMS, Internal)
            , Entry("+420123456782", 0, SMS, Internal)
            , Entry("+420123456784", 0, SMS, Internal)
            };

        // Result
        CallStatistics stats = CallStatistics();
        stats.totalCost = 901;
        stats.internalCallTime = 0;
        stats.internalCallCost = 0;
        stats.externalCallTime = 0;
        stats.externalCallCost = 0;
        stats.internalSMSAmount = 12;
        stats.internalSMSCost = 1;
        stats.externalSMSAmount = 0;
        stats.externalSMSCost = 0;
        stats.calls = 0;

        REQUIRE(collectStatistics(data) == stats);
    }
    SECTION("External SMS")
    {
        // Test data
        std::vector<Entry> data =
            { Entry("+420123456789", 0, SMS, External)
            , Entry("+420123456789", 0, SMS, External)
            , Entry("+420123456789", 0, SMS, External)
            , Entry("+420123456789", 0, SMS, External)
            , Entry("+420123456789", 0, SMS, External)
            , Entry("+420123456789", 0, SMS, External)
            , Entry("+420123456789", 0, SMS, External)
            , Entry("+420123456789", 0, SMS, External)
            , Entry("+420123456789", 0, SMS, External)
            , Entry("+420123456789", 0, SMS, External)
            , Entry("+420123456789", 0, SMS, External)
            , Entry("+420123456789", 0, SMS, External)
            , Entry("+420732563345", 0, SMS, External)
            };

        // Result
        CallStatistics stats = CallStatistics();
        stats.totalCost = 904;
        stats.internalCallTime = 0;
        stats.internalCallCost = 0;
        stats.externalCallTime = 0;
        stats.externalCallCost = 0;
        stats.internalSMSAmount = 0;
        stats.internalSMSCost = 0;
        stats.externalSMSAmount = 13;
        stats.externalSMSCost = 4;
        stats.calls = 0;

        REQUIRE(collectStatistics(data) == stats);
    }
    SECTION("Internal Calls")
    {
        // Test data
        std::vector<Entry> data =
            { Entry("+420123456789", 3000, Call, Internal)
            , Entry("+420123456789", 3000, Call, Internal)
            , Entry("+420732563345", 300, Call, Internal)
            };

        // Result
        CallStatistics stats = CallStatistics();
        stats.totalCost = 900;
        stats.internalCallTime = 6300;
        stats.internalCallCost = 0;
        stats.externalCallTime = 0;
        stats.externalCallCost = 0;
        stats.internalSMSAmount = 0;
        stats.internalSMSCost = 0;
        stats.externalSMSAmount = 0;
        stats.externalSMSCost = 0;
        stats.calls = 3;

        REQUIRE(collectStatistics(data) == stats);

        // First minute is always charged full, then by seconds.
        // So lets check it out ;).
        data.push_back(Entry("+420123456789", 30, Call, Internal));
        stats.calls += 1;
        stats.internalCallTime += 30;
        stats.internalCallCost += 1.5;
        stats.totalCost += 1.5;
        REQUIRE(collectStatistics(data) == stats);

        data.push_back(Entry("+420123456789", 70, Call, Internal));
        stats.calls += 1;
        stats.internalCallTime += 70;
        stats.internalCallCost += 1.75;
        stats.totalCost += 1.75;
        REQUIRE(collectStatistics(data) == stats);
    }
    SECTION("External Calls")
    {
        // Test data
        std::vector<Entry> data =
            { Entry("+420123456789", 3000, Call, External)
            , Entry("+420123456789", 3000, Call, External)
            , Entry("+420732563345", 300, Call, External)
            };

        // Result
        CallStatistics stats = CallStatistics();
        stats.totalCost = 900;
        stats.internalCallTime = 0;
        stats.internalCallCost = 0;
        stats.externalCallTime = 6300;
        stats.externalCallCost = 0;
        stats.internalSMSAmount = 0;
        stats.internalSMSCost = 0;
        stats.externalSMSAmount = 0;
        stats.externalSMSCost = 0;
        stats.calls = 3;

        REQUIRE(collectStatistics(data) == stats);

        // First minute is always charged full, then by seconds.
        // So lets check it out ;).
        data.push_back(Entry("+420123456789", 30, Call, External));
        stats.calls += 1;
        stats.externalCallTime += 30;
        stats.externalCallCost += 3.5;
        stats.totalCost += 3.5;
        REQUIRE(collectStatistics(data) == stats);

        data.push_back(Entry("+420123456789", 90, Call, External));
        stats.calls += 1;
        stats.externalCallTime += 90;
        stats.externalCallCost += 5.25;
        stats.totalCost += 5.25;
        REQUIRE(collectStatistics(data) == stats);
    }
    SECTION("Internal Calls small Residuals")
    {
        // Test data
        std::vector<Entry> data =
            { Entry("+420123456789", 3000, Call, Internal)
            , Entry("+420123456789", 3001, Call, Internal)
            };

        // Result
        CallStatistics stats = CallStatistics();
        stats.totalCost = 901.5;
        stats.internalCallTime = 6001;
        stats.internalCallCost = 1.5;
        stats.externalCallTime = 0;
        stats.externalCallCost = 0;
        stats.internalSMSAmount = 0;
        stats.internalSMSCost = 0;
        stats.externalSMSAmount = 0;
        stats.externalSMSCost = 0;
        stats.calls = 2;

        REQUIRE(collectStatistics(data) == stats);
    }
    SECTION("Internal Calls large Residuals")
    {
        // Test data
        std::vector<Entry> data =
            { Entry("+420123456789", 3000, Call, Internal)
            , Entry("+420123456789", 3090, Call, Internal)
            };

        // Result
        CallStatistics stats = CallStatistics();
        stats.totalCost = 902.25;
        stats.internalCallTime = 6090;
        stats.internalCallCost = 2.25;
        stats.externalCallTime = 0;
        stats.externalCallCost = 0;
        stats.internalSMSAmount = 0;
        stats.internalSMSCost = 0;
        stats.externalSMSAmount = 0;
        stats.externalSMSCost = 0;
        stats.calls = 2;

        REQUIRE(collectStatistics(data) == stats);
    }
    SECTION("External Calls small Residuals")
    {
        // Test data
        std::vector<Entry> data =
            { Entry("+420123456789", 3000, Call, External)
            , Entry("+420123456789", 3001, Call, External)
            };

        // Result
        CallStatistics stats = CallStatistics();
        stats.totalCost = 903.5;
        stats.internalCallTime = 0;
        stats.internalCallCost = 0;
        stats.externalCallTime = 6001;
        stats.externalCallCost = 3.5;
        stats.internalSMSAmount = 0;
        stats.internalSMSCost = 0;
        stats.externalSMSAmount = 0;
        stats.externalSMSCost = 0;
        stats.calls = 2;

        REQUIRE(collectStatistics(data) == stats);
    }
    SECTION("External Calls large Residuals")
    {
        // Test data
        std::vector<Entry> data =
            { Entry("+420123456789", 3000, Call, External)
            , Entry("+420123456789", 3090, Call, External)
            };

        // Result
        CallStatistics stats = CallStatistics();
        stats.totalCost = 905.25;
        stats.internalCallTime = 0;
        stats.internalCallCost = 0;
        stats.externalCallTime = 6090;
        stats.externalCallCost = 5.25;
        stats.internalSMSAmount = 0;
        stats.internalSMSCost = 0;
        stats.externalSMSAmount = 0;
        stats.externalSMSCost = 0;
        stats.calls = 2;

        REQUIRE(collectStatistics(data) == stats);
    }
}
