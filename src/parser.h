#pragma once

#include <string>
#include <vector>


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
    std::string number;
    int duration;
    ChannelType channelType;
    DestinationType destinationType;
};

extern std::vector<Entry> retrieveData(const char* filePath);
