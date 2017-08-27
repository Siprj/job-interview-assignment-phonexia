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
    Entry
        ( std::string number
        , int duration
        , ChannelType channelType
        , DestinationType destinationType
        )
    {
        this->number = number;
        this->duration = duration;
        this->channelType = channelType;
        this->destinationType = destinationType;
    };

    Entry() {};

    std::string number;
    int duration;
    ChannelType channelType;
    DestinationType destinationType;
};

extern std::vector<Entry> retrieveData(const char* filePath);
