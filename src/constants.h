#pragma once

#include <vector>
#include <string>


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

class Free
{
public:
   static std::vector<std::string> numbers;
};
