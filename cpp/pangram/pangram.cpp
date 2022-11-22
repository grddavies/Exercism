#include "pangram.h"

namespace pangram
{
    bool is_pangram(const std::string &str)
    {
        uint_fast32_t flags = 0;
        for (const char &c : str)
        {
            if (std::isalpha(c))
                flags |= 1 << (std::tolower(c) - 'a');
        }
        return flags == ((1 << 26) - 1);
    }
} // namespace pangram
