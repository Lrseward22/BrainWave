#ifndef BRAINWAVE_UTILS_MANGLER_H
#define BRAINWAVE_UTILS_MANGLER_H

#include <string>

namespace brainwave {

class Mangler {
public:
    static std::string mangleStart();
    static std::string mangleClass(std::string name);
    static std::string mangleFunction(std::string name);
    static std::string mangleConstructor();
    static std::string mangleStatic();
    static std::string mangleType(std::string type);
};
} // Namespace brainwave

#endif
