#include <brainwave/Utils/Mangler.h>

using namespace brainwave;

std::string Mangler::mangleStart() { 
    return "_bw";
}
std::string Mangler::mangleClass(std::string name) { 
    return std::to_string(name.size()) + name;
}
std::string Mangler::mangleFunction(std::string name) { 
    return std::to_string(name.size()) + name;
}
std::string Mangler::mangleType(std::string type) { 
    return std::to_string(type.size()) + type;
}

