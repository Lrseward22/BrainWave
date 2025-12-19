#include <brainwave/Sema/Sema.h>
#include <iostream>

using namespace brainwave;

// Get ast
// Figure out what stmt it is
// Handle it accordingly
std::unique_ptr<AST> Sema::next() {
    ast = P.parse();
    while (ast) {
        ast->print();
        std::cout << "\n";
        ast = P.parse();
    }
    return std::move(ast);
}
