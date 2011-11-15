#include "foo.hxx"

#include <cstdlib>

int
main(int argv, char** argc) {
    Foo::Foo*
    foo = new Foo::Foo();
    
    foo->foo();
    delete foo;
    
    return EXIT_SUCCESS;
}

