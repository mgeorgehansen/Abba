CXX_SOURCES = main.cxx foo.cxx
CXX_OBJECTS = main.o foo.o

CXXFLAGS = -std=c++0x

.PHONY: all clean
all: foo

clean:
	$(RM) -rf foo *.o

foo: $(CXX_OBJECTS)
	$(CXX) $(CPPFLAGS) $(CFLAGS) $(CXXFLAGS) -o $@ $^

%.o: %.cxx
	$(CXX) -c $(CPPFLAGS) $(CFLAGS) $(CXXFLAGS) -o $@ $<

