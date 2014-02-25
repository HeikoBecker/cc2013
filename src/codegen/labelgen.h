#ifndef LABELGEN_H
#define LABELGEN_H
#include <string>

class LabelGen {

	private:
		unsigned long long int whileCounter, thenCounter, elseCounter, garbageCounter;
	public:
		LabelGen();
		~LabelGen();
		std::string makeWhileLabel();
		std::string makeThenLabel();
		std::string makeElseLabel();
		std::string makeOtherLabel();
};

#endif
