#include "labelgen.h"
#include <string>

using namespace std;

namespace Codegeneration {
LabelGen::LabelGen() :
       		whileCounter( 0), thenCounter(0), elseCounter(0),
		garbageCounter(0){}

LabelGen::~LabelGen(){}

string LabelGen::makeWhileLabel(){
	string res = "WhileBlock";
	res +=(whileCounter++);
	return res;
}

string LabelGen::makeThenLabel(){
	string res = "ThenBlock";
	res += (thenCounter++);
	return res;
}

string LabelGen::makeElseLabel(){
	string res = "ElseBlock";
	res += (elseCounter++);
	return res;
}

string LabelGen::makeOtherLabel(){
	string res = "CC2013MakesCoolCompilers";
	res += (garbageCounter++);
	return res;
}
}
