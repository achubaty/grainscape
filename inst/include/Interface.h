#pragma once
#include "Engine.h"

//acts as the interface for the engine. Just create any language wrapper and call this function
static bool CalcEngine(InputData & inData, OutputData & outData,float increment, char * errmsg)
{
	Engine eng(&inData, &outData, increment,errmsg);
	if (!eng.initialize())
	{
		return false;
	}
	eng.start();
	return true;
}
