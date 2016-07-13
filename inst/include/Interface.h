#pragma once
#include "Engine.h"
#include <vector>

//acts as the interface for the engine. Just create any language wrapper and call this function
static bool CalcEngine(InputData & inData, OutputData & outData, float increment = 1.0f)
{
	Engine eng(&inData, &outData, increment);
	if (!eng.initialize())
		return false;
	eng.start();
	return true;
}
