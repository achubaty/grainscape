#pragma once
#include "Engine.h"

//acts as the interface for the engine. Just create any language wrapper and call this function
static bool CalcEngine(InputData & inData, OutputData & outData, char * errmsg, float thresh = 0.0001f)
{
  Engine eng(&inData, &outData, errmsg,thresh);
  if (!eng.initialize())
  {
    return false;
  }
  eng.start();
  return true;
}
