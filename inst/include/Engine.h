#pragma once
#include "Calculation.h"
#include "DataStruct.h"
#include <cstring>

#include <Rinternals.h>

#define MAX_CHAR_SIZE 1024

class Engine
{
private:
	//properties
	InputData * in_data;			//stores the data given by the user
	OutputData * out_data;			//stores the data that's given to the user

	float internal_time;			//engine's internal time
	float time_increment;			//engine's internal time increment

	float maxCost;
	float costRes;

	queue active_cell_holder;
	queue temporary_active_cell_holder;
	std::vector<ActiveCell> spread_list;

	LinkMap iLinkMap;				//A map that has all the cell connections

	flMap voronoi_map;				//A voronoi map that contains float numbers in each cell
	flMap link_map;					//A link map that contains float numbers in eachh cell
	flMap cost_map;

	bool initialized;

	char * error_message;
	//functions
	bool cellIsZero(int row, int col);
	void activeCellSpreadChecker(ActiveCell * ac);
	void createActiveCell(ActiveCell * ac, int row, int col);
	void updateOutputMap(std::vector<float> & vm, flMap mm);
	void writeErrorMessage(char* msg);
public:
	Engine(InputData * in_d, OutputData * out_d, float increment, char * errmsg);
	Engine();
	~Engine();

	bool initialize();
	void start();

	static float emax(std::vector<float> vec);
};

