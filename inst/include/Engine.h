#pragma once
#include "Calculation.h"
#include "DataStruct.h"


class Engine
{
private:
	//properties
	InputData * in_data;			//stores the data given by the user
	OutputData * out_data;			//stores the data that's given to the user

	float internal_time;			//engine's internal time
	float time_increment;			//engine's internal time increment

	queue active_cell_holder;
	queue temporary_active_cell_holder;
	std::vector<ActiveCell> spread_list;

	LinkMap iLinkMap;				//A map that has all the cell connections

	flMap voronoi_map;				//A voronoi map that contains float numbers in each cell
	flMap link_map;					//A link map that contains float numbers in eachh cell
	flMap cost_map;

	bool initialized;

	//functions
	bool cellIsZero(int row, int col);
	void activeCellSpreadChecker(ActiveCell * ac);
	void createActiveCell(ActiveCell * ac, int row, int col);
	void updateOutputMap(std::vector<float> & vm, flMap mm);
public:
	Engine(InputData * in_d, OutputData * out_d, float increment);
	Engine();
	~Engine();

	bool initialize();
	void start();
};

