/** Engine Class
 * The engine class is specifically created to spread and find connections between habitats given a cost vector and the habitat id or number.
 * The engine is initialized by converting the cost vector into a map, it then finds the habitats or patches that correspond to the habitat id, and
 * finds the initial active cells. Any errors or misbehaviours in the engine is recorded and sent to the interface for diagnostic purposes. 
*/

#pragma once
#include "Calculation.h"			//Contains functions for finding patches, linking cells, and other common functions
#include "DataStruct.h"				//Contains data structures necessary for the engine to work
#include <cstring>					

#define MAX_CHAR_SIZE 1024			//Serves as the maximum amount of letters that can or should be stored for the error message

class Engine
{
private:
	//properties
	InputData * in_data;				//stores the data given by the user
	OutputData * out_data;				//stores the data that's given to the user

	float maxCost;						//maximum cost in the cost vector
	float costRes;						//cost resolution
	float zeroThreshold;				//degree of error for the floating point zero value (floating point have both +ve and -ve values)

	queue active_cell_holder;				//active cells holder
	queue temporary_active_cell_holder;		//a temporary active cell holder when the active cell holder is being modified
	std::vector<ActiveCell> spread_list;	//stores the active cells that are ready to spread to their adjacent cells

	LinkMap iLinkMap;				//A map that has all the cell connections

	flMap voronoi_map;				//A voronoi map that contains float numbers in each cell
	flMap link_map;					//A link map that contains float numbers in eachh cell
	flMap cost_map;

	bool initialized;				//indicates if the engine is ready to run or begin

	char * error_message;			//a pointer to a character, the contents of this variable is modified if the engine does not work properly

	//functions
	//indicates if the cell or pixel has a zero value in the voronoi map
	bool cellIsZero(int row, int col);							

	//checks if the given active cell is ready to spread.If the cell can spread, it is added to the spread list
	void activeCellSpreadChecker(ActiveCell * ac);

	//creates new active cells and stores them in the temporary active cell holder
	void createActiveCell(ActiveCell * ac, int row, int col);

	//it updates the output data's vectors,, given the vector that needs to be updated (vm) and the map that contains the new data (mm)
	void updateOutputMap(std::vector<float> & vm, flMap mm);

	//if an engine error ever occurs this function is called to send the reason for the crash to an external char [] variable
	//it writes whatever was in the given msg to the engine's error_message variable
	void writeErrorMessage(char* msg);
public:
	//constructor that takes in a pointer to the input data, pointer to the output data, pointer to a character, and a zero threshold (which is defaulted to 0.0001)
	Engine(InputData * in_d, OutputData * out_d, char * errmsg, float threshold = 0.000f);

	//default constructor
	Engine();

	//destructor
	~Engine();

	//initializes the engine
	bool initialize();

	//starts the engine or it begins the alogrithm
	void start();

	//finds the maximum value in the vector of values
	static float emax(std::vector<float> vec);
};

