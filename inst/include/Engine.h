/** Engine Class
 * The engine class is specifically created to spread and find connections between habitats given a cost vector and the habitat id or number.
 * The engine is initialized by converting the cost vector into a map, it then finds the habitats or patches that correspond to the habitat id, and
 * finds the initial active cells. Any errors or misbehaviours in the engine is recorded and sent to the interface for diagnostic purposes. 
*/

#pragma once
#include "DataStruct.h"                //Contains data structures necessary for the engine to work
#include <cstring>        
#include <algorithm>
#include <cmath>

#define MAX_CHAR_SIZE 1024            //Serves as the maximum amount of letters that can or should be stored for the error message

class Engine
{
private:
    //properties
    InputData * in_data;                //stores the data given by the user
    OutputData * out_data;                //stores the data that's given to the user

    float maxCost;                        //maximum cost in the cost vector
    float costRes;                        //cost resolution
    float zeroThreshold;                //degree of error for the floating point zero value (floating point have both +ve and -ve values)

    ActiveCellQueue active_cell_holder;                //active cells holder
    ActiveCellQueue temporary_active_cell_holder;    //a temporary active cell holder when the active cell holder is being modified
    std::vector<ActiveCell> spread_list;            //stores the active cells that are ready to spread to their adjacent cells

    LinkMap iLinkMap;                //A map that has all the cell connections

    flMap voronoi_map;                //A voronoi map that contains float numbers in each cell
    flMap link_map;                    //A link map that contains float numbers in each cell
    flMap cost_map;                    //A map taht contains all the costs/resistances per cell

    bool initialized;                //indicates if the engine is ready to run or begin

    char * error_message;            //a pointer to a character, the contents of this variable is modified if the engine does not work properly

    //Functions://
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

    //Initialization functions://
    //Common functions
    //checks to see if the inserted row or column is out of bounds
    bool outOfBounds(int row, int col, int nrow, int ncol);
    //checks to see if the two cells are equivalent
    bool cellsEqual(Cell c1, Cell c2);

    //Patch Finding Functions
    //combines two patches if they are contiguous
    int combinePatches(int & ind1, int & ind2, std::vector<Patch> & list);
    //finds the appropriate index of the sough patch based on the given ID
    int getIndexFromList(float & id, std::vector<Patch> & patches);
    //called from the initialization step to find all the patches in the map
    std::vector<Patch> findPatches(int nrow, int ncol, int habitat);

    //Linking functions
    //connects two cells together
    void connectCell(ActiveCell * ac, int row, int col, float cost);
    //finds the least cost path between two cells, only called when boundaries meet
    void findPath(LinkCell * ac1, LinkCell * ac2, std::vector<Link> & path_list);
    //fills the link map that contains floating point values
    void fillLinkMap(flMap & map, std::vector<Link> path_list);
    //finds all the cells that create the link
    Cell parseMap(LinkCell lc, Link & path);
    //it looks for indirect path that's "cheaper" than the direct path and gives the new path the indirect path if found
    void lookForIndirectPath(std::vector<Link> & path_list, Link & path);
    
public:
    //constructor that takes in a pointer to the input data, pointer to the output data, pointer to a character, and a zero threshold (which is defaulted to 0.0001)
    Engine(InputData * in_d, OutputData * out_d, char * errmsg, float threshold = 0.000001f);

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

    //finds the euclidean distance between two cells
    static float calcDistance(Cell c1, Cell c2);
};

