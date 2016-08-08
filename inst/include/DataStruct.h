#pragma once
#include <vector>

//base cell or pixel structure
struct Cell
{
	int row, column;
	int id;
};

struct Link
{
	Cell start, end;				//start and end nodes
	std::vector<Cell> connection;	//collection of cells that create the link
	float cost;						//cost of the link or path
};

struct Patch
{
	std::vector<Cell> body;			//body of the patch
	int id;							//id of the patch
};

//Map/Matrix container of type float - used to contain the output data (voronoi, link, and patches) as well as the cost map
typedef std::vector<float> flCol;
typedef std::vector<flCol> flMap; 

//LinkCell is used in a map to connect Cells together as the spreading progresses
struct LinkCell 
	: Cell   //inherits the cell structure
{
	Cell fromCell, originCell;	//fromCell - cell that it connects to | originCell - perimeter cell in the patch that the link came from
	float distance, cost;	
};

//Link map - used to create the links
typedef std::vector<LinkCell> lcCol;
typedef std::vector<lcCol> LinkMap;

//ActiveCell is used in the spreading algorithm
struct ActiveCell
	:Cell	//inherits the cell structure
{
	float time, distance, resistance, parentResistance;
	Cell originCell;
};

//interface input data
struct InputData
{
	std::vector<float> cost_vec;
	std::vector<float> distinctValues;
	int nrow, ncol;
	float habitat, nodata;
};

//interface output data
struct OutputData
{
	std::vector<float> voronoi_map, link_map, patch_map;
	std::vector<Link> link_data;
	std::vector<Patch> patch_list;
};

//queue for active cell holders used for spreading
struct ActiveCellHolder
{
	float value;
	std::vector<ActiveCell> list;

	void add(ActiveCell c)
	{
		if (list.size() <= 0)
			list.push_back(c);
		else
		{
			for (int i = list.size() - 1; i >= 0; i--)
			{
				if (list[i].distance <= c.distance)
				{
					list.insert(list.begin() + i + 1, c);
					break;
				}
			}
		}
	}

	unsigned int size()
	{
		return list.size();
	}
};

struct ActiveCellQueue
{
	std::vector<ActiveCellHolder> holder_list;

	void insertH(ActiveCellHolder h)
	{
		int index = 0;
		bool found = false;
		if (holder_list.size() <= 0)
			holder_list.push_back(h);
		else
		{
			for (int i = holder_list.size() - 1; i >= 0; i--)
			{
				if (holder_list[i].value <= h.value)
				{
					if (holder_list[i].value < h.value)
					{
						index = i + 1;
					}
					else
					{
						index = i;
						found = true;
					}
					break;
				}
			}

			if (found)
			{
				for (unsigned int i = 0; i < h.list.size(); i++)
				{
					holder_list[index].add(h.list[i]);
				}
			}
			else
			{
				holder_list.insert(holder_list.begin() + index, h);
			}
		}
	}

	unsigned int size()
	{
		return holder_list.size();
	}

};