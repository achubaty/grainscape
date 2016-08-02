#include "../inst/include/Engine.h"
#include <algorithm>

Engine::Engine(InputData * in_d, OutputData * out_d,char * errmsg, float threshold)
{ 
	in_data = in_d;
	out_data = out_d;;
	initialized = false;
	voronoi_map = flMap(in_d->nrow, flCol(in_d->ncol, 0.0f));
	link_map = flMap(in_d->nrow, flCol(in_d->ncol, 0.0f));
	cost_map = flMap(in_d->nrow, flCol(in_d->ncol, 0.0f));
	error_message = errmsg;
	maxCost = emax(in_d->distinctValues);
	costRes = in_d->distinctValues[0];
	zeroThreshold = threshold;
}

Engine::Engine()
{
	in_data = 0;
	out_data = 0;
	initialized = false;
	maxCost = 0.0f;
}

Engine::~Engine()
{}

bool Engine::initialize()
{
	unsigned int size = in_data->nrow * in_data->ncol;
	if (size != in_data->cost_vec.size())
	{
		char msg[] = "Product of number of rows and columns did not match cost/resistance vector size\n";
		writeErrorMessage(msg);
		return false;
	}

	//insert the cost values in the actual cost map
	for (unsigned int i = 0; i < cost_map.size(); i++)
	{
		for (unsigned int j = 0; j < cost_map[0].size(); j++)
		{
			cost_map[i][j] = (float)in_data->cost_vec[i*in_data->ncol + j];
			if ((float)in_data->cost_vec[i*in_data->ncol + j] <= 0.0001f)
				Rprintf("Zero at element %d or (%d, %d)\n", i*in_data->ncol + j, i, j);
		}
	}

	//find the patches first
	out_data->patch_list = findPatches(cost_map, in_data->nrow, in_data->ncol, in_data->habitat, voronoi_map);
	if (out_data->patch_list.size() <= 0)
	{
		char msg[] = "No patches found\n";
		writeErrorMessage(msg);
		return false;
	}

	//update the output patch vector
	updateOutputMap(out_data->patch_map, voronoi_map);

	//update link map
	for (int i = 0; i < in_data->nrow; i++)
	{
		for (int j = 0; j < in_data->ncol; j++)
		{
			if (voronoi_map[i][j] > 0.0f)
			{
				link_map[i][j] = 1.0f;
			}
		}
	}

	//get the initial active/spread cells
	for (int i = 0; i < in_data->nrow; i++)
	{
		for (int j = 0; j < in_data->ncol; j++)
		{
			if (cost_map[i][j] == in_data->habitat)
			{
				bool isActive = false;
				//look at all teh 4 adjacent cells
				//left
				isActive = cellIsZero(i, j - 1);
				//right
				if (!isActive && cellIsZero(i, j + 1))
					isActive = true;
				//bottom
				if (!isActive && cellIsZero(i + 1, j))
					isActive = true;
				//top
				if (!isActive && cellIsZero(i - 1, j))
					isActive = true;

				//if the cell is supposed to be an active cell then add it to the 
				//active cell holder
				if (isActive)
				{
					holder holder_t;
					holder_t.value = 0.0f;
					Cell c;
					c.row = i;
					c.column = j;
					c.id = (int)(voronoi_map[i][j]);
					ActiveCell ac;
					ac.time = 0.0f;
					ac.id = c.id;
					ac.distance = 0.0f;
					ac.resistance = cost_map[i][j];
					ac.originCell = c;
					ac.row = i;
					ac.column = j;
					ac.parentResistance = 0.0f;
					holder_t.add(ac);
					active_cell_holder.insertH(holder_t);
				}
			}
		}
	}

	if (active_cell_holder.size() <= 0)
	{
		char msg[] = "No initial active cells found\n";
		writeErrorMessage(msg);
		return false;
	}
	//resize link map and initialize the cells
	iLinkMap = LinkMap(in_data->nrow, lcCol(in_data->ncol));
	for (unsigned int i = 0; i < active_cell_holder.size(); i++)
	{
		ActiveCell ac = active_cell_holder.holder_list[0].list[i];
		LinkCell lc;
		lc.row = ac.row;
		lc.column = ac.column;
		lc.fromCell = ac.originCell;
		lc.originCell = ac.originCell;
		lc.distance = 0.0f;
		lc.id = ac.id;
		lc.cost = 0.0f;
		iLinkMap[lc.row][lc.column] = lc;
	}

	initialized = true;
	return initialized;
}

void Engine::start()
{
	while (active_cell_holder.size() > 0)
	{	
		//clear the temporary active cell holder
		temporary_active_cell_holder.holder_list.clear();

		//go through each active cell to see if any of them are ready to spread
		//if they are ready to spread then add them to the spread_list
		//if not then include that active cell in the temporary active cell holder
		for (unsigned int i = 0; i < active_cell_holder.size(); i++)
		{
			std::vector<ActiveCell> ac_to_check = active_cell_holder.holder_list[i].list;
			for (unsigned int j = 0; j < ac_to_check.size(); j++)
			{
				activeCellSpreadChecker(&ac_to_check[j]);
			}
		}

		//go through each spreading cell and check all the adjacent cells if they can be conquered
		for (unsigned int i = 0; i < spread_list.size(); i++)
		{
			//top
			createActiveCell(&spread_list[i], spread_list[i].row - 1, spread_list[i].column);
			//bottom
			createActiveCell(&spread_list[i], spread_list[i].row + 1, spread_list[i].column);
			//left
			createActiveCell(&spread_list[i], spread_list[i].row, spread_list[i].column - 1);
			//right
			createActiveCell(&spread_list[i], spread_list[i].row, spread_list[i].column + 1);
		}
		//clear the spread list
		spread_list.clear();
		//set the new active cells
		active_cell_holder = temporary_active_cell_holder;
	}

	//fill the link map
	fillLinkMap(link_map, out_data->link_data);
	updateOutputMap(out_data->link_map, link_map);
	updateOutputMap(out_data->voronoi_map, voronoi_map);
}

void Engine::updateOutputMap(std::vector<float> & vm, flMap mm)
{
	vm = std::vector<float>(in_data->nrow*in_data->ncol, 0);
	for (int i = 0; i < in_data->nrow; i++)
	{
		for (int j = 0; j < in_data->ncol; j++)
		{
			vm[i*in_data->ncol + j] = mm[i][j];
		}
	}
}

bool Engine::cellIsZero(int row, int col)
{
	if (!outOfBounds(row, col, in_data->nrow, in_data->ncol) && voronoi_map[row][col] == 0.0f)
		return true;
	return false;
}

void Engine::activeCellSpreadChecker(ActiveCell * ac)
{
	//if the time difference is greater than the resistance value at cell c
	//include it in the spread_list
	//if ((internal_time - ac->time) >= ac->resistance)
	if (ac->time >= ac->resistance)
	{
		//include the list in spread list in an order
		if (spread_list.size() <= 0)
			spread_list.push_back(*ac);
		else
		{
			//the active cell with the shortest eucledian distance should be placed up front
			// the spread_list is in increasing order of euclediance distance
			int index = 0;
			for (int i = spread_list.size() - 1; i >= 0; i--)
			{
				if (spread_list[i].distance <= ac->distance)
				{
					index = i + 1;
					break;
				}
			}
			spread_list.insert(spread_list.begin() + index, *ac);
		}
	}
	else
	{
		//if the time difference is still smaller than the resistance value
		//include it in the temporary_active_cell_holder
		//find the proper queue that the active cell belongs to
		ac->time += std::max(1.0f, (ac->resistance - ac->parentResistance) * costRes / maxCost);
		holder h_temp;
		h_temp.value = ac->distance;
		h_temp.list.push_back(*ac);
		if (temporary_active_cell_holder.size() <= 0)
		{
			temporary_active_cell_holder.holder_list.push_back(h_temp);
		}
		else
		{
			temporary_active_cell_holder.insertH(h_temp);
		}
	}
}

void Engine::createActiveCell(ActiveCell * ac, int row, int col)
{
	//if not out of bounds and have not been conquered by other patches then create a new active cell
	if (!outOfBounds(row, col, in_data->nrow, in_data->ncol) && voronoi_map[row][col] == 0.0f)
	{
		Cell c;
		c.row = row;
		c.column = col;
		c.id = ac->id;
		float dist = calcDistance(ac->originCell, c);
		ActiveCell new_ac;
		new_ac.time = 0.0f;
		new_ac.distance = dist;
		new_ac.resistance = cost_map[row][col];
		new_ac.originCell = ac->originCell;
		new_ac.id = c.id;
		new_ac.row = c.row;
		new_ac.column = c.column;
		new_ac.parentResistance = ac->resistance;

		voronoi_map[row][col] = (float)(ac->id);

		if (abs(cost_map[row][col] - in_data->nodata) <= zeroThreshold)//handle no data values
		{
			new_ac.distance = dist;
			new_ac.resistance = 0.0f;
			new_ac.originCell = ac->originCell;
			new_ac.id = c.id;
			new_ac.row = c.row;
			new_ac.column = c.column;
			new_ac.parentResistance = 0.0f;
		}
		else //connect cells
			connectCell(ac, row, col, cost_map[row][col], iLinkMap);

		

		holder h_temp;
		h_temp.value = dist;
		h_temp.list.push_back(new_ac);
		if (temporary_active_cell_holder.size() <= 0)
		{
			temporary_active_cell_holder.holder_list.push_back(h_temp);
		}
		else
		{
			temporary_active_cell_holder.insertH(h_temp);
		}
	}


	//create the links
	if (!outOfBounds(row, col, in_data->nrow, in_data->ncol) && voronoi_map[row][col] != 0.0f && voronoi_map[row][col] != ac->id )
	{
		if (abs(cost_map[row][col] - in_data->nodata) <= zeroThreshold)
		{
			//Rprintf("Not a valid link\n");
			return;
		}
		else
			findPath(&iLinkMap[ac->row][ac->column], &iLinkMap[row][col], out_data->link_data, iLinkMap);
	}
}

void Engine::writeErrorMessage(char* msg)
{
	if (strlen(msg) > strlen(error_message))
		return;

	for (unsigned int i = 0; i < strlen(error_message); i++)
	{
		if (i < strlen(msg))
			error_message[i] = msg[i];
		else
			error_message[i] = 0;
	}
}


float Engine::emax(std::vector<float> vec)
{
	float ret = vec[0];
	for (unsigned int i = 1; i < vec.size(); i++)
	{
		if (ret < vec[i])
			ret = vec[i];
	}
	return ret;
}