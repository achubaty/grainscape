#include "../inst/include/Engine.h"

Engine::Engine(InputData * in_d, OutputData * out_d, float increment,char * errmsg)
{ 
	in_data = in_d;
	out_data = out_d;
	time_increment = increment;
	internal_time = 0.0f;
	initialized = false;
	voronoi_map = flMap(in_d->nrow, flCol(in_d->ncol, 0.0f));
	link_map = flMap(in_d->nrow, flCol(in_d->ncol, 0.0f));
	cost_map = flMap(in_data->nrow, flCol(in_data->ncol, 0.0f));
	error_message = errmsg;
}

Engine::Engine()
{
	in_data = 0;
	out_data = 0;
	initialized = false;
	internal_time = 0.0f;
	time_increment = 1.0f;
}

Engine::~Engine()
{}

bool Engine::initialize()
{
	Rprintf("Initializing...\n");
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
	Rprintf("Found %d patches\n", out_data->patch_list.size());

	//update the output patch vector
	updateOutputMap(out_data->patch_map, voronoi_map);

	//update link map
	for (int i = 0; i < in_data->nrow; i++)
	{
		for (int j = 0; j < in_data->ncol; j++)
		{
			if (voronoi_map[i][j] > 0)
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

				if (isActive)
				{
					holder holder_t;
					holder_t.value = 0.0f;
					Cell c;
					c.row = i;
					c.column = j;
					c.id = (int)(voronoi_map[i][j]);
					ActiveCell ac;
					ac.time = internal_time;
					ac.id = c.id;
					ac.distance = 0.0f;
					ac.resistance = cost_map[i][j];
					ac.originCell = c;
					ac.row = i;
					ac.column = j;
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

	Rprintf("Found %d active cells\n", active_cell_holder.holder_list[0].size());
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
		//update the time
		internal_time += time_increment;

		//seperate the first elements of active_cell_holder from the rest
		std::vector<ActiveCell> ac_to_check = active_cell_holder.holder_list[0].list;
		temporary_active_cell_holder = active_cell_holder;
		//erase the first element from the temporary active_cell_holder
		temporary_active_cell_holder.holder_list.erase(temporary_active_cell_holder.holder_list.begin());
		//go through each of the active cells to see if they need to spread
		for (unsigned int i = 0; i < ac_to_check.size(); i++)
		{
			activeCellSpreadChecker(&ac_to_check[i]);
		}
		//go hrough spread list to start spreading by 1 through the voronoi map
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
		spread_list.clear();

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
	if ((internal_time - ac->time) >= ac->resistance)
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
		new_ac.time = internal_time;
		new_ac.distance = dist;
		new_ac.resistance = cost_map[row][col];
		new_ac.originCell = ac->originCell;
		new_ac.id = c.id;
		new_ac.row = c.row;
		new_ac.column = c.column;

		if (cost_map[row][col] == in_data->nodata) //handle no data values
		{
			new_ac.time = internal_time;
			new_ac.distance = dist;
			new_ac.resistance = 0.0f;
			new_ac.originCell = ac->originCell;
			new_ac.id = c.id;
			new_ac.row = c.row;
			new_ac.column = c.column;
		}
		else //connect cells
			connectCell(ac, row, col, cost_map[row][col], iLinkMap);

		voronoi_map[row][col] = (float)(ac->id);

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
	if (!outOfBounds(row, col, in_data->nrow, in_data->ncol) && voronoi_map[row][col] != 0.0f && voronoi_map[row][col] != ac->id && cost_map[row][col] != in_data->nodata)
	{
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