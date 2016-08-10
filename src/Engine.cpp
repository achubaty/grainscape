#include "../inst/include/Engine.h"

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
	//check to see if the input vector is equal to the number of cells in a map
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
  out_data->patch_list = findPatches(in_data->nrow, in_data->ncol, in_data->habitat);
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
          ActiveCellHolder holder_t;
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

  //if the initial active cell holder is zero then there's no need for the engine to start
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

	if (!initialized)
	{
		char msg[] = "Engine is not initialized. Failed";
		writeErrorMessage(msg);
		return;
	}

	//keep looping until there aren't any more active cells
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
    ActiveCellHolder h_temp;
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

    if (fabs(cost_map[row][col] - in_data->nodata) <= zeroThreshold)//handle no data values
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
      connectCell(ac, row, col, cost_map[row][col]);

    ActiveCellHolder h_temp;
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
    if (fabs(cost_map[row][col] - in_data->nodata) <= zeroThreshold)
    {
      return;
    }
    else
      findPath(&iLinkMap[ac->row][ac->column], &iLinkMap[row][col], out_data->link_data);
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

bool Engine::outOfBounds(int row, int col, int nrow, int ncol)
{
  if (row < 0 || row >= nrow || col < 0 || col >= ncol)
    return true;
  return false;
}

float Engine::calcDistance(Cell c1, Cell c2)
{
  int dr = c1.row - c2.row;
  int dc = c1.column - c2.column;
  return sqrt((float)(dr*dr) + (float)(dc*dc));
}

bool Engine::cellsEqual(Cell c1, Cell c2)
{
  if (c1.row == c2.row && c1.column == c2.column)
    return true;
  return false;
}

//Patch Finding Functions
std::vector<Patch> Engine::findPatches(int nrow, int ncol, int habitat)
{
	std::vector<Patch> ret;
	int idCount = 5;

	//row loop
	for (int row = 0; row < nrow; row++)
	{
		for (int col = 0; col < ncol; col++)
		{
			if (cost_map[row][col] == (float)habitat)
			{
				int ind1 = -1;
				//top left
				if (!outOfBounds(row - 1, col - 1, nrow, ncol) && cost_map[row - 1][col - 1] == habitat)
				{
					ind1 = getIndexFromList(voronoi_map[row - 1][col - 1], ret);
				}
				//left
				if (!outOfBounds(row, col - 1, nrow, ncol) && cost_map[row][col - 1] == habitat)
				{
					ind1 = getIndexFromList(voronoi_map[row][col - 1], ret);
				}

				//top and top right will be in the same patch
				int ind2 = -1;
				//top right
				if (!outOfBounds(row - 1, col + 1, ncol, nrow) && cost_map[row - 1][col + 1] == habitat)
				{
					ind2 = getIndexFromList(voronoi_map[row - 1][col + 1], ret);
				}
				//top
				if (!outOfBounds(row - 1, col, ncol, nrow) && cost_map[row - 1][col] == habitat)
				{
					ind2 = getIndexFromList(voronoi_map[row - 1][col], ret);
				}

				int finalInd = -1;
				//Go through cases
				if (ind1 == ind2) finalInd = ind1;
				else if (ind1 == -1 && ind2 != -1) finalInd = ind2;
				else if (ind1 != -1 && ind2 == -1) finalInd = ind1;
				else if (ind1 != -1 && ind2 != -1)
				{
					finalInd = combinePatches(ind1, ind2, ret);
				}
				else finalInd = -1;

				if (finalInd == -1)
				{
					//No index found then create a new patch
					Patch temp;
					temp.id = idCount++;
					Cell c = { row, col, temp.id };
					voronoi_map[row][col] = (float)(temp.id);
					temp.body.push_back(c);
					ret.push_back(temp);
				}
				else
				{
					Cell c = { row, col, ret[finalInd].id };
					ret[finalInd].body.push_back(c);
					voronoi_map[row][col] = (float)(ret[finalInd].id);
				}
			}
		}
	}
	return ret;
}

int Engine::combinePatches(int & ind1, int & ind2, std::vector<Patch> & list)
{
  int ret = -1;
  Patch * p1 = &list[ind1];
  Patch * p2 = &list[ind2];
  if (p1->body.size() > p2->body.size())
  {
    if (ind1 > ind2)
      ret = ind1 - 1;
    else
      ret = ind1;
    //transfer all of p2's cells to p1 and update the patch map
    std::vector<Cell> cList = p2->body;

    int id = p1->id;
    for (unsigned int i = 0; i < cList.size(); i++)
    {
      p1->body.push_back(cList[i]);
      voronoi_map[cList[i].row][cList[i].column] = (float)id;
    }
    list.erase(list.begin() + ind2);
  }
  else
  {
    if (ind2 > ind1)
      ret = (ind2 - 1);
    else
      ret = ind2;

    //transfer all of p1's cells to p2 and update the patch map
    std::vector<Cell> cList = p1->body;
    int id = p2->id;
    for (unsigned int i = 0; i < cList.size(); i++)
    {
      p2->body.push_back(cList[i]);
	  voronoi_map[cList[i].row][cList[i].column] = (float)id;
    }
    list.erase(list.begin() + ind1);
  }
  return ret;
}

int Engine::getIndexFromList(float & id, std::vector<Patch> & patches)
{
  for (unsigned int i = 0; i < patches.size(); i++)
  {
    if (patches[i].id == id)
      return i;
  }
  return -1;
}

//Linking Functions
void Engine::connectCell(ActiveCell * ac, int row, int col, float cost)
{
  LinkCell lc;
  lc.row = row;
  lc.column = col;
  lc.distance = ac->distance;
  lc.id = ac->id;
  lc.fromCell.row = ac->row;
  lc.fromCell.column = ac->column;
  lc.fromCell.id = lc.id;
  lc.originCell = ac->originCell;
  lc.cost = cost;
  iLinkMap[row][col] = lc;
}

void Engine::findPath(LinkCell * ac1, LinkCell * ac2, std::vector<Link> & path_list)
{
  //check if the path already exists in the path_list
  for (unsigned int i = 0; i < path_list.size(); i++)
  {
    if ((path_list[i].end.id == ac1->id && path_list[i].start.id == ac2->id) || (path_list[i].start.id == ac1->id && path_list[i].end.id == ac2->id))
    {
      return;
    }
  }

  //create the path
  Link path;
  path.cost = 0.0f;
  //start cell
  LinkCell lc_temp = iLinkMap[ac1->row][ac1->column];
  path.start = parseMap(lc_temp, path);
  //end cell
  lc_temp = iLinkMap[ac2->row][ac2->column];
  path.end = parseMap(lc_temp, path);

  //check if a cheaper indirect path is available
  lookForIndirectPath(path_list, path);
  path_list.push_back(path);
}

void Engine::fillLinkMap(flMap & map, std::vector<Link> path_list)
{
  for (unsigned int i = 0; i < path_list.size(); i++)
  {
    for (unsigned int j = 0; j < path_list[i].connection.size(); j++)
    {
      int row = path_list[i].connection[j].row;
      int col = path_list[i].connection[j].column;
      map[row][col] = 1.0f;
    }
  }
}

Cell Engine::parseMap(LinkCell lc, Link & path)
{
  Cell ret;
  Cell c1 = { lc.row, lc.column, lc.id };
  while (!cellsEqual(c1, lc.originCell))
  {
    path.cost += lc.cost;
    path.connection.push_back(c1);
    ret = lc;
    Cell from = lc.fromCell;
	lc = iLinkMap[from.row][from.column];
    c1.row = lc.row;
    c1.column = lc.column;
    c1.id = lc.id;
  }
  return ret;
}

void Engine::lookForIndirectPath(std::vector<Link> & path_list, Link & path)
{
  //if path_list is empty then automatically return false
  if (path_list.size() <= 0)
  {
    return;
  }

  for (unsigned int i = 0; i < path_list.size(); i++)
  {
    if (path_list[i].start.id == path.start.id || path_list[i].end.id == path.start.id)
    {
      Link * pPath1 = &path_list[i];
      float cost = pPath1->cost;
      std::vector<Link> connection;
      for (unsigned int j = 0; j < path_list.size(); j++)
      {
        Link * pPath2 = &path_list[j];
        if (pPath1 != pPath2)
        {

          cost += pPath2->cost;
          if (cost > path.cost)
          {
            cost -= pPath2->cost;
            break;
          }
          connection.push_back(*pPath1);
          pPath1 = pPath2;
          if (pPath2->end.id == path.end.id || pPath2->start.id == path.end.id)
          {
            path.cost = cost;
            path.connection = connection[0].connection;
            for (unsigned int k = 1; k < connection.size(); k++)
            {
              for (unsigned int m = 0; m < connection[k].connection.size(); m++)
              {
                path.connection.push_back(connection[k].connection[m]);
              }
            }
            return;
          }
        }
      }
    }
  }
}
