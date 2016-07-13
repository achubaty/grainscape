#include "../inst/include/Calculation.h"
#include <Rcpp.h>
using namespace Rcpp;

bool outOfBounds(int row, int col, int nrow, int ncol)
{
	if (row < 0 || row >= nrow || col < 0 || col >= ncol)
		return true;
	return false;
}

float calcDistance(Cell c1, Cell c2)
{
	int dr = c1.row - c2.row;
	int dc = c1.column - c2.column;
	return sqrt((float)(dr*dr) + (float)(dc*dc));
}

bool cellsEqual(Cell c1, Cell c2)
{
	if (c1.row == c2.row && c1.column == c2.column)
		return true;
	return false;
}

//Patch Finding Functions
int combinePatches(int & ind1, int & ind2, std::vector<Patch> & list, flMap & patchMap)
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
			patchMap[cList[i].row][cList[i].column] = (float)id;
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
			patchMap[cList[i].row][cList[i].column] = (float)id;
		}
		list.erase(list.begin() + ind1);
	}
	return ret;
}

int getIndexFromList(float & id, std::vector<Patch> & patches)
{
	for (unsigned int i = 0; i < patches.size(); i++)
	{
		if (patches[i].id == id)
			return i;
	}
	return -1;
}

std::vector<Patch> findPatches(flMap & cost, int nrow, int ncol, int habitat, flMap & map)
{
	std::vector<Patch> ret;
	int idCount = 5;

	//row loop
	for (int row = 0; row < nrow; row++)
	{
		for (int col = 0; col < ncol; col++)
		{
			if ((cost)[row][col] == (float)habitat)
			{
				int ind1 = -1;
				//top left
				if (!outOfBounds(row - 1, col - 1, nrow, ncol) && cost[row - 1][col - 1] == habitat)
				{
					ind1 = getIndexFromList((map)[row - 1][col - 1], ret);
				}
				//left
				if (!outOfBounds(row, col - 1, nrow, ncol) && cost[row][col - 1] == habitat)
				{
					ind1 = getIndexFromList((map)[row][col - 1], ret);
				}

				//top and top right will be in the same patch
				int ind2 = -1;
				//top right
				if (!outOfBounds(row - 1, col + 1, ncol, nrow) && cost[row - 1][col + 1] == habitat)
				{
					ind2 = getIndexFromList((map)[row - 1][col + 1], ret);
				}
				//top
				if (!outOfBounds(row - 1, col, ncol, nrow) && cost[row - 1][col] == habitat)
				{
					ind2 = getIndexFromList((map)[row - 1][col], ret);
				}

				int finalInd = -1;
				//Go through cases
				if (ind1 == ind2) finalInd = ind1;
				else if (ind1 == -1 && ind2 != -1) finalInd = ind2;
				else if (ind1 != -1 && ind2 == -1) finalInd = ind1;
				else if (ind1 != -1 && ind2 != -1)
				{
					finalInd = combinePatches(ind1, ind2, ret, (map));
				}
				else finalInd = -1;

				if (finalInd == -1)
				{
					//No index found then create a new patch
					Patch temp;
					temp.id = idCount++;
					Cell c = { row, col, temp.id };
					(map)[row][col] = (float)(temp.id);
					temp.body.push_back(c);
					ret.push_back(temp);
				}
				else
				{
					Cell c = { row, col, ret[finalInd].id };
					ret[finalInd].body.push_back(c);
					(map)[row][col] = (float)(ret[finalInd].id);
				}
			}
		}
	}
	return ret;
}

//Linking Functions
void connectCell(ActiveCell * ac, int row, int col, float cost, LinkMap & link_map)
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
	link_map[row][col] = lc;
}

void findPath(LinkCell * ac1, LinkCell * ac2, std::vector<Link> & path_list, LinkMap & link_map)
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
	LinkCell lc_temp = link_map[ac1->row][ac1->column];
	path.start = parseMap(lc_temp, path, link_map);
	//end cell
	lc_temp = link_map[ac2->row][ac2->column];
	path.end = parseMap(lc_temp, path,link_map);

	//check if a cheaper indirect path is available
	lookForIndirectPath(path_list, path);
	path_list.push_back(path);
}

void fillLinkMap(flMap & map, std::vector<Link> path_list)
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

Cell parseMap(LinkCell lc, Link & path,LinkMap & link_map)
{
	Cell ret;
	Cell c1 = { lc.row, lc.column, lc.id };
	while (!cellsEqual(c1, lc.originCell))
	{
		path.cost += lc.cost;
		path.connection.push_back(c1);
		ret = lc;
		Cell from = lc.fromCell;
		lc = link_map[from.row][from.column];
		c1.row = lc.row;
		c1.column = lc.column;
		c1.id = lc.id;
	}
	return ret;
}

void lookForIndirectPath(std::vector<Link> & path_list, Link & path)
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
