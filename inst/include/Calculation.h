#pragma once
#include "DataStruct.h"
#include <vector>
#include <cmath>

//Common functions
bool outOfBounds(int row, int col, int nrow, int ncol);
float calcDistance(Cell c1, Cell c2);
bool cellsEqual(Cell c1, Cell c2);

//Patch Finding Functions
int combinePatches(int & ind1, int & ind2, std::vector<Patch> & list, flMap & pachMap);
int getIndexFromList(float & id, std::vector<Patch> & patches);

std::vector<Patch> findPatches(flMap & cost, int nrow, int ncol, int habitat, flMap & map);

//Linking functions
void connectCell(ActiveCell * ac, int row, int col, float cost, LinkMap & link_map);
void findPath(LinkCell * ac1, LinkCell * ac2, std::vector<Link> & path_list, LinkMap & link_map);
void fillLinkMap(flMap & map, std::vector<Link> path_list);

Cell parseMap(LinkCell lc, Link & path, LinkMap & link_map);
void lookForIndirectPath(std::vector<Link> & path_list, Link & path);

